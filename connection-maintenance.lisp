(in-package #:org.shirakumo.lichat.serverlib)

(define-condition failure-condition (error)
  ((failure-type :initarg :type :reader failure-type)
   (failure-args :initarg :args :reader failure-args))
  (:report (lambda (c s) (format s "Failure: (~a ~{~s~^ ~})"
                                 (failure-type c) (failure-args c)))))

(define-condition severe-failure-condition (failure-condition)
  ())

(defun fail! (type-ish &rest initargs)
  (error 'failure-condition :type type-ish :args initargs))

(defun fail!! (type-ish &rest initargs)
  (error 'severe-failure-condition :type type-ish :args initargs))

(defun send! (connection type-ish &rest initargs)
  (unless (getf initargs :from)
    (push (lichat-protocol:name (server connection)) initargs)
    (push :from initargs))
  (send (apply #'make-instance
               (find-symbol (symbol-name type-ish) :lichat-protocol)
               initargs)
        connection))

(define-compiler-macro send! (&whole whole &environment env connection type-ish &rest initargs)
  (if (constantp type-ish env)
      (let ((conn (gensym "CONNECTION")))
        `(let ((,conn ,connection))
           (send (make-instance (load-time-value (find-symbol (string ,type-ish) :lichat-protocol))
                                ,@(unless (getf initargs :from)
                                    `(:from (lichat-protocol:name (server ,conn))))
                                ,@initargs)
                 ,conn)))
      whole))

(defmethod check-connection-timeout ((connection connection))
  (when (< (idle-timeout (server connection))
           (- (get-universal-time) (last-update connection)))
    ;; Avoid infinite recursion by updating. Doesn't matter since we're closing
    ;; down anyway.
    (setf (last-update connection) (get-universal-time))
    (send! connection 'connection-unstable
           :text (format NIL "Ping idle-timeout of ~d second~:p reached."
                         (idle-timeout (server connection))))
    (send! connection 'disconnect)
    (teardown-connection connection)
    (error "Connection has timed out.")))

;; We handle the timeout here because the protocol specifies that the server has to
;; regularly send out a ping request, meaning SEND will be called regularly too.
(defmethod send :before ((object lichat-protocol:object) (connection connection))
  (check-connection-timeout connection))

(defmethod send :after ((object lichat-protocol:update) (channel backlogged-channel))
  (setf (car (backlog channel)) object)
  (setf (backlog channel) (cdr (backlog channel))))

(defmethod send ((object lichat-protocol:object) (channel channel))
  (dolist (user (lichat-protocol:users channel))
    (send object user)))

(defmethod send ((object lichat-protocol:object) (user user))
  (dolist (connection (lichat-protocol:connections user))
    (send object connection)))

(defmethod pass-flood-gate ((connection connection) update)
  T)

(defmethod pass-flood-gate ((connection flood-protected-connection) (update lichat-protocol:update))
  (let ((frame (floor (get-universal-time) (flood-frame (server connection)))))
    (incf (frame-count connection))
    (cond ((/= frame (last-frame connection))
           ;; New frame, clear out.
           (setf (last-frame connection) frame)
           (setf (frame-count connection) 0))
          ((= (flood-limit (server connection)) (frame-count connection))
           ;; We've just reached the limit; notify the client.
           (send! connection 'too-many-updates :update-id (lichat-protocol:id update))
           NIL)
          ((< (flood-limit (server connection)) (frame-count connection))
           ;; We are over the limit. Just drop the update.
           NIL)
          (T
           ;; Within the limit, carry on.
           T))))

(defmethod process ((connection connection) (stream stream))
  (let ((message))
    (handler-case
        (setf message (lichat-protocol:from-wire stream (read-limit connection)))
      (lichat-protocol:read-limit-hit (err)
        (declare (ignore err))
        (send! connection 'update-too-long))
      (lichat-protocol:wire-condition (err)
        (send! connection 'malformed-update :text (princ-to-string err)))
      (lichat-protocol:incompatible-value-type-for-slot (err)
        (send! connection 'malformed-update :text (princ-to-string err))))
    (when (typep message 'lichat-protocol:object)
      (process connection message))
    message))

(defmethod process :around ((connection flood-protected-connection) (update lichat-protocol:update))
  (when (pass-flood-gate connection update)
    (call-next-method)))

(defmethod process :around ((connection connection) (update lichat-protocol:update))
  (setf (last-update connection) (get-universal-time))
  ;; FIXME: handle timestamp difference. For now we just force server time.
  (setf (lichat-protocol:clock update) (get-universal-time))
  (restart-case
      (handler-case
          (call-next-method)
        (severe-failure-condition (err)
          (apply #'send! connection (failure-type err) (failure-args err))
          (send! connection 'disconnect)
          (teardown-connection connection))
        (failure-condition (err)
          (apply #'send! connection (failure-type err) (failure-args err)))
        (lichat-protocol:protocol-condition (err)
          (send! connection 'failure
                 :text (format NIL "Internal error: ~a" err))))
    (continue ()
      :report "Respond with a failure and return."
      (send! connection 'update-failure
             :update-id (lichat-protocol:id update)
             :text (format NIL "Internal error; update flushed.")))))
