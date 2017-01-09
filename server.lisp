#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.serverlib)

(defclass timeoutable ()
  ((timeout :initform NIL :accessor timeout)))

(defmethod start-timeout ((timeoutable timeoutable))
  (setf (timeout timeoutable) (+ (get-universal-time) (lichat-protocol:lifetime timeoutable))))

(defmethod reset-timeout ((timeoutable timeoutable))
  (setf (timeout timeoutable) NIL))

(defmethod alive-p ((timeoutable timeoutable))
  (when (timeout timeoutable)
    (< (get-universal-time) (timeout timeoutable))))

;; FIXME: What about channels created by unregs? What happens if
;;        the unreg leaves and potentially a new user under the same
;;        name enters? Might need to rethink perms for unregs.
;; FIXME: Secure asynchronous access to users/profiles/channels.
(defclass server (lichat-protocol:user)
  ((users :initform (make-hash-table :test 'equal) :accessor users)
   (profiles :initform (make-hash-table :test 'equal) :accessor profiles)
   (channels :initform (make-hash-table :test 'equal) :accessor channels)
   (salt :initarg :salt :accessor salt)
   ;; FIXME: Flood control
   )
  (:default-initargs
   :salt ""))

(defmethod initialize-instance :after ((server server) &key name)
  (check-type name lichat-protocol:username)
  (setf (find-user name server) server)
  (create name name server)
  (join (find-channel name server) server))

(defclass connection (lichat-protocol:connection)
  ((server :initarg :server :accessor server)))

(defclass channel (lichat-protocol:channel timeoutable)
  ())

(defclass profile (lichat-protocol:profile timeoutable)
  ())

(defclass user (lichat-protocol:user)
  ())

(defun coerce-username (name-ish)
  (etypecase name-ish
    (lichat-protocol:update
     (string-downcase (lichat-protocol:from name-ish)))
    (lichat-protocol:user
     (string-downcase (lichat-protocol:name name-ish)))
    (string (string-downcase name-ish))))

(defun coerce-channelname (name-ish)
  (etypecase name-ish
    (lichat-protocol:channel-update
     (string-downcase (lichat-protocol:channel name-ish)))
    (lichat-protocol:channel
     (string-downcase (lichat-protocol:name name-ish)))
    (string (string-downcase name-ish))))

(defmethod find-user (name (server server))
  (gethash (coerce-username name) (users server)))

(defmethod (setf find-user) (user name (server server))
  (setf (gethash (coerce-username name) (users server)) user))

(defmethod remove-user (name (server server))
  (remhash (coerce-username name) (users server)))

(defmethod find-profile (name (server server))
  ;; FIXME: Check timeout, remove if done
  (gethash (coerce-username name) (profiles server)))

(defmethod (setf find-profile) (profile name (server server))
  (setf (gethash (coerce-username name) (profiles server)) profile))

(defmethod remove-profile (name (server server))
  (remhash (coerce-username name) (profiles server)))

(defmethod find-channel (name (server server))
  (cond ((eql name T)
         (find-channel (lichat-protocol:name server) server))
        (T
         ;; FIXME: Check timeout, remove if done
         (gethash (coerce-channelname name) (channels server)))))

(defmethod (setf find-channel) (channel name (server server))
  (setf (gethash (coerce-channelname name) (channels server)) channel))

(defmethod remove-channel (name (server server))
  (remhash (coerce-channelname name) (channels server)))

(defun prep-perms (registrant perms)
  (sublis `((:registrant . ,registrant)) perms))

(defun permitted (action channel user)
  (let ((entry (assoc action (lichat-protocol:permissions channel))))
    (when entry
      (or (eql T (second entry))
          (find (lichat-protocol:name user) (second entry) :test #'string-equal)))))

(defmethod create (registrant name server)
  (let* ((username (lichat-protocol:name (find-user registrant server)))
         (channel (cond ((not name)
                         (make-instance 'channel
                                        :name (format NIL "@~a" (lichat-protocol:next-id))
                                        :permissions (prep-perms username lichat-protocol:*default-anonymous-channel-permissions*)
                                        :lifetime 0))
                        ((string= name (lichat-protocol:name server))
                         (make-instance 'channel
                                        :name name
                                        :permissions (prep-perms username lichat-protocol:*default-primary-channel-permissions*)
                                        :lifetime most-positive-fixnum))
                        (T
                         (make-instance 'channel
                                        :name name
                                        :permissions (prep-perms username lichat-protocol:*default-regular-channel-permissions*)
                                        :lifetime lichat-protocol:*default-channel-lifetime*)))))
    (setf (find-channel (lichat-protocol:name channel) server) channel)))

(defmethod join ((channel lichat-protocol:channel) (user lichat-protocol:user) &optional id)
  (pushnew user (lichat-protocol:users channel))
  (pushnew channel (lichat-protocol:channels user))
  (reset-timeout channel)
  (send! channel 'join :from (lichat-protocol:name user)
                       :channel (lichat-protocol:name channel)
                       :id (or id (lichat-protocol:next-id))))

(defmethod leave ((channel lichat-protocol:channel) (user lichat-protocol:user) &optional id)
  (send! channel 'leave :from (lichat-protocol:name user)
                        :channel (lichat-protocol:name channel)
                        :id (or id (lichat-protocol:next-id)))
  (setf (lichat-protocol:users channel) (remove user (lichat-protocol:users channel)))
  (setf (lichat-protocol:channels user) (remove channel (lichat-protocol:channels user)))
  (unless (lichat-protocol:users channel)
    (start-timeout channel)))

(defmethod register (registrant password server)
  (let ((user (find-user registrant server)))
    (unless user
      (error "No such user."))
    (when (find-profile (lichat-protocol:name user) server)
      (error "Already registered."))
    (setf (find-profile user server)
          (make-instance 'profile
                         :name (lichat-protocol:name user)
                         :password (cryptos:pbkdf2-hash password (salt server))))))

(define-condition failure-condition (error)
  ((failure-type :initarg :type :reader failure-type)
   (failure-args :initarg :args :reader failure-args))
  (:report (lambda (c s) (format s "Failure: (~a ~{~s~^ ~})"
                                 (failure-type c) (failure-args c)))))

(defun fail! (type-ish &rest initargs)
  (error 'failure-condition :type type-ish :args initargs))

(defun send! (connection type-ish &rest initargs)
  (unless (getf initargs :from)
    (push (lichat-protocol:name (server connection)) initargs)
    (push :from initargs))
  (send (apply #'make-instance
               (find-symbol (symbol-name type-ish) :lichat-protocol)
               initargs)
        connection))

(defmethod send ((object lichat-protocol:wire-object) (channel lichat-protocol:channel))
  (dolist (user (lichat-protocol:users channel))
    (send object user)))

(defmethod send ((object lichat-protocol:wire-object) (user lichat-protocol:user))
  (dolist (connection (lichat-protocol:connections user))
    (send object connection)))

(defmethod process ((connection connection) (stream stream))
  (let ((message))
    (handler-case
        (setf message (lichat-protocol:from-wire stream))
      (lichat-protocol:wire-condition (err)
        (send! connection 'malformed-update
               :text (princ-to-string err))))
    (when (typep message 'lichat-protocol:wire-object)
      (process connection message))
    message))

(defmacro define-update-handler (type (connection update) &body body)
  `(defmethod process ((,connection connection) (,update ,(find-symbol (string type) :lichat-protocol)))
     ,@body))

(defmethod process :around ((connection connection) (update lichat-protocol:update))
  (restart-case
      (handler-case
          (call-next-method)
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

(defmethod init-connection ((connection connection) update)
  (let* ((username (lichat-protocol:from update))
         (server (server connection))
         (user (find-user username server))
         (user-already-there user))
    (cond (user-already-there
           (setf (lichat-protocol:user connection) user))
          (T
           (setf user (make-instance 'user :name username))
           (setf (find-user username server) user)
           (setf (lichat-protocol:user connection) user)))
    (push connection (lichat-protocol:connections user))
    (when (find-profile user server)
      (reset-timeout (find-profile user server)))
    (send! connection 'connect
           :id (lichat-protocol:id update)
           :version (lichat-protocol:protocol-version))
    (if user-already-there
        (dolist (channel (channels user))
          (send! connection 'join :from (lichat-protocol:name user)
                                  :channel (lichat-protocol:name channel)))
        (join (find-channel (lichat-protocol:name server) server) user))
    connection))

(defmethod teardown-connection ((connection connection))
  (let ((user (lichat-protocol:user connection)))
    (when user
      (setf (lichat-protocol:connections user)
            (remove connection (lichat-protocol:connections user)))
      (unless (lichat-protocol:connections user)
        (remove-user user (server connection))
        (dolist (channel (lichat-protocol:channels user))
          (leave channel user))
        (let ((profile (find-profile (lichat-protocol:name user) (server connection))))
          (when profile (start-timeout profile)))))))

(defun check-permitted (connection update &optional (channel (lichat-protocol:channel update)))
  (unless (permitted (type-of update) (find-channel channel (server connection))
                     (find-user (lichat-protocol:from update) (server connection)))
    (fail! 'lichat-protocol:insufficient-permissions :update-id (lichat-protocol:id update))))

(defun check-from (connection update)
  (let ((user (find-user (lichat-protocol:from update) (server connection))))
    (unless (eql user (lichat-protocol:user connection))
      (fail! 'lichat-protocol:username-mismatch :update-id (lichat-protocol:id update)))
    user))

(defun check-target (connection update)
  (let ((user (find-user (lichat-protocol:target update) (server connection))))
    (unless user
      (fail! 'lichat-protocol:no-such-user :update-id (lichat-protocol:id update)))
    user))

(defun check-channel (connection update &optional (must-be-in T))
  (let ((channel (find-channel (lichat-protocol:channel update) (server connection))))
    (unless channel
      (fail! 'lichat-protocol:no-such-channel :update-id (lichat-protocol:id update)))
    (when (and must-be-in (not (find channel (lichat-protocol:channels (lichat-protocol:user connection)))))
      (fail! 'lichat-protocol:not-in-channel :update-id (lichat-protocol:id update)))
    channel))

(defun check-channelname (connection update)
  (let ((name (lichat-protocol:channel update)))
    (when name
      (unless (lichat-protocol:channelname-p name)
        (fail! 'lichat-protocol:bad-name :update-id (lichat-protocol:id update)))
      (when (find-channel name (server connection))
        (fail! 'lichat-protocol:channelname-taken :update-id (lichat-protocol:id update))))))

(define-update-handler connect (connection update)
  (unless (lichat-protocol:username-p (lichat-protocol:from update))
    (fail! 'lichat-protocol:bad-name :update-id (lichat-protocol:id update)))
  (cond ((string/= (lichat-protocol:version update)
                   (lichat-protocol:protocol-version))
         (fail! 'lichat-protocol:incompatible-version
                :update-id (lichat-protocol:id update)
                :compatible-versions (list (lichat-protocol:protocol-version))))
        ((lichat-protocol:password update)
         (let ((profile (find-profile update (server connection))))
           (cond ((not profile)
                  (fail! 'lichat-protocol:no-such-profile
                         :update-id (lichat-protocol:id update)))
                 ((string/= (cryptos:pbkdf2-hash (lichat-protocol:password update)
                                                 (salt (server connection)))
                            (lichat-protocol:password profile))
                  (fail! 'lichat-protocol:invalid-password
                         :update-id (lichat-protocol:id update)))
                 (T
                  (init-connection connection update)))))
        ((find-user update (server connection))
         (fail! 'lichat-protocol:username-taken
                :update-id (lichat-protocol:id update)))
        ((find-profile update (server connection))
         (fail! 'lichat-protocol:username-taken
                :update-id (lichat-protocol:id update)))
        (T
         (init-connection connection update))))

(define-update-handler disconnect (connection update)
  (teardown-connection connection)
  (ignore-errors (send update connection))
  (invoke-restart 'close-connection))

(define-update-handler message (connection update)
  (let ((channel (check-channel connection update)))
    (check-from connection update)
    (check-permitted connection update)
    (send update channel)))

(define-update-handler join (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update NIL)))
    (check-permitted connection update)
    (when (find channel (lichat-protocol:channels user))
      (fail! 'lichat-protocol:already-in-channel :update-id (lichat-protocol:id update)))
    (join channel user (lichat-protocol:id update))))

(define-update-handler leave (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (check-permitted connection update)
    (leave channel user (lichat-protocol:id update))))

(define-update-handler ping (connection update)
  ;; Do something with the timing.
  (send! connection 'pong
         :id (lichat-protocol:id update)))

(define-update-handler channels (connection update)
  (let ((user (find-user (lichat-protocol:from update) (server connection))))
    (send! connection 'channels
           :id (lichat-protocol:id update)
           :channels (loop for channel being the hash-values of (channels (server connection)) 
                           when (permitted (type-of update) channel user)
                           collect (lichat-protocol:name channel)))))

(define-update-handler users (connection update)
  (let ((channel (check-channel connection update)))
    (check-permitted connection update)
    (send! connection 'users
           :id (lichat-protocol:id update)
           :channel (lichat-protocol:name channel)
           :users (mapcar #'lichat-protocol:name (lichat-protocol:users channel)))))

(define-update-handler create (connection update)
  (let ((user (check-from connection update)))
    (check-channelname connection update)
    (check-permitted connection update T)
    (join (create user
                  (lichat-protocol:channel update)
                  (server connection))
          user
          (lichat-protocol:id update))))

(define-update-handler kick (connection update)
  (let ((channel (check-channel connection update))
        (target (check-target connection update)))
    (check-from connection update)
    (unless (find channel (lichat-protocol:channels target))
      (fail! 'lichat-protocol:not-in-channel :update-id (lichat-protocol:id update)))
    (check-permitted connection update)
    (send update channel)
    (leave channel target)))

(define-update-handler pull (connection update)
  (let ((channel (check-channel connection update))
        (target (check-target connection update)))
    (check-from connection update)
    (when (find channel (lichat-protocol:channels target))
      (fail! 'lichat-protocol:already-in-channel :update-id (lichat-protocol:id update)))
    (check-permitted connection update)
    (send update channel)
    (send update target)))

(define-update-handler permissions (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (cond ((lichat-protocol:permissions update)
           (check-permitted connection update)
           (setf (lichat-protocol:permissions channel)
                 (lichat-protocol:permissions update))
           (send! channel 'permissions
                  :id (lichat-protocol:id update)
                  :from (lichat-protocol:name user)
                  :channel (lichat-protocol:name channel)
                  :permissions (lichat-protocol:permissions channel)))
          (T
           (send! connection 'permissions
                  :id (lichat-protocol:id update)
                  :from (lichat-protocol:name user)
                  :channel (lichat-protocol:name channel)
                  :permissions (lichat-protocol:permissions channel))))))

(define-update-handler register (connection update)
  (let ((user (check-from connection update)))
    (register user (lichat-protocol:password update) (server connection))
    (send update connection)))
