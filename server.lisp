#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.server)

(defclass timeoutable ()
  ((timeout :initform NIL :accessor timeout)))

(defmethod start-timeout ((timeoutable timeoutable))
  (setf (timeout timeoutable) (+ (get-universal-time) (lichat-protocol:lifetime timeoutable))))

(defmethod reset-timeout ((timeoutable timeoutable))
  (setf (timeout timeoutable) NIL))

(defmethod alive-p ((timeoutable timeoutable))
  (when (timeout timeoutable)
    (< (get-universal-time) (timeout timeoutable))))

(defclass server (lichat-protocol:user)
  ((users :initform (make-hash-table :test 'equal) :accessor users)
   (profiles :initform (make-hash-table :test 'equal) :accessor profiles)
   (channels :initform (make-hash-table :test 'equal) :accessor channels)
   (salt :initarg :salt :accessor salt))
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

(defmethod find-user (name server)
  (gethash (coerce-username name) (users server)))

(defmethod (setf find-user) (user name server)
  (setf (gethash (coerce-username name) (users server)) user))

(defmethod remove-user (name server)
  (remhash (coerce-username name) (users server)))

(defmethod find-profile (name server)
  ;; FIXME: Check timeout, remove if done
  (gethash (coerce-username name) (profiles server)))

(defmethod (setf find-profile) (profile name server)
  (setf (gethash (coerce-username name) (profiles server)) profile))

(defmethod remove-profile (name server)
  (remhash (coerce-username name) (profiles server)))

(defmethod find-channel (name server)
  (cond ((eql name T)
         (find-channel (lichat-protocol:name server) server))
        (T
         ;; FIXME: Check timeout, remove if done
         (gethash (coerce-channelname name) (channels server)))))

(defmethod (setf find-channel) (channel name server)
  (setf (gethash (coerce-channelname name) (channels server)) channel))

(defmethod remove-channel (name server)
  (remhash (coerce-channelname name) (channels server)))

(defun prep-perms (registrant perms)
  (sublis `((:registrant . ,registrant)) perms))

(defun permitted (action channel user)
  (let ((entry (find action (lichat-protocol:permissions channel))))
    (when entry
      (or (eql T (second action))
          (find (lichat-protocol:name user) (second action) :test #'string-equal)))))

(defun check-permitted (action channel user)
  (unless (permitted action channel user)
    (error "Not permitted.")))

(defmethod create (registrant name server)
  (let ((username (lichat-protocol:name (find-user registrant server))))
    (setf (find-channel name server)
          (cond ((find-channel name server)
                 (error "A channel with that name already exists."))
                ((or (not name) (string= name ""))
                 (make-instance 'channel
                                :name (format NIL "@~a" (lichat-protocol:next-id))
                                :permissions (prep-perms username lichat-protocol:*default-anonymous-channel-permissions*)
                                :lifetime 0))
                ((string= name (lichat-protocol:name server))
                 (make-instance 'channel
                                :name name
                                :permissions (prep-perms username lichat-protocol:*default-primary-channel-permissions*)
                                :lifetime most-positive-fixnum))
                ((typep name 'lichat-protocol:channelname)
                 (make-instance 'channel
                                :name name
                                :permissions (prep-perms username lichat-protocol:*default-regular-channel-permissions*)
                                :lifetime lichat-protocol:*default-channel-lifetime*))
                (T
                 (error "Invalid channel name ~s" name))))))

(defmethod join ((channel lichat-protocol:channel) (user lichat-protocol:user))
  (when (find channel (lichat-protocol:channels user))
    (error "Already in channel."))
  (push user (lichat-protocol:users channel))
  (push channel (lichat-protocol:channels user))
  (reset-timeout channel)
  (send! channel 'join :from (lichat-protocol:name user)
                       :channel (lichat-protocol:name channel)))

(defmethod leave ((channel lichat-protocol:channel) (user lichat-protocol:user))
  (unless (find channel (lichat-protocol:channels user))
    (error "Not in channel."))
  (send! channel 'leave :from (lichat-protocol:name user)
                        :channel (lichat-protocol:name channel))
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
      (handler-case
          (process connection message)
        (failure-condition (err)
          (apply #'send! connection (failure-type err) (failure-args err)))
        (lichat-protocol:protocol-condition (err)
          (send! connection 'failure
                 :text (format NIL "Internal error: ~a" err)))))
    message))

(defmacro define-update-handler (type (connection update) &body body)
  `(defmethod process ((,connection connection) (,update ,(find-symbol (string type) :lichat-protocol)))
     (flet ((find-user (ident)
              (find-user ident (server ,connection)))
            (find-profile (ident)
              (find-profile ident (server ,connection)))
            (find-channel (ident)
              (find-channel ident (server ,connection))))
       (declare (ignorable find-user find-profile find-channel))
       ,@body)))

(defmethod process :around ((connection connection) (update lichat-protocol:update))
  (restart-case
      (call-next-method)
    (continue ()
      :report "Respond with a failure and return."
      (send! connection 'update-failure
             :update-id (lichat-protocol:id update)
             :text (format NIL "Internal error; update flushed.")))))

(define-update-handler connect (connection update)
  (cond ((string/= (lichat-protocol:version update)
                   (lichat-protocol:protocol-version))
         (fail! 'lichat-protocol:incompatible-version
                :update-id (lichat-protocol:id update)
                :compatible-versions (list (lichat-protocol:protocol-version))
                :text (format NIL "~a is not supported." (lichat-protocol:version update))))
        ((lichat-protocol:password update)
         (let ((profile (find-profile update)))
           (cond ((not profile)
                  (fail! 'lichat-protocol:no-such-profile
                         :update-id (lichat-protocol:id update)
                         :text (format NIL "~a is not registered." (lichat-protocol:from update))))
                 ((string/= (cryptos:pbkdf2-hash (lichat-protocol:password update)
                                                 (salt (server connection)))
                            (lichat-protocol:password profile))
                  (fail! 'lichat-protocol:invalid-password
                         :update-id (lichat-protocol:id update)
                         :text (format NIL "Your password is wrong.")))
                 (T
                  (init-connection connection (lichat-protocol:from update))))))
        ((find-user update)
         (fail! 'lichat-protocol:username-taken
                :update-id (lichat-protocol:id update)
                :text (format NIL "The name ~s is already in use." (lichat-protocol:from update))))
        ((find-profile update)
         (fail! 'lichat-protocol:username-taken
                :update-id (lichat-protocol:id update)
                :text (format NIL "The name ~s is registered." (lichat-protocol:from update))))
        (T
         (init-connection connection (lichat-protocol:from update)))))

(defmethod init-connection ((connection connection) username)
  (let* ((server (server connection))
         (user (find-user username server)))
    (cond (user
           (setf (lichat-protocol:user connection) user)
           (dolist (channel (channels user))
             (send! connection 'join :from (lichat-protocol:name user)
                                     :channel (lichat-protocol:name channel))))
          (T
           (setf user (make-instance 'user :name username))
           (setf (find-user username server) user)
           (setf (lichat-protocol:user connection) user)
           (join (find-channel (lichat-protocol:name server) server) user)))
    (push connection (lichat-protocol:connections user))
    (reset-timeout (find-profile (lichat-protocol:name user) server))
    (send! connection 'connect
           :version (lichat-protocol:protocol-version))
    connection))

(defmethod teardown-connection ((connection connection))
  (let ((user (lichat-protocol:user connection)))
    (setf (lichat-protocol:connections user)
          (remove connection (lichat-protocol:connections user)))
    (unless (lichat-protocol:connections user)
      (remove-user user (server connection))
      (dolist (channel (lichat-protocol:channels user))
        (leave channel user))
      (start-timeout (find-profile (lichat-protocol:name user) (server connection))))))

(define-update-handler disconnect (connection update)
  (teardown-connection connection)
  (send! connection 'disconnect))

;;; FIXME FOR INVALID FROM FIELDS.
;; the protocol has FROM fields in order to both
;; identify the source on the receiving side, and
;; to act in stead of someone else on the sending
;; side. However, this must be checked and properly
;; treated in the processing functions, which it
;; currently is not.

(defun check-from (connection update)
  (let ((user (find-user (lichat-protocol:from update) connection)))
    (unless (eql user (lichat-protocol:user connection))
      (error "FROM field does not match connection source."))
    user))

(defun check-target (connection update)
  (let ((user (find-user (lichat-protocol:target update) connection)))
    (unless user
      (error "No such target."))
    user))

(defun check-channel (connection update)
  (let ((channel (find-channel (lichat-protocol:channel update) connection)))
    (unless channel
      (error "No such channel."))
    (unless (find channel (lichat-protocol:channels (lichat-protocol:user connection)))
      (error "User not in channel."))
    channel))

(define-update-handler message (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (check-permitted 'message channel user)
    (send update channel)))

(define-update-handler join (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (check-permitted 'join channel user)
    (join channel user)))

(define-update-handler leave (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (check-permitted 'leave channel user)
    (leave channel user)))

(define-update-handler ping (connection update)
  ;; Do something with the timing.
  (send! connection 'pong
         :id (lichat-protocol:id update)))

(define-update-handler channels (connection update)
  (send! connection 'channels
         :channels (loop for channel being the hash-values of (channels (server connection)) 
                         when (check-permitted 'channels channel user)
                         collect (lichat-protocol:name channel))))

(define-update-handler users (connection update)
  (let ((channel (check-channel connection update)))
    (check-permitted 'users channel user)
    (send! connection 'users
           :channel (lichat-protocol:name channel)
           :users (lichat-protocol:users (lichat-protocol:users channel)))))

(define-update-handler create (connection update)
  (let ((user (check-from connection update)))
    (check-permitted 'message (find-channel T (server connection)) user)
    (join (create user
                  (lichat-protocol:channel update)
                  (server connection))
          user)))

(define-update-handler kick (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update))
        (target (check-target connection update)))
    (unless (find channel (lichat-protocol:channels target))
      (error "Target not in channel."))
    (check-permitted 'kick channel user)
    (send! channel 'kick
           :from (lichat-protocol:name user)
           :channel (lichat-protocol:name channel)
           :target (lichat-protocol:name target))
    (leave channel target)))

(define-update-handler pull (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update))
        (target (check-target connection update)))
    (when (find channel (lichat-protocol:channels target))
      (error "Target already in channel."))
    (check-permitted 'pull channel user)
    (join channel target)))

(define-update-handler permissions (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (cond ((lichat-protocol:permissions update)
           (check-permitted 'permissions channel user)
           (setf (lichat-protocol:permissions channel)
                 (lichat-protocol:permissions update))
           (send! channel 'permissions
                  :from (lichat-protocol:name user)
                  :channel (lichat-protocol:name channel)
                  :permissions (lichat-protocol:permissions channel)))
          (T
           (send! connection 'permissions
                  :from (lichat-protocol:name user)
                  :channel (lichat-protocol:name channel)
                  :permissions (lichat-protocol:permissions channel))))))

(define-update-handler register (connection update)
  (let ((user (check-from connection update)))
    (register user (lichat-protocol:password update) (server connection))
    (send! connection 'register
           :password "")))
