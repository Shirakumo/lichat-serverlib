#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.serverlib)

;; FIXME: What about channels created by unregs? What happens if
;;        the unreg leaves and potentially a new user under the same
;;        name enters? Might need to rethink perms for unregs.

(defun prep-perms (registrant perms)
  (sublis `((:registrant . ,registrant)) perms))

(defun rule-permitted (rule name)
  (etypecase rule
    ((eql T) T)
    (null NIL)
    (string (string= rule name))
    (list (ecase (first rule)
            (or (loop for sub in rule
                      thereis (rule-permitted sub name)))
            (and (loop for sub in rule
                       always (rule-permitted sub name)))
            (not (not (rule-permitted (second rule) name)))))))

(defun permitted (action channel user)
  (let ((entry (cdr (assoc action (lichat-protocol:permissions channel))))
        (name (coerce-username user)))
    (loop for rule in entry
          thereis (rule-permitted rule name))))

(defmethod create (registrant name (server server))
  (let* ((username (lichat-protocol:name (find-user registrant server)))
         (channel (cond ((not name)
                         (make-channel server
                                       :name (format NIL "@~a" (lichat-protocol:next-id))
                                       :permissions (prep-perms username lichat-protocol:*default-anonymous-channel-permissions*)
                                       :lifetime 0))
                        ((string= name (lichat-protocol:name server))
                         (make-channel server
                                       :name name
                                       :permissions (prep-perms username lichat-protocol:*default-primary-channel-permissions*)
                                       :lifetime most-positive-fixnum))
                        (T
                         (make-channel server
                                       :name name
                                       :permissions (prep-perms username lichat-protocol:*default-regular-channel-permissions*)
                                       :lifetime lichat-protocol:*default-channel-lifetime*)))))
    (setf (find-channel (lichat-protocol:name channel) server) channel)))

(defmethod join ((channel channel) (user user) &optional id)
  (pushnew user (lichat-protocol:users channel))
  (pushnew channel (lichat-protocol:channels user))
  (reset-timeout channel)
  (send! channel 'join :from (lichat-protocol:name user)
                       :channel (lichat-protocol:name channel)
                       :id (or id (lichat-protocol:next-id))))

(defmethod leave ((channel channel) (user user) &optional id)
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
          (make-profile server
                        :name (lichat-protocol:name user)
                        :password (cryptos:pbkdf2-hash password (salt server))))))

(defmethod init-connection ((connection connection) update)
  (let* ((username (lichat-protocol:from update))
         (server (server connection))
         (user (find-user username server))
         (user-already-there user))
    (cond (user-already-there
           (setf (lichat-protocol:user connection) user))
          (T
           (setf user (make-user server :name username))
           (setf (find-user username server) user)
           (setf (lichat-protocol:user connection) user)))
    (pushnew connection (lichat-protocol:connections user))
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
    (fail! 'lichat-protocol:insufficient-permissions
           :update-id (lichat-protocol:id update)
           :text (format NIL "You are not permitted to ~a in ~a."
                         (type-of update) channel))))

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
