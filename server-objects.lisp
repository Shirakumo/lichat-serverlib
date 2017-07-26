#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.serverlib)

(defclass timeoutable ()
  ((timeout :initarg :timeout :initform NIL :accessor timeout)))

(defmethod start-timeout ((timeoutable timeoutable))
  (setf (timeout timeoutable) (+ (get-universal-time) (lichat-protocol:lifetime timeoutable))))

(defmethod reset-timeout ((timeoutable timeoutable))
  (setf (timeout timeoutable) NIL))

(defmethod alive-p ((timeoutable timeoutable))
  (or (not (timeout timeoutable))
      (< (get-universal-time) (timeout timeoutable))))

(defclass channel (lichat-protocol:channel timeoutable)
  ())

(defun make-ring (size)
  (let ((list (make-list size)))
    (setf (cdr (last list)) list)))

(defclass backlogged-channel (channel)
  ((backlog :initform (make-ring 100) :accessor backlog)
   (join-times :initform (make-hash-table :test 'eq) :accessor join-times)))

(defclass user (lichat-protocol:user)
  ())

(defclass connection (lichat-protocol:connection)
  ((server :initarg :server :accessor server)
   (last-update :initform (get-universal-time) :accessor last-update)
   (read-limit :initarg :read-limit :accessor read-limit))
  (:default-initargs
   :server NIL))

(defmethod initialize-instance :after ((connection connection) &key server)
  (when (and server (not (slot-boundp connection 'read-limit)))
    (setf (read-limit connection) (default-read-limit server))))

(defmethod (setf server) :after ((server server) (connection connection))
  (when (not (slot-boundp connection 'read-limit))
    (setf (read-limit connection) (default-read-limit server))))

(defclass flood-protected-connection (connection)
  ((last-frame :initform 0 :accessor last-frame)
   (frame-count :initform 0 :accessor frame-count)))

(defclass profile (lichat-protocol:profile timeoutable)
  ())

(defclass server (user)
  ((users :initform (make-hash-table :test 'equal) :accessor users)
   (profiles :initform (make-hash-table :test 'equal) :accessor profiles)
   (channels :initform (make-hash-table :test 'equal) :accessor channels)
   (salt :initarg :salt :accessor salt)
   (idle-timeout :initarg :idle-timeout :accessor idle-timeout)
   (allowed-content-types :initarg :allowed-content-types :accessor allowed-content-types)
   (default-read-limit :initarg :default-read-limit :accessor default-read-limit))
  (:default-initargs
   :salt ""
   :idle-timeout 120
   :allowed-content-types NIL
   :default-read-limit NIL))

(defclass flood-protected-server (server)
  ((flood-frame :initarg :flood-frame :accessor flood-frame)
   (flood-limit :initarg :flood-limit :accessor flood-limit))
  (:default-initargs
   :flood-frame 30
   :flood-limit 40))

(defmethod initialize-instance :after ((server server) &key name)
  (check-type name lichat-protocol:username)
  (setf (find-user name server) server)
  (create name name server)
  (join (find-channel name server) server))

(defmethod make-connection ((server server) &rest initargs)
  (apply #'make-instance 'connection initargs))

(defmethod make-connection ((server flood-protected-server) &rest initargs)
  (apply #'make-instance 'flood-protected-connection initargs))

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

(defmethod make-user ((server server) &rest initargs)
  (apply #'make-instance 'user initargs))

(defmethod list-users ((server server))
  (loop for user being the hash-values of (users server)
        collect user))

(defmethod find-profile (name (server server))
  (let* ((name (coerce-username name))
         (profile (gethash name (profiles server))))
    (cond ((not profile) NIL)
          ((alive-p profile)
           profile)
          (T
           (remhash name (profiles server))
           NIL))))

(defmethod (setf find-profile) (profile name (server server))
  (setf (gethash (coerce-username name) (profiles server)) profile))

(defmethod remove-profile (name (server server))
  (remhash (coerce-username name) (profiles server)))

(defmethod make-profile ((server server) &rest initargs)
  (apply #'make-instance 'profile initargs))

(defmethod list-profiles ((server server))
  (loop for profile being the hash-values of (profiles server)
        collect profile))

(defmethod find-channel (name (server server))
  (cond ((eql name T)
         (find-channel (lichat-protocol:name server) server))
        (T
         (let* ((name (coerce-channelname name))
                (channel (gethash name (channels server))))
           (cond ((not channel) NIL)
                 ((alive-p channel)
                  channel)
                 (T
                  (remhash name (channels server))
                  NIL))))))

(defmethod (setf find-channel) (channel name (server server))
  (cond ((eql name T)
         (setf (find-channel (lichat-protocol:name server) server) channel))
        (T
         (setf (gethash (coerce-channelname name) (channels server)) channel))))

(defmethod remove-channel (name (server server))
  (cond ((eql name T)
         (remove-channel (lichat-protocol:name server) server))
        (T
         (remhash (coerce-channelname name) (channels server)))))

(defmethod make-channel ((server server) &rest initargs)
  (apply #'make-instance 'channel initargs))

(defmethod list-channels ((server server))
  (loop for channel being the hash-values of (channels server)
        collect channel))
