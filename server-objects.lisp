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
  (or (not (timeout timeoutable))
      (< (get-universal-time) (timeout timeoutable))))

(defclass channel (lichat-protocol:channel timeoutable)
  ())

(defclass user (lichat-protocol:user)
  ())

(defclass connection (lichat-protocol:connection)
  ((server :initarg :server :accessor server)
   (last-update :initform (get-universal-time) :accessor last-update)))

(defclass flood-protected-connection (connection)
  (last-frame :initform 0 :accessor last-frame)
  (frame-counter :initform 0 :accessor frame-counter))

(defclass profile (lichat-protocol:profile timeoutable)
  ())

(defclass server (user)
  ((users :initform (make-hash-table :test 'equal) :accessor users)
   (profiles :initform (make-hash-table :test 'equal) :accessor profiles)
   (channels :initform (make-hash-table :test 'equal) :accessor channels)
   (salt :initarg :salt :accessor salt)
   (timeout :initarg :timeout :accessor timeout))
  (:default-initargs
   :salt ""
   :timeout 120))

(defclass flood-protected-server (server)
  ((flood-frame :initarg :flood-frame :accessor flood-frame)
   (flood-limit :initarg :flood-limit :accessor flood-limit))
  (:default-initargs
   :flood-frame 30
   :flood-limit 30))

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
  (setf (gethash (coerce-channelname name) (channels server)) channel))

(defmethod remove-channel (name (server server))
  (remhash (coerce-channelname name) (channels server)))

(defmethod make-channel ((server server) &rest initargs)
  (apply #'make-instance 'channel initargs))
