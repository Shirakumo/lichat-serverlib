#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.server)

(defclass server (lichat-protocol:user)
  ((users :initform (make-hash-table :test 'equal) :accessor users)
   (profiles :initform (make-hash-table :test 'equal) :accessor profiles)
   (channels :initform (make-hash-table :test 'equal) :accessor channels)
   (salt :initarg :salt :accessor salt))
  (:default-initargs
   :salt ""))

(defclass connection (lichat-protocol:connection)
  ((server :initarg :server :accessor server)))

(defmethod initialize-instance :after ((server server) &key name)
  (check-type name lichat-protocol:username)
  (setf (find-channel name server)
        (make-channel name name server))
  (setf (find-user name server) server)
  (join (find-channel name server) server))

(defun coerce-username (name-ish)
  (etypecase name-ish
    (lichat-protocol:update
     (string-downcase (lichat-protocol:from name-ish)))
    (string (string-downcase name-ish))))

(defun coerce-channelname (name-ish)
  (etypecase name-ish
    (lichat-protocol:channel-update
     (string-downcase (lichat-protocol:channel name-ish)))
    (string (string-downcase name-ish))))

(defmethod find-user (name server)
  (gethash (coerce-username name) (users server)))

(defmethod (setf find-user) (user name server)
  (setf (gethash (coerce-username name) (users server)) user))

(defmethod remove-user (name server)
  (remhash (coerce-username name) (users server)))

(defmethod find-profile (name server)
  (gethash (coerce-username name) (profiles server)))

(defmethod (setf find-profile) (profile name server)
  (setf (gethash (coerce-username name) (profiles server)) profile))

(defmethod remove-profile (name server)
  (remhash (coerce-username name) (profiles server)))

(defmethod find-channel (name server)
  (gethash (coerce-channelname name) (channels server)))

(defmethod (setf find-channel) (channel name server)
  (setf (gethash (coerce-channelname name) (channels server)) channel))

(defmethod remove-channel (name server)
  (remhash (coerce-channelname name) (channels server)))

(defun prep-perms (registrant perms)
  (sublis `((:registrant . ,registrant)) perms))

(defmethod make-channel (registrant name server)
  (cond ((find-channel name server)
         (error "A channel with that name already exists."))
        ((or (not name) (string= name ""))
         (make-instance 'lichat-protocol:channel
                        :name (format NIL "@~a" (lichat-protocol:next-id))
                        :permissions (prep-perms registrant lichat-protocol:*default-anonymous-channel-permissions*)
                        :lifetime 0))
        ((string= name (lichat-protocol:name server))
         (make-instance 'lichat-protocol:channel
                        :name name
                        :permissions (prep-perms registrant lichat-protocol:*default-primary-channel-permissions*)
                        :lifetime most-positive-fixnum))
        ((typep name 'lichat-protocol:channelname)
         (make-instance 'lichat-protocol:channel
                        :name name
                        :permissions (prep-perms registrant lichat-protocol:*default-regular-channel-permissions*)
                        :lifetime lichat-protocol:*default-channel-lifetime*))
        (T
         (error "Invalid channel name ~s" name))))

(defmethod join ((channel lichat-protocol:channel) (user lichat-protocol:user))
  (when (find channel (lichat-protocol:channels user))
    (error "Already in channel."))
  (push user (lichat-protocol:users channel))
  (push channel (lichat-protocol:channels user))
  (send! channel 'join :from (lichat-protocol:name user)
                       :channel (lichat-protocol:name channel)))

(defmethod leave ((channel lichat-protocol:channel) (user lichat-protocol:user))
  (unless (find channel (lichat-protocol:channels user))
    (error "Not in channel."))
  (send! channel 'leave :from (lichat-protocol:name user)
                        :channel (lichat-protocol:name channel))
  (setf (lichat-protocol:users channel) (remove user (lichat-protocol:users channel)))
  (setf (lichat-protocol:channels user) (remove channel (lichat-protocol:channels user))))

(define-condition failure-condition (error)
  ((failure-type :initarg :type :reader failure-type)
   (failure-args :initarg :args :reader failure-args))
  (:report (lambda (c s) (format s "Failure: (~a ~{~s~^ ~})"
                                 (failure-type c) (failure-args c)))))

(defun fail! (type-ish &rest initargs)
  (error 'failure-condition :type type-ish :args initargs))

(defun send! (target type-ish &rest initargs)
  (unless (getf initargs :from)
    (push "STUB!" initargs)
    (push :from initargs))
  (send (apply #'make-instance
               (find-symbol (symbol-name type-ish) :lichat-protocol)
               initargs)
        target))

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
      (lichat-protocol:reader-condition (err)
        (send! connection 'malformed-update
               :text (princ-to-string err))))
    (when (typep message 'lichat-protocol:wire-object)
      (handler-case
          (process connection message)
        (failure-condition (err)
          (apply #'send! connection (failure-type err) (failure-args err)))
        (lichat-protocol:protocol-condition (err)
          (send! connection 'failure :text (format NIL "Internal error: ~a" err)))))
    message))

(defmethod process ((connection connection) (update lichat-protocol:connect))
  (let ((server (server connection)))
    (cond ((string/= (lichat-protocol:version update)
                     (lichat-protocol:protocol-version))
           (fail! 'lichat-protocol:incompatible-version
                  :update-id (lichat-protocol:id update)
                  :compatible-versions (list (lichat-protocol:protocol-version))
                  :text (format NIL "~a is not supported." (lichat-protocol:version update))))
          ((lichat-protocol:password update)
           (let ((profile (find-profile update server)))
             (cond ((not profile)
                    (fail! 'lichat-protocol:no-such-profile
                           :update-id (lichat-protocol:id update)
                           :text (format NIL "~a is not registered." (lichat-protocol:from update))))
                   ((string/= (cryptos:pbkdf2-hash (lichat-protocol:password update)
                                                   (salt server))
                              (lichat-protocol:password profile))
                    (fail! 'lichat-protocol:invalid-password
                           :update-id (lichat-protocol:id update)
                           :text (format NIL "Your password is wrong.")))
                   (T
                    (init-connection connection (lichat-protocol:from update))))))
          ((find-user update server)
           (fail! 'lichat-protocol:username-taken
                  :update-id (lichat-protocol:id update)
                  :text (format NIL "The name ~s is already in use." (lichat-protocol:from update))))
          ((find-profile update server)
           (fail! 'lichat-protocol:username-taken
                  :update-id (lichat-protocol:id update)
                  :text (format NIL "The name ~s is registered." (lichat-protocol:from update))))
          (T
           (init-connection connection (lichat-protocol:from update))))))

(defmethod init-connection ((connection connection) username)
  (let* ((server (server connection))
         (user (find-user username server)))
    (cond (user
           (setf (lichat-protocol:user connection) user)
           (dolist (channel (channels user))
             (send! connection 'join :from (lichat-protocol:name user)
                                     :channel (lichat-protocol:name channel))))
          (T
           (setf user (make-instance 'lichat-protocol:user :name username))
           (setf (find-user username server) user)
           (join (find-channel (lichat-protocol:name server) server) user)))
    (push connection (lichat-protocol:connections user))
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
        (leave channel user)))))

(defmethod process ((connection connection) (update lichat-protocol:disconnect))
  (teardown-connection connection)
  (send! connection 'disconnect))

;;; FIXME FOR INVALID FROM FIELDS.
;; the protocol has FROM fields in order to both
;; identify the source on the receiving side, and
;; to act in stead of someone else on the sending
;; side. However, this must be checked and properly
;; treated in the processing functions, which it
;; currently is not.

(defmethod process ((connection connection) (update lichat-protocol:message))
  (let ((user (find-user (lichat-protocol:from update) (server connection)))
        (channel (find-channel (lichat-protocol:channel update) (server connection))))
    (unless channel
      (error "No such channel."))
    (unless (find channel (lichat-protocol:channels user))
      (error "Not in channel."))
    ;; FIXME: Check FROM
    (send update channel)))

(defmethod process ((connection connection) (update lichat-protocol:join))
  (let ((user (find-user (lichat-protocol:from update) (server connection)))
        (channel (find-channel (lichat-protocol:channel update) (server connection))))
    (join channel user)))

(defmethod process ((connection connection) (update lichat-protocol:leave))
  (let ((user (find-user (lichat-protocol:from update) (server connection)))
        (channel (find-channel (lichat-protocol:channel update) (server connection))))
    (leave channel user)))

(defmethod process ((connection connection) (ping lichat-protocol:ping))
  ;; Do something with the timing.
  (send (make-instance 'lichat-protocol:pong :clock (get-universal-time)) connection))
