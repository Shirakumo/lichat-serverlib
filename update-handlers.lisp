#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.serverlib)

(defmacro define-update-handler (type (connection update) &body body)
  `(defmethod process ((,connection connection) (,update ,(find-symbol (string type) :lichat-protocol)))
     ,@body))

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

(define-update-handler ping (connection update)
  ;; Do something with the timing.
  (send! connection 'pong
         :id (lichat-protocol:id update)))

(define-update-handler pong (connection update)
  ;; Do something with the timing.
  )

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

(define-update-handler channels (connection update)
  (let ((user (find-user (lichat-protocol:from update) (server connection))))
    (send! connection 'channels
           :id (lichat-protocol:id update)
           :channels (loop for channel being the hash-values of (channels (server connection)) 
                           when (permitted (type-of update) channel user)
                           collect (lichat-protocol:name channel)))))

(define-update-handler users (connection update)
  (let ((user (check-from connection update))
        (channel (check-channel connection update)))
    (unless (find channel (lichat-protocol:channels user))
      (check-permitted connection update))
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
    (check-permitted connection update)
    (when (find channel (lichat-protocol:channels target))
      (fail! 'lichat-protocol:already-in-channel :update-id (lichat-protocol:id update)))
    (join channel target (lichat-protocol:id update))))

(define-update-handler permissions (connection update)
  (let ((channel (check-channel connection update)))
    (check-from connection update)
    (when (lichat-protocol:permissions update)
      (check-permitted connection update)
      (setf (lichat-protocol:permissions channel)
            (lichat-protocol:permissions update)))
    (send! connection 'permissions
           :id (lichat-protocol:id update)
           :from (lichat-protocol:name (server connection))
           :channel (lichat-protocol:name channel)
           :permissions (lichat-protocol:permissions channel))))

(define-update-handler register (connection update)
  (let ((user (check-from connection update)))
    (register user (lichat-protocol:password update) (server connection))
    (send update connection)))

(define-update-handler user-info (connection update)
  (let ((target (check-target connection update)))
    (check-from connection update)
    (send! connection 'user-info
           :id (lichat-protocol:id update)
           :target (lichat-protocol:name target)
           :connections (max 1 (length (lichat-protocol:connections target)))
           :registered (if (find-profile target (server connection))
                           T NIL))))
