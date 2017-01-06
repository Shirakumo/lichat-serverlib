#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat-serverlib
  (:nicknames #:org.shirakumo.lichat.serverlib)
  (:use #:cl)
  (:export
   #:server
   #:hostname
   #:users
   #:profiles
   #:channels
   #:connection
   #:server
   #:coerce-username
   #:coerce-channelname
   #:find-user
   #:remove-user
   #:find-profile
   #:remove-profile
   #:find-channel
   #:remove-channel
   #:make-channel
   #:join
   #:leave
   #:send!
   #:send
   #:process
   #:init-connection
   #:teardown-connection))
