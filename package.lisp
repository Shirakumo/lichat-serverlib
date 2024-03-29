(in-package #:cl-user)
(defpackage #:lichat-serverlib
  (:nicknames #:org.shirakumo.lichat.serverlib)
  (:use #:cl)
  ;; connection-maintenance.lisp
  (:export
   #:failure-condition
   #:failure-type
   #:failure-args
   #:fail!
   #:send!
   #:send
   #:process
   #:check-connection-timeout
   #:pass-flood-gate)
  ;; emotes.lisp
  (:export
   #:*allowed-emote-content-types*
   #:emote
   #:add-emote
   #:add-emotes
   #:remove-emote)
  ;; server-objects.lisp
  (:export
   #:timeoutable
   #:timeout
   #:start-timeout
   #:reset-timeout
   #:alive-p
   #:channel
   #:backlogged-channel
   #:backlog
   #:join-times
   #:user
   #:connection
   #:server
   #:last-update
   #:read-limit
   #:flood-protected-connection
   #:last-frame
   #:frame-count
   #:profile
   #:password
   #:password-valid-p
   #:server
   #:users
   #:profiles
   #:channels
   #:idle-timeout
   #:allowed-content-types
   #:default-read-limit
   #:flood-protected-server
   #:flood-frame
   #:flood-limit
   #:make-connection
   #:coerce-username
   #:coerce-channelname
   #:find-user
   #:remove-user
   #:make-user
   #:list-users
   #:find-profile
   #:remove-profile
   #:make-profile
   #:list-profiles
   #:find-channel
   #:remove-channel
   #:make-channel
   #:list-channels)
  ;; server-operations.lisp
  (:export
   #:prep-perms
   #:rule-permitted
   #:permitted
   #:create
   #:join
   #:leave
   #:register
   #:init-connection
   #:teardown-connection
   #:check-permitted
   #:check-from
   #:check-target
   #:check-channel
   #:check-channelname)
  ;; update-handlers.lisp
  (:export
   #:define-update-handler))
