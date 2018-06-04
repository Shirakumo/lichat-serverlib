#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.serverlib)

;; connection-maintenance.lisp
(docs:define-docs
  (type failure-condition
    "Wrapper condition to carry a failure update.

See FAILURE-TYPE
See FAILURE-ARGS
See FAIL!")

  (function failure-type
    "The type of the failure update that should be constructed to relay this error to the client.

See FAILURE-CONDITION")

  (function failure-args
    "The initargs of the failure update that should be constructed to relay this error to the client.

See FAILURE-CONDITION")

  (function fail!
    "Signal a FAILURE-CONDITION that carries the designated failure and sends it off to the client.

See FAILURE-CONDITION")

  (function send!
    "Convenience function to send an update to a connection.

Specifically, this will:
 * Coerce the type-ish to a protocol type by looking the symbol
   up in the lichat-protocol package.
 * Add the :FROM initarg if it does not yet exist by using the
   connection's server's name.

See SEND")

  (function send
    "Send an update to a place.

Places that are accepted:
 * CONNECTION -- Directly sends the update over the wire. Note
                 that the primary method for this must be
                 implemented by the overarching server.
 * USER       -- Relays the update to all connections of the user.
 * CHANNEL    -- Relays the update to all users in the channel.

This function has to be called in a regular interval by the
overarching server. It will take care of handling connection
timeouts and will close the connection in case of one.

See CONNECTION
See USER
See CHANNEL")

  (function pass-flood-gate
    "Updates flood information for the connection and returns whether the connection is fine or not.

See FLOOD-PROTECTED-CONNECTION
See PROCESS")

  (function process
    "Process an update source on a connection.

Sources that are accepted:
 * STREAM                 -- An update is read from the stream
                             and sent off to PROCESS again.
 * LICHAT-PROTOCOL:UPDATE -- The update is handled accordingly.

If an error of type FAILURE-CONDITION is signalled during the
evaluation of non-around methods, the encapsulated failure update
is sent to the connection. If an error of type LICHAT-PROTOCOL:
PROTOCOL-CONDITION is signalled during the evaluation of non-around
methods, an update of type LICHAT-PROTOCOL:FAILURE is sent to the
connection.

A CONTINUE restart is established around the method that can be
used to respond with a generic LICHAT-PROTOCOL:UPDATE-FAILURE
and return from the PROCESS method.

This method will also take care to handle flooding and timeout
recording for the connection.

The overarching server must establish a restart called CLOSE-
CONNECTION around the PROCESS method, or around all calls of it.
When this restart is invoked, the server must close the underlying
connection.

See LICHAT-PROTOCOL:FROM-WIRE
See LICHAT-PROTOCOL:PROTOCOL-CONDITION
See LICHAT-PROTOCOL:UPDATE-FAILURE
See FAILURE-CONDITION"))

;; server-objects.lisp
(docs:define-docs
  (type timeoutable
    "Superclass for objects that can be invalidated through a timeout.

See TIMEOUT
See START-TIMEOUT
See RESET-TIMEOUT
See ALIVE-P")

  (function timeout
    "The universal-time by which the object will be invalidated, if any.

See TIMEOUTABLE")

  (function start-timeout
    "Starts the timeout by setting the object's TIMEOUT slot to the correct time.

See TIMEOUTABLE
See LICHAT-PROTOCOL:LIFETIME")

  (function reset-timeout
    "Resets the timeout to never occur.

See TIMEOUTABLE")

  (function alive-p
    "Returns T if the object is still considered to be alive.")

  (type channel
    "A server-side channel object.

See LICHAT-PROTOCOL:CHANNEL
See TIMEOUTABLE")

  (type user
    "A server-side user object

See LICHAT-PROTOCOL:USER")

  (type connection
    "The representation of a client connection.

See SERVER
See LAST-UPDATE
See LICHAT-PROTOCOL:CONNECTION")

  (function server
    "Accessor to the connection's server.

See CONNECTION")

  (function last-update
    "Accessor to the universal-time of when the last update was processed on the connection.

See CONNECTION")

  (type flood-protected-connection
    "A connection with basic time-frame flood-protection.

See CONNECTION
See LAST-FRAME
See FRAME-COUNT")

  (function last-frame
    "Accessor to the last frame number the connection handled.

See FLOOD-PROTECTED-CONNECTION")

  (function frame-count
    "Accessor to the number of updates that were processed within the last frame.

See FLOOD-PROTECTED-CONNECTION")

  (type profile
    "A server-side profile object

See LICHAT-PROTOCOL:PROFILE
See TIMEOUTABLE")

  (type server
    "An object representation of the server.

See USER
See USERS
See PROFILES
See CHANNELS
See IDLE-TIMEOUT")

  (function users
    "Accessor to the hash-table of users that exist on the server.

See SERVER")

  (function profiles
    "Accessor to the hash-table of profiles that exist on the server.

See SERVER")

  (function channels
    "Accessor to the hash-table of channels that exist on the server.

See SERVER")

  (function idle-timeout
    "Accessor to the number of seconds a connection can be idle before it is considered timed out.

See SERVER")

  (type flood-protected-server
    "A server with basic time-frame flood-protection.

See SERVER
See FLOOD-FRAME
See FLOOD-LIMIT")

  (function flood-frame
    "Accessor to the number of seconds for which a flood frame lasts.

See FLOOD-PROTECTED-SERVER")

  (function flood-limit
    "Accessor to the number of updates that can be received within a frame before the connection is limited.

See FLOOD-PROTECTED-SERVER")

  (function make-connection
    "Construct an appropriate connection object for the server.

See CONNECTION")

  (function coerce-username
    "Attempt to coerce the name-ish to a user name.

Valid types are:
LICHAT-PROTOCOL:UPDATE
LICHAT-PROTOCOL:USER
STRING")

  (function coerce-channelname
    "Attempt to coerce the name-ish to a channel name.

Valid types are:
LICHAT-PROTOCOL:CHANNEL-UPDATE
LICHAT-PROTOCOL:CHANNEL
STRING")

  (function find-user
    "Accesses a user by the given name-ish on the server.

See COERCE-USERNAME
See USERS
See REMOVE-USER")

  (function remove-user
    "Removes the user by the given name-ish from the server.

See COERCE-USERNAME
See USERS
See FIND-USER")

  (function make-user
    "Construct an appropriate user object for the server.

See USER")

  (function list-users
    "Return a fresh list of all known user objects for the server.

See USER")

  (function find-profile
    "Accesses a profile by the given name-ish on the server.

See COERCE-USERNAME
See PROFILES
See REMOVE-PROFILE")

  (function remove-profile
    "Removes the profile by the given name-ish from the server.

See COERCE-USERNAME
See PROFILES
See FIND-PROFILE")

  (function make-profile
    "Construct an appropriate user object for the server.

See PROFILE")

  (function list-users
    "Return a fresh list of all known profile objects for the server.

See PROFILE")

  (function find-channel
    "Accesses a channel by the given name-ish on the server.

See COERCE-CHANNELNAME
See CHANNELS
See REMOVE-CHANNEL")

  (Function remove-channel
    "Removes the channel by the given name from the server.

See COERCE-CHANNELNAME
See CHANNELS
See FIND-CHANNEL")

  (function make-channel
    "Construct an appropriate channel object for the server.

See CHANNEL")

  (function list-channels
    "Return a fresh list of all known channel objects for the server.

See CHANNEL"))

;; server-operations.lisp
(docs:define-docs
  (function prep-perms
    "Prepare the list of permissions rule for the registrant.")

  (function rule-permitted
    "Checks whether the given rule allows the user of the given name access.

The rule evaluation is implemented according to §2.5")

  (function permitted
    "Returns true if the action on the channel by the user is permitted.

Action should be the class-name of an update.

See RULE-PERMITTED
See LICHAT-PROTOCOL:PERMISSIONS")

  (function create
    "Creates an appropriate channel object for the registrant on the server.

If NAME is NIL, a random ID with an @ prefix is chosen
that does not already exist on the server.
If NAME is STRING= to the name of the server, a primary
channel is created.
Otherwise a regular channel is created.

This is in accordance to §2.4

Note that this will /not/ check whether a channel of the
given name already exists and will instead just replace
it.

See PREP-PERMS
See LICHAT-PROTOCOL:CREATE
See MAKE-CHANNEL
See FIND-CHANNEL
See LICHAT-PROTOCOL:*DEFAULT-ANONYMOUS-CHANNEL-PERMISSIONS*
See LICHAT-PROTOCOL:*DEFAULT-PRIMARY-CHANNEL-PERMISSIONS*
See LICHAT-PROTOCOL:*DEFAULT-REGULAR-CHANNEL-PERMISSIONS*")

  (function join
    "Join a user to a channel.

Note that this will /not/ check whether the user is already
in the channel or not and will just join them regardless.

A JOIN update is sent to all channel inhabitants, with the
given ID, or a fresh one if none is given.

The channel's timeout is reset.

See LICHAT-PROTOCOL:JOIN
See LICHAT-PROTOCOL:NEXT-ID
See RESET-TIMEOUT")

  (function leave
    "Leave a user from a channel.

Note that this will /not/ check whether the user actually
is in the channel or not and will just leave them regardless.

A LEAVE update is sent to all channel inhabitants, with the
given ID, or a fresh one if none is given.

If no users remain, the channel's timeout is started.

If NOTIFY-SELF is NIL, the LEAVE update is sent to every
channel inhabitant other than the user leaving. Otherwise,
the update is sent to everyone including the one leaving.

See LICHAT-PROTOCOL:LEAVE
See LICHAT-PROTOCOL:NEXT-ID
See START-TIMEOUT")

  (function register
    "Register a user profile.

Note that this will /not/ check whether the associated user
actually exists on the server or not and will just create
the profile regardless.

See FIND-PROFILE
See MAKE-PROFILE
See LICHAT-PROTOCOL:REGISTER")

  (function init-connection
    "Registers the connection on the server and prepares it for use.

This corresponds to §4.1.8 to §4.1.12 with the additional
act that a related profile's timeout is reset, if such a
profile is found.

See RESET-TIMEOUT")

  (function teardown-connection
    "Removes and cleans up the connection on the server.

This corresponds to §4.3.3 to §4.3.4 with the additional
act that a related profile's timeout is started, if such a
profile is found.

See START-TIMEOUT")

  (function check-permitted
    "Checks whether the given update is permitted.

If it is not, a FAILURE-CONDITION for a LICHAT-PROTOCOL:
INSUFFICIENT-PERMISSIONS failure is signalled.

This is in accordance with §5.1.7

See PERMITTED
See LICHAT-PROTOCOL:INSUFFICIENT-PERMISSIONS")

  (function check-from
    "Checks whether the FROM field matches the update.

If it is not, a FAILURE-CONDITION for a LICHAT-PROTOCOL:
USERNAME-MISMATCH failure is signalled. If it is, the
corresponding user object is returned.

This is in accordance with §5.1.4

See LICHAT-PROTOCOL:USERNAME-MISMATCH
See FIND-USER")

  (function check-target
    "Checks whether the TARGET field is a valid user.

If it is not, a FAILURE-CONDITION for a LICHAT-PROTOCOL:
NO-SUCH-USER failure is signalled. If it is, the
corresponding user object is returned.

This is in accordance with §5.1.6

See LICHAT-PROTOCOL:NO-SUCH-USER
See FIND-USER")

  (function check-channel
    "Checks whether the CHANNEL field is a valid channel.

If the channel does not exist, a FAILURE-CONDITION for a
LICHAT-PROTOCOL:NO-SUCH-CHANNEL failure is signalled. If it
does exist, but the user is not a part of it, a FAILURE
CONDITION for a LICHAT-PROTOCOL:NOT-IN-CHANNEL failure is
signalled. Otherwise, the corresponding channel object is
returned.

This is in accordance with §5.1.5 §5.4.2 §5.4.3 §5.4.4 §5.4.5

See LICHAT-PROTOCOL:NO-SUCH-CHANNEL
See LICHAT-PROTOCOL:NOT-IN-CHANNEL
See FIND-CHANNEL")

  (function check-channelname
    "Checks whether the CHANNEL field has a valid, unused name.

If the channel name is malformed, a FAILURE-CONDITION for
a LICHAT-PROTOCOL:BAD-NAME failure is signalled. If it is
valid, but a channel of that name already exists, a FAILURE-
CONDITION for a LICHAT-PROTOCOL:CHANNELNAME-TAKEN failure is
signalled.

This is in accordance with §5.1.3 §5.3.2

See LICHAT-PROTOCOL:BAD-NAME
See LICHAT-PROTOCOL:CHANNELNAME-TAKEN
See FIND-CHANNEL
See LICHAT-PROTOCOL:CHANNELNAME-P"))

;; update-handlers.lisp
(docs:define-docs
  (function define-update-handler
    "Easy wrapper to define a handler method for an update on a connection.

UPDATE is coerced to a symbol found in the LICHAT-PROTOCOL
package.

This creates a PROCESS method.

See PROCESS"))
