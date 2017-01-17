## About Lichat-Serverlib
This is an implementation of the server-side protocol part of the [Lichat protocol](https://shirakumo.org/projects/lichat-protocol). It provides an extensible, transport-agnostic implementation that can be used to base a functional server on. All that the real server needs to take care of is the underlying connection establishment and reading.

You do not need this library if you are looking to implement a client. You don't *need* it if you're looking to implement a server either, but it will take care of a large part of the implementation tasks for you if you do use it.

## Implementing an Overarching Server
In order to implement an actual server, you will want to do the following:

* Subclass at least `server` and `connection`.
* Provide functions to start a listener on a server instance.
* For each client that connects, create a `connection` object.
* Provide functions to repeatedly read updates from a `connection` and send them to `process`.
* Supply a  `close-connection` restart that closes the connection to the client around the `process` calls.
* Provide a primary method for `send` specialised on your `connection` subclass that handles the actual wire transmission of an update.
* Handle potential synchronisation or mutual-exclusion issues to users, channels, profiles, and connections on the server if your server is multi-threaded.

For a simple illustrative example, see the [lichat-tcp-server](https://shirakumo.org/projects/lichat-tcp-server).

## Also See

* [lichat-protocol](https://shirakumo.org/projects/lichat-protocol) The Lichat protocol specification.
* [lichat-tcp-server](https://shirakumo.org/projects/lichat-tcp-server) A basic, threaded, TCP-based implementation of a Lichat server.
* [lichat-tcp-client](https://shirakumo.org/projects/lichat-tcp-client) A basic, threaded, TCP-based implementation of a Lichat client.
