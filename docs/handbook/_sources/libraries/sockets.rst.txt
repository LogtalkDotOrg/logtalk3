.. _library_sockets:

``sockets``
===========

Portable abstraction over TCP sockets. Provides a high-level API for
client and server socket operations that works across all supported
backend Prolog systems: ECLiPSe, GNU Prolog, SICStus Prolog, and
SWI-Prolog.

Design rationale
----------------

Different Prolog systems provide socket functionality at different
abstraction levels. Some backends (notably SICStus Prolog) do not
provide low-level socket creation predicates that can be separated from
binding or connecting. This library therefore provides a higher-level
API with predicates ``client_open/4`` and ``server_open/3`` that
abstracts over these differences.

API documentation
-----------------

Open the
`../../apis/library_index.html#sockets <../../apis/library_index.html#sockets>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(sockets(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(sockets(tester)).

Usage
-----

Creating a client connection
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To connect to a server:

::

   ?- socket::client_open(localhost, 8080, Stream, []).

The ``Stream`` returned will be either a single stream handle or a
compound term ``stream_pair(Input, Output)`` depending on the backend
Prolog system. Use ``socket::close/1`` to close the stream when done.

Creating a server
~~~~~~~~~~~~~~~~~

To create a server that accepts connections:

::

   ?- socket::server_open(8080, ServerSocket, []),
      socket::server_accept(ServerSocket, ClientStream, ClientInfo, []),
      % ... communicate with client using ClientStream ...
      socket::close(ClientStream),
      socket::server_close(ServerSocket).

If the port is passed as a variable to ``server_open/3``, an available
port will be selected and unified with the variable.

Getting the current host name
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   ?- socket::current_host(Host).

API Summary
-----------

- ``client_open(+Host, +Port, -Stream, +Options)`` - Connect to a server
- ``server_open(?Port, -ServerSocket, +Options)`` - Create a server
  socket
- ``server_accept(+ServerSocket, -Stream, -ClientInfo, +Options)`` -
  Accept connection
- ``server_close(+ServerSocket)`` - Close a server socket
- ``close(+Stream)`` - Close a client or accepted connection stream
- ``current_host(-Host)`` - Get the current machine's hostname

Backend-specific notes
----------------------

Stream representation
~~~~~~~~~~~~~~~~~~~~~

Different backends represent socket streams differently:

- **SICStus Prolog and ECLiPSe**: Return a single bidirectional stream.
- **GNU Prolog, SWI-Prolog**: Return ``stream_pair(Input, Output)`` with
  separate streams for reading and writing.

Binary mode
~~~~~~~~~~~

The library automatically sets streams to binary mode where applicable
to ensure consistent behavior across backends. This is important for
sending and receiving raw bytes.

Known issues
------------

Recent versions of macOS seem to disable the mapping of ``localhost`` to
``127.0.0.1``. This issue may prevent some functionality from working.
This can be fixed either by editing the ``/etc/hosts`` file or by using
``'127.0.0.1'`` as the host argument instead of ``localhost``.
