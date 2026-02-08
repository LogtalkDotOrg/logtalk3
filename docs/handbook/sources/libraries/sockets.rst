.. _library_sockets:

``sockets``
===========

Portable abstraction over TCP sockets. Provides a high-level API for
client and server socket operations that works across all supported
backend Prolog systems: ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
and Trealla Prolog.

Design rationale
----------------

Different Prolog systems provide socket functionality at different
abstraction levels. Some backends (notably SICStus Prolog and Trealla
Prolog) do not provide low-level socket creation predicates that can be
separated from binding or connecting. This library therefore provides a
higher-level API with predicates ``client_open/4-5`` and
``server_open/2-3`` that abstracts over these differences.

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

To connect to a server using default options:

::

   ?- socket::client_open(localhost, 8080, Input, Output).

The predicates ``client_open/4-5`` and ``server_accept/4-5`` return
separate input and output streams. For backends where the same stream is
used for bidirectional communication (SICStus Prolog and ECLiPSe), the
same stream handle is returned in both arguments. Use
``socket::close/2`` to close both streams when done.

Creating a server
~~~~~~~~~~~~~~~~~

To create a server that accepts connections using default options:

::

   ?- socket::server_open(8080, ServerSocket),
      socket::server_accept(ServerSocket, Input, Output, ClientInfo),
      % ... communicate with client using Input and Output ...
      socket::close(Input, Output),
      socket::server_close(ServerSocket).

If the port is passed as a variable to ``server_open/2-3``, an available
port will be selected and unified with the variable.

Getting the current host name
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   ?- socket::current_host(Host).

API Summary
-----------

- ``client_open(+Host, +Port, -InputStream, -OutputStream, +Options)`` -
  Connect to a server
- ``client_open(+Host, +Port, -InputStream, -OutputStream)`` - Connect
  to a server using default options
- ``server_open(?Port, -ServerSocket, +Options)`` - Create a server
  socket
- ``server_open(?Port, -ServerSocket)`` - Create a server socket using
  default options
- ``server_accept(+ServerSocket, -InputStream, -OutputStream, -ClientInfo, +Options)``
  - Accept connection
- ``server_accept(+ServerSocket, -InputStream, -OutputStream, -ClientInfo)``
  - Accept connection using default options
- ``server_close(+ServerSocket)`` - Close a server socket
- ``close(+InputStream, +OutputStream)`` - Close a client or accepted
  connection
- ``current_host(-Host)`` - Get the current machine's hostname

Backend-specific notes
----------------------

Stream representation
~~~~~~~~~~~~~~~~~~~~~

The library provides separate input and output stream arguments in
``client_open/4-5`` and ``server_accept/4-5``. For backends where the
same stream is used for bidirectional communication (ECLiPSe, SICStus
Prolog, and Trealla Prolog), the same stream handle is returned in both
the input and output arguments. For backends that use separate streams
(GNU Prolog and SWI-Prolog), separate stream handles are returned.

Binary and text modes
~~~~~~~~~~~~~~~~~~~~~

The library automatically sets streams by default to binary mode (e.g.,
for sending and receiving raw bytes). An option is supported for opening
streams in text mode.

Known issues
------------

Recent versions of macOS seem to disable the mapping of ``localhost`` to
``127.0.0.1``. This issue may prevent some functionality from working.
This can be fixed either by editing the ``/etc/hosts`` file or by using
``'127.0.0.1'`` as the host argument instead of ``localhost``.
