.. _library_http_socket:

``http_socket``
===============

The ``http_socket`` library is the first transport-aware layer on top of
the ``http_client_core``, ``http_server``, and ``sockets`` libraries. It
uses TCP sockets for connection management while delegating HTTP message
framing and connection semantics to the existing stream core and server
layers.

This library can be used with backend Prolog systems that supports the
``sockets`` library: ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
Trealla Prolog, and XVM.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_socket <../../apis/library_index.html#http_socket>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_socket(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_socket(tester)).

Current scope
-------------

The current implementation provides eighteen predicates:

- ``open_listener/4`` opens a TCP listener using the ``sockets`` library
  and returns the listener handle.
- ``close_listener/1`` closes a listener previously opened with
  ``open_listener/4``.
- ``open_connection/4`` opens a reusable client connection using the
  ``sockets`` library and returns a connection handle.
- ``close_connection/1`` closes a reusable client connection previously
  opened with ``open_connection/4``.
- ``connection_streams/3`` returns the binary input and output streams
  carried by a reusable client connection handle or by an upgraded
  WebSocket connection handle.
- ``open_connection_pool/4`` opens a managed reusable connection pool
  for a given host and port.
- ``close_connection_pool/1`` closes a managed reusable connection pool
  and all currently available pooled connections.
- ``connection_pool_stats/2`` returns pool statistics as
  ``stats(Available, InUse, Total, MinSize, MaxSize)``.
- ``exchange/3`` performs one HTTP exchange on an open reusable client
  connection or by temporarily acquiring a pooled reusable client
  connection.
- ``exchange_connection/3`` performs a sequence of HTTP exchanges on an
  open reusable client connection or by temporarily acquiring a pooled
  reusable client connection.
- ``exchange/4`` opens a client socket, performs one HTTP exchange, and
  closes the connection.
- ``exchange_connection/4`` opens a client socket, performs a sequence
  of HTTP exchanges on a persistent connection, and closes the
  connection.
- ``serve_once/3`` accepts one client socket connection, serves that
  connection using the ``http_server`` library, and closes the accepted
  streams.
- ``serve_websocket_once/5`` accepts one client socket connection,
  serves one WebSocket opening handshake using the ``http_server``
  library, and returns an upgraded connection handle that remains open
  on success.
- ``serve_listener/4`` accepts and serves a bounded number of client
  connections on the same listener.
- ``serve_listener/5`` accepts and serves a bounded number of client
  connections using configurable shutdown and worker options.
- ``serve_until_shutdown/4`` accepts and serves client connections until
  an external shutdown request is issued for the associated control
  term.
- ``request_shutdown/1`` stops an open-ended ``serve_until_shutdown/4``
  loop, closes its listener, and lets active workers finish before
  returning.

This layer is intentionally thin. It delegates request and response
framing to ``http_client_core`` and ``http_server``, and delegates
socket creation and teardown to the ``sockets`` library.

Reusable client connections are represented by
``http_connection(Host, Port, ...)`` handle terms that carry normalized
endpoint metadata and can be passed to ``exchange/3``,
``exchange_connection/3``, ``connection_streams/3``, and
``close_connection/1``.

Accepted WebSocket upgrades are represented by
``http_websocket_connection(ClientInfo, Input, Output)`` handle terms
that can be passed to ``connection_streams/3`` and
``close_connection/1``.

Managed connection pools are represented by
``http_connection_pool(Host, Port, ...)`` handle terms that carry
normalized endpoint metadata and can be passed to ``exchange/3``,
``exchange_connection/3``, ``close_connection_pool/1``, and
``connection_pool_stats/2``.

The ``open_connection_pool/4`` predicate supports the following options:

- ``min_size(N)`` pre-opens ``N`` reusable connections when the pool is
  created. The default is ``0``.
- ``max_size(N)`` limits the pool to at most ``N`` managed connections.
  The default is ``10``.
- ``connection_options(Options)`` passes socket options through to
  ``open_connection/4`` when creating pooled connections.

Pool exchanges fail immediately with
``resource_error(http_socket_connection_pool)`` when no pooled
connection is available and the pool is already at its maximum size.

The ``serve_listener/5`` predicate currently supports two option
families:

- ``shutdown(keep_open)`` leaves the listener open after the bounded
  serving loop. This is the default.
- ``shutdown(close)`` closes the listener when the bounded serving loop
  finishes or aborts.
- ``workers(serial)`` serves accepted connections sequentially in the
  caller thread. This is the default.
- ``workers(per_connection)`` spawns one worker thread per accepted
  connection and waits for all workers before returning.
- ``workers(pool(N))`` serves accepted connections in batches of up to
  ``N`` worker threads, waiting for each batch to finish before
  accepting the next batch.
- ``workers(pool(N, rolling))`` keeps up to ``N`` worker threads active
  and accepts the next connection as soon as one worker finishes.

The ``serve_until_shutdown/4`` predicate supports the ``workers/1``
option family:

- ``workers(serial)`` serves accepted connections sequentially in the
  caller thread. This is the default.
- ``workers(per_connection)`` spawns one worker thread per accepted
  connection and waits for active workers when shutdown is requested.
- ``workers(pool(N))`` serves accepted connections using at most ``N``
  concurrent worker threads, waiting for worker completion notifications
  before accepting additional connections.
- ``workers(pool(N, rolling))`` is accepted as an explicit alias for the
  rolling fixed-size worker-pool policy used by ``workers(pool(N))``.

Open-ended serving loops should use a fresh non-variable control term
for each call to ``serve_until_shutdown/4``.

WebSocket handshake workflow
----------------------------

The current WebSocket transport workflow is intentionally limited to the
opening handshake handoff:

- Client-side code can use ``http_client::open_websocket/4`` to obtain a
  reusable upgraded connection handle.
- Server-side code can use ``serve_websocket_once/5`` with a handler
  that builds a valid ``101 Switching Protocols`` response, typically
  via ``http_server::accept_websocket/3``.
- When the opening handshake succeeds, the returned upgraded connection
  remains open and its streams are available through
  ``connection_streams/3`` for use with ``http_websocket::read_frame/2``
  and ``http_websocket::write_frame/2`` or, at the next abstraction
  level, with ``http_websocket_messages::read_message/2`` and
  ``http_websocket_messages::write_message/2``, or with the stateful
  ``http_websocket_session`` layer when interleaved control frames and
  role-aware masking policies matter, including automatic close replies,
  optional automatic pong replies, the higher-level ``run_session/3-4``
  callback loop that takes ownership of the upgraded connection and
  closes it automatically after the close handshake completes, the
  client-side ``http_websocket_client_session::open/4-5`` convenience,
  and the server-side ``http_websocket_server_session::serve_once/6-7``
  convenience.
- Rejected or malformed handshakes are written to the peer and then
  reported as errors by ``serve_websocket_once/5``; the accepted streams
  are closed in that case.

Current limitations
-------------------

- Availability depends on the supported backends of the ``sockets``
  library.
- The library does not provide TLS or URL-based request helpers.
- The WebSocket transport helper is limited to the opening handshake and
  upgraded connection handle management. It does not provide frame
  parsing, message I/O, or higher-level session handling.
- The ``workers(per_connection)`` and ``workers(pool(N))`` options
  depend on backend thread support.
- Server-side helpers do not provide supervision trees.
