.. _library_http_websocket_service:

``http_websocket_service``
==========================

This library provides higher-level callback-driven WebSocket session
loops on top of the ``http_websocket_session`` state-machine library and
upgraded WebSocket connections provided by a selected
``http_transport_protocol`` implementation. It adds connection lifecycle
ownership, optional auto-pong, keepalive, and idle-timeout policies,
client- and server-side conveniences that collapse the opening handshake
and the session loop into a single call, and a registry-backed broadcast
helper for multi-session servers.

By default, the convenience objects use the ``http_socket_transport``
transport. The parametric
``http_websocket_service(_HTTPSocket_, _Role_, _TextRepresentation_)``,
``http_websocket_client_service(_HTTPSocket_)``, and
``http_websocket_server_service(_HTTPSocket_)`` objects can also use
alternative ``http_transport_protocol`` implementations such as
``http_process_transport``.

This library can be used with backend Prolog systems that support
unbound integer arithmetic and the ``sockets`` library: ECLiPSe, SICStus
Prolog, SWI-Prolog, Trealla Prolog, and XVM. On backends that don't
support threads, only a subset of the library is usable; see the
"Backends without thread support" section below for the details.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_websocket_service <../../apis/library_index.html#http_websocket_service>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_websocket_service(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_websocket_service(tester)).

Current scope
-------------

The current implementation provides:

- ``run_session/3-4`` for higher-level callback-driven loops over
  upgraded WebSocket connection handles produced by the selected
  transport parameterization that keep reading until the close handshake
  completes or the peer closes the stream and then close the upgraded
  connection automatically, with optional auto-pong, keepalive,
  idle-timeout, and maximum payload length policies.
- ``open/4-5`` in the ``http_websocket_client_service`` object as a
  client-side convenience that combines
  ``http_client::open_websocket/4``, optional initial outbound messages,
  and the higher-level session loop. The accepted URL schemes depend on
  the selected transport parameterization, e.g. ``ws://`` for
  ``http_socket_transport`` and both ``ws://`` and ``wss://`` for
  ``http_process_transport``.
- ``serve_once/6-7`` in the ``http_websocket_server_service`` object as
  a server-side convenience that combines the selected transport
  parameterization ``serve_websocket_once/5`` predicate with the
  higher-level session loop.
- ``serve_until_shutdown/5-6`` and ``request_shutdown/1`` in the
  ``http_websocket_server_service`` object for registry-backed
  multi-session servers with queued broadcast delivery.
- ``open/1``, ``close/1``, ``register/2``, ``unregister/2``, ``send/3``,
  ``broadcast/2``, ``broadcast_except/3``, and ``take_pending/3`` in the
  ``http_websocket_service_registry`` object for managing active
  sessions and queued outbound messages.

Higher-level session loop
-------------------------

The ``run_session/3-4`` predicates accept an upgraded WebSocket
connection handle produced by the selected transport parameterization
together with a handler object that implements the
``http_websocket_service_handler_protocol`` protocol:

::

   handle(Message, Replies)

For each received normalized message, the handler returns a list of
normalized reply messages to write before the next read. The loop:

- starts from the initial session state,
- reuses the stateful session orchestration including automatic close
  replies and optional auto-pong policy,
- writes the handler replies using the stateful session write
  predicates,
- continues until the close handshake reaches
  ``closed(SentPayload, ReceivedPayload)`` or the peer closes the
  stream, and
- closes the upgraded connection automatically before returning.

The ``run_session/4`` predicate accepts these loop options:

- ``auto_pong(on)`` or ``auto_pong(off)``
- ``keepalive_interval(Seconds)``
- ``idle_timeout(Seconds)``
- ``max_payload_length(Bytes)``

When ``keepalive_interval(Seconds)`` is used, the loop sends empty ping
messages after ``Seconds`` of inbound silence. When
``idle_timeout(Seconds)`` is used, the loop sends
``message(close, status(1001, idle_timeout))`` after ``Seconds`` of
inbound silence and then waits one more idle interval for the peer close
reply. These timed options require backend thread support. The keepalive
and idle-timeout policies are tracked using absolute wall-clock
deadlines, making the loop robust to scheduling jitter and slow reads.

A small server-side session handler can look like:

::

   :- object(chat_session_handler,
       implements(http_websocket_service_handler_protocol)).
       
       handle(message(text, ping), [message(text, pong)]) :-
           !.
       handle(message(text, Text), [message(text, Text)]) :-
           !.
       handle(message(close, _Payload), []) :-
           !.
       handle(_Message, []).

   :- end_object.

After a successful opening handshake returns an upgraded ``Connection``
handle, run the loop with either:

::

   | ?- http_websocket_server_service::run_session(Connection, chat_session_handler, FinalState).

or:

::

   | ?- http_websocket_server_service::run_session(Connection, chat_session_handler, FinalState, [auto_pong(on)]).

or with timed loop policies:

::

   | ?- http_websocket_server_service::run_session(Connection, chat_session_handler, FinalState, [auto_pong(on), keepalive_interval(30), idle_timeout(120)]).

The ``run_session/3-4`` predicates take ownership of the upgraded
connection, so no explicit transport ``close_connection/1`` call is
needed afterwards.

For a client that wants to collapse the opening handshake and the
callback loop into a single call, the ``http_websocket_client_service``
object provides:

::

   open(URL, SessionHandler, Response, FinalState)

or:

::

   open(URL, SessionHandler, Response, FinalState, [protocols([chat]), initial_messages([message(text, hello)]), auto_pong(on)])

The accepted WebSocket URL schemes depend on the selected transport
parameterization. For example, the default
``http_websocket_client_service`` object uses ``http_socket_transport``
and therefore accepts ``ws://`` URLs, while
``http_websocket_client_service(http_process_transport)`` also accepts
``wss://`` URLs.

The ``initial_messages(Messages)`` option writes the given list of
normalized outbound messages immediately after the handshake and before
the first session read. The ``open/4-5`` predicates also accept the
``keepalive_interval/1``, ``idle_timeout/1``, and
``max_payload_length/1`` session-loop options. They take ownership of
the upgraded connection and close it automatically when the session loop
finishes.

For a server that wants to collapse the opening handshake and the
callback loop into a single call, the ``http_websocket_server_service``
object provides:

::

   serve_once(Listener, HandshakeHandler, SessionHandler, Response, FinalState, ClientInfo)

or:

::

   serve_once(Listener, HandshakeHandler, SessionHandler, Response, FinalState, ClientInfo, [auto_pong(on)])

These predicates accept one incoming socket connection, serve one valid
opening handshake via the given HTTP handler, run the session loop via
the given session handler, and then close the upgraded connection
automatically. They also accept the ``keepalive_interval/1``,
``idle_timeout/1``, and ``max_payload_length/1`` session-loop options.

For server-side helpers, the ``Listener`` must come from the same
selected transport parameterization as the service object, e.g.
``http_websocket_server_service(http_process_transport)`` must be paired
with a listener opened by ``http_process_transport::open_listener/4``.

Registry-backed server helper
-----------------------------

For multi-session servers that need queued outbound delivery and
broadcast, the ``http_websocket_server_service`` object also provides:

::

   serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control)

or:

::

   serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, [auto_pong(on), keepalive_interval(30), idle_timeout(120)])

This helper accepts opening handshakes until:

- ``http_websocket_server_service::request_shutdown(Control)`` is
  called, or
- the listener is closed externally.

Each accepted upgraded connection is registered in ``Registry`` and
handled in its own session worker. The registry queues outbound messages
so that each worker only writes to its own connection.

When used with ``serve_until_shutdown/5-6``, the session handler may
still return plain normalized reply messages, but it may also return
these action wrappers:

- ``reply(Message)``
- ``broadcast(Message)``
- ``broadcast_others(Message)``

For example, a simple chat-style broadcast handler can look like:

::

   :- object(chat_broadcast_handler,
       implements(http_websocket_service_handler_protocol)).
       
       handle(message(text, Text), [broadcast_others(message(text, Text))]) :-
           !.
       handle(message(close, _Payload), []) :-
           !.
       handle(_Message, []).

   :- end_object.

The ``http_websocket_service_registry`` object provides the registry
handle and queue-management predicates used by this helper. The
registry-backed server helper requires backend thread support.

Backends without thread support
-------------------------------

The library loads on any backend that supports the ``sockets`` library
and unbounded integer arithmetic, but the available functionality
depends on whether the backend supports threads.

Available without thread support:

- The ``run_session/3-4``, ``http_websocket_client_service::open/4-5``,
  and ``http_websocket_server_service::serve_once/6-7`` predicates when
  called without the ``keepalive_interval/1`` and ``idle_timeout/1``
  options. In this case, the session loop reads from the connection
  using plain blocking reads and no background reader is required.
- The ``http_websocket_service_registry`` data predicates (``open/1``,
  ``close/1``, ``register/2``, ``unregister/2``, ``send/3``,
  ``broadcast/2``, ``broadcast_except/3``, and ``take_pending/3``),
  which are plain database operations.

Requiring thread support:

- The ``keepalive_interval/1`` and ``idle_timeout/1`` loop options.
  These policies need a timed session loop that polls a background
  reader, which is implemented using threads. When used on a backend
  without thread support, the loop predicates throw a
  ``not_available(http_websocket_service_timing)`` error before reading
  or writing any frames.
- Registry-driven session loops (i.e., sessions run on behalf of the
  registry-backed server helper). These impose a polling interval so
  that messages and broadcasts queued by other sessions can be flushed
  promptly, which also requires the timed session loop and thus thread
  support.
- The ``serve_until_shutdown/5-6`` and ``request_shutdown/1``
  predicates, which run each accepted session in its own worker thread.
  On backends without thread support, these predicates throw a
  ``not_available(http_websocket_server_service_registry)`` error.

In all cases, unsupported usage fails fast with a ``not_available/1``
error at validation time instead of hanging or silently misbehaving.

Current workflow
----------------

- Use the ``http_websocket_session`` library directly when you need
  stateful reads, close-state tracking, or role-aware writes on binary
  streams that you manage yourself.
- When a single callback object should own the connection lifecycle,
  pass the upgraded handle to ``run_session/3-4`` and let the service
  layer close it when the close handshake completes.
- When client-side code should own the full upgraded connection
  lifecycle, call ``http_websocket_client_service::open/4-5`` and let
  the service layer handle the opening handshake, any configured initial
  outbound messages, and the final connection close.
- When the client transport must support secure ``wss://`` URLs, use the
  a TLS-capable parameterization such as ``http_process_transport``.
- When server-side code already owns a listener and wants a single entry
  point for handshake plus session execution, call
  ``http_websocket_server_service::serve_once/6-7``.
- When server-side code uses a non-default transport such as
  ``http_process_transport``, use the matching parametric service object
  and open the listener with that same transport parameterization.
- When server-side code needs multiple active sessions plus queued
  broadcast delivery, create a registry with
  ``http_websocket_service_registry::open/1`` and call
  ``http_websocket_server_service::serve_until_shutdown/5-6``.

Current limitations
-------------------

- The ``run_session/3-4`` and ``serve_once/6-7`` helpers are
  synchronous, single-connection callback loops.
- On backends without thread support, the timed loop options
  ``keepalive_interval/1`` and ``idle_timeout/1``, registry-driven
  session loops, and the ``serve_until_shutdown/5-6`` helper are not
  available and throw ``not_available/1`` errors; see the "Backends
  without thread support" section above.
