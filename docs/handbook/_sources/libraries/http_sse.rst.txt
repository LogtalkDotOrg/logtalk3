.. _library_http_sse:

``http_sse``
============

This library provides high-level Server-Sent Events (SSE) predicates for
opening and closing connections, for exchanging events, and for running
common client and server session loops, following the `WHATWG
Server-Sent Events
specification <https://html.spec.whatwg.org/multipage/server-sent-events.html>`__.

Unlike WebSocket, SSE is a unidirectional protocol layered directly on
top of a regular HTTP request and response: the client sends a single
``GET`` request and the server replies with a ``200``
``text/event-stream`` response whose body is never fully framed with a
``Content-Length`` or ``Transfer-Encoding`` header, but instead written
and read incrementally, event by event, for as long as the connection
stays open. Only the server sends events; the client only receives them.

By default, client predicates select a transport from the URL scheme:
``http://`` uses ``http_socket_transport`` and ``https://`` uses
``http_process_transport``. Server predicates that accept an already
open listener default to ``http_socket_transport``. Applications can
select a transport explicitly with the ``transport/1`` option when they
need lower-level control or a custom ``http_transport_protocol``
implementation.

This library can be used with backend Prolog systems that support
unbound integer arithmetic and the ``sockets`` library: ECLiPSe, SICStus
Prolog, SWI-Prolog, Trealla Prolog, and XVM.

API documentation
-----------------

Open the
`../../apis/library_index.html#http-sse <../../apis/library_index.html#http-sse>`__
link in a web browser.

Loading
-------

To load the full library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_sse(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_sse(tester)).

Current scope
-------------

The current implementation provides the following predicates:

- ``open/2-3`` for opening client SSE connections and returning opaque
  handles. The ``transport/1`` option selects the transport;
  ``transport(default)`` derives it from the URL scheme.
- ``accept/3-4`` for accepting one server-side SSE request on an open
  listener and returning an opaque handle. The listener must be created
  by the selected transport. Use ``transport(Transport)`` when accepting
  from a listener opened with a transport other than
  ``http_socket_transport``.
- ``send/2-3`` for writing one outbound event, comment, or reconnection
  time record using a handle, and ``receive/2-3`` for reading and
  dispatching the next event, following the event stream interpretation
  algorithm from the specification.
- ``close/1-2`` for best-effort closing of a handle, optionally writing
  a final record first.
- ``property/2`` for inspecting handle properties such as the response,
  the last received event id, or the current reconnection time.
- ``send_data/2``, ``send_event/3``, ``send_id_data/3``,
  ``send_comment/2``, ``send_retry/2``, ``send_json/2``,
  ``receive_json/2``, ``send_term/2``, ``receive_term/2``, and
  ``receive_data/2`` convenience predicates for common cases.
- ``open_session/4-5`` for callback-driven client sessions that
  reconnect, resuming from the last received event id, when the
  connection drops, and ``serve_once/5-6`` for callback-driven server
  sessions, both built directly on top of the direct API and the
  ``http_sse_service_handler_protocol`` protocol.

The current implementation writes SSE response bodies without a
``Content-Length`` or ``Transfer-Encoding`` header, relying instead on
the connection being closed to mark the end of the stream (a
close-delimited body, as allowed by RFC 7230 for HTTP responses).
Response status lines and headers are generated to an in-memory codes
list and then written to the connection with explicit byte-level output,
matching the approach ``http_server_core::write_response/2`` already
uses for its common case; this avoids relying on a stream sink accepting
mixed character and byte output, which is not equally well supported
across all backend Prolog compilers. Reading a peer-closed connection as
``end_of_file`` on ``receive/2`` is exercised by the test suite over
``http_socket_transport``, which propagates a closed connection promptly
as a read-side end of file condition.

The test suite includes backend-independent wire-format tests, which do
not require thread support, and live tests that exercise both
``http_socket_transport`` and ``http_process_transport`` by passing the
selected transport through the ``transport/1`` option on backends
supporting threads.

Usage
-----

For the common direct client case:

\| ?- http_sse::open('http://127.0.0.1:8080/events', SSE, []),
http_sse::receive(SSE, Event), http_sse::close(SSE).

For the common direct server case:

\| ?- http_socket_transport::open_listener('127.0.0.1', 8080, Listener,
[]), http_sse::accept(Listener, SSE, ClientInfo,
[transport(http_socket_transport)]), http_sse::send_data(SSE, 'hello'),
http_sse::close(SSE).

For callback-driven client sessions that reconnect automatically, use:

\| ?- http_sse::open_session('http://127.0.0.1:8080/events', Handler,
Response, State, [transport(default)]).

For callback-driven server sessions that should stay on the high-level
surface, use:

\| ?- http_sse::serve_once(Listener, Handler, Response, State,
ClientInfo, [transport(http_socket_transport)]).
