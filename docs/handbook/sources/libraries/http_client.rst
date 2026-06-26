.. _library_http_client:

``http_client``
===============

The ``http_client`` library provides a request-oriented client layer on
top of the ``http_core``, ``url``, and parameterized HTTP transport
libraries. It is the request-oriented client-side entry point: it builds
normalized requests from absolute URLs plus options and delegates
transport to the selected ``http_socket_protocol``-compatible layer,
such as ``http_socket`` or ``http_socket_process``.

This library can be used with backend Prolog systems that support
unbound integer arithmetic and the ``sockets`` library: ECLiPSe, SICStus
Prolog, SWI-Prolog, Trealla Prolog, and XVM.

When using the default ``http_client`` object, the transport
parameterization is ``http_socket``. The parametric
``http_client(_HTTPSocket_)`` object can also be instantiated with
alternative transports that implement the same protocol.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_client <../../apis/library_index.html#http_client>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_client(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_client(tester)).

Usage examples
--------------

The examples below are self-contained. They use a local ``http_socket``
listener so they can be reproduced without any external service. They
assume a backend with thread support because ``threaded_once/2`` and
``threaded_exit/2`` are used to run the local server concurrently with
the client call.

Define a small echo handler once:

::

   :- object(notes_http_client_echo_handler,
       implements(http_handler_protocol)).
       
       handle(Request, Response) :-
           http_core::version(Request, Version),
           http_core::body(Request, Body),
           http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

   :- end_object.

Start a local listener, send a request, and inspect the normalized
response:

::

   | ?- http_socket::open_listener('127.0.0.1', Port, Listener, []),
        threaded_once(http_socket::serve_once(Listener, notes_http_client_echo_handler, _), Tag),
        atomic_list_concat(['http://127.0.0.1:', Port, '/echo'], URL),
        http_client::post(URL, content('text/plain', text(hello)), Response, []),
        threaded_exit(http_socket::serve_once(Listener, notes_http_client_echo_handler, _), Tag),
        http_socket::close_listener(Listener).

   Response = response(http(1,1), status(200, 'OK'), _, content('text/plain', text(hello)), _).

For a multipart form-data request, define a handler that inspects the
normalized request body using ``http_multipart``:

::

   :- object(notes_http_client_multipart_handler,
       implements(http_handler_protocol)).
       
       handle(Request, Response) :-
           http_core::version(Request, Version),
           http_core::body(Request, Body),
           http_multipart::fields(Body, [field(title, Title, _FieldParameters)]),
           http_multipart::files(Body, [file(upload, Filename, 'text/plain', text(hello), _FileParameters)]),
           atomic_list_concat(['title=', Title, '; upload=', Filename], Text),
           http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Text)), [], Response).

   :- end_object.

   | ?- http_socket::open_listener('127.0.0.1', Port, Listener, []),
        threaded_once(http_socket::serve_once(Listener, notes_http_client_multipart_handler, _), Tag),
        atomic_list_concat(['http://127.0.0.1:', Port, '/upload'], URL),
        http_client::post(
            URL,
            form_data([
                field(title, 'Logtalk', []),
                file(upload, 'notes.txt', 'text/plain', text(hello), [])
            ]),
            Response,
            []
        ),
        threaded_exit(http_socket::serve_once(Listener, notes_http_client_multipart_handler, _), Tag),
        http_socket::close_listener(Listener).

   Response = response(http(1,1), status(200, 'OK'), _, content('text/plain', text('title=Logtalk; upload=notes.txt')), _).

For a WebSocket opening handshake, define a handler that accepts the
upgrade:

::

   :- object(notes_http_client_websocket_handler,
       implements(http_handler_protocol)).
       
       handle(Request, Response) :-
           http_server::accept_websocket(Request, Response, [protocol(chat)]).

   :- end_object.

   | ?- http_socket::open_listener('127.0.0.1', Port, Listener, []),
        threaded_once(http_socket::serve_once(Listener, notes_http_client_websocket_handler, _), Tag),
        atomic_list_concat(['ws://127.0.0.1:', Port, '/socket'], URL),
        http_client::open_websocket(URL, Connection, Response, [protocols([chat]), key('dGhlIHNhbXBsZSBub25jZQ==')]),
        http_socket::close_connection(Connection),
        threaded_exit(http_socket::serve_once(Listener, notes_http_client_websocket_handler, _), Tag),
        http_socket::close_listener(Listener).

   Response = response(http(1,1), status(101, 'Switching Protocols'), _, empty, _).

The ``Connection`` term returned by ``open_websocket/4`` remains open.
After the handshake, either close it explicitly with
``http_socket::close_connection/1`` or hand it to the
``http_websocket``, ``http_websocket_messages``,
``http_websocket_session``, or ``http_websocket_service`` libraries.

Current scope
-------------

The initial request-oriented implementation provides:

- ``request/4`` for one-shot requests against absolute URLs supported by
  the selected transport parameterization.
- ``open_websocket/4`` for validated WebSocket opening handshakes
  against absolute WebSocket URLs supported by the selected transport
  parameterization, returning the upgraded reusable connection handle
  and the ``101`` response.
- ``request/5`` for requests over an already open compatible reusable
  connection or connection pool handle, with endpoint validation against
  the supplied URL.
- ``get/3-4``, ``head/3-4``, ``delete/3-4``, ``post/4-5``, ``put/4-5``,
  and ``patch/4-5`` convenience predicates.

When the same request-construction or WebSocket opening-handshake option
is given multiple times, the first occurrence is used.

Supported request options are:

- ``headers(Headers)`` to supply normalized request headers.
- ``body(Body)`` to supply a normalized request body. The
  request-oriented facade also accepts ``body(form_data(Items))`` as a
  convenience descriptor for multipart form-data requests; in that case
  it builds the multipart body via ``http_multipart`` and injects a
  generated boundary property unless the caller already provided one in
  ``properties/1``.
- ``query(Pairs)`` to append URL-encoded query pairs to the URL query
  string.
- ``version(Version)`` to override the default ``http(1,1)`` version.
- ``properties(Properties)`` to supply additional normalized request
  properties.
- ``connection_options(Options)`` to pass transport-specific options
  through to the underlying ``open_connection/4`` call for one-shot
  requests. When the URL uses the ``https://`` scheme,
  ``connection_transport(tls)`` is added automatically unless
  ``Options`` already specify a transport explicitly.

Supported WebSocket opening-handshake options are:

- ``headers(Headers)`` to supply additional normalized handshake request
  headers other than the handshake-managed headers.
- ``query(Pairs)`` to append URL-encoded query pairs to the URL query
  string.
- ``version(Version)`` to override the default ``http(1,1)`` version.
  The opening handshake requires HTTP/1.1 or later.
- ``protocols(Protocols)`` to request one or more subprotocol tokens.
- ``key(Key)`` to provide an explicit ``Sec-WebSocket-Key`` value. When
  omitted, a fresh key is generated automatically.
- ``connection_options(Options)`` to pass transport-specific options
  through to the selected transport ``open_connection/4`` predicate.
  When the URL uses the ``wss://`` scheme, ``connection_transport(tls)``
  is added automatically unless ``Options`` already specify a transport
  explicitly.

The stream-based primitives remain available from the
``http_client_core`` object.

Multipart workflow
------------------

The current multipart workflow is intentionally small but practical:

- For generic multipart bodies, callers can keep using normalized
  ``content(MediaType, multipart(Parts))`` terms.
- For common form-data requests, callers can use ``form_data(Items)``
  through the existing body path, including ``post/4-5``, ``put/4-5``,
  and ``patch/4-5``.
- The ``Items`` descriptors are the same
  ``field(Name, Value, Parameters)`` and
  ``file(Name, Filename, MediaType, Payload, Parameters)`` descriptors
  supported by the ``http_multipart::form_data_body/2`` helper.
- In both descriptor shapes, ``Parameters`` is the ordered list of extra
  ``Content-Disposition: form-data`` parameters to preserve or generate.
- The reserved ``name`` and ``filename`` parameters stay explicit helper
  arguments and must not be repeated in the ``Parameters`` list.
- When using the convenience descriptor, the client facade adds a
  multipart boundary property automatically so the request can be
  generated on the wire without extra caller bookkeeping.

For example, callers can send extra disposition parameters explicitly:

::

   | ?- http_client::post(
        URL,
        form_data([
            field(title, 'Logtalk', [charset-utf8]),
            file(upload, 'notes.txt', 'text/plain', text(hello), [creation_date-'2026-06-08'])
        ]),
        Response,
        []
    ).

WebSocket workflow
------------------

The current WebSocket workflow is intentionally limited to the opening
handshake:

- Call ``open_websocket/4`` with an absolute WebSocket URL supported by
  the selected transport parameterization.
- The helper opens a dedicated reusable transport connection, sends a
  normalized opening-handshake request, and validates the server ``101``
  response including the ``Sec-WebSocket-Accept`` value.
- When the call succeeds, the returned connection handle remains open so
  a higher layer can take over the upgraded stream, typically by calling
  the selected transport ``connection_streams/3`` predicate and then
  using the ``http_websocket`` frame predicates, the
  ``http_websocket_messages`` message predicates, or the stateful
  ``http_websocket_session`` predicates with explicit close-state and
  automatic control-message handling, including the higher-level
  ``http_websocket_service`` ``run_session/3-4`` callback loop.
- When client-side code wants one entry point for handshake plus session
  execution, the ``http_websocket_client_service::open/4-5`` predicates
  layer on top of ``open_websocket/4``, can write optional initial
  outbound messages, and then run the callback-driven session loop.
- Callers are responsible for eventually closing that connection using
  the selected transport ``close_connection/1`` predicate if a later
  layer does not take ownership of it. The
  ``http_websocket_service::run_session/3-4`` predicates do take
  ownership of the upgraded connection and close it automatically when
  the session loop finishes, as do the higher-level
  ``http_websocket_client_service::open/4-5`` predicates.

Current limitations
-------------------

- URL scheme support depends on the selected transport parameterization.
  With the default ``http_socket`` parameterization, only absolute
  ``http://`` URLs are currently supported by the request-oriented
  facade and only plain ``ws://`` opening handshakes are supported by
  ``open_websocket/4``. The corresponding TLS-backed ``https://`` and
  ``wss://`` schemes are therefore not supported in this
  parameterization.
- When the selected transport parameterization supports secure schemes,
  the request-oriented facade defaults ``https://`` and ``wss://`` URLs
  without an explicit port to port ``443`` and adds
  ``connection_transport(tls)`` to the connection options automatically
  unless already specified explicitly.
- This TLS-aware behavior applies, for example, to the
  ``http_socket_process`` parameterization, which provides transport
  support for secure schemes via the same ``http_socket_protocol``
  interface.
- The ``open_websocket/4`` predicate is limited to opening-handshake
  validation. See the dedicated WebSocket libraries for frame parsing,
  message reassembly, and related functionality.
