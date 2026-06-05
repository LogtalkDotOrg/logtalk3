.. _library_http_core:

``http_core``
=============

This library provides the transport-independent core of the HTTP stack.
It currently covers normalized request and response terms, shared
protocols, wire parsing and generation for the message start-lines,
headers, and bodies, plus semantic normalization for a core set of
frequently used headers.

The library predicates are currently defined in the ``http_core``
object.

Layering
--------

Use the ``http_core`` library as the base of the current HTTP and API
stack:

- Start here when you need normalized ``request/6`` and ``response/5``
  terms, shared protocols, body codecs, or wire parsing and generation.
- Add `http_server <../http_server/NOTES.md>`__ or
  `http_client <../http_client/NOTES.md>`__ when you need transport-side
  orchestration on top of those normalized messages.
- Add `http_router <../http_router/NOTES.md>`__ when you want
  declarative path-template and method dispatch over handler objects
  implementing the ``http_handler_protocol`` protocol.
- Add `rest <../rest/NOTES.md>`__ when you want a higher-level authoring
  layer on top of ``http_router``.
- Pair any of those layers with `open_api <../open_api/NOTES.md>`__ when
  you want OpenAPI document derivation or request and response contract
  validation.

API documentation
-----------------

Open the
`../../apis/library_index.html#http <../../apis/library_index.html#http>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http(tester)).

Current scope
-------------

- Define core shared protocols for request objects, response objects,
  body codecs, and handlers
- Construct normalized ``request/6`` and ``response/5`` terms using
  ``request/7`` and ``response/6``
- Validate normalized methods, targets, versions, statuses, headers,
  bodies, and higher-layer properties
- Provide generic accessors over normalized request and response terms
- Parse and generate request lines, status lines, header blocks, bodies,
  full requests, and full responses
- Generate effective response headers independently from the response
  body bytes using ``generate_response_headers/2`` when higher layers
  need to stream file-backed bodies without materializing them in memory
  first
- Decode and generate chunked transfer coding for full requests and
  responses, including normalized ``trailers/1`` properties
- Parse and generate multipart bodies at both the standalone body level
  and the full-message level when a boundary is available
- Provide the normalized multipart body and part representation reused
  by the higher-level ``http_multipart`` helpers and the current client
  and server integration tests
- Normalize and generate the core WebSocket opening-handshake headers
  needed by the next client and server transport slices
- Dispatch to concrete body codecs for ``application/octet-stream``,
  ``text/plain``, ``application/json``, and
  ``application/x-www-form-urlencoded``
- Normalize semantic header values for ``Content-Length``,
  ``Content-Type``, ``Cookie``, ``Set-Cookie``, ``Host``,
  ``Connection``, ``Upgrade``, ``Sec-WebSocket-Key``,
  ``Sec-WebSocket-Version``, ``Sec-WebSocket-Accept``,
  ``Sec-WebSocket-Protocol``, and ``Transfer-Encoding``
- Derive and validate normalized properties such as ``content_type/2``,
  ``content_length/1``, ``host/1-2``, ``cookies/1``, ``set_cookies/1``,
  ``query_pairs/1``, ``path_segments/1``, ``scheme/1``,
  ``connection/1``, ``upgrade/1``, ``websocket_key/1``,
  ``websocket_version/1``, ``websocket_accept/1``,
  ``websocket_protocol/1``, ``transfer_encoding/1``, ``trailers/1``, and
  ``decoded_body/1``
- Reuse ``url`` for absolute-target validation and ``http_cookies`` for
  ``Cookie`` and ``Set-Cookie`` header validation

Normalized terms
----------------

Requests use the fixed-arity term:

::

   request(Method, Target, Version, Headers, Body, Properties)

Responses use the fixed-arity term:

::

   response(Version, Status, Headers, Body, Properties)

Supported request targets are:

- ``asterisk``
- ``origin(Path)``
- ``origin(Path, Query)``
- ``absolute(URLComponents)``
- ``authority(Host)``
- ``authority(Host, Port)``

Supported body terms are:

- ``empty``
- ``content(MediaType, binary(Bytes))``
- ``content(MediaType, file(Path, Offset, Length))``
- ``content(MediaType, text(Text))``
- ``content(MediaType, json(Term))``
- ``content(MediaType, form(Pairs))``
- ``content(MediaType, multipart(Parts))``

Multipart parts use recursive normalized terms:

::

   part(Headers, Body, Properties)

Current limitations
-------------------

- Chunked transfer coding support is limited to the bare
  ``Transfer-Encoding: chunked`` case; stacked transfer codings remain
  out of scope for now
- Multipart wire parsing and generation require a boundary parameter in
  the effective ``Content-Type`` metadata, or an explicit ``boundary/1``
  option when using ``parse_body/4`` or ``generate_body/3``
- File-backed bodies are generated only; parsing normalizes incoming
  bodies to in-memory payload terms. The declared ``Offset`` and
  ``Length`` determine the response ``Content-Length`` metadata when the
  body is generated.
- The body codec set is intentionally small and focused on a limited set
  of media types
- Header normalization is selective; headers outside the supported set
  are preserved as validated raw ``Name-Value`` pairs
- The WebSocket support in this layer is limited to HTTP
  opening-handshake metadata normalization. ``wss://`` and TLS-backed
  transport orchestration are currently not supported
