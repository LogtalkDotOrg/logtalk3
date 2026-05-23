.. _library_http_websocket:

``http_websocket``
==================

This library provides the next transport-neutral WebSocket slice on top
of the opening-handshake helpers already available in the ``http``,
``http_client``, ``http_server``, and ``http_socket`` libraries. It
focuses on normalized frame terms plus binary frame parsing and
generation.

API documentation
-----------------

Open the `../../docs/index.html <../../docs/index.html>`__ file in a web
browser and choose the libraries index and then the ``http_websocket``
library.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_websocket(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_websocket(tester)).

Current scope
-------------

The current slice provides:

- ``frame/5`` for constructing validated normalized WebSocket frame
  terms.
- ``is_frame/1`` for testing normalized frame terms.
- ``final/2``, ``opcode/2``, ``payload/2``, ``properties/2``, and
  ``property/2`` for inspecting normalized frame terms.
- ``parse/2`` and ``generate/2`` for binary source and sink terms.
- ``read_frame/2`` and ``write_frame/2`` for incremental frame I/O on
  binary streams.

Normalized terms
----------------

Frames use the normalized term:

::

   frame(Final, Opcode, Payload, Properties)

Where:

- ``Final`` is either ``final`` or ``more``.
- ``Opcode`` is one of ``continuation``, ``text``, ``binary``,
  ``close``, ``ping``, or ``pong``.
- ``Payload`` is a list of bytes.
- ``Properties`` currently recognizes:

  - ``masking_key(Key)`` where ``Key`` is a four-byte list.
  - ``reserved_bits(Bits)`` where ``Bits`` is an ordered subset of
    ``[rsv1, rsv2, rsv3]``.

Current workflow
----------------

- Use ``http_client::open_websocket/4`` or
  ``http_socket::serve_websocket_once/5`` to complete the opening
  handshake and obtain an upgraded connection handle.
- Use ``http_socket::connection_streams/3`` to obtain the binary input
  and output streams carried by that upgraded connection handle.
- Use ``read_frame/2`` and ``write_frame/2`` on those streams to
  exchange WebSocket frames.
- Use the higher-level ``http_websocket_messages`` library when you need
  continuation reassembly or UTF-8-aware text and close-reason handling.
- Use the ``http_websocket_session`` library when fragmented reads must
  preserve pending state across interleaved control frames or when
  outgoing writes should apply client or server masking policy
  automatically.
- Use ``parse/2`` and ``generate/2`` when working with in-memory byte
  lists or binary files instead of live streams.

Current limitations
-------------------

- This slice operates at the frame level only. It does not yet provide
  application session loops.
- Message reassembly plus UTF-8-aware text and close-reason handling now
  live in the ``http_websocket_messages`` library.
- Client and server masking policy is not enforced by role in this frame
  layer. The presence of a ``masking_key/1`` property controls whether
  outgoing frames are masked. Use the ``http_websocket_session`` layer
  when you want role-aware writes and role-aware incoming masking
  validation.
- Reserved bits are preserved structurally but no extension negotiation
  or extension semantics are implemented yet.
