.. _library_http_websocket_messages:

``http_websocket_messages``
===========================

This library provides the next transport-neutral WebSocket slice on top
of the ``http_websocket`` frame library. It reassembles continuation
sequences for text and binary messages, decodes and encodes UTF-8 text
payloads, and adds a normalized message term for text, binary, ping,
pong, and close messages.

This library can be used with backend Prolog systems that supports
unbound integer arithmetic.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_websocket_messages <../../apis/library_index.html#http_websocket_messages>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_websocket_messages(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_websocket_messages(tester)).

Current scope
-------------

The current implementation provides:

- ``message/3`` for constructing validated normalized WebSocket message
  terms.
- ``is_message/1`` for testing normalized message terms.
- ``type/2`` and ``payload/2`` for inspecting normalized message terms.
- ``read_message/2`` for reading one WebSocket message from a binary
  stream, including continuation reassembly for text and binary
  messages.
- ``write_message/2`` for writing one WebSocket message as a single
  final frame.

This layer remains intentionally stateless. The
``http_websocket_session`` library builds on top of it when the caller
needs explicit session state, interleaved control-frame surfacing, or
role-aware outgoing fragmentation and masking.

Normalized terms
----------------

Messages use the normalized term:

::

   message(Type, Payload)

Where:

- ``Type`` is one of ``text``, ``binary``, ``ping``, ``pong``, or
  ``close``.
- For ``text``, ``Payload`` is text in the selected representation.
- For ``binary``, ``ping``, and ``pong``, ``Payload`` is a list of
  bytes.
- For ``close``, ``Payload`` is one of:

  - ``empty``
  - ``status(Code)``
  - ``status(Code, Reason)``

The parameterized object ``http_websocket_messages(TextRepresentation)``
accepts ``atom``, ``chars``, or ``codes``. The plain
``http_websocket_messages`` object uses atoms.

Current workflow
----------------

- Complete the opening handshake with ``http_client::open_websocket/4``
  or ``http_socket::serve_websocket_once/5``.
- Obtain the upgraded binary streams with
  ``http_socket::connection_streams/3``.
- Use ``read_message/2`` and ``write_message/2`` when you want
  UTF-8-aware text messages and continuation reassembly instead of raw
  frame access.
- Use the higher-level ``http_websocket_session`` library when
  fragmented data reads must coexist with interleaved control messages
  or when writes should apply client or server masking policy
  automatically.
- Use the lower-level ``http_websocket`` predicates directly when you
  need raw frame properties such as masking keys or reserved bits.

Current limitations
-------------------

- Outgoing messages are generated as a single final frame. This layer
  does not provide outgoing fragmentation policies.
- Fragmented text and binary messages are reassembled only when the
  following frames are plain continuation frames. Use the
  ``http_websocket_session`` library when interleaved control messages
  must be surfaced while the fragmented data remains pending.
- Client and server masking policy is not enforced automatically by role
  in this stateless layer. Use the ``http_websocket_session`` library
  when that policy should be applied automatically.
- This layer does not provide application session loops. For full
  close-handshake orchestration or automatic ping/pong policies, use the
  ``http_websocket_session`` layer.
