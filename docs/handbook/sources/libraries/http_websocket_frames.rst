.. _library_http_websocket_frames:

``http_websocket_frames``
=========================

This library provides the transport-neutral WebSocket helpers shared by
the current HTTP stack. It includes normalized frame terms and binary
frame parsing and generation in the ``http_websocket_frames`` object.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_websocket_frames <../../apis/library_index.html#http_websocket_frames>`__
link in a web browser.

Loading
-------

To load the full library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_websocket_frames(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_websocket_frames(tester)).

Current scope
-------------

The current implementation provides:

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

Valid properties
----------------

The ``Properties`` list accepted by ``frame/5``, ``generate/2``, and
``write_frame/2`` can contain only the following properties:

- ``masking_key(Key)``

  - ``Key`` must be a list of exactly four bytes.
  - Each byte must be an integer in the ``0..255`` range.

- ``reserved_bits(Bits)``

  - ``Bits`` must be a list containing any ordered subset of
    ``[rsv1, rsv2, rsv3]``.
  - Duplicate reserved-bit atoms are invalid.

Each property can be given at most once. When both properties are
present, the normalized property list is returned in the canonical order
``[reserved_bits(Bits), masking_key(Key)]``.

Current workflow
----------------

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

- This library operates at the frame level only. It does not yet provide
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
