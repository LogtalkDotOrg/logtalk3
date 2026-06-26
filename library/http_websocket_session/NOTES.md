________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`http_websocket_session`
========================

This library provides the next WebSocket slice on top of the
`http_websocket_messages` library. It adds a state term for incremental reads,
surfaces interleaved control messages while a fragmented text or binary message
is still pending, applies role-aware client and server masking when writing
messages, and can automatically orchestrate close replies and optional pong
replies.

This library is a pure state-machine layer working on binary streams: it does
not create threads and does not depend on socket support. The higher-level
callback-driven session loops, keepalive and idle-timeout policies, and the
registry-backed broadcast helpers for upgraded `http_socket` connections are
provided by the `http_websocket_service` library.

This library can be used with backend Prolog systems that support unbound
integer arithmetic.


API documentation
-----------------

Open the [../../apis/library_index.html#http_websocket_session](../../apis/library_index.html#http_websocket_session)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_websocket_session(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_websocket_session(tester)).


Current scope
-------------

The current implementation provides:

- `initial_state/1` and `is_state/1` for working with session state terms.
- `read_message/4` for stateful message reads that preserve pending fragmented
  data across interleaved control messages while tracking close-handshake state.
- `read_message/5` for stateful message reads plus automatic close-handshake
  replies.
- `read_message/6` for stateful message reads plus automatic close-handshake
  replies and optional auto-pong and maximum payload length policies.
- `write_message/2-3` as stateless convenience wrappers for role-aware writes.
- `write_message/4-5` for role-aware writes that also update the session state,
  including close-handshake transitions.


Role-aware objects
------------------

The parameterized object `http_websocket_session(Role, TextRepresentation)`
accepts:

- `Role` equal to `client` or `server`
- `TextRepresentation` equal to `atom`, `chars`, or `codes`

The library also provides two convenience objects using atom text:

- `http_websocket_client_session`
- `http_websocket_server_session`

Client sessions expect unmasked inbound frames and mask all outgoing frames.
Server sessions expect masked inbound frames and leave outgoing frames
unmasked.


Stateful reads
--------------

The `read_message/4` predicate threads an explicit session state term:

	read_message(Stream, State0, State, Message)

When a fragmented text or binary message is in progress and a control frame is
received before the final continuation frame, the control frame is returned as a
normalized `message(Type, Payload)` term and the pending fragmented data is kept
in `State` for the next call.

Close-handshake state is tracked in the returned state. Open states keep the
compact form `session_state(Pending)`. Once a close message has been sent or
received, the state uses `session_state(Pending, CloseStatus)` where
`CloseStatus` is one of:

- `close_sent(Payload)`
- `close_received(Payload)`
- `closed(SentPayload, ReceivedPayload)`


Automatic control handling
--------------------------

The `read_message/5` predicate automatically writes a matching close reply when
it reads a close message and the session state shows that no close message has
yet been sent.

The `read_message/6` predicate adds the options:

- `auto_pong(on)` or `auto_pong(off)`
- `max_payload_length(Bytes)`

When `auto_pong(on)` is used, incoming ping messages are still returned to the
caller but a matching pong message is written automatically on the output
stream. When `max_payload_length(Bytes)` is used, inbound frames whose declared
payload length is greater than `Bytes` are rejected before allocating payload
storage.


Outgoing fragmentation
----------------------

The `write_message/3` predicate accepts the option:

- `fragment_size(Size)`

For `text` and `binary` messages, this splits the payload into frames of at
most `Size` bytes. The first frame uses the original opcode and continuation
frames use the `continuation` opcode. Control messages remain single final
frames regardless of the option.


Current workflow
----------------

- Complete the opening handshake with `http_client::open_websocket/4` or
  `http_socket::serve_websocket_once/5`.
- Use the `http_websocket_messages` layer when you only need stateless message
  normalization and simple reassembly.
- Use the `http_websocket_session` layer when you need stateful reads across
  interleaved control messages, close-state tracking, automatic close replies,
  optional auto-pong behavior, or role-aware outgoing fragmentation and masking.
- Use the `http_websocket_service` library when a callback object should own
  the connection lifecycle through higher-level session loops with optional
  keepalive, idle-timeout, and registry-backed broadcast policies.


Current limitations
-------------------

- Fragmentation is controlled only by fixed payload chunk size.
- Reserved-bit extension negotiation and extension semantics remain delegated to
  lower layers.
