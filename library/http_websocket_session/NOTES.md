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
messages, can automatically orchestrate close replies and optional pong
replies, supports optional keepalive and idle-timeout policies, and now
includes higher-level callback loops and a registry-backed broadcast helper
for upgraded `http_socket` WebSocket connections.

This library can be used with backend Prolog systems that supports the
`sockets` library: ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
Trealla Prolog, and XVM.


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

The current slice provides:

- `initial_state/1` and `is_state/1` for working with session state terms.
- `read_message/4` for stateful message reads that preserve pending fragmented
  data across interleaved control messages while tracking close-handshake state.
- `read_message/5` for stateful message reads plus automatic close-handshake
  replies.
- `read_message/6` for stateful message reads plus automatic close-handshake
  replies and optional auto-pong policy.
- `write_message/2-3` as stateless convenience wrappers for role-aware writes.
- `write_message/4-5` for role-aware writes that also update the session state,
  including close-handshake transitions.
- `run_session/3-4` for higher-level callback-driven loops over upgraded
  `http_socket` WebSocket connection handles that keep reading until the close
  handshake completes or the peer closes the stream and then close the upgraded
  connection automatically, with optional auto-pong, keepalive, and idle-timeout
  policies.
- `open/4-5` in the `http_websocket_client_session` object as a client-side
  convenience that combines `http_client::open_websocket/4`, optional initial
  outbound messages, and the higher-level session loop.
- `serve_once/6-7` in the `http_websocket_server_session` object as a
  server-side convenience that combines `http_socket::serve_websocket_once/5`
  with the higher-level session loop.
- `serve_until_shutdown/5-6` and `request_shutdown/1` in the
  `http_websocket_server_session` object for registry-backed multi-session
  servers with queued broadcast delivery.
- `open/1`, `close/1`, `register/2`, `unregister/2`, `send/3`,
  `broadcast/2`, `broadcast_except/3`, and `take_pending/3` in the
  `http_websocket_session_registry` object for managing active sessions and
  queued outbound messages.


Role-aware objects
------------------

The parameterized object `http_websocket_session(Role, TextRepresentation)`
accepts:

- `Role` equal to `client` or `server`
- `TextRepresentation` equal to `atom`, `chars`, or `codes`

The library also provides two convenience objects using atom text:

- `http_websocket_client_session`
- `http_websocket_server_session`

Client sessions expect masked inbound frames and mask all outgoing frames.
Server sessions expect unmasked inbound frames and leave outgoing frames
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

The `read_message/6` predicate adds the option:

- `auto_pong(on)`
- `auto_pong(off)`

When `auto_pong(on)` is used, incoming ping messages are still returned to the
caller but a matching pong message is written automatically on the output
stream.


Higher-level session loop
-------------------------

The `run_session/3-4` predicates accept an upgraded
`http_socket` WebSocket connection handle together with a handler object that
implements the `http_websocket_session_handler_protocol` protocol:

	handle(Message, Replies)

For each received normalized message, the handler returns a list of normalized
reply messages to write before the next read. The loop:

- starts from the initial session state,
- reuses the stateful `read_message/6` orchestration including automatic close
  replies and optional auto-pong policy,
- writes the handler replies using the stateful `write_message/4` predicate,
- continues until the close handshake reaches `closed(SentPayload,
  ReceivedPayload)` or the peer closes the stream, and
- closes the upgraded connection automatically before returning.

The `run_session/4` predicate accepts these loop options:

- `auto_pong(on)` or `auto_pong(off)`
- `keepalive_interval(Seconds)`
- `idle_timeout(Seconds)`

When `keepalive_interval(Seconds)` is used, the loop sends empty ping messages
after `Seconds` of inbound silence. When `idle_timeout(Seconds)` is used, the
loop sends `message(close, status(1001, idle_timeout))` after `Seconds` of
inbound silence and then waits one more idle interval for the peer close reply.
These timed options require backend thread support.

A small server-side session handler can look like:

	:- object(chat_session_handler,
		implements(http_websocket_session_handler_protocol)).
		
		handle(message(text, ping), [message(text, pong)]) :-
			!.
		handle(message(text, Text), [message(text, Text)]) :-
			!.
		handle(message(close, _Payload), []) :-
			!.
		handle(_Message, []).
	
	:- end_object.

After a successful opening handshake returns an upgraded `Connection` handle,
run the loop with either:

	| ?- http_websocket_server_session::run_session(Connection, chat_session_handler, FinalState).

or:

	| ?- http_websocket_server_session::run_session(Connection, chat_session_handler, FinalState, [auto_pong(on)]).

or with timed loop policies:

	| ?- http_websocket_server_session::run_session(Connection, chat_session_handler, FinalState, [auto_pong(on), keepalive_interval(30), idle_timeout(120)]).

The `run_session/3-4` predicates take ownership of the upgraded connection, so
no explicit `http_socket::close_connection/1` call is needed afterwards.

For a client that wants to collapse the opening handshake and the callback loop
into a single call, the `http_websocket_client_session` object provides:

	open(URL, SessionHandler, Response, FinalState)

or:

	open(URL, SessionHandler, Response, FinalState, [protocols([chat]), initial_messages([message(text, hello)]), auto_pong(on)])

The `initial_messages(Messages)` option writes the given list of normalized
outbound messages immediately after the handshake and before the first session
read. The `open/4-5` predicates also accept the `keepalive_interval/1` and
`idle_timeout/1` session-loop options. They take ownership of the upgraded
connection and close it automatically when the session loop finishes.

For a server that wants to collapse the opening handshake and the callback loop
into a single call, the `http_websocket_server_session` object also provides:

	serve_once(Listener, HandshakeHandler, SessionHandler, Response, FinalState, ClientInfo)

or:

	serve_once(Listener, HandshakeHandler, SessionHandler, Response, FinalState, ClientInfo, [auto_pong(on)])

These predicates accept one incoming socket connection, serve one valid
opening handshake via the given HTTP handler, run the session loop via the
given session handler, and then close the upgraded connection automatically.
They also accept the `keepalive_interval/1` and `idle_timeout/1` session-loop
options.


Registry-backed server helper
-----------------------------

For multi-session servers that need queued outbound delivery and broadcast, the
`http_websocket_server_session` object also provides:

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control)

or:

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, [auto_pong(on), keepalive_interval(30), idle_timeout(120)])

This helper accepts opening handshakes until:

- `http_websocket_server_session::request_shutdown(Control)` is called, or
- the listener is closed externally.

Each accepted upgraded connection is registered in `Registry` and handled in its
own session worker. The registry queues outbound messages so that each worker
only writes to its own connection.

When used with `serve_until_shutdown/5-6`, the session handler may still return
plain normalized reply messages, but it may also return these action wrappers:

- `reply(Message)`
- `broadcast(Message)`
- `broadcast_others(Message)`

For example, a simple chat-style broadcast handler can look like:

	:- object(chat_broadcast_handler,
		implements(http_websocket_session_handler_protocol)).
		
		handle(message(text, Text), [broadcast_others(message(text, Text))]) :-
			!.
		handle(message(close, _Payload), []) :-
			!.
		handle(_Message, []).
	
	:- end_object.

The `http_websocket_session_registry` object provides the registry handle and
queue-management predicates used by this helper. The registry-backed server
helper requires backend thread support.


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
- When client-side code should own the full upgraded connection lifecycle,
  call `http_websocket_client_session::open/4-5` and let the session layer
  handle the opening handshake, any configured initial outbound messages, and
  the final connection close.
- When a single callback object should own the connection lifecycle, pass the
  upgraded handle to `run_session/3-4` and let the session layer close it when
  the close handshake completes.
- When server-side code already owns a listener and wants a single entry point
  for handshake plus session execution, call
  `http_websocket_server_session::serve_once/6-7`.
- When server-side code needs multiple active sessions plus queued broadcast
  delivery, create a registry with `http_websocket_session_registry::open/1`
  and call `http_websocket_server_session::serve_until_shutdown/5-6`.


Current limitations
-------------------

- The `run_session/3-4` and `serve_once/6-7` helpers remain synchronous,
  single-connection callback loops, but they are available on backends without
  thread support when used without timed loop options.
- The timed loop options `keepalive_interval/1` and `idle_timeout/1` require
  backend thread support.
- The `serve_until_shutdown/5-6` helper requires backend thread support.
- The explicit session state remains available for callers that need direct
  control of read and write sequencing.
- Fragmentation is currently controlled only by fixed payload chunk size.
- Reserved-bit extension negotiation and extension semantics remain delegated to
  lower layers.
