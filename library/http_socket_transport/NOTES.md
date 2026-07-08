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


`http_socket_transport`
=============

The `http_socket_transport` library is the first transport-aware layer on top of the
`http_client_core`, `http_server_core`, and `sockets` libraries. It uses TCP sockets
for connection management while delegating HTTP message framing and connection
semantics to the existing stream core and server layers.

This library can be used with backend Prolog systems that support unbound
integer arithmetic and the `sockets` library: ECLiPSe, SICStus Prolog,
SWI-Prolog, Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../apis/library_index.html#http_socket_transport](../../apis/library_index.html#http_socket_transport)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_socket_transport(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_socket_transport(tester)).


Current scope
-------------

The current implementation provides nineteen public predicates, grouped by
responsibility:

- Listener lifecycle: `open_listener/4`, `close_listener/1`, and
  `request_listener_shutdown/1`.
- Reusable client connections: `open_connection/4`, `close_connection/1`,
  `connection_streams/3`, `exchange/3`, `exchange_sequence/3`, `exchange/4`,
  and `exchange_sequence/4`.
- Managed connection pools: `open_connection_pool/4`,
  `close_connection_pool/1`, and `connection_pool_stats/2`.
- Server-side request serving: `serve_once/3`, `serve_websocket_once/5`,
  `serve_listener/4`, and `serve_listener/5`.
- Open-ended serving control: `serve_until_shutdown/4`,
  `serve_until_shutdown/5`, and `request_shutdown/1`.

Although it exposes these transport-oriented helpers, the library stays focused
on socket-backed connection and listener management. It delegates HTTP message
framing and connection semantics to `http_client_core` and `http_server_core`, and
delegates socket creation and teardown to the `sockets` library.

Reusable client connections are represented by `http_connection(Host, Port, ...)`
handle terms that carry normalized endpoint metadata and can be passed to
`exchange/3`, `exchange_sequence/3`, `connection_streams/3`, and
`close_connection/1`.

Accepted WebSocket upgrades are represented by
`http_websocket_connection(ClientInfo, Input, Output)` handle terms that can be
passed to `connection_streams/3` and `close_connection/1`.

Managed connection pools are represented by
`http_connection_pool(Host, Port, ...)` handle terms that carry normalized
endpoint metadata and can be passed to `exchange/3`, `exchange_sequence/3`,
`close_connection_pool/1`, and `connection_pool_stats/2`.

Open-ended serving is controlled by a user-supplied control term. A control
term is any fresh non-variable term chosen by the caller to identify one active
`serve_until_shutdown/4` or `serve_until_shutdown/5` loop. The same term is
later passed to `request_shutdown/1` to stop that specific serving loop,
distinguishing it from any other listener loops running at the same time.

The `open_connection_pool/4` predicate supports the following options:

- `min_size(N)` pre-opens `N` reusable connections when the pool is created.
  The default is `0`.
- `max_size(N)` limits the pool to at most `N` managed connections. The default
  is `10`.
- `connection_options(Options)` passes socket options through to
  `open_connection/4` when creating pooled connections.

Pool exchanges fail immediately with `resource_error(http_socket_transport_connection_pool)`
when no pooled connection is available and the pool is already at its maximum
size.

The `serve_listener/5` predicate supports the following option families:

Shutdown options:

- `shutdown(keep_open)` leaves the listener open after the bounded serving loop.
  This is the default.
- `shutdown(close)` closes the listener when the bounded serving loop finishes
  or aborts.

Worker options:

- `workers(serial)` serves accepted connections sequentially in the caller
  thread. This is the default.
- `workers(per_connection)` spawns one worker thread per accepted connection and
  waits for all workers before returning.
- `workers(pool(N))` serves accepted connections in batches of up to `N` worker
  threads, waiting for each batch to finish before accepting the next batch.
- `workers(pool(N, rolling))` keeps up to `N` worker threads active and
  accepts the next connection as soon as one worker finishes.

Calling `request_listener_shutdown/1` for a listener with an active bounded
`serve_listener/5` loop wakes any blocked accept call and lets the loop return
with the client information terms accepted before the shutdown request.

The `serve_until_shutdown/4` and `serve_until_shutdown/5` predicates support the
`workers/1` option family:

- `workers(serial)` serves accepted connections sequentially in the caller
  thread. This is the default.
- `workers(per_connection)` spawns one worker thread per accepted connection
  and waits for active workers when shutdown is requested.
- `workers(pool(N))` serves accepted connections using at most `N` concurrent
  worker threads, waiting for worker completion notifications before accepting
  additional connections.
- `workers(pool(N, rolling))` is accepted as an explicit alias for the rolling
  fixed-size worker-pool policy used by `workers(pool(N))`.

Open-ended serving loops should use a fresh control term for each call to
`serve_until_shutdown/4` or `serve_until_shutdown/5`.

When the same listener, serving, or pool-management option is given multiple
times, the first occurrence is used.

WebSocket handshake workflow
----------------------------

The current WebSocket transport workflow is intentionally limited to the opening
handshake handoff:

- Client-side code can use `http_client::open_websocket/4` to obtain a reusable
  upgraded connection handle.
- Server-side code can use `serve_websocket_once/5` with a handler that builds
  a valid `101 Switching Protocols` response, typically via
  `http_server_core::accept_websocket/3`.
- When the opening handshake succeeds, the returned upgraded connection remains
  open and its streams are available through `connection_streams/3` for use
  with `http_websocket::read_frame/2` and `http_websocket::write_frame/2` or,
  at the next abstraction level, with
  `http_websocket_messages::read_message/2` and
  `http_websocket_messages::write_message/2`, or with the stateful
  `http_websocket_session` layer when interleaved control frames and
  role-aware masking policies matter, including automatic close replies and
  optional automatic pong replies, or with the `http_websocket_service`
  layer, which provides the higher-level `run_session/3-4`
  callback loop that takes ownership of the upgraded connection and closes it
  automatically after the close handshake completes, the client-side
  `http_websocket_client_service::open/4-5` convenience, and the server-side
  `http_websocket_server_service::serve_once/6-7` convenience.
- Rejected or malformed handshakes are written to the peer and then reported as
  errors by `serve_websocket_once/5`; the accepted streams are closed in that
  case.


Current limitations
-------------------

- Availability depends on the supported backends of the `sockets` library.
- The library does not provide TLS or URL-based request helpers.
- The WebSocket transport helper is limited to the opening handshake and
  upgraded connection handle management. It does not provide frame parsing,
  message I/O, or higher-level session handling.
- The `workers(per_connection)` and `workers(pool(N))` options depend on
  backend thread support.
- Server-side helpers do not provide supervision trees.
