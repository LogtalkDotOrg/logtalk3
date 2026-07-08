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


`http_websocket`
================

This library provides high-level WebSocket predicates for opening and
closing connections, for exchanging messages, and for running common client
and server session loops.

By default, the `http_websocket` object uses the `http_socket_transport` transport,
which is limited to the `ws://` scheme. The parametric `http_websocket(_HTTPSocket_)`
object supports alternative `http_transport_protocol` implementations such as
`http_process_transport`, which supports both `ws://` and `wss://` schemes.

This library can be used with backend Prolog systems that support unbound
integer arithmetic and the `sockets` library: ECLiPSe, SICStus Prolog,
SWI-Prolog, Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../apis/library_index.html#http_websocket](../../apis/library_index.html#http_websocket)
link in a web browser.


Loading
-------

To load the full library, load the `loader.lgt` file:

	| ?- logtalk_load(http_websocket(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_websocket(tester)).


Current scope
-------------

The current implementation provides the following predicates:

- `open/2-3` for opening client WebSocket connections and
  returning opaque handles. The accepted `ws://` and `wss://` schemes depend
  on the selected transport parameterization.
- `accept/3-4` for accepting one server-side WebSocket
  connection on an open listener and returning opaque handles. The listener
  must be created by the same transport parameterization, such as
  `http_socket_transport` or `http_process_transport`.
- `send/2-3`, `receive/2-3`, and
  `close/1-2` for direct message exchange using those
  handles.
- `property/2` for inspecting handle properties such as the
  handshake response, selected subprotocol, or underlying upgraded
  connection.
- `send_text/2`, `receive_text/2`, `send_binary/2`,
  `receive_binary/2`, `send_json/2`, `receive_json/2`, `send_term/2`, and
  `receive_term/2` convenience predicates for common payload formats.
- `open_session/4-5` and `serve_once/5-6`
  for callback-driven client and server sessions built on top of the
  `http_websocket_service` layer.

The default `http_websocket` object is the `http_websocket(http_socket_transport)`
specialization. The test suite also exercises the `http_process_transport`
parameterization, which supports the `wss://` scheme.


Current workflow
----------------

For the common direct client case:

  | ?- http_websocket::open('ws://127.0.0.1:8080/echo', WebSocket, [protocols([chat])]),
       http_websocket::send_text(WebSocket, hello),
       http_websocket::receive_text(WebSocket, Reply),
       http_websocket::close(WebSocket, status(1000, done)).

For the common direct server case:

  | ?- http_socket_transport::open_listener('127.0.0.1', 8080, Listener, []),
       http_websocket::accept(Listener, WebSocket, ClientInfo, [protocol(chat)]),
       http_websocket::receive(WebSocket, Message),
       http_websocket::send(WebSocket, Message).

To use the process-backed transport instead, pair the matching listener and
WebSocket parameterization:

  | ?- http_process_transport::open_listener('127.0.0.1', 8080, Listener, []),
       http_websocket(http_process_transport)::accept(Listener, WebSocket, ClientInfo, [protocol(chat)]),
       http_websocket(http_process_transport)::receive(WebSocket, Message),
       http_websocket(http_process_transport)::send(WebSocket, Message).

For callback-driven client sessions that should stay on the high-level
surface, use:

  | ?- http_websocket::open_session(URL, Handler, Response, State, [protocols([chat]), initial_messages([message(text, hello)])]).

For callback-driven server sessions that should stay on the high-level
surface, use:

  | ?- http_websocket::serve_once(Listener, Handler, Response, State, ClientInfo, [protocol(chat)]).


Lower-level layers
------------------

The `http_websocket` library now hides the most common plumbing, but the
lower-level libraries remain available when you need them:

- Use `http_websocket_frames` when working with raw frame parsing and
  generation.
- Use `http_websocket_messages` when you want transport-neutral message I/O
  without connection or session ownership.
- Use `http_websocket_session` when you need the stateful message layer with
  detailed close/ping state control but without callback loops or threads.
- Use `http_websocket_service` when you need the full callback-driven session
  layer or registry-backed broadcast helpers.
