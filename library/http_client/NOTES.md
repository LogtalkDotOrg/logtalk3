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


`http_client`
=============

The `http_client` library provides a request-oriented client layer on top of the
`http_core`, `url`, and `http_socket` libraries. It is the request-oriented
client-side entry point: it builds normalized requests from absolute `http://`
URLs plus options and delegates transport to the sockets-backed layer.

The low-level stream primitives previously exposed as the main API now live in
the `http_client_core` object. That object remains the transport-neutral
stream-processing layer used internally by `http_socket`.

This library can be used with backend Prolog systems that supports the
`sockets` library: ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../apis/library_index.html#http_client](../../apis/library_index.html#http_client)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_client(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_client(tester)).


Usage examples
--------------

The examples below are self-contained. They use a local `http_socket`
listener so they can be reproduced without any external service. They assume a
backend with thread support because `threaded_once/2` and `threaded_exit/2`
are used to run the local server concurrently with the client call.

Define a small echo handler once:

	:- object(notes_http_client_echo_handler,
		implements(http_handler_protocol)).
		
		handle(Request, Response) :-
			http_core::version(Request, Version),
			http_core::body(Request, Body),
			http_core::response(Version, status(200, 'OK'), [], Body, [], Response).
	
	:- end_object.

Start a local listener, send a request, and inspect the normalized response:

	| ?- http_socket::open_listener('127.0.0.1', Port, Listener, []),
	     threaded_once(http_socket::serve_once(Listener, notes_http_client_echo_handler, _), Tag),
	     atomic_list_concat(['http://127.0.0.1:', Port, '/echo'], URL),
	     http_client::post(URL, content('text/plain', text(hello)), Response, []),
	     threaded_exit(http_socket::serve_once(Listener, notes_http_client_echo_handler, _), Tag),
	     http_socket::close_listener(Listener).
	
	Response = response(http(1,1), status(200, 'OK'), _, content('text/plain', text(hello)), _).

For a multipart form-data request, define a handler that inspects the normalized
request body using `http_multipart`:

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

For a WebSocket opening handshake, define a handler that accepts the upgrade:

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

The `Connection` term returned by `open_websocket/4` remains open. After the
handshake, either close it explicitly with `http_socket::close_connection/1`
or hand it to the `http_websocket`, `http_websocket_messages`,
`http_websocket_session`, or `http_websocket_service` libraries.


Current scope
-------------

The initial request-oriented implementation provides:

- `request/4` for one-shot requests against absolute `http://` URLs.
- `open_websocket/4` for validated WebSocket opening handshakes against
  absolute `ws://` URLs, returning the upgraded reusable connection handle and
  the `101` response.
- `request/5` for requests over an already open `http_socket` connection or
  connection pool handle, with endpoint validation against the supplied URL.
- `get/3-4`, `head/3-4`, `delete/3-4`, `post/4-5`, `put/4-5`, and `patch/4-5`
  convenience predicates.

Supported request options are:

- `headers(Headers)` to supply normalized request headers.
- `body(Body)` to supply a normalized request body. The request-oriented
  facade also accepts `body(form_data(Items))` as a convenience descriptor for
  multipart form-data requests; in that case it builds the multipart body via
  `http_multipart` and injects a generated boundary property unless the caller
  already provided one in `properties/1`.
- `query(Pairs)` to append URL-encoded query pairs to the URL query string.
- `version(Version)` to override the default `http(1,1)` version.
- `properties(Properties)` to supply additional normalized request properties.

Supported WebSocket opening-handshake options are:

- `headers(Headers)` to supply additional normalized handshake request headers
  other than the handshake-managed headers.
- `query(Pairs)` to append URL-encoded query pairs to the URL query string.
- `version(Version)` to override the default `http(1,1)` version. The opening
  handshake requires HTTP/1.1 or later.
- `protocols(Protocols)` to request one or more subprotocol tokens.
- `key(Key)` to provide an explicit `Sec-WebSocket-Key` value. When omitted, a
  fresh key is generated automatically.
- `connection_options(Options)` to pass socket options through to
  `http_socket::open_connection/4`.

Multipart workflow
------------------

The current multipart workflow is intentionally small but practical:

- For generic multipart bodies, callers can keep using normalized
  `content(MediaType, multipart(Parts))` terms.
- For common form-data requests, callers can use `form_data(Items)` through the
  existing body path, including `post/4-5`, `put/4-5`, and `patch/4-5`.
- The `Items` descriptors are the same `field(Name, Value, Parameters)` and
  `file(Name, Filename, MediaType, Payload, Parameters)` descriptors
  supported by the `http_multipart::form_data_body/2` helper.
- In both descriptor shapes, `Parameters` is the ordered list of extra
  `Content-Disposition: form-data` parameters to preserve or generate.
- The reserved `name` and `filename` parameters stay explicit helper
  arguments and must not be repeated in the `Parameters` list.
- When using the convenience descriptor, the client facade adds a multipart
  boundary property automatically so the request can be generated on the wire
  without extra caller bookkeeping.

For example, callers can send extra disposition parameters explicitly:

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

- Call `open_websocket/4` with an absolute `ws://` URL.
- The helper opens a dedicated reusable `http_socket` connection, sends a
  normalized opening-handshake request, and validates the server `101`
  response including the `Sec-WebSocket-Accept` value.
- When the call succeeds, the returned connection handle remains open so a
  higher layer can take over the upgraded stream, typically by calling
  `http_socket::connection_streams/3` and then using the `http_websocket`
  frame predicates, the `http_websocket_messages` message predicates, or the
  stateful `http_websocket_session` predicates with explicit close-state and
  automatic control-message handling, including the higher-level
    `http_websocket_service` `run_session/3-4` callback loop.
  - When client-side code wants one entry point for handshake plus session
    execution, the `http_websocket_client_service::open/4-5` predicates layer
    on top of `open_websocket/4`, can write optional initial outbound messages,
    and then run the callback-driven session loop.
- Callers are responsible for eventually closing that connection using
  `http_socket::close_connection/1` if a later layer does not take ownership of
  it. The `http_websocket_service::run_session/3-4` predicates do take
  ownership of the upgraded connection and close it automatically when the
    session loop finishes, as do the higher-level
    `http_websocket_client_service::open/4-5` predicates.

The `http_client_core` object continues to provide the original stream-based
predicates:

- `write_request/2`
- `read_response/2`
- `exchange/4`
- `exchange_connection/4`


Framing rules
-------------

The `http_client_core` object supports response framing for:

- status codes that never carry a body (`1xx`, `204`, `205`, and `304`)
- fixed-length bodies via `Content-Length`
- chunked bodies via `Transfer-Encoding: chunked`, including trailers
- close-delimited bodies terminated by end-of-file

Request-aware exchanges performed by `http_client_core::exchange/4` and
`http_client_core::exchange_connection/4` also support `HEAD` responses. In
that case the normalized response body is `empty` and the response is annotated
with the metadata properties `body_omitted(head)` and, when applicable,
`omitted_body_length(Length)`.

Close-delimited responses returned by `http_client_core::read_response/2` and
`http_client_core::exchange/4` are
annotated with the property `body_framing(close_delimited)`. Sequential
exchanges can only use a close-delimited response as the final response in the
sequence because the connection is consumed while reading the body.


Current limitations
-------------------

- Only absolute `http://` URLs are supported by the request-oriented
  facade. `https://` URLs are rejected until transport-level TLS support is
  added.
- The WebSocket helper supports only plain `ws://` opening
  handshakes. `wss://` remains out of scope until TLS support exists.
- The `open_websocket/4` predicate is limited to opening-handshake validation.
  See the dedicated WebSocket libraries for frame parsing, message reassembly,
  and related functionality.
- Only the transport coding sequence `[chunked]` is recognized when reading
  streamed response bodies in `http_client_core`.
- Close-delimited response bodies can only be used as the final response in an
  `http_client_core::exchange_connection/4` sequence.
- The `http_client_core` object assumes binary streams for both input and
  output.
