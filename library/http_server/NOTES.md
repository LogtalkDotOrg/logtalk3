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


`http_server`
=============

The `http_server` library provides a portable stream-oriented server layer on top
of the `http_core` library. It is designed for the first orchestration step above
the wire parser and generator: read exactly one request from an existing binary
stream, dispatch it to a handler object implementing the `http_handler_protocol`
protocol, and write exactly one response.

This library can be used with backend Prolog systems that support unbound
integer arithmetic and the `sockets` library: ECLiPSe, SICStus Prolog,
SWI-Prolog, Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../apis/library_index.html#http_server](../../apis/library_index.html#http_server)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_server(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_server(tester)).


Usage examples
--------------

The examples below use in-memory streams or local files so they can be
reproduced directly.

Define a simple handler object once:

	:- object(notes_http_server_echo_handler,
		implements(http_handler_protocol)).
		
		handle(Request, Response) :-
			http_core::version(Request, Version),
			http_core::body(Request, Body),
			http_core::response(Version, status(200, 'OK'), [], Body, [], Response).
	
	:- end_object.

To serve a single request from existing binary streams:

	| ?- Request = 'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 5\r\n\r\nhello',
	     atom_codes(Request, RequestBytes),
	     open('request.tmp', write, RequestStream, [type(binary)]),
	     forall(member(RequestByte, RequestBytes), put_byte(RequestStream, RequestByte)),
	     close(RequestStream),
	     open('request.tmp', read, Input, [type(binary)]),
	     open('response.tmp', write, Output, [type(binary)]),
	     http_server::serve(Input, Output, notes_http_server_echo_handler),
	     close(Input),
	     close(Output),
	     http_core::parse_response(file('response.tmp'), Response).

	Response = response(http(1,1), status(200, 'OK'), _, content('text/plain', text(hello)), _).

To keep serving requests on the same stream pair until persistence rules say to
stop, write several requests into the same input stream and call
`serve_connection/3` once:

	| ?- Requests = 'POST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\nonePOST /echo HTTP/1.1\r\nhost: example.com\r\ncontent-type: text/plain\r\ncontent-length: 3\r\n\r\ntwo',
	     atom_codes(Requests, RequestBytes),
	     open('requests.tmp', write, RequestStream, [type(binary)]),
	     forall(member(RequestByte, RequestBytes), put_byte(RequestStream, RequestByte)),
	     close(RequestStream),
	     open('requests.tmp', read, Input, [type(binary)]),
	     open('responses.tmp', write, Output, [type(binary)]),
	     http_server::serve_connection(Input, Output, notes_http_server_echo_handler),
	     close(Input),
	     close(Output).

The generated `responses.tmp` file contains two consecutive normalized HTTP
responses, one for `one` and one for `two`.

For multipart request inspection, define a handler that uses `http_multipart`
on the normalized request body:

	:- object(notes_http_server_multipart_handler,
		implements(http_handler_protocol)).
		
		handle(Request, Response) :-
			http_core::version(Request, Version),
			http_core::body(Request, Body),
			http_multipart::fields(Body, [field(title, Title, _FieldParameters)]),
			http_multipart::files(Body, [file(upload, Filename, 'text/plain', text(hello), _FileParameters)]),
			atomic_list_concat(['title=', Title, '; upload=', Filename], Text),
			http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Text)), [], Response).
	
	:- end_object.

That handler can be used unchanged with `serve/3` or `serve_connection/3`
because this library exposes multipart requests in the same normalized form
produced by `http_core::parse_request/2`.

The multipart helper predicates used by handlers return the parameter-aware
`field(Name, Value, Parameters)` and
`file(Name, Filename, MediaType, Payload, Parameters)` descriptors defined by
the `http_multipart` library.

In both descriptor shapes, `Parameters` is the ordered list of extra
`Content-Disposition: form-data` parameters. The reserved `name` and
`filename` parameters stay explicit helper arguments and are not repeated in
that list.

For a WebSocket opening handshake, define a handler that delegates to
`accept_websocket/3`:

	:- object(notes_http_server_websocket_handler,
		implements(http_handler_protocol)).
		
		handle(Request, Response) :-
			http_server::accept_websocket(Request, Response, [protocol(chat)]).
	
	:- end_object.

	| ?- Request = 'GET /socket HTTP/1.1\r\nhost: example.com\r\nconnection: Upgrade\r\nupgrade: websocket\r\nsec-websocket-key: dGhlIHNhbXBsZSBub25jZQ==\r\nsec-websocket-version: 13\r\nsec-websocket-protocol: chat\r\n\r\n',
	     atom_codes(Request, RequestBytes),
	     open('ws_request.tmp', write, RequestStream, [type(binary)]),
	     forall(member(RequestByte, RequestBytes), put_byte(RequestStream, RequestByte)),
	     close(RequestStream),
	     open('ws_request.tmp', read, Input, [type(binary)]),
	     open('ws_response.tmp', write, Output, [type(binary)]),
	     http_server::serve_websocket(Input, Output, notes_http_server_websocket_handler, Outcome),
	     close(Input),
	     close(Output).

	Outcome = accepted(RequestTerm, Response),
	Response = response(http(1,1), status(101, 'Switching Protocols'), _, empty, _).

After `serve_websocket/4` succeeds, higher layers must take ownership of the
underlying upgraded connection or streams. This library stops at the HTTP
opening handshake.


Current scope
-------------

The current implementation provides seven predicates:

- `read_request/2` reads exactly one request from a binary stream without
  requiring end-of-file.
- `write_response/2` writes exactly one normalized response to a binary stream.
- `write_response/2` streams file-backed response bodies directly after writing
	the generated header block when the response does not use chunked transfer
	coding.
- `dispatch/3` calls a handler object and normalizes handler failures into
  `500 Internal Server Error` responses.
- `accept_websocket/3` validates a normalized WebSocket opening-handshake
  request and builds the corresponding `101 Switching Protocols` response,
  including `Sec-WebSocket-Accept` and optional subprotocol selection.
- `serve_websocket/4` reads one request, dispatches one response, writes it to
  the output stream, and reports whether the exchange completed with a valid
  WebSocket opening-handshake response.
- `serve/3` connects request reading, handler dispatch, and response writing,
  returning `400 Bad Request` for malformed input. For `HEAD` requests it sends
	the generated response headers but suppresses the response body bytes,
	including for file-backed responses.
- `serve_connection/3` repeatedly serves requests on the same stream pair using
  HTTP persistence rules and stops on end-of-file or `Connection: close`
  semantics. For `HEAD` requests it sends the generated response headers but
	suppresses the response body bytes, including for file-backed responses.

Because incoming request bodies are normalized by the `http_core` library,
handler objects can inspect `multipart/form-data` requests directly using the
`http_multipart` predicates after reading or serving a request.

This layer intentionally stays transport-neutral. It does not accept sockets,
listen for connections, or manage concurrency. Those concerns belong to higher
layers or backend-specific integration built on top of this stream contract.


Framing rules
-------------

Request framing currently supports:

- header termination using `CRLF CRLF`
- fixed-length bodies via `Content-Length`
- chunked request bodies via `Transfer-Encoding: chunked`, including trailers

When neither `Content-Length` nor `Transfer-Encoding` is present, the request is
read as having an empty body so that subsequent bytes remain available for the
next request on the stream.

Multipart workflow
------------------

This library does not introduce a separate multipart server API. Multipart
requests are exposed to handlers as the same normalized request bodies produced
by `http_core::parse_request/2`, which means handlers can use `http_multipart::fields/2`
and `http_multipart::files/2` directly on the request body.

WebSocket handshake workflow
----------------------------

This layer now provides a focused helper for the HTTP opening handshake:

- Handlers can call `accept_websocket/3` to validate an incoming normalized
  handshake request and build the matching `101` response.
- Servers that need to stop after one opening handshake can call
  `serve_websocket/4` and inspect whether it returned `accepted(Request,
  Response)` or `rejected(Response)`.
- The optional `protocol(Protocol)` option selects one offered subprotocol.
- The helper is intentionally transport-neutral: it does not take ownership of
  the upgraded stream or begin frame processing.
- Higher layers that need to continue on the upgraded connection must stop at
  a single served message and then take over the underlying stream or socket.
- The `http_websocket_server_service::serve_once/6-7` predicates build on top
  of this handshake helper when server-side code wants a single entry point for
  handshake plus callback-driven session execution.


Current limitations
-------------------

- Only the transport coding sequence `[chunked]` is recognized when reading
  streamed request bodies.
- The `accept_websocket/3` helper is limited to the HTTP opening handshake.
	This layer does not hand upgraded connections off to a frame-processing
	loop, and `serve_connection/3` remains HTTP-request oriented.
- The library does not provide socket accept loops, connection pooling, or
  concurrency management.
- The library assumes binary streams for both input and output.
