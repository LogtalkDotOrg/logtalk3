%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% The server accepts one SSE client using the high-level porcelain API,
% advertises a reconnection time, and pushes a short sequence of named
% "tick" events before ending the stream. Server-Sent Events only flow from
% server to client, so, unlike a WebSocket echo, there is nothing for the
% server to read back from the connection.

:- object(http_sse_ticker_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Small SSE ticker server used by the example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, -compound), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener, serves one SSE client with a short tick sequence, and returns the response plus the sent events.',
		argnames is ['Port', 'Session']
	]).

	:- public(serve_listener/2).
	:- mode(serve_listener(+compound, -compound), one_or_error).
	:- info(serve_listener/2, [
		comment is 'Serves one SSE client on an already opened listener with a short tick sequence and returns the response plus the sent events.',
		argnames is ['Listener', 'Session']
	]).

	% This convenience predicate is useful when the server is run in its own
	% session. It opens the listener, waits for one client, serves that
	% client, and then closes the listener.
	serve(Port, Session) :-
		http_server::open('127.0.0.1', Port, Server, []),
		http_server::server_property(Server, listener(Listener)),
		catch(
			serve_listener(Listener, Session),
			Error,
			(	catch(http_server::close(Server), _, true),
				throw(Error)
			)
		),
		http_server::close(Server).

	% The `retry/1` option tells the client, as soon as the connection is
	% accepted, how long to wait before reconnecting should the connection
	% drop; a real ticker would otherwise resend events the client already
	% received using `last_event_id/1` on the next `open/3`.
	serve_listener(Listener, session(Response, SentEvents)) :-
		http_sse::accept(Listener, SSE, _ClientInfo, [transport(http_socket_transport), retry(2000)]),
		http_sse::property(SSE, response(Response)),
		send_ticks(SSE, 1, 3, SentEvents),
		http_sse::close(SSE).

	send_ticks(_SSE, Number, Count, []) :-
		Number > Count,
		!.
	send_ticks(SSE, Number, Count, [event(tick, Text, none)| Events]) :-
		Number =< Count,
		tick_text(Number, Text),
		http_sse::send_event(SSE, tick, Text),
		Number1 is Number + 1,
		send_ticks(SSE, Number1, Count, Events).

	tick_text(Number, Text) :-
		number_codes(Number, Codes),
		atom_codes(NumberAtom, Codes),
		atom_concat('tick ', NumberAtom, Text).

:- end_object.


% The direct client assumes an SSE server is already running. It opens an
% SSE connection with the porcelain API and reads events, one at a time,
% until the server ends the stream, which `receive/2` reports by
% returning `end_of_file` (and, at that point, the handle is already
% closed and deregistered, so no explicit `close/1` is needed).

:- object(http_sse_ticker_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'SSE client used by the ticker example.'
	]).

	:- public(run/2).
	:- mode(run(+integer, -compound), one_or_error).
	:- info(run/2, [
		comment is 'Connects to the local SSE server and collects every event up to the end of the stream.',
		argnames is ['Port', 'Session']
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	run(Port, session(Response, ReceivedEvents)) :-
		sse_url(Port, URL),
		http_sse::open(URL, SSE, [transport(http_socket_transport)]),
		http_sse::property(SSE, response(Response)),
		catch(
			collect_events(SSE, ReceivedEvents),
			Error,
			(	catch(http_sse::close(SSE), _, true),
				throw(Error)
			)
		).

	collect_events(SSE, [Event| Events]) :-
		http_sse::receive(SSE, Event),
		Event \== end_of_file,
		!,
		collect_events(SSE, Events).
	collect_events(_SSE, []).

	% The client uses the higher-level http:// URL facade instead of
	% constructing the request manually.
	sse_url(Port, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, '/ticks'], URL).

:- end_object.


% The demo object keeps the example self-contained when backend threads are
% available: one thread runs the SSE server while the main thread runs the
% direct client. This mirrors the http_websocket_echo demo but focused on
% the high-level SSE open/accept/send/receive flow instead of the
% two-way WebSocket message exchange.

:- object(http_sse_ticker_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Self-contained demo object for the SSE ticker example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.'
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns both the server-side and client-side session summaries when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		% The demo opens an ephemeral port first so the client can connect to
		% a known endpoint while the server thread blocks waiting for the
		% request.
		run(Result) :-
			http_server::open('127.0.0.1', Port, Server, []),
			http_server::server_property(Server, listener(Listener)),
			threaded_once(http_sse_ticker_server::serve_listener(Listener, ServerSession), Tag),
			catch(
				http_sse_ticker_client::run(Port, ClientSession),
				Error,
				(	cleanup_demo(Server, Tag),
					throw(Error)
				)
			),
			http_server::request_listener_shutdown(Server),
			threaded_exit(http_sse_ticker_server::serve_listener(Listener, ServerSession), Tag),
			catch(http_server::close(Server), _, true),
			Result = result(ServerSession, ClientSession).

		cleanup_demo(Server, Tag) :-
			http_server::server_property(Server, listener(Listener)),
			http_server::request_listener_shutdown(Server),
			catch(threaded_exit(http_sse_ticker_server::serve_listener(Listener, _ServerSession), Tag), _, true),
			catch(http_server::close(Server), _, true).

		print_result(result(_ServerSession, session(_Response, Events))) :-
			print_events(Events).

		print_events([]).
		print_events([event(_Type, Text, _Id)| Events]) :-
			write('Received SSE event: '), write(Text), nl,
			print_events(Events).

	:- else.

		run :-
			write('This demo needs backend thread support. Run http_sse_ticker_server::serve/2 and http_sse_ticker_client::run/2 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_sse_ticker_demo::run/1)).

	:- endif.

:- end_object.
