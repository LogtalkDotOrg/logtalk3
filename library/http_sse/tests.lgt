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


:- object(sse_serve_once_handler,
	implements(http_sse_service_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Helper server session handler used by http_sse wrapper tests.'
	]).

	next([], [sent(greeting)], [event(greeting, hello, none)], continue).
	next([sent(greeting)], done, [data(bye)], stop).

:- end_object.


:- object(sse_open_session_handler,
	implements(http_sse_service_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Helper client session handler used by http_sse wrapper tests.'
	]).

	:- private(events_/1).
	:- dynamic(events_/1).

	handle(Event, stop) :-
		retractall(events_(_)),
		assertz(events_(Event)).

	:- public(last_event/1).
	last_event(Event) :-
		events_(Event).

:- end_object.


:- object(tests(_HTTPTransport_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-27,
		comment is 'Unit tests for the "http_sse" library.'
	]).

	:- uses(http_core, [
		header/3, property/2, status/2
	]).

	:- uses(_HTTPTransport_, [
		close_listener/1, open_listener/4
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	condition(current_object(_HTTPTransport_)).

	cover(http_sse).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_sse_direct_data_2_01, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_direct_exchange(Listener, ServerSession), Tag),
			catch(
				client_direct_exchange(Port, ClientSession),
				Error,
				(	catch(close_listener(Listener), _, true),
					catch(threaded_exit(server_direct_exchange(Listener, _ServerSession), Tag), _, true),
					throw(Error)
				)
			),
			threaded_exit(server_direct_exchange(Listener, ServerSession), Tag),
			catch(close_listener(Listener), _, true),
			ServerSession = session(ServerResponse),
			status(ServerResponse, status(200, 'OK')),
			header(ServerResponse, content_type, media_type('text/event-stream', _)),
			ClientSession = session(ClientResponse, Event1, Event2, Event3, Retry),
			status(ClientResponse, status(200, 'OK')),
			header(ClientResponse, content_type, media_type('text/event-stream', _)),
			Event1 == event(greeting, hello, id1),
			Event2 == event(message, 'line1\nline2', id1),
			Event3 == event(message, bye, id1),
			Retry == 4000.

		% peer-close detection (`receive/2` returning `end_of_file`) relies on the
		% transport promptly propagating a closed connection as a read-side end of
		% file condition. This holds for `http_socket_transport` and is exercised
		% here regardless of the parameter under test; see the NOTES.md "Current
		% scope" section for the corresponding `http_process_transport` caveat.
		test(http_sse_receive_end_of_file_2_01, deterministic) :-
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_immediate_close(Listener), Tag),
			catch(
				client_expect_end_of_file(Port, Event),
				Error,
				(	catch(http_socket_transport::close_listener(Listener), _, true),
					catch(threaded_exit(server_immediate_close(Listener), Tag), _, true),
					throw(Error)
				)
			),
			threaded_exit(server_immediate_close(Listener), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			Event == end_of_file.

		test(http_sse_direct_json_2_01, deterministic(ReplyJSON == JSON)) :-
			JSON = {message-hello, count-1},
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_json_exchange(Listener, JSON), Tag),
			catch(
				client_json_exchange(Port, ReplyJSON),
				Error,
				(	catch(close_listener(Listener), _, true),
					catch(threaded_exit(server_json_exchange(Listener, JSON), Tag), _, true),
					throw(Error)
				)
			),
			threaded_exit(server_json_exchange(Listener, JSON), Tag),
			catch(close_listener(Listener), _, true).

		test(http_sse_direct_term_2_01, deterministic(ReplyTerm == point(1, 2))) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_term_exchange(Listener), Tag),
			catch(
				client_term_exchange(Port, point(1, 2), ReplyTerm),
				Error,
				(	catch(close_listener(Listener), _, true),
					catch(threaded_exit(server_term_exchange(Listener), Tag), _, true),
					throw(Error)
				)
			),
			threaded_exit(server_term_exchange(Listener), Tag),
			catch(close_listener(Listener), _, true).

		test(http_sse_serve_once_6_01, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_serve_once_exchange(Listener, ServerState), Tag),
			catch(
				client_collect_events(Port, 2, ClientEvents),
				Error,
				(	catch(close_listener(Listener), _, true),
					catch(threaded_exit(server_serve_once_exchange(Listener, _ServerState), Tag), _, true),
					throw(Error)
				)
			),
			threaded_exit(server_serve_once_exchange(Listener, ServerState), Tag),
			catch(close_listener(Listener), _, true),
			ServerState == done,
			ClientEvents == [event(greeting, hello, none), event(message, bye, none)].

		test(http_sse_open_session_5_01, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_for_open_session(Listener), Tag),
			catch(
				client_open_session_exchange(Port, ClientResponse, ClientState),
				Error,
				(	catch(close_listener(Listener), _, true),
					catch(threaded_exit(server_for_open_session(Listener), Tag), _, true),
					throw(Error)
				)
			),
			threaded_exit(server_for_open_session(Listener), Tag),
			catch(close_listener(Listener), _, true),
			status(ClientResponse, status(200, 'OK')),
			header(ClientResponse, content_type, media_type('text/event-stream', _)),
			ClientState == closed(id1),
			sse_open_session_handler::last_event(event(message, hello, id1)).

		% auxiliary predicates

		server_direct_exchange(Listener, session(Response)) :-
			http_sse::accept(Listener, SSE, _ClientInfo, [transport(_HTTPTransport_)]),
			http_sse::property(SSE, response(Response)),
			http_sse::send(SSE, event(greeting, hello, id1)),
			http_sse::send_data(SSE, 'line1\nline2'),
			http_sse::send_comment(SSE, heartbeat),
			http_sse::send_retry(SSE, 4000),
			http_sse::close(SSE, data(bye)).

		client_direct_exchange(Port, session(Response, Event1, Event2, Event3, Retry)) :-
			sse_url(Port, URL),
			http_sse::open(URL, SSE, [transport(_HTTPTransport_)]),
			http_sse::property(SSE, response(Response)),
			http_sse::receive(SSE, Event1),
			http_sse::receive(SSE, Event2),
			http_sse::receive(SSE, Event3),
			http_sse::property(SSE, retry(Retry)).

		server_immediate_close(Listener) :-
			http_sse::accept(Listener, SSE, _ClientInfo, [transport(http_socket_transport)]),
			http_sse::close(SSE).

		client_expect_end_of_file(Port, Event) :-
			sse_url(Port, URL),
			http_sse::open(URL, SSE, [transport(http_socket_transport)]),
			http_sse::receive(SSE, Event).

		server_json_exchange(Listener, JSON) :-
			http_sse::accept(Listener, SSE, _ClientInfo, [transport(_HTTPTransport_)]),
			http_sse::send_json(SSE, JSON),
			http_sse::close(SSE).

		client_json_exchange(Port, ReplyJSON) :-
			sse_url(Port, URL),
			http_sse::open(URL, SSE, [transport(_HTTPTransport_)]),
			http_sse::receive_json(SSE, ReplyJSON).

		server_term_exchange(Listener) :-
			http_sse::accept(Listener, SSE, _ClientInfo, [transport(_HTTPTransport_)]),
			http_sse::send_term(SSE, point(1, 2)),
			http_sse::close(SSE).

		client_term_exchange(Port, _Term, ReplyTerm) :-
			sse_url(Port, URL),
			http_sse::open(URL, SSE, [transport(_HTTPTransport_)]),
			http_sse::receive_term(SSE, ReplyTerm).

		server_serve_once_exchange(Listener, ServerState) :-
			http_sse::serve_once(Listener, sse_serve_once_handler, _Response, ServerState, _ClientInfo, [transport(_HTTPTransport_)]).

		client_collect_events(Port, Count, Events) :-
			sse_url(Port, URL),
			http_sse::open(URL, SSE, [transport(_HTTPTransport_)]),
			collect_events(SSE, Count, Events).

		collect_events(_SSE, 0, []) :-
			!.
		collect_events(SSE, Count, [Event| Events]) :-
			Count > 0,
			http_sse::receive(SSE, Event),
			Event \== end_of_file,
			Count1 is Count - 1,
			collect_events(SSE, Count1, Events).

		server_for_open_session(Listener) :-
			http_sse::accept(Listener, SSE, _ClientInfo, [transport(_HTTPTransport_)]),
			http_sse::send(SSE, event(none, hello, id1)),
			http_sse::close(SSE).

		client_open_session_exchange(Port, Response, State) :-
			sse_url(Port, URL),
			http_sse::open_session(URL, sse_open_session_handler, Response, State, [transport(_HTTPTransport_), reconnect(off)]).

		sse_url(Port, URL) :-
			atomic_list_concat(['http://127.0.0.1:', Port, '/events'], URL).

	:- endif.

:- end_object.
