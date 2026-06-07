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


:- object(websocket_echo_session_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-04,
		comment is 'Helper session handler used by http_websocket wrapper tests.'
	]).

	handle(message(text, Text), [message(text, Text)]) :-
		!.
	handle(message(close, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.


:- object(websocket_close_after_echo_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-04,
		comment is 'Helper client session handler that closes after receiving one text reply.'
	]).

	handle(message(text, _Text), [message(close, status(1000, done))]) :-
		!.
	handle(message(close, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-04,
		comment is 'Unit tests for the "http_websocket" library.'
	]).

	:- uses(http_core, [
		property/2, status/2
	]).

	:- uses(http_websocket_messages, [
		message/3 as normalized_message/3
	]).

	cover(http_websocket).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_websocket_direct_text_2_01, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_accept_text_exchange(Listener, ServerSession), Tag),
			catch(
				client_direct_text_exchange(Port, ClientSession),
				Error,
				(	catch(http_socket::close_listener(Listener), _, true),
					catch(once(threaded_exit(server_accept_text_exchange(Listener, _ServerSession), Tag)), _, true),
					throw(Error)
				)
			),
			once(threaded_exit(server_accept_text_exchange(Listener, ServerSession), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			ServerSession = session(ServerResponse, server, chat, message(text, hello), message(close, status(1000, done))),
			ClientSession = session(ClientResponse, client, chat, message(text, hello), message(text, 'Echo: hello')),
			status(ServerResponse, status(101, 'Switching Protocols')),
			property(ServerResponse, websocket_protocol([chat])),
			status(ClientResponse, status(101, 'Switching Protocols')),
			property(ClientResponse, websocket_protocol([chat])).

		test(http_websocket_direct_json_2_01, deterministic(ReplyJSON == JSON)) :-
			JSON = {message-hello, count-1},
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_accept_json_exchange(Listener, _ReceivedJSON), Tag),
			catch(
				client_direct_json_exchange(Port, JSON, ReplyJSON),
				Error,
				(	catch(http_socket::close_listener(Listener), _, true),
					catch(once(threaded_exit(server_accept_json_exchange(Listener, _ServerReceivedJSON), Tag)), _, true),
					throw(Error)
				)
			),
			once(threaded_exit(server_accept_json_exchange(Listener, _ReceivedJSON), Tag)),
			catch(http_socket::close_listener(Listener), _, true).

		test(http_websocket_direct_term_2_01, deterministic(ReplyTerm == hello(world, 42))) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_accept_term_exchange(Listener, _ReceivedTerm), Tag),
			catch(
				client_direct_term_exchange(Port, hello(world, 42), ReplyTerm),
				Error,
				(	catch(http_socket::close_listener(Listener), _, true),
					catch(once(threaded_exit(server_accept_term_exchange(Listener, _ServerReceivedTerm), Tag)), _, true),
					throw(Error)
				)
			),
			once(threaded_exit(server_accept_term_exchange(Listener, _ReceivedTerm), Tag)),
			catch(http_socket::close_listener(Listener), _, true).

		test(http_websocket_open_session_5_01, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_accept_for_open_session(Listener, ServerSession), Tag),
			catch(
				client_open_session_exchange(Port, ClientResponse, ClientState),
				Error,
				(	catch(http_socket::close_listener(Listener), _, true),
					catch(once(threaded_exit(server_accept_for_open_session(Listener, _ServerSession), Tag)), _, true),
					throw(Error)
				)
			),
			once(threaded_exit(server_accept_for_open_session(Listener, ServerSession), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			ServerSession = session(ServerResponse, message(text, hello), message(close, status(1000, done))),
			status(ServerResponse, status(101, 'Switching Protocols')),
			property(ServerResponse, websocket_protocol([chat])),
			status(ClientResponse, status(101, 'Switching Protocols')),
			property(ClientResponse, websocket_protocol([chat])),
			ClientState = session_state(idle, closed(status(1000, done), status(1000, done))).

		test(http_websocket_serve_once_6_01, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(server_serve_once_exchange(Listener, ServerResponse, ServerState), Tag),
			catch(
				client_direct_text_exchange(Port, ClientSession),
				Error,
				(	catch(http_socket::close_listener(Listener), _, true),
					catch(once(threaded_exit(server_serve_once_exchange(Listener, _Response, _State), Tag)), _, true),
					throw(Error)
				)
			),
			once(threaded_exit(server_serve_once_exchange(Listener, ServerResponse, ServerState), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			ClientSession = session(ClientResponse, client, chat, message(text, hello), message(text, hello)),
			status(ServerResponse, status(101, 'Switching Protocols')),
			property(ServerResponse, websocket_protocol([chat])),
			ServerState = session_state(idle, closed(status(1000, done), status(1000, done))),
			status(ClientResponse, status(101, 'Switching Protocols')),
			property(ClientResponse, websocket_protocol([chat])).

		% auxiliary predicates

		server_accept_text_exchange(Listener, session(Response, Role, Protocol, ReceivedMessage, CloseMessage)) :-
			http_websocket::accept(Listener, WebSocket, _ClientInfo, [protocol(chat)]),
			http_websocket::property(WebSocket, role(Role)),
			http_websocket::property(WebSocket, response(Response)),
			http_websocket::property(WebSocket, subprotocol(Protocol)),
			http_websocket::receive(WebSocket, ReceivedMessage),
			echo_reply_message(ReceivedMessage, ReplyMessage),
			http_websocket::send(WebSocket, ReplyMessage),
			http_websocket::receive(WebSocket, CloseMessage).

		client_direct_text_exchange(Port, session(Response, Role, Protocol, SentMessage, ReplyMessage)) :-
			websocket_url(Port, URL),
			normalized_message(text, hello, SentMessage),
			http_websocket::open(URL, WebSocket, [protocols([chat])]),
			http_websocket::property(WebSocket, role(Role)),
			http_websocket::property(WebSocket, response(Response)),
			http_websocket::property(WebSocket, subprotocol(Protocol)),
			http_websocket::send(WebSocket, SentMessage),
			http_websocket::receive(WebSocket, ReplyMessage),
			http_websocket::close(WebSocket, status(1000, done)).

		server_accept_json_exchange(Listener, ReceivedJSON) :-
			http_websocket::accept(Listener, WebSocket, _ClientInfo, []),
			http_websocket::receive_json(WebSocket, ReceivedJSON),
			http_websocket::send_json(WebSocket, ReceivedJSON),
			http_websocket::receive(WebSocket, _CloseMessage).

		client_direct_json_exchange(Port, JSON, ReplyJSON) :-
			websocket_url(Port, URL),
			http_websocket::open(URL, WebSocket, []),
			http_websocket::send_json(WebSocket, JSON),
			http_websocket::receive_json(WebSocket, ReplyJSON),
			http_websocket::close(WebSocket, status(1000, done)).

		server_accept_term_exchange(Listener, ReceivedTerm) :-
			http_websocket::accept(Listener, WebSocket, _ClientInfo, []),
			http_websocket::receive_term(WebSocket, ReceivedTerm),
			http_websocket::send_term(WebSocket, ReceivedTerm),
			http_websocket::receive(WebSocket, _CloseMessage).

		client_direct_term_exchange(Port, Term, ReplyTerm) :-
			websocket_url(Port, URL),
			http_websocket::open(URL, WebSocket, []),
			http_websocket::send_term(WebSocket, Term),
			http_websocket::receive_term(WebSocket, ReplyTerm),
			http_websocket::close(WebSocket, status(1000, done)).

		server_accept_for_open_session(Listener, session(Response, ReceivedMessage, CloseMessage)) :-
			http_websocket::accept(Listener, WebSocket, _ClientInfo, [protocol(chat)]),
			http_websocket::property(WebSocket, response(Response)),
			http_websocket::receive(WebSocket, ReceivedMessage),
			echo_reply_message(ReceivedMessage, ReplyMessage),
			http_websocket::send(WebSocket, ReplyMessage),
			http_websocket::receive(WebSocket, CloseMessage).

		client_open_session_exchange(Port, Response, State) :-
			websocket_url(Port, URL),
			http_websocket::open_session(URL, websocket_close_after_echo_handler, Response, State, [protocols([chat]), initial_messages([message(text, hello)])]).

		server_serve_once_exchange(Listener, Response, State) :-
			http_websocket::serve_once(Listener, websocket_echo_session_handler, Response, State, _ClientInfo, [protocol(chat)]).

		websocket_url(Port, URL) :-
			number_codes(Port, PortCodes),
			atom_codes(PortAtom, PortCodes),
			atom_concat('ws://127.0.0.1:', PortAtom, Prefix),
			atom_concat(Prefix, '/echo', URL).

		echo_reply_message(message(text, Text), ReplyMessage) :-
			atom_concat('Echo: ', Text, ReplyText),
			normalized_message(text, ReplyText, ReplyMessage).

	:- endif.

:- end_object.
