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

% The server accepts one WebSocket client using the high-level porcelain API,
% reads one text message, writes one text reply, then waits for the client
% close frame. Keeping the exchange to one message in each direction makes the
% control flow easy to follow.

:- object(websocket_echo_server).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Small WebSocket echo server used by the example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, -compound), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener, serves one WebSocket client, and returns the handshake plus the exchanged messages.',
		argnames is ['Port', 'Session']
	]).

	:- public(serve_listener/2).
	:- mode(serve_listener(+compound, -compound), one_or_error).
	:- info(serve_listener/2, [
		comment is 'Serves one WebSocket client on an already opened listener and returns the handshake plus the exchanged messages.',
		argnames is ['Listener', 'Session']
	]).

	% This convenience predicate is useful when the server is run in its own
	% session. It opens the listener, waits for one client, serves that client,
	% and then closes the listener.
	serve(Port, Session) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			serve_listener(Listener, Session),
			Error,
			(	catch(http_socket::close_listener(Listener), _, true),
				throw(Error)
			)
		),
		http_socket::close_listener(Listener).

	serve_listener(Listener, session(HandshakeResponse, ReceivedMessage, ReplyMessage)) :-
		http_websocket::accept(Listener, WebSocket, _ClientInfo, [protocol(chat)]),
		http_websocket::property(WebSocket, response(HandshakeResponse)),
		http_websocket::receive(WebSocket, ReceivedMessage),
		echo_reply_message(ReceivedMessage, ReplyMessage),
		http_websocket::send(WebSocket, ReplyMessage),
		http_websocket::receive(WebSocket, _CloseMessage).

	% The example focuses on text messages. The server turns the received text
	% into a new text message so both reading and writing are visible.
	echo_reply_message(message(text, Text), ReplyMessage) :-
		atom_concat('Echo: ', Text, ReplyText),
		http_websocket_messages::message(text, ReplyText, ReplyMessage).

:- end_object.


% The direct client assumes a WebSocket server is already running. It opens a
% WebSocket connection with the porcelain API, sends one text message, reads one
% reply, and then closes the connection.

:- object(websocket_echo_client).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'WebSocket client used by the echo example.'
	]).

	:- public(run/3).
	:- mode(run(+integer, +atom, -compound), one_or_error).
	:- info(run/3, [
		comment is 'Connects to the local WebSocket server, sends one text message, reads one reply, and returns the handshake plus the exchanged messages.',
		argnames is ['Port', 'Text', 'Session']
	]).

	run(Port, Text, session(HandshakeResponse, SentMessage, ReplyMessage)) :-
		websocket_url(Port, URL),
		http_websocket::open(URL, WebSocket, [protocols([chat])]),
		http_websocket::property(WebSocket, response(HandshakeResponse)),
		catch(
			client_exchange(WebSocket, Text, SentMessage, ReplyMessage),
			Error,
			(	catch(http_websocket::close(WebSocket), _, true),
				throw(Error)
			)
		).

	client_exchange(WebSocket, Text, SentMessage, ReplyMessage) :-
		http_websocket_messages::message(text, Text, SentMessage),
		http_websocket::send(WebSocket, SentMessage),
		http_websocket::receive(WebSocket, ReplyMessage),
		http_websocket::close(WebSocket, status(1000, done)).

	% The client uses the higher-level ws:// URL facade instead of constructing
	% the opening handshake request manually.
	websocket_url(Port, URL) :-
		number_codes(Port, PortCodes),
		atom_codes(PortAtom, PortCodes),
		atom_concat('ws://127.0.0.1:', PortAtom, Prefix),
		atom_concat(Prefix, '/echo', URL).

:- end_object.


% The demo object keeps the example self-contained when backend threads are
% available: one thread runs the WebSocket server while the main thread runs
% the direct client. This is similar in spirit to the http_open_api demo but
% focused on the high-level WebSocket open/send/receive flow instead of the
% lower-level request/response plumbing.

:- object(http_websocket_echo_demo).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Self-contained demo object for the WebSocket echo example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
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

		% The demo opens an ephemeral port first so the client can connect to a
		% known endpoint while the server thread blocks waiting for the handshake.
		run(Result) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(websocket_echo_server::serve_listener(Listener, ServerSession), Tag),
			catch(
				websocket_echo_client::run(Port, hello, ClientSession),
				Error,
				(	cleanup_demo(Listener, Tag),
					throw(Error)
				)
			),
			once(threaded_exit(websocket_echo_server::serve_listener(Listener, ServerSession), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			Result = result(ServerSession, ClientSession).

		cleanup_demo(Listener, Tag) :-
			catch(http_socket::close_listener(Listener), _, true),
			catch(once(threaded_exit(websocket_echo_server::serve_listener(Listener, _ServerSession), Tag)), _, true).

		print_result(result(_ServerSession, session(_HandshakeResponse, SentMessage, ReplyMessage))) :-
			http_websocket_messages::payload(SentMessage, SentText),
			http_websocket_messages::payload(ReplyMessage, ReplyText),
			write('Sent WebSocket message: '),
			write(SentText),
			nl,
			write('Received WebSocket reply: '),
			write(ReplyText),
			nl.

	:- else.

		run :-
			write('This demo needs backend thread support. Run websocket_echo_server::serve/2 and websocket_echo_client::run/3 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_websocket_echo_demo::run/1)).

	:- endif.

:- end_object.
