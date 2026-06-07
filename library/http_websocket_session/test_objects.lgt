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


:- object(websocket_http_websocket_session_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-07,
		comment is 'WebSocket handshake handler used by the http_websocket_session tests.'
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, [protocol(chat)]).

:- end_object.


:- object(websocket_session_loop_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-07,
		comment is 'Callback object used by the higher-level session-loop tests.'
	]).

	handle(message(text, hello), [message(text, ok)]) :-
		!.
	handle(message(ping, _Payload), []) :-
		!.
	handle(message(close, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.


:- object(websocket_client_session_loop_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-07,
		comment is 'Callback object used by the client-side handshake-plus-session convenience tests.'
	]).

	handle(message(text, ok), [message(close, status(1000))]) :-
		!.
	handle(message(ping, _Payload), []) :-
		!.
	handle(message(close, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.


:- object(websocket_keepalive_close_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-07,
		comment is 'Callback object used to capture a client keepalive ping and then close the session.'
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Clears the last captured message.'
	]).

	:- public(captured/1).
	:- mode(captured(-compound), zero_or_one).
	:- info(captured/1, [
		comment is 'Returns the last captured message.',
		argnames is ['Message']
	]).

	:- private(captured_/1).
	:- dynamic(captured_/1).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			reset/0,
			record_/1,
			captured/1
		]).
	:- endif.

	reset :-
		retractall(captured_(_)).

	captured(Message) :-
		captured_(Message).

	handle(message(ping, Payload), [message(close, status(1000))]) :-
		!,
		record_(message(ping, Payload)).
	handle(message(close, _Payload), []) :-
		!.
	handle(_Message, []).

	record_(Message) :-
		retractall(captured_(_)),
		assertz(captured_(Message)).

:- end_object.


:- object(websocket_broadcast_session_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-07,
		comment is 'Callback object used by the registry-backed broadcast server tests.'
	]).

	handle(message(text, Text), [broadcast_others(message(text, Text))]) :-
		!.
	handle(message(ping, _Payload), []) :-
		!.
	handle(message(close, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.


:- object(websocket_close_broadcast_session_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Callback object used to verify that registry actions are skipped when a peer close is received.'
	]).

	handle(message(close, _Payload), [broadcast_others(message(text, should_not_broadcast))]) :-
		!.
	handle(message(ping, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.


:- object(websocket_close_then_broadcast_session_handler,
	implements(http_websocket_session_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Callback object used to verify that queued registry messages are not flushed after sending a close frame.'
	]).

	handle(message(text, hello), [reply(message(close, status(1000))), broadcast(message(text, should_not_flush))]) :-
		!.
	handle(message(close, _Payload), []) :-
		!.
	handle(message(ping, _Payload), []) :-
		!.
	handle(_Message, []).

:- end_object.
