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


:- object(http_websocket_client_service(_HTTPTransport_),
	extends(http_websocket_service(_HTTPTransport_, client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Client-side convenience for callback-driven WebSocket sessions with atom text representation, combining the opening handshake, optional initial outbound messages, and the higher-level session loop.',
		parnames is ['HTTPTransport']
	]).

	:- public(open/4).
	:- mode(open(+atom, +object_identifier, -compound, -compound), one_or_error).
	:- info(open/4, [
		comment is 'Builds a WebSocket opening handshake from the given absolute WebSocket URL supported by the selected transport parameterization, opens the upgraded connection, optionally writes initial outbound messages, then runs one callback-driven client session loop until the close handshake completes or the peer closes the stream.',
		argnames is ['URL', 'Handler', 'Response', 'State'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute WebSocket URL' - domain_error(http_client_websocket_url, 'URL'),
			'``URL`` uses an unsupported WebSocket scheme' - domain_error(http_client_websocket_scheme, 'Scheme'),
			'The WebSocket server rejects the opening handshake version' - domain_error(http_client_websocket_version_rejection, 'Response'),
			'The WebSocket server rejects authentication' - domain_error(http_client_websocket_authentication_rejection, 'Response'),
			'The WebSocket server redirects the opening handshake' - domain_error(http_client_websocket_redirection_rejection, 'Response'),
			'The WebSocket server rejects the opening handshake' - domain_error(http_client_websocket_rejection, 'Response'),
			'The WebSocket server response is not a valid opening handshake response' - domain_error(http_client_websocket_response, 'Response'),
			'``Handler`` is a variable' - instantiation_error,
			'``Handler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'Handler'),
			'The upgraded connection handle is invalid' - domain_error(http_socket_transport_connection, 'Connection'),
			'The delegated session loop raises a WebSocket session error' - domain_error(http_websocket_session_sequence, 'Frame'),
			'``Handler`` returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply')
		]
	]).

	:- public(open/5).
	:- mode(open(+atom, +object_identifier, -compound, -compound, +list), one_or_error).
	:- info(open/5, [
		comment is 'Builds a WebSocket opening handshake from the given absolute WebSocket URL supported by the selected transport parameterization, opens the upgraded connection, optionally writes initial outbound messages, then runs one callback-driven client session loop using the given combined handshake and session options.',
		argnames is ['URL', 'Handler', 'Response', 'State', 'Options'],
		exceptions is [
			'``URL`` is a variable' - instantiation_error,
			'``URL`` is not a supported absolute WebSocket URL' - domain_error(http_client_websocket_url, 'URL'),
			'``URL`` uses an unsupported WebSocket scheme' - domain_error(http_client_websocket_scheme, 'Scheme'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid initial message list' - type_error(list, 'Messages'),
			'``Options`` contains an invalid initial message' - domain_error(http_websocket_client_service_initial_message, 'Message'),
			'``Options`` contains an invalid automatic pong setting' - domain_error(http_websocket_client_service_option, auto_pong('Option')),
			'``Options`` contains an invalid client service option' - domain_error(http_websocket_client_service_option, 'Option'),
			'``Options`` contains an invalid WebSocket client option' - domain_error(http_client_websocket_option, 'Option'),
			'``Options`` contains an invalid WebSocket HTTP version' - domain_error(http_client_websocket_version, 'Version'),
			'``Options`` contains reserved WebSocket headers' - domain_error(http_client_websocket_headers, 'Headers'),
			'``Options`` contains an invalid WebSocket service loop option' - domain_error(http_websocket_service_option, 'Option'),
			'Timed session-loop options are not available on this backend' - not_available(http_websocket_service_timing),
			'The WebSocket server rejects the opening handshake version' - domain_error(http_client_websocket_version_rejection, 'Response'),
			'The WebSocket server rejects authentication' - domain_error(http_client_websocket_authentication_rejection, 'Response'),
			'The WebSocket server redirects the opening handshake' - domain_error(http_client_websocket_redirection_rejection, 'Response'),
			'The WebSocket server rejects the opening handshake' - domain_error(http_client_websocket_rejection, 'Response'),
			'The WebSocket server response is not a valid opening handshake response' - domain_error(http_client_websocket_response, 'Response'),
			'``Handler`` is a variable' - instantiation_error,
			'``Handler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'Handler'),
			'The upgraded connection handle is invalid' - domain_error(http_socket_transport_connection, 'Connection'),
			'The delegated session loop raises a WebSocket session error' - domain_error(http_websocket_session_sequence, 'Frame'),
			'``Handler`` returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply')
		],
		remarks is [
			'Repeated options' - 'When the same handshake or session option is given multiple times, the first occurrence is used.',
			'Handshake options' - 'The `headers/1`, `query/1`, `version/1`, `protocols/1`, `key/1`, and `connection_options/1` options are forwarded to `open_websocket/4`. The accepted URL schemes depend on the selected transport parameterization, e.g. `ws://` for `http_socket_transport` and both `ws://` and `wss://` for `http_process_transport`.',
			'Session option ``auto_pong(on|off)``' - 'Controls automatic pong replies during the session loop.',
			'Session option ``initial_messages(Messages)``' - 'Writes the given list of normalized outbound messages before entering the session loop.',
			'Session option ``keepalive_interval(Seconds)``' - 'Schedules empty ping messages when the peer stays silent for the given positive number of seconds. This option requires backend thread support.',
			'Session option ``idle_timeout(Seconds)``' - 'Closes the session with ``status(1001, idle_timeout)`` after the given positive number of seconds without an inbound message. This option requires backend thread support.',
			'Session option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in the session loop. Use a non-negative integer.'
		]
	]).

	:- uses(_HTTPTransport_, [
		close_connection/1, connection_streams/3
	]).

	:- uses(list, [
		member/2, valid/1 as proper_list/1
	]).

	:- uses(user, [
		setup_call_cleanup/3
	]).

	open(URL, Handler, Response, State) :-
		open(URL, Handler, Response, State, []).

	open(URL, Handler, Response, State, Options) :-
		parse_open_options(Options, InitialMessages, SessionOptions, WebSocketOptions),
		http_client::open_websocket(URL, Connection, Response, [transport(_HTTPTransport_)| WebSocketOptions]),
		setup_call_cleanup(
			^^initial_state(State0),
			(	write_initial_messages(Connection, State0, State1, InitialMessages),
				^^run_session_connection(Connection, handler_replies(Handler), State1, State, SessionOptions)
			),
			catch(close_connection(Connection), _, true)
		).

	parse_open_options(Options, InitialMessages, SessionOptions, WebSocketOptions) :-
		check_open_options(Options),
		(	member(initial_messages(InitialMessages0), Options) ->
			validate_initial_messages(InitialMessages0, InitialMessages)
		;	InitialMessages = []
		),
		(	member(auto_pong(AutoPong0), Options) ->
			validate_auto_pong(AutoPong0, AutoPong)
		;	AutoPong = off
		),
		(	member(keepalive_interval(Keepalive0), Options) ->
			validate_interval_option(keepalive_interval, Keepalive0, KeepaliveInterval)
		;	KeepaliveInterval = none
		),
		(	member(idle_timeout(IdleTimeout0), Options) ->
			validate_interval_option(idle_timeout, IdleTimeout0, IdleTimeout)
		;	IdleTimeout = none
		),
		(	member(max_payload_length(MaxPayloadLength0), Options) ->
			^^validate_non_negative_integer_option(http_websocket_client_service_option, max_payload_length, MaxPayloadLength0, MaxPayloadLength)
		;	MaxPayloadLength = none
		),
		build_session_options(AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength, SessionOptions),
		filter_websocket_open_options(Options, WebSocketOptions).

	check_open_options(Options) :-
		(	var(Options) ->
			instantiation_error
		;	proper_list(Options) ->
			true
		;	type_error(list, Options)
		).

	validate_initial_messages(Messages0, Messages) :-
		(	var(Messages0) ->
			instantiation_error
		;	proper_list(Messages0) ->
			validate_initial_message_list(Messages0),
			Messages = Messages0
		;	type_error(list, Messages0)
		).

	validate_initial_message_list([]).
	validate_initial_message_list([Message| Messages]) :-
		(	^^is_message(Message) ->
			true
		;	domain_error(http_websocket_client_service_initial_message, Message)
		),
		validate_initial_message_list(Messages).

	validate_auto_pong(on, on) :-
		!.
	validate_auto_pong(off, off) :-
		!.
	validate_auto_pong(Option, _AutoPong) :-
		domain_error(http_websocket_client_service_option, auto_pong(Option)).

	validate_interval_option(_Name, Interval, Interval) :-
		number(Interval),
		Interval > 0,
		!.
	validate_interval_option(Name, Interval, _ValidatedInterval) :-
		Option =.. [Name, Interval],
		domain_error(http_websocket_client_service_option, Option).

	build_session_options(AutoPong, KeepaliveInterval, IdleTimeout, MaxPayloadLength, SessionOptions) :-
		SessionOptions0 = [auto_pong(AutoPong)],
		prepend_interval_option(keepalive_interval, KeepaliveInterval, SessionOptions0, SessionOptions1),
		prepend_interval_option(idle_timeout, IdleTimeout, SessionOptions1, SessionOptions2),
		prepend_interval_option(max_payload_length, MaxPayloadLength, SessionOptions2, SessionOptions).

	prepend_interval_option(_Name, none, SessionOptions, SessionOptions) :-
		!.
	prepend_interval_option(Name, Interval, SessionOptions, [Option| SessionOptions]) :-
		Option =.. [Name, Interval].

	filter_websocket_open_options([], []).
	filter_websocket_open_options([initial_messages(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([auto_pong(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([keepalive_interval(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([idle_timeout(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([max_payload_length(_)| Options], FilteredOptions) :-
		!,
		filter_websocket_open_options(Options, FilteredOptions).
	filter_websocket_open_options([Option| Options], [Option| FilteredOptions]) :-
		filter_websocket_open_options(Options, FilteredOptions).

	write_initial_messages(_Connection, State, State, []) :-
		!.
	write_initial_messages(Connection, State0, State, Messages) :-
		connection_streams(Connection, _Input, Output),
		write_initial_message_list(Output, State0, State, Messages).

	write_initial_message_list(_Output, State, State, []) :-
		!.
	write_initial_message_list(Output, State0, State, [Message| Messages]) :-
		^^write_message(Output, State0, State1, Message),
		write_initial_message_list(Output, State1, State, Messages).

:- end_object.



:- object(http_websocket_client_service,
	extends(http_websocket_service(http_socket_transport, client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'By default, the client-side convenience for callback-driven WebSocket sessions with atom text representation, combining the opening handshake, optional initial outbound messages, and the higher-level session loop, uses the ``http_socket_transport`` library.'
	]).

:- end_object.
