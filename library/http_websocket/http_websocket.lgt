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


:- object(http_websocket,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-04,
		comment is 'High-level WebSocket predicates for opening and closing connections, exchanging messages, and running common client and server session loops.'
	]).

	:- public(open/2).
	:- mode(open(+atom, -compound), one_or_error).
	:- info(open/2, [
		comment is 'Opens a client WebSocket connection to the given ``ws://`` or ``wss://`` URL and returns an opaque handle managed by this object.',
		argnames is ['URL', 'WebSocket']
	]).

	:- public(open/3).
	:- mode(open(+atom, -compound, +list), one_or_error).

	:- info(open/3, [
		comment is 'Opens a client WebSocket connection to the given URL, returning an opaque handle. Client handshake options are forwarded to ``http_client::open_websocket/4``. The direct API also accepts ``auto_pong(on|off)`` and ``max_payload_length(Bytes|none)`` options.',
		argnames is ['URL', 'WebSocket', 'Options']
	]).

	:- public(accept/3).
	:- mode(accept(+compound, -compound, -compound), one_or_error).
	:- info(accept/3, [
		comment is 'Accepts one incoming WebSocket connection on the given listener using the default opening-handshake policy and returns an opaque server-side handle together with the accepted client information.',
		argnames is ['Listener', 'WebSocket', 'ClientInfo']
	]).

	:- public(accept/4).
	:- mode(accept(+compound, -compound, -compound, +list), one_or_error).

	:- info(accept/4, [
		comment is 'Accepts one incoming WebSocket connection on the given listener and returns an opaque server-side handle. Server opening-handshake options are forwarded to ``http_server::accept_websocket/3``. The direct API also accepts ``auto_pong(on|off)`` and ``max_payload_length(Bytes|none)`` options.',
		argnames is ['Listener', 'WebSocket', 'ClientInfo', 'Options']
	]).

	:- public(send/2).
	:- mode(send(+compound, +term), one_or_error).
	:- info(send/2, [
		comment is 'Writes one outbound WebSocket message using the opaque handle. Accepts normalized ``message(Type, Payload)`` terms and the convenience wrappers ``text(Text)``, ``binary(Bytes)``, ``ping(Bytes)``, ``pong(Bytes)``, ``close(Payload)``, ``json(JSON)``, and ``term(Term)``.',
		argnames is ['WebSocket', 'Message']
	]).

	:- public(send/3).
	:- mode(send(+compound, +term, +list), one_or_error).
	:- info(send/3, [
		comment is 'Writes one outbound WebSocket message using the opaque handle and the given write options, forwarding those options to the underlying session ``write_message/5`` predicate.',
		argnames is ['WebSocket', 'Message', 'Options']
	]).

	:- public(receive/2).
	:- mode(receive(+compound, -term), one_or_error).
	:- info(receive/2, [
		comment is 'Reads the next normalized WebSocket message using the opaque handle. Returns ``end_of_file`` when the peer closes the transport before another message is available.',
		argnames is ['WebSocket', 'Message']
	]).

	:- public(receive/3).
	:- mode(receive(+compound, -term, +list), one_or_error).
	:- info(receive/3, [
		comment is 'Reads the next normalized WebSocket message using the opaque handle and the given read options. The direct API accepts per-call ``auto_pong(on|off)`` and ``max_payload_length(Bytes|none)`` overrides.',
		argnames is ['WebSocket', 'Message', 'Options']
	]).

	:- public(close/1).
	:- mode(close(+compound), one_or_error).
	:- info(close/1, [
		comment is 'Best-effort graceful close of the WebSocket handle using the normal close payload ``empty``.',
		argnames is ['WebSocket']
	]).

	:- public(close/2).
	:- mode(close(+compound, +term), one_or_error).
	:- info(close/2, [
		comment is 'Best-effort graceful close of the WebSocket handle using the given close payload, which must be valid for a normalized ``message(close, Payload)`` term.',
		argnames is ['WebSocket', 'Payload']
	]).

	:- public(property/2).
	:- mode(property(+compound, ?compound), zero_or_more).
	:- info(property/2, [
		comment is 'Enumerates properties of an open opaque WebSocket handle. Supported properties are ``role(Role)``, ``response(Response)``, ``connection(Connection)``, ``state(State)``, ``client_info(ClientInfo)``, and ``subprotocol(Protocol)``.',
		argnames is ['WebSocket', 'Property']
	]).

	:- public(send_text/2).
	:- mode(send_text(+compound, +atom), one_or_error).
	:- info(send_text/2, [
		comment is 'Convenience wrapper for sending one text message.',
		argnames is ['WebSocket', 'Text']
	]).

	:- public(receive_text/2).
	:- mode(receive_text(+compound, -term), one_or_error).
	:- info(receive_text/2, [
		comment is 'Reads the next message and returns its text payload when the message type is ``text``. Returns ``end_of_file`` unchanged.',
		argnames is ['WebSocket', 'Text']
	]).

	:- public(send_binary/2).
	:- mode(send_binary(+compound, +list(byte)), one_or_error).
	:- info(send_binary/2, [
		comment is 'Convenience wrapper for sending one binary message.',
		argnames is ['WebSocket', 'Bytes']
	]).

	:- public(receive_binary/2).
	:- mode(receive_binary(+compound, -term), one_or_error).
	:- info(receive_binary/2, [
		comment is 'Reads the next message and returns its binary payload when the message type is ``binary``. Returns ``end_of_file`` unchanged.',
		argnames is ['WebSocket', 'Bytes']
	]).

	:- public(send_json/2).
	:- mode(send_json(+compound, +nonvar), one_or_error).
	:- info(send_json/2, [
		comment is 'Serializes a JSON term as UTF-8 text and sends it as one text message. Uses curly terms for parsed JSON objects, dashes for parsed JSON pairs, and atoms for parsed JSON strings.',
		argnames is ['WebSocket', 'JSON']
	]).

	:- public(receive_json/2).
	:- mode(receive_json(+compound, -term), one_or_error).
	:- info(receive_json/2, [
		comment is 'Reads the next text message and decodes its payload as JSON. Returns ``end_of_file`` unchanged. Uses curly terms for parsed JSON objects, dashes for parsed JSON pairs, and atoms for parsed JSON strings.',
		argnames is ['WebSocket', 'JSON']
	]).

	:- public(send_term/2).
	:- mode(send_term(+compound, +term), one_or_error).
	:- info(send_term/2, [
		comment is 'Serializes a Prolog term using canonical write options and sends it as one text message.',
		argnames is ['WebSocket', 'Term']
	]).

	:- public(receive_term/2).
	:- mode(receive_term(+compound, -term), one_or_error).
	:- info(receive_term/2, [
		comment is 'Reads the next text message and parses its payload as one Prolog term. Returns ``end_of_file`` unchanged.',
		argnames is ['WebSocket', 'Term']
	]).

	:- public(open_session/4).
	:- mode(open_session(+atom, +object_identifier, -compound, -compound), one_or_error).

	:- info(open_session/4, [
		comment is 'Convenience wrapper over ``http_websocket_client_session::open/4`` that keeps the user in the high-level ``http_websocket`` surface for callback-driven client sessions.',
		argnames is ['URL', 'Handler', 'Response', 'State']
	]).

	:- public(open_session/5).
	:- mode(open_session(+atom, +object_identifier, -compound, -compound, +list), one_or_error).

	:- info(open_session/5, [
		comment is 'Convenience wrapper over ``http_websocket_client_session::open/5``.',
		argnames is ['URL', 'Handler', 'Response', 'State', 'Options']
	]).

	:- public(serve_once/5).
	:- mode(serve_once(+compound, +object_identifier, -compound, -compound, -compound), one_or_error).
	:- info(serve_once/5, [
		comment is 'Convenience wrapper that accepts one incoming WebSocket connection on the given listener, performs the opening handshake using the default server policy, and runs one callback-driven server session.',
		argnames is ['Listener', 'Handler', 'Response', 'State', 'ClientInfo']
	]).

	:- public(serve_once/6).
	:- mode(serve_once(+compound, +object_identifier, -compound, -compound, -compound, +list), one_or_error).
	:- info(serve_once/6, [
		comment is 'Convenience wrapper that accepts one incoming WebSocket connection on the given listener and runs one callback-driven server session using the given combined handshake and session options.',
		argnames is ['Listener', 'Handler', 'Response', 'State', 'ClientInfo', 'Options']
	]).

	:- private(handle_seed_/1).
	:- dynamic(handle_seed_/1).
	:- mode(handle_seed_(?positive_integer), zero_or_one).
	:- info(handle_seed_/1, [
		comment is 'Last allocated opaque WebSocket handle identifier.',
		argnames is ['HandleId']
	]).

	:- private(handle_state_/8).
	:- dynamic(handle_state_/8).
	:- mode(handle_state_(?positive_integer, ?atom, ?compound, ?compound, ?term, ?compound, ?atom, ?term), zero_or_more).
	:- info(handle_state_/8, [
		comment is 'Stored opaque WebSocket handle state.',
		argnames is [
			'HandleId', 'Role', 'Connection', 'Response', 'ClientInfo', 'State', 'AutoPong',
			'MaxPayloadLength'
		]
	]).

	:- synchronized([
		allocate_handle_id/1, register_handle/8, update_handle_state/2, retract_handle_state/2,
		handle_id_outcome/2
	]).

	:- uses(list, [
		member/2, valid/1 as proper_list/1
	]).

	:- uses(http_client, [
		open_websocket/4 as open_client_websocket/4
	]).

	:- uses(http_core, [
		property/2 as http_property/2
	]).

	:- uses(http_socket, [
		close_connection/1, connection_streams/3, serve_websocket_once/5
	]).

	:- uses(http_websocket_client_session, [
		initial_state/1 as client_initial_state/1, open/5 as open_client_session/5,
		read_message/6 as client_read_message/6, write_message/5 as client_write_message/5
	]).

	:- uses(http_websocket_messages, [
		is_message/1, message/3 as websocket_message/3
	]).

	:- uses(http_websocket_server_session, [
		initial_state/1 as server_initial_state/1, read_message/6 as server_read_message/6,
		serve_once/7 as serve_server_session/7, write_message/5 as server_write_message/5
	]).

	:- uses(json, [
		parse/2 as json_decode/2, generate/2 as json_encode/2
	]).

	:- uses(term_io, [
		read_term_from_atom/3, write_term_to_atom/3
	]).

	open(URL, WebSocket) :-
		open(URL, WebSocket, []).

	open(URL, WebSocket, Options) :-
		parse_direct_options(Options, AutoPong, MaxPayloadLength, WebSocketOptions),
		open_client_websocket(URL, Connection, Response, WebSocketOptions),
		client_initial_state(State),
		catch(
			register_new_handle(client, Connection, Response, none, State, AutoPong, MaxPayloadLength, WebSocket),
			Error,
			(	catch(close_connection(Connection), _, true),
				throw(Error)
			)
		).

	accept(Listener, WebSocket, ClientInfo) :-
		accept(Listener, WebSocket, ClientInfo, []).

	accept(Listener, WebSocket, ClientInfo, Options) :-
		parse_direct_options(Options, AutoPong, MaxPayloadLength, AcceptOptions),
		serve_websocket_once(Listener, http_websocket_accept_handler(AcceptOptions), Connection, Response, ClientInfo),
		server_initial_state(State),
		catch(
			register_new_handle(server, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength, WebSocket),
			Error,
			(	catch(close_connection(Connection), _, true),
				throw(Error)
			)
		).

	send(WebSocket, Message) :-
		send(WebSocket, Message, []).

	send(WebSocket, Message0, Options) :-
		normalize_outbound_message(Message0, Message),
		with_handle_state(WebSocket, Role, Connection, _Response, _ClientInfo, State0, _AutoPong, _MaxPayloadLength),
		connection_streams(Connection, _Input, Output),
		catch(
			role_write_message(Role, Output, State0, State, Message, Options),
			Error,
			(	best_effort_close(WebSocket),
				throw(Error)
			)
		),
		update_or_close_handle(WebSocket, Connection, State).

	receive(WebSocket, Message) :-
		receive(WebSocket, Message, []).

	receive(WebSocket, Message, Options) :-
		with_handle_state(WebSocket, Role, Connection, _Response, _ClientInfo, State0, AutoPong0, MaxPayloadLength0),
		parse_read_override_options(Options, AutoPong0, MaxPayloadLength0, ReadOptions),
		connection_streams(Connection, Input, Output),
		catch(
			role_read_message(Role, Input, Output, State0, State, Message, ReadOptions),
			Error,
			(	best_effort_close(WebSocket),
				throw(Error)
			)
		),
		update_or_close_after_receive(WebSocket, Connection, State, Message).

	close(WebSocket) :-
		close(WebSocket, empty).

	close(WebSocket, Payload) :-
		normalize_close_message(Payload, Message),
		close_handle(WebSocket, Message).

	property(WebSocket, role(Role)) :-
		with_handle_state(WebSocket, Role, _Connection, _Response, _ClientInfo, _State, _AutoPong, _MaxPayloadLength).
	property(WebSocket, response(Response)) :-
		with_handle_state(WebSocket, _Role, _Connection, Response, _ClientInfo, _State, _AutoPong, _MaxPayloadLength).
	property(WebSocket, connection(Connection)) :-
		with_handle_state(WebSocket, _Role, Connection, _Response, _ClientInfo, _State, _AutoPong, _MaxPayloadLength).
	property(WebSocket, state(State)) :-
		with_handle_state(WebSocket, _Role, _Connection, _Response, _ClientInfo, State, _AutoPong, _MaxPayloadLength).
	property(WebSocket, client_info(ClientInfo)) :-
		with_handle_state(WebSocket, server, _Connection, _Response, ClientInfo, _State, _AutoPong, _MaxPayloadLength).
	property(WebSocket, subprotocol(Protocol)) :-
		with_handle_state(WebSocket, _Role, _Connection, Response, _ClientInfo, _State, _AutoPong, _MaxPayloadLength),
		http_property(Response, websocket_protocol([Protocol])).

	send_text(WebSocket, Text) :-
		send(WebSocket, text(Text)).

	receive_text(WebSocket, Text) :-
		receive(WebSocket, Message),
		message_text(Message, Text).

	send_binary(WebSocket, Bytes) :-
		send(WebSocket, binary(Bytes)).

	receive_binary(WebSocket, Bytes) :-
		receive(WebSocket, Message),
		message_binary(Message, Bytes).

	send_json(WebSocket, JSON) :-
		send(WebSocket, json(JSON)).

	receive_json(WebSocket, JSON) :-
		receive_text(WebSocket, Text),
		decode_json_text(Text, JSON).

	send_term(WebSocket, Term) :-
		send(WebSocket, term(Term)).

	receive_term(WebSocket, Term) :-
		receive_text(WebSocket, Text),
		decode_term_text(Text, Term).

	open_session(URL, Handler, Response, State) :-
		open_session(URL, Handler, Response, State, []).

	open_session(URL, Handler, Response, State, Options) :-
		open_client_session(URL, Handler, Response, State, Options).

	serve_once(Listener, Handler, Response, State, ClientInfo) :-
		serve_once(Listener, Handler, Response, State, ClientInfo, []).

	serve_once(Listener, Handler, Response, State, ClientInfo, Options) :-
		parse_server_session_options(Options, AcceptOptions, SessionOptions),
		serve_server_session(Listener, http_websocket_accept_handler(AcceptOptions), Handler, Response, State, ClientInfo, SessionOptions).

	allocate_handle_id(HandleId) :-
		(	retract(handle_seed_(CurrentHandleId)) ->
			HandleId is CurrentHandleId + 1
		;	HandleId = 1
		),
		assertz(handle_seed_(HandleId)).

	register_handle(HandleId, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength) :-
		assertz(handle_state_(HandleId, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength)).

	update_handle_state(HandleId, State) :-
		retract(handle_state_(HandleId, Role, Connection, Response, ClientInfo, _OldState, AutoPong, MaxPayloadLength)),
		assertz(handle_state_(HandleId, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength)).

	retract_handle_state(HandleId, Outcome) :-
		(	retract(handle_state_(HandleId, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength)) ->
			Outcome = handle(Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength)
		;	Outcome = missing
		).

	handle_id_outcome(HandleId, Outcome) :-
		(	handle_state_(HandleId, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength) ->
			Outcome = handle(Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength)
		;	Outcome = missing
		).

	register_new_handle(Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength, http_websocket_handle(HandleId)) :-
		allocate_handle_id(HandleId),
		register_handle(HandleId, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength).

	with_handle_state(WebSocket, Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength) :-
		handle_identifier(WebSocket, HandleId),
		handle_id_outcome(HandleId, Outcome),
		(	Outcome = handle(Role, Connection, Response, ClientInfo, State, AutoPong, MaxPayloadLength) ->
			true
		;	existence_error(http_websocket_handle, WebSocket)
		).

	handle_identifier(WebSocket, _HandleId) :-
		var(WebSocket),
		!,
		instantiation_error.
	handle_identifier(http_websocket_handle(HandleId), HandleId) :-
		integer(HandleId),
		!.
	handle_identifier(WebSocket, _HandleId) :-
		domain_error(http_websocket_handle, WebSocket).

	parse_direct_options(Options, AutoPong, MaxPayloadLength, ForwardedOptions) :-
		validate_options(Options),
		(	member(auto_pong(AutoPong0), Options) ->
			validate_auto_pong(AutoPong0, AutoPong)
		;	AutoPong = on
		),
		(	member(max_payload_length(MaxPayloadLength0), Options) ->
			validate_max_payload_length(MaxPayloadLength0, MaxPayloadLength)
		;	MaxPayloadLength = none
		),
		filter_direct_options(Options, ForwardedOptions).

	parse_read_override_options(Options, DefaultAutoPong, DefaultMaxPayloadLength, ReadOptions) :-
		validate_options(Options),
		(	member(auto_pong(AutoPong0), Options) ->
			validate_auto_pong(AutoPong0, AutoPong)
		;	AutoPong = DefaultAutoPong
		),
		(	member(max_payload_length(MaxPayloadLength0), Options) ->
			validate_max_payload_length(MaxPayloadLength0, MaxPayloadLength)
		;	MaxPayloadLength = DefaultMaxPayloadLength
		),
		ReadOptions = [auto_pong(AutoPong), max_payload_length(MaxPayloadLength)].

	parse_server_session_options(Options, AcceptOptions, SessionOptions) :-
		validate_options(Options),
		filter_server_session_options(Options, AcceptOptions, SessionOptions).

	validate_options(Options) :-
		(	var(Options) ->
			instantiation_error
		;	proper_list(Options) ->
			true
		;	type_error(list, Options)
		).

	validate_auto_pong(on, on) :-
		!.
	validate_auto_pong(off, off) :-
		!.
	validate_auto_pong(AutoPong, _ValidatedAutoPong) :-
		domain_error(http_websocket_option, auto_pong(AutoPong)).

	validate_max_payload_length(none, none) :-
		!.
	validate_max_payload_length(MaxPayloadLength, MaxPayloadLength) :-
		integer(MaxPayloadLength),
		MaxPayloadLength >= 0,
		!.
	validate_max_payload_length(MaxPayloadLength, _ValidatedMaxPayloadLength) :-
		domain_error(http_websocket_option, max_payload_length(MaxPayloadLength)).

	filter_direct_options([], []).
	filter_direct_options([auto_pong(_)| Options], FilteredOptions) :-
		!,
		filter_direct_options(Options, FilteredOptions).
	filter_direct_options([max_payload_length(_)| Options], FilteredOptions) :-
		!,
		filter_direct_options(Options, FilteredOptions).
	filter_direct_options([Option| Options], [Option| FilteredOptions]) :-
		filter_direct_options(Options, FilteredOptions).

	filter_server_session_options([], [], []).
	filter_server_session_options([Option| Options], AcceptOptions, [Option| SessionOptions]) :-
		server_session_option(Option),
		!,
		filter_server_session_options(Options, AcceptOptions, SessionOptions).
	filter_server_session_options([Option| Options], [Option| AcceptOptions], SessionOptions) :-
		filter_server_session_options(Options, AcceptOptions, SessionOptions).

	server_session_option(auto_pong(_)).
	server_session_option(keepalive_interval(_)).
	server_session_option(idle_timeout(_)).
	server_session_option(max_payload_length(_)).

	role_read_message(client, Input, Output, State0, State, Message, Options) :-
		client_read_message(Input, Output, State0, State, Message, Options).
	role_read_message(server, Input, Output, State0, State, Message, Options) :-
		server_read_message(Input, Output, State0, State, Message, Options).

	role_write_message(client, Output, State0, State, Message, Options) :-
		client_write_message(Output, State0, State, Message, Options).
	role_write_message(server, Output, State0, State, Message, Options) :-
		server_write_message(Output, State0, State, Message, Options).

	update_or_close_handle(WebSocket, Connection, State) :-
		(	terminal_state(State) ->
			close_connection_and_unregister(WebSocket, Connection)
		;	update_live_handle(WebSocket, State)
		).

	update_or_close_after_receive(WebSocket, Connection, State, Message) :-
		(	Message == end_of_file ->
			close_connection_and_unregister(WebSocket, Connection)
		;	update_or_close_handle(WebSocket, Connection, State)
		).

	update_live_handle(WebSocket, State) :-
		handle_identifier(WebSocket, HandleId),
		update_handle_state(HandleId, State).

	terminal_state(session_state(_Pending, closed(_SentPayload, _ReceivedPayload))).

	close_handle(WebSocket, Message) :-
		with_handle_state(WebSocket, Role, Connection, _Response, _ClientInfo, State0, _AutoPong, _MaxPayloadLength),
		connection_streams(Connection, _Input, Output),
		catch(
			role_write_message(Role, Output, State0, _State, Message, []),
			Error,
			(	close_connection_and_unregister(WebSocket, Connection),
				throw(Error)
			)
		),
		close_connection_and_unregister(WebSocket, Connection).

	close_connection_and_unregister(WebSocket, Connection) :-
		handle_identifier(WebSocket, HandleId),
		retract_handle_state(HandleId, _Outcome),
		catch(close_connection(Connection), _, true).

	best_effort_close(WebSocket) :-
		(	var(WebSocket) ->
			true
		;	catch(with_handle_state(WebSocket, _Role, Connection, _Response, _ClientInfo, _State, _AutoPong, _MaxPayloadLength), _, fail) ->
			close_connection_and_unregister(WebSocket, Connection)
		;	true
		).

	normalize_outbound_message(Message0, Message) :-
		(	is_message(Message0) ->
			Message = Message0
		;	Message0 = text(Text) ->
			websocket_message(text, Text, Message)
		;	Message0 = binary(Bytes) ->
			websocket_message(binary, Bytes, Message)
		;	Message0 = ping(Bytes) ->
			websocket_message(ping, Bytes, Message)
		;	Message0 = pong(Bytes) ->
			websocket_message(pong, Bytes, Message)
		;	Message0 = close(Payload) ->
			websocket_message(close, Payload, Message)
		;	Message0 = json(JSON) ->
			json_text(JSON, Text),
			websocket_message(text, Text, Message)
		;	Message0 = term(Term) ->
			term_text(Term, Text),
			websocket_message(text, Text, Message)
		;	domain_error(http_websocket_message, Message0)
		).

	normalize_close_message(Payload, Message) :-
		websocket_message(close, Payload, Message).

	json_text(JSON, Text) :-
		json_encode(atom(Text), JSON).

	decode_json_text(end_of_file, end_of_file) :-
		!.
	decode_json_text(Text, JSON) :-
		(	json_decode(atom(Text), JSON) ->
			true
		;	domain_error(http_websocket_json_text, Text)
		).

	term_text(Term, Text) :-
		write_term_to_atom(Term, Text, [quoted(true), ignore_ops(true), numbervars(true)]).

	decode_term_text(end_of_file, end_of_file) :-
		!.
	decode_term_text(Text, Term) :-
		read_term_from_atom(Text, Term0, [syntax_errors(error)]),
		(	Term0 == end_of_file ->
			domain_error(http_websocket_term_text, Text)
		;	Term = Term0
		).

	message_text(end_of_file, end_of_file) :-
		!.
	message_text(message(text, Text), Text) :-
		!.
	message_text(Message, _Text) :-
		domain_error(http_websocket_text_message, Message).

	message_binary(end_of_file, end_of_file) :-
		!.
	message_binary(message(binary, Bytes), Bytes) :-
		!.
	message_binary(Message, _Bytes) :-
		domain_error(http_websocket_binary_message, Message).

:- end_object.


:- object(http_websocket_accept_handler(_Options_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-04,
		comment is 'Internal opening-handshake adapter used by the high-level http_websocket server predicates.',
		parnames is ['Options']
	]).

	handle(Request, Response) :-
		http_server::accept_websocket(Request, Response, _Options_).

:- end_object.
