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


:- object(http_server,
	imports(http_message_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Portable stream-based HTTP server orchestration predicates.',
		remarks is [
			'Binary streams' - 'All stream predicates expect binary input and output streams.',
			'Per-message primitives' - 'The read_request/2, write_response/2, dispatch/3, and serve/3 predicates process a single HTTP message.',
			'Connection loop' - 'The serve_connection/3 predicate repeatedly serves requests on the same stream pair according to HTTP persistence rules.',
			'HEAD requests' - 'The serve/3 and serve_connection/3 predicates suppress response body bytes on the wire for HEAD requests while preserving the generated response headers.',
			'WebSocket handshake responses' - 'The accept_websocket/3 predicate validates a normalized opening-handshake request and builds the corresponding ``101 Switching Protocols`` response, but this stream layer does not yet hand upgraded connections off to a frame-processing layer.'
		]
	]).

	:- public(read_request/2).
	:- mode(read_request(+stream, --compound), zero_or_one).
	:- info(read_request/2, [
		comment is 'Reads exactly one HTTP request from a binary stream and returns it as a normalized request term. Fails on clean end-of-file before reading any bytes.',
		argnames is ['Input', 'Request']
	]).

	:- public(write_response/2).
	:- mode(write_response(+stream, +compound), one_or_error).
	:- info(write_response/2, [
		comment is 'Writes exactly one normalized HTTP response term to a binary stream.',
		argnames is ['Output', 'Response']
	]).

	:- public(dispatch/3).
	:- mode(dispatch(+object_identifier, +compound, -compound), one_or_error).
	:- info(dispatch/3, [
		comment is 'Dispatches a normalized HTTP request to a handler object implementing the http_handler_protocol protocol. Handler failures, exceptions, or invalid responses are converted to an internal server error response.',
		argnames is ['Handler', 'Request', 'Response']
	]).

	:- public(serve/3).
	:- mode(serve(+stream, +stream, +object_identifier), zero_or_one).
	:- info(serve/3, [
		comment is 'Reads one request from a binary input stream, dispatches it to a handler object, and writes one response to a binary output stream. HEAD requests suppress response body bytes on the wire while preserving the generated response headers. Malformed requests are converted to bad request responses.',
		argnames is ['Input', 'Output', 'Handler']
	]).

	:- public(serve_connection/3).
	:- mode(serve_connection(+stream, +stream, +object_identifier), one_or_error).
	:- info(serve_connection/3, [
		comment is 'Repeatedly reads requests from a binary input stream, dispatches them to a handler object, and writes responses to a binary output stream until end-of-file, a ``101 Switching Protocols`` response, or connection-close semantics terminate the connection. HEAD requests suppress response body bytes on the wire while preserving the generated response headers.',
		argnames is ['Input', 'Output', 'Handler']
	]).

	:- public(accept_websocket/3).
	:- mode(accept_websocket(+compound, --compound, +list), one_or_error).
	:- info(accept_websocket/3, [
		comment is 'Validates a normalized WebSocket opening-handshake request and builds a matching ``101 Switching Protocols`` response. The ``protocol(Protocol)`` option can be used to select a single offered subprotocol.',
		argnames is ['Request', 'Response', 'Options']
	]).

	:- public(serve_websocket/4).
	:- mode(serve_websocket(+stream, +stream, +object_identifier, --compound), one_or_error).
	:- info(serve_websocket/4, [
		comment is 'Reads one request from a binary input stream, dispatches it to a handler object, writes one response to a binary output stream, and returns ``accepted(Request, Response)`` when the exchange completed with a valid WebSocket opening-handshake response. Malformed requests and non-upgrade responses are written to the stream and reported as ``rejected(Response)``. Clean end-of-file before any bytes are read is reported as ``end_of_file``.',
		argnames is ['Input', 'Output', 'Handler', 'Outcome']
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2, valid/1 as proper_list/1
	]).

	accept_websocket(Request, Response, Options) :-
		parse_accept_websocket_options(Options, Headers, Properties0, ProtocolOption),
		validate_accept_websocket_headers(Headers),
		validate_accept_websocket_properties(Properties0),
		validate_websocket_request(Request, Version, Key, OfferedProtocols),
		select_websocket_protocol(ProtocolOption, OfferedProtocols, SelectedProtocol),
		http::websocket_accept(Key, Accept),
		websocket_response_properties(Properties0, Accept, SelectedProtocol, Properties),
		http::response(Version, status(101, 'Switching Protocols'), Headers, empty, Properties, Response).

	read_request(Input, Request) :-
		read_request_bytes(Input, Bytes),
		(	http::parse_request(bytes(Bytes), Request) ->
			true
		;	domain_error(http_request_stream, malformed_request(Bytes))
		).

	write_response(Output, Response) :-
		http::generate_response(bytes(Bytes), Response),
		write_bytes(Bytes, Output),
		flush_output(Output).

	dispatch(Handler, Request, Response) :-
		validate_handler(Handler),
		http::version(Request, Version),
		(	catch(Handler::handle(Request, Candidate), _, fail),
			http::is_response(Candidate) ->
			Response = Candidate
		;	internal_server_error_response(Version, Response)
		).

	serve(Input, Output, Handler) :-
		validate_handler(Handler),
		try_read_request(Input, Result),
		(	Result = request(Request) ->
			dispatch(Handler, Request, Response),
			write_response_for_request(Output, Request, Response)
		;	Result = error(Response),
			write_response(Output, Response)
		).

	serve_connection(Input, Output, Handler) :-
		validate_handler(Handler),
		serve_connection_loop(Input, Output, Handler).

	serve_websocket(Input, Output, Handler, Outcome) :-
		validate_handler(Handler),
		try_read_request(Input, Result),
		serve_websocket_result(Result, Output, Handler, Outcome).

	parse_accept_websocket_options(Options, Headers, Properties, ProtocolOption) :-
		check_accept_websocket_options(Options),
		reverse(Options, ReversedOptions),
		accept_websocket_option(ReversedOptions, headers, [], Headers),
		accept_websocket_option(ReversedOptions, properties, [], Properties),
		accept_websocket_option(ReversedOptions, protocol, none, ProtocolOption).

	accept_websocket_option([], _Name, Default, Default).
	accept_websocket_option([Option| _Options], Name, _Default, Value) :-
		Option =.. [Name, Value],
		!.
	accept_websocket_option([_Option| Options], Name, Default, Value) :-
		accept_websocket_option(Options, Name, Default, Value).

	check_accept_websocket_options([]).
	check_accept_websocket_options([Option| Options]) :-
		check_accept_websocket_option(Option),
		check_accept_websocket_options(Options).

	check_accept_websocket_option(headers(Headers)) :-
		proper_list(Headers),
		!.
	check_accept_websocket_option(properties(Properties)) :-
		proper_list(Properties),
		!.
	check_accept_websocket_option(protocol(Protocol)) :-
		atom(Protocol),
		!.
	check_accept_websocket_option(Option) :-
		domain_error(http_server_websocket_option, Option).

	validate_accept_websocket_headers(Headers) :-
		( 	member(Name-_, Headers),
			memberchk(Name, [connection, upgrade, sec_websocket_key, sec_websocket_version, sec_websocket_accept, sec_websocket_protocol]) ->
			domain_error(http_server_websocket_headers, Headers)
		; 	true
		).

	validate_accept_websocket_properties(Properties) :-
		( 	member(Property, Properties),
			functor(Property, Functor, _Arity),
			memberchk(Functor, [connection, upgrade, websocket_key, websocket_version, websocket_accept, websocket_protocol]) ->
			domain_error(http_server_websocket_properties, Properties)
		; 	true
		).

	validate_websocket_request(Request, Version, Key, OfferedProtocols) :-
		( 	valid_websocket_request(Request, Version, Key, OfferedProtocols) ->
			true
		; 	domain_error(http_server_websocket_request, Request)
		).

	valid_websocket_request(Request, Version, Key, OfferedProtocols) :-
		http::method(Request, get),
		http::version(Request, Version),
		websocket_http_version(Version),
		http::body(Request, empty),
		^^message_connection_tokens(Request, ConnectionTokens),
		memberchk(upgrade, ConnectionTokens),
		message_upgrade_tokens(Request, UpgradeTokens),
		memberchk(websocket, UpgradeTokens),
		message_websocket_key(Request, Key),
		message_websocket_version(Request, 13),
		( 	message_websocket_protocols(Request, OfferedProtocols) ->
			true
		; 	OfferedProtocols = []
		).

	websocket_http_version(http(Major, Minor)) :-
		Major >= 1,
		( 	Major > 1 ->
			true
		; 	Minor >= 1
		),
		!.

	select_websocket_protocol(none, _OfferedProtocols, none) :-
		!.
	select_websocket_protocol(Protocol, OfferedProtocols, Protocol) :-
		OfferedProtocols \== [],
		member(Protocol, OfferedProtocols),
		!.
	select_websocket_protocol(Protocol, _OfferedProtocols, _SelectedProtocol) :-
		domain_error(http_server_websocket_protocol, Protocol).

	websocket_response_properties(Properties0, Accept, none, [connection([upgrade]), upgrade([websocket]), websocket_accept(Accept)| Properties0]) :-
		!.
	websocket_response_properties(Properties0, Accept, Protocol, [connection([upgrade]), upgrade([websocket]), websocket_accept(Accept), websocket_protocol([Protocol])| Properties0]) :-
		Protocol \== none.

	message_upgrade_tokens(Message, Tokens) :-
		( 	http::property(Message, upgrade(Tokens)) ->
			true
		; 	http::header(Message, upgrade, Tokens)
		),
		!.

	message_websocket_key(Message, Key) :-
		( 	http::property(Message, websocket_key(Key)) ->
			true
		; 	http::header(Message, sec_websocket_key, Key)
		),
		!.

	message_websocket_version(Message, Version) :-
		( 	http::property(Message, websocket_version(Version)) ->
			true
		; 	http::header(Message, sec_websocket_version, Version)
		),
		!.

	message_websocket_protocols(Message, Protocols) :-
		( 	http::property(Message, websocket_protocol(Protocols)) ->
			true
		; 	http::header(Message, sec_websocket_protocol, Protocols)
		),
		!.

	serve_websocket_result(end_of_file, _Output, _Handler, end_of_file).
	serve_websocket_result(error(Response), Output, _Handler, rejected(Response)) :-
		write_response(Output, Response).
	serve_websocket_result(request(Request), Output, Handler, Outcome) :-
		dispatch(Handler, Request, Response),
		write_response_for_request(Output, Request, Response),
		( 	valid_websocket_upgrade_response(Request, Response) ->
			Outcome = accepted(Request, Response)
		; 	Outcome = rejected(Response)
		).

	valid_websocket_upgrade_response(Request, Response) :-
		http::status(Response, status(101, _ReasonPhrase)),
		http::body(Response, empty),
		^^message_connection_tokens(Response, ConnectionTokens),
		memberchk(upgrade, ConnectionTokens),
		message_upgrade_tokens(Response, UpgradeTokens),
		memberchk(websocket, UpgradeTokens),
		message_websocket_key(Request, Key),
		http::websocket_accept(Key, Accept),
		message_websocket_accept(Response, Accept),
		valid_websocket_protocol_response(Request, Response).

	valid_websocket_protocol_response(Request, Response) :-
		message_websocket_protocols(Request, OfferedProtocols),
		!,
		message_websocket_protocols(Response, [SelectedProtocol]),
		memberchk(SelectedProtocol, OfferedProtocols).
	valid_websocket_protocol_response(_Request, Response) :-
		\+ message_websocket_protocols(Response, _Protocols).

	message_websocket_accept(Message, Accept) :-
		( 	http::property(Message, websocket_accept(Accept)) ->
			true
		; 	http::header(Message, sec_websocket_accept, Accept)
		),
		!.

	serve_connection_loop(Input, Output, Handler) :-
		try_read_request(Input, Result),
		(	Result == end_of_file ->
			true
		;	Result = error(Response0) ->
			apply_connection_action(close, Response0, Response),
			write_response(Output, Response)
		;	Result = request(Request),
			dispatch(Handler, Request, Response0),
			connection_action(Request, Response0, Action),
			apply_connection_action(Action, Response0, Response),
			write_response_for_request(Output, Request, Response),
			(	Action == keep_alive ->
				serve_connection_loop(Input, Output, Handler)
			;	true
			)
		).

	write_response_for_request(Output, Request, Response) :-
		head_request(Request),
		!,
		head_response_bytes(Response, Bytes),
		write_bytes(Bytes, Output),
		flush_output(Output).
	write_response_for_request(Output, _Request, Response) :-
		write_response(Output, Response).

	head_request(Request) :-
		http::method(Request, head).

	head_response_bytes(Response, HeaderBytes) :-
		http::generate_response(bytes(Bytes), Response),
		strip_response_body_bytes(Bytes, HeaderBytes).

	strip_response_body_bytes(Bytes, HeaderBytes) :-
		(	split_response_header_bytes(Bytes, [], HeaderBytes) ->
			true
		;	domain_error(http_response_stream, malformed_response(Bytes))
		).

	split_response_header_bytes([0'\r,0'\n,0'\r,0'\n| _], Acc, HeaderBytes) :-
		!,
		reverse(Acc, PrefixBytes),
		append(PrefixBytes, [0'\r,0'\n,0'\r,0'\n], HeaderBytes).
	split_response_header_bytes([Byte| Bytes], Acc, HeaderBytes) :-
		split_response_header_bytes(Bytes, [Byte| Acc], HeaderBytes).

	try_read_request(Input, Result) :-
		catch(
			(	read_request(Input, Request) ->
				Result = request(Request)
			;	Result = end_of_file
			),
			Error,
			(	bad_request_response(Error, Response),
				Result = error(Response)
			)
		).

	connection_action(_Request, Response, upgrade) :-
		switching_protocols_response(Response),
		!.

	connection_action(_Request, Response, close) :-
		^^message_has_connection_token(Response, close),
		!.
	connection_action(Request, _Response, keep_alive) :-
		request_keep_alive(Request),
		!.
	connection_action(_Request, _Response, close).

	request_keep_alive(Request) :-
		^^request_persistent(Request).

	switching_protocols_response(Response) :-
		http::status(Response, status(101, _ReasonPhrase)).

	apply_connection_action(upgrade, Response, Response) :-
		!.

	apply_connection_action(Action, Response0, Response) :-
		Response0 = response(Version, _Status, _Headers, _Body, _Properties),
		^^message_connection_tokens(Response0, Tokens0),
		remove_persistence_tokens(Tokens0, Tokens1),
		connection_action_tokens(Action, Version, Tokens1, Tokens),
		rewrite_response_connection_tokens(Response0, Tokens, Response).

	connection_action_tokens(keep_alive, Version, Tokens0, Tokens) :-
		(	^^version_persistent_by_default(Version) ->
			Tokens = Tokens0
		;	ensure_token('keep-alive', Tokens0, Tokens)
		).
	connection_action_tokens(close, Version, Tokens0, Tokens) :-
		(	^^version_persistent_by_default(Version) ->
			ensure_token(close, Tokens0, Tokens)
		;	Tokens = Tokens0
		).

	remove_persistence_tokens([], []).
	remove_persistence_tokens([close| Tokens], FilteredTokens) :-
		!,
		remove_persistence_tokens(Tokens, FilteredTokens).
	remove_persistence_tokens(['keep-alive'| Tokens], FilteredTokens) :-
		!,
		remove_persistence_tokens(Tokens, FilteredTokens).
	remove_persistence_tokens([Token| Tokens], [Token| FilteredTokens]) :-
		remove_persistence_tokens(Tokens, FilteredTokens).

	ensure_token(Token, Tokens, Tokens) :-
		member(Token, Tokens),
		!.
	ensure_token(Token, Tokens0, Tokens) :-
		append(Tokens0, [Token], Tokens).

	rewrite_response_connection_tokens(response(Version, Status, Headers0, Body, Properties0), Tokens, Response) :-
		remove_connection_headers(Headers0, Headers),
		remove_connection_properties(Properties0, Properties1),
		(	Tokens == [] ->
			Properties = Properties1
		;	Properties = [connection(Tokens)| Properties1]
		),
		http::response(Version, Status, Headers, Body, Properties, Response).

	remove_connection_headers(Headers, FilteredHeaders) :-
		remove_connection_headers(Headers, [], ReversedHeaders),
		reverse(ReversedHeaders, FilteredHeaders).

	remove_connection_headers([], Headers, Headers).
	remove_connection_headers([connection-_Tokens| Headers], Acc, FilteredHeaders) :-
		!,
		remove_connection_headers(Headers, Acc, FilteredHeaders).
	remove_connection_headers([Header| Headers], Acc, FilteredHeaders) :-
		remove_connection_headers(Headers, [Header| Acc], FilteredHeaders).

	remove_connection_properties(Properties, FilteredProperties) :-
		remove_connection_properties(Properties, [], ReversedProperties),
		reverse(ReversedProperties, FilteredProperties).

	remove_connection_properties([], Properties, Properties).
	remove_connection_properties([connection(_Tokens)| Properties], Acc, FilteredProperties) :-
		!,
		remove_connection_properties(Properties, Acc, FilteredProperties).
	remove_connection_properties([Property| Properties], Acc, FilteredProperties) :-
		remove_connection_properties(Properties, [Property| Acc], FilteredProperties).

	read_request_bytes(Input, Bytes) :-
		read_line_bytes(Input, LineResult),
		LineResult = line(RequestLineBytes),
		append(RequestLineBytes, [0'\r,0'\n], RequestLineBytesCRLF),
		read_header_block(Input, HeaderBytes, Headers),
		read_request_body_bytes(Input, Headers, BodyBytes),
		append(RequestLineBytesCRLF, HeaderBytes, HeadBytes),
		append(HeadBytes, [0'\r,0'\n| BodyBytes], Bytes).

	read_header_block(Input, HeaderBytes, Headers) :-
		read_header_block_bytes(Input, HeaderBytes, []),
		http::parse_headers(codes(HeaderBytes), Headers).

	read_header_block_bytes(Input, Bytes0, Bytes) :-
		read_line_bytes(Input, LineResult),
		(	LineResult == end_of_file ->
			domain_error(http_request_stream, unexpected_end_of_file)
		;	LineResult = line(LineBytes),
			read_header_block_line(Input, LineBytes, Bytes0, Bytes)
		).

	read_header_block_line(_Input, [], Bytes, Bytes) :-
		!.
	read_header_block_line(Input, LineBytes, Bytes0, Bytes) :-
		append(LineBytes, [0'\r,0'\n| Bytes1], Bytes0),
		read_header_block_bytes(Input, Bytes1, Bytes).

	read_request_body_bytes(Input, Headers, BodyBytes) :-
		(	member(transfer_encoding-Encodings, Headers) ->
			read_transfer_body_bytes(Input, Encodings, BodyBytes)
		;	member(content_length-Length, Headers) ->
			read_exact_bytes(Input, Length, BodyBytes)
		;	BodyBytes = []
		).

	read_transfer_body_bytes(Input, [chunked], BodyBytes) :-
		!,
		read_chunked_body_bytes(Input, BodyBytes, []).
	read_transfer_body_bytes(_Input, Encodings, _BodyBytes) :-
		domain_error(http_request_stream, unsupported_transfer_encoding(Encodings)).

	read_chunked_body_bytes(Input, Bytes0, Bytes) :-
		read_line_bytes(Input, LineResult),
		(	LineResult == end_of_file ->
			domain_error(http_request_stream, unexpected_end_of_file)
		;	LineResult = line(SizeLineBytes),
			^^chunk_size_line_size(SizeLineBytes, Size),
			append(SizeLineBytes, [0'\r,0'\n| Bytes1], Bytes0),
			(	Size =:= 0 ->
				read_header_block(Input, TrailerBytes, _),
				append(TrailerBytes, [0'\r,0'\n| Bytes], Bytes1)
			;	read_exact_bytes(Input, Size, DataBytes),
				append(DataBytes, [0'\r,0'\n| Bytes2], Bytes1),
				read_crlf(Input),
				read_chunked_body_bytes(Input, Bytes2, Bytes)
			)
		).

	read_exact_bytes(Input, Length, Bytes) :-
		integer(Length),
		Length >= 0,
		!,
		read_exact_bytes(Input, Length, Bytes, []).
	read_exact_bytes(_Input, Length, _Bytes) :-
		domain_error(non_negative_integer, Length).

	read_exact_bytes(_Input, 0, Bytes, Bytes) :-
		!.
	read_exact_bytes(Input, Length, [Byte| Bytes0], Bytes) :-
		get_byte(Input, Byte),
		(	Byte =:= -1 ->
			domain_error(http_request_stream, unexpected_end_of_file)
		;	NextLength is Length - 1,
			read_exact_bytes(Input, NextLength, Bytes0, Bytes)
		).

	read_crlf(Input) :-
		get_byte(Input, CarriageReturn),
		get_byte(Input, LineFeed),
		(	CarriageReturn =:= 0'\r,
			LineFeed =:= 0'\n ->
			true
		;	domain_error(http_request_stream, invalid_line_ending(CarriageReturn, LineFeed))
		).

	read_line_bytes(Input, Result) :-
		get_byte(Input, Byte),
		(	Byte =:= -1 ->
			Result = end_of_file
		;	read_line_bytes(Input, Byte, [], Result)
		).

	read_line_bytes(Input, Byte, Acc, Result) :-
		(	Byte =:= 0'\r ->
			get_byte(Input, NextByte),
			(	NextByte =:= 0'\n ->
				reverse(Acc, Bytes),
				Result = line(Bytes)
			;	domain_error(http_request_stream, invalid_line_ending(Byte, NextByte))
			)
		;	Byte =:= 0'\n ->
			domain_error(http_request_stream, bare_line_feed)
		;	get_byte(Input, NextByte),
			read_line_bytes(Input, NextByte, [Byte| Acc], Result)
		).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	validate_handler(Handler) :-
		(	conforms_to_protocol(Handler, http_handler_protocol) ->
			true
		;	domain_error(http_handler_protocol, Handler)
		).

	bad_request_response(_Error, Response) :-
		error_response(http(1, 1), 400, 'Bad Request', 'Bad Request', Response).

	internal_server_error_response(Version, Response) :-
		error_response(Version, 500, 'Internal Server Error', 'Internal Server Error', Response).

	error_response(Version, Code, Reason, Message, Response) :-
		http::response(Version, status(Code, Reason), [], content('text/plain', text(Message)), [], Response).

:- end_object.
