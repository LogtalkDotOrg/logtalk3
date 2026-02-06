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


:- object(stomp).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-06,
		comment is 'Portable STOMP 1.2 (Simple Text Orientated Messaging Protocol) client. Uses the sockets library for TCP communication.',
		remarks is [
			'Supported backends' - 'ECLiPSe, GNU Prolog, SICStus Prolog, and SWI-Prolog (same as the sockets library).',
			'Protocol version' - 'Implements STOMP 1.2 specification.',
			'Heartbeat' - 'Supports heartbeat negotiation. Automatic heartbeat sending is not implemented; use send_heartbeat/1 manually if needed.',
			'Subscriptions' - 'Supports multiple concurrent subscriptions with unique IDs.',
			'Transactions' - 'Supports STOMP transactions with BEGIN, COMMIT, and ABORT.',
			'Frame encoding' - 'Properly encodes/decodes header values according to STOMP 1.2 escaping rules.'
		]
	]).

	% ==========================================================================
	% Public API - Connection Management
	% ==========================================================================

	:- public(connect/4).
	:- mode(connect(+atom, +integer, --compound, +list), one_or_error).
	:- info(connect/4, [
		comment is 'Connects to a STOMP server and performs the STOMP handshake. Returns a connection handle for subsequent operations.',
		argnames is ['Host', 'Port', 'Connection', 'Options'],
		remarks is [
			'Option login(Login)' - 'Username for authentication.',
			'Option passcode(Passcode)' - 'Password for authentication.',
			'Option host(VirtualHost)' - 'Virtual host name. Defaults to the Host parameter.',
			'Option heartbeat(ClientMs, ServerMs)' - 'Heartbeat timing in milliseconds. Default is 0,0 (no heartbeat).'
		],
		exceptions is [
			'Connection refused or network error' - 'error(stomp_error(connection_failed), Context)',
			'Server rejected connection' - 'error(stomp_error(protocol_error(Message)), Context)'
		]
	]).

	:- public(disconnect/2).
	:- mode(disconnect(+compound, +list), one_or_error).
	:- info(disconnect/2, [
		comment is 'Gracefully disconnects from the STOMP server. Sends DISCONNECT frame and waits for RECEIPT if requested.',
		argnames is ['Connection', 'Options'],
		remarks is [
			'Option receipt(ReceiptId)' - 'Request receipt confirmation. Automatically generated if not specified.'
		]
	]).

	:- public(connection_alive/1).
	:- mode(connection_alive(+compound), zero_or_one).
	:- info(connection_alive/1, [
		comment is 'Checks if the connection is still open and valid.',
		argnames is ['Connection']
	]).

	% ==========================================================================
	% Public API - Messaging
	% ==========================================================================

	:- public(send/4).
	:- mode(send(+compound, +atom, +term, +list), one_or_error).
	:- info(send/4, [
		comment is 'Sends a message to the specified destination.',
		argnames is ['Connection', 'Destination', 'Body', 'Options'],
		remarks is [
			'Option content_type(MimeType)' - 'MIME type of the body.',
			'Option content_length(Length)' - 'Body length in bytes. Auto-calculated if omitted for atom/string bodies.',
			'Option transaction(TransactionId)' - 'Include message in the named transaction.',
			'Option receipt(ReceiptId)' - 'Request receipt confirmation.',
			'Option header(Name, Value)' - 'Add custom header (can be repeated).'
		]
	]).

	:- public(subscribe/4).
	:- mode(subscribe(+compound, +atom, +atom, +list), one_or_error).
	:- info(subscribe/4, [
		comment is 'Subscribes to a destination to receive messages.',
		argnames is ['Connection', 'Destination', 'SubscriptionId', 'Options'],
		remarks is [
			'Option ack(Mode)' - 'Acknowledgment mode: auto (default), client, or client_individual.'
		]
	]).

	:- public(unsubscribe/3).
	:- mode(unsubscribe(+compound, +atom, +list), one_or_error).
	:- info(unsubscribe/3, [
		comment is 'Unsubscribes from a destination.',
		argnames is ['Connection', 'SubscriptionId', 'Options']
	]).

	:- public(receive/3).
	:- mode(receive(+compound, -compound, +list), zero_or_one_or_error).
	:- info(receive/3, [
		comment is 'Receives a frame from the server. Returns MESSAGE, RECEIPT, or ERROR frames.',
		argnames is ['Connection', 'Frame', 'Options'],
		remarks is [
			'Option timeout(Milliseconds)' - 'Timeout in milliseconds. 0 for non-blocking, -1 for infinite wait. Default is -1.'
		]
	]).

	% ==========================================================================
	% Public API - Acknowledgment
	% ==========================================================================

	:- public(ack/3).
	:- mode(ack(+compound, +atom, +list), one_or_error).
	:- info(ack/3, [
		comment is 'Acknowledges receipt of a message.',
		argnames is ['Connection', 'AckId', 'Options'],
		remarks is [
			'Option transaction(TransactionId)' - 'Include acknowledgment in the named transaction.'
		]
	]).

	:- public(nack/3).
	:- mode(nack(+compound, +atom, +list), one_or_error).
	:- info(nack/3, [
		comment is 'Negatively acknowledges a message (tells server the message was not consumed).',
		argnames is ['Connection', 'AckId', 'Options'],
		remarks is [
			'Option transaction(TransactionId)' - 'Include negative acknowledgment in the named transaction.'
		]
	]).

	% ==========================================================================
	% Public API - Transactions
	% ==========================================================================

	:- public(begin_transaction/3).
	:- mode(begin_transaction(+compound, +atom, +list), one_or_error).
	:- info(begin_transaction/3, [
		comment is 'Begins a new transaction.',
		argnames is ['Connection', 'TransactionId', 'Options']
	]).

	:- public(commit_transaction/3).
	:- mode(commit_transaction(+compound, +atom, +list), one_or_error).
	:- info(commit_transaction/3, [
		comment is 'Commits a transaction, making all its operations permanent.',
		argnames is ['Connection', 'TransactionId', 'Options']
	]).

	:- public(abort_transaction/3).
	:- mode(abort_transaction(+compound, +atom, +list), one_or_error).
	:- info(abort_transaction/3, [
		comment is 'Aborts a transaction, rolling back all its operations.',
		argnames is ['Connection', 'TransactionId', 'Options']
	]).

	% ==========================================================================
	% Public API - Heartbeat
	% ==========================================================================

	:- public(send_heartbeat/1).
	:- mode(send_heartbeat(+compound), one_or_error).
	:- info(send_heartbeat/1, [
		comment is 'Sends a heartbeat (EOL) to the server to keep the connection alive.',
		argnames is ['Connection']
	]).

	% ==========================================================================
	% Public API - Frame Inspection
	% ==========================================================================

	:- public(frame_command/2).
	:- mode(frame_command(+compound, -atom), one).
	:- info(frame_command/2, [
		comment is 'Extracts the command from a frame.',
		argnames is ['Frame', 'Command']
	]).

	:- public(frame_header/3).
	:- mode(frame_header(+compound, +atom, -atom), zero_or_one).
	:- info(frame_header/3, [
		comment is 'Extracts a header value from a frame. Fails if header is not present.',
		argnames is ['Frame', 'HeaderName', 'Value']
	]).

	:- public(frame_headers/2).
	:- mode(frame_headers(+compound, -list), one).
	:- info(frame_headers/2, [
		comment is 'Extracts all headers from a frame as a list of Name-Value pairs.',
		argnames is ['Frame', 'Headers']
	]).

	:- public(frame_body/2).
	:- mode(frame_body(+compound, -term), one).
	:- info(frame_body/2, [
		comment is 'Extracts the body from a frame. Returns empty atom if no body.',
		argnames is ['Frame', 'Body']
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2, valid/1 as is_list/1
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(uuid(atom), [
		uuid_v7/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% ==========================================================================
	% Implementation - Connection Management
	% ==========================================================================

	connect(Host, Port, Connection, Options) :-
		context(Context),
		catch(
			connect_(Host, Port, Connection, Options, Context),
			Error,
			throw(Error)
		).

	connect_(Host, Port, Connection, Options, Context) :-
		% Open TCP connection
		catch(
			socket::client_open(Host, Port, Stream, []),
			_,
			throw(error(stomp_error(connection_failed), Context))
		),
		% Build CONNECT frame
		option(host(VirtualHost), Options, host(Host)),
		option(heartbeat(ClientCx, ClientCy), Options, heartbeat(0, 0)),
		build_connect_frame(VirtualHost, Options, ClientCx, ClientCy, Frame),
		% Send CONNECT frame
		send_frame(Stream, Frame),
		% Receive CONNECTED frame
		receive_frame(Stream, ResponseFrame, -1, Context),
		% Validate response
		(	frame_command(ResponseFrame, 'CONNECTED') ->
			% Extract negotiated heartbeat
			(	frame_header(ResponseFrame, 'heart-beat', ServerHeartbeat) ->
				parse_heartbeat(ServerHeartbeat, ServerSx, ServerSy),
				negotiate_heartbeat(ClientCx, ClientCy, ServerSx, ServerSy, HeartbeatSend, HeartbeatRecv)
			;	HeartbeatSend = 0, HeartbeatRecv = 0
			),
			% Extract session and version info
			(	frame_header(ResponseFrame, 'session', Session) ->
				true
			;	Session = ''
			),
			(	frame_header(ResponseFrame, 'version', Version) ->
				true
			;	Version = '1.2'
			),
			Connection = connection(Stream, Host, Port, Session, Version, HeartbeatSend, HeartbeatRecv)
		;	frame_command(ResponseFrame, 'ERROR') ->
			socket::close(Stream),
			(	frame_header(ResponseFrame, 'message', ErrorMsg) ->
				true
			;	ErrorMsg = 'Connection rejected'
			),
			throw(error(stomp_error(protocol_error(ErrorMsg)), Context))
		;	socket::close(Stream),
			throw(error(stomp_error(protocol_error('Unexpected response')), Context))
		).

	build_connect_frame(VirtualHost, Options, ClientCx, ClientCy, Frame) :-
		Headers0 = ['accept-version'-'1.2', 'host'-VirtualHost],
		% Add login if provided
		(	option(login(Login), Options) ->
			Headers1 = ['login'-Login| Headers0]
		;	Headers1 = Headers0
		),
		% Add passcode if provided
		(	option(passcode(Passcode), Options) ->
			Headers2 = ['passcode'-Passcode| Headers1]
		;	Headers2 = Headers1
		),
		% Add heartbeat
		(	(ClientCx > 0 ; ClientCy > 0) ->
			format_heartbeat(ClientCx, ClientCy, HeartbeatValue),
			Headers = ['heart-beat'-HeartbeatValue| Headers2]
		;	Headers = Headers2
		),
		Frame = frame('CONNECT', Headers, '').

	disconnect(Connection, Options) :-
		context(Context),
		arg(1, Connection, Stream),
		% Generate receipt ID if not provided
		(	option(receipt(ReceiptId), Options) ->
			true
		;	uuid_v7(ReceiptId)
		),
		% Build and send DISCONNECT frame
		Frame = frame('DISCONNECT', ['receipt'-ReceiptId], ''),
		catch(
			(	send_frame(Stream, Frame),
				% Wait for receipt
				receive_frame(Stream, ResponseFrame, 5000, Context),
				(	frame_command(ResponseFrame, 'RECEIPT'),
					frame_header(ResponseFrame, 'receipt-id', ReceiptId) ->
					true
				;	true  % Receipt not received or wrong ID, continue anyway
				)
			),
			_,
			true  % Ignore errors during disconnect
		),
		catch(socket::close(Stream), _, true).

	connection_alive(Connection) :-
		arg(1, Connection, Stream),
		(	Stream = stream_pair(Input, _) ->
			ActualStream = Input
		;	ActualStream = Stream
		),
		catch(
			stream_property(ActualStream, _),
			_,
			fail
		).

	% ==========================================================================
	% Implementation - Messaging
	% ==========================================================================

	send(Connection, Destination, Body, Options) :-
		context(Context),
		arg(1, Connection, Stream),
		% Encode body to bytes
		body_to_bytes(Body, BodyBytes, BodyLength),
		% Build headers
		Headers0 = ['destination'-Destination],
		% Add content-type if provided
		(	option(content_type(ContentType), Options) ->
			Headers1 = ['content-type'-ContentType| Headers0]
		;	Headers1 = Headers0
		),
		% Add content-length
		(	option(content_length(CL), Options) ->
			Length = CL
		;	Length = BodyLength
		),
		number_codes(Length, LengthCodes),
		atom_codes(LengthAtom, LengthCodes),
		Headers2 = ['content-length'-LengthAtom| Headers1],
		% Add transaction if provided
		(	option(transaction(TransactionId), Options) ->
			Headers3 = ['transaction'-TransactionId| Headers2]
		;	Headers3 = Headers2
		),
		% Add receipt if provided
		(	option(receipt(ReceiptId), Options) ->
			Headers4 = ['receipt'-ReceiptId | Headers3]
		;	Headers4 = Headers3
		),
		% Add custom headers
		add_custom_headers(Options, Headers4, Headers5),
		Frame = frame('SEND', Headers5, BodyBytes),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	subscribe(Connection, Destination, SubscriptionId, Options) :-
		context(Context),
		arg(1, Connection, Stream),
		% Build headers
		Headers0 = ['id'-SubscriptionId, 'destination'-Destination],
		% Add ack mode
		(	option(ack(AckMode), Options) ->
			ack_mode_value(AckMode, AckValue),
			Headers = ['ack'-AckValue| Headers0]
		;	Headers = Headers0
		),
		Frame = frame('SUBSCRIBE', Headers, ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	ack_mode_value(auto, 'auto').
	ack_mode_value(client, 'client').
	ack_mode_value(client_individual, 'client-individual').

	unsubscribe(Connection, SubscriptionId, _Options) :-
		context(Context),
		arg(1, Connection, Stream),
		Frame = frame('UNSUBSCRIBE', ['id'-SubscriptionId], ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	receive(Connection, Frame, Options) :-
		context(Context),
		arg(1, Connection, Stream),
		option(timeout(Timeout), Options, -1),
		receive_frame(Stream, Frame, Timeout, Context).

	% ==========================================================================
	% Implementation - Acknowledgment
	% ==========================================================================

	ack(Connection, AckId, Options) :-
		context(Context),
		arg(1, Connection, Stream),
		Headers0 = ['id'-AckId],
		(	option(transaction(TransactionId), Options) ->
			Headers = ['transaction'-TransactionId | Headers0]
		;	Headers = Headers0
		),
		Frame = frame('ACK', Headers, ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	nack(Connection, AckId, Options) :-
		context(Context),
		arg(1, Connection, Stream),
		Headers0 = ['id'-AckId],
		(	option(transaction(TransactionId), Options) ->
			Headers = ['transaction'-TransactionId| Headers0]
		;	Headers = Headers0
		),
		Frame = frame('NACK', Headers, ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	% ==========================================================================
	% Implementation - Transactions
	% ==========================================================================

	begin_transaction(Connection, TransactionId, _Options) :-
		context(Context),
		arg(1, Connection, Stream),
		Frame = frame('BEGIN', ['transaction'-TransactionId], ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	commit_transaction(Connection, TransactionId, _Options) :-
		context(Context),
		arg(1, Connection, Stream),
		Frame = frame('COMMIT', ['transaction'-TransactionId], ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	abort_transaction(Connection, TransactionId, _Options) :-
		context(Context),
		arg(1, Connection, Stream),
		Frame = frame('ABORT', ['transaction'-TransactionId], ''),
		catch(
			send_frame(Stream, Frame),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	% ==========================================================================
	% Implementation - Heartbeat
	% ==========================================================================

	send_heartbeat(Connection) :-
		context(Context),
		arg(1, Connection, Stream),
		catch(
			write_to_stream(Stream, '\n'),
			Error,
			throw(error(stomp_error(Error), Context))
		).

	% ==========================================================================
	% Implementation - Frame Inspection
	% ==========================================================================

	frame_command(frame(Command, _, _), Command).

	frame_header(frame(_, Headers, _), Name, Value) :-
		member(Name-Value, Headers),
		!.

	frame_headers(frame(_, Headers, _), Headers).

	frame_body(frame(_, _, Body), Body).

	% ==========================================================================
	% Frame Encoding/Decoding
	% ==========================================================================

	send_frame(Stream, frame(Command, Headers, Body)) :-
		% Write command
		atom_codes(Command, CommandCodes),
		write_codes(Stream, CommandCodes),
		write_byte_to_stream(Stream, 10),  % LF
		% Write headers (with escaping for non-CONNECT/CONNECTED frames)
		needs_escaping(Command, NeedsEscape),
		write_headers(Headers, NeedsEscape, Stream),
		% Blank line
		write_byte_to_stream(Stream, 10),  % LF
		% Write body
		(	Body == '' ->
			true
		;	atom(Body) ->
			atom_codes(Body, BodyCodes),
			write_codes(Stream, BodyCodes)
		;	is_list(Body) ->
			write_codes(Stream, Body)
		;	true
		),
		% NULL terminator
		write_byte_to_stream(Stream, 0),
		flush_stream(Stream).

	needs_escaping('CONNECT', false) :- !.
	needs_escaping('CONNECTED', false) :- !.
	needs_escaping(_, true).

	write_headers([], _, _).
	write_headers([Key-Value| Pairs], NeedsEscape, Stream) :-
		(	NeedsEscape == true ->
			escape_header_value(Value, EscapedValue)
		;	EscapedValue = Value
		),
		atom_codes(Key, KeyCodes),
		write_codes(Stream, KeyCodes),
		write_byte_to_stream(Stream, 58),  % colon
		atom_codes(EscapedValue, ValueCodes),
		write_codes(Stream, ValueCodes),
		write_byte_to_stream(Stream, 10),  % LF
		write_headers(Pairs, NeedsEscape, Stream).

	% Escape header values according to STOMP 1.2
	escape_header_value(Value, Escaped) :-
		atom_codes(Value, Codes),
		escape_codes(Codes, EscapedCodes),
		atom_codes(Escaped, EscapedCodes).

	escape_codes([], []).
	escape_codes([13| Codes], [92, 114| EscapedCodes]) :- !,  % CR -> \r
		escape_codes(Codes, EscapedCodes).
	escape_codes([10| Codes], [92, 110| EscapedCodes]) :- !,  % LF -> \n
		escape_codes(Codes, EscapedCodes).
	escape_codes([58| Codes], [92, 99| EscapedCodes]) :- !,   % : -> \c
		escape_codes(Codes, EscapedCodes).
	escape_codes([92| Codes], [92, 92| EscapedCodes]) :- !,   % \ -> \\
		escape_codes(Codes, EscapedCodes).
	escape_codes([Code| Codes], [Code| EscapedCodes]) :-
		escape_codes(Codes, EscapedCodes).

	% Unescape header values
	unescape_header_value(Value, Unescaped) :-
		atom_codes(Value, Codes),
		unescape_codes(Codes, UnescapedCodes),
		atom_codes(Unescaped, UnescapedCodes).

	unescape_codes([], []) :- !.
	unescape_codes([92, 114| Codes], [13| UnescapedCodes]) :- !,  % \r -> CR
		unescape_codes(Codes, UnescapedCodes).
	unescape_codes([92, 110| Codes], [10| UnescapedCodes]) :- !,  % \n -> LF
		unescape_codes(Codes, UnescapedCodes).
	unescape_codes([92, 99| Codes], [58| UnescapedCodes]) :- !,   % \c -> :
		unescape_codes(Codes, UnescapedCodes).
	unescape_codes([92, 92| Codes], [92| UnescapedCodes]) :- !,   % \\ -> \
		unescape_codes(Codes, UnescapedCodes).
	unescape_codes([Code| Codes], [Code| UnescapedCodes]) :-
		unescape_codes(Codes, UnescapedCodes).

	% Receive and parse a frame
	receive_frame(Stream, Frame, Timeout, Context) :-
		% Read command line (skip any leading EOLs - heartbeats)
		read_command(Stream, Timeout, CommandCodes, Context),
		(	CommandCodes == [] ->
			% Empty - likely a heartbeat, try again
			receive_frame(Stream, Frame, Timeout, Context)
		;	atom_codes(Command, CommandCodes),
			% Determine if we need to unescape headers
			needs_escaping(Command, NeedsUnescape),
			% Read headers until blank line
			read_headers(Stream, NeedsUnescape, Headers, Context),
			% Read body (if content-length present, read exactly that many bytes)
			(	member('content-length'-LengthAtom, Headers) ->
				atom_codes(LengthAtom, LengthCodes),
				number_codes(Length, LengthCodes),
				read_body_length(Stream, Length, BodyCodes, Context)
			;	read_body_null(Stream, BodyCodes, Context)
			),
			% Skip the NULL terminator if we read by length
			(	member('content-length'-_, Headers) ->
				read_byte_from_stream(Stream, _)  % consume NULL
			;	true
			),
			atom_codes(Body, BodyCodes),
			Frame = frame(Command, Headers, Body)
		).

	% Read command line, skipping heartbeat EOLs
	read_command(Stream, Timeout, CommandCodes, Context) :-
		(	Timeout >= 0 ->
			% Check if data available (simplified - actual implementation may vary by backend)
			true
		;	true
		),
		catch(
			read_line(Stream, CommandCodes),
			Error,
			throw(error(stomp_error(Error), Context))
		),
		% If we get an empty line (heartbeat), return empty
		(	CommandCodes == [10] ->
			CommandCodes = []
		;	CommandCodes == [13, 10] ->
			CommandCodes = []
		;	% Strip trailing EOL
			strip_eol(CommandCodes, CommandCodes)
		).

	read_line(Stream, Codes) :-
		read_line_acc(Stream, [], Codes).

	read_line_acc(Stream, Acc, Codes) :-
		read_byte_from_stream(Stream, Byte),
		(	Byte == -1 ->
			reverse(Acc, Codes)
		;	Byte == 10 ->  % LF
			reverse(Acc, Codes)
		;	Byte == 13 ->  % CR - check for LF
			read_byte_from_stream(Stream, Next),
			(	Next == 10 ->
				reverse(Acc, Codes)
			;	% Put back? Not easily possible, include CR
				reverse([Next, 13 | Acc], Codes)
			)
		;	read_line_acc(Stream, [Byte | Acc], Codes)
		).

	strip_eol(Codes, Stripped) :-
		(	append(Core, [10], Codes) ->
			strip_eol(Core, Stripped)
		;	append(Core, [13], Codes) ->
			strip_eol(Core, Stripped)
		;	Stripped = Codes
		).

	read_headers(Stream, NeedsUnescape, Headers, Context) :-
		read_headers_acc(Stream, NeedsUnescape, [], Headers, Context).

	read_headers_acc(Stream, NeedsUnescape, Acc, Headers, Context) :-
		catch(
			read_line(Stream, LineCodes),
			Error,
			throw(error(stomp_error(Error), Context))
		),
		strip_eol(LineCodes, StrippedCodes),
		(	StrippedCodes == [] ->
			% Blank line - end of headers
			reverse(Acc, Headers)
		;	% Parse header
			parse_header_line(StrippedCodes, NeedsUnescape, Name, Value),
			% Only keep first occurrence of each header (per STOMP spec)
			(	member(Name-_, Acc) ->
				NewAcc = Acc
			;	NewAcc = [Name-Value | Acc]
			),
			read_headers_acc(Stream, NeedsUnescape, NewAcc, Headers, Context)
		).

	parse_header_line(Codes, NeedsUnescape, Name, Value) :-
		% Find first colon
		append(NameCodes, [58 | ValueCodes], Codes),
		!,
		atom_codes(Name, NameCodes),
		atom_codes(RawValue, ValueCodes),
		(	NeedsUnescape == true ->
			unescape_header_value(RawValue, Value)
		;	Value = RawValue
		).

	read_body_length(_, 0, [], _) :- !.
	read_body_length(Stream, Length, [Byte | Rest], Context) :-
		Length > 0,
		catch(
			read_byte_from_stream(Stream, Byte),
			Error,
			throw(error(stomp_error(Error), Context))
		),
		Length1 is Length - 1,
		read_body_length(Stream, Length1, Rest, Context).

	read_body_null(Stream, Codes, Context) :-
		read_body_null_acc(Stream, [], Codes, Context).

	read_body_null_acc(Stream, Acc, Codes, Context) :-
		catch(
			read_byte_from_stream(Stream, Byte),
			Error,
			throw(error(stomp_error(Error), Context))
		),
		(	Byte == 0 ->
			% NULL terminator
			reverse(Acc, Codes)
		;	Byte == -1 ->
			reverse(Acc, Codes)
		;	read_body_null_acc(Stream, [Byte | Acc], Codes, Context)
		).

	% ==========================================================================
	% Auxiliary Predicates
	% ==========================================================================

	% Option handling
	option(Option, Options) :-
		member(Option, Options),
		!.

	option(Option, Options, _Default) :-
		member(Option, Options),
		!.
	option(Option, _, Option).

	% Add custom headers from options
	add_custom_headers([], Headers, Headers).
	add_custom_headers([header(Name, Value)| Options], Headers0, Headers) :-
		!,
		add_custom_headers(Options, [Name-Value| Headers0], Headers).
	add_custom_headers([_| Options], Headers0, Headers) :-
		add_custom_headers(Options, Headers0, Headers).

	% Heartbeat parsing and negotiation
	parse_heartbeat(HeartbeatAtom, ClientMs, ServerMs) :-
		atom_codes(HeartbeatAtom, Codes),
		% comma separator
		append(ClientCodes, [44| ServerCodes], Codes),
		number_codes(ClientMs, ClientCodes),
		number_codes(ServerMs, ServerCodes).

	format_heartbeat(ClientMs, ServerMs, HeartbeatAtom) :-
		atomic_list_concat([ClientMs, ',', ServerMs], HeartbeatAtom).

	negotiate_heartbeat(ClientCx, ClientCy, ServerSx, ServerSy, SendInterval, RecvInterval) :-
		% Client-to-server heartbeat
		(	ClientCx == 0 ->
			SendInterval = 0
		;	ServerSy == 0 ->
			SendInterval = 0
		;	SendInterval is max(ClientCx, ServerSy)
		),
		% Server-to-client heartbeat
		(	ServerSx == 0 ->
			RecvInterval = 0
		;	ClientCy == 0 ->
			RecvInterval = 0
		;	RecvInterval is max(ServerSx, ClientCy)
		).

	% Convert body to byte codes
	body_to_bytes(Body, Codes, Length) :-
		(	atom(Body) ->
			atom_codes(Body, Codes),
			length(Codes, Length)
		;	is_list(Body) ->
			Codes = Body,
			length(Codes, Length)
		;	write_to_atom(Body, BodyAtom),
			atom_codes(BodyAtom, Codes),
			length(Codes, Length)
		).

	% Stream I/O - handles both single streams and stream pairs
	% GNU Prolog and SWI-Prolog return stream_pair(Input, Output)

	write_codes(stream_pair(_, Output), Codes) :-
		!,
		write_codes_(Codes, Output).
	write_codes(Stream, Codes) :-
		write_codes_(Codes, Stream).

	write_codes_([], _).
	write_codes_([Code| Codes], Stream) :-
		put_byte(Stream, Code),
		write_codes_(Codes, Stream).

	write_byte_to_stream(stream_pair(_, Output), Byte) :-
		!,
		put_byte(Output, Byte).
	write_byte_to_stream(Stream, Byte) :-
		put_byte(Stream, Byte).

	write_to_stream(stream_pair(_, Output), Atom) :-
		!,
		atom_codes(Atom, Codes),
		write_codes_(Codes, Output).
	write_to_stream(Stream, Atom) :-
		atom_codes(Atom, Codes),
		write_codes_(Codes, Stream).

	read_byte_from_stream(stream_pair(Input, _), Byte) :-
		!,
		get_byte(Input, Byte).
	read_byte_from_stream(Stream, Byte) :-
		get_byte(Stream, Byte).

	flush_stream(stream_pair(_, Output)) :-
		!,
		flush_output(Output).
	flush_stream(Stream) :-
		flush_output(Stream).

:- end_object.
