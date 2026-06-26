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


:- object(http_websocket_session_read_options,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Auxiliary object defining the supported read options and default values for HTTP WebSocket sessions.'
	]).

	:- public(merged_options/2).
	:- mode(merged_options(+list(compound), -list(compound)), one).
	:- info(merged_options/2, [
		comment is 'Merges validated read options with the declared default options.',
		argnames is ['UserOptions', 'MergedOptions']
	]).

	valid_option(auto_pong(on)).
	valid_option(auto_pong(off)).
	valid_option(max_payload_length(none)).
	valid_option(max_payload_length(MaxPayloadLength)) :-
		integer(MaxPayloadLength),
		MaxPayloadLength >= 0.

	default_option(auto_pong(off)).
	default_option(max_payload_length(none)).

	merged_options(UserOptions, MergedOptions) :-
		^^merge_options(UserOptions, MergedOptions).

:- end_object.


:- object(http_websocket_session_write_options,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Auxiliary object defining the supported write options and default values for HTTP WebSocket sessions.'
	]).

	:- public(merged_options/2).
	:- mode(merged_options(+list(compound), -list(compound)), one).
	:- info(merged_options/2, [
		comment is 'Merges validated write options with the declared default options.',
		argnames is ['UserOptions', 'MergedOptions']
	]).

	valid_option(fragment_size(none)).
	valid_option(fragment_size(Size)) :-
		integer(Size),
		Size > 0.

	default_option(fragment_size(none)).

	merged_options(UserOptions, MergedOptions) :-
		^^merge_options(UserOptions, MergedOptions).

:- end_object.


:- object(http_websocket_session(_Role_, _TextRepresentation_),
	extends(http_websocket_messages(_TextRepresentation_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Stateful WebSocket session predicates that build on top of the message layer to surface interleaved control frames, apply role-aware masking policies, and track close-handshake state. The callback-driven session loops are provided by the http_websocket_service library.',
		parameters is [
			'Role' - 'Peer role for masking policy. Possible values are ``client`` and ``server``.',
			'TextRepresentation' - 'Text representation to be used for text messages and close reasons. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		],
		remarks is [
			'Option precedence' - 'When the same read or write option is given multiple times, the first occurrence is used.'
		]
	]).

	:- public(initial_state/1).
	:- mode(initial_state(-compound), one).
	:- info(initial_state/1, [
		comment is 'Returns the initial session state term to be threaded through stateful reads.',
		argnames is ['State']
	]).

	:- public(is_state/1).
	:- mode(is_state(@term), zero_or_one).
	:- info(is_state/1, [
		comment is 'True when the argument is a valid session state term.',
		argnames is ['State']
	]).

	:- public(read_message/4).
	:- mode(read_message(+stream_or_alias, +compound, -compound, -term), one_or_error).
	:- info(read_message/4, [
		comment is 'Reads the next session-level WebSocket message from a binary stream using the given input state. Fragmented text and binary messages are reassembled across calls, interleaved control frames are surfaced immediately, and close-message state transitions are tracked in the returned state.',
		argnames is ['Stream', 'State', 'UpdatedState', 'Message'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame'),
			'The inbound frame is invalid' - domain_error(http_websocket_frame, 'Frame'),
			'The inbound message text is invalid' - domain_error(http_websocket_message_text, 'Payload')
		],
		remarks is [
			'End of file' - 'Returns ``end_of_file`` only when the input state is idle and the stream is already exhausted.',
			'Masking policy' - 'Incoming frame masking is validated against the session role: client sessions expect unmasked inbound frames and server sessions expect masked inbound frames.',
			'Peer close' - 'When a close frame is read, any fragmented data message in progress is discarded. Subsequent reads from the resulting ``close_received/1`` or ``closed/2`` state are rejected.'
		]
	]).

	:- public(read_message/5).
	:- mode(read_message(+stream_or_alias, +stream_or_alias, +compound, -compound, -term), one_or_error).
	:- info(read_message/5, [
		comment is 'Reads the next session-level WebSocket message from a binary input stream using the given state and automatically orchestrates the close handshake on the given binary output stream when a close message is received.',
		argnames is ['Input', 'Output', 'State', 'UpdatedState', 'Message'],
		exceptions is [
			'``Input`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame'),
			'The inbound frame is invalid' - domain_error(http_websocket_frame, 'Frame'),
			'The inbound message text is invalid' - domain_error(http_websocket_message_text, 'Payload')
		],
		remarks is [
			'Close handshake' - 'When a close message is received and no close message has yet been sent for the tracked session state, a matching close message is written automatically on the output stream and the returned state records the completed handshake.'
		]
	]).

	:- public(read_message/6).
	:- mode(read_message(+stream_or_alias, +stream_or_alias, +compound, -compound, -term, +list), one_or_error).
	:- info(read_message/6, [
		comment is 'Reads the next session-level WebSocket message from a binary input stream using the given state, automatically orchestrates the close handshake on the given binary output stream, and applies optional automatic control-message policies.',
		argnames is ['Input', 'Output', 'State', 'UpdatedState', 'Message', 'Options'],
		exceptions is [
			'``Input`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid read option' - domain_error(http_websocket_session_read_option, 'Option'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame'),
			'The inbound payload length exceeds the configured limit' - domain_error(http_websocket_payload_length_limit, 'PayloadLength')
		],
		remarks is [
			'Option ``auto_pong(on)``' - 'Automatically writes a pong message with the same payload when a ping message is read while still returning the ping message to the caller.',
			'Option ``auto_pong(off)``' - 'Disables automatic pong replies. This is the default.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Use a non-negative integer.'
		]
	]).

	:- public(write_message/2).
	:- mode(write_message(+stream_or_alias, +compound), one_or_error).
	:- info(write_message/2, [
		comment is 'Stateless convenience wrapper that writes one validated WebSocket message using the masking policy implied by the session role. Client sessions mask all outgoing frames; server sessions leave them unmasked.',
		argnames is ['Stream', 'Message'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Message`` contains invalid text' - domain_error(http_websocket_message_text, 'Text'),
			'The generated outbound frame is invalid' - domain_error(http_websocket_frame, 'Frame')
		]
	]).

	:- public(write_message/3).
	:- mode(write_message(+stream_or_alias, +compound, +list), one_or_error).
	:- info(write_message/3, [
		comment is 'Stateless convenience wrapper that writes one validated WebSocket message using the masking policy implied by the session role and the given write options.',
		argnames is ['Stream', 'Message', 'Options'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid write option' - domain_error(http_websocket_session_write_option, 'Option'),
			'The generated outbound frame is invalid' - domain_error(http_websocket_frame, 'Frame')
		],
		remarks is [
			'Option ``fragment_size(Size)``' - 'When writing ``text`` or ``binary`` messages, split the payload into frames of at most ``Size`` bytes. Control messages always remain single final frames.'
		]
	]).

	:- public(write_message/4).
	:- mode(write_message(+stream_or_alias, +compound, -compound, +compound), one_or_error).
	:- info(write_message/4, [
		comment is 'Writes one validated WebSocket message using the masking policy implied by the session role and updates the given session state with any close-handshake transition caused by the write.',
		argnames is ['Stream', 'State', 'UpdatedState', 'Message'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'The write violates the current close-handshake state' - domain_error(http_websocket_session_state, 'State')
		],
		remarks is [
			'Closing state' - 'After a peer close is recorded, only the matching close reply may be written. After a local close is recorded, further data messages are rejected.'
		]
	]).

	:- public(write_message/5).
	:- mode(write_message(+stream_or_alias, +compound, -compound, +compound, +list), one_or_error).
	:- info(write_message/5, [
		comment is 'Writes one validated WebSocket message using the masking policy implied by the session role and the given write options while updating the given session state with any close-handshake transition caused by the write.',
		argnames is ['Stream', 'State', 'UpdatedState', 'Message', 'Options'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid write option' - domain_error(http_websocket_session_write_option, 'Option'),
			'The write violates the current close-handshake state' - domain_error(http_websocket_session_state, 'State')
		],
		remarks is [
			'Option ``fragment_size(Size)``' - 'When writing ``text`` or ``binary`` messages, split the payload into frames of at most ``Size`` bytes. Control messages always remain single final frames.'
		]
	]).

	% protected hooks used by the http_websocket_service library session loops

	:- protected(validate_role/0).
	:- mode(validate_role, one_or_error).
	:- info(validate_role/0, [
		comment is 'Validates the session role parameter. Throws a domain error when the role is neither ``client`` nor ``server``.',
		exceptions is [
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_')
		]
	]).

	:- protected(read_session_message/5).
	:- mode(read_session_message(+stream_or_alias, +compound, +term, -compound, -term), one_or_error).
	:- info(read_session_message/5, [
		comment is 'Protected helper that validates the session role and state and reads the next session-level message, returning the updated pending-fragment bookkeeping term and the message. Close-state transitions are left to the caller.',
		argnames is ['Stream', 'State', 'MaxPayloadLength', 'Pending', 'Message'],
		exceptions is [
			'The session role parameter is neither ``client`` nor ``server``' - domain_error(http_websocket_session_role, '_Role_'),
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'The inbound frame/message sequence violates RFC 6455 session rules' - domain_error(http_websocket_session_sequence, 'Frame'),
			'The inbound frame violates the current peer masking policy' - domain_error(http_websocket_session_masking, 'Frame'),
			'The inbound frame uses unsupported extensions' - domain_error(http_websocket_session_extensions, 'Frame')
		]
	]).

	:- protected(apply_session_read/6).
	:- mode(apply_session_read(+stream_or_alias, +compound, +atom, +compound, +term, -compound), one_or_error).
	:- info(apply_session_read/6, [
		comment is 'Protected helper that applies the close-state and automatic control-message transitions for a message read outside this object and returns the updated session state.',
		argnames is ['Output', 'State', 'AutoPong', 'Pending', 'Message', 'UpdatedState'],
		exceptions is [
			'``State`` is not a valid WebSocket session state' - domain_error(http_websocket_session_state, 'State'),
			'``AutoPong`` is not a supported automatic pong setting' - domain_error(http_websocket_session_read_option, auto_pong('AutoPong')),
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message')
		]
	]).

	:- protected(current_close_state/2).
	:- mode(current_close_state(+compound, -compound), one).
	:- info(current_close_state/2, [
		comment is 'Protected helper that returns the close-handshake component of a session state term. Open states are reported as ``open``.',
		argnames is ['State', 'CloseState']
	]).

	:- protected(close_handshake_completed/1).
	:- mode(close_handshake_completed(+compound), zero_or_one).
	:- info(close_handshake_completed/1, [
		comment is 'Protected helper that is true when the given session state records a completed close handshake.',
		argnames is ['State']
	]).

	:- protected(best_effort_protocol_error_close/3).
	:- mode(best_effort_protocol_error_close(+stream_or_alias, +compound, +callable), one).
	:- info(best_effort_protocol_error_close/3, [
		comment is 'Protected helper that writes a best-effort close message mapping the given protocol error to its close status code when the session close state still allows it. Write errors are ignored.',
		argnames is ['Output', 'State', 'Error']
	]).

	:- protected(generate_masking_key/1).
	:- mode(generate_masking_key(-list(byte)), one).
	:- info(generate_masking_key/1, [
		comment is 'Protected hook that returns the four-byte masking key to use for outgoing client frames. The default implementation delegates to ``crypto::random_bytes/2``.',
		argnames is ['Key']
	]).

	:- uses(list, [
		append/3, reverse/2
	]).

	:- uses(crypto, [
		random_bytes/2
	]).

	initial_state(session_state(idle)).

	is_state(State) :-
		catch(validate_state(State, _Pending, _Close), _, fail).

	read_message(Stream, State0, State, Message) :-
		(	var(Stream) ->
			instantiation_error
		;	validate_role,
			validate_state(State0, Pending0, Close0),
			read_message_state(Stream, Pending0, Close0, none, Pending, Close, Message),
			output_state(Pending, Close, State)
		).

	read_message(Input, Output, State0, State, Message) :-
		read_message(Input, Output, State0, State, Message, []).

	read_message(Input, Output, State0, State, Message, Options) :-
		(	var(Input) ->
			instantiation_error
		;	var(Output) ->
			instantiation_error
		;	validate_role,
			parse_read_options(Options, AutoPong, MaxPayloadLength),
			validate_state(State0, Pending0, Close0),
			read_message_state(Input, Pending0, Close0, MaxPayloadLength, Pending1, Close1, Message),
			maybe_auto_control_message(Output, Pending1, Close1, Message, AutoPong, Pending, Close),
			output_state(Pending, Close, State)
		).

	write_message(Stream, Message) :-
		write_message(Stream, Message, []).

	write_message(Stream, Message, Options) :-
		(	var(Stream) ->
			instantiation_error
		;	validate_role,
			write_message_state(Stream, idle, open, Message, Options, _Pending, _Close)
		).

	write_message(Stream, State0, State, Message) :-
		write_message(Stream, State0, State, Message, []).

	write_message(Stream, State0, State, Message, Options) :-
		(	var(Stream) ->
			instantiation_error
		;	validate_role,
			validate_state(State0, Pending0, Close0),
			write_message_state(Stream, Pending0, Close0, Message, Options, Pending, Close),
			output_state(Pending, Close, State)
		).

	validate_role :-
		(	_Role_ == client ->
			true
		;	_Role_ == server ->
			true
		;	domain_error(http_websocket_session_role, _Role_)
		).

	validate_state(State0, Pending, Close) :-
		(	var(State0) ->
			instantiation_error
		;	State0 == session_state(idle) ->
			Pending = idle,
			Close = open
		;	State0 = session_state(Pending0) ->
			validate_pending_state(Pending0, Pending),
			Close = open
		;	State0 = session_state(Pending0, Close0) ->
			validate_pending_state(Pending0, Pending),
			validate_close_state(Close0, Close)
		;	domain_error(http_websocket_session_state, State0)
		).

	validate_pending_state(idle, idle) :-
		!.
	validate_pending_state(fragment(Type, Chunks), fragment(Type, Chunks)) :-
		!,
		validate_pending_fragment(Type, Chunks).
	validate_pending_state(Pending0, _Pending) :-
		domain_error(http_websocket_session_state, session_state(Pending0)).

	validate_pending_fragment(Type, Chunks) :-
		(	(Type == text; Type == binary),
			Chunks = [_| _],
			valid_pending_chunks(Chunks) ->
			true
		;	domain_error(http_websocket_session_state, session_state(fragment(Type, Chunks)))
		).

	validate_close_state(open, open) :-
		!.
	validate_close_state(close_sent(Payload0), close_sent(Payload)) :-
		!,
		normalize_close_state_payload(Payload0, Payload).
	validate_close_state(close_received(Payload0), close_received(Payload)) :-
		!,
		normalize_close_state_payload(Payload0, Payload).
	validate_close_state(closed(SentPayload0, ReceivedPayload0), closed(SentPayload, ReceivedPayload)) :-
		!,
		normalize_close_state_payload(SentPayload0, SentPayload),
		normalize_close_state_payload(ReceivedPayload0, ReceivedPayload).
	validate_close_state(CloseState, _Close) :-
		domain_error(http_websocket_session_state, session_state(idle, CloseState)).

	normalize_close_state_payload(Payload0, Payload) :-
		(	catch(^^encode_message(message(close, Payload0), message(close, Payload), _Opcode, _PayloadBytes), _, fail) ->
			true
		;	domain_error(http_websocket_session_state, session_state(idle, Payload0))
		).

	output_state(Pending, open, session_state(Pending)) :-
		!.
	output_state(Pending, Close, session_state(Pending, Close)).

	read_session_message(Stream, State0, MaxPayloadLength, Pending, Message) :-
		validate_role,
		validate_state(State0, Pending0, _Close0),
		read_pending_message(Stream, Pending0, MaxPayloadLength, Pending, Message).

	apply_session_read(Output, State0, AutoPong, Pending0, Message, State) :-
		validate_state(State0, _Pending, Close0),
		update_close_state_after_read(Close0, Message, Close1),
		maybe_auto_control_message(Output, Pending0, Close1, Message, AutoPong, Pending, Close),
		output_state(Pending, Close, State).

	current_close_state(session_state(_Pending), open) :-
		!.
	current_close_state(session_state(_Pending, Close), Close).

	close_handshake_completed(session_state(_Pending, closed(_SentPayload, _ReceivedPayload))).

	best_effort_protocol_error_close(Output, State0, Error) :-
		protocol_error_close_payload(Error, Payload),
		validate_state(State0, Pending0, Close0),
		protocol_error_close_state(Close0),
		!,
		catch(write_message_state(Output, Pending0, Close0, message(close, Payload), [], _Pending, _Close), _, true).
	best_effort_protocol_error_close(_Output, _State0, _Error).

	protocol_error_close_state(open).
	protocol_error_close_state(close_received(_Payload)).

	protocol_error_close_payload(error(domain_error(http_websocket_session_sequence, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_session_masking, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_session_extensions, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_frame, _Frame), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_opcode, _Opcode), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_byte_sequence, _Bytes), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_close_payload, _Payload), _Context), status(1002)).
	protocol_error_close_payload(error(domain_error(http_websocket_message_text, _Payload), _Context), status(1007)).
	protocol_error_close_payload(error(domain_error(http_websocket_payload_length_limit, _PayloadLength), _Context), status(1009)).

	read_message_state(Stream, Pending0, Close0, MaxPayloadLength, Pending, Close, Message) :-
		validate_read_close_state(Pending0, Close0),
		read_pending_message(Stream, Pending0, MaxPayloadLength, Pending0b, Message),
		pending_after_read(Pending0b, Message, Pending),
		update_close_state_after_read(Close0, Message, Close).

	validate_read_close_state(_Pending0, open) :-
		!.
	validate_read_close_state(_Pending0, close_sent(_Payload)) :-
		!.
	validate_read_close_state(Pending0, Close0) :-
		output_state(Pending0, Close0, State),
		domain_error(http_websocket_session_state, State).

	pending_after_read(_Pending0, message(close, _Payload), idle) :-
		!.
	pending_after_read(Pending0, _Message, Pending0).

	update_close_state_after_read(Close0, end_of_file, Close0) :-
		!.
	update_close_state_after_read(Close0, message(close, Payload), Close) :-
		!,
		next_close_state_after_read(Close0, Payload, Close).
	update_close_state_after_read(Close0, _Message, Close0).

	next_close_state_after_read(open, Payload, close_received(Payload)) :-
		!.
	next_close_state_after_read(close_sent(SentPayload), Payload, closed(SentPayload, Payload)) :-
		!.
	next_close_state_after_read(close_received(Payload), _NewPayload, close_received(Payload)) :-
		!.
	next_close_state_after_read(closed(SentPayload, ReceivedPayload), _Payload, closed(SentPayload, ReceivedPayload)).

	valid_pending_chunks([]).
	valid_pending_chunks([Chunk| Chunks]) :-
		http_websocket_frames::frame(final, binary, Chunk, [], _Frame),
		valid_pending_chunks(Chunks).

	read_pending_message(Stream, idle, MaxPayloadLength, Pending, Message) :-
		!,
		read_websocket_frame(Stream, MaxPayloadLength, Frame),
		read_idle_frame(Frame, Stream, MaxPayloadLength, Pending, Message).
	read_pending_message(Stream, fragment(Type, Chunks0), MaxPayloadLength, Pending, Message) :-
		read_websocket_frame(Stream, MaxPayloadLength, Frame),
		read_fragment_frame(Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message).

	read_idle_frame(end_of_file, _Stream, _MaxPayloadLength, idle, end_of_file) :-
		!.
	read_idle_frame(Frame, Stream, MaxPayloadLength, Pending, Message) :-
		validate_incoming_frame(Frame),
		http_websocket_frames::opcode(Frame, Opcode),
		http_websocket_frames::final(Frame, Final),
		http_websocket_frames::payload(Frame, PayloadBytes),
		read_idle_opcode(Opcode, Final, PayloadBytes, Frame, Stream, MaxPayloadLength, Pending, Message).

	read_idle_opcode(continuation, _Final, _PayloadBytes, Frame, _Stream, _MaxPayloadLength, _Pending, _Message) :-
		!,
		domain_error(http_websocket_session_sequence, Frame).
	read_idle_opcode(text, final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(text, PayloadBytes, Message).
	read_idle_opcode(binary, final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(binary, PayloadBytes, Message).
	read_idle_opcode(text, more, PayloadBytes, _Frame, Stream, MaxPayloadLength, Pending, Message) :-
		!,
		read_fragmented_message(Stream, text, [PayloadBytes], MaxPayloadLength, Pending, Message).
	read_idle_opcode(binary, more, PayloadBytes, _Frame, Stream, MaxPayloadLength, Pending, Message) :-
		!,
		read_fragmented_message(Stream, binary, [PayloadBytes], MaxPayloadLength, Pending, Message).
	read_idle_opcode(ping, _Final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(ping, PayloadBytes, Message).
	read_idle_opcode(pong, _Final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		!,
		^^decode_message(pong, PayloadBytes, Message).
	read_idle_opcode(close, _Final, PayloadBytes, _Frame, _Stream, _MaxPayloadLength, idle, Message) :-
		^^decode_message(close, PayloadBytes, Message).

	read_fragmented_message(Stream, Type, Chunks0, MaxPayloadLength, Pending, Message) :-
		read_websocket_frame(Stream, MaxPayloadLength, Frame),
		read_fragment_frame(Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message).

	read_fragment_frame(end_of_file, _Stream, _Type, _Chunks0, _MaxPayloadLength, _Pending, _Message) :-
		!,
		domain_error(http_websocket_session_sequence, end_of_file).
	read_fragment_frame(Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message) :-
		validate_incoming_frame(Frame),
		http_websocket_frames::opcode(Frame, Opcode),
		http_websocket_frames::final(Frame, Final),
		http_websocket_frames::payload(Frame, PayloadBytes),
		read_fragment_opcode(Opcode, Final, PayloadBytes, Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message).

	read_fragment_opcode(continuation, final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, idle, Message) :-
		!,
		reverse([PayloadBytes| Chunks0], Chunks),
		append_chunks(Chunks, Bytes, []),
		^^decode_message(Type, Bytes, Message).
	read_fragment_opcode(continuation, more, PayloadBytes, _Frame, Stream, Type, Chunks0, MaxPayloadLength, Pending, Message) :-
		!,
		read_fragmented_message(Stream, Type, [PayloadBytes| Chunks0], MaxPayloadLength, Pending, Message).
	read_fragment_opcode(ping, _Final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, fragment(Type, Chunks0), Message) :-
		!,
		^^decode_message(ping, PayloadBytes, Message).
	read_fragment_opcode(pong, _Final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, fragment(Type, Chunks0), Message) :-
		!,
		^^decode_message(pong, PayloadBytes, Message).
	read_fragment_opcode(close, _Final, PayloadBytes, _Frame, _Stream, Type, Chunks0, _MaxPayloadLength, fragment(Type, Chunks0), Message) :-
		!,
		^^decode_message(close, PayloadBytes, Message).
	read_fragment_opcode(_Opcode, _Final, _PayloadBytes, Frame, _Stream, _Type, _Chunks0, _MaxPayloadLength, _Pending, _Message) :-
		domain_error(http_websocket_session_sequence, Frame).

	read_websocket_frame(Stream, none, Frame) :-
		!,
		http_websocket_frames::read_frame(Stream, Frame).
	read_websocket_frame(Stream, MaxPayloadLength, Frame) :-
		http_websocket_frames::read_frame(Stream, Frame, [max_payload_length(MaxPayloadLength)]).

	validate_incoming_frame(Frame) :-
		validate_incoming_frame_extensions(Frame),
		validate_incoming_frame_masking(Frame).

	validate_incoming_frame_extensions(Frame) :-
		(	\+ http_websocket_frames::property(Frame, reserved_bits(_Bits)) ->
			true
		;	domain_error(http_websocket_session_extensions, Frame)
		).

	validate_incoming_frame_masking(Frame) :-
		_Role_ == client,
		!,
		(	\+ http_websocket_frames::property(Frame, masking_key(_)) ->
			true
		;	domain_error(http_websocket_session_masking, Frame)
		).
	validate_incoming_frame_masking(Frame) :-
		(	http_websocket_frames::property(Frame, masking_key(_)) ->
			true
		;	domain_error(http_websocket_session_masking, Frame)
		).

	parse_read_options(Options, AutoPong, MaxPayloadLength) :-
		check_read_options(Options),
		http_websocket_session_read_options::merged_options(Options, MergedOptions),
		http_websocket_session_read_options::option(auto_pong(AutoPong), MergedOptions),
		http_websocket_session_read_options::option(max_payload_length(MaxPayloadLength), MergedOptions).

	check_read_options(Options) :-
		catch(
			http_websocket_session_read_options::check_options(Options),
			error(domain_error(option, Option), _),
			domain_error(http_websocket_session_read_option, Option)
		).

	parse_write_options(Options, FragmentSize) :-
		check_write_options(Options),
		http_websocket_session_write_options::merged_options(Options, MergedOptions),
		http_websocket_session_write_options::option(fragment_size(FragmentSize), MergedOptions).

	check_write_options(Options) :-
		catch(
			http_websocket_session_write_options::check_options(Options),
			error(domain_error(option, Option), _),
			domain_error(http_websocket_session_write_option, Option)
		).

	write_message_state(Stream, Pending0, Close0, Message0, Options, Pending, Close) :-
		parse_write_options(Options, FragmentSize),
		^^encode_message(Message0, Message, Opcode, PayloadBytes),
		validate_write_close_state(Pending0, Close0, Message),
		write_encoded_message(Stream, Opcode, PayloadBytes, FragmentSize),
		Pending = Pending0,
		update_close_state_after_write(Close0, Message, Close).

	validate_write_close_state(_Pending0, open, _Message) :-
		!.
	validate_write_close_state(_Pending0, close_sent(_SentPayload), message(Type, _MessagePayload)) :-
		\+ data_opcode(Type),
		!.
	validate_write_close_state(_Pending0, close_received(_ReceivedPayload), message(close, _Payload)) :-
		!.
	validate_write_close_state(Pending0, Close0, _Message) :-
		output_state(Pending0, Close0, State),
		domain_error(http_websocket_session_state, State).

	update_close_state_after_write(Close0, message(close, Payload), Close) :-
		!,
		next_close_state_after_write(Close0, Payload, Close).
	update_close_state_after_write(Close0, _Message, Close0).

	next_close_state_after_write(open, Payload, close_sent(Payload)) :-
		!.
	next_close_state_after_write(close_received(ReceivedPayload), Payload, closed(Payload, ReceivedPayload)) :-
		!.
	next_close_state_after_write(close_sent(SentPayload), _Payload, close_sent(SentPayload)) :-
		!.
	next_close_state_after_write(closed(SentPayload, ReceivedPayload), _Payload, closed(SentPayload, ReceivedPayload)).

	maybe_auto_control_message(Output, Pending0, close_received(Payload), message(close, _MessagePayload), _AutoPong, Pending, Close) :-
		!,
		write_message_state(Output, Pending0, close_received(Payload), message(close, Payload), [], Pending, Close).
	maybe_auto_control_message(Output, Pending0, Close0, message(ping, Payload), on, Pending, Close) :-
		!,
		write_message_state(Output, Pending0, Close0, message(pong, Payload), [], Pending, Close).
	maybe_auto_control_message(_Output, Pending, Close, _Message, _AutoPong, Pending, Close).

	write_encoded_message(Stream, Opcode, PayloadBytes, FragmentSize) :-
		(	data_opcode(Opcode),
			FragmentSize \== none ->
			split_payload_bytes(PayloadBytes, FragmentSize, Chunks),
			write_chunk_frames(Stream, Opcode, Chunks)
		;	write_session_frame(Stream, final, Opcode, PayloadBytes)
		).

	data_opcode(text).
	data_opcode(binary).

	write_chunk_frames(Stream, Opcode, []) :-
		!,
		write_session_frame(Stream, final, Opcode, []).
	write_chunk_frames(Stream, Opcode, [Chunk]) :-
		!,
		write_session_frame(Stream, final, Opcode, Chunk).
	write_chunk_frames(Stream, Opcode, [Chunk| Chunks]) :-
		write_session_frame(Stream, more, Opcode, Chunk),
		write_continuation_frames(Stream, Chunks).

	write_continuation_frames(Stream, [Chunk]) :-
		!,
		write_session_frame(Stream, final, continuation, Chunk).
	write_continuation_frames(Stream, [Chunk| Chunks]) :-
		write_session_frame(Stream, more, continuation, Chunk),
		write_continuation_frames(Stream, Chunks).

	write_session_frame(Stream, Final, Opcode, PayloadBytes) :-
		outgoing_frame_properties(Properties),
		http_websocket_frames::frame(Final, Opcode, PayloadBytes, Properties, Frame),
		http_websocket_frames::write_frame(Stream, Frame).

	outgoing_frame_properties(Properties) :-
		(	_Role_ == client ->
			::generate_masking_key(Key),
			Properties = [masking_key(Key)]
		;	Properties = []
		).

	generate_masking_key(Key) :-
		random_bytes(4, Key).

	split_payload_bytes(Bytes, FragmentSize, Chunks) :-
		take_payload_chunks(Bytes, FragmentSize, Chunks).

	take_payload_chunks([], _FragmentSize, []) :-
		!.
	take_payload_chunks(Bytes, FragmentSize, [Chunk| Chunks]) :-
		take_payload_chunk(Bytes, FragmentSize, Chunk, Rest),
		take_payload_chunks(Rest, FragmentSize, Chunks).

	take_payload_chunk(Rest, 0, [], Rest) :-
		!.
	take_payload_chunk([], _Remaining, [], []) :-
		!.
	take_payload_chunk([Byte| Bytes], Remaining, [Byte| Chunk], Rest) :-
		NextRemaining is Remaining - 1,
		take_payload_chunk(Bytes, NextRemaining, Chunk, Rest).

	append_chunks([], Bytes, Bytes).
	append_chunks([Chunk| Chunks], Bytes0, Bytes) :-
		append(Chunk, Bytes1, Bytes0),
		append_chunks(Chunks, Bytes1, Bytes).

:- end_object.


:- object(http_websocket_client_session,
	extends(http_websocket_session(client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Stateful WebSocket session predicates for client-side use with atom text representation.'
	]).

:- end_object.


:- object(http_websocket_server_session,
	extends(http_websocket_session(server, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Stateful WebSocket session predicates for server-side use with atom text representation.'
	]).

:- end_object.
