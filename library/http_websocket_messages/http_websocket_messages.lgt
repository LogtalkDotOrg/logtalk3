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


:- object(http_websocket_messages(_TextRepresentation_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Transport-neutral WebSocket message predicates built on top of the http_websocket frame layer.',
		remarks is [
			'Fragmentation model' - 'This layer reassembles fragmented text and binary messages only when all continuation frames arrive contiguously. Interleaved control frames during fragmented messages raise ``http_websocket_message_sequence``. Use ``http_websocket_session`` for RFC 6455 live-stream processing that must surface interleaved control frames while reassembling data messages.'
		],
		parameters is [
			'TextRepresentation' - 'Text representation to be used for text messages and close reasons. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	:- public(message/3).
	:- mode(message(+atom, +term, -compound), one_or_error).
	:- info(message/3, [
		comment is 'Constructs a validated normalized WebSocket message term from the message type atom and payload term.',
		argnames is ['Type', 'Payload', 'Message'],
		exceptions is [
			'``Type`` is not a supported WebSocket message type' - domain_error(http_websocket_message_type, 'Type'),
			'``Payload`` is not valid for the requested WebSocket message type' - domain_error(http_websocket_message, 'Message'),
			'``Payload`` is not a valid WebSocket close payload' - domain_error(http_websocket_close_payload, 'Payload'),
			'``Payload`` contains an invalid WebSocket close code' - domain_error(http_websocket_close_code, 'Code'),
			'``Payload`` is not valid text for the selected text representation' - domain_error(http_websocket_message_text, 'Text')
		],
		remarks is [
			'Type ``text``' - 'Payload is text in the selected text representation.',
			'Type ``binary``' - 'Payload is a list of bytes.',
			'Type ``ping`` or ``pong``' - 'Payload is a list of bytes.',
			'Type ``close``' - 'Payload is one of ``empty``, ``status(Code)``, or ``status(Code, Reason)``.'
		]
	]).

	:- public(is_message/1).
	:- mode(is_message(@term), zero_or_one).
	:- info(is_message/1, [
		comment is 'True when the argument is a valid normalized WebSocket message term.',
		argnames is ['Message']
	]).

	:- public(read_message/2).
	:- mode(read_message(+stream_or_alias, -term), one_or_error).
	:- info(read_message/2, [
		comment is 'Reads one WebSocket message from a binary stream. Continuation frames are reassembled for text and binary messages when they arrive contiguously. Returns ``end_of_file`` when the stream is already exhausted.',
		argnames is ['Stream', 'Message'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'The inbound frames do not form a valid normalized WebSocket message sequence' - domain_error(http_websocket_message_sequence, 'Frame'),
			'The inbound frame is invalid' - domain_error(http_websocket_frame, 'Frame'),
			'The inbound message payload is invalid' - domain_error(http_websocket_message, 'Message'),
			'The inbound message text is invalid for the selected text representation' - domain_error(http_websocket_message_text, 'Text')
		],
		remarks is [
			'Interleaved control frames' - 'Control frames interleaved in the middle of a fragmented data message are not surfaced by this simplified layer and instead raise ``http_websocket_message_sequence``. Use ``http_websocket_session`` when interleaved control frames must be handled while a fragmented message is in progress.'
		]
	]).

	:- public(write_message/2).
	:- mode(write_message(+stream_or_alias, +compound), one_or_error).
	:- info(write_message/2, [
		comment is 'Writes one validated WebSocket message as a single final frame to a binary stream and flushes the stream.',
		argnames is ['Stream', 'Message'],
		exceptions is [
			'``Stream`` is a variable' - instantiation_error,
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Message`` has an unsupported message type' - domain_error(http_websocket_message_type, 'Type'),
			'``Message`` has an invalid close payload' - domain_error(http_websocket_close_payload, 'Payload'),
			'``Message`` has invalid text for the selected text representation' - domain_error(http_websocket_message_text, 'Text')
		]
	]).

	:- public(type/2).
	:- mode(type(+compound, -atom), one_or_error).
	:- info(type/2, [
		comment is 'Returns the type atom of a validated WebSocket message term.',
		argnames is ['Message', 'Type'],
		exceptions is [
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Message`` has an unsupported message type' - domain_error(http_websocket_message_type, 'Type')
		]
	]).

	:- public(payload/2).
	:- mode(payload(+compound, -term), one_or_error).
	:- info(payload/2, [
		comment is 'Returns the payload term of a validated WebSocket message term.',
		argnames is ['Message', 'Payload'],
		exceptions is [
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Message`` has an unsupported message type' - domain_error(http_websocket_message_type, 'Type')
		]
	]).

	:- protected(encode_message/4).
	:- mode(encode_message(+compound, -compound, -atom, -list(byte)), one_or_error).
	:- info(encode_message/4, [
		comment is 'Protected helper that normalizes a WebSocket message term and returns the normalized term together with the corresponding frame opcode and payload bytes.',
		argnames is ['Message', 'NormalizedMessage', 'Opcode', 'PayloadBytes'],
		exceptions is [
			'``Message`` is not a valid normalized WebSocket message term' - domain_error(http_websocket_message, 'Message'),
			'``Message`` has an unsupported message type' - domain_error(http_websocket_message_type, 'Type'),
			'``Message`` has an invalid close payload' - domain_error(http_websocket_close_payload, 'Payload'),
			'``Message`` has invalid text for the selected text representation' - domain_error(http_websocket_message_text, 'Text')
		]
	]).

	:- protected(decode_message/3).
	:- mode(decode_message(+atom, +list(byte), -compound), one_or_error).
	:- info(decode_message/3, [
		comment is 'Protected helper that decodes payload bytes for the given message type atom into a normalized WebSocket message term.',
		argnames is ['Type', 'PayloadBytes', 'Message'],
		exceptions is [
			'``Type`` is not a supported WebSocket message type' - domain_error(http_websocket_message_type, 'Type'),
			'``PayloadBytes`` is not a valid close payload' - domain_error(http_websocket_close_payload, 'PayloadBytes'),
			'``PayloadBytes`` contains an invalid close code' - domain_error(http_websocket_close_code, 'Code'),
			'``PayloadBytes`` does not decode to valid text for the selected text representation' - domain_error(http_websocket_message_text, 'PayloadBytes')
		]
	]).

	:- uses(list, [
		append/3, reverse/2
	]).

	message(Type, Payload0, Message) :-
		encode_message(message(Type, Payload0), Message, _Opcode, _PayloadBytes).

	is_message(Message) :-
		catch(encode_message(Message, _NormalizedMessage, _Opcode, _PayloadBytes), _, fail).

	read_message(Stream, Message) :-
		(	var(Stream) ->
			instantiation_error
		;	http_websocket_frames::read_frame(Stream, Frame),
			read_frame_message(Frame, Stream, Message)
		).

	write_message(Stream, Message) :-
		(	var(Stream) ->
			instantiation_error
		;	encode_message(Message, _NormalizedMessage, Opcode, PayloadBytes),
			http_websocket_frames::frame(final, Opcode, PayloadBytes, [], Frame),
			http_websocket_frames::write_frame(Stream, Frame)
		).

	type(Message, Type) :-
		encode_message(Message, message(Type, _Payload), _Opcode, _PayloadBytes).

	payload(Message, Payload) :-
		encode_message(Message, message(_Type, Payload), _Opcode, _PayloadBytes).

	encode_message(Message0, Message, Opcode, PayloadBytes) :-
		normalize_message(Message0, Message, Opcode, PayloadBytes).

	decode_message(text, PayloadBytes, Message) :-
		!,
		decode_data_message(text, PayloadBytes, Message).
	decode_message(binary, PayloadBytes, Message) :-
		!,
		decode_data_message(binary, PayloadBytes, Message).
	decode_message(ping, PayloadBytes, message(ping, PayloadBytes)) :-
		!,
		http_websocket_frames::frame(final, ping, PayloadBytes, [], _Frame).
	decode_message(pong, PayloadBytes, message(pong, PayloadBytes)) :-
		!,
		http_websocket_frames::frame(final, pong, PayloadBytes, [], _Frame).
	decode_message(close, PayloadBytes, Message) :-
		!,
		http_websocket_frames::frame(final, close, PayloadBytes, [], _Frame),
		close_payload_message(PayloadBytes, Message).
	decode_message(Type, _PayloadBytes, _Message) :-
		domain_error(http_websocket_message_type, Type).

	read_frame_message(end_of_file, _Stream, end_of_file) :-
		!.
	read_frame_message(Frame, Stream, Message) :-
		http_websocket_frames::opcode(Frame, Opcode),
		http_websocket_frames::final(Frame, Final),
		http_websocket_frames::payload(Frame, PayloadBytes),
		read_message_opcode(Opcode, Final, PayloadBytes, Frame, Stream, Message).

	read_message_opcode(continuation, _Final, _PayloadBytes, Frame, _Stream, _Message) :-
		domain_error(http_websocket_message_sequence, Frame).
	read_message_opcode(text, Final, PayloadBytes, _Frame, Stream, Message) :-
		read_data_message(Final, text, PayloadBytes, Stream, Message).
	read_message_opcode(binary, Final, PayloadBytes, _Frame, Stream, Message) :-
		read_data_message(Final, binary, PayloadBytes, Stream, Message).
	read_message_opcode(ping, _Final, PayloadBytes, _Frame, _Stream, message(ping, PayloadBytes)).
	read_message_opcode(pong, _Final, PayloadBytes, _Frame, _Stream, message(pong, PayloadBytes)).
	read_message_opcode(close, _Final, PayloadBytes, _Frame, _Stream, Message) :-
		close_payload_message(PayloadBytes, Message).

	read_data_message(final, Type, PayloadBytes, _Stream, Message) :-
		decode_data_message(Type, PayloadBytes, Message).
	read_data_message(more, Type, PayloadBytes0, Stream, Message) :-
		read_continuation_payload(Stream, [PayloadBytes0], PayloadBytes),
		decode_data_message(Type, PayloadBytes, Message).

	read_continuation_payload(Stream, Chunks0, PayloadBytes) :-
		http_websocket_frames::read_frame(Stream, Frame),
		(	Frame == end_of_file ->
			domain_error(http_websocket_message_sequence, end_of_file)
		;	http_websocket_frames::opcode(Frame, continuation) ->
			http_websocket_frames::payload(Frame, Chunk),
			http_websocket_frames::final(Frame, Final),
			(	Final == final ->
				reverse([Chunk| Chunks0], Chunks),
				append_chunks(Chunks, PayloadBytes, [])
			;	read_continuation_payload(Stream, [Chunk| Chunks0], PayloadBytes)
			)
		;	domain_error(http_websocket_message_sequence, Frame)
		).

	append_chunks([], Bytes, Bytes).
	append_chunks([Chunk| Chunks], Bytes0, Bytes) :-
		append(Chunk, Bytes1, Bytes0),
		append_chunks(Chunks, Bytes1, Bytes).

	decode_data_message(text, PayloadBytes, message(text, Text)) :-
		bytes_text(PayloadBytes, Text).
	decode_data_message(binary, PayloadBytes, message(binary, PayloadBytes)).

	close_payload_message([], message(close, empty)) :-
		!.
	close_payload_message([Byte0, Byte1], message(close, status(Code))) :-
		!,
		Code is (Byte0 << 8) \/ Byte1.
	close_payload_message([Byte0, Byte1| ReasonBytes], message(close, status(Code, Reason))) :-
		Code is (Byte0 << 8) \/ Byte1,
		bytes_text(ReasonBytes, Reason).

	normalize_message(Message0, Message, Opcode, PayloadBytes) :-
		(	var(Message0) ->
			instantiation_error
		;	Message0 = message(Type, Payload0) ->
			message_type_opcode(Type, Opcode),
			normalize_message_payload(Type, Payload0, Payload, PayloadBytes),
			Message = message(Type, Payload)
		;	domain_error(http_websocket_message, Message0)
		).

	message_type_opcode(text, text) :-
		!.
	message_type_opcode(binary, binary) :-
		!.
	message_type_opcode(ping, ping) :-
		!.
	message_type_opcode(pong, pong) :-
		!.
	message_type_opcode(close, close) :-
		!.
	message_type_opcode(Type, _Opcode) :-
		domain_error(http_websocket_message_type, Type).

	normalize_message_payload(text, Text0, Text, PayloadBytes) :-
		text_bytes(Text0, Text, PayloadBytes),
		http_websocket_frames::frame(final, text, PayloadBytes, [], _Frame).
	normalize_message_payload(binary, Payload, Payload, Payload) :-
		http_websocket_frames::frame(final, binary, Payload, [], _Frame).
	normalize_message_payload(ping, Payload, Payload, Payload) :-
		http_websocket_frames::frame(final, ping, Payload, [], _Frame).
	normalize_message_payload(pong, Payload, Payload, Payload) :-
		http_websocket_frames::frame(final, pong, Payload, [], _Frame).
	normalize_message_payload(close, Payload0, Payload, PayloadBytes) :-
		(	normalize_close_payload(Payload0, Payload, PayloadBytes) ->
			http_websocket_frames::frame(final, close, PayloadBytes, [], _Frame)
		;	domain_error(http_websocket_close_payload, Payload0)
		).

	normalize_close_payload(empty, empty, []).
	normalize_close_payload(status(Code), status(Code), [Byte0, Byte1]) :-
		close_code_bytes(Code, Byte0, Byte1).
	normalize_close_payload(status(Code, Reason0), status(Code, Reason), [Byte0, Byte1| ReasonBytes]) :-
		close_code_bytes(Code, Byte0, Byte1),
		text_bytes(Reason0, Reason, ReasonBytes).

	close_code_bytes(Code, Byte0, Byte1) :-
		(	integer(Code) ->
			Byte0 is (Code >> 8) /\ 0xFF,
			Byte1 is Code /\ 0xFF
		;	domain_error(http_websocket_close_code, Code)
		).

	text_bytes(Text0, Text, Bytes) :-
		(	text_codes(_TextRepresentation_, Text0, Codes),
		 	utf_8::codes_to_bytes(Codes, Bytes),
			codes_text(_TextRepresentation_, Codes, Text) ->
			true
		;	domain_error(http_websocket_message_text, Text0)
		).

	bytes_text(Bytes, Text) :-
		(	utf_8::bytes_to_codes(Bytes, Codes),
			codes_text(_TextRepresentation_, Codes, Text) ->
			true
		;	domain_error(http_websocket_message_text, Bytes)
		).

	text_codes(atom, Text, Codes) :-
		(	atom(Text) ->
			atom_codes(Text, Codes)
		;	domain_error(http_websocket_message_text, Text)
		).
	text_codes(chars, Text, Codes) :-
		(	Text = chars(Chars) ->
			chars_to_codes(Chars, Codes)
		;	domain_error(http_websocket_message_text, Text)
		).
	text_codes(codes, Text, Codes) :-
		(	Text = codes(Codes) ->
			true
		;	domain_error(http_websocket_message_text, Text)
		).

	codes_text(atom, Codes, Text) :-
		atom_codes(Text, Codes).
	codes_text(chars, Codes, Text) :-
		codes_to_chars(Codes, Chars),
		Text = chars(Chars).
	codes_text(codes, Codes, Text) :-
		Text = codes(Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.


:- object(http_websocket_messages,
	extends(http_websocket_messages(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'Transport-neutral WebSocket message predicates using atoms for text messages and close reasons.'
	]).

:- end_object.
