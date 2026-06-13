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


:- object(http_client_core,
	imports(http_message_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-13,
		comment is 'Portable stream-based HTTP client core predicates.',
		remarks is [
			'Binary streams' - 'All stream predicates expect binary input and output streams.',
			'Per-message primitives' - 'The ``write_request/2``, ``read_response/2``, and ``exchange/4`` predicates process a single HTTP exchange.',
			'Connection sequence' - 'The ``exchange_connection/4`` predicate performs sequential exchanges on the same stream pair while HTTP persistence rules allow it.'
		]
	]).

	:- public(write_request/2).
	:- mode(write_request(+stream, +compound), one_or_error).
	:- info(write_request/2, [
		comment is 'Writes exactly one normalized HTTP request term to a binary stream.',
		argnames is ['Output', 'Request']
	]).

	:- public(read_response/2).
	:- mode(read_response(+stream, --compound), zero_or_one).
	:- info(read_response/2, [
		comment is 'Reads exactly one HTTP response from a binary stream and returns it as a normalized response term. Fails on clean end-of-file before reading any bytes. Responses may be status-bodyless, Content-Length framed, Transfer-Encoding chunked, or close-delimited by end-of-file. Close-delimited responses are annotated with a ``body_framing(close_delimited)`` property.',
		argnames is ['Input', 'Response']
	]).

	:- public(exchange/4).
	:- mode(exchange(+stream, +stream, +compound, --compound), one_or_error).
	:- info(exchange/4, [
		comment is 'Writes one normalized request to a binary output stream and then reads one normalized response from a binary input stream.',
		argnames is ['Input', 'Output', 'Request', 'Response']
	]).

	:- public(exchange_connection/4).
	:- mode(exchange_connection(+stream, +stream, ++list(compound), --list(compound)), one_or_error).
	:- info(exchange_connection/4, [
		comment is 'Performs a sequence of request-response exchanges on the same binary stream pair. If additional requests remain when HTTP persistence rules require connection closure, an error is thrown.',
		argnames is ['Input', 'Output', 'Requests', 'Responses']
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2
	]).

	write_request(Output, Request) :-
		http_core::generate_request(bytes(Bytes), Request),
		write_bytes(Bytes, Output),
		flush_output(Output).

	read_response(Input, Response) :-
		read_response_bytes(Input, Bytes, BodyFraming),
		(	http_core::parse_response(bytes(Bytes), Response0) ->
			attach_body_framing_property(BodyFraming, Response0, Response)
		;	domain_error(http_response_stream, malformed_response(Bytes))
		).

	exchange(Input, Output, Request, Response) :-
		write_request(Output, Request),
		(	read_response_for_request(Input, Request, Response) ->
			true
		;	domain_error(http_response_stream, unexpected_end_of_file)
		).

	exchange_connection(Input, Output, Requests, Responses) :-
		validate_request_sequence(Requests),
		exchange_connection_requests(Requests, Input, Output, Responses).

	exchange_connection_requests([], _Input, _Output, []).
	exchange_connection_requests([Request| Requests], Input, Output, [Response| Responses]) :-
		exchange(Input, Output, Request, Response),
		(	Requests == [] ->
			Responses = []
		;	^^connection_persistent(Request, Response) ->
			exchange_connection_requests(Requests, Input, Output, Responses)
		;	domain_error(http_client_connection, remaining_requests(Requests))
		).

	validate_request_sequence(Requests) :-
		(	var(Requests) ->
			domain_error(http_client_connection, Requests)
		;	Requests == [] ->
			true
		;	Requests = [Request| Rest] ->
			validate_request(Request),
			validate_request_sequence(Rest)
		;	domain_error(http_client_connection, Requests)
		).

	validate_request(Request) :-
		(	http_core::is_request(Request) ->
			true
		;	domain_error(http_request, Request)
		).

	read_response_for_request(Input, Request, Response) :-
		http_core::method(Request, Method),
		(	Method == head ->
			read_head_response(Input, Response)
		;	read_response(Input, Response)
		).

	read_head_response(Input, Response) :-
		read_line_bytes(Input, LineResult),
		LineResult = line(StatusLineBytes),
		append(StatusLineBytes, [0'\r, 0'\n], StatusLineBytesCRLF),
		(	http_core::parse_status_line(codes(StatusLineBytesCRLF), status_line(Version, Status)) ->
			true
		;	domain_error(http_response_stream, malformed_status_line(StatusLineBytesCRLF))
		),
		read_header_block(Input, _HeaderBytes, Headers),
		build_head_response(Version, Status, Headers, Response).

	build_head_response(Version, Status, Headers0, Response) :-
		strip_head_content_length_headers(Headers0, Headers, Properties0),
		http_core::response(Version, Status, Headers, empty, [body_omitted(head)| Properties0], Response).

	strip_head_content_length_headers([], [], []).
	strip_head_content_length_headers([content_length-Length| Headers0], Headers, [omitted_body_length(Length)| Properties]) :-
		!,
		strip_head_content_length_headers(Headers0, Headers, Properties).
	strip_head_content_length_headers([Header| Headers0], [Header| Headers], Properties) :-
		strip_head_content_length_headers(Headers0, Headers, Properties).

	read_response_bytes(Input, Bytes, BodyFraming) :-
		read_line_bytes(Input, LineResult),
		LineResult = line(StatusLineBytes),
		append(StatusLineBytes, [0'\r, 0'\n], StatusLineBytesCRLF),
		(	http_core::parse_status_line(codes(StatusLineBytesCRLF), status_line(_Version, Status)) ->
			true
		;	domain_error(http_response_stream, malformed_status_line(StatusLineBytesCRLF))
		),
		read_header_block(Input, HeaderBytes, Headers),
		read_response_body_bytes(Input, Status, Headers, BodyBytes, BodyFraming),
		append(StatusLineBytesCRLF, HeaderBytes, HeadBytes),
		append(HeadBytes, [0'\r, 0'\n| BodyBytes], Bytes).

	attach_body_framing_property(none, Response, Response) :-
		!.
	attach_body_framing_property(close_delimited, response(Version, Status, Headers, Body, Properties0), Response) :-
		(	member(body_framing(close_delimited), Properties0) ->
			Properties = Properties0
		;	Properties = [body_framing(close_delimited)| Properties0]
		),
		http_core::response(Version, Status, Headers, Body, Properties, Response).

	read_header_block(Input, HeaderBytes, Headers) :-
		read_header_block_bytes(Input, HeaderBytes, []),
		http_core::parse_headers(codes(HeaderBytes), Headers).

	read_header_block_bytes(Input, Bytes0, Bytes) :-
		read_line_bytes(Input, LineResult),
		(	LineResult == end_of_file ->
			domain_error(http_response_stream, unexpected_end_of_file)
		;	LineResult = line(LineBytes),
			read_header_block_line(Input, LineBytes, Bytes0, Bytes)
		).

	read_header_block_line(_Input, [], Bytes, Bytes) :-
		!.
	read_header_block_line(Input, LineBytes, Bytes0, Bytes) :-
		append(LineBytes, [0'\r, 0'\n| Bytes1], Bytes0),
		read_header_block_bytes(Input, Bytes1, Bytes).

	read_response_body_bytes(_Input, Status, _Headers, [], none) :-
		status_without_body(Status),
		!.
	read_response_body_bytes(Input, _Status, Headers, BodyBytes, none) :-
		member(transfer_encoding-Encodings, Headers),
		!,
		read_transfer_body_bytes(Input, Encodings, BodyBytes).
	read_response_body_bytes(Input, _Status, Headers, BodyBytes, none) :-
		member(content_length-Length, Headers),
		!,
		read_exact_bytes(Input, Length, BodyBytes).
	read_response_body_bytes(Input, _Status, Headers, BodyBytes, close_delimited) :-
		\+ member(transfer_encoding-_, Headers),
		\+ member(content_length-_, Headers),
		read_remaining_bytes(Input, BodyBytes).

	status_without_body(status(Code, _Reason)) :-
		(	Code >= 100, Code < 200 ->
			true
		;	memberchk(Code, [204, 205, 304])
		).

	read_transfer_body_bytes(Input, [chunked], BodyBytes) :-
		!,
		read_chunked_body_bytes(Input, BodyBytes, []).
	read_transfer_body_bytes(_Input, Encodings, _BodyBytes) :-
		domain_error(http_response_stream, unsupported_transfer_encoding(Encodings)).

	read_chunked_body_bytes(Input, Bytes0, Bytes) :-
		read_line_bytes(Input, LineResult),
		(	LineResult == end_of_file ->
			domain_error(http_response_stream, unexpected_end_of_file)
		;	LineResult = line(SizeLineBytes),
			^^chunk_size_line_size(SizeLineBytes, Size),
			append(SizeLineBytes, [0'\r, 0'\n| Bytes1], Bytes0),
			(	Size =:= 0 ->
				read_header_block(Input, TrailerBytes, _),
				append(TrailerBytes, [0'\r, 0'\n| Bytes], Bytes1)
			;	read_exact_bytes(Input, Size, DataBytes),
				append(DataBytes, [0'\r, 0'\n| Bytes2], Bytes1),
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
			domain_error(http_response_stream, unexpected_end_of_file)
		;	NextLength is Length - 1,
			read_exact_bytes(Input, NextLength, Bytes0, Bytes)
		).

	read_remaining_bytes(Input, Bytes) :-
		get_byte(Input, Byte),
		read_remaining_bytes(Byte, Input, Bytes).

	read_remaining_bytes(-1, _Input, []) :-
		!.
	read_remaining_bytes(Byte, Input, [Byte| Bytes]) :-
		get_byte(Input, NextByte),
		read_remaining_bytes(NextByte, Input, Bytes).

	read_crlf(Input) :-
		get_byte(Input, CarriageReturn),
		get_byte(Input, LineFeed),
		(	CarriageReturn =:= 0'\r,
			LineFeed =:= 0'\n ->
			true
		;	domain_error(http_response_stream, invalid_line_ending(CarriageReturn, LineFeed))
		).

	read_line_bytes(Input, Result) :-
		get_byte(Input, Byte),
		(	Byte =:= -1 ->
			Result = end_of_file
		;	read_line_bytes(Byte, Input, Bytes),
			Result = line(Bytes)
		).

	read_line_bytes(0'\r, Input, []) :-
		!,
		get_byte(Input, Byte),
		(	Byte =:= 0'\n ->
			true
		;	domain_error(http_response_stream, invalid_line_ending(0'\r, Byte))
		).
	read_line_bytes(0'\n, _Input, _) :-
		domain_error(http_response_stream, bare_line_feed).
	read_line_bytes(Byte, Input, [Byte| Bytes]) :-
		get_byte(Input, NextByte),
		read_line_bytes(NextByte, Input, Bytes).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

:- end_object.
