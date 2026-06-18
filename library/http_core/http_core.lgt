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


:- object(http_core,
	imports(http_text_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-18,
		comment is 'Transport-independent normalized HTTP request and response constructors, validators, wire parsers and generators, and body codec dispatch.'
	]).

	:- public(request/7).
	:- mode(request(+atom, +compound, +compound, +list(compound), +compound, +list(compound), -compound), one_or_error).
	:- info(request/7, [
		comment is 'Constructs a validated normalized ``request/6`` term from the given method, target, version, headers, body, and properties.',
		argnames is ['Method', 'Target', 'Version', 'Headers', 'Body', 'Properties', 'Request'],
		exceptions is [
			'``Method`` is not a valid HTTP method atom' - domain_error(http_method, 'Method'),
			'``Target`` is not a valid normalized HTTP target term' - domain_error(http_target, 'Target'),
			'``Version`` is not a valid HTTP version term' - domain_error(http_version, 'Version'),
			'``Headers`` is not a valid normalized HTTP header list' - domain_error(http_headers, 'Headers'),
			'``Body`` is not a valid normalized HTTP body term' - domain_error(http_body, 'Body'),
			'``Properties`` is not a valid normalized HTTP property list' - domain_error(http_properties, 'Properties'),
			'The request headers, body, or properties violate normalized HTTP request semantics' - error
		]
	]).

	:- public(response/6).
	:- mode(response(+compound, +compound, +list(compound), +compound, +list(compound), -compound), one_or_error).
	:- info(response/6, [
		comment is 'Constructs a validated normalized ``response/5`` term from the given version, status, headers, body, and properties.',
		argnames is ['Version', 'Status', 'Headers', 'Body', 'Properties', 'Response'],
		exceptions is [
			'``Version`` is not a valid HTTP version term' - domain_error(http_version, 'Version'),
			'``Status`` is not a valid normalized HTTP status term' - domain_error(http_status, 'Status'),
			'``Headers`` is not a valid normalized HTTP header list' - domain_error(http_headers, 'Headers'),
			'``Body`` is not a valid normalized HTTP body term' - domain_error(http_body, 'Body'),
			'``Properties`` is not a valid normalized HTTP property list' - domain_error(http_properties, 'Properties'),
			'The response headers, body, or properties violate normalized HTTP response semantics' - error
		]
	]).

	:- public(is_request/1).
	:- mode(is_request(+term), zero_or_one).
	:- info(is_request/1, [
		comment is 'Succeeds if the given term is a structurally and semantically valid normalized ``request/6`` term.',
		argnames is ['Request']
	]).

	:- public(is_response/1).
	:- mode(is_response(+term), zero_or_one).
	:- info(is_response/1, [
		comment is 'Succeeds if the given term is a structurally and semantically valid normalized ``response/5`` term.',
		argnames is ['Response']
	]).

	:- public(parse_request/2).
	:- mode(parse_request(++compound, --compound), one_or_error).
	:- info(parse_request/2, [
		comment is 'Parses a complete HTTP request message from the given source (``bytes(List)``, ``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) into a normalized ``request/6`` term.',
		argnames is ['Source', 'Request'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid HTTP source term' - domain_error(http_source, 'Source'),
			'The parsed request violates normalized HTTP request validation or semantics' - error
		]
	]).

	:- public(generate_request/2).
	:- mode(generate_request(+compound, ++compound), one_or_error).
	:- info(generate_request/2, [
		comment is 'Generates a complete HTTP request message to the given sink (``bytes(List)``, ``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) from a normalized ``request/6`` term.',
		argnames is ['Sink', 'Request'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink'),
			'``Request`` is not a valid normalized HTTP request term' - error
		]
	]).

	:- public(parse_response/2).
	:- mode(parse_response(++compound, --compound), one_or_error).
	:- info(parse_response/2, [
		comment is 'Parses a complete HTTP response message from the given source (``bytes(List)``, ``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) into a normalized ``response/5`` term.',
		argnames is ['Source', 'Response'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid HTTP source term' - domain_error(http_source, 'Source'),
			'The parsed response violates normalized HTTP response validation or semantics' - error
		]
	]).

	:- public(generate_response/2).
	:- mode(generate_response(+compound, ++compound), one_or_error).
	:- info(generate_response/2, [
		comment is 'Generates a complete HTTP response message to the given sink (``bytes(List)``, ``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) from a normalized ``response/5`` term.',
		argnames is ['Sink', 'Response'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink'),
			'``Response`` is not a valid normalized HTTP response term' - error
		]
	]).

	:- public(generate_response_headers/2).
	:- mode(generate_response_headers(+compound, ++compound), one_or_error).
	:- info(generate_response_headers/2, [
		comment is 'Generates an HTTP response status line and effective header block, terminated by an empty line, to the given sink from a normalized ``response/5`` term.',
		argnames is ['Sink', 'Response'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink'),
			'``Response`` is not a valid normalized HTTP response term' - error
		]
	]).

	:- public(parse_request_line/2).
	:- mode(parse_request_line(++compound, --compound), one_or_error).
	:- info(parse_request_line/2, [
		comment is 'Parses an HTTP request line from the given source (``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) into a ``request_line(Method, Target, Version)`` term.',
		argnames is ['Source', 'RequestLine'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid HTTP source term' - domain_error(http_source, 'Source')
		]
	]).

	:- public(generate_request_line/2).
	:- mode(generate_request_line(+compound, ++compound), one_or_error).
	:- info(generate_request_line/2, [
		comment is 'Generates an HTTP request line to the given sink (``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) from a ``request_line(Method, Target, Version)`` term.',
		argnames is ['Sink', 'RequestLine'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink')
		]
	]).

	:- public(parse_status_line/2).
	:- mode(parse_status_line(++compound, --compound), one_or_error).
	:- info(parse_status_line/2, [
		comment is 'Parses an HTTP status line from the given source (``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) into a ``status_line(Version, Status)`` term.',
		argnames is ['Source', 'StatusLine'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid HTTP source term' - domain_error(http_source, 'Source')
		]
	]).

	:- public(generate_status_line/2).
	:- mode(generate_status_line(+compound, ++compound), one_or_error).
	:- info(generate_status_line/2, [
		comment is 'Generates an HTTP status line to the given sink (``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) from a ``status_line(Version, Status)`` term.',
		argnames is ['Sink', 'StatusLine'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink')
		]
	]).

	:- public(parse_headers/2).
	:- mode(parse_headers(++compound, -list(compound)), one_or_error).
	:- info(parse_headers/2, [
		comment is 'Parses an HTTP header block from the given source (``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) into normalized header ``Name-Value`` pairs with typed values for recognized headers.',
		argnames is ['Source', 'Headers'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid HTTP source term' - domain_error(http_source, 'Source')
		]
	]).

	:- public(generate_headers/2).
	:- mode(generate_headers(+compound, ++list(compound)), one_or_error).
	:- info(generate_headers/2, [
		comment is 'Generates an HTTP header block to the given sink (``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) from normalized header ``Name-Value`` pairs.',
		argnames is ['Sink', 'Headers'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink'),
			'``Headers`` is not a valid normalized HTTP header list' - domain_error(http_headers, 'Headers')
		]
	]).

	:- public(parse_body/4).
	:- mode(parse_body(++compound, ++atom, +list(compound), --compound), one_or_error).
	:- info(parse_body/4, [
		comment is 'Parses a body payload from the given source (``bytes(List)``, ``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) according to the given media type and options into a normalized body term.',
		argnames is ['Source', 'MediaType', 'Options', 'Body'],
		exceptions is [
			'``Source`` is a variable' - instantiation_error,
			'``Source`` is neither a variable nor a valid HTTP source term' - domain_error(http_source, 'Source'),
			'``MediaType`` is not a valid HTTP media type atom' - domain_error(http_media_type, 'MediaType'),
			'``Options`` is not a valid HTTP body options list' - domain_error(http_body_options, 'Options'),
			'No registered HTTP body codec exists for ``MediaType`` when codec-based decoding is required' - existence_error(http_body_codec, 'MediaType')
		]
	]).

	:- public(generate_body/3).
	:- mode(generate_body(+compound, ++compound, +list(compound)), one_or_error).
	:- info(generate_body/3, [
		comment is 'Generates a body payload to the given sink (``bytes(List)``, ``codes(List)``, ``chars(List)``, ``atom(Atom)``, ``file(Path)``, or ``stream(Stream)``) from a normalized body term and options.',
		argnames is ['Sink', 'Body', 'Options'],
		exceptions is [
			'``Sink`` is a variable' - instantiation_error,
			'``Sink`` is neither a variable nor a valid HTTP sink term' - domain_error(http_sink, 'Sink'),
			'``Body`` is not a valid normalized HTTP body term' - domain_error(http_body, 'Body'),
			'``Options`` is not a valid HTTP body options list' - domain_error(http_body_options, 'Options'),
			'No registered HTTP body codec exists for the body media type when codec-based encoding is required' - existence_error(http_body_codec, 'MediaType')
		]
	]).

	:- public(encode_body/4).
	:- mode(encode_body(+atom, ++term, +list(compound), -compound), one_or_error).
	:- info(encode_body/4, [
		comment is 'Encodes a semantic payload term using the registered concrete body codec for the given media type into a normalized body term.',
		argnames is ['MediaType', 'Payload', 'Options', 'Body'],
		exceptions is [
			'``MediaType`` is not a valid HTTP media type atom' - domain_error(http_media_type, 'MediaType'),
			'``Options`` is not a valid HTTP body options list' - domain_error(http_body_options, 'Options'),
			'No registered HTTP body codec exists for ``MediaType``' - existence_error(http_body_codec, 'MediaType')
		]
	]).

	:- public(decode_body/4).
	:- mode(decode_body(+atom, ++compound, +list(compound), --term), one_or_error).
	:- info(decode_body/4, [
		comment is 'Decodes a normalized body term using the registered concrete body codec for the given media type into a semantic payload term.',
		argnames is ['MediaType', 'Body', 'Options', 'Payload'],
		exceptions is [
			'``MediaType`` is not a valid HTTP media type atom' - domain_error(http_media_type, 'MediaType'),
			'``Options`` is not a valid HTTP body options list' - domain_error(http_body_options, 'Options'),
			'No registered HTTP body codec exists for ``MediaType``' - existence_error(http_body_codec, 'MediaType')
		]
	]).

	:- public(method/2).
	:- mode(method(+compound, -atom), one_or_error).
	:- info(method/2, [
		comment is 'Returns the request method from a normalized ``request/6`` term.',
		argnames is ['Request', 'Method'],
		exceptions is []
	]).

	:- public(target/2).
	:- mode(target(+compound, -compound), one_or_error).
	:- info(target/2, [
		comment is 'Returns the request target from a normalized ``request/6`` term.',
		argnames is ['Request', 'Target'],
		exceptions is []
	]).

	:- public(version/2).
	:- mode(version(+compound, -compound), one_or_error).
	:- info(version/2, [
		comment is 'Returns the HTTP version from a normalized request or response term.',
		argnames is ['Message', 'Version'],
		exceptions is []
	]).

	:- public(status/2).
	:- mode(status(+compound, -compound), one_or_error).
	:- info(status/2, [
		comment is 'Returns the response status from a normalized ``response/5`` term.',
		argnames is ['Response', 'Status'],
		exceptions is []
	]).

	:- public(headers/2).
	:- mode(headers(+compound, -list(compound)), one_or_error).
	:- info(headers/2, [
		comment is 'Returns the normalized header list from a request or response term.',
		argnames is ['Message', 'Headers'],
		exceptions is []
	]).

	:- public(header/3).
	:- mode(header(+compound, ?atom, ?term), zero_or_more).
	:- info(header/3, [
		comment is 'Enumerates normalized header ``Name`` and ``Value`` pairs from a request or response term.',
		argnames is ['Message', 'Name', 'Value']
	]).

	:- public(body/2).
	:- mode(body(+compound, -compound), one_or_error).
	:- info(body/2, [
		comment is 'Returns the normalized body term from a request or response term.',
		argnames is ['Message', 'Body'],
		exceptions is []
	]).

	:- public(property/2).
	:- mode(property(+compound, ?compound), zero_or_more).
	:- info(property/2, [
		comment is 'Enumerates normalized derived or higher-layer properties from a request or response term.',
		argnames is ['Message', 'Property']
	]).

	:- uses(list, [
		append/2, append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(base64, [
		parse/2, generate/2
	]).

	:- uses(reader, [
		file_to_bytes/2, file_to_codes/2, stream_to_bytes/2, stream_to_codes/2
	]).

	request(Method, Target, Version, Headers, Body, Properties, Request) :-
		validate_method(Method),
		validate_target(Target),
		validate_version(Version),
		validate_headers(Headers),
		validate_body(Body),
		validate_properties(Properties),
		validate_request_semantics(Target, Headers, Body, Properties),
		Request = request(Method, Target, Version, Headers, Body, Properties).

	response(Version, Status, Headers, Body, Properties, Response) :-
		validate_version(Version),
		validate_status(Status),
		validate_headers(Headers),
		validate_body(Body),
		validate_properties(Properties),
		validate_response_semantics(Headers, Body, Properties),
		Response = response(Version, Status, Headers, Body, Properties).

	is_request(Request) :-
		catch(validate_request_term(Request), _, fail).

	is_response(Response) :-
		catch(validate_response_term(Response), _, fail).

	parse_request(Source, Request) :-
		source_to_bytes(Source, Bytes),
		phrase(request_head(Method, Target, Version, Headers), Bytes, BodyBytes),
		!,
		message_body_from_headers(Headers, BodyBytes, Body, ExtraProperties),
		derived_request_properties(Target, Headers, Body, DerivedProperties),
		merge_property_lists([DerivedProperties, ExtraProperties], Properties),
		validate_method(Method),
		validate_target(Target),
		validate_version(Version),
		validate_headers(Headers),
		validate_body(Body),
		validate_properties(Properties),
		validate_request_semantics(Target, Headers, Body, Properties, parsed),
		Request = request(Method, Target, Version, Headers, Body, Properties).

	generate_request(Sink, Request) :-
		validate_request_term(Request),
		Request = request(Method, Target, Version, Headers0, Body, Properties),
		request_body_bytes(Headers0, Body, Properties, RawBodyBytes),
		request_effective_headers(Target, Headers0, Body, Properties, Headers),
		message_wire_body(Headers, Properties, RawBodyBytes, BodyBytes),
		request_line_codes(request_line(Method, Target, Version), LineCodes),
		headers_codes(Headers, HeaderCodes),
		append(LineCodes, HeaderCodes, Prefix0),
		append(Prefix0, [0'\r, 0'\n], Prefix1),
		append(Prefix1, BodyBytes, Bytes),
		bytes_to_sink(Sink, Bytes).

	parse_response(Source, Response) :-
		source_to_bytes(Source, Bytes),
		phrase(response_head(Version, Status, Headers), Bytes, BodyBytes),
		!,
		message_body_from_headers(Headers, BodyBytes, Body, ExtraProperties),
		derived_response_properties(Headers, Body, DerivedProperties),
		merge_property_lists([DerivedProperties, ExtraProperties], Properties),
		validate_version(Version),
		validate_status(Status),
		validate_headers(Headers),
		validate_body(Body),
		validate_properties(Properties),
		validate_response_semantics(Headers, Body, Properties, parsed),
		Response = response(Version, Status, Headers, Body, Properties).

	generate_response(Sink, Response) :-
		validate_response_term(Response),
		Response = response(Version, Status, Headers0, Body, Properties),
		response_body_bytes(Headers0, Body, Properties, RawBodyBytes),
		response_effective_headers(Headers0, Body, Properties, Headers),
		message_wire_body(Headers, Properties, RawBodyBytes, BodyBytes),
		response_prefix_bytes(Version, Status, Headers, HeaderBytes),
		append(HeaderBytes, BodyBytes, Bytes),
		bytes_to_sink(Sink, Bytes).

	generate_response_headers(Sink, Response) :-
		validate_response_term(Response),
		response_header_bytes(Response, HeaderBytes),
		bytes_to_sink(Sink, HeaderBytes).

	parse_request_line(Source, RequestLine) :-
		source_to_codes(Source, Codes),
		phrase(public_request_line(RequestLine), Codes),
		!.

	generate_request_line(Sink, RequestLine) :-
		request_line_codes(RequestLine, Codes),
		codes_to_sink(Sink, Codes).

	parse_status_line(Source, StatusLine) :-
		source_to_codes(Source, Codes),
		phrase(public_status_line(StatusLine), Codes),
		!.

	generate_status_line(Sink, StatusLine) :-
		status_line_codes(StatusLine, Codes),
		codes_to_sink(Sink, Codes).

	parse_headers(Source, Headers) :-
		source_to_codes(Source, Codes),
		phrase(public_headers(Headers), Codes).

	generate_headers(Sink, Headers) :-
		validate_headers(Headers),
		headers_codes(Headers, Codes),
		codes_to_sink(Sink, Codes).

	parse_body(Source, MediaType, Options, Body) :-
		validate_media_type(MediaType),
		validate_body_options(Options),
		source_body_options(Source, Options, EffectiveOptions),
		source_to_bytes(Source, Bytes),
		body_bytes_to_term(Bytes, MediaType, EffectiveOptions, Body).

	source_body_options(_Source, Options, Options) :-
		member(wire_bytes(true), Options),
		!.
	source_body_options(Source, Options, [wire_bytes(true)| Options]) :-
		raw_body_byte_source(Source),
		!.
	source_body_options(_Source, Options, Options).

	raw_body_byte_source(file(_)).
	raw_body_byte_source(stream(_)).
	raw_body_byte_source(bytes(_)).

	generate_body(Sink, Body, Options) :-
		validate_body(Body),
		validate_body_options(Options),
		body_term_to_bytes(Body, Options, Bytes),
		bytes_to_sink(Sink, Bytes).

	encode_body(MediaType, Payload, Options, Body) :-
		validate_media_type(MediaType),
		validate_body_options(Options),
		(	body_codec(MediaType, Codec) ->
			Codec::encode_body(MediaType, Payload, Options, Body)
		;	existence_error(http_body_codec, MediaType)
		).

	decode_body(MediaType, Body, Options, Payload) :-
		validate_media_type(MediaType),
		validate_body_options(Options),
		(	body_codec(MediaType, Codec) ->
			Codec::decode_body(MediaType, Body, Options, Payload)
		;	existence_error(http_body_codec, MediaType)
		).

	method(request(Method, _, _, _, _, _), Method).

	target(request(_, Target, _, _, _, _), Target).

	version(request(_, _, Version, _, _, _), Version).
	version(response(Version, _, _, _, _), Version).

	status(response(_, Status, _, _, _), Status).

	headers(request(_, _, _, Headers, _, _), Headers).
	headers(response(_, _, Headers, _, _), Headers).

	header(Message, Name, Value) :-
		headers(Message, Headers),
		(	nonvar(Name), nonvar(Value) ->
			memberchk(Name-Value, Headers)
		;	member(Name-Value, Headers)
		).

	body(request(_, _, _, _, Body, _), Body).
	body(response(_, _, _, Body, _), Body).

	property(request(_, _, _, _, _, Properties), Property) :-
		(	nonvar(Property) ->
			memberchk(Property, Properties)
		;	member(Property, Properties)
		).
	property(response(_, _, _, _, Properties), Property) :-
		(	nonvar(Property) ->
			memberchk(Property, Properties)
		;	member(Property, Properties)
		).

	validate_request_term(request(Method, Target, Version, Headers, Body, Properties)) :-
		validate_method(Method),
		validate_target(Target),
		validate_version(Version),
		validate_headers(Headers),
		validate_body(Body),
		validate_properties(Properties),
		validate_request_semantics(Target, Headers, Body, Properties).

	validate_response_term(response(Version, Status, Headers, Body, Properties)) :-
		validate_version(Version),
		validate_status(Status),
		validate_headers(Headers),
		validate_body(Body),
		validate_properties(Properties),
		validate_response_semantics(Headers, Body, Properties).

	validate_request_semantics(Target, Headers, Body, Properties) :-
		validate_request_semantics(Target, Headers, Body, Properties, strict).

	validate_request_semantics(Target, Headers, Body, Properties, Mode) :-
		validate_transfer_encoding_semantics(Headers, Properties),
		validate_header_body_semantics(Headers, Body, Properties, Mode),
		validate_header_target_semantics(Target, Headers),
		derived_request_properties(Target, Headers, Body, DerivedProperties),
		validate_property_semantics(Properties, DerivedProperties).

	validate_response_semantics(Headers, Body, Properties) :-
		validate_response_semantics(Headers, Body, Properties, strict).

	validate_response_semantics(Headers, Body, Properties, Mode) :-
		validate_transfer_encoding_semantics(Headers, Properties),
		validate_header_body_semantics(Headers, Body, Properties, Mode),
		derived_response_properties(Headers, Body, DerivedProperties),
		validate_property_semantics(Properties, DerivedProperties).

	validate_transfer_encoding_semantics(Headers, Properties) :-
		(	transfer_encoding_tokens(Headers, Properties, Tokens) ->
			validate_supported_transfer_encoding(Tokens),
			validate_chunked_content_length_semantics(Headers, Tokens)
		;	true
		),
		validate_trailer_property_semantics(Headers, Properties).

	validate_supported_transfer_encoding([chunked]) :-
		!.
	validate_supported_transfer_encoding(Tokens) :-
		domain_error(http_transfer_encoding, Tokens).

	validate_chunked_content_length_semantics(Headers, [chunked]) :-
		(	semantic_single_header_value(Headers, content_length, _) ->
			domain_error(http_header_semantics, transfer_encoding([chunked]))
		;	true
		),
		!.
	validate_chunked_content_length_semantics(_Headers, _Tokens).

	validate_trailer_property_semantics(Headers, Properties) :-
		(	member(TrailersProperty, Properties), functor(TrailersProperty, trailers, 1) ->
			(	transfer_encoding_tokens(Headers, Properties, Tokens) ->
				(	Tokens == [chunked] ->
					true
				;	domain_error(http_property_semantics, TrailersProperty)
				)
			;	true
			)
		;	true
		).

	validate_header_body_semantics(Headers, Body, Properties, Mode) :-
		(	semantic_single_header_value(Headers, content_length, Length) ->
			validate_content_length_semantics(Mode, Headers, Properties, Length, Body)
		;	true
		),
		(	semantic_single_header_value(Headers, content_type, MediaTypeProperty) ->
			validate_content_type_semantics(MediaTypeProperty, Body)
		;	true
		).

	validate_content_length_semantics(parsed, _Headers, _Properties, _Length, _Body).
	validate_content_length_semantics(strict, Headers, Properties, Length, Body) :-
		validate_content_length_semantics(Headers, Properties, Length, Body).

	validate_header_target_semantics(Target, Headers) :-
		(	semantic_single_header_value(Headers, host, HeaderHost), target_host_property(Target, TargetHost) ->
			(	HeaderHost == TargetHost ->
				true
			;	domain_error(http_header_semantics, host(HeaderHost))
			)
		;	true
		).

	validate_content_length_semantics(Headers, Properties, Length, Body) :-
		(	body_length_if_known(Headers, Properties, Body, BodyLength) ->
			(	Length =:= BodyLength ->
				true
			;	domain_error(http_header_semantics, content_length(Length))
			)
		;	true
		).

	validate_content_type_semantics(media_type(HeaderMediaType, _Parameters), empty) :-
		HeaderMediaType \== '',
		!.
	validate_content_type_semantics(media_type(HeaderMediaType, _Parameters), content(BodyMediaType, _Payload)) :-
		(	same_media_type(HeaderMediaType, BodyMediaType) ->
			true
		;	domain_error(http_header_semantics, content_type(HeaderMediaType))
		).

	validate_property_semantics(Properties, DerivedProperties) :-
		validate_explicit_property_uniqueness(Properties),
		validate_explicit_property_semantics(Properties, DerivedProperties).

	validate_explicit_property_uniqueness([]).
	validate_explicit_property_uniqueness([Property| Properties]) :-
		(	same_semantic_property(Properties, Property, Other), Other \== Property ->
			domain_error(http_property_semantics, Property)
		;	true
		),
		validate_explicit_property_uniqueness(Properties).

	validate_explicit_property_semantics([], _).
	validate_explicit_property_semantics([Property| Properties], DerivedProperties) :-
		(	semantic_property(Property), same_semantic_property(DerivedProperties, Property, DerivedProperty) ->
			(	DerivedProperty == Property ->
				true
			;	domain_error(http_property_semantics, Property)
			)
		;	true
		),
		validate_explicit_property_semantics(Properties, DerivedProperties).

	derived_request_properties(Target, Headers, Body, Properties) :-
		properties_from_target(Target, TargetProperties),
		properties_from_headers(Headers, HeaderProperties),
		properties_from_body(Body, BodyProperties),
		merge_property_lists([TargetProperties, HeaderProperties, BodyProperties], Properties).

	derived_response_properties(Headers, Body, Properties) :-
		properties_from_headers(Headers, HeaderProperties),
		properties_from_body(Body, BodyProperties),
		merge_property_lists([HeaderProperties, BodyProperties], Properties).

	properties_from_target(Target, Properties) :-
		properties_from_target(Target, [], ReversedProperties),
		reverse(ReversedProperties, Properties).

	properties_from_target(absolute(Components), Acc, Properties) :-
		(	memberchk(scheme(Scheme0), Components) ->
			normalize_atom_text(Scheme0, Scheme),
			add_unique_property(scheme(Scheme), Acc, Acc0)
		;	Acc0 = Acc
		),
		(	memberchk(authority(Authority), Components), authority_host_property(Authority, HostProperty) ->
			add_unique_property(HostProperty, Acc0, Acc1)
		;	Acc1 = Acc0
		),
		(	memberchk(path(Path), Components) ->
			path_segments_property(Path, PathProperty),
			add_optional_property(PathProperty, Acc1, Acc2)
		;	Acc2 = Acc1
		),
		(	memberchk(query(Query), Components), query_pairs_property(Query, QueryProperty) ->
			add_unique_property(QueryProperty, Acc2, Properties)
		;	Properties = Acc2
		).
	properties_from_target(authority(Host), Acc, Properties) :-
		normalize_atom_text(Host, NormalizedHost),
		add_unique_property(host(NormalizedHost), Acc, Properties).
	properties_from_target(authority(Host, Port), Acc, Properties) :-
		normalize_atom_text(Host, NormalizedHost),
		add_unique_property(host(NormalizedHost, Port), Acc, Properties).
	properties_from_target(origin(Path), Acc, Properties) :-
		path_segments_property(Path, PathProperty),
		add_optional_property(PathProperty, Acc, Properties).
	properties_from_target(origin(Path, Query), Acc, Properties) :-
		path_segments_property(Path, PathProperty),
		add_optional_property(PathProperty, Acc, Acc0),
		(	query_pairs_property(Query, QueryProperty) ->
			add_unique_property(QueryProperty, Acc0, Properties)
		;	Properties = Acc0
		).
	properties_from_target(asterisk, Properties, Properties).

	properties_from_headers(Headers, Properties) :-
		properties_from_headers(Headers, [], ReversedProperties),
		reverse(ReversedProperties, Properties).

	properties_from_headers(Headers, Acc, Properties) :-
		(	semantic_single_header_value(Headers, host, HostProperty) ->
			add_unique_property(HostProperty, Acc, Acc0)
		;	Acc0 = Acc
		),
		(	semantic_single_header_value(Headers, content_type, media_type(MediaType, Parameters)) ->
			add_unique_property(content_type(MediaType, Parameters), Acc0, Acc1)
		;	Acc1 = Acc0
		),
		(	semantic_single_header_value(Headers, content_length, Length) ->
			add_unique_property(content_length(Length), Acc1, Acc2)
		;	Acc2 = Acc1
		),
		(	semantic_single_header_value(Headers, cookie, Pairs) ->
			add_unique_property(cookies(Pairs), Acc2, Acc3)
		;	Acc3 = Acc2
		),
		header_values(Headers, set_cookie, SetCookies),
		(	SetCookies \== [] ->
			add_unique_property(set_cookies(SetCookies), Acc3, Acc4)
		;	Acc4 = Acc3
		),
		(	semantic_single_header_value(Headers, connection, ConnectionTokens) ->
			add_unique_property(connection(ConnectionTokens), Acc4, Acc5)
		;	Acc5 = Acc4
		),
		(	semantic_single_header_value(Headers, upgrade, UpgradeTokens) ->
			add_unique_property(upgrade(UpgradeTokens), Acc5, Acc6)
		;	Acc6 = Acc5
		),
		(	semantic_single_header_value(Headers, sec_websocket_key, Key) ->
			add_unique_property(websocket_key(Key), Acc6, Acc7)
		;	Acc7 = Acc6
		),
		(	semantic_websocket_version_header_value(Headers, Version) ->
			add_unique_property(websocket_version(Version), Acc7, Acc8)
		;	Acc8 = Acc7
		),
		(	semantic_single_header_value(Headers, sec_websocket_accept, Accept) ->
			add_unique_property(websocket_accept(Accept), Acc8, Acc9)
		;	Acc9 = Acc8
		),
		(	semantic_single_header_value(Headers, sec_websocket_protocol, Protocols) ->
			add_unique_property(websocket_protocol(Protocols), Acc9, Acc10)
		;	Acc10 = Acc9
		),
		(	semantic_single_header_value(Headers, transfer_encoding, TransferTokens) ->
			add_unique_property(transfer_encoding(TransferTokens), Acc10, Properties)
		;	Properties = Acc10
		).

	properties_from_body(Body, Properties) :-
		properties_from_body(Body, [], ReversedProperties),
		reverse(ReversedProperties, Properties).

	properties_from_body(Body, Acc, Properties) :-
		(	body_decoded_state(Body, false),
			body_length_if_known(Body, Length) ->
			add_unique_property(content_length(Length), Acc, Acc0)
		;	Acc0 = Acc
		),
		body_decoded_state(Body, Decoded),
		add_unique_property(decoded_body(Decoded), Acc0, Properties).

	merge_property_lists(PropertyLists, Properties) :-
		merge_property_lists(PropertyLists, [], ReversedProperties),
		reverse(ReversedProperties, Properties).

	merge_property_lists([], Properties, Properties).
	merge_property_lists([List| Lists], Acc, Properties) :-
		merge_property_list(List, Acc, Acc0),
		merge_property_lists(Lists, Acc0, Properties).

	merge_property_list([], Properties, Properties).
	merge_property_list([Property| Rest], Acc, Properties) :-
		add_unique_property(Property, Acc, Acc0),
		merge_property_list(Rest, Acc0, Properties).

	add_optional_property(none, Properties, Properties) :-
		!.
	add_optional_property(Property, Properties0, Properties) :-
		add_unique_property(Property, Properties0, Properties).

	add_unique_property(Property, Properties, Properties) :-
		member(Property, Properties),
		!.
	add_unique_property(Property, Properties, Properties) :-
		semantic_property(Property),
		same_semantic_property(Properties, Property, ExistingProperty),
		ExistingProperty == Property,
		!.
	add_unique_property(Property, Properties, _) :-
		semantic_property(Property),
		same_semantic_property(Properties, Property, ExistingProperty),
		ExistingProperty \== Property,
		!,
		domain_error(http_property_semantics, Property).
	add_unique_property(Property, Properties, [Property| Properties]).

	same_semantic_property([ExistingProperty| _], Property, ExistingProperty) :-
		same_property_kind(ExistingProperty, Property),
		!.
	same_semantic_property([_| Properties], Property, ExistingProperty) :-
		same_semantic_property(Properties, Property, ExistingProperty).

	same_property_kind(Property1, Property2) :-
		functor(Property1, Functor, Arity),
		functor(Property2, Functor, Arity),
		semantic_property_functor(Functor, Arity).

	semantic_property(Property) :-
		functor(Property, Functor, Arity),
		semantic_property_functor(Functor, Arity).

	semantic_property_functor(content_type, 2).
	semantic_property_functor(content_length, 1).
	semantic_property_functor(host, 1).
	semantic_property_functor(host, 2).
	semantic_property_functor(cookies, 1).
	semantic_property_functor(set_cookies, 1).
	semantic_property_functor(query_pairs, 1).
	semantic_property_functor(path_segments, 1).
	semantic_property_functor(scheme, 1).
	semantic_property_functor(decoded_body, 1).
	semantic_property_functor(connection, 1).
	semantic_property_functor(upgrade, 1).
	semantic_property_functor(websocket_key, 1).
	semantic_property_functor(websocket_version, 1).
	semantic_property_functor(websocket_accept, 1).
	semantic_property_functor(websocket_protocol, 1).
	semantic_property_functor(transfer_encoding, 1).

	request_effective_headers(Target, Headers0, Body, Properties, Headers) :-
		maybe_add_request_host_header(Target, Properties, Headers0, Headers1),
		maybe_add_cookie_header(Properties, Headers1, Headers2),
		maybe_add_connection_header(Properties, Headers2, Headers3),
		maybe_add_upgrade_header(Properties, Headers3, Headers4),
		maybe_add_websocket_key_header(Properties, Headers4, Headers5),
		maybe_add_websocket_version_header(Properties, Headers5, Headers6),
		maybe_add_websocket_protocol_header(Properties, Headers6, Headers7),
		maybe_add_transfer_encoding_header(Properties, Headers7, Headers8),
		maybe_add_content_type_header(Body, Properties, Headers8, Headers9),
		maybe_add_content_length_header(Body, Properties, Headers9, Headers).

	response_effective_headers(Headers0, Body, Properties, Headers) :-
		maybe_add_set_cookie_headers(Properties, Headers0, Headers1),
		maybe_add_connection_header(Properties, Headers1, Headers2),
		maybe_add_upgrade_header(Properties, Headers2, Headers3),
		maybe_add_websocket_accept_header(Properties, Headers3, Headers4),
		maybe_add_websocket_version_header(Properties, Headers4, Headers5),
		maybe_add_websocket_protocol_header(Properties, Headers5, Headers6),
		maybe_add_transfer_encoding_header(Properties, Headers6, Headers7),
		maybe_add_content_type_header(Body, Properties, Headers7, Headers8),
		maybe_add_content_length_header(Body, Properties, Headers8, Headers).

	response_header_bytes(response(Version, Status, Headers0, Body, Properties), Bytes) :-
		response_effective_headers(Headers0, Body, Properties, Headers),
		response_prefix_bytes(Version, Status, Headers, Bytes).

	response_prefix_bytes(Version, Status, Headers, Bytes) :-
		status_line_codes(status_line(Version, Status), LineCodes),
		headers_codes(Headers, HeaderCodes),
		append(LineCodes, HeaderCodes, Prefix0),
		append(Prefix0, [0'\r, 0'\n], Bytes).

	maybe_add_request_host_header(_Target, _Properties, Headers, Headers) :-
		header_name_present(Headers, host),
		!.
	maybe_add_request_host_header(_Target, Properties, Headers, [host-HostValue| Headers]) :-
		(	member(host(Host), Properties), HostValue = host(Host)
		;	member(host(Host, Port), Properties), HostValue = host(Host, Port)
		),
		!.
	maybe_add_request_host_header(Target, _Properties, Headers, [host-HostValue| Headers]) :-
		target_host_property(Target, HostValue),
		!.
	maybe_add_request_host_header(_Target, _Properties, Headers, Headers).

	maybe_add_cookie_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, cookie),
		!.
	maybe_add_cookie_header(Properties, Headers, [cookie-Pairs| Headers]) :-
		memberchk(cookies(Pairs), Properties),
		!.
	maybe_add_cookie_header(_Properties, Headers, Headers).

	maybe_add_set_cookie_headers(_Properties, Headers, Headers) :-
		header_name_present(Headers, set_cookie),
		!.
	maybe_add_set_cookie_headers(Properties, Headers, EffectiveHeaders) :-
		memberchk(set_cookies(SetCookies), Properties),
		set_cookie_headers(SetCookies, SetCookieHeaders),
		append(SetCookieHeaders, Headers, EffectiveHeaders),
		!.
	maybe_add_set_cookie_headers(_Properties, Headers, Headers).

	set_cookie_headers([], []).
	set_cookie_headers([SetCookie| SetCookies], [set_cookie-SetCookie| Headers]) :-
		set_cookie_headers(SetCookies, Headers).

	maybe_add_connection_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, connection),
		!.
	maybe_add_connection_header(Properties, Headers, [connection-Tokens| Headers]) :-
		member(connection(Tokens), Properties),
		!.
	maybe_add_connection_header(_Properties, Headers, Headers).

	maybe_add_upgrade_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, upgrade),
		!.
	maybe_add_upgrade_header(Properties, Headers, [upgrade-Tokens| Headers]) :-
		member(upgrade(Tokens), Properties),
		!.
	maybe_add_upgrade_header(_Properties, Headers, Headers).

	maybe_add_websocket_key_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, sec_websocket_key),
		!.
	maybe_add_websocket_key_header(Properties, Headers, [sec_websocket_key-Key| Headers]) :-
		member(websocket_key(Key), Properties),
		!.
	maybe_add_websocket_key_header(_Properties, Headers, Headers).

	maybe_add_websocket_version_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, sec_websocket_version),
		!.
	maybe_add_websocket_version_header(Properties, Headers, [sec_websocket_version-Version| Headers]) :-
		memberchk(websocket_version(Version), Properties),
		!.
	maybe_add_websocket_version_header(_Properties, Headers, Headers).

	maybe_add_websocket_accept_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, sec_websocket_accept),
		!.
	maybe_add_websocket_accept_header(Properties, Headers, [sec_websocket_accept-Accept| Headers]) :-
		member(websocket_accept(Accept), Properties),
		!.
	maybe_add_websocket_accept_header(_Properties, Headers, Headers).

	maybe_add_websocket_protocol_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, sec_websocket_protocol),
		!.
	maybe_add_websocket_protocol_header(Properties, Headers, [sec_websocket_protocol-Protocols| Headers]) :-
		member(websocket_protocol(Protocols), Properties),
		!.
	maybe_add_websocket_protocol_header(_Properties, Headers, Headers).

	maybe_add_transfer_encoding_header(_Properties, Headers, Headers) :-
		header_name_present(Headers, transfer_encoding),
		!.
	maybe_add_transfer_encoding_header(Properties, Headers, [transfer_encoding-Tokens| Headers]) :-
		member(transfer_encoding(Tokens), Properties),
		!.
	maybe_add_transfer_encoding_header(_Properties, Headers, Headers).

	maybe_add_content_type_header(empty, _Properties, Headers, Headers) :-
		!.
	maybe_add_content_type_header(_Body, _Properties, Headers, Headers) :-
		header_name_present(Headers, content_type),
		!.
	maybe_add_content_type_header(_Body, Properties, Headers, [content_type-media_type(MediaType, Parameters)| Headers]) :-
		member(content_type(MediaType, Parameters), Properties),
		!.
	maybe_add_content_type_header(content(MediaType, _Payload), _Properties, Headers, [content_type-media_type(MediaType, [])| Headers]).

	maybe_add_content_length_header(_Body, _Properties, Headers, Headers) :-
		header_name_present(Headers, content_length),
		!.
	maybe_add_content_length_header(_Body, _Properties, Headers, Headers) :-
		chunked_transfer_headers(Headers),
		!.
	maybe_add_content_length_header(Body, Properties, Headers, [content_length-Length| Headers]) :-
		body_length_if_known(Headers, Properties, Body, Length),
		!.
	maybe_add_content_length_header(_Body, _Properties, Headers, Headers).

	chunked_transfer_headers(Headers) :-
		semantic_single_header_value(Headers, transfer_encoding, [chunked]).

	header_name_present([Name-_| _], Name) :-
		!.
	header_name_present([_| Headers], Name) :-
		header_name_present(Headers, Name).

	request_body_bytes(Headers, Body, Properties, Bytes) :-
		wire_body_serialization_options(Headers, Properties, Body, Options),
		body_term_to_bytes(Body, Options, Bytes).

	response_body_bytes(Headers, Body, Properties, Bytes) :-
		wire_body_serialization_options(Headers, Properties, Body, Options),
		body_term_to_bytes(Body, Options, Bytes).

	body_serialization_options(_Headers, _Properties, empty, []) :-
		!.
	body_serialization_options(Headers, Properties, content(MediaType, _), Options) :-
		body_options_from_headers_and_properties(Headers, Properties, MediaType, Options).

	wire_body_serialization_options(Headers, Properties, Body, [wire_bytes(true)| Options]) :-
		body_serialization_options(Headers, Properties, Body, Options).

	body_options_from_headers_and_properties(Headers, _Properties, MediaType, Options) :-
		semantic_single_header_value(Headers, content_type, media_type(HeaderMediaType, Parameters)),
		same_media_type(HeaderMediaType, MediaType),
		!,
		body_options_from_content_type(MediaType, Parameters, Options).
	body_options_from_headers_and_properties(_Headers, Properties, MediaType, Options) :-
		memberchk(content_type(PropertyMediaType, Parameters), Properties),
		same_media_type(PropertyMediaType, MediaType),
		!,
		body_options_from_content_type(MediaType, Parameters, Options).
	body_options_from_headers_and_properties(_Headers, _Properties, MediaType, []) :-
		\+ multipart_media_type(MediaType),
		!.
	body_options_from_headers_and_properties(_Headers, _Properties, MediaType, _) :-
		domain_error(http_multipart_boundary, MediaType).

	body_options_from_content_type(MediaType, Parameters, [boundary(Boundary)]) :-
		multipart_media_type(MediaType),
		!,
		(	multipart_boundary_parameter(Parameters, Boundary) ->
			true
		;	domain_error(http_multipart_boundary, Parameters)
		).
	body_options_from_content_type(_MediaType, Parameters, [charset(Charset)]) :-
		member(charset-Charset, Parameters),
		!.
	body_options_from_content_type(_MediaType, _Parameters, []).

	body_bytes_to_term([], _MediaType, _Options, empty) :-
		!.
	body_bytes_to_term(Bytes, MediaType, Options, Body) :-
		multipart_media_type(MediaType),
		!,
		parse_multipart_body(Bytes, MediaType, Options, Body).
	body_bytes_to_term(Bytes, MediaType, Options, Body) :-
		parse_wire_payload(MediaType, Bytes, Options, Payload),
		encode_body(MediaType, Payload, Options, Body).

	body_term_to_bytes(empty, _Options, []) :-
		!.
	body_term_to_bytes(content(_MediaType, file(Path, Offset, Length)), _Options, Bytes) :-
		!,
		file_body_bytes(Path, Offset, Length, Bytes).
	body_term_to_bytes(content(MediaType, multipart(Parts)), Options, Bytes) :-
		multipart_media_type(MediaType),
		!,
		generate_multipart_body(Parts, Options, Bytes).
	body_term_to_bytes(Body, Options, Bytes) :-
		Body = content(MediaType, _Payload),
		decode_body(MediaType, Body, Options, Payload),
		generate_wire_payload(MediaType, Payload, Options, Bytes).

	message_body_from_headers(Headers, Bytes, Body, Properties) :-
		transfer_body_bytes(Headers, Bytes, BodyBytes, Properties),
		payload_body_from_headers(Headers, BodyBytes, Body).

	payload_body_from_headers(_Headers, [], empty) :-
		!.
	payload_body_from_headers(Headers, Bytes, Body) :-
		(	semantic_single_header_value(Headers, content_type, media_type(MediaType, Parameters)) ->
			body_options_from_content_type(MediaType, Parameters, Options),
			parse_body(bytes(Bytes), MediaType, Options, Body)
		;	encode_body('application/octet-stream', Bytes, [], Body)
		).

	transfer_body_bytes(Headers, Bytes, BodyBytes, Properties) :-
		(	semantic_single_header_value(Headers, transfer_encoding, Tokens) ->
			decode_transfer_bytes(Tokens, Bytes, BodyBytes, Properties)
		;	Properties = [],
			(	semantic_single_header_value(Headers, content_length, Length) ->
				exact_length_bytes(Length, Bytes, BodyBytes)
			;	BodyBytes = Bytes
			)
		).

	decode_transfer_bytes([chunked], Bytes, BodyBytes, Properties) :-
		!,
		phrase(chunked_body_bytes(Chunks, Trailers), Bytes),
		append(Chunks, BodyBytes),
		(	Trailers == [] ->
			Properties = []
		;	Properties = [trailers(Trailers)]
		).
	decode_transfer_bytes(Tokens, _Bytes, _BodyBytes, _Properties) :-
		domain_error(http_transfer_encoding, Tokens).

	message_wire_body(Headers, Properties, RawBodyBytes, WireBodyBytes) :-
		(	transfer_encoding_tokens(Headers, Properties, [chunked]) ->
			trailers_property(Properties, Trailers),
			generate_chunked_body(RawBodyBytes, Trailers, WireBodyBytes)
		;	WireBodyBytes = RawBodyBytes
		).

	trailers_property(Properties, Trailers) :-
		(	member(trailers(Trailers), Properties) ->
			true
		;	Trailers = []
		).

	transfer_encoding_tokens(Headers, _Properties, Tokens) :-
		semantic_single_header_value(Headers, transfer_encoding, Tokens),
		!.
	transfer_encoding_tokens(_Headers, Properties, Tokens) :-
		memberchk(transfer_encoding(Tokens), Properties).

	derived_part_properties(Headers, Body, Properties) :-
		properties_from_headers(Headers, HeaderProperties),
		properties_from_body(Body, BodyProperties),
		merge_property_lists([HeaderProperties, BodyProperties], Properties).

	parse_multipart_body(Bytes, MediaType, Options, content(MediaType, multipart(Parts))) :-
		multipart_boundary_option(Options, Boundary),
		multipart_boundary_codes(Boundary, BoundaryCodes),
		parse_multipart_parts(Bytes, BoundaryCodes, Parts).

	generate_multipart_body(Parts, Options, Bytes) :-
		multipart_boundary_option(Options, Boundary),
		multipart_boundary_codes(Boundary, BoundaryCodes),
		multipart_parts_bytes(Parts, BoundaryCodes, Bytes).

	multipart_boundary_option(Options, Boundary) :-
		(	member(boundary(Boundary0), Options) ->
			text_to_atom(Boundary0, Boundary),
			valid_multipart_boundary(Boundary)
		;	domain_error(http_multipart_boundary, Options)
		).

	valid_multipart_boundary(Boundary) :-
		Boundary \== '',
		valid_header_text(Boundary).

	multipart_boundary_parameter([boundary-Boundary| _], BoundaryAtom) :-
		text_to_atom(Boundary, BoundaryAtom),
		valid_multipart_boundary(BoundaryAtom),
		!.
	multipart_boundary_parameter([_| Parameters], Boundary) :-
		multipart_boundary_parameter(Parameters, Boundary).

	multipart_boundary_codes(Boundary, BoundaryCodes) :-
		atom_codes(Boundary, BoundaryCodes).

	parse_multipart_parts(Bytes, BoundaryCodes, []) :-
		multipart_close_marker_codes(BoundaryCodes, CloseMarkerCodes),
		codes_prefix_rest(Bytes, CloseMarkerCodes, _),
		!.
	parse_multipart_parts(Bytes, BoundaryCodes, Parts) :-
		multipart_open_marker_codes(BoundaryCodes, OpenMarkerCodes),
		codes_prefix_rest(Bytes, OpenMarkerCodes, RestBytes),
		parse_multipart_part_sequence(RestBytes, BoundaryCodes, Parts).

	parse_multipart_part_sequence(Bytes, BoundaryCodes, [Part| Parts]) :-
		split_multipart_part(Bytes, BoundaryCodes, PartBytes, RestBytes, Final),
		parse_multipart_part_bytes(PartBytes, Part),
		(	Final == true ->
			Parts = []
		;	parse_multipart_part_sequence(RestBytes, BoundaryCodes, Parts)
		).

	split_multipart_part(Bytes, BoundaryCodes, PartBytes, RestBytes, Final) :-
		split_multipart_part(Bytes, BoundaryCodes, [], PartBytes, RestBytes, Final).

	split_multipart_part(Bytes, BoundaryCodes, Acc, PartBytes, RestBytes, true) :-
		multipart_final_separator_codes(BoundaryCodes, FinalSeparatorCodes),
		codes_prefix_rest(Bytes, FinalSeparatorCodes, RestBytes),
		reverse(Acc, PartBytes),
		!.
	split_multipart_part(Bytes, BoundaryCodes, Acc, PartBytes, RestBytes, true) :-
		multipart_final_separator_no_crlf_codes(BoundaryCodes, FinalSeparatorCodes),
		codes_prefix_rest(Bytes, FinalSeparatorCodes, RestBytes),
		reverse(Acc, PartBytes),
		!.
	split_multipart_part(Bytes, BoundaryCodes, Acc, PartBytes, RestBytes, false) :-
		multipart_continue_separator_codes(BoundaryCodes, ContinueSeparatorCodes),
		codes_prefix_rest(Bytes, ContinueSeparatorCodes, RestBytes),
		reverse(Acc, PartBytes),
		!.
	split_multipart_part([Byte| Bytes], BoundaryCodes, Acc, PartBytes, RestBytes, Final) :-
		split_multipart_part(Bytes, BoundaryCodes, [Byte| Acc], PartBytes, RestBytes, Final).

	parse_multipart_part_bytes(Bytes, part(Headers, Body, Properties)) :-
		phrase(headers_section(Headers), Bytes, RestBytes),
		RestBytes = [0'\r, 0'\n| BodyBytes],
		payload_body_from_headers(Headers, BodyBytes, Body),
		derived_part_properties(Headers, Body, Properties).

	multipart_parts_bytes([], BoundaryCodes, Bytes) :-
		multipart_close_marker_codes(BoundaryCodes, Bytes).
	multipart_parts_bytes([Part| Parts], BoundaryCodes, Bytes) :-
		multipart_part_bytes(Part, BoundaryCodes, PartBytes),
		multipart_parts_bytes(Parts, BoundaryCodes, RestBytes),
		append(PartBytes, RestBytes, Bytes).

	multipart_part_bytes(part(Headers0, Body, Properties), BoundaryCodes, Bytes) :-
		part_effective_headers(Headers0, Body, Properties, Headers),
		wire_body_serialization_options(Headers, Properties, Body, Options),
		body_term_to_bytes(Body, Options, BodyBytes),
		multipart_open_marker_codes(BoundaryCodes, BoundaryBytes),
		headers_codes(Headers, HeaderBytes),
		append(BoundaryBytes, HeaderBytes, Prefix0),
		append(Prefix0, [0'\r, 0'\n], Prefix1),
		append(Prefix1, BodyBytes, Prefix2),
		append(Prefix2, [0'\r, 0'\n], Bytes).

	part_effective_headers(Headers0, Body, Properties, Headers) :-
		maybe_add_content_type_header(Body, Properties, Headers0, Headers).

	multipart_open_marker_codes(BoundaryCodes, Codes) :-
		append([[0'-, 0'-], BoundaryCodes, [0'\r, 0'\n]], Codes).

	multipart_close_marker_codes(BoundaryCodes, Codes) :-
		append([[0'-, 0'-], BoundaryCodes, [0'-, 0'-, 0'\r, 0'\n]], Codes).

	multipart_continue_separator_codes(BoundaryCodes, Codes) :-
		append([[0'\r, 0'\n, 0'-, 0'-], BoundaryCodes, [0'\r, 0'\n]], Codes).

	multipart_final_separator_codes(BoundaryCodes, Codes) :-
		append([[0'\r, 0'\n, 0'-, 0'-], BoundaryCodes, [0'-, 0'-, 0'\r, 0'\n]], Codes).

	multipart_final_separator_no_crlf_codes(BoundaryCodes, Codes) :-
		append([[0'\r, 0'\n, 0'-, 0'-], BoundaryCodes, [0'-, 0'-]], Codes).

	generate_chunked_body(BodyBytes, Trailers, Bytes) :-
		chunk_bytes_block(BodyBytes, ChunkBytes),
		headers_codes(Trailers, TrailerBytes),
		append([ChunkBytes, [0'0, 0'\r, 0'\n], TrailerBytes, [0'\r, 0'\n]], Bytes).

	chunk_bytes_block([], []).
	chunk_bytes_block(Bytes, ChunkBytes) :-
		length(Bytes, Length),
		hex_number_codes(Length, LengthCodes),
		append([LengthCodes, [0'\r, 0'\n], Bytes, [0'\r, 0'\n]], ChunkBytes).

	chunked_body_bytes([Chunk| Chunks], Trailers) -->
		chunk_header(Size),
		{Size > 0},
		chunk_data(Size, Chunk),
		crlf,
		!,
		chunked_body_bytes(Chunks, Trailers).
	chunked_body_bytes([], Trailers) -->
		chunk_header(0),
		headers_section(Trailers),
		crlf.

	chunk_header(Size) -->
		optional_whitespace,
		hex_token(SizeCodes),
		optional_whitespace,
		chunk_extensions,
		crlf,
		{hex_codes_number(SizeCodes, Size)}.

	hex_token([Code| Codes]) -->
		[Code],
		{hex_digit_code(Code)},
		hex_token_tail(Codes).

	hex_token_tail([Code| Codes]) -->
		[Code],
		{hex_digit_code(Code)},
		!,
		hex_token_tail(Codes).
	hex_token_tail([]) -->
		[].

	chunk_extensions -->
		[0';],
		chunk_extension_text,
		!,
		chunk_extensions.
	chunk_extensions -->
		[].

	chunk_extension_text -->
		[Code],
		{Code =\= 0'\r, Code =\= 0'\n},
		!,
		chunk_extension_text.
	chunk_extension_text -->
		[].

	chunk_data(0, []) -->
		[].
	chunk_data(Size, [Byte| Bytes]) -->
		[Byte],
		{Size > 0, NextSize is Size - 1},
		chunk_data(NextSize, Bytes).

	parse_wire_payload(MediaType, Bytes, Options, Payload) :-
		(	json_media_type(MediaType) ->
			payload_text_codes(MediaType, Bytes, Options, Codes),
			json::parse(codes(Codes), Payload)
		;	form_media_type(MediaType) ->
			parse_www_form_codes(Bytes, Payload)
		;	text_media_type(MediaType) ->
			payload_text_codes(MediaType, Bytes, Options, Codes),
			atom_codes(Payload, Codes)
		;	octet_stream_media_type(MediaType) ->
			Payload = Bytes
		;	existence_error(http_body_codec, MediaType)
		).

	payload_text_codes(MediaType, Bytes, Options, Codes) :-
		(	payload_charset(MediaType, Options, CharsetObject) ->
			CharsetObject::bytes_to_codes(Bytes, Codes)
		;	Codes = Bytes
		).

	payload_charset(_MediaType, Options, CharsetObject) :-
		memberchk(wire_bytes(true), Options),
		memberchk(charset(Charset), Options),
		!,
		charset_object(Charset, CharsetObject).
	payload_charset(MediaType, Options, utf_8) :-
		memberchk(wire_bytes(true), Options),
		json_media_type(MediaType).

	charset_object(Charset, CharsetObject) :-
		normalize_atom_text(Charset, NormalizedCharset),
		atom_codes(NormalizedCharset, CharsetCodes),
		charset_object_name_codes(CharsetCodes, CharsetObjectCodes),
		atom_codes(CharsetObject, CharsetObjectCodes),
		catch(CharsetObject::preferred_mime_name(_), _, fail).

	charset_object_name_codes([], []).
	charset_object_name_codes([0'-| CharsetCodes], [0'_| CharsetObjectCodes]) :-
		!,
		charset_object_name_codes(CharsetCodes, CharsetObjectCodes).
	charset_object_name_codes([Code| CharsetCodes], [Code| CharsetObjectCodes]) :-
		charset_object_name_codes(CharsetCodes, CharsetObjectCodes).

	generate_wire_payload(MediaType, Payload, Options, Bytes) :-
		(	json_media_type(MediaType) ->
			json::generate(codes(Codes), Payload),
			payload_codes_to_bytes(MediaType, Codes, Options, Bytes)
		;	form_media_type(MediaType) ->
			parse_form_payload(Payload, Pairs),
			generate_www_form_codes(Pairs, Bytes)
		;	text_media_type(MediaType) ->
			text_to_codes(Payload, Codes),
			payload_codes_to_bytes(MediaType, Codes, Options, Bytes)
		;	octet_stream_media_type(MediaType) ->
			( Payload = binary(RawBytes) -> Bytes = RawBytes ; Bytes = Payload )
		;	existence_error(http_body_codec, MediaType)
		).

	payload_codes_to_bytes(MediaType, Codes, Options, Bytes) :-
		(	payload_charset(MediaType, Options, CharsetObject) ->
			CharsetObject::codes_to_bytes(Codes, Bytes)
		;	Bytes = Codes
		).

	body_codec(MediaType, http_json_body_codec) :-
		json_media_type(MediaType),
		!.
	body_codec(MediaType, http_form_body_codec) :-
		form_media_type(MediaType),
		!.
	body_codec(MediaType, http_text_body_codec) :-
		text_media_type(MediaType),
		!.
	body_codec(MediaType, http_octet_stream_body_codec) :-
		octet_stream_media_type(MediaType).

	json_media_type(MediaType) :-
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		(	NormalizedMediaType == 'application/json'
		;	NormalizedMediaType == 'text/json'
		;	atom_codes(NormalizedMediaType, Codes), codes_suffix(Codes, [0'+, 0'j, 0's, 0'o, 0'n])
		).

	form_media_type(MediaType) :-
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		NormalizedMediaType == 'application/x-www-form-urlencoded'.

	text_media_type(MediaType) :-
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		atom_codes(NormalizedMediaType, Codes),
		codes_prefix(Codes, [0't, 0'e, 0'x, 0't, 0'/]).

	octet_stream_media_type(MediaType) :-
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		NormalizedMediaType == 'application/octet-stream'.

	multipart_media_type(MediaType) :-
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		atom_codes(NormalizedMediaType, Codes),
		codes_prefix(Codes, [0'm, 0'u, 0'l, 0't, 0'i, 0'p, 0'a, 0'r, 0't, 0'/]).

	request_head(Method, Target, Version, Headers) -->
		request_line_bytes(request_line(Method, Target, Version)),
		crlf,
		headers_section(Headers),
		crlf.

	response_head(Version, Status, Headers) -->
		status_line_bytes(status_line(Version, Status)),
		crlf,
		headers_section(Headers),
		crlf.

	public_request_line(RequestLine) -->
		request_line_bytes(RequestLine),
		optional_line_ending.

	public_status_line(StatusLine) -->
		status_line_bytes(StatusLine),
		optional_line_ending.

	public_headers(Headers) -->
		headers_section(Headers),
		optional_line_ending.

	request_line_bytes(request_line(Method, Target, Version)) -->
		method_token(MethodCodes),
		[0' ],
		request_target_token(TargetCodes),
		[0' ],
		http_version_token(Version),
		{
			normalize_method_codes(MethodCodes, Method),
			parse_request_target_codes(TargetCodes, Target)
		}.

	status_line_bytes(status_line(Version, Status)) -->
		http_version_token(Version),
		[0' ],
		status_code_token(Code),
		(	[0' ] ->
			reason_phrase_token(ReasonPhraseCodes),
			{atom_codes(ReasonPhrase, ReasonPhraseCodes)}
		;	{ReasonPhrase = ''}
		),
		{Status = status(Code, ReasonPhrase)}.

	http_version_token(http(Major, Minor)) -->
		[0'H, 0'T, 0'T, 0'P, 0'/],
		digits(MajorCodes),
		[0'.],
		digits(MinorCodes),
		{number_codes(Major, MajorCodes), number_codes(Minor, MinorCodes)}.

	method_token([Code| Codes]) -->
		[Code],
		{method_token_code(Code)},
		method_token_tail(Codes).

	method_token_tail([Code| Codes]) -->
		[Code],
		{method_token_code(Code)},
		!,
		method_token_tail(Codes).
	method_token_tail([]) -->
		[].

	request_target_token([Code| Codes]) -->
		[Code],
		{Code =\= 0' , Code =\= 0'\r, Code =\= 0'\n},
		request_target_token_tail(Codes).

	request_target_token_tail([Code| Codes]) -->
		[Code],
		{Code =\= 0' , Code =\= 0'\r, Code =\= 0'\n},
		!,
		request_target_token_tail(Codes).
	request_target_token_tail([]) -->
		[].

	status_code_token(Code) -->
		digit(Code1),
		digit(Code2),
		digit(Code3),
		{number_codes(Code, [Code1, Code2, Code3])}.

	reason_phrase_token([Code| Codes]) -->
		[Code],
		{Code =\= 0'\r, Code =\= 0'\n},
		!,
		reason_phrase_token(Codes).
	reason_phrase_token([]) -->
		[].

	headers_section([Header| Headers]) -->
		header_line(Header),
		crlf,
		!,
		headers_section(Headers).
	headers_section([]) -->
		[].

	header_line(Name-Value) -->
		header_name_token(NameCodes),
		[0':],
		optional_whitespace,
		header_value_token(ValueCodes0),
		{
			trim_trailing_ows_codes(ValueCodes0, ValueCodes),
			normalize_header_name_codes(NameCodes, Name),
			parse_header_value(Name, ValueCodes, Value)
		}.

	header_name_token([Code| Codes]) -->
		[Code],
		{header_name_code(Code)},
		header_name_token_tail(Codes).

	header_name_token_tail([Code| Codes]) -->
		[Code],
		{header_name_code(Code)},
		!,
		header_name_token_tail(Codes).
	header_name_token_tail([]) -->
		[].

	header_value_token([Code| Codes]) -->
		[Code],
		{Code =\= 0'\r, Code =\= 0'\n},
		!,
		header_value_token(Codes).
	header_value_token([]) -->
		[].

	optional_whitespace -->
		[Code],
		{ows_code(Code)},
		!,
		optional_whitespace.
	optional_whitespace -->
		[].

	optional_line_ending -->
		crlf,
		!.
	optional_line_ending -->
		[0'\n],
		!.
	optional_line_ending -->
		[].

	crlf -->
		[0'\r, 0'\n].

	request_line_codes(RequestLine, Codes) :-
		phrase(request_line_output(RequestLine), Codes).

	status_line_codes(StatusLine, Codes) :-
		phrase(status_line_output(StatusLine), Codes).

	headers_codes(Headers, Codes) :-
		headers_codes_lists(Headers, Lists),
		append(Lists, Codes).

	headers_codes_lists([], []).
	headers_codes_lists([Header| Headers], [Codes| Lists]) :-
		header_codes(Header, Codes),
		headers_codes_lists(Headers, Lists).

	header_codes(Name-Value, Codes) :-
		header_name_wire_codes(Name, NameCodes),
		header_value_codes(Name, Value, ValueCodes),
		append(NameCodes, [0':, 0' ], Prefix),
		append(Prefix, ValueCodes, Prefix0),
		append(Prefix0, [0'\r, 0'\n], Codes).

	request_line_output(request_line(Method, Target, Version)) -->
		{
			method_wire_codes(Method, MethodCodes),
			request_target_output_codes(Target, TargetCodes),
			version_wire_codes(Version, VersionCodes)
		},
		codes(MethodCodes),
		[0' ],
		codes(TargetCodes),
		[0' ],
		codes(VersionCodes),
		crlf.

	status_line_output(status_line(Version, status(Code, ReasonPhrase))) -->
		{
			version_wire_codes(Version, VersionCodes),
			number_codes(Code, StatusCodes),
			reason_phrase_codes(ReasonPhrase, ReasonPhraseCodes)
		},
		codes(VersionCodes),
		[0' ],
		codes(StatusCodes),
		(	{ReasonPhraseCodes == []} ->
			[]
		;	[0' ], codes(ReasonPhraseCodes)
		),
		crlf.

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

	method_wire_codes(Method, Codes) :-
		validate_method(Method),
		atom_codes(Method, LowerCodes),
		uppercase_ascii_codes(LowerCodes, Codes).

	version_wire_codes(http(Major, Minor), Codes) :-
		validate_version(http(Major, Minor)),
		number_codes(Major, MajorCodes),
		number_codes(Minor, MinorCodes),
		append([ [0'H, 0'T, 0'T, 0'P, 0'/], MajorCodes, [0'.], MinorCodes ], Codes).

	reason_phrase_codes(ReasonPhrase, Codes) :-
		(	ReasonPhrase == '' ->
			Codes = []
		;	valid_header_text(ReasonPhrase),
			text_to_codes(ReasonPhrase, Codes)
		).

	request_target_output_codes(asterisk, [0'*]).
	request_target_output_codes(origin(Path), Codes) :-
		atom_codes(Path, Codes).
	request_target_output_codes(origin(Path, Query), Codes) :-
		atom_codes(Path, PathCodes),
		text_to_codes(Query, QueryCodes),
		append([PathCodes, [0'?], QueryCodes], Codes).
	request_target_output_codes(absolute(Components), Codes) :-
		url(atom)::generate(Components, URL),
		atom_codes(URL, Codes).
	request_target_output_codes(authority(Host), Codes) :-
		host_value_codes(host(Host), Codes).
	request_target_output_codes(authority(Host, Port), Codes) :-
		host_value_codes(host(Host, Port), Codes).

	parse_request_target_codes([0'*], asterisk) :-
		!.
	parse_request_target_codes(Codes, absolute(Components)) :-
		codes_contain(Codes, [0':, 0'/, 0'/]),
		!,
		atom_codes(URL, Codes),
		url(atom)::parse(URL, Components).
	parse_request_target_codes(Codes, Target) :-
		Codes = [0'/| _],
		!,
		(	split_once(0'?, Codes, PathCodes, QueryCodes) ->
			atom_codes(Path, PathCodes),
			atom_codes(Query, QueryCodes),
			Target = origin(Path, Query)
		;	atom_codes(Path, Codes),
			Target = origin(Path)
		).
	parse_request_target_codes(Codes, authority(Host, Port)) :-
		parse_host_value_codes(Codes, host(Host, Port)),
		!.
	parse_request_target_codes(Codes, authority(Host)) :-
		parse_host_value_codes(Codes, host(Host)).

	parse_header_value(content_length, ValueCodes, Value) :-
		normalize_header_semantics(content_length, ValueCodes, Value),
		!.
	parse_header_value(content_type, ValueCodes, Value) :-
		normalize_header_semantics(content_type, ValueCodes, Value),
		!.
	parse_header_value(cookie, ValueCodes, Value) :-
		normalize_header_semantics(cookie, ValueCodes, Value),
		!.
	parse_header_value(set_cookie, ValueCodes, Value) :-
		normalize_header_semantics(set_cookie, ValueCodes, Value),
		!.
	parse_header_value(host, ValueCodes, Value) :-
		normalize_header_semantics(host, ValueCodes, Value),
		!.
	parse_header_value(connection, ValueCodes, Value) :-
		normalize_header_semantics(connection, ValueCodes, Value),
		!.
	parse_header_value(upgrade, ValueCodes, Value) :-
		normalize_header_semantics(upgrade, ValueCodes, Value),
		!.
	parse_header_value(sec_websocket_key, ValueCodes, Value) :-
		normalize_header_semantics(sec_websocket_key, ValueCodes, Value),
		!.
	parse_header_value(sec_websocket_version, ValueCodes, Value) :-
		normalize_header_semantics(sec_websocket_version, ValueCodes, Value),
		!.
	parse_header_value(sec_websocket_accept, ValueCodes, Value) :-
		normalize_header_semantics(sec_websocket_accept, ValueCodes, Value),
		!.
	parse_header_value(sec_websocket_protocol, ValueCodes, Value) :-
		normalize_header_semantics(sec_websocket_protocol, ValueCodes, Value),
		!.
	parse_header_value(sec_websocket_extensions, ValueCodes, Value) :-
		(	normalize_header_semantics(sec_websocket_extensions, ValueCodes, Value) ->
			true
		;	atom_codes(RawValue, ValueCodes),
			domain_error(http_header_value(sec_websocket_extensions), RawValue)
		),
		!.
	parse_header_value(transfer_encoding, ValueCodes, Value) :-
		normalize_header_semantics(transfer_encoding, ValueCodes, Value),
		!.
	parse_header_value(_, ValueCodes, Value) :-
		atom_codes(Value, ValueCodes).

	header_value_codes(content_length, Value, Codes) :-
		normalize_header_semantics(content_length, Value, Length),
		number_codes(Length, Codes),
		!.
	header_value_codes(content_type, Value, Codes) :-
		normalize_header_semantics(content_type, Value, MediaTypeProperty),
		media_type_property_codes(MediaTypeProperty, Codes),
		!.
	header_value_codes(cookie, Value, Codes) :-
		normalize_header_semantics(cookie, Value, Pairs),
		http_cookies(atom)::generate_cookie(Pairs, Cookie),
		atom_codes(Cookie, Codes),
		!.
	header_value_codes(set_cookie, Value, Codes) :-
		normalize_header_semantics(set_cookie, Value, set_cookie(Name, CookieValue, Attributes)),
		http_cookies(atom)::generate_set_cookie(Name, CookieValue, Attributes, SetCookie),
		atom_codes(SetCookie, Codes),
		!.
	header_value_codes(host, Value, Codes) :-
		normalize_header_semantics(host, Value, HostValue),
		host_value_codes(HostValue, Codes),
		!.
	header_value_codes(connection, Value, Codes) :-
		normalize_header_semantics(connection, Value, Tokens),
		token_list_codes(Tokens, Codes),
		!.
	header_value_codes(upgrade, Value, Codes) :-
		normalize_header_semantics(upgrade, Value, Tokens),
		token_list_codes(Tokens, Codes),
		!.
	header_value_codes(sec_websocket_key, Value, Codes) :-
		normalize_header_semantics(sec_websocket_key, Value, Key),
		atom_codes(Key, Codes),
		!.
	header_value_codes(sec_websocket_version, Value, Codes) :-
		(	Value = [_| _] ->
			normalize_websocket_versions(Value, Versions),
			websocket_versions_codes(Versions, Codes)
		;	normalize_header_semantics(sec_websocket_version, Value, Version),
			websocket_versions_codes(Version, Codes)
		),
		!.
	header_value_codes(sec_websocket_accept, Value, Codes) :-
		normalize_header_semantics(sec_websocket_accept, Value, Accept),
		atom_codes(Accept, Codes),
		!.
	header_value_codes(sec_websocket_protocol, Value, Codes) :-
		normalize_header_semantics(sec_websocket_protocol, Value, Protocols),
		token_list_codes(Protocols, Codes),
		!.
	header_value_codes(sec_websocket_extensions, Value, Codes) :-
		normalize_header_semantics(sec_websocket_extensions, Value, Extensions),
		atom_codes(Extensions, Codes),
		!.
	header_value_codes(transfer_encoding, Value, Codes) :-
		normalize_header_semantics(transfer_encoding, Value, Tokens),
		token_list_codes(Tokens, Codes),
		!.
	header_value_codes(_, Value, Codes) :-
		valid_header_text(Value),
		text_to_codes(Value, Codes).

	header_name_wire_codes(Name, Codes) :-
		atom_codes(Name, NameCodes),
		header_name_codes_wire_codes(NameCodes, Codes).

	header_name_codes_wire_codes([], []).
	header_name_codes_wire_codes([0'_| NameCodes], [0'-| Codes]) :-
		!,
		header_name_codes_wire_codes(NameCodes, Codes).
	header_name_codes_wire_codes([Code| NameCodes], [Code| Codes]) :-
		header_name_codes_wire_codes(NameCodes, Codes).

	normalize_header_name_codes(NameCodes, Name) :-
		lowercase_header_name_codes(NameCodes, NormalizedCodes),
		atom_codes(Name, NormalizedCodes).

	lowercase_header_name_codes([], []).
	lowercase_header_name_codes([0'-| Codes], [0'_| NormalizedCodes]) :-
		!,
		lowercase_header_name_codes(Codes, NormalizedCodes).
	lowercase_header_name_codes([Code| Codes], [NormalizedCode| NormalizedCodes]) :-
		normalize_ascii_code(Code, NormalizedCode),
		lowercase_header_name_codes(Codes, NormalizedCodes).

	normalize_header_semantics(content_length, Value, Length) :-
		(	integer(Value) ->
			Length = Value
		;	text_to_codes(Value, Codes),
			trim_ows_codes(Codes, TrimmedCodes),
			number_codes(Length, TrimmedCodes)
		),
		Length >= 0.
	normalize_header_semantics(content_type, media_type(MediaType, Parameters), media_type(NormalizedMediaType, Parameters)) :-
		validate_media_type(MediaType),
		validate_parameter_pairs(Parameters),
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		!.
	normalize_header_semantics(content_type, Value, media_type(MediaType, Parameters)) :-
		text_to_codes(Value, Codes),
		parse_media_type_codes(Codes, media_type(MediaType, Parameters)).
	normalize_header_semantics(cookie, Pairs, Pairs) :-
		valid_text_pairs(Pairs),
		!.
	normalize_header_semantics(cookie, Value, Pairs) :-
		text_to_atom(Value, Cookie),
		http_cookies(atom)::parse_cookie(Cookie, Pairs).
	normalize_header_semantics(set_cookie, set_cookie(Name, CookieValue, Attributes), set_cookie(Name, CookieValue, Attributes)) :-
		valid_set_cookie_term(set_cookie(Name, CookieValue, Attributes)),
		!.
	normalize_header_semantics(set_cookie, Value, set_cookie(Name, CookieValue, Attributes)) :-
		text_to_atom(Value, SetCookie),
		http_cookies(atom)::parse_set_cookie(SetCookie, Name, CookieValue, Attributes).
	normalize_header_semantics(host, host(Host), host(NormalizedHost)) :-
		valid_host(Host),
		normalize_atom_text(Host, NormalizedHost),
		!.
	normalize_header_semantics(host, host(Host, Port), host(NormalizedHost, Port)) :-
		valid_host(Host),
		valid_port(Port),
		normalize_atom_text(Host, NormalizedHost),
		!.
	normalize_header_semantics(host, Value, HostValue) :-
		text_to_codes(Value, Codes),
		parse_host_value_codes(Codes, HostValue).
	normalize_header_semantics(connection, Tokens, NormalizedTokens) :-
		valid_token_list(Tokens),
		!,
		normalize_token_list(Tokens, NormalizedTokens).
	normalize_header_semantics(connection, Value, Tokens) :-
		text_to_codes(Value, Codes),
		parse_token_list_codes(Codes, Tokens).
	normalize_header_semantics(upgrade, Tokens, NormalizedTokens) :-
		valid_token_list(Tokens),
		!,
		normalize_token_list(Tokens, NormalizedTokens).
	normalize_header_semantics(upgrade, Value, Tokens) :-
		text_to_codes(Value, Codes),
		parse_token_list_codes(Codes, Tokens).
	normalize_header_semantics(sec_websocket_key, Value, Key) :-
		normalize_websocket_key(Value, Key).
	normalize_header_semantics(sec_websocket_version, Value, Version) :-
		normalize_websocket_version(Value, Version).
	normalize_header_semantics(sec_websocket_accept, Value, Accept) :-
		normalize_websocket_accept(Value, Accept).
	normalize_header_semantics(sec_websocket_protocol, Tokens, NormalizedTokens) :-
		valid_unique_token_list(Tokens),
		!,
		normalize_token_list(Tokens, NormalizedTokens).
	normalize_header_semantics(sec_websocket_protocol, Value, Tokens) :-
		text_to_codes(Value, Codes),
		parse_unique_token_list_codes(Codes, Tokens).
	normalize_header_semantics(sec_websocket_extensions, Value, Extensions) :-
		normalize_websocket_extensions(Value, Extensions).
	normalize_header_semantics(transfer_encoding, Tokens, NormalizedTokens) :-
		valid_token_list(Tokens),
		!,
		normalize_token_list(Tokens, NormalizedTokens).
	normalize_header_semantics(transfer_encoding, Value, Tokens) :-
		text_to_codes(Value, Codes),
		parse_token_list_codes(Codes, Tokens).

	semantic_single_header_value(Headers, Name, Value) :-
		header_values(Headers, Name, Values),
		single_header_value(Name, Values, Value).

	semantic_websocket_version_header_value(Headers, Version) :-
		websocket_header_values(Headers, Values0),
		Values0 \== [],
		flatten_websocket_versions(Values0, Versions),
		(	Versions = [SingleVersion] ->
			Version = SingleVersion
		;	Version = Versions
		).

	websocket_header_values([], []).
	websocket_header_values([sec_websocket_version-Value| Headers], [Value| Values]) :-
		!,
		websocket_header_values(Headers, Values).
	websocket_header_values([_Header| Headers], Values) :-
		websocket_header_values(Headers, Values).

	header_values(Headers, Name, Values) :-
		header_values(Headers, Name, [], ReversedValues),
		reverse(ReversedValues, Values).

	header_values([], _Name, Values, Values).
	header_values([HeaderName-RawValue| Headers], Name, Acc, Values) :-
		(	HeaderName == Name, normalize_semantic_header_value(Name, RawValue, Value) ->
			header_values(Headers, Name, [Value| Acc], Values)
		;	header_values(Headers, Name, Acc, Values)
		).

	normalize_semantic_header_value(Name, RawValue, Value) :-
		(	member(Name, [content_length, content_type, cookie, set_cookie, host, connection, upgrade, sec_websocket_key, sec_websocket_version, sec_websocket_accept, sec_websocket_protocol, sec_websocket_extensions, transfer_encoding]) ->
			normalize_header_semantics(Name, RawValue, Value)
		;	Value = RawValue
		).

	normalize_websocket_key(Value, Key) :-
		normalize_websocket_base64_value(Value, 16, Key).

	normalize_websocket_accept(Value, Accept) :-
		normalize_websocket_base64_value(Value, 20, Accept).

	normalize_websocket_extensions(Value, Extensions) :-
		text_to_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		Codes \== [],
		parse_websocket_extensions_codes(Codes),
		atom_codes(Extensions, Codes).

	normalize_websocket_base64_value(Value, Size, NormalizedValue) :-
		text_to_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		Codes \== [],
		catch(parse(codes(Codes), Bytes), _, fail),
		length(Bytes, Size),
		generate(atom(NormalizedValue), Bytes).

	normalize_websocket_version(Version, Version) :-
		integer(Version),
		Version >= 0,
		!.
	normalize_websocket_version(Value, Version) :-
		text_to_codes(Value, Codes0),
		trim_ows_codes(Codes0, Codes),
		parse_websocket_version_codes(Codes, Versions),
		(	Versions = [SingleVersion] ->
			Version = SingleVersion
		;	Version = Versions
		).

	single_header_value(_Name, [Value], Value) :-
		!.
	single_header_value(_Name, [Value| Values], Value) :-
		all_same_header_values(Values, Value),
		!.
	single_header_value(Name, [_| _], _) :-
		domain_error(http_header_semantics, duplicate(Name)).

	all_same_header_values([], _).
	all_same_header_values([Value| Values], Value) :-
		all_same_header_values(Values, Value).

	flatten_websocket_versions([], []).
	flatten_websocket_versions([[Version| Versions0]| Values0], Versions) :-
		!,
		flatten_websocket_versions(Values0, Versions1),
		append([Version| Versions0], Versions1, Versions).
	flatten_websocket_versions([Version| Values0], [Version| Versions]) :-
		flatten_websocket_versions(Values0, Versions).

	target_host_property(absolute(Components), HostProperty) :-
		memberchk(authority(Authority), Components),
		authority_host_property(Authority, HostProperty).
	target_host_property(authority(Host), host(NormalizedHost)) :-
		normalize_atom_text(Host, NormalizedHost).
	target_host_property(authority(Host, Port), host(NormalizedHost, Port)) :-
		normalize_atom_text(Host, NormalizedHost).

	authority_host_property(Authority, HostProperty) :-
		text_to_codes(Authority, Codes),
		parse_host_value_codes(Codes, HostProperty).

	path_segments_property(Path, path_segments(Segments)) :-
		atom_codes(Path, PathCodes0),
		(	PathCodes0 = [0'/| PathCodes] ->
			true
		;	PathCodes = PathCodes0
		),
		split_codes(0'/, PathCodes, SegmentCodeLists),
		segment_code_lists_atoms(SegmentCodeLists, Segments).

	query_pairs_property(Query, query_pairs(Pairs)) :-
		text_to_codes(Query, QueryCodes),
		parse_www_form_codes(QueryCodes, Pairs).

	body_decoded_state(empty, false) :-
		!.
	body_decoded_state(content(_, binary(_)), false) :-
		!.
	body_decoded_state(content(_, file(_, _, _)), false) :-
		!.
	body_decoded_state(content(_, text(_)), true) :-
		!.
	body_decoded_state(content(_, json(_)), true) :-
		!.
	body_decoded_state(content(_, form(_)), true) :-
		!.
	body_decoded_state(content(_, multipart(_)), true).

	body_length_if_known(empty, 0) :-
		!.
	body_length_if_known(content(_, file(_, _, Length)), Length) :-
		!.
	body_length_if_known(Body, Length) :-
		catch(body_term_to_bytes(Body, [], Bytes), _, fail),
		length(Bytes, Length).

	body_length_if_known(Headers, Properties, Body, Length) :-
		catch(
			(	wire_body_serialization_options(Headers, Properties, Body, Options),
				body_term_to_bytes(Body, Options, Bytes),
				length(Bytes, Length)
			),
			_,
			fail
		).
	body_length_if_known(_Headers, _Properties, Body, Length) :-
		body_length_if_known(Body, Length).

	file_body_bytes(Path, Offset, Length, Bytes) :-
		open(Path, read, Stream, [type(binary)]),
		catch(
			(	skip_stream_bytes(Stream, Offset, file(Path, Offset, Length)),
				read_stream_bytes(Stream, Length, Bytes, file(Path, Offset, Length))
			),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream).

	skip_stream_bytes(_Stream, 0, _FileBody) :-
		!.
	skip_stream_bytes(Stream, Remaining, FileBody) :-
		get_byte(Stream, Byte),
		(	Byte =:= -1 ->
			domain_error(http_body_file_range, FileBody)
		;	NextRemaining is Remaining - 1,
			skip_stream_bytes(Stream, NextRemaining, FileBody)
		).

	read_stream_bytes(Stream, Length, Bytes, FileBody) :-
		read_stream_bytes(Stream, Length, Bytes, [], FileBody).

	read_stream_bytes(_Stream, 0, Bytes, Bytes, _FileBody) :-
		!.
	read_stream_bytes(Stream, Remaining, [Byte| Bytes0], Bytes, FileBody) :-
		get_byte(Stream, Byte),
		(	Byte =:= -1 ->
			domain_error(http_body_file_range, FileBody)
		;	NextRemaining is Remaining - 1,
			read_stream_bytes(Stream, NextRemaining, Bytes0, Bytes, FileBody)
		).

	exact_length_bytes(Length, Bytes, ExactBytes) :-
		prefix_bytes(Length, Bytes, ExactBytes, Rest),
		Rest == [].

	prefix_bytes(0, Bytes, [], Bytes) :-
		!.
	prefix_bytes(Length, [Byte| Bytes], [Byte| Prefix], Rest) :-
		Length > 0,
		NextLength is Length - 1,
		prefix_bytes(NextLength, Bytes, Prefix, Rest).

	source_to_codes(Source, Codes) :-
		(	var(Source) ->
			instantiation_error
		;	Source = file(File) ->
			file_to_codes(File, Codes)
		;	Source = stream(Stream) ->
			stream_to_codes(Stream, Codes)
		;	Source = codes(Codes) ->
			true
		;	Source = chars(Chars) ->
			chars_to_codes(Chars, Codes)
		;	Source = atom(Atom) ->
			atom_codes(Atom, Codes)
		;	domain_error(http_source, Source)
		).

	source_to_bytes(Source, Bytes) :-
		(	var(Source) ->
			instantiation_error
		;	Source = file(File) ->
			file_to_bytes(File, Bytes)
		;	Source = stream(Stream) ->
			stream_to_bytes(Stream, Bytes)
		;	Source = bytes(Bytes) ->
			true
		;	Source = codes(Bytes) ->
			true
		;	Source = chars(Chars) ->
			chars_to_codes(Chars, Bytes)
		;	Source = atom(Atom) ->
			atom_codes(Atom, Bytes)
		;	domain_error(http_source, Source)
		).

	codes_to_sink(Sink, Codes) :-
		(	var(Sink) ->
			instantiation_error
		;	Sink = file(File) ->
			open(File, write, Stream),
			write_codes(Codes, Stream),
			close(Stream)
		;	Sink = stream(Stream) ->
			write_codes(Codes, Stream)
		;	Sink = codes(Codes) ->
			true
		;	Sink = chars(Chars) ->
			codes_to_chars(Codes, Chars)
		;	Sink = atom(Atom) ->
			atom_codes(Atom, Codes)
		;	domain_error(http_sink, Sink)
		).

	bytes_to_sink(Sink, Bytes) :-
		(	var(Sink) ->
			instantiation_error
		;	Sink = file(File) ->
			open(File, write, Stream, [type(binary)]),
			write_bytes(Bytes, Stream),
			close(Stream)
		;	Sink = stream(Stream) ->
			write_bytes(Bytes, Stream)
		;	Sink = bytes(Bytes) ->
			true
		;	Sink = codes(Bytes) ->
			true
		;	Sink = chars(Chars) ->
			codes_to_chars(Bytes, Chars)
		;	Sink = atom(Atom) ->
			atom_codes(Atom, Bytes)
		;	domain_error(http_sink, Sink)
		).

	write_codes([], _Stream).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

	write_bytes([], _Stream).
	write_bytes([Byte| Bytes], Stream) :-
		put_byte(Stream, Byte),
		write_bytes(Bytes, Stream).

	validate_method(Method) :-
		(	valid_method(Method) ->
			true
		;	domain_error(http_method, Method)
		).

	valid_method(get).
	valid_method(post).
	valid_method(put).
	valid_method(delete).
	valid_method(head).
	valid_method(options).
	valid_method(patch).
	valid_method(trace).
	valid_method(connect).

	validate_target(Target) :-
		(	valid_target(Target) ->
			true
		;	domain_error(http_target, Target)
		).

	valid_target(asterisk).
	valid_target(origin(Path)) :-
		valid_origin_path(Path).
	valid_target(origin(Path, Query)) :-
		valid_origin_path(Path),
		valid_text(Query).
	valid_target(absolute(URLComponents)) :-
		valid_absolute_url_components(URLComponents).
	valid_target(authority(Host)) :-
		valid_host(Host).
	valid_target(authority(Host, Port)) :-
		valid_host(Host),
		valid_port(Port).

	valid_origin_path(Path) :-
		atom(Path),
		atom_codes(Path, [0'/| _]).

	valid_absolute_url_components(URLComponents) :-
		catch(url(atom)::generate(URLComponents, URL), _, fail),
		catch(url(atom)::valid(URL), _, fail).

	validate_version(Version) :-
		(	valid_version(Version) ->
			true
		;	domain_error(http_version, Version)
		).

	valid_version(http(Major, Minor)) :-
		integer(Major),
		Major >= 0,
		integer(Minor),
		Minor >= 0.

	validate_status(Status) :-
		(	valid_status(Status) ->
			true
		;	domain_error(http_status, Status)
		).

	valid_status(status(Code, ReasonPhrase)) :-
		integer(Code),
		Code >= 100,
		Code =< 599,
		valid_text(ReasonPhrase).

	validate_headers(Headers) :-
		(	var(Headers) ->
			domain_error(http_headers, Headers)
		;	validate_header_list(Headers)
		).

	validate_header_list(Headers) :-
		(	Headers == [] ->
			true
		;	Headers = [Header| Rest] ->
			validate_header(Header),
			validate_header_list(Rest)
		;	domain_error(http_headers, Headers)
		).

	validate_header(Name-Value) :-
		!,
		(	valid_header_name(Name) ->
			validate_header_value(Name, Value)
		;	domain_error(http_header_name, Name)
		).
	validate_header(Header) :-
		domain_error(http_header, Header).

	valid_header_name(Name) :-
		atom(Name),
		atom_codes(Name, [Code| Codes]),
		normalized_header_name_code(Code),
		valid_header_name_codes(Codes).

	valid_header_name_codes([]).
	valid_header_name_codes([Code| Codes]) :-
		normalized_header_name_code(Code),
		valid_header_name_codes(Codes).

	normalized_header_name_code(Code) :-
		lowercase_alpha(Code),
		!.
	normalized_header_name_code(Code) :-
		digit_code(Code),
		!.
	normalized_header_name_code(0'_).
	normalized_header_name_code(0'!).
	normalized_header_name_code(0'#).
	normalized_header_name_code(0'$).
	normalized_header_name_code(0'%).
	normalized_header_name_code(0'&).
	normalized_header_name_code(39).
	normalized_header_name_code(0'*).
	normalized_header_name_code(0'+).
	normalized_header_name_code(0'.).
	normalized_header_name_code(0'^).
	normalized_header_name_code(96).
	normalized_header_name_code(0'|).
	normalized_header_name_code(0'~).

	validate_header_value(content_length, Value) :-
		!,
		(	normalize_header_semantics(content_length, Value, _) ->
			true
		;	domain_error(http_header_value(content_length), Value)
		).
	validate_header_value(content_type, Value) :-
		!,
		(	normalize_header_semantics(content_type, Value, _) ->
			true
		;	domain_error(http_header_value(content_type), Value)
		).
	validate_header_value(cookie, Value) :-
		!,
		(	normalize_header_semantics(cookie, Value, _) ->
			true
		;	domain_error(http_header_value(cookie), Value)
		).
	validate_header_value(set_cookie, Value) :-
		!,
		(	normalize_header_semantics(set_cookie, Value, _) ->
			true
		;	domain_error(http_header_value(set_cookie), Value)
		).
	validate_header_value(host, Value) :-
		!,
		(	normalize_header_semantics(host, Value, _) ->
			true
		;	domain_error(http_header_value(host), Value)
		).
	validate_header_value(connection, Value) :-
		!,
		(	normalize_header_semantics(connection, Value, _) ->
			true
		;	domain_error(http_header_value(connection), Value)
		).
	validate_header_value(upgrade, Value) :-
		!,
		(	normalize_header_semantics(upgrade, Value, _) ->
			true
		;	domain_error(http_header_value(upgrade), Value)
		).
	validate_header_value(sec_websocket_key, Value) :-
		!,
		(	normalize_header_semantics(sec_websocket_key, Value, _) ->
			true
		;	domain_error(http_header_value(sec_websocket_key), Value)
		).
	validate_header_value(sec_websocket_version, Value) :-
		!,
		(	( 	Value = [_| _] ->
				normalize_websocket_versions(Value, _)
			;	normalize_header_semantics(sec_websocket_version, Value, _)
			) ->
			true
		;	domain_error(http_header_value(sec_websocket_version), Value)
		).
	validate_header_value(sec_websocket_accept, Value) :-
		!,
		(	normalize_header_semantics(sec_websocket_accept, Value, _) ->
			true
		;	domain_error(http_header_value(sec_websocket_accept), Value)
		).
	validate_header_value(sec_websocket_protocol, Value) :-
		!,
		(	normalize_header_semantics(sec_websocket_protocol, Value, _) ->
			true
		;	domain_error(http_header_value(sec_websocket_protocol), Value)
		).
	validate_header_value(sec_websocket_extensions, Value) :-
		!,
		(	normalize_header_semantics(sec_websocket_extensions, Value, _) ->
			true
		;	domain_error(http_header_value(sec_websocket_extensions), Value)
		).
	validate_header_value(transfer_encoding, Value) :-
		!,
		(	normalize_header_semantics(transfer_encoding, Value, _) ->
			true
		;	domain_error(http_header_value(transfer_encoding), Value)
		).
	validate_header_value(_, Value) :-
		(	valid_header_text(Value) ->
			true
		;	domain_error(http_header_value, Value)
		).

	validate_body(Body) :-
		(	valid_body(Body) ->
			true
		;	domain_error(http_body, Body)
		).

	valid_body(empty).
	valid_body(content(MediaType, Payload)) :-
		valid_media_type(MediaType),
		valid_payload(Payload).

	validate_media_type(MediaType) :-
		(	valid_media_type(MediaType) ->
			true
		;	domain_error(http_media_type, MediaType)
		).

	valid_media_type(MediaType) :-
		atom(MediaType),
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		atom_codes(NormalizedMediaType, Codes),
		valid_media_type_codes(Codes).

	valid_media_type_codes(Codes) :-
		split_once(0'/, Codes, TypeCodes, SubtypeCodes),
		TypeCodes \== [],
		SubtypeCodes \== [],
		valid_token_codes(TypeCodes),
		valid_token_codes(SubtypeCodes).

	valid_payload(binary(Bytes)) :-
		valid_byte_list(Bytes).
	valid_payload(file(Path, Offset, Length)) :-
		atom(Path),
		integer(Offset),
		Offset >= 0,
		integer(Length),
		Length >= 0.
	valid_payload(text(Text)) :-
		valid_text(Text).
	valid_payload(json(_)).
	valid_payload(form(Pairs)) :-
		valid_text_pairs(Pairs).
	valid_payload(multipart(Parts)) :-
		valid_multipart_parts(Parts).

	valid_multipart_parts([]).
	valid_multipart_parts([Part| Parts]) :-
		valid_multipart_part(Part),
		valid_multipart_parts(Parts).

	valid_multipart_part(part(Headers, Body, Properties)) :-
		catch(validate_headers(Headers), _, fail),
		catch(validate_body(Body), _, fail),
		catch(validate_properties(Properties), _, fail).

	validate_properties(Properties) :-
		(	var(Properties) ->
			domain_error(http_properties, Properties)
		;	validate_property_list(Properties)
		).

	validate_property_list(Properties) :-
		(	Properties == [] ->
			true
		;	Properties = [Property| Rest] ->
			validate_property(Property),
			validate_property_list(Rest)
		;	domain_error(http_properties, Properties)
		).

	validate_property(Property) :-
		(	valid_property(Property) ->
			true
		;	domain_error(http_property, Property)
		).

	valid_property(Property) :-
		nonvar(Property),
		functor(Property, Functor, _),
		valid_property_by_functor(Functor, Property).

	valid_property_by_functor(content_type, content_type(MediaType, Parameters)) :-
		valid_media_type(MediaType),
		validate_parameter_pairs(Parameters).
	valid_property_by_functor(content_length, content_length(Length)) :-
		integer(Length),
		Length >= 0.
	valid_property_by_functor(host, host(Host)) :-
		valid_host(Host).
	valid_property_by_functor(host, host(Host, Port)) :-
		valid_host(Host),
		valid_port(Port).
	valid_property_by_functor(cookies, cookies(Pairs)) :-
		valid_text_pairs(Pairs).
	valid_property_by_functor(set_cookies, set_cookies(SetCookies)) :-
		valid_set_cookie_values(SetCookies).
	valid_property_by_functor(query_pairs, query_pairs(Pairs)) :-
		valid_named_pairs(Pairs).
	valid_property_by_functor(path_segments, path_segments(Segments)) :-
		valid_atom_list(Segments).
	valid_property_by_functor(path_params, path_params(Pairs)) :-
		valid_named_pairs(Pairs).
	valid_property_by_functor(trailers, trailers(Trailers)) :-
		catch(validate_headers(Trailers), _, fail).
	valid_property_by_functor(scheme, scheme(Scheme)) :-
		normalize_atom_text(Scheme, NormalizedScheme),
		memberchk(NormalizedScheme, [http, https]).
	valid_property_by_functor(peer, peer(Address, Port)) :-
		valid_host(Address),
		valid_port(Port).
	valid_property_by_functor(decoded_body, decoded_body(Boolean)) :-
		valid_boolean(Boolean).
	valid_property_by_functor(open_api_probe, open_api_probe(Boolean)) :-
		valid_boolean(Boolean).
	valid_property_by_functor(connection, connection(Tokens)) :-
		valid_token_list(Tokens).
	valid_property_by_functor(upgrade, upgrade(Tokens)) :-
		valid_token_list(Tokens).
	valid_property_by_functor(websocket_key, websocket_key(Key)) :-
		valid_websocket_key(Key).
	valid_property_by_functor(websocket_version, websocket_version(Version)) :-
		(	Version = [_| _] ->
			normalize_websocket_versions(Version, _)
		;	normalize_websocket_version(Version, _)
		).
	valid_property_by_functor(websocket_accept, websocket_accept(Accept)) :-
		valid_websocket_accept(Accept).
	valid_property_by_functor(websocket_protocol, websocket_protocol(Protocols)) :-
		valid_unique_token_list(Protocols).
	valid_property_by_functor(transfer_encoding, transfer_encoding(Tokens)) :-
		valid_token_list(Tokens).
	valid_property_by_functor(Functor, Property) :-
		\+ recognized_property_functor(Functor),
		compound(Property).

	recognized_property_functor(content_type).
	recognized_property_functor(content_length).
	recognized_property_functor(host).
	recognized_property_functor(cookies).
	recognized_property_functor(set_cookies).
	recognized_property_functor(query_pairs).
	recognized_property_functor(path_segments).
	recognized_property_functor(path_params).
	recognized_property_functor(trailers).
	recognized_property_functor(scheme).
	recognized_property_functor(peer).
	recognized_property_functor(decoded_body).
	recognized_property_functor(connection).
	recognized_property_functor(upgrade).
	recognized_property_functor(websocket_key).
	recognized_property_functor(websocket_version).
	recognized_property_functor(websocket_accept).
	recognized_property_functor(websocket_protocol).
	recognized_property_functor(transfer_encoding).

	validate_body_options(Options) :-
		(	var(Options) ->
			domain_error(http_body_options, Options)
		;	valid_options_list(Options)
		).

	valid_options_list([]).
	valid_options_list([_Option| Options]) :-
		valid_options_list(Options).

	validate_parameter_pairs(Parameters) :-
		(	valid_parameter_pairs(Parameters) ->
			true
		;	domain_error(http_parameters, Parameters)
		).

	valid_set_cookie_values([]).
	valid_set_cookie_values([SetCookie| SetCookies]) :-
		valid_set_cookie_term(SetCookie),
		valid_set_cookie_values(SetCookies).

	valid_set_cookie_term(set_cookie(Name, Value, Attributes)) :-
		catch(http_cookies(atom)::generate_set_cookie(Name, Value, Attributes, _), _, fail).

	valid_host(Host) :-
		atom(Host),
		Host \== ''.

	valid_port(Port) :-
		integer(Port),
		Port >= 1,
		Port =< 65535.

	valid_boolean(true).
	valid_boolean(false).

	valid_parameter_pairs([]).
	valid_parameter_pairs([Name-Value| Parameters]) :-
		atom(Name),
		valid_text(Value),
		valid_parameter_pairs(Parameters).

	valid_text_pairs([]).
	valid_text_pairs([Name-Value| Pairs]) :-
		atom(Name),
		valid_text(Value),
		valid_text_pairs(Pairs).

	valid_named_pairs([]).
	valid_named_pairs([Name-Value| Pairs]) :-
		atom(Name),
		nonvar(Value),
		valid_named_pairs(Pairs).

	valid_atom_list([]).
	valid_atom_list([Atom| Atoms]) :-
		atom(Atom),
		valid_atom_list(Atoms).

	valid_token_list([]).
	valid_token_list([Token| Tokens]) :-
		atom(Token),
		atom_codes(Token, Codes),
		valid_token_codes(Codes),
		valid_token_list(Tokens).

	valid_unique_token_list(Tokens) :-
		valid_token_list(Tokens),
		normalize_token_list(Tokens, NormalizedTokens),
		unique_atom_list(NormalizedTokens).

	unique_atom_list([]).
	unique_atom_list([Token| Tokens]) :-
		\+ member(Token, Tokens),
		unique_atom_list(Tokens).

	valid_token_codes([]) :-
		fail.
	valid_token_codes([Code| Codes]) :-
		token_code(Code),
		valid_token_codes_tail(Codes).

	valid_token_codes_tail([]).
	valid_token_codes_tail([Code| Codes]) :-
		token_code(Code),
		valid_token_codes_tail(Codes).

	valid_byte_list([]).
	valid_byte_list([Byte| Bytes]) :-
		integer(Byte),
		Byte >= 0,
		Byte =< 255,
		valid_byte_list(Bytes).

	valid_text(Text) :-
		atom(Text),
		!.
	valid_text(Text) :-
		valid_character_list(Text),
		!.
	valid_text(Text) :-
		valid_code_list(Text).

	valid_header_text(Text) :-
		valid_text(Text),
		text_to_codes(Text, Codes),
		codes_without_line_breaks(Codes).

	valid_websocket_key(Key) :-
		valid_websocket_base64_atom(Key, 16).

	valid_websocket_accept(Accept) :-
		valid_websocket_base64_atom(Accept, 20).

	valid_websocket_base64_atom(Value, Size) :-
		atom(Value),
		atom_codes(Value, Codes),
		catch(parse(codes(Codes), Bytes), _, fail),
		length(Bytes, Size),
		catch(generate(atom(Value), Bytes), _, fail).

	valid_digit_codes([Code| Codes]) :-
		digit_code(Code),
		valid_digit_codes_tail(Codes).

	valid_digit_codes_tail([]).
	valid_digit_codes_tail([Code| Codes]) :-
		digit_code(Code),
		valid_digit_codes_tail(Codes).

	valid_character_list([]).
	valid_character_list([Character| Characters]) :-
		atom(Character),
		atom_length(Character, 1),
		valid_character_list(Characters).

	valid_code_list([]).
	valid_code_list([Code| Codes]) :-
		integer(Code),
		Code >= 0,
		Code =< 1114111,
		valid_code_list(Codes).

	codes_without_line_breaks([]).
	codes_without_line_breaks([Code| Codes]) :-
		Code =\= 0'\r,
		Code =\= 0'\n,
		codes_without_line_breaks(Codes).

	text_to_atom(Text, Atom) :-
		text_to_codes(Text, Codes),
		atom_codes(Atom, Codes).

	text_to_codes(Text, Codes) :-
		(	atom(Text) ->
			atom_codes(Text, Codes)
		;	valid_character_list(Text) ->
			chars_to_codes(Text, Codes)
		;	Codes = Text
		).

	normalize_atom_text(Text, NormalizedText) :-
		text_to_codes(Text, Codes),
		lowercase_ascii_codes(Codes, NormalizedCodes),
		atom_codes(NormalizedText, NormalizedCodes).

	normalize_media_type_atom(MediaType, NormalizedMediaType) :-
		normalize_atom_text(MediaType, NormalizedMediaType).

	same_media_type(MediaType1, MediaType2) :-
		normalize_media_type_atom(MediaType1, NormalizedMediaType1),
		normalize_media_type_atom(MediaType2, NormalizedMediaType2),
		NormalizedMediaType1 == NormalizedMediaType2.

	media_type_property_codes(media_type(MediaType, Parameters), Codes) :-
		normalize_media_type_atom(MediaType, NormalizedMediaType),
		atom_codes(NormalizedMediaType, MediaTypeCodes),
		parameter_pairs_codes(Parameters, ParameterCodes),
		append([MediaTypeCodes| ParameterCodes], Codes).

	parameter_pairs_codes([], []).
	parameter_pairs_codes([Name-Value| Parameters], [[0';, 0' ]| Codes]) :-
		normalize_atom_text(Name, NormalizedName),
		atom_codes(NormalizedName, NameCodes),
		parameter_value_codes(Value, ValueCodes),
		append(NameCodes, [0'=| ValueCodes], PairCodes),
		Codes = [PairCodes| RestCodes],
		parameter_pairs_codes(Parameters, RestCodes).

	parameter_value_codes(Value, ValueCodes) :-
		text_to_codes(Value, Codes),
		(	valid_token_codes(Codes) ->
			ValueCodes = Codes
		;	append([[0'"], Codes, [0'"]], ValueCodes)
		).

	parse_media_type_codes(Codes, media_type(MediaType, Parameters)) :-
		trim_ows_codes(Codes, TrimmedCodes),
		split_codes(0';, TrimmedCodes, [MediaTypeCodes| ParameterSegments]),
		media_type_codes_atom(MediaTypeCodes, MediaType),
		parse_media_type_parameters(ParameterSegments, Parameters).

	media_type_codes_atom(MediaTypeCodes0, MediaType) :-
		trim_ows_codes(MediaTypeCodes0, MediaTypeCodes),
		MediaTypeCodes \== [],
		lowercase_ascii_codes(MediaTypeCodes, NormalizedCodes),
		valid_media_type_codes(NormalizedCodes),
		atom_codes(MediaType, NormalizedCodes).

	parse_media_type_parameters([], []).
	parse_media_type_parameters([Segment| Segments], [Name-Value| Parameters]) :-
		trim_ows_codes(Segment, TrimmedSegment),
		split_once(0'=, TrimmedSegment, NameCodes0, ValueCodes0),
		trim_ows_codes(NameCodes0, NameCodes),
		trim_ows_codes(ValueCodes0, ValueCodes1),
		NameCodes \== [],
		lowercase_ascii_codes(NameCodes, NormalizedNameCodes),
		atom_codes(Name, NormalizedNameCodes),
		unquote_codes(ValueCodes1, ValueCodes),
		atom_codes(Value, ValueCodes),
		parse_media_type_parameters(Segments, Parameters).

	unquote_codes([0'"| Codes0], Codes) :-
		append(Codes, [0'"], Codes0),
		!.
	unquote_codes(Codes, Codes).

	parse_form_payload(form(Pairs), Pairs) :-
		!.
	parse_form_payload(Pairs, Pairs).

	parse_www_form_codes(Codes, Pairs) :-
		split_codes(0'&, Codes, PairSegments),
		pair_segments_pairs(PairSegments, Pairs).

	pair_segments_pairs([], []).
	pair_segments_pairs([Segment| Segments], [Name-Value| Pairs]) :-
		parse_pair_segment(Segment, Name, Value),
		pair_segments_pairs(Segments, Pairs).

	parse_pair_segment(Segment, Name, Value) :-
		(	split_once(0'=, Segment, NameCodes0, ValueCodes0) ->
			true
		;	NameCodes0 = Segment,
			ValueCodes0 = []
		),
		www_form_decode_codes(NameCodes0, NameCodes),
		www_form_decode_codes(ValueCodes0, ValueCodes),
		atom_codes(Name, NameCodes),
		atom_codes(Value, ValueCodes).

	generate_www_form_codes([], []).
	generate_www_form_codes([Name-Value| Pairs], Codes) :-
		www_form_encode_text(Name, NameCodes),
		www_form_encode_text(Value, ValueCodes),
		(	Pairs == [] ->
			append([NameCodes, [0'=], ValueCodes], Codes)
		;	generate_www_form_codes(Pairs, RestCodes),
			append([NameCodes, [0'=], ValueCodes, [0'&], RestCodes], Codes)
		).

	www_form_encode_text(Text, Codes) :-
		text_to_codes(Text, RawCodes),
		www_form_encode_codes(RawCodes, Codes).

	www_form_encode_codes([], []).
	www_form_encode_codes([0' | RawCodes], [0'+| Codes]) :-
		!,
		www_form_encode_codes(RawCodes, Codes).
	www_form_encode_codes([Code| RawCodes], [Code| Codes]) :-
		www_form_safe_code(Code),
		!,
		www_form_encode_codes(RawCodes, Codes).
	www_form_encode_codes([Code| RawCodes], [0'%, High, Low| Codes]) :-
		hex_pair(Code, High, Low),
		www_form_encode_codes(RawCodes, Codes).

	www_form_decode_codes([], []).
	www_form_decode_codes([0'+| Codes], [0' | DecodedCodes]) :-
		!,
		www_form_decode_codes(Codes, DecodedCodes).
	www_form_decode_codes([0'%, High, Low| Codes], [Code| DecodedCodes]) :-
		!,
		hex_value(High, HighValue),
		hex_value(Low, LowValue),
		Code is HighValue * 16 + LowValue,
		www_form_decode_codes(Codes, DecodedCodes).
	www_form_decode_codes([Code| Codes], [Code| DecodedCodes]) :-
		www_form_decode_codes(Codes, DecodedCodes).

	www_form_safe_code(Code) :-
		lowercase_alpha(Code).
	www_form_safe_code(Code) :-
		uppercase_alpha(Code).
	www_form_safe_code(Code) :-
		digit_code(Code).
	www_form_safe_code(Code) :-
		memberchk(Code, [0'-, 0'_, 0'., 0'*]).

	segment_code_lists_atoms([], []).
	segment_code_lists_atoms([[]| Segments], Atoms) :-
		!,
		segment_code_lists_atoms(Segments, Atoms).
	segment_code_lists_atoms([SegmentCodes0| Segments], [Segment| Atoms]) :-
		percent_decode_codes(SegmentCodes0, DecodedCodes),
		atom_codes(Segment, DecodedCodes),
		segment_code_lists_atoms(Segments, Atoms).

	percent_decode_codes([], []).
	percent_decode_codes([0'%, High, Low| Codes], [Code| DecodedCodes]) :-
		!,
		hex_value(High, HighValue),
		hex_value(Low, LowValue),
		Code is HighValue * 16 + LowValue,
		percent_decode_codes(Codes, DecodedCodes).
	percent_decode_codes([Code| Codes], [Code| DecodedCodes]) :-
		percent_decode_codes(Codes, DecodedCodes).

	parse_host_value_codes(Codes0, HostValue) :-
		strip_userinfo_codes(Codes0, Codes),
		Codes = [0'[| _],
		!,
		parse_bracketed_host_codes(Codes, HostValue).
	parse_host_value_codes(Codes0, host(Host, Port)) :-
		strip_userinfo_codes(Codes0, Codes),
		colon_count(Codes, 1),
		split_last_colon(Codes, HostCodes0, PortCodes),
		PortCodes \== [],
		number_codes(Port, PortCodes),
		lowercase_ascii_codes(HostCodes0, HostCodes),
		atom_codes(Host, HostCodes),
		valid_host(Host),
		valid_port(Port),
		!.
	parse_host_value_codes(Codes0, host(Host)) :-
		strip_userinfo_codes(Codes0, Codes),
		lowercase_ascii_codes(Codes, HostCodes),
		atom_codes(Host, HostCodes),
		valid_host(Host).

	parse_bracketed_host_codes([0'[| Codes], host(Host)) :-
		split_once(0'], Codes, HostCodes0, RestCodes),
		RestCodes == [],
		lowercase_ascii_codes(HostCodes0, HostCodes),
		atom_codes(Host, HostCodes),
		valid_host(Host),
		!.
	parse_bracketed_host_codes([0'[| Codes], host(Host, Port)) :-
		split_once(0'], Codes, HostCodes0, [0':| PortCodes]),
		number_codes(Port, PortCodes),
		valid_port(Port),
		lowercase_ascii_codes(HostCodes0, HostCodes),
		atom_codes(Host, HostCodes),
		valid_host(Host).

	host_value_codes(host(Host), Codes) :-
		atom_codes(Host, Codes).
	host_value_codes(host(Host, Port), Codes) :-
		atom_codes(Host, HostCodes0),
		(	member(0':, HostCodes0), HostCodes0 = [0'[| _] ->
			HostCodes = HostCodes0
		;	member(0':, HostCodes0) ->
			append([0'[| HostCodes0], [0']], HostCodes)
		;	HostCodes = HostCodes0
		),
		number_codes(Port, PortCodes),
		append([HostCodes, [0':], PortCodes], Codes).

	parse_token_list_codes(Codes0, Tokens) :-
		trim_ows_codes(Codes0, Codes),
		split_codes(0',, Codes, TokenCodeLists),
		token_code_lists_atoms(TokenCodeLists, Tokens).

	parse_unique_token_list_codes(Codes0, Tokens) :-
		parse_token_list_codes(Codes0, Tokens),
		valid_unique_token_list(Tokens).

	parse_websocket_extensions_codes(Codes0) :-
		trim_ows_codes(Codes0, Codes),
		Codes \== [],
		phrase(websocket_extensions, Codes).

	parse_websocket_version_codes(Codes0, Versions) :-
		trim_ows_codes(Codes0, Codes),
		split_codes(0',, Codes, VersionCodeLists),
		websocket_version_code_lists(VersionCodeLists, Versions).

	websocket_extensions -->
		websocket_extension,
		websocket_extensions_tail.

	websocket_extensions_tail -->
		optional_whitespace,
		[0',],
		optional_whitespace,
		!,
		websocket_extension,
		websocket_extensions_tail.
	websocket_extensions_tail -->
		optional_whitespace,
		[].

	websocket_extension -->
		http_token,
		websocket_extension_parameters.

	websocket_extension_parameters -->
		optional_whitespace,
		[0';],
		optional_whitespace,
		!,
		http_token,
		websocket_extension_parameter_value,
		websocket_extension_parameters.
	websocket_extension_parameters -->
		[].

	websocket_extension_parameter_value -->
		optional_whitespace,
		[0'=],
		optional_whitespace,
		!,
		websocket_extension_parameter_data.
	websocket_extension_parameter_value -->
		[].

	websocket_extension_parameter_data -->
		http_token,
		!.
	websocket_extension_parameter_data -->
		quoted_http_text.

	http_token -->
		[Code],
		{token_code(Code)},
		http_token_tail.

	http_token_tail -->
		[Code],
		{token_code(Code)},
		!,
		http_token_tail.
	http_token_tail -->
		[].

	quoted_http_text -->
		[0'"],
		quoted_http_text_codes,
		[0'"].

	quoted_http_text_codes -->
		[0'\\, Code],
		{Code =\= 0'\r, Code =\= 0'\n},
		!,
		quoted_http_text_codes.
	quoted_http_text_codes -->
		[Code],
		{Code =\= 0'", Code =\= 0'\\, Code =\= 0'\r, Code =\= 0'\n},
		!,
		quoted_http_text_codes.
	quoted_http_text_codes -->
		[].

	token_code_lists_atoms([], []).
	token_code_lists_atoms([TokenCodes0| TokenCodeLists], [Token| Tokens]) :-
		trim_ows_codes(TokenCodes0, TokenCodes),
		TokenCodes \== [],
		lowercase_ascii_codes(TokenCodes, NormalizedTokenCodes),
		valid_token_codes(NormalizedTokenCodes),
		atom_codes(Token, NormalizedTokenCodes),
		token_code_lists_atoms(TokenCodeLists, Tokens).

	websocket_version_code_lists([], []).
	websocket_version_code_lists([VersionCodes0| VersionCodeLists], [Version| Versions]) :-
		trim_ows_codes(VersionCodes0, VersionCodes),
		VersionCodes \== [],
		valid_digit_codes(VersionCodes),
		number_codes(Version, VersionCodes),
		websocket_version_code_lists(VersionCodeLists, Versions).

	normalize_websocket_versions([], []).
	normalize_websocket_versions([Version0| Versions0], [Version| Versions]) :-
		normalize_websocket_version(Version0, Version),
		integer(Version),
		normalize_websocket_versions(Versions0, Versions).

	normalize_token_list([], []).
	normalize_token_list([Token| Tokens], [NormalizedToken| NormalizedTokens]) :-
		normalize_atom_text(Token, NormalizedToken),
		normalize_token_list(Tokens, NormalizedTokens).

	token_list_codes([], []).
	token_list_codes([Token], Codes) :-
		normalize_atom_text(Token, NormalizedToken),
		atom_codes(NormalizedToken, Codes).
	token_list_codes([Token| Tokens], Codes) :-
		normalize_atom_text(Token, NormalizedToken),
		atom_codes(NormalizedToken, TokenCodes),
		token_list_codes(Tokens, RestCodes),
		append([TokenCodes, [0',, 0' ], RestCodes], Codes).

	websocket_versions_codes(Version, Codes) :-
		integer(Version),
		!,
		number_codes(Version, Codes).
	websocket_versions_codes([Version], Codes) :-
		!,
		number_codes(Version, Codes).
	websocket_versions_codes([Version| Versions], Codes) :-
		number_codes(Version, VersionCodes),
		websocket_versions_codes(Versions, RestCodes),
		append([VersionCodes, [0',, 0' ], RestCodes], Codes).

	method_token_code(Code) :-
		token_code(Code).

	header_name_code(Code) :-
		normalized_header_name_code(Code).
	header_name_code(Code) :-
		uppercase_alpha(Code).
	header_name_code(0'-).

	token_code(Code) :-
		lowercase_alpha(Code).
	token_code(Code) :-
		uppercase_alpha(Code).
	token_code(Code) :-
		digit_code(Code).
	token_code(Code) :-
		memberchk(Code, [0'!, 0'#, 0'$, 0'%, 0'&, 0'*, 0'+, 0'-, 0'., 0'^, 0'_, 0'`, 0'|, 0'~]).

	lowercase_alpha(Code) :-
		Code >= 0'a,
		Code =< 0'z.

	uppercase_alpha(Code) :-
		Code >= 0'A,
		Code =< 0'Z.

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	digit(Code) -->
		[Code],
		{digit_code(Code)}.

	digits([Code| Codes]) -->
		digit(Code),
		digits_tail(Codes).

	digits_tail([Code| Codes]) -->
		digit(Code),
		!,
		digits_tail(Codes).
	digits_tail([]) -->
		[].

	normalize_method_codes(Codes, Method) :-
		lowercase_ascii_codes(Codes, LowerCodes),
		atom_codes(Method, LowerCodes),
		validate_method(Method).

	normalize_ascii_code(Code, NormalizedCode) :-
		(	uppercase_alpha(Code) ->
			NormalizedCode is Code + 32
		;	NormalizedCode = Code
		).

	ows_code(Code) :-
		^^ows_code(Code).

	lowercase_ascii_codes(Codes, LowerCodes) :-
		^^lowercase_ascii_codes(Codes, LowerCodes).

	uppercase_ascii_codes(Codes, UpperCodes) :-
		^^uppercase_ascii_codes(Codes, UpperCodes).

	trim_trailing_ows_codes(Codes, TrimmedCodes) :-
		^^trim_trailing_ows_codes(Codes, TrimmedCodes).

	trim_ows_codes(Codes, TrimmedCodes) :-
		^^trim_ows_codes(Codes, TrimmedCodes).

	strip_userinfo_codes(Codes, StrippedCodes) :-
		(	last_separator_suffix(0'@, Codes, Suffix) ->
			StrippedCodes = Suffix
		;	StrippedCodes = Codes
		).

	last_separator_suffix(Separator, Codes, Suffix) :-
		last_separator_suffix(Separator, Codes, none, Suffix),
		Suffix \== none.

	last_separator_suffix(_Separator, [], Suffix, Suffix).
	last_separator_suffix(Separator, [Separator| Codes], _CurrentSuffix, Suffix) :-
		!,
		last_separator_suffix(Separator, Codes, Codes, Suffix).
	last_separator_suffix(Separator, [_| Codes], CurrentSuffix, Suffix) :-
		last_separator_suffix(Separator, Codes, CurrentSuffix, Suffix).

	colon_count([], 0).
	colon_count([0':| Codes], Count) :-
		!,
		colon_count(Codes, NextCount),
		Count is NextCount + 1.
	colon_count([_| Codes], Count) :-
		colon_count(Codes, Count).

	split_last_colon(Codes, Left, Right) :-
		reverse(Codes, ReversedCodes),
		split_once(0':, ReversedCodes, ReversedRight, ReversedLeft),
		reverse(ReversedLeft, Left),
		reverse(ReversedRight, Right).

	split_once(Separator, Codes, Left, Right) :-
		split_once(Separator, Codes, [], Left, Right).

	split_once(_Separator, [], _Acc, _Left, _Right) :-
		fail.
	split_once(Separator, [Separator| Codes], Acc, Left, Codes) :-
		reverse(Acc, Left),
		!.
	split_once(Separator, [Code| Codes], Acc, Left, Right) :-
		split_once(Separator, Codes, [Code| Acc], Left, Right).

	split_codes(Separator, Codes, Segments) :-
		split_codes(Separator, Codes, [], Segments).

	split_codes(_Separator, [], CurrentCodes, [Segment]) :-
		reverse(CurrentCodes, Segment),
		!.
	split_codes(Separator, [Separator| Codes], CurrentCodes, [Segment| Segments]) :-
		!,
		reverse(CurrentCodes, Segment),
		split_codes(Separator, Codes, [], Segments).
	split_codes(Separator, [Code| Codes], CurrentCodes, Segments) :-
		split_codes(Separator, Codes, [Code| CurrentCodes], Segments).

	codes_prefix(Codes, Prefix) :-
		append(Prefix, _, Codes).

	codes_suffix(Codes, Suffix) :-
		append(_, Suffix, Codes).

	codes_contain(Codes, SubCodes) :-
		append(_, RestCodes, Codes),
		append(SubCodes, _, RestCodes).

	codes_prefix_rest(Codes, Prefix, Rest) :-
		append(Prefix, Rest, Codes).

	hex_digit_code(Code) :-
		hex_value(Code, _).

	hex_codes_number(Codes, Number) :-
		hex_codes_number(Codes, 0, Number).

	hex_codes_number([], Number, Number).
	hex_codes_number([Code| Codes], Acc, Number) :-
		hex_value(Code, Value),
		NextAcc is Acc * 16 + Value,
		hex_codes_number(Codes, NextAcc, Number).

	hex_number_codes(Number, Codes) :-
		Number >= 0,
		hex_number_codes(Number, [], Codes).

	hex_number_codes(Number, Acc, Codes) :-
		Digit is Number mod 16,
		hex_digit(Digit, Code),
		Quotient is Number // 16,
		(	Quotient =:= 0 ->
			Codes = [Code| Acc]
		;	hex_number_codes(Quotient, [Code| Acc], Codes)
		).

	hex_pair(Code, High, Low) :-
		HighValue is Code // 16,
		LowValue is Code mod 16,
		hex_digit(HighValue, High),
		hex_digit(LowValue, Low).

	hex_digit(Value, Code) :-
		(	Value < 10 ->
			Code is 0'0 + Value
		;	Code is 0'A + Value - 10
		).

	hex_value(Code, Value) :-
		Code >= 0'0,
		Code =< 0'9,
		!,
		Value is Code - 0'0.
	hex_value(Code, Value) :-
		Code >= 0'A,
		Code =< 0'F,
		!,
		Value is Code - 0'A + 10.
	hex_value(Code, Value) :-
		Code >= 0'a,
		Code =< 0'f,
		Value is Code - 0'a + 10.

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.
