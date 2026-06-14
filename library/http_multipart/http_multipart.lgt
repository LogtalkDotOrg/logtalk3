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


:- object(http_multipart,
	imports(http_text_helpers)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-08,
		comment is 'Multipart helper predicates built on top of the normalized body and part terms provided by the http library.'
	]).

	:- public(body/3).
	:- mode(body(+atom, +list(compound), -compound), one_or_error).
	:- info(body/3, [
		comment is 'Constructs a validated normalized multipart body term from a multipart media type atom and a list of multipart parts.',
		argnames is ['MediaType', 'Parts', 'Body']
	]).

	:- public(is_body/1).
	:- mode(is_body(@term), zero_or_one).
	:- info(is_body/1, [
		comment is 'True when the argument is a valid normalized multipart body term.',
		argnames is ['Body']
	]).

	:- public(parse/4).
	:- mode(parse(++compound, +atom, +list, -compound), one_or_error).

	:- info(parse/4, [
		comment is 'Parses a multipart body from a source term by delegating to the underlying ``http_core::parse_body/4`` predicate after validating the media type.',
		argnames is ['Source', 'MediaType', 'Options', 'Body']
	]).

	:- public(generate/3).
	:- mode(generate(++compound, +compound, +list), one_or_error).

	:- info(generate/3, [
		comment is 'Generates a multipart body to a sink term by delegating to the underlying ``http_core::generate_body/3`` predicate after validating the multipart body term.',
		argnames is ['Sink', 'Body', 'Options']
	]).

	:- public(media_type/2).
	:- mode(media_type(+compound, -atom), one_or_error).
	:- info(media_type/2, [
		comment is 'Returns the media type atom of a validated multipart body term.',
		argnames is ['Body', 'MediaType']
	]).

	:- public(parts/2).
	:- mode(parts(+compound, -list(compound)), one_or_error).
	:- info(parts/2, [
		comment is 'Returns the list of multipart part terms of a validated multipart body term.',
		argnames is ['Body', 'Parts']
	]).

	:- public(fields/2).
	:- mode(fields(+compound, -list(compound)), one_or_error).
	:- info(fields/2, [
		comment is 'Returns the list of form-data field(Name, Value, Parameters) descriptors found in a validated multipart/form-data body, preserving part order.',
		argnames is ['Body', 'Fields']
	]).

	:- public(files/2).
	:- mode(files(+compound, -list(compound)), one_or_error).
	:- info(files/2, [
		comment is 'Returns the list of form-data ``file(Name, Filename, MediaType, Payload, Parameters)`` descriptors found in a validated multipart/form-data body, preserving part order.',
		argnames is ['Body', 'Files']
	]).

	:- public(part/4).
	:- mode(part(+list(compound), +compound, +list(compound), -compound), one_or_error).
	:- info(part/4, [
		comment is 'Constructs a validated normalized multipart part term from headers, body, and properties.',
		argnames is ['Headers', 'Body', 'Properties', 'Part']
	]).

	:- public(is_part/1).
	:- mode(is_part(@term), zero_or_one).
	:- info(is_part/1, [
		comment is 'True when the argument is a valid normalized multipart part term.',
		argnames is ['Part']
	]).

	:- public(part_headers/2).
	:- mode(part_headers(+compound, -list(compound)), one_or_error).
	:- info(part_headers/2, [
		comment is 'Returns the headers of a validated multipart part term.',
		argnames is ['Part', 'Headers']
	]).

	:- public(part_body/2).
	:- mode(part_body(+compound, -compound), one_or_error).
	:- info(part_body/2, [
		comment is 'Returns the body of a validated multipart part term.',
		argnames is ['Part', 'Body']
	]).

	:- public(part_properties/2).
	:- mode(part_properties(+compound, -list(compound)), one_or_error).
	:- info(part_properties/2, [
		comment is 'Returns the properties of a validated multipart part term.',
		argnames is ['Part', 'Properties']
	]).

	:- public(field/4).
	:- mode(field(+compound, -atom, -term, -list(compound)), zero_or_one).
	:- info(field/4, [
		comment is 'True when the validated multipart part is a textual form-data field part, returning its field name, text value, and additional disposition parameters.',
		argnames is ['Part', 'Name', 'Value', 'Parameters']
	]).

	:- public(file/6).
	:- mode(file(+compound, -atom, -atom, -atom, -compound, -list(compound)), zero_or_one).
	:- info(file/6, [
		comment is 'True when the validated multipart part is a form-data file part, returning its field name, filename, media type, payload term, and additional disposition parameters.',
		argnames is ['Part', 'Name', 'Filename', 'MediaType', 'Payload', 'Parameters']
	]).

	:- public(field_part/4).
	:- mode(field_part(+atom, +term, +list(compound), -compound), one_or_error).
	:- info(field_part/4, [
		comment is 'Constructs a normalized multipart part for a textual form-data field using a content-disposition header, a text/plain body, and additional disposition parameters.',
		argnames is ['Name', 'Value', 'Parameters', 'Part']
	]).

	:- public(file_part/6).
	:- mode(file_part(+atom, +atom, +atom, +compound, +list(compound), -compound), one_or_error).
	:- info(file_part/6, [
		comment is 'Constructs a normalized multipart part for a form-data file using a content-disposition header, the given media type and payload, and additional disposition parameters.',
		argnames is ['Name', 'Filename', 'MediaType', 'Payload', 'Parameters', 'Part']
	]).

	:- public(form_data_body/2).
	:- mode(form_data_body(+list(compound), -compound), one_or_error).
	:- info(form_data_body/2, [
		comment is 'Constructs a multipart/form-data body from an ordered list of ``field(Name, Value, Parameters)`` and ``file(Name, Filename, MediaType, Payload, Parameters)`` descriptors.',
		argnames is ['Items', 'Body']
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	body(MediaType, Parts, Body) :-
		validate_multipart_media_type(MediaType),
		validate_multipart_parts(Parts),
		Body = content(MediaType, multipart(Parts)).

	is_body(Body) :-
		catch(validate_multipart_body(Body), _, fail).

	parse(Source, MediaType, Options, Body) :-
		validate_multipart_media_type(MediaType),
		http_core::parse_body(Source, MediaType, Options, Body),
		validate_multipart_body(Body).

	generate(Sink, Body, Options) :-
		validate_multipart_body(Body),
		http_core::generate_body(Sink, Body, Options).

	media_type(Body, MediaType) :-
		validate_multipart_body(Body),
		Body = content(MediaType, multipart(_)).

	parts(Body, Parts) :-
		validate_multipart_body(Body),
		Body = content(_MediaType, multipart(Parts)).

	fields(Body, Fields) :-
		validate_form_data_body(Body),
		parts(Body, Parts),
		parts_fields(Parts, Fields).

	files(Body, Files) :-
		validate_form_data_body(Body),
		parts(Body, Parts),
		parts_files(Parts, Files).

	part(Headers, Body, Properties, Part) :-
		Part = part(Headers, Body, Properties),
		validate_multipart_part(Part).

	is_part(Part) :-
		catch(validate_multipart_part(Part), _, fail).

	part_headers(Part, Headers) :-
		validate_multipart_part(Part),
		Part = part(Headers, _Body, _Properties).

	part_body(Part, Body) :-
		validate_multipart_part(Part),
		Part = part(_Headers, Body, _Properties).

	part_properties(Part, Properties) :-
		validate_multipart_part(Part),
		Part = part(_Headers, _Body, Properties).

	field(Part, Name, Value, Parameters) :-
		validate_multipart_part(Part),
		part_form_data_disposition(Part, Name, no, Parameters),
		part_body(Part, Body),
		field_body_value(Body, Value),
		!.

	file(Part, Name, Filename, MediaType, Payload, Parameters) :-
		validate_multipart_part(Part),
		part_form_data_disposition(Part, Name, yes(Filename), Parameters),
		part_body(Part, content(MediaType, Payload)),
		!.

	field_part(Name, Value, Parameters, Part) :-
		validate_form_data_name(Name),
		validate_form_data_parameters(Parameters, NormalizedParameters),
		form_data_disposition_header_value(Name, no, NormalizedParameters, HeaderValue),
		part([content_disposition-HeaderValue], content('text/plain', text(Value)), [], Part).

	file_part(Name, Filename, MediaType, Payload, Parameters, Part) :-
		validate_form_data_name(Name),
		validate_form_data_filename(Filename),
		validate_form_data_parameters(Parameters, NormalizedParameters),
		form_data_disposition_header_value(Name, yes(Filename), NormalizedParameters, HeaderValue),
		part([content_disposition-HeaderValue], content(MediaType, Payload), [], Part).

	form_data_body(Items, Body) :-
		form_data_items_parts(Items, Parts),
		body('multipart/form-data', Parts, Body).

	validate_multipart_body(content(MediaType, multipart(Parts))) :-
		!,
		validate_multipart_media_type(MediaType),
		validate_multipart_parts(Parts).
	validate_multipart_body(Body) :-
		domain_error(http_multipart_body, Body).

	validate_multipart_parts([]) :-
		!.
	validate_multipart_parts([Part| Parts]) :-
		!,
		validate_multipart_part(Part),
		validate_multipart_parts(Parts).
	validate_multipart_parts(Parts) :-
		domain_error(http_multipart_parts, Parts).

	validate_multipart_part(Part) :-
		(	valid_multipart_part(Part) ->
			true
		;	domain_error(http_multipart_part, Part)
		).

	valid_multipart_part(part(Headers, Body, Properties)) :-
		catch(http_core::response(http(1, 1), status(200, 'OK'), Headers, Body, Properties, _Response), _, fail).

	validate_multipart_media_type(MediaType) :-
		(	valid_multipart_media_type(MediaType) ->
			true
		;	domain_error(http_multipart_media_type, MediaType)
		).

	validate_form_data_body(Body) :-
		validate_multipart_body(Body),
		(	Body = content(MediaType, multipart(_)), same_media_type(MediaType, 'multipart/form-data') ->
			true
		;	domain_error(http_multipart_form_data_body, Body)
		).

	validate_form_data_name(Name) :-
		validate_form_data_text(http_multipart_form_data_name, Name).

	validate_form_data_filename(Filename) :-
		validate_form_data_text(http_multipart_form_data_filename, Filename).

	validate_form_data_parameters(Parameters, NormalizedParameters) :-
		validate_form_data_parameters(Parameters, [], NormalizedParameters).

	validate_form_data_parameters([], _SeenNames, []) :-
		!.
	validate_form_data_parameters([Parameter| Parameters], SeenNames, [NormalizedName-Value| NormalizedParameters]) :-
		!,
		validate_form_data_parameter(Parameter, NormalizedName, Value),
		validate_form_data_parameter_name_uniqueness(NormalizedName, Parameter, SeenNames, SeenNames0),
		validate_form_data_parameters(Parameters, SeenNames0, NormalizedParameters).
	validate_form_data_parameters(Parameters, _SeenNames, _NormalizedParameters) :-
		domain_error(http_multipart_form_data_parameters, Parameters).

	validate_form_data_parameter(Name-Value, NormalizedName, Value) :-
		!,
		validate_form_data_parameter_name(Name, NormalizedName),
		validate_form_data_parameter_value(Value).
	validate_form_data_parameter(Parameter, _NormalizedName, _Value) :-
		domain_error(http_multipart_form_data_parameter, Parameter).

	validate_form_data_parameter_name(Name, NormalizedName) :-
		(	valid_form_data_parameter_name(Name, NormalizedName) ->
			true
		;	domain_error(http_multipart_form_data_parameter_name, Name)
		).

	validate_form_data_parameter_value(Value) :-
		validate_form_data_text(http_multipart_form_data_parameter_value, Value).

	validate_form_data_text(Domain, Text) :-
		(	valid_form_data_text(Text) ->
			true
		;	domain_error(Domain, Text)
		).

	valid_form_data_text(Text) :-
		valid(text, Text),
		text_to_codes(Text, Codes),
		codes_without_line_breaks(Codes).

	valid_form_data_parameter_name(Name, NormalizedName) :-
		valid(text, Name),
		text_to_codes(Name, Codes),
		Codes \== [],
		codes_are_token(Codes),
		lowercase_ascii_codes(Codes, NormalizedCodes),
		atom_codes(NormalizedName, NormalizedCodes).

	validate_form_data_parameter_name_uniqueness(NormalizedName, Parameter, SeenNames, [NormalizedName| SeenNames]) :-
		(	reserved_form_data_parameter_name(NormalizedName) ->
			domain_error(http_multipart_form_data_parameter, Parameter)
		;	member(NormalizedName, SeenNames) ->
			domain_error(http_multipart_form_data_parameter, Parameter)
		;	true
		).

	reserved_form_data_parameter_name(name).
	reserved_form_data_parameter_name(filename).

	field_body_value(content(_MediaType, text(Value)), Value).
	field_body_value(content('application/octet-stream', binary(Bytes)), Value) :-
		atom_codes(Value, Bytes).

	valid_multipart_media_type(MediaType) :-
		catch(http_core::response(http(1, 1), status(200, 'OK'), [], content(MediaType, binary([])), [], _Response), _, fail),
		multipart_media_type(MediaType).

	parts_fields([], []).
	parts_fields([Part| Parts], Fields) :-
		(	field(Part, Name, Value, Parameters) ->
			Fields = [field(Name, Value, Parameters)| Fields0]
		;	Fields = Fields0
		),
		parts_fields(Parts, Fields0).

	parts_files([], []).
	parts_files([Part| Parts], Files) :-
		(	file(Part, Name, Filename, MediaType, Payload, Parameters) ->
			Files = [file(Name, Filename, MediaType, Payload, Parameters)| Files0]
		;	Files = Files0
		),
		parts_files(Parts, Files0).

	form_data_items_parts([], []) :-
		!.
	form_data_items_parts([Item| Items], [Part| Parts]) :-
		!,
		form_data_item_part(Item, Part),
		form_data_items_parts(Items, Parts).
	form_data_items_parts(Items, _Parts) :-
		domain_error(http_multipart_form_data_items, Items).

	form_data_item_part(field(Name, Value, Parameters), Part) :-
		!,
		field_part(Name, Value, Parameters, Part).
	form_data_item_part(file(Name, Filename, MediaType, Payload, Parameters), Part) :-
		!,
		file_part(Name, Filename, MediaType, Payload, Parameters, Part).
	form_data_item_part(Item, _Part) :-
		domain_error(http_multipart_form_data_item, Item).

	part_form_data_disposition(Part, Name, Filename, ExtraParameters) :-
		part_headers(Part, Headers),
		memberchk(content_disposition-Value, Headers),
		parse_content_disposition(Value, 'form-data', Parameters),
		parse_form_data_disposition_parameters(Parameters, Name, Filename, ExtraParameters).

	parse_form_data_disposition_parameters(Parameters, Name, Filename, ExtraParameters) :-
		parse_form_data_disposition_parameters(Parameters, no, no, [], Name, Filename, ExtraParameters).

	parse_form_data_disposition_parameters([], yes(Name), Filename, _SeenNames, Name, Filename, []) :-
		!.
	parse_form_data_disposition_parameters([], no, _Filename0, _SeenNames, _Name, _Filename, _ExtraParameters) :-
		fail.
	parse_form_data_disposition_parameters([ParameterName-ParameterValue| Parameters], Name0, Filename0, SeenNames, Name, Filename, ExtraParameters) :-
		(	member(ParameterName, SeenNames) ->
			fail
		;	ParameterName == name ->
			Name0 == no,
			parse_form_data_disposition_parameters(Parameters, yes(ParameterValue), Filename0, [ParameterName| SeenNames], Name, Filename, ExtraParameters)
		;	ParameterName == filename ->
			Filename0 == no,
			parse_form_data_disposition_parameters(Parameters, Name0, yes(ParameterValue), [ParameterName| SeenNames], Name, Filename, ExtraParameters)
		;	ExtraParameters = [ParameterName-ParameterValue| ExtraParameters0],
			parse_form_data_disposition_parameters(Parameters, Name0, Filename0, [ParameterName| SeenNames], Name, Filename, ExtraParameters0)
		).

	parse_content_disposition(Value, Type, Parameters) :-
		text_to_codes(Value, Codes),
		phrase(content_disposition(Type, Parameters), Codes).

	content_disposition(Type, Parameters) -->
		optional_whitespace,
		token_codes(TypeCodes),
		optional_whitespace,
		content_disposition_parameters(Parameters),
		optional_whitespace,
		{
			normalize_atom_text(TypeCodes, Type)
		}.

	content_disposition_parameters([Name-Value| Parameters]) -->
		[0';],
		optional_whitespace,
		token_codes(NameCodes),
		optional_whitespace,
		[0'=],
		optional_whitespace,
		parameter_value_codes(ValueCodes),
		optional_whitespace,
		{
			normalize_atom_text(NameCodes, Name),
			atom_codes(Value, ValueCodes)
		},
		content_disposition_parameters(Parameters).
	content_disposition_parameters([]) -->
		[].

	parameter_value_codes(ValueCodes) -->
		quoted_value_codes(ValueCodes),
		!.
	parameter_value_codes(ValueCodes) -->
		token_codes(ValueCodes).

	quoted_value_codes(ValueCodes) -->
		[0'\"],
		quoted_value_content_codes(ValueCodes),
		[0'\"].

	quoted_value_content_codes([Code| Codes]) -->
		[0'\\],
		[Code],
		!,
		quoted_value_content_codes(Codes).
	quoted_value_content_codes([Code| Codes]) -->
		[Code],
		{
			quoted_value_code(Code)
		},
		!,
		quoted_value_content_codes(Codes).
	quoted_value_content_codes([]) -->
		[].

	token_codes([Code| Codes]) -->
		[Code],
		{
			token_code(Code)
		},
		!,
		token_codes_tail(Codes).

	token_codes_tail([Code| Codes]) -->
		[Code],
		{
			token_code(Code)
		},
		!,
		token_codes_tail(Codes).
	token_codes_tail([]) -->
		[].

	optional_whitespace -->
		[Code],
		{
			whitespace_code(Code)
		},
		!,
		optional_whitespace.
	optional_whitespace -->
		[].

	quoted_value_code(Code) :-
		Code =\= 0'",
		Code =\= 0'\\,
		Code =\= 0'\r,
		Code =\= 0'\n.

	codes_are_token([]).
	codes_are_token([Code| Codes]) :-
		token_code(Code),
		codes_are_token(Codes).

	token_code(Code) :-
		Code > 31,
		Code < 127,
		\+ separator_code(Code).

	separator_code(0'( ).
	separator_code(0')).
	separator_code(0'<).
	separator_code(0'>).
	separator_code(0'@).
	separator_code(0',).
	separator_code(0';).
	separator_code(0':).
	separator_code(0'\\).
	separator_code(0'").
	separator_code(0'/).
	separator_code(0'[).
	separator_code(0']).
	separator_code(0'?).
	separator_code(0'=).
	separator_code(0'{).
	separator_code(0'}).
	separator_code(0' ).
	separator_code(0'\t).

	whitespace_code(32).
	whitespace_code(0'\t).

	form_data_disposition_header_value(Name, Filename, Parameters, HeaderValue) :-
		form_data_disposition_parameters(Filename, Name, Parameters, HeaderParameters),
		content_disposition_header_value('form-data', HeaderParameters, HeaderValue).

	form_data_disposition_parameters(no, Name, Parameters, [name-Name| Parameters]).
	form_data_disposition_parameters(yes(Filename), Name, Parameters, [name-Name, filename-Filename| Parameters]).

	content_disposition_header_value(Type, Parameters, HeaderValue) :-
		text_to_codes(Type, TypeCodes),
		content_disposition_parameters_codes(Parameters, ParameterCodes),
		append(TypeCodes, ParameterCodes, Codes),
		atom_codes(HeaderValue, Codes).

	content_disposition_parameters_codes([], []).
	content_disposition_parameters_codes([Name-Value| Parameters], Codes) :-
		text_to_codes(Name, NameCodes),
		text_to_codes(Value, ValueCodes0),
		escaped_quoted_codes(ValueCodes0, ValueCodes),
		content_disposition_parameters_codes(Parameters, Codes2),
		append([0';,32| NameCodes], [0'=,0'"| ValueCodes], Codes1),
		append(Codes1, [0'"| Codes2], Codes).

	escaped_quoted_codes([], []).
	escaped_quoted_codes([Code| Codes], [0'\\, Code| EscapedCodes]) :-
		(	Code =:= 0'"
		;	Code =:= 0'\\
		),
		!,
		escaped_quoted_codes(Codes, EscapedCodes).
	escaped_quoted_codes([Code| Codes], [Code| EscapedCodes]) :-
		escaped_quoted_codes(Codes, EscapedCodes).

	multipart_media_type(MediaType) :-
		atom(MediaType),
		atom_codes(MediaType, Codes0),
		lowercase_ascii_codes(Codes0, Codes),
		codes_prefix(Codes, [0'm, 0'u, 0'l, 0't, 0'i, 0'p, 0'a, 0'r, 0't, 0'/]).

	lowercase_ascii_codes(Codes, LowercaseCodes) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes).

	codes_prefix(Codes, Prefix) :-
		append(Prefix, _Suffix, Codes).

	codes_without_line_breaks([]).
	codes_without_line_breaks([Code| Codes]) :-
		Code =\= 0'\r,
		Code =\= 0'\n,
		codes_without_line_breaks(Codes).

	text_to_codes(Text, Codes) :-
		(	atom(Text) ->
			atom_codes(Text, Codes)
		;	valid(list(character), Text) ->
			chars_to_codes(Text, Codes)
		;	Codes = Text
		).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	normalize_atom_text(Text, NormalizedText) :-
		text_to_codes(Text, Codes),
		lowercase_ascii_codes(Codes, NormalizedCodes),
		atom_codes(NormalizedText, NormalizedCodes).

	same_media_type(MediaType1, MediaType2) :-
		normalize_atom_text(MediaType1, NormalizedMediaType1),
		normalize_atom_text(MediaType2, NormalizedMediaType2),
		NormalizedMediaType1 == NormalizedMediaType2.

:- end_object.
