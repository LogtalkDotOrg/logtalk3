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


:- object(json_lines(_ObjectRepresentation_, _PairRepresentation_, _StringRepresentation_),
	implements(json_lines_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-05-27,
		comment is 'JSON Lines parser and generator.',
		parameters is [
			'ObjectRepresentation' - 'Object representation to be used when decoding JSON objects. Possible values are ``curly`` (default) and ``list``.',
			'PairRepresentation' - 'Pair representation to be used when decoding JSON objects. Possible values are ``dash`` (default), ``equal``, and ``colon``.',
			'StringRepresentation' - 'Text representation to be used when decoding JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), Terms) :-
		open(File, read, Stream),
		line_to_codes(Stream, Codes),
		parse_stream(Codes, Stream, Terms),
		!.
	parse(stream(Stream), Terms) :-
		line_to_codes(Stream, Codes),
		parse_stream(Codes, Stream, Terms),
		!.
	parse(line(Stream), [Term]) :-
		line_to_codes(Stream, Codes),
		phrase(json_value(Term), Codes),
		!.
	parse(codes(Codes), Terms) :-
		phrase(json_values(Terms), Codes),
		!.
	parse(chars(Chars), Terms) :-
		chars_to_codes(Chars, Codes),
		phrase(json_values(Terms), Codes),
		!.
	parse(atom(Atom), Terms) :-
		atom_codes(Atom, Codes),
		phrase(json_values(Terms), Codes),
		!.
	parse(Source, _) :-
		domain_error(json_lines_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), Terms) :-
		phrase(encode(Terms), Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate(stream(Stream), Terms) :-
		phrase(encode(Terms), Codes),
		write_codes(Codes, Stream),
		!.
	generate(codes(Codes), Terms) :-
		phrase(encode(Terms), Codes),
		!.
	generate(chars(Chars), Terms) :-
		phrase(encode(Terms), Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate(atom(Atom), Terms) :-
		phrase(encode(Terms), Codes),
		atom_codes(Atom, Codes),
		!.
	generate(Sink, _) :-
		domain_error(json_lines_sink, Sink).

	% parsing

	parse_stream(end_of_file, Stream, []) :-
		close(Stream).
	parse_stream([Code| Codes], Stream, [Term| Terms]) :-
		phrase(json_value(Term), [Code| Codes]),
		line_to_codes(Stream, NextCodes),
		parse_stream(NextCodes, Stream, Terms).

	% avoid parsing the last element twice
	json_values([Value| Values]) -->
		json_value(Value),
		!,
		(	[0'\n] ->
			json_values(Values)
		;	{Values = []}
		).
	json_values([]) -->
		eos.

	json_value(Value) -->
		json_white_space, [Code], json_value(Code, Value), json_white_space.

	json_value(0'", String) -->
		json_string(String).
	json_value(0'0, Number) -->
		json_number(0'0, Number).
	json_value(0'1, Number) -->
		json_number(0'1, Number).
	json_value(0'2, Number) -->
		json_number(0'2, Number).
	json_value(0'3, Number) -->
		json_number(0'3, Number).
	json_value(0'4, Number) -->
		json_number(0'4, Number).
	json_value(0'5, Number) -->
		json_number(0'5, Number).
	json_value(0'6, Number) -->
		json_number(0'6, Number).
	json_value(0'7, Number) -->
		json_number(0'7, Number).
	json_value(0'8, Number) -->
		json_number(0'8, Number).
	json_value(0'9, Number) -->
		json_number(0'9, Number).
	json_value(0'-, Number) -->
		json_number(0'-, Number).
	json_value(0't, @true) -->
		[0'r, 0'u, 0'e].
	json_value(0'f, @false) -->
		[0'a, 0'l, 0's, 0'e].
	json_value(0'n, @null) -->
		[0'u, 0'l, 0'l].
	json_value(0'{, Object) -->
		json_white_space, json_object(_ObjectRepresentation_, Object).
	json_value(0'[, Array) -->
		json_white_space, json_array(Array).

	json_object(curly, {}) -->
		[0'}], !.
	json_object(curly, {Pairs}) -->
		json_object_pairs(curly, Pairs), [0'}].
	json_object(list, json([])) -->
		[0'}], !.
	json_object(list, json(Pairs)) -->
		json_object_pairs(list, Pairs), [0'}].

	% avoid parsing the last element twice
	json_object_pairs(curly, Pairs) -->
		json_object_pair(_PairRepresentation_, Pair),
		(	[0',] ->
			{Pairs = (Pair, Rest)},
			json_object_pairs(curly, Rest)
		;	{Pairs = Pair}
		).
	json_object_pairs(list, [Pair| Pairs]) -->
		json_object_pair(_PairRepresentation_, Pair),
		(	[0',] ->
			json_object_pairs(list, Pairs)
		;	{Pairs = []}
		).

	json_object_pair(dash, Key-Value) -->
		json_white_space, [0'"], json_string(Key), json_white_space, [0':], json_value(Value).
	json_object_pair(equal, Key=Value) -->
		json_white_space, [0'"], json_string(Key), json_white_space, [0':], json_value(Value).
	json_object_pair(colon, ':'(Key,Value)) -->
		json_white_space, [0'"], json_string(Key), json_white_space, [0':], json_value(Value).

	json_array([]) -->
		[0']], !.
	json_array(Elements) -->
		json_array_elements(Elements), [0']].

	% avoid parsing the last element twice
	json_array_elements([Element| Elements]) -->
		json_value(Element),
		(	[0',] ->
			json_array_elements(Elements)
		;	{Elements = []}
		).

	json_white_space -->
		[Code], {white_space_code(Code)}, !, json_white_space.
	json_white_space -->
		[].

	white_space_code(32).
	white_space_code(0'\t).
	white_space_code(0'\r).

	json_string(String) -->
		[Code], json_string(Code, Codes),
		{json_string_to_string_term(_StringRepresentation_, Codes, String)}.

	% neither " nor \ alone nor the whitespace characters are
	% allowed inside a string according to the standard
	json_string(0'\\, [Code| Codes]) -->
		!, json_string_escape(Code), [Next], json_string(Next, Codes).
	json_string(0'", []) -->
		!.
	json_string(Code, [Code| Codes]) -->
		[Next], json_string(Next, Codes).

	json_string_escape(Code) -->
		[0'u, H1, H2, H3, H4], !, {is_hex(H1), is_hex(H2), is_hex(H3), is_hex(H4), number_codes(Code, [0'0, 0'x, H1, H2, H3, H4])}.
	json_string_escape(Code) -->
		[Code0], {valid_escape_code(Code0, Code)}.

	% see https://www.json.org/json-en.html
	valid_escape_code(0'",  34).
	valid_escape_code(0'\\, 92).
	valid_escape_code(0'/,  47).
	valid_escape_code(0'b,   8).
	valid_escape_code(0'f,  12).
	valid_escape_code(0'n,  10).
	valid_escape_code(0'r,  13).
	valid_escape_code(0't,   9).

	json_number(Code, Number) -->
		json_integer(Code, Codes, Tail0),
		json_fractional(Tail0, Tail),
		json_exponent(Tail),
		{number_codes(Number, Codes)}.

	json_integer(Digit, [Digit| Digits], Tail) -->
		json_digits(Digits, Tail),
		% JSON forbids leading zeros
		{Digit =:= 0'0 -> Digits == Tail; true}.

	json_fractional([0'.| Digits], Tail) -->
		[0'.], !, json_digits(Digits, Tail).
	json_fractional([0'., 0'0| Tail], Tail), [Code] -->
		[Code],	{Code == 0'E; Code == 0'e}, !.
	json_fractional(Tail, Tail) -->
		[].

	json_exponent([E, Sign, Code| Codes]) -->
		[E], {E == 0'E; E == 0'e}, !,
		json_exponent_sign(Sign), json_digit(Code), json_digits(Codes, []).
	json_exponent([]) -->
		[].

	json_exponent_sign(0'+) --> [0'+], !.
	json_exponent_sign(0'-) --> [0'-], !.
	json_exponent_sign(0'+) --> [].

	json_digit(Digit) -->
		[Digit], {0'0 =< Digit, Digit =< 0'9}.

	json_digits([Digit| Digits], Tail) -->
		json_digit(Digit), !, json_digits(Digits, Tail).
	json_digits(Tail, Tail) -->
		[].

	json_string_to_string_term(atom, Codes, Atom) :-
		atom_codes(Atom, Codes).
	json_string_to_string_term(chars, Codes, chars(Chars)) :-
		codes_to_chars(Codes, Chars).
	json_string_to_string_term(codes, Codes, codes(Codes)).

	% generating

	encode([Term| Terms]) -->
		!,
		(	encode_term(Term) ->
			[0'\n], encode(Terms)
		;	{domain_error(json_term, Term)}
		).
	encode([]) -->
		!, [].
	encode(Terms) -->
		{type_error(list, Terms)}.

	encode_term(Term) -->
		{\+ ground(Term), instantiation_error}.

	encode_term(Number) -->
		{number(Number)},
		!,
		{number_codes(Number, [Code| Codes])}, codes([Code| Codes]).

	encode_term(@Literal) -->
		!, {atom(Literal)}, encode_literal(Literal).

	encode_term({}) -->
		!, {_ObjectRepresentation_ == curly}, [0'{, 0'}].
	encode_term({Pairs}) -->
		!, {_ObjectRepresentation_ == curly}, [0'{], encode_pairs(Pairs), [0'}].
	encode_term(json(Pairs)) -->
		!, {_ObjectRepresentation_ == list}, [0'{], encode_pairs(Pairs), [0'}].

	encode_term([]) -->
		!, [0'[, 0']].
	encode_term([Head| Tail]) -->
		!, [0'[], encode_array(Tail, Head), [0']].

	encode_term(chars(Chars)) -->
		!, {chars_to_codes(Chars, Codes)}, [0'"], encode_string(Codes), [0'"].
	encode_term(codes(Codes)) -->
		!, [0'"], encode_string(Codes), [0'"].
	encode_term(Atom) -->
		{atom(Atom)},
		{atom_codes(Atom, Codes)}, [0'"], encode_string(Codes), [0'"].

	encode_literal(true) -->
		[0't, 0'r, 0'u, 0'e].
	encode_literal(false) -->
		[0'f, 0'a, 0'l, 0's, 0'e].
	encode_literal(null) -->
		[0'n, 0'u, 0'l, 0'l].

	encode_pairs((Pair, Pairs)) -->
		!, encode_pair(Pair), [0',], encode_pairs(Pairs).
	encode_pairs([Pair| Pairs]) -->
		!, encode_pairs(Pairs, Pair).
	encode_pairs([]) -->
		!, [].
	encode_pairs(Pair) -->
		encode_pair(Pair).

	encode_pairs([], Pair) -->
		encode_pair(Pair).
	encode_pairs([Next| Pairs], Pair) -->
		encode_pair(Pair), [0',], encode_pairs(Pairs, Next).

	encode_array([], Head) -->
		encode_term(Head).
	encode_array([Next| Tail], Head) -->
		encode_term(Head), [0',], encode_array(Tail, Next).

	encode_pair(Key-Value) -->
		encode_term(Key), [0':], encode_term(Value).
	encode_pair(Key=Value) -->
		encode_term(Key), [0':], encode_term(Value).
	encode_pair(':'(Key,Value)) -->
		encode_term(Key), [0':], encode_term(Value).

	encode_string([]) -->
		[].
	encode_string([Code| Codes]) -->
		encode_string(Code, Codes).

	encode_string(0'\\, [0'u, H1, H2, H3, H4| Codes]) -->
		{is_hex(H1), is_hex(H2), is_hex(H3), is_hex(H4)},
		!,
		[0'\\, 0'u, H1, H2, H3, H4],
		encode_string(Codes).
	encode_string(0'", Codes) -->
		!, [0'\\, 0'"], encode_string(Codes).
	encode_string(0'\\, Codes) -->
		!, [0'\\, 0'\\], encode_string(Codes).
	encode_string( 8, Codes) -->
		!, [0'\\, 0'b], encode_string(Codes).
	encode_string(12, Codes) -->
		!, [0'\\, 0'f], encode_string(Codes).
	encode_string(10, Codes) -->
		!, [0'\\, 0'n], encode_string(Codes).
	encode_string(13, Codes) -->
		!, [0'\\, 0'r], encode_string(Codes).
	encode_string( 9, Codes) -->
		!, [0'\\, 0't], encode_string(Codes).
	encode_string(Code, Codes) -->
		[Code], encode_string(Codes).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

	% auxiliary predicates

	line_to_codes(Stream, Codes) :-
		(	at_end_of_stream(Stream) ->
			Codes = end_of_file
		;	get_code(Stream, Code),
			(	Code == -1 ->
				Codes = end_of_file
			;	line_to_codes(Code, Stream, Codes)
			)
		).

	line_to_codes(-1, _, []) :-
		!.
	line_to_codes(10, _, []) :-
		!.
	line_to_codes(13, Stream, []) :-
		!,
		(	peek_code(Stream, 10) ->
			get_code(Stream, 10)
		;	true
		).
	line_to_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		line_to_codes(NextCode, Stream, Codes).

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

	is_hex(0'0).
	is_hex(0'1).
	is_hex(0'2).
	is_hex(0'3).
	is_hex(0'4).
	is_hex(0'5).
	is_hex(0'6).
	is_hex(0'7).
	is_hex(0'8).
	is_hex(0'9).
	is_hex(0'a).
	is_hex(0'b).
	is_hex(0'c).
	is_hex(0'd).
	is_hex(0'e).
	is_hex(0'f).
	is_hex(0'A).
	is_hex(0'B).
	is_hex(0'C).
	is_hex(0'D).
	is_hex(0'E).
	is_hex(0'F).

:- end_object.


:- object(json_lines(StringRepresentation),
	extends(json_lines(curly, dash, StringRepresentation))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-05-27,
		comment is 'JSON Lines parser and generator. Uses curly terms for parsed JSON objects and dashes for parsed JSON pairs.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

:- end_object.


:- object(json_lines,
	extends(json_lines(curly, dash, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-05-27,
		comment is 'JSON Lines parser and generator. Uses curly terms for parsed JSON objects, dashes for parsed JSON pairs, and atoms for parsed JSON strings.'
	]).

:- end_object.
