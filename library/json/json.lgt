%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org> and
%  Jacinto Dávila <jdavila@optimusprime.ai>
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


:- object(json(_StringRepresentation_),
	implements(json_protocol)).

	:- info([
		version is 0:10:0,
		author is 'Paulo Moura and Jacinto Dávila',
		date is 2021-03-22,
		comment is 'JSON parser and generator.',
		parameters is [
			'StringRepresentation' - 'Text representation to be used when decoding JSON strings. Possible values are ``atom`` (default), ``chars``, and ``codes``.'
		]
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), JSON) :-
		reader::file_to_codes(File, Codes),
		phrase(json(JSON), Codes),
		!.
	parse(stream(Stream), JSON) :-
		reader::stream_to_codes(Stream, Codes),
		phrase(json(JSON), Codes),
		!.
	parse(codes(Codes), JSON) :-
		phrase(json(JSON), Codes),
		!.
	parse(chars(Chars), JSON) :-
		chars_to_codes(Chars, Codes),
		phrase(json(JSON), Codes),
		!.
	parse(atom(Atom), JSON) :-
		atom_codes(Atom, Codes),
		phrase(json(JSON), Codes),
		!.
	parse(Source, _) :-
		domain_error(json_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), Term) :-
		phrase(encode(Term), Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate(stream(Stream), Term) :-
		phrase(encode(Term), Codes),
		write_codes(Codes, Stream),
		!.
	generate(codes(Codes), Term) :-
		phrase(encode(Term), Codes),
		!.
	generate(chars(Chars), Term) :-
		phrase(encode(Term), Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate(atom(Atom), Term) :-
		phrase(encode(Term), Codes),
		atom_codes(Atom, Codes),
		!.
	generate(Sink, _) :-
		domain_error(json_sink, Sink).

	json(JSON) -->
		json_white_space, json_value(JSON), json_white_space.

	json_value(String) -->
		json_string(String), !.
	json_value(Number) -->
		json_number(Number), !.
	json_value(@true) -->
		[0't, 0'r, 0'u, 0'e], !.
	json_value(@false) -->
		[0'f, 0'a, 0'l, 0's, 0'e], !.
	json_value(@null) -->
		[0'n, 0'u, 0'l, 0'l], !.
	json_value(Object) -->
		json_object(Object), !.
	json_value(Array) -->
		json_array(Array).

	json_object({}) -->
		[0'{], json_white_space, [0'}], !.
	json_object({Pairs}) -->
		[0'{], json_object_pairs(Pairs), [0'}].

	json_object_pairs((Pair, Pairs)) -->
		json_object_pair(Pair), [0',], !, json_object_pairs(Pairs).
	json_object_pairs(Pair) -->
		json_object_pair(Pair).

	json_object_pair(Key-Value) -->
		json_white_space, json_string(Key), json_white_space, [0':], json(Value).

	json_array([]) -->
		[0'[], json_white_space, [0']], !.
	json_array(Elements) -->
		[0'[], json_array_elements(Elements), [0']].

	json_array_elements([Element| Elements]) -->
		json(Element), [0',], !, json_array_elements(Elements).
	json_array_elements([Element]) -->
		json(Element).

	json_white_space -->
		[Code], {white_space_code(Code)}, !, json_white_space.
	json_white_space -->
		[].

	white_space_code(32).
	white_space_code(0'\t).
	white_space_code(0'\n).
	white_space_code(0'\r).

	json_string(String) -->
		[0'"], json_string_contents(Codes), [0'"],
		{json_string_to_string_term(_StringRepresentation_, Codes, String)}.

	json_string_contents([0'\\, 0'u, H1, H2, H3, H4| Codes]) -->
		[0'\\, 0'u, H1, H2, H3, H4], !, {is_hex(H1), is_hex(H2), is_hex(H3), is_hex(H4)},
		json_string_contents(Codes).

	json_string_contents([0'\\, Code| Codes]) -->
		[0'\\, Code], !, {valid_escape_code(Code)},
		json_string_contents(Codes).

	json_string_contents([Code| Codes]) -->
		[Code],
		{\+ invalid_string_code(Code)}, !,
		json_string_contents(Codes).
	json_string_contents([]) -->
		[].

	% neither " nor \ alone nor the whitespace characters are
	% allowed inside a string according to the standard
	invalid_string_code(0'").
	invalid_string_code(0'\\).
	invalid_string_code(0'\n).
	invalid_string_code(0'\t).
	invalid_string_code(0'\r).

	% see https://www.json.org/json-en.html
	valid_escape_code(0'").
	valid_escape_code(0'\\).
	valid_escape_code(0'/).
	valid_escape_code(0'b).
	valid_escape_code(0'f).
	valid_escape_code(0'n).
	valid_escape_code(0'r).
	valid_escape_code(0't).

	json_number(Number) -->
		json_integer(Codes, Tail0),
		json_fractional(Tail0, Tail),
		json_exponent(Tail),
		{number_codes(Number, Codes)}.

	json_integer([Digit| Digits], Tail) -->
		json_digit(Digit), !, json_digits(Digits, Tail),
		% JSON forbids leading zeros
		{Digit =:= 0'0 -> Digits == Tail; true}.
	json_integer([0'-, Digit| Digits], Tail) -->
		[0'-], json_digit(Digit), json_digits(Digits, Tail).

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

	encode(Term) -->
		{var(Term), instantiation_error}.

	encode(Number) -->
		{number(Number)},
		!,
		{number_codes(Number, [Code| Codes])}, [Code| Codes].

	encode(@Literal) -->
		!, {atom(Literal)}, encode_literal(Literal).

	encode({}) -->
		!, [0'{, 0'}].
	encode({Pairs}) -->
		!, [0'{], encode_pairs(Pairs), [0'}].

	encode([]) -->
		!, [0'[, 0']].
	encode([Head| Tail]) -->
		!, [0'[], encode_array(Tail, Head), [0']].

	encode(chars(Chars)) -->
		!, {chars_to_codes(Chars, Codes)}, [0'"], encode_string(Codes), [0'"].
	encode(codes(Codes)) -->
		!, [0'"], encode_string(Codes), [0'"].
	encode(Atom) -->
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
	encode_pairs(Pair) -->
		encode_pair(Pair).

	encode_array([Next| Tail], Head) -->
		encode(Head), [0',], encode_array(Tail, Next).
	encode_array([], Head) -->
		encode(Head).

	encode_pair(Key-Value) -->
		encode(Key), [0':], encode(Value).

	encode_string([]) -->
		[].
	encode_string([Code| Codes]) -->
		encode_string(Code, Codes).

	encode_string(0'\\, [0'u, H1, H2, H3, H4| Codes]) -->
		{is_hex(H1), is_hex(H2), is_hex(H3), is_hex(H4)},
		!,
		[0'\\, 0'u, H1, H2, H3, H4],
		encode_string(Codes).
	encode_string(34, Codes) -->
		!, [0'\\, 0'"], encode_string(Codes).
	encode_string(92, Codes) -->
		!, [0'\\, 0'\\], encode_string(Codes).
	encode_string(47, Codes) -->
		!, [0'\\, 0'/], encode_string(Codes).
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

	% auxiliary predicates

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

	is_hex(Code) :-
		(	0'0 =< Code, Code =< 0'9
		;	0'a =< Code, Code =< 0'f
		;	0'A =< Code, Code =< 0'F
		),
		!.

:- end_object.


:- object(json,
	extends(json(atom))).

	:- info([
		version is 1:0:0,
		author is 'Jacinto Dávila',
		date is 2021-03-05,
		comment is 'JSON parser and generator. Uses atoms to represent decoded JSON strings.'
	]).

:- end_object.
