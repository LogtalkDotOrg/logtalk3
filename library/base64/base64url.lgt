%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(base64url).

	:- info([
		version is 0:9:0,
		author is 'Paulo Moura',
		date is 2021-03-10,
		comment is 'Base64URL parser and generator.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --types([atom,chars,codes])), one_or_error).
	:- info(parse/2, [
		comment is 'Parses the Base64URL data from the given source (``atom(Atom)``, ``chars(List)``, or ``codes(List)`` into a URL (using the same format as the source).',
		argnames is ['Source', 'URL']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +types([atom,chars,codes])), one_or_error).
	:- info(generate/2, [
		comment is 'Generates Base64URL data in the representation specified in the first argument (``atom(Atom)``, ``chars(List)``, or ``codes(List)`` for the given URL (given in the same format as the sink).',
		argnames is ['Sink', 'URL']
	]).

	parse(atom(Atom), URL) :-
		atom_codes(Atom, Codes),
		phrase(decode(Codes), URLCodes),
		atom_codes(URL, URLCodes).
	parse(chars(Chars), URL) :-
		chars_to_codes(Chars, Codes),
		phrase(decode(Codes), URLCodes),
		codes_to_chars(URLCodes, URL).
	parse(codes(Codes), URL) :-
		phrase(decode(Codes), URL).

	generate(atom(Atom), URL) :-
		atom_codes(URL, URLCodes),
		phrase(encode(URLCodes), Codes),
		atom_codes(Atom, Codes).
	generate(chars(Chars), URL) :-
		chars_to_codes(URL, URLCodes),
		phrase(encode(URLCodes), Codes),
		codes_to_chars(Codes, Chars).
	generate(codes(Codes), URL) :-
		phrase(encode(URL), Codes).

	% parser (decoding)

	decode([Code1, Code2, Code3, Code4| Codes]) -->
		!,
		codes_to_bytes(Code4, Code3, Code2, Code1),
		decode(Codes).
	decode([Code1, Code2, Code3]) -->
		!,
		codes_to_bytes(Code3, Code2, Code1).
	decode([Code1, Code2]) -->
		codes_to_bytes(Code2, Code1).
	decode([]) -->
		[].

	codes_to_bytes(Code2, Code1) -->
		!,
		{	code_to_index(Code1, Index1),
			code_to_index(Code2, Index2),
			Byte1 is (Index1 << 2) \/ (Index2 >> 4)
		},
		[Byte1].
	codes_to_bytes(Code3, Code2, Code1) -->
		!,
		{	code_to_index(Code1, Index1),
			code_to_index(Code2, Index2),
			code_to_index(Code3, Index3),
			Byte1 is (Index1 << 2) \/ (Index2 >> 4),
			Byte2 is ((Index2 /\ 15) << 4) \/ (Index3 >> 2)
		},
		[Byte1, Byte2].
	codes_to_bytes(Code4, Code3, Code2, Code1) -->
		{	code_to_index(Code1, Index1),
			code_to_index(Code2, Index2),
			code_to_index(Code3, Index3),
			code_to_index(Code4, Index4),
			Byte1 is (Index1 << 2) \/ (Index2 >> 4),
			Byte2 is ((Index2 /\ 15) << 4) \/ (Index3 >> 2),
			Byte3 is ((Index3 /\ 3) << 6) \/ Index4
		},
		[Byte1, Byte2, Byte3].

	code_to_index(Code, Index) :-
		(	Code =:= 0'_ -> Index is 62
		;	Code =:= 0'- -> Index is 63
		;	0'0 =< Code, Code =< 0'9 -> Index is Code - 0'0 + 52
		;	0'A =< Code, Code =< 0'Z -> Index is Code - 0'A
		;	0'a =< Code, Code =< 0'z -> Index is Code - 0'a + 26
		;	representation_error(base64)
		),
		!.

	% generator (encoding)

	encode([Byte1, Byte2, Byte3| Bytes]) -->
		!,
		bytes_to_codes(Byte1, Byte2, Byte3),
		encode(Bytes).
	encode([Byte1, Byte2]) -->
		!,
		bytes_to_codes(Byte1, Byte2).
	encode([Byte]) -->
		!,
		bytes_to_codes(Byte).
	encode([]) -->
		[].

	bytes_to_codes(Byte1, Byte2, Byte3) -->
		{	Index1 is Byte1 >> 2,
			Index2 is ((Byte1 /\ 3) << 4) \/ (Byte2 >> 4),
			Index3 is ((Byte2 /\ 15) << 2) \/ (Byte3 >> 6),
			Index4 is Byte3 /\ 63,
			index_to_code(Index1, Code1),
			index_to_code(Index2, Code2),
			index_to_code(Index3, Code3),
			index_to_code(Index4, Code4)
		},
		[Code1, Code2, Code3, Code4].

	bytes_to_codes(Byte1, Byte2) -->
		{	Index1 is Byte1 >> 2,
			Index2 is ((Byte1 /\ 3) << 4) \/ (Byte2 >> 4),
			Index3 is ((Byte2 /\ 15) << 2),
			index_to_code(Index1, Code1),
			index_to_code(Index2, Code2),
			index_to_code(Index3, Code3)
		},
		[Code1, Code2, Code3].

	bytes_to_codes(Byte1) -->
		{	Index1 is Byte1 >> 2,
			Index2 is ((Byte1 /\ 3) << 4),
			index_to_code(Index1, Code1),
			index_to_code(Index2, Code2)
		},
		[Code1, Code2].

	index_to_code(Index, Code) :-
		(	Index =< 25 -> Code is 0'A + Index
		;	Index =< 51 -> Code is 0'a + Index - 26
		;	Index =< 61 -> Code is 0'0 + Index - 52
		;	Index =:= 62 -> Code is 0'_
		;	Code is 0'-
		),
		!.

	% auxiliary predicates

	chars_to_codes([], []).
	chars_to_codes([Char| Chars], [Code| Codes]) :-
		char_code(Char, Code),
		chars_to_codes(Chars, Codes).

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.
