%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(base64).

	:- info([
		version is 0:9:0,
		author is 'Paulo Moura',
		date is 2021-03-10,
		comment is 'Base64 parser and generator.'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(byte)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses the Base64 contents read from the given source (``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, or ``file(Path)`` into a list of bytes.',
		argnames is ['Source', 'Bytes']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates Base64 in the representation specified in the first argument (``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, or ``file(Path)`` for the list of bytes in the second argument.',
		argnames is ['Sink', 'Bytes']
	]).

	parse(file(File), Bytes) :-
		reader::file_to_codes(File, Codes),
		phrase(decode(Codes), Bytes).
	parse(stream(Stream), Bytes) :-
		reader::stream_to_codes(Stream, Codes),
		phrase(decode(Codes), Bytes).
	parse(atom(Atom), Bytes) :-
		atom_codes(Atom, Codes),
		phrase(decode(Codes), Bytes).
	parse(chars(Chars), Bytes) :-
		chars_to_codes(Chars, Codes),
		phrase(decode(Codes), Bytes).
	parse(codes(Codes), Bytes) :-
		phrase(decode(Codes), Bytes).

	generate(file(File), Bytes) :-
		phrase(encode(Bytes), Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream).
	generate(stream(Stream), Bytes) :-
		phrase(encode(Bytes), Codes),
		write_codes(Codes, Stream).
	generate(atom(Atom), Bytes) :-
		phrase(encode(Bytes), Codes),
		atom_codes(Atom, Codes).
	generate(chars(Chars), Bytes) :-
		phrase(encode(Bytes), Codes),
		codes_to_chars(Codes, Chars).
	generate(codes(Codes), Bytes) :-
		phrase(encode(Bytes), Codes).

	% parser (decoding)

	decode([Code1, Code2, Code3, Code4| Codes]) -->
		codes_to_bytes(Code4, Code3, Code2, Code1),
		decode(Codes).
	decode([]) -->
		[].

	codes_to_bytes(0'=, 0'=, Code2, Code1) -->
		!,
		{	code_to_index(Code1, Index1),
			code_to_index(Code2, Index2),
			Byte1 is (Index1 << 2) \/ (Index2 >> 4)
		},
		[Byte1].
	codes_to_bytes(0'=, Code3, Code2, Code1) -->
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
		(	Code =:= 0'/ -> Index is 62
		;	Code =:= 0'+ -> Index is 63
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
		[Code1, Code2, Code3, 0'=].

	bytes_to_codes(Byte1) -->
		{	Index1 is Byte1 >> 2,
			Index2 is ((Byte1 /\ 3) << 4),
			index_to_code(Index1, Code1),
			index_to_code(Index2, Code2)
		},
		[Code1, Code2, 0'=, 0'=].

	index_to_code(Index, Code) :-
		(	Index =< 25 -> Code is 0'A + Index
		;	Index =< 51 -> Code is 0'a + Index - 26
		;	Index =< 61 -> Code is 0'0 + Index - 52
		;	Index =:= 62 -> Code is 0'/
		;	Code is 0'+
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

	write_codes([], _).
	write_codes([Code| Codes], Stream) :-
		put_code(Stream, Code),
		write_codes(Codes, Stream).

:- end_object.
