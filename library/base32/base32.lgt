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


:- object(base32).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-01-23,
		comment is 'Base32 encoder and decoder (RFC 4648).'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(byte)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses Base32 data from the given source (``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, or ``file(Path)``) into a list of bytes.',
		argnames is ['Source', 'Bytes']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates Base32 in the representation specified in the first argument (``atom(Atom)``, ``chars(List)``, ``codes(List)``, ``stream(Stream)``, or ``file(Path)``) for the list of bytes in the second argument.',
		argnames is ['Sink', 'Bytes']
	]).

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(file(File), Bytes) :-
		reader::file_to_codes(File, Codes),
		phrase(decode(Codes), Bytes),
		!.
	parse(stream(Stream), Bytes) :-
		reader::stream_to_codes(Stream, Codes),
		phrase(decode(Codes), Bytes),
		!.
	parse(atom(Atom), Bytes) :-
		atom_codes(Atom, Codes),
		phrase(decode(Codes), Bytes),
		!.
	parse(chars(Chars), Bytes) :-
		chars_to_codes(Chars, Codes),
		phrase(decode(Codes), Bytes),
		!.
	parse(codes(Codes), Bytes) :-
		phrase(decode(Codes), Bytes),
		!.
	parse(Source, _) :-
		domain_error(base32_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(file(File), Bytes) :-
		phrase(encode(Bytes), Codes),
		open(File, write, Stream),
		write_codes(Codes, Stream),
		close(Stream),
		!.
	generate(stream(Stream), Bytes) :-
		phrase(encode(Bytes), Codes),
		write_codes(Codes, Stream),
		!.
	generate(atom(Atom), Bytes) :-
		phrase(encode(Bytes), Codes),
		atom_codes(Atom, Codes),
		!.
	generate(chars(Chars), Bytes) :-
		phrase(encode(Bytes), Codes),
		codes_to_chars(Codes, Chars).
	generate(codes(Codes), Bytes) :-
		phrase(encode(Bytes), Codes),
		!.
	generate(Sink, _) :-
		domain_error(base32_sink, Sink).

	% parser (decoding) - 8 Base32 chars decode to 5 bytes

	decode([]) -->
		[].
	decode([C1, C2, C3, C4, C5, C6, C7, C8| Codes]) -->
		codes_to_bytes(C1, C2, C3, C4, C5, C6, C7, C8),
		decode(Codes).

	codes_to_bytes(C1, C2, 0'=, 0'=, 0'=, 0'=, 0'=, 0'=) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			B1 is (I1 << 3) \/ (I2 >> 2)
		},
		[B1].
	codes_to_bytes(C1, C2, C3, C4, 0'=, 0'=, 0'=, 0'=) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			code_to_index(C4, I4),
			B1 is (I1 << 3) \/ (I2 >> 2),
			B2 is ((I2 /\ 3) << 6) \/ (I3 << 1) \/ (I4 >> 4)
		},
		[B1, B2].
	codes_to_bytes(C1, C2, C3, C4, C5, 0'=, 0'=, 0'=) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			code_to_index(C4, I4),
			code_to_index(C5, I5),
			B1 is (I1 << 3) \/ (I2 >> 2),
			B2 is ((I2 /\ 3) << 6) \/ (I3 << 1) \/ (I4 >> 4),
			B3 is ((I4 /\ 15) << 4) \/ (I5 >> 1)
		},
		[B1, B2, B3].
	codes_to_bytes(C1, C2, C3, C4, C5, C6, C7, 0'=) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			code_to_index(C4, I4),
			code_to_index(C5, I5),
			code_to_index(C6, I6),
			code_to_index(C7, I7),
			B1 is (I1 << 3) \/ (I2 >> 2),
			B2 is ((I2 /\ 3) << 6) \/ (I3 << 1) \/ (I4 >> 4),
			B3 is ((I4 /\ 15) << 4) \/ (I5 >> 1),
			B4 is ((I5 /\ 1) << 7) \/ (I6 << 2) \/ (I7 >> 3)
		},
		[B1, B2, B3, B4].
	codes_to_bytes(C1, C2, C3, C4, C5, C6, C7, C8) -->
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			code_to_index(C4, I4),
			code_to_index(C5, I5),
			code_to_index(C6, I6),
			code_to_index(C7, I7),
			code_to_index(C8, I8),
			B1 is (I1 << 3) \/ (I2 >> 2),
			B2 is ((I2 /\ 3) << 6) \/ (I3 << 1) \/ (I4 >> 4),
			B3 is ((I4 /\ 15) << 4) \/ (I5 >> 1),
			B4 is ((I5 /\ 1) << 7) \/ (I6 << 2) \/ (I7 >> 3),
			B5 is ((I7 /\ 7) << 5) \/ I8
		},
		[B1, B2, B3, B4, B5].

	% A-Z = 0-25, 2-7 = 26-31
	code_to_index(Code, Index) :-
		(	0'A =< Code, Code =< 0'Z -> Index is Code - 0'A
		;	0'a =< Code, Code =< 0'z -> Index is Code - 0'a
		;	0'2 =< Code, Code =< 0'7 -> Index is Code - 0'2 + 26
		;	representation_error(base32)
		),
		!.

	% generator (encoding) - 5 bytes encode to 8 Base32 chars

	encode([]) -->
		[].
	encode([B1, B2, B3, B4, B5| Bytes]) -->
		!,
		bytes_to_codes_5(B1, B2, B3, B4, B5),
		encode(Bytes).
	encode([B1, B2, B3, B4]) -->
		!,
		bytes_to_codes_4(B1, B2, B3, B4).
	encode([B1, B2, B3]) -->
		!,
		bytes_to_codes_3(B1, B2, B3).
	encode([B1, B2]) -->
		!,
		bytes_to_codes_2(B1, B2).
	encode([B1]) -->
		bytes_to_codes_1(B1).

	bytes_to_codes_5(B1, B2, B3, B4, B5) -->
		{	I1 is B1 >> 3,
			I2 is ((B1 /\ 7) << 2) \/ (B2 >> 6),
			I3 is (B2 >> 1) /\ 31,
			I4 is ((B2 /\ 1) << 4) \/ (B3 >> 4),
			I5 is ((B3 /\ 15) << 1) \/ (B4 >> 7),
			I6 is (B4 >> 2) /\ 31,
			I7 is ((B4 /\ 3) << 3) \/ (B5 >> 5),
			I8 is B5 /\ 31,
			index_to_code(I1, C1),
			index_to_code(I2, C2),
			index_to_code(I3, C3),
			index_to_code(I4, C4),
			index_to_code(I5, C5),
			index_to_code(I6, C6),
			index_to_code(I7, C7),
			index_to_code(I8, C8)
		},
		[C1, C2, C3, C4, C5, C6, C7, C8].

	bytes_to_codes_4(B1, B2, B3, B4) -->
		{	I1 is B1 >> 3,
			I2 is ((B1 /\ 7) << 2) \/ (B2 >> 6),
			I3 is (B2 >> 1) /\ 31,
			I4 is ((B2 /\ 1) << 4) \/ (B3 >> 4),
			I5 is ((B3 /\ 15) << 1) \/ (B4 >> 7),
			I6 is (B4 >> 2) /\ 31,
			I7 is ((B4 /\ 3) << 3),
			index_to_code(I1, C1),
			index_to_code(I2, C2),
			index_to_code(I3, C3),
			index_to_code(I4, C4),
			index_to_code(I5, C5),
			index_to_code(I6, C6),
			index_to_code(I7, C7)
		},
		[C1, C2, C3, C4, C5, C6, C7, 0'=].

	bytes_to_codes_3(B1, B2, B3) -->
		{	I1 is B1 >> 3,
			I2 is ((B1 /\ 7) << 2) \/ (B2 >> 6),
			I3 is (B2 >> 1) /\ 31,
			I4 is ((B2 /\ 1) << 4) \/ (B3 >> 4),
			I5 is ((B3 /\ 15) << 1),
			index_to_code(I1, C1),
			index_to_code(I2, C2),
			index_to_code(I3, C3),
			index_to_code(I4, C4),
			index_to_code(I5, C5)
		},
		[C1, C2, C3, C4, C5, 0'=, 0'=, 0'=].

	bytes_to_codes_2(B1, B2) -->
		{	I1 is B1 >> 3,
			I2 is ((B1 /\ 7) << 2) \/ (B2 >> 6),
			I3 is (B2 >> 1) /\ 31,
			I4 is ((B2 /\ 1) << 4),
			index_to_code(I1, C1),
			index_to_code(I2, C2),
			index_to_code(I3, C3),
			index_to_code(I4, C4)
		},
		[C1, C2, C3, C4, 0'=, 0'=, 0'=, 0'=].

	bytes_to_codes_1(B1) -->
		{	I1 is B1 >> 3,
			I2 is ((B1 /\ 7) << 2),
			index_to_code(I1, C1),
			index_to_code(I2, C2)
		},
		[C1, C2, 0'=, 0'=, 0'=, 0'=, 0'=, 0'=].

	index_to_code(Index, Code) :-
		(	Index =< 25 -> Code is 0'A + Index
		;	Code is 0'2 + Index - 26
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

