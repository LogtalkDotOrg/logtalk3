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


:- object(base85).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-01-23,
		comment is 'Base85 encoder and decoder (Ascii85/btoa variant).'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(byte)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses Base85 (Ascii85) data from the given source (``atom(Atom)``, ``chars(List)``, or ``codes(List)``) into a list of bytes.',
		argnames is ['Source', 'Bytes']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates Base85 (Ascii85) in the representation specified in the first argument (``atom(Atom)``, ``chars(List)``, or ``codes(List)``) for the list of bytes in the second argument.',
		argnames is ['Sink', 'Bytes']
	]).

	% Ascii85 uses characters '!' (33) to 'u' (117), plus 'z' for all-zero groups

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(atom(Atom), Bytes) :-
		atom_codes(Atom, Codes),
		strip_delimiters(Codes, StrippedCodes),
		phrase(decode(StrippedCodes), Bytes),
		!.
	parse(chars(Chars), Bytes) :-
		chars_to_codes(Chars, Codes),
		strip_delimiters(Codes, StrippedCodes),
		phrase(decode(StrippedCodes), Bytes),
		!.
	parse(codes(Codes), Bytes) :-
		strip_delimiters(Codes, StrippedCodes),
		phrase(decode(StrippedCodes), Bytes),
		!.
	parse(Source, _) :-
		domain_error(base85_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(atom(Atom), Bytes) :-
		phrase(encode(Bytes), Codes),
		atom_codes(Atom, Codes),
		!.
	generate(chars(Chars), Bytes) :-
		phrase(encode(Bytes), Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate(codes(Codes), Bytes) :-
		phrase(encode(Bytes), Codes),
		!.
	generate(Sink, _) :-
		domain_error(base85_sink, Sink).

	% strip optional <~ and ~> delimiters
	strip_delimiters([0'<, 0'~| Codes], Stripped) :-
		!,
		strip_end_delimiter(Codes, Stripped).
	strip_delimiters(Codes, Stripped) :-
		strip_end_delimiter(Codes, Stripped).

	strip_end_delimiter(Codes, Stripped) :-
		append_(Stripped, [0'~, 0'>], Codes),
		!.
	strip_end_delimiter(Codes, Codes).

	append_([], L, L).
	append_([H| T], L, [H| R]) :-
		append_(T, L, R).

	% decoder - 5 Ascii85 chars decode to 4 bytes

	decode([]) -->
		[].
	decode([0'z| Codes]) -->
		!,
		[0, 0, 0, 0],
		decode(Codes).
	decode([C1, C2, C3, C4, C5| Codes]) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			code_to_index(C4, I4),
			code_to_index(C5, I5),
			Value is I1 * 52200625 + I2 * 614125 + I3 * 7225 + I4 * 85 + I5,
			B1 is (Value >> 24) /\ 255,
			B2 is (Value >> 16) /\ 255,
			B3 is (Value >> 8) /\ 255,
			B4 is Value /\ 255
		},
		[B1, B2, B3, B4],
		decode(Codes).
	decode([C1, C2, C3, C4]) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			code_to_index(C4, I4),
			Value is I1 * 52200625 + I2 * 614125 + I3 * 7225 + I4 * 85 + 84,
			B1 is (Value >> 24) /\ 255,
			B2 is (Value >> 16) /\ 255,
			B3 is (Value >> 8) /\ 255
		},
		[B1, B2, B3].
	decode([C1, C2, C3]) -->
		!,
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			code_to_index(C3, I3),
			Value is I1 * 52200625 + I2 * 614125 + I3 * 7225 + 84 * 85 + 84,
			B1 is (Value >> 24) /\ 255,
			B2 is (Value >> 16) /\ 255
		},
		[B1, B2].
	decode([C1, C2]) -->
		{	code_to_index(C1, I1),
			code_to_index(C2, I2),
			Value is I1 * 52200625 + I2 * 614125 + 84 * 7225 + 84 * 85 + 84,
			B1 is (Value >> 24) /\ 255
		},
		[B1].

	code_to_index(Code, Index) :-
		(	0'! =< Code, Code =< 0'u ->
			Index is Code - 0'!
		;	representation_error(base85)
		),
		!.

	% encoder - 4 bytes encode to 5 Ascii85 chars

	encode([]) -->
		[].
	encode([0, 0, 0, 0| Bytes]) -->
		!,
		[0'z],
		encode(Bytes).
	encode([B1, B2, B3, B4| Bytes]) -->
		!,
		{	Value is (B1 << 24) \/ (B2 << 16) \/ (B3 << 8) \/ B4,
			I5 is Value mod 85, V1 is Value // 85,
			I4 is V1 mod 85, V2 is V1 // 85,
			I3 is V2 mod 85, V3 is V2 // 85,
			I2 is V3 mod 85, I1 is V3 // 85,
			C1 is I1 + 0'!, C2 is I2 + 0'!, C3 is I3 + 0'!,
			C4 is I4 + 0'!, C5 is I5 + 0'!
		},
		[C1, C2, C3, C4, C5],
		encode(Bytes).
	encode([B1, B2, B3]) -->
		!,
		{	Value is (B1 << 24) \/ (B2 << 16) \/ (B3 << 8),
			_ is Value mod 85, V1 is Value // 85,
			I4 is V1 mod 85, V2 is V1 // 85,
			I3 is V2 mod 85, V3 is V2 // 85,
			I2 is V3 mod 85, I1 is V3 // 85,
			C1 is I1 + 0'!, C2 is I2 + 0'!, C3 is I3 + 0'!, C4 is I4 + 0'!
		},
		[C1, C2, C3, C4].
	encode([B1, B2]) -->
		!,
		{	Value is (B1 << 24) \/ (B2 << 16),
			_ is Value mod 85, V1 is Value // 85,
			_ is V1 mod 85, V2 is V1 // 85,
			I3 is V2 mod 85, V3 is V2 // 85,
			I2 is V3 mod 85, I1 is V3 // 85,
			C1 is I1 + 0'!, C2 is I2 + 0'!, C3 is I3 + 0'!
		},
		[C1, C2, C3].
	encode([B1]) -->
		{	Value is (B1 << 24),
			_ is Value mod 85, V1 is Value // 85,
			_ is V1 mod 85, V2 is V1 // 85,
			_ is V2 mod 85, V3 is V2 // 85,
			I2 is V3 mod 85, I1 is V3 // 85,
			C1 is I1 + 0'!, C2 is I2 + 0'!
		},
		[C1, C2].

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

