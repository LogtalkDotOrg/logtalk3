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


:- object(base58).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-01-23,
		comment is 'Base58 encoder and decoder (Bitcoin alphabet variant).'
	]).

	:- public(parse/2).
	:- mode(parse(++compound, --list(byte)), one_or_error).
	:- info(parse/2, [
		comment is 'Parses Base58 data from the given source (``atom(Atom)``, ``chars(List)``, or ``codes(List)``) into a list of bytes.',
		argnames is ['Source', 'Bytes']
	]).

	:- public(generate/2).
	:- mode(generate(+compound, +list(byte)), one_or_error).
	:- info(generate/2, [
		comment is 'Generates Base58 in the representation specified in the first argument (``atom(Atom)``, ``chars(List)``, or ``codes(List)``) for the list of bytes in the second argument.',
		argnames is ['Sink', 'Bytes']
	]).

	% Bitcoin alphabet: 123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz
	% Excludes 0, O, I, l to avoid visual ambiguity

	parse(Source, _) :-
		var(Source),
		instantiation_error.
	parse(atom(Atom), Bytes) :-
		atom_codes(Atom, Codes),
		decode(Codes, Bytes),
		!.
	parse(chars(Chars), Bytes) :-
		chars_to_codes(Chars, Codes),
		decode(Codes, Bytes),
		!.
	parse(codes(Codes), Bytes) :-
		decode(Codes, Bytes),
		!.
	parse(Source, _) :-
		domain_error(base58_source, Source).

	generate(Sink, _) :-
		var(Sink),
		instantiation_error.
	generate(atom(Atom), Bytes) :-
		encode(Bytes, Codes),
		atom_codes(Atom, Codes),
		!.
	generate(chars(Chars), Bytes) :-
		encode(Bytes, Codes),
		codes_to_chars(Codes, Chars),
		!.
	generate(codes(Codes), Bytes) :-
		encode(Bytes, Codes),
		!.
	generate(Sink, _) :-
		domain_error(base58_sink, Sink).

	% decoder - convert Base58 string to bytes

	decode(Codes, Bytes) :-
		count_leading(Codes, 0'1, LeadingZeros, RestCodes),
		codes_to_number(RestCodes, 0, Number),
		number_to_bytes(Number, [], RestBytes),
		leading_zeros(LeadingZeros, RestBytes, Bytes).

	count_leading([Code| Codes], Code, N, Rest) :-
		!,
		count_leading(Codes, Code, N1, Rest),
		N is N1 + 1.
	count_leading(Codes, _, 0, Codes).

	codes_to_number([], Acc, Acc).
	codes_to_number([Code| Codes], Acc, Number) :-
		code_to_index(Code, Index),
		Acc1 is Acc * 58 + Index,
		codes_to_number(Codes, Acc1, Number).

	number_to_bytes(0, Bytes, Bytes) :- !.
	number_to_bytes(Number, Acc, Bytes) :-
		Byte is Number /\ 255,
		Number1 is Number >> 8,
		number_to_bytes(Number1, [Byte| Acc], Bytes).

	leading_zeros(0, Bytes, Bytes) :- !.
	leading_zeros(N, Bytes, [0| Result]) :-
		N1 is N - 1,
		leading_zeros(N1, Bytes, Result).

	% encoder - convert bytes to Base58 string

	encode(Bytes, Codes) :-
		count_leading(Bytes, 0, LeadingZeros, RestBytes),
		bytes_to_number(RestBytes, 0, Number),
		number_to_codes(Number, [], RestCodes),
		leading_ones(LeadingZeros, RestCodes, Codes).

	bytes_to_number([], Acc, Acc).
	bytes_to_number([Byte| Bytes], Acc, Number) :-
		Acc1 is (Acc << 8) \/ Byte,
		bytes_to_number(Bytes, Acc1, Number).

	number_to_codes(0, Codes, Codes) :- !.
	number_to_codes(Number, Acc, Codes) :-
		Index is Number mod 58,
		index_to_code(Index, Code),
		Number1 is Number // 58,
		number_to_codes(Number1, [Code| Acc], Codes).

	leading_ones(0, Codes, Codes) :- !.
	leading_ones(N, Codes, [0'1| Result]) :-
		N1 is N - 1,
		leading_ones(N1, Codes, Result).

	% Bitcoin alphabet mapping
	% 123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz

	code_to_index(Code, Index) :-
		(	0'1 =< Code, Code =< 0'9 -> Index is Code - 0'1
		;	0'A =< Code, Code =< 0'H -> Index is Code - 0'A + 9
		;	0'J =< Code, Code =< 0'N -> Index is Code - 0'J + 17
		;	0'P =< Code, Code =< 0'Z -> Index is Code - 0'P + 22
		;	0'a =< Code, Code =< 0'k -> Index is Code - 0'a + 33
		;	0'm =< Code, Code =< 0'z -> Index is Code - 0'm + 44
		;	representation_error(base58)
		),
		!.

	index_to_code(Index, Code) :-
		(	Index < 9 -> Code is 0'1 + Index
		;	Index < 17 -> Code is 0'A + Index - 9
		;	Index < 22 -> Code is 0'J + Index - 17
		;	Index < 33 -> Code is 0'P + Index - 22
		;	Index < 44 -> Code is 0'a + Index - 33
		;	Code is 0'm + Index - 44
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

