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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-01-23,
		comment is 'Unit tests for the "base58" library.'
	]).

	cover(base58).

	% Test vectors based on common Base58 examples

	test(base58_generate_2_empty, true(Base58 == '')) :-
		base58::generate(atom(Base58), []).

	test(base58_generate_2_hello, true(Base58 == 'JxF12TrwUP45BMd')) :-
		atom_codes('Hello World', Bytes),
		base58::generate(atom(Base58), Bytes).

	test(base58_parse_2_empty, true(Bytes == [])) :-
		base58::parse(atom(''), Bytes).

	test(base58_parse_2_hello, true(Atom == 'Hello World')) :-
		base58::parse(atom('JxF12TrwUP45BMd'), Bytes),
		atom_codes(Atom, Bytes).

	test(base58_roundtrip_01, true(Bytes == Bytes0)) :-
		atom_codes('Hello, World!', Bytes0),
		base58::generate(codes(Codes), Bytes0),
		base58::parse(codes(Codes), Bytes).

	test(base58_roundtrip_02, true(Bytes == Bytes0)) :-
		atom_codes('The quick brown fox', Bytes0),
		base58::generate(atom(Atom), Bytes0),
		base58::parse(atom(Atom), Bytes).

	test(base58_leading_zeros, true(Bytes == [0, 0, 0, 1, 2, 3])) :-
		base58::generate(atom(Base58), [0, 0, 0, 1, 2, 3]),
		base58::parse(atom(Base58), Bytes).

	test(base58_generate_2_chars, true) :-
		atom_codes('test', Bytes),
		base58::generate(chars(Chars), Bytes),
		base58::parse(chars(Chars), Bytes2),
		Bytes == Bytes2.

	test(base58_parse_2_codes, true(Bytes == Bytes2)) :-
		atom_codes('test', Bytes),
		base58::generate(codes(Codes), Bytes),
		base58::parse(codes(Codes), Bytes2).

	% Bitcoin address test (leading 1s represent zero bytes)
	test(base58_leading_ones, true(Base58 == '111Ldp')) :-
		base58::generate(atom(Base58), [0, 0, 0, 1, 2, 3]).

:- end_object.

