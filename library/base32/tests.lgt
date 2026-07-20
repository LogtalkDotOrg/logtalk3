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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2025-07-20,
		comment is 'Unit tests for the "base32" library.'
	]).

	cover(base32).

	% RFC 4648 test vectors
	test(base32_generate_2_empty, deterministic(Base32 == '')) :-
		base32::generate(atom(Base32), []).

	test(base32_generate_2_f, deterministic(Base32 == 'MY======')) :-
		atom_codes(f, Bytes),
		base32::generate(atom(Base32), Bytes).

	test(base32_generate_2_fo, deterministic(Base32 == 'MZXQ====')) :-
		atom_codes(fo, Bytes),
		base32::generate(atom(Base32), Bytes).

	test(base32_generate_2_foo, deterministic(Base32 == 'MZXW6===')) :-
		atom_codes(foo, Bytes),
		base32::generate(atom(Base32), Bytes).

	test(base32_generate_2_foob, deterministic(Base32 == 'MZXW6YQ=')) :-
		atom_codes(foob, Bytes),
		base32::generate(atom(Base32), Bytes).

	test(base32_generate_2_fooba, deterministic(Base32 == 'MZXW6YTB')) :-
		atom_codes(fooba, Bytes),
		base32::generate(atom(Base32), Bytes).

	test(base32_generate_2_foobar, deterministic(Base32 == 'MZXW6YTBOI======')) :-
		atom_codes(foobar, Bytes),
		base32::generate(atom(Base32), Bytes).

	test(base32_parse_2_empty, deterministic(Bytes == [])) :-
		base32::parse(atom(''), Bytes).

	test(base32_parse_2_f, deterministic(Atom == f)) :-
		base32::parse(atom('MY======'), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_parse_2_fo, deterministic(Atom == fo)) :-
		base32::parse(atom('MZXQ===='), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_parse_2_foo, deterministic(Atom == foo)) :-
		base32::parse(atom('MZXW6==='), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_parse_2_foob, deterministic(Atom == foob)) :-
		base32::parse(atom('MZXW6YQ='), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_parse_2_fooba, deterministic(Atom == fooba)) :-
		base32::parse(atom('MZXW6YTB'), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_parse_2_foobar, deterministic(Atom == foobar)) :-
		base32::parse(atom('MZXW6YTBOI======'), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_roundtrip_01, deterministic(Bytes == Bytes0)) :-
		atom_codes('Hello, World!', Bytes0),
		base32::generate(codes(Codes), Bytes0),
		base32::parse(codes(Codes), Bytes).

	test(base32_roundtrip_02, deterministic(Bytes == Bytes0)) :-
		atom_codes('The quick brown fox jumps over the lazy dog.', Bytes0),
		base32::generate(atom(Atom), Bytes0),
		base32::parse(atom(Atom), Bytes).

	test(base32_generate_2_chars, deterministic(Chars == ['M','Z','X','W','6','Y','T','B','O','I','=','=','=','=','=','='])) :-
		atom_codes(foobar, Bytes),
		base32::generate(chars(Chars), Bytes).

	test(base32_parse_2_codes, deterministic(Atom == foobar)) :-
		atom_codes('MZXW6YTBOI======', Codes),
		base32::parse(codes(Codes), Bytes),
		atom_codes(Atom, Bytes).

	test(base32_parse_2_lowercase, deterministic(Atom == foobar)) :-
		base32::parse(atom('mzxw6ytboi======'), Bytes),
		atom_codes(Atom, Bytes).

:- end_object.
