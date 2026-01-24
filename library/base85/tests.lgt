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
		comment is 'Unit tests for the "base85" library.'
	]).

	cover(base85).

	% Ascii85 test vectors

	test(base85_generate_2_empty, true(Base85 == '')) :-
		base85::generate(atom(Base85), []).

	test(base85_generate_2_man, true(Base85 == '9jqo^')) :-
		atom_codes('Man ', Bytes),
		base85::generate(atom(Base85), Bytes).

	test(base85_parse_2_empty, true(Bytes == [])) :-
		base85::parse(atom(''), Bytes).

	test(base85_parse_2_man, true(Atom == 'Man ')) :-
		base85::parse(atom('9jqo^'), Bytes),
		atom_codes(Atom, Bytes).

	test(base85_roundtrip_01, true(Bytes == Bytes0)) :-
		atom_codes('Hello, World!', Bytes0),
		base85::generate(codes(Codes), Bytes0),
		base85::parse(codes(Codes), Bytes).

	test(base85_roundtrip_02, true(Bytes == Bytes0)) :-
		atom_codes('Test message', Bytes0),
		base85::generate(atom(Atom), Bytes0),
		base85::parse(atom(Atom), Bytes).

	test(base85_zeros, true(Base85 == 'z')) :-
		base85::generate(atom(Base85), [0, 0, 0, 0]).

	test(base85_parse_zeros, true(Bytes == [0, 0, 0, 0])) :-
		base85::parse(atom('z'), Bytes).

	test(base85_generate_2_chars, true) :-
		atom_codes('test', Bytes),
		base85::generate(chars(Chars), Bytes),
		base85::parse(chars(Chars), Bytes2),
		Bytes == Bytes2.

	test(base85_parse_2_codes, true(Bytes == Bytes2)) :-
		atom_codes('test', Bytes),
		base85::generate(codes(Codes), Bytes),
		base85::parse(codes(Codes), Bytes2).

	% Test with delimiters
	test(base85_parse_with_delimiters, true(Atom == 'Man ')) :-
		base85::parse(atom('<~9jqo^~>'), Bytes),
		atom_codes(Atom, Bytes).

	test(base85_multiple_zeros, true(Base85 == 'zz')) :-
		base85::generate(atom(Base85), [0, 0, 0, 0, 0, 0, 0, 0]).

:- end_object.
