%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2024-09-20,
		comment is 'Unit tests for the ISO Prolog standard atom_codes/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.5.4

	test(iso_atom_codes_2_01, true(L == [])) :-
		{atom_codes('', L)}.

	test(iso_atom_codes_2_02, true(L == [0'[, 0']])) :-
		{atom_codes([], L)}.

	test(iso_atom_codes_2_03, true(L == [39])) :-
		{atom_codes('''', L)}.

	test(iso_atom_codes_2_04, true(L == [0'a, 0'n, 0't])) :-
		{atom_codes('ant', L)}.

	test(iso_atom_codes_2_05, true(Str == 'sop')) :-
		{atom_codes(Str, [0's,0'o,0'p])}.

	test(iso_atom_codes_2_06, true(X == [0'o,0'r,0't,0'h])) :-
		{atom_codes('North', [0'N| X])}.

	test(iso_atom_codes_2_07, false) :-
		{atom_codes('soap', [0's, 0'o, 0'p])}.

	test(iso_atom_codes_2_08, error(instantiation_error)) :-
		{atom_codes(_X, _Y)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_atom_codes_2_09, error(type_error(atom,f(a)))) :-
		{atom_codes(f(a), _L)}.

	test(eddbali_atom_codes_2_10, error(type_error(list,0'x))) :-
		{atom_codes(_, 0'x)}.

	test(eddbali_atom_codes_2_11, error(representation_error(character_code))) :-
		{atom_codes(_A, [0'i,0's,-1])}.

	% the following two tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- test(sics_atom_codes_2_12, true(C == [80,233,99,115]), [note('Requires Prolog portable solution to specify text encoding')]) :-
		{atom_codes('Pécs', C)}.

	- test(sics_atom_codes_2_13, true(A == 'Pécs'), [note('Requires Prolog portable solution to specify text encoding')]) :-
		{atom_codes(A, [80,233,99,115])}.

	% tests from the Logtalk portability work

	test(lgt_atom_codes_2_14, error(type_error(integer,a))) :-
		{atom_codes(abc, [a,b,c])}.

	test(lgt_atom_codes_2_15, true(Codes == [])) :-
		{atom_codes('', Codes)}.

	test(lgt_atom_codes_2_16, true(Atom == '')) :-
		{atom_codes(Atom, [])}.

	test(lgt_atom_codes_2_17, true) :-
		{atom_codes('', [])}.

	test(lgt_atom_codes_2_18, true(v(A,B,C) == v(65,66,67))) :-
		{atom_codes('ABC', [A,B,C])}.

	test(lgt_atom_codes_2_19, true) :-
		{atom_codes('ABC', [65,66,67])}.

	test(lgt_atom_codes_2_20, false) :-
		{atom_codes('ABC', [66| _])}.

	test(lgt_atom_codes_2_21, error(type_error(list,[66| 67]))) :-
		{atom_codes('ABC', [66| 67])}.

	test(lgt_atom_codes_2_22, error(type_error(list,[65,66,67| 'D']))) :-
		{atom_codes('ABC', [65,66,67| 'D'])}.

	test(lgt_atom_codes_2_23, error(type_error(integer, 'D'))) :-
		{atom_codes('ABC', [65,66,67,'D'])}.

	test(lgt_atom_codes_2_24, true(Tail == [])) :-
		{atom_codes('ABC', [65,66,67| Tail])}.

:- end_object.
