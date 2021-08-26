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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2021-08-26,
		comment is 'Unit tests for the ISO Prolog standard atom_chars/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.4.4

	test(iso_atom_chars_2_01, true(L == [])) :-
		{atom_chars('', L)}.

	test(iso_atom_chars_2_02, true(L == ['[',']'])) :-
		{atom_chars([], L)}.

	test(iso_atom_chars_2_03, true(L == [''''])) :-
		{atom_chars('''', L)}.

	test(iso_atom_chars_2_04, true(L == ['a','n','t'])) :-
		{atom_chars('ant', L)}.

	test(iso_atom_chars_2_05, true(Str == 'sop')) :-
		{atom_chars(Str, ['s','o','p'])}.

	test(iso_atom_chars_2_06, true(X == ['o','r','t','h'])) :-
		{atom_chars('North', ['N'| X])}.

	test(iso_atom_chars_2_07, false) :-
		{atom_chars('soap', ['s','o','p'])}.

	test(iso_atom_chars_2_08, error(instantiation_error)) :-
		{atom_chars(_X, _Y)}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_atom_chars_2_09, error(instantiation_error)) :-
		{atom_chars(_A, [a,_E,c])}.

	test(eddbali_atom_chars_2_10, error(instantiation_error)) :-
		{atom_chars(_A, [a,b|_L])}.

	test(eddbali_atom_chars_2_11, error(type_error(atom,f(a)))) :-
		{atom_chars(f(a), _L)}.

	test(eddbali_atom_chars_2_12, error(type_error(list,iso))) :-
		{atom_chars(_A, iso)}.

	test(eddbali_atom_chars_2_13, error(type_error(character,f(b)))) :-
		{atom_chars(_A, [a,f(b)])}.

	% the following two tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- test(sics_atom_chars_2_14, true(L == ['P','é','c','s'])) :-
		{atom_chars('Pécs', L)}.

	- test(sics_atom_chars_2_15, true(A == 'Pécs')) :-
		{atom_chars(A, ['P','é','c','s'])}.

	% tests from the Logtalk portability work

	test(lgt_atom_chars_2_16, error(type_error(character,1))) :-
		{atom_chars(abc, [1,2,3])}.

	test(lgt_atom_chars_2_17, true(Chars == [])) :-
		{atom_chars('', Chars)}.

	test(lgt_atom_chars_2_18, true(Atom == '')) :-
		{atom_chars(Atom, [])}.

	test(lgt_atom_chars_2_19, true) :-
		{atom_chars('', [])}.

	test(lgt_atom_chars_2_20, true(v(A,B,C) == v('A','B','C'))) :-
		{atom_chars('ABC', [A,B,C])}.

	test(lgt_atom_chars_2_21, true) :-
		{atom_chars('ABC', ['A','B','C'])}.

:- end_object.
