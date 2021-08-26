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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-08-26,
		comment is 'Unit tests for the ISO Prolog standard atom_concat/3 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.2.4

	test(iso_atom_concat_3_01, true(S3 == 'hello world')) :-
		{atom_concat('hello', ' world', S3)}.

	test(iso_atom_concat_3_02, true(S1 == 'small')) :-
		{atom_concat(S1, ' world', 'small world')}.

	test(iso_atom_concat_3_03, false) :-
		{atom_concat('hello',' world', 'small world')}.

	test(iso_atom_concat_3_04, true(L == [''-'hello', 'h'-'ello', 'he'-'llo', 'hel'-'lo', 'hell'-'o', 'hello'-''])) :-
		findall(T1-T2, {atom_concat(T1, T2, 'hello')}, L).

	test(iso_atom_concat_3_05, error(instantiation_error)) :-
		{atom_concat(small, _V2, _V4)}.

	test(eddbali_atom_concat_3_06, error(instantiation_error)) :-
		{atom_concat(_A, 'iso', _C)}.

	test(eddbali_atom_concat_3_07, error(instantiation_error)) :-
		{atom_concat('iso', _B, _C)}.

	test(eddbali_atom_concat_3_08, error(type_error(atom,f(a)))) :-
		{atom_concat(f(a), 'iso', _C)}.

	test(eddbali_atom_concat_3_09, error(type_error(atom,f(a)))) :-
		{atom_concat('iso', f(a), _C)}.

	test(eddbali_atom_concat_3_10, error(type_error(atom,f(a)))) :-
		{atom_concat(_A, _B, f(a))}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	% the following four tests are disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8 for
	% all Logtalk supported backend Prolog compilers

	- test(sics_atom_concat_3_11, true(N == 'Bartók Béla')) :-
		{atom_concat('Bartók ', 'Béla', N)}.

	- test(sics_atom_concat_3_12, true(N == 'Bartók ')) :-
		{atom_concat(N, 'Béla', 'Bartók Béla')}.

	- test(sics_atom_concat_3_13, true(N == 'Béla')) :-
		{atom_concat('Bartók ', N, 'Bartók Béla')}.

	- test(sics_atom_concat_3_14, true(L == [''-'Pécs', 'P'-'écs', 'Pé'-'cs', 'Péc'-'s', 'Pécs'-''])) :-
		findall(T1-T2, {atom_concat(T1, T2, 'Pécs')}, L).

	% tests from the Logtalk portability work

	test(lgt_atom_concat_3_15, true(A == '')) :-
		{atom_concat(A, '.', '.')}.

	test(lgt_atom_concat_3_16, true(A == '')) :-
		{atom_concat('.', A, '.')}.

	test(lgt_atom_concat_3_17, true) :-
		{atom_concat(a, b, ab)}.

	test(lgt_atom_concat_3_18, false) :-
		{atom_concat(a, _, bc)}.

:- end_object.
