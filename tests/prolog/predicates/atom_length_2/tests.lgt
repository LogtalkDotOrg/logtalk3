%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		date is 2020-11-16,
		comment is 'Unit tests for the ISO Prolog standard atom_length/2 built-in predicate.'
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.1.4

	test(iso_atom_length_2_01, true(N == 17)) :-
		{atom_length('enchanted evening', N)}.

	test(iso_atom_length_2_02, true(N == 17)) :-
		{atom_length('enchanted\
 evening', N)}.

	test(iso_atom_length_2_03, true(N == 0)) :-
		{atom_length('', N)}.

	test(iso_atom_length_2_04, false) :-
		{atom_length('scarlet', 5)}.

	test(iso_atom_length_2_05, error(instantiation_error)) :-
		{atom_length(_Atom, 4)}.

	test(iso_atom_length_2_06, error(type_error(atom,1.23))) :-
		{atom_length(1.23, 4)}.

	test(iso_atom_length_2_07, error(type_error(integer,'4'))) :-
		{atom_length(atom, '4')}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_atom_length_2_08, error(domain_error(not_less_than_zero,-4))) :-
		{atom_length(atom, -4)}.

	% the following test is disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8
	% for all Logtalk supported backend Prolog compilers

	- test(sics_atom_length_2_09, true(N == 11)) :-
		{atom_length('Bartók Béla', N)}.

:- end_object.
