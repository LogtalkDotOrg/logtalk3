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
		date is 2021-08-12,
		comment is 'Unit tests for the ISO Prolog standard number_chars/2 built-in predicate.'
	]).

	% tests from the ISO/IEC 13211-1:1995(E) standard, section 8.16.7.4

	test(iso_number_chars_2_01, true(L == ['3','3'])) :-
		{number_chars(33, L)}.

	test(iso_number_chars_2_02, true) :-
		{number_chars(33, ['3','3'])}.

	test(iso_number_chars_2_03, true(N == 33.0)) :-
		{number_chars(33.0, L), number_chars(N, L)}.

	test(iso_number_chars_2_04, true(X == 3.3)) :-
		{number_chars(X, ['3','.','3','E','+','0'])}.

	test(iso_number_chars_2_05, true) :-
		{number_chars(3.3, ['3'| _L])}.

	test(iso_number_chars_2_06, true(A == -25)) :-
		{number_chars(A, ['-','2','5'])}.

	test(iso_number_chars_2_07, true(A == 3)) :-
		{number_chars(A, ['\n',' ','3'])}.

	test(iso_number_chars_2_08, error(syntax_error(_))) :-
		{number_chars(_A, ['3',' '])}.

	test(iso_number_chars_2_09, true(A == 15)) :-
		{number_chars(A, ['0',x,f])}.

	test(iso_number_chars_2_10, true(A == 0'a)) :-
		{number_chars(A, ['0','''',a])}.

	test(iso_number_chars_2_11, true(A == 4.2)) :-
		{number_chars(A, ['4','.','2'])}.

	test(iso_number_chars_2_12, true(A == 4.2)) :-
		{number_chars(A, ['4','2','.','0','e','-','1'])}.

	% tests from the Prolog ISO conformance testing framework written by Péter Szabó and Péter Szeredi

	test(eddbali_number_chars_2_13, error(instantiation_error)) :-
		{number_chars(_A, _L)}.

	test(eddbali_number_chars_2_14, error(type_error(number,a))) :-
		{number_chars(a, _L)}.

	test(eddbali_number_chars_2_15, error(type_error(list,4))) :-
		{number_chars(_A, 4)}.

	test(eddbali_number_chars_2_16, error(type_error(character,2))) :-
		{number_chars(_A, ['4',2])}.

	test(sics_number_chars_2_17, error(instantiation_error)) :-
		{number_chars(_A, [a|_L])}.

	test(sics_number_chars_2_18, error(instantiation_error)) :-
		{number_chars(_A, [a,_L])}.

	test(sics_number_chars_2_19, true(X == 9)) :-
		{number_chars(X, [' ','0','o','1','1'])}.

	test(sics_number_chars_2_20, true(X == 17)) :-
		{number_chars(X, [' ','0','x','1','1'])}.

	test(sics_number_chars_2_21, true(X == 3)) :-
		{number_chars(X, [' ','0','b','1','1'])}.

	test(sics_number_chars_2_22, error(syntax_error(_))) :-
		{number_chars(_X, ['0','o','8'])}.

	test(sics_number_chars_2_23, error(syntax_error(_))) :-
		{number_chars(_X, ['0','b','2'])}.

	test(sics_number_chars_2_24, error(syntax_error(_))) :-
		{number_chars(_X, ['0','x','g'])}.

	% the following test is disabled as there is no portable
	% way to specify a supporting text encoding such as UTF-8
	% for all Logtalk supported backend Prolog compilers

	- test(sics_number_chars_2_25, error(syntax_error(_))) :-
		{number_chars(_X, ['á'])}.

	test(sics_number_chars_2_26, error(syntax_error(_))) :-
		{number_chars(_X, ['a'])}.

	test(sics_number_chars_2_27, error(syntax_error(_))) :-
		{number_chars(_X, ['0','x','0','.','0'])}.

	% tests from the Logtalk portability work

	test(lgt_number_chars_2_28, true(A-B == '4'-'2')) :-
		{number_chars(42, [A,B])}.

	test(lgt_number_chars_2_29, error(type_error(character,1))) :-
		{number_chars(1234, [1,2,3,4])}.

	% tests from (or derived from) the WG17 test suite

	test(wg17_number_chars_2_30, true) :-
		{number_chars(1.0e9, ['1',.,'0','E','9'])}.

	test(wg17_number_chars_2_31, true) :-
		{number_chars(1.0e9, ['1',.,'0','E','+','9'])}.

	test(wg17_number_chars_2_32, error(syntax_error(_))) :-
		{number_chars(1, [])}.

	test(wg17_number_chars_2_33, error(syntax_error(_))) :-
		{number_chars(_, ['3',.])}.

	test(wg17_number_chars_2_34, error(syntax_error(_))) :-
		{number_chars(_, ['0','B','1'])}.

	test(wg17_number_chars_2_35, error(syntax_error(_))) :-
		{number_chars(_, ['0','O','7'])}.

	test(wg17_number_chars_2_36, error(syntax_error(_))) :-
		{number_chars(_, ['0','X',f])}.

:- end_object.
