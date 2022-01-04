%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		date is 2022-01-04,
		comment is 'Unit tests for the Prolog msb/1 built-in function.'
	]).

	% tests from the Logtalk portability work

	test(lgt_msb_1_01, true(N == 7)) :-
		{N is msb(128)}.

	test(lgt_msb_1_02, true(N == 6)) :-
		{N is msb(127)}.

	test(lgt_msb_1_03, true(N == 2)) :-
		{N is msb(4)}.

	test(lgt_msb_1_04, true(N == 1)) :-
		{N is msb(3)}.

	test(lgt_msb_1_05, true(N == 1)) :-
		{N is msb(2)}.

	test(lgt_msb_1_06, true(N == 0)) :-
		{N is msb(1)}.

	test(lgt_msb_1_07, true(N == 103), [condition(current_prolog_flag(bounded, false))]) :-
		number_chars(M, ['1','2','8','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0']),
		{N is msb(M)}.

	test(lgt_msb_1_08, true(N == 103), [condition(current_prolog_flag(bounded, false))]) :-
		number_chars(M, ['1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1','1']),
		{N is msb(M)}.

	test(lgt_msb_1_09, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_N is msb(N)}.

	test(lgt_msb_1_10, error(type_error(integer,3.14))) :-
		% try to delay the error to runtime
		{_N is msb(3.14)}.

	test(lgt_msb_1_11, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_N is msb(Foo)}.

	test(lgt_msb_1_12, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_N is msb(Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

:- end_object.
