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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-07-14,
		comment is 'Unit tests for the ISO Prolog standard float_fractional_part/1 built-in predicate.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2
	]).

	% tests from the Logtalk portability work

	test(lgt_float_fractional_part_1_01, true(X =~= 0.0)) :-
		{X is float_fractional_part(9.0)}.

	test(lgt_float_fractional_part_1_02, true(X =~= 0.9)) :-
		{X is float_fractional_part(9.9)}.

	test(lgt_float_fractional_part_1_03, error(instantiation_error)) :-
		% try to delay the error to runtime
		variable(N),
		{_X is float_fractional_part(N)}.

	test(lgt_float_fractional_part_1_04, error(type_error(float,9))) :-
		% try to delay the error to runtime
		{_X is float_fractional_part(9)}.

	test(lgt_float_fractional_part_1_05, error(type_error(evaluable,foo/0))) :-
		% try to delay the error to runtime
		foo(0, Foo),
		{_X is float_fractional_part(Foo)}.

	test(lgt_float_fractional_part_1_06, error(type_error(evaluable,foo/1))) :-
		% try to delay the error to runtime
		foo(1, Foo),
		{_X is float_fractional_part(Foo)}.

	% auxiliary predicates used to delay errors to runtime

	variable(_).

	foo(0, foo).
	foo(1, foo(1)).

:- end_object.
