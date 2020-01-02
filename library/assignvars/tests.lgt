%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.01,
		author is 'Paulo Moura',
		date is 2019/09/05,
		comment is 'Unit tests for the "assignvars" library.'
	]).

	:- uses(assignvars, [
		assignable/1, assignable/2,
		op(100, xfx, '<='), ('<=')/2,
		op(100, xfx, '=>'), ('=>')/2
	]).

	cover(assignvars).

	% assignable/1

	test(assignvars_assignable_1_01, deterministic(nonvar(Assignable))) :-
		assignable(Assignable).

	test(assignvars_assignable_1_02, error(type_error(variable,a))) :-
		assignable(a).

	% assignable/2

	test(assignvars_assignable_2_01, deterministic) :-
		assignable(_, 1).

	test(assignvars_assignable_2_02, error(type_error(variable,a))) :-
		assignable(a, 1).

	% '<='/2

	test(assignvars_set_2_01, deterministic(nonvar(Assignable))) :-
		assignable(Assignable),
		Assignable <= 1.

	test(assignvars_set_2_02, deterministic) :-
		assignable(Assignable),
		Assignable <= 1,
		Assignable <= 2,
		Assignable <= 3.

	test(assignvars_set_2_03, deterministic) :-
		assignable(Assignable),
		Assignable <= 1,
		once((	Assignable <= 2,
			fail
		;	true
		)).

	test(assignvars_set_2_04, error(instantiation_error)) :-
		assignable(Assignable),
		Assignable <= _.

	% '=>'/2

	test(assignvars_get_2_01, deterministic(Value == 1)) :-
		assignable(Assignable),
		Assignable <= 1,
		Assignable => Value.

	test(assignvars_get_2_02, deterministic(Value == 3)) :-
		assignable(Assignable),
		Assignable <= 1,
		Assignable <= 2,
		Assignable <= 3,
		Assignable => Value.

	test(assignvars_get_2_03, deterministic(Value == 1)) :-
		assignable(Assignable),
		Assignable <= 1,
		once((	Assignable <= 2,
			fail
		;	true
		)),
		Assignable => Value.

	test(assignvars_get_2_04, error(instantiation_error)) :-
		_ => _.

:- end_object.
