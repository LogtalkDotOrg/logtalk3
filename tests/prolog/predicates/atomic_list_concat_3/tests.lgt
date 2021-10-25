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
		author is 'Paul Brown and Paulo Moura',
		date is 2021-10-25,
		comment is 'Unit tests for the atomic_list_concat/3 predicate.'
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	quick_check(atomic_list_concat_property, atomic_list_concat(+list(atomic), +atomic, -atom)).

	test(atomic_list_concat_three_elements, deterministic(Atom == 'a_42_c')) :-
		atomic_list_concat([a, 42, c], '_', Atom).

	test(atomic_list_concat_two_elements, deterministic(Atom == 'a_42')) :-
		atomic_list_concat([a, 42], '_', Atom).

	test(atomic_list_concat_one_element, deterministic(Atom == 'a')) :-
		atomic_list_concat([a], '_', Atom).

	test(atomic_list_concat_empty, deterministic(Atom == '')) :-
		atomic_list_concat([], '-', Atom).

	test(atomic_list_concat_var_head, error(instantiation_error)) :-
		atomic_list_concat([_, bar], '_', _).

	test(atomic_list_concat_var_tail, error(instantiation_error)) :-
		atomic_list_concat([foo, bar| _], '_', _).

	test(atomic_list_concat_var_second, error(instantiation_error)) :-
		atomic_list_concat([foo, bar], _, _).

	test(atomic_list_concat_non_atomic_first, error(type_error(atomic,a(1)))) :-
		atomic_list_concat([a(1), bar], '_', _).

	test(atomic_list_concat_non_atomic_second, error(type_error(atomic,a(1)))) :-
		atomic_list_concat([foo, bar], a(1), _).

:- end_object.
