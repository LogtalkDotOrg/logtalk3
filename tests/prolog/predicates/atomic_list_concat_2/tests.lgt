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
		comment is 'Unit tests for the atomic_list_concat/2 predicate.'
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	quick_check(atomic_list_concat_2_property, atomic_list_concat(+list(atomic), -atom)).

	test(atomic_list_concat_2_empty, deterministic(Res == '')) :-
		atomic_list_concat([], Res).

	test(atomic_list_concat_2_long_empty_list, deterministic(Res == 'a1b+2.0[]c-3.5')) :-
		atomic_list_concat([a, 1, b, '', +, 2.0, [], c, -3.5], Res).

	test(atomic_list_concat_2_long_empty_curly, deterministic(Res == 'a1b+2.0{}c-3.5')) :-
		atomic_list_concat([a, 1, b, '', +, 2.0, {}, c, -3.5], Res).

	test(atomic_list_concat_2_var_head, error(instantiation_error)) :-
		atomic_list_concat([_, bar], foobar).

	test(atomic_list_concat_2_var_tail, error(instantiation_error)) :-
		atomic_list_concat([foo, bar| _], foobar).

	test(atomic_list_concat_2_non_atomic, error(type_error(atomic,a(1)))) :-
		atomic_list_concat([a(1), bar], foobar).

:- end_object.
