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
		date is 2021-10-25,
		comment is 'Unit tests for the atomic_concat/3 predicate.'
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	quick_check(atomic_concat_3_property, atomic_concat(+atomic, +atomic, -atom)).

	test(atomic_concat_3_atom_number, deterministic(Atom == 'a42')) :-
		atomic_concat(a, 42, Atom).

	test(atomic_concat_3_number_atom, deterministic(Atom == '42a')) :-
		atomic_concat(42, a, Atom).

	test(atomic_concat_3_empty, deterministic(Atom == '')) :-
		atomic_concat('', '', Atom).

	test(atomic_concat_3_var_first, error(instantiation_error)) :-
		atomic_concat(_, bar, _).

	test(atomic_concat_3_var_second, error(instantiation_error)) :-
		atomic_concat(foo, _, _).

	test(atomic_concat_3_non_atomic_first, error(type_error(atomic,a(1)))) :-
		atomic_concat(a(1), bar, _).

	test(atomic_concat_3_non_atomic_second, error(type_error(atomic,a(1)))) :-
		atomic_concat(foo, a(1), _).

:- end_object.
