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
		date is 2021-06-08,
		comment is 'Unit tests for the "patching" example.'
	]).

	cover(broken).
	cover(metaclass).
	cover(broken_class).
	cover(instance).
	cover(patch).

	test(patching_01, true(Objects == [broken, broken_class])) :-
		setof(Object, complements_object(patch, Object), Objects).

	test(patching_02, true) :-
		broken::is_proper_list([1,2,3]).

	test(patching_03, true) :-
		instance::is_proper_list([1,2,3]).

	test(patching_04, false) :-
		broken::is_proper_list(_).

	test(patching_05, false) :-
		instance::is_proper_list(_).

	test(patching_06, false) :-
		broken::is_proper_list([a,b,c|_]).

	test(patching_07, false) :-
		instance::is_proper_list([a,b,c|_]).

	test(patching_08, error(permission_error(access, private_predicate, last/3))) :-
		broken::last(_, _, _).

	test(patching_09, error(permission_error(access, private_predicate, last/3))) :-
		instance::last(_, _, _).

	test(patching_10, true) :-
		broken::nextto(2, 3, [1,2,3]).

:- end_object.
