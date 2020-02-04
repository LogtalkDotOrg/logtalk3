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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2019-08-06,
		comment is 'Unit tests for the uses/1 built-in directive.'
	]).

	test(uses_1_01, true(List == [1,2,3])) :-
		findall(X, obj::p(X), List).

	test(uses_1_02, true(List == [1,2,3])) :-
		findall(X, obj1(list)::p(X), List).

	test(uses_1_03, true) :-
		obj2(integer)::p.

	test(uses_1_04, fail) :-
		obj2(atom)::p.

	% multiple aliases for the same object are allowed
	test(uses_1_05, true) :-
		create_object(_, [], [uses([o as a, o as b])], []).

	% repeated declarations of the same alias for the same object are not allowed
	test(uses_1_06, error(permission_error(repeat,object_alias,a))) :-
		create_object(_, [], [uses([o as a, o as a])], []).

	% redefining an alias to reference another object is not allowed
	test(uses_1_07, error(permission_error(modify,object_alias,a))) :-
		create_object(_, [], [uses([o1 as a, o2 as a])], []).

	% defining an alias of an alias is not allowed
	test(uses_1_08, error(permission_error(create,alias_alias,b))) :-
		create_object(_, [], [uses([o as a, a as b])], []).

:- end_object.
