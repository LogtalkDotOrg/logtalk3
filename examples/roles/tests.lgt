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
		version is 1:1:1,
		author is 'Paulo Moura',
		date is 2020-10-20,
		comment is 'Unit tests for the "roles" example.'
	]).

	test(roles_01) :-
		prototype::foo(Foo),
		Foo == 1.

	test(roles_02) :-
		descendant::foo(Foo),
		Foo == 2.

	test(roles_03) :-
		descendant::bar(X, Y),
		X == 1, Y == 2.

	test(roles_04) :-
		superclass::foo(Foo),
		Foo == 1.

	test(roles_05) :-
		\+ subclass::current_predicate(_).

	test(roles_06) :-
		findall(P, instance::current_predicate(P), Ps),
		Ps == [bar/2, foo/1].

	test(roles_07) :-
		instance::foo(Foo),
		Foo == 2.

	test(roles_08) :-
		instance::bar(X, Y),
		X == 1, Y == 2.

	test(roles_09) :-
		empty_instance::foo(Foo),
		Foo == 1.

:- end_object.
