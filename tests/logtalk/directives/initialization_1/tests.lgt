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
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2021-05-09,
		comment is 'Unit tests for the initialization/1 built-in directive.'
	]).

	% allow declaring new predicates at runtime
	:- set_logtalk_flag(dynamic_declarations, allow).
	% avoid distracting warnings
	:- set_logtalk_flag(missing_directives, silent).
	:- set_logtalk_flag(unknown_predicates, silent).

	% multiple initialization/1 directives must be executed in order
	:- initialization(assertz(foo(1))).
	:- initialization(assertz(foo(2))).
	:- initialization(assertz(foo(3))).

	% verify that the foo/1 predicate is known
	test(initialization_1_01, true) :-
		current_predicate(foo/1).

	% verify that the foo/1 predicate is declared private
	test(initialization_1_02, true) :-
		predicate_property(foo(_), private).

	% verify that the foo/1 predicate is declared dynamic
	test(initialization_1_03, true) :-
		predicate_property(foo(_), (dynamic)).

	% verify initialization/1 directives goal execution order
	test(initialization_1_04, true(L == [1, 2, 3])) :-
		findall(X, foo(X), L).

	% test multiple initialization/1 directives in dynamically created objects
	test(initialization_1_05, true(L == [1, 2, 3])) :-
		create_object(Object, [], [
			public(o/1), dynamic(o/1),
			initialization(assertz(o(1))), initialization(assertz(o(2))), initialization(assertz(o(3)))
		], []),
		findall(N, Object::o(N), L).

:- end_object.
