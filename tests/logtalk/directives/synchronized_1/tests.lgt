%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- set_logtalk_flag(undefined_predicates, silent).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2025-07-10,
		comment is 'Unit tests for the synchronized/1 built-in directive.'
	]).

	% test all possible syntaxes for the directive

	:- private(a/0).
	:- synchronized(a/0).

	:- private((b/1, c/2)).
	:- synchronized((b/1, c/2)).

	:- private([d/3, e/4]).
	:- synchronized([d/3, e/4]).

	% test synchronized predicate calls

	:- public(s/1).
	:- synchronized(s/1).

	s(1).
	s(2).
	s(3).

	:- public(t/1).

	t(X) :-
		s(X).

	% calls to predicates declared synchronized but not defined
	% must fail instead of throwing an existence error

	:- synchronized(r/2).

	test(synchronized_1_01, true) :-
		predicate_property(a, private),
		predicate_property(a, synchronized).

	test(synchronized_1_02, true) :-
		predicate_property(b(_), private),
		predicate_property(b(_), synchronized),
		predicate_property(c(_,_), private),
		predicate_property(c(_,_), synchronized).

	test(synchronized_1_03, true) :-
		predicate_property(d(_,_,_), private),
		predicate_property(d(_,_,_), synchronized),
		predicate_property(e(_,_,_,_), private),
		predicate_property(e(_,_,_,_), synchronized).

	% when threads are not supported, the synchronized/1 directive simply makes
	% the predicates deterministic by wrapping its calls using once/1

	test(synchronized_1_04, true(L == [1])) :-
		findall(X, s(X), L).

	test(synchronized_1_05, true(L == [1])) :-
		findall(X, t(X), L).

	test(synchronized_1_06, false) :-
		r(_, _).

	% synchronized predicates can also be declared for dynamically created
	% objects and categories

	test(synchronized_1_07, true(Object::predicate_property(s(_), synchronized)), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [synchronized(s/1), public(s/1)], [(s(N) :- (N = 1; N = 2; N = 3))]).

	test(synchronized_1_08, true(L == [1]), [cleanup(abolish_object(Object))]) :-
		create_object(Object, [], [synchronized(s/1), public(s/1)], [(s(N) :- (N = 1; N = 2; N = 3))]),
		findall(X, Object::s(X), L).

	test(synchronized_1_09, true(Object::predicate_property(s(_), synchronized)), [cleanup((abolish_object(Object), abolish_category(Category)))]) :-
		create_category(Category, [], [synchronized(s/1), public(s/1)], [(s(N) :- (N = 1; N = 2; N = 3))]),
		create_object(Object, [imports(Category)], [], []).

	test(synchronized_1_10, true(L == [1]), [cleanup((abolish_object(Object), abolish_category(Category)))]) :-
		create_category(Category, [], [synchronized(s/1), public(s/1)], [(s(N) :- (N = 1; N = 2; N = 3))]),
		create_object(Object, [imports(Category)], [], []),
		findall(X, Object::s(X), L).

:- end_object.
