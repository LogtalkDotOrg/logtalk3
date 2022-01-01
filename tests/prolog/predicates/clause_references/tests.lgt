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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-12-22,
		comment is 'Unit tests for the de facto standard Prolog built-in predicates that take a clause reference argument.'
	]).

	:- uses(user, [
		a/1, b/1, y/1, z/1
	]).

	test(asserta_2_01, true(X == 0)) :-
		asserta(a(0), _),
		a(X).

	test(asserta_2_02, true(nonvar(Ref))) :-
		asserta(a(1), Ref).

	test(asserta_2_03, true(Ref1 \== Ref2)) :-
		asserta(a(2), Ref1),
		asserta(a(2), Ref2).

	test(asserta_2_04, true(Ref1 \== Ref2)) :-
		asserta(a(3), Ref1),
		asserta(a(4), Ref2).

	test(asserta_2_05, error(uninstantiation_error(_))) :-
		asserta(a(5), ref).

	test(assertz_2_01, true(X == 0)) :-
		assertz(z(0), _),
		z(X).

	test(assertz_2_02, true(nonvar(Ref))) :-
		assertz(z(1), Ref).

	test(assertz_2_03, true(Ref1 \== Ref2)) :-
		assertz(z(2), Ref1),
		assertz(z(2), Ref2).

	test(assertz_2_04, true(Ref1 \== Ref2)) :-
		assertz(z(3), Ref1),
		assertz(z(4), Ref2).

	test(assertz_2_05, error(uninstantiation_error(_))) :-
		assertz(a(5), ref).

	test(clause_3_01, true((Y == 1, Body == true))) :-
		assertz(y(1), Ref),
		clause(y(Y), Body, Ref).

	test(clause_3_02, true(Ref1 == Ref2)) :-
		assertz(y(2), Ref1),
		clause(y(2), true, Ref2).

	test(clause_3_03, true(nonvar(Ref))) :-
		assertz(y(3), _),
		clause(y(_), _, Ref).

	test(clause_3_04, false) :-
		assertz(y(4), Ref),
		assertz(y(5), _),
		clause(y(5), _, Ref).

	test(clause_3_05, error(instantiation_error)) :-
		assertz(y(6), Ref),
		clause(_, _, Ref).

	test(clause_3_06, error(type_error(_, 3.14))) :-
		assertz(y(7), _),
		clause(y(_), _, 3.14).

	test(erase_1_01, true(L == [])) :-
		assertz(b(1), Ref),
		erase(Ref),
		findall(X, b(X), L).

	test(erase_1_02, true(L == [3])) :-
		assertz(b(2), Ref),
		assertz(b(3), _),
		erase(Ref),
		findall(X, b(X), L).

	test(erase_1_03, error(instantiation_error)) :-
		erase(_).

	test(erase_1_04, error(type_error(_, 3.14))) :-
		erase(3.14).

	cleanup :-
		abolish(a/1),
		abolish(b/1),
		abolish(y/1),
		abolish(z/1).

:- end_object.
