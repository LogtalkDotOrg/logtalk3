%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
		date is 2022-06-01,
		comment is 'Unit tests for the database built-in methods that take a clause reference argument.'
	]).

	test(asserta_2_01, true(X == 0)) :-
		test_object::asserta(a(0), _),
		test_object::a(X).

	test(asserta_2_02, true(nonvar(Ref))) :-
		test_object::asserta(a(1), Ref).

	test(asserta_2_03, true(Ref1 \== Ref2)) :-
		test_object::asserta(a(2), Ref1),
		test_object::asserta(a(2), Ref2).

	test(asserta_2_04, true(Ref1 \== Ref2)) :-
		test_object::asserta(a(3), Ref1),
		test_object::asserta(a(4), Ref2).

	test(asserta_2_05, true(X == 0)) :-
		test_object::asserta((aa(N) :- N = 0), _),
		test_object::aa(X).

	test(asserta_2_06, true(nonvar(Ref))) :-
		test_object::asserta((aa(N) :- N = 1), Ref).

	test(asserta_2_07, true(Ref1 \== Ref2)) :-
		test_object::asserta((aa(N) :- N = 2), Ref1),
		test_object::asserta((aa(N) :- N = 2), Ref2).

	test(asserta_2_08, true(Ref1 \== Ref2)) :-
		test_object::asserta((aa(N) :- N = 3), Ref1),
		test_object::asserta((aa(N) :- N = 4), Ref2).

	test(asserta_2_09, error(uninstantiation_error(_))) :-
		test_object::asserta(a(5), ref).

	test(asserta_2_10, error(uninstantiation_error(_))) :-
		test_object::asserta((aa(N) :- N = 5), ref).

	test(assertz_2_01, true(X == 0)) :-
		test_object::assertz(z(0), _),
		test_object::z(X).

	test(assertz_2_02, true(nonvar(Ref))) :-
		test_object::assertz(z(1), Ref).

	test(assertz_2_03, true(Ref1 \== Ref2)) :-
		test_object::assertz(z(2), Ref1),
		test_object::assertz(z(2), Ref2).

	test(assertz_2_04, true(Ref1 \== Ref2)) :-
		test_object::assertz(z(3), Ref1),
		test_object::assertz(z(4), Ref2).

	test(assertz_2_05, true(X == 0)) :-
		test_object::assertz((zz(N) :- N = 0), _),
		test_object::zz(X).

	test(assertz_2_06, true(nonvar(Ref))) :-
		test_object::assertz((zz(N) :- N = 1), Ref).

	test(assertz_2_07, true(Ref1 \== Ref2)) :-
		test_object::assertz((zz(N) :- N = 2), Ref1),
		test_object::assertz((zz(N) :- N = 2), Ref2).

	test(assertz_2_08, true(Ref1 \== Ref2)) :-
		test_object::assertz((zz(N) :- N = 3), Ref1),
		test_object::assertz((zz(N) :- N = 4), Ref2).

	test(assertz_2_09, error(uninstantiation_error(_))) :-
		test_object::assertz(z(5), ref).

	test(assertz_2_10, error(uninstantiation_error(_))) :-
		test_object::assertz((zz(N) :- N = 5), ref).

	test(clause_3_01, true((Y == 1, Body == true))) :-
		test_object::assertz(y(1), Ref),
		test_object::clause(y(Y), Body, Ref).

	test(clause_3_02, true(Ref1 == Ref2)) :-
		test_object::assertz(y(2), Ref1),
		test_object::clause(y(2), true, Ref2).

	test(clause_3_03, true(nonvar(Ref))) :-
		test_object::assertz(y(3), _),
		test_object::clause(y(_), _, Ref).

	test(clause_3_04, true(Ref1 == Ref2)) :-
		test_object::assertz((yy(N) :- N = 2), Ref1),
		test_object::clause(yy(2), N = 2, Ref2).

	test(clause_3_05, true(nonvar(Ref))) :-
		test_object::assertz((yy(N) :- N = 3), _),
		test_object::clause(yy(_), N = 3, Ref).

	test(clause_3_06, false) :-
		test_object::assertz(y(4), Ref),
		test_object::assertz(y(5), _),
		test_object::clause(y(5), _, Ref).

	test(clause_3_07, true(Head-Body == y(6)-true)) :-
		test_object::assertz(y(6), Ref),
		test_object::clause(Head, Body, Ref).

	test(clause_3_08, error(type_error(_, 3.14))) :-
		test_object::assertz(y(7), _),
		test_object::clause(y(_), _, 3.14).

	test(erase_1_01, true(L == [])) :-
		test_object::assertz(b(1), Ref),
		erase(Ref),
		findall(X, test_object::b(X), L).

	test(erase_1_02, true(L == [3])) :-
		test_object::assertz(b(2), Ref),
		test_object::assertz(b(3), _),
		erase(Ref),
		findall(X, test_object::b(X), L).

	test(erase_1_03, error(instantiation_error)) :-
		erase(_).

	test(erase_1_04, error(type_error(_, 3.14))) :-
		erase(3.14).

	cleanup :-
		test_object::abolish(a/1),
		test_object::abolish(b/1),
		test_object::abolish(y/1),
		test_object::abolish(z/1).

:- end_object.
