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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-12-24,
		comment is 'Unit tests for the database built-in methods that take a clause reference argument.'
	]).

	test(asserta_2_01, true(X == 1)) :-
		test_object::asserta(a(1), _),
		test_object::a(X).

	test(asserta_2_02, true(Ref1 \== Ref2)) :-
		test_object::asserta(a(2), Ref1),
		test_object::asserta(a(2), Ref2).

	test(asserta_2_03, true(Ref1 \== Ref2)) :-
		test_object::asserta(a(3), Ref1),
		test_object::asserta(a(4), Ref2).

	test(assertz_2_01, true(X == 1)) :-
		test_object::assertz(z(1), _),
		test_object::z(X).

	test(assertz_2_02, true(Ref1 \== Ref2)) :-
		test_object::assertz(z(2), Ref1),
		test_object::assertz(z(2), Ref2).

	test(assertz_2_03, true(Ref1 \== Ref2)) :-
		test_object::assertz(z(3), Ref1),
		test_object::assertz(z(4), Ref2).

	test(clause_3_01, true((Y == 1, Body == true))) :-
		test_object::assertz(y(1), Ref),
		test_object::clause(y(Y), Body, Ref).

	test(clause_3_02, true(Ref1 == Ref2)) :-
		test_object::assertz(y(2), Ref1),
		test_object::clause(y(2), true, Ref2).

	test(erase_1_01, true(L == [])) :-
		test_object::assertz(b(1), Ref),
		erase(Ref),
		findall(X, test_object::b(X), L).

	test(erase_1_02, true(L == [3])) :-
		test_object::assertz(b(2), Ref),
		test_object::assertz(b(3), _),
		erase(Ref),
		findall(X, test_object::b(X), L).

	cleanup :-
		test_object::abolish(a/1),
		test_object::abolish(b/1),
		test_object::abolish(y/1),
		test_object::abolish(z/1).

:- end_object.
