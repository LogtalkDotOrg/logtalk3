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
		comment is 'Unit tests for the multifile/1 built-in directive.'
	]).

	:- multifile(multifile_primary_object::m1/1).
	multifile_primary_object::m1(3).

	:- multifile(multifile_primary_object::m2/1).
	multifile_primary_object::m2(3).

	:- multifile(multifile_primary_category::n1/1).
	multifile_primary_category::n1(3).

	test(multifile_1_01, true) :-
		multifile_primary_object::predicate_property(m1(_), (multifile)),
		multifile_primary_object::predicate_property(m1(_), static).

	test(multifile_1_02, true(L == [1, 2, 3, 4, 5])) :-
		setof(X, multifile_primary_object::m1(X), L).

	test(multifile_1_03, true) :-
		multifile_primary_object::predicate_property(m2(_), (multifile)),
		multifile_primary_object::predicate_property(m2(_), (dynamic)).

	test(multifile_1_04, true(L == [1, 2, 3, 4, 5, 6])) :-
		setof(X, multifile_primary_object::m2(X), L).

	test(multifile_1_05, true) :-
		multifile_primary_object(_)::predicate_property(a(_,_), (multifile)),
		multifile_primary_object(_)::predicate_property(a(_,_), static).

	test(multifile_1_06, true(L == [1-0, 2-0, 3-0])) :-
		setof(X-Y, multifile_primary_object(0)::a(X,Y), L).

	test(multifile_1_07, true(Y == 1)) :-
		multifile_primary_object(1)::a(1, Y).

	test(multifile_1_08, true(Y == 1)) :-
		multifile_primary_object(Y)::a(1, 1).

	test(multifile_1_09, true(X == Y)) :-
		multifile_primary_object(X)::a(1, Y).

	test(multifile_1_10, true) :-
		multifile_other_object::predicate_property(n1(_), (multifile)),
		multifile_other_object::predicate_property(n1(_), static).

	test(multifile_1_11, true(L == [1, 2, 3, 4, 5])) :-
		setof(X, multifile_other_object::n1(X), L).

	test(multifile_1_12, true) :-
		multifile_other_object(_)::predicate_property(b(_, _), (multifile)),
		multifile_other_object(_)::predicate_property(b(_, _), static).

	test(multifile_1_13, true(L == [1-0, 2-0])) :-
		setof(X-Y, multifile_other_object(0)::b(X,Y), L).

	test(multifile_1_14, true(Y == 1)) :-
		multifile_other_object(1)::b(1, Y).

	test(multifile_1_15, true(Y == 1)) :-
		multifile_other_object(Y)::b(1, 1).

	test(multifile_1_16, true(X == Y)) :-
		multifile_other_object(X)::b(1, Y).

	test(multifile_1_17, true(L == [m2-6, n2-5])) :-
		findall(X-Y, multifile_other_object::db(X,Y), L).

	test(multifile_1_18, true(L == [a-2-0, b-2-0])) :-
		findall(X-Y-Z, multifile_other_object(_)::dbp(X,Y,Z), L).

	% tests for multifile meta-predicates

	test(multifile_1_19, true(X == 1)) :-
		multifile_primary_object::m3(f(X)).

	test(multifile_1_20, true) :-
		multifile_primary_object(1)::aa(int).

	test(multifile_1_21, false) :-
		multifile_primary_object(a)::aa(int).

	test(multifile_1_22, true(X == 1)) :-
		multifile_other_object::n3(f(X)).

	test(multifile_1_23, true) :-
		multifile_other_object(1)::bb(int).

	test(multifile_1_24, false) :-
		multifile_other_object(a)::bb(int).

	% auxiliary predicates

	f(1).

	int(T) :-
		integer(T).

:- end_object.
