%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:6:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2024-04-29,
		comment is 'Unit tests for the "multifile" example.'
	]).

	cover(main).
	cover(other).
	cover(more).

	test(multifile_01, true(Solutions == [1, 2, 3, 4, 5])) :-
		findall(X, main::a(X), Solutions).

	test(multifile_02a, true) :-
		main::current_predicate(a/1).

	test(multifile_02b, true) :-
		main::predicate_property(a(_), public).

	test(multifile_02c, true) :-
		main::predicate_property(a(_), multifile).

	test(multifile_02d, true) :-
		main::predicate_property(a(_), number_of_clauses(5)).

	test(multifile_03, true(Solutions == [one, two, three])) :-
		findall(X, main::b(X), Solutions).

	test(multifile_04a, true) :-
		main::current_predicate(b/1).

	test(multifile_04b, true) :-
		main::predicate_property(b(_), public).

	test(multifile_04c, true) :-
		main::predicate_property(b(_), multifile).

	test(multifile_04d, true) :-
		main::predicate_property(b(_), number_of_clauses(2)).

	test(multifile_05, true(N == 3)) :-
		object_property(main, number_of_clauses(N)).

	test(multifile_06, true(N == 3)) :-
		object_property(main, number_of_user_clauses(N)).

	test(multifile_07, true(N == 1)) :-
		object_property(main, number_of_rules(N)).

	test(multifile_08, true(N == 1)) :-
		object_property(main, number_of_user_rules(N)).

	test(multifile_09, true(L ==[1-[2,3], end-[1,2,3]])) :-
		findall(X-Rest, phrase(main::nt(X), [1,2,3], Rest), L).

	test(multifile_10a, true) :-
		main::current_predicate(nt/3).

	test(multifile_10b, true) :-
		main::predicate_property(nt(_,_,_), public).

	test(multifile_10c, true) :-
		main::predicate_property(nt(_,_,_), multifile).

	test(multifile_10d, true(NT == nt//1)) :-
		main::predicate_property(nt(_,_,_), non_terminal(NT)).

	test(multifile_10e, true) :-
		main::predicate_property(nt(_,_,_), number_of_clauses(2)).

:- end_object.
