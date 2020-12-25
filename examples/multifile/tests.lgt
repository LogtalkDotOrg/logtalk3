%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:4:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2017-01-05,
		comment is 'Unit tests for the "multifile" example.'
	]).

	cover(main).
	cover(other).
	cover(more).

	test(multifile_01) :-
		findall(X, main::a(X), Solutions),
		Solutions == [1, 2, 3, 4, 5].

	test(multifile_02) :-
		main::current_predicate(a/1),
		main::predicate_property(a(_), public),
		main::predicate_property(a(_), multifile),
		main::predicate_property(a(_), number_of_clauses(5)).

	test(multifile_03) :-
		findall(X, main::b(X), Solutions),
		Solutions == [one, two, three].

	test(multifile_04) :-
		main::current_predicate(b/1),
		main::predicate_property(b(_), public),
		main::predicate_property(b(_), multifile),
		main::predicate_property(b(_), number_of_clauses(2)).

	test(multifile_05) :-
		object_property(main, number_of_clauses(N)),
		N == 2.

	test(multifile_06) :-
		object_property(main, number_of_user_clauses(N)),
		N == 2.

	test(multifile_07) :-
		object_property(main, number_of_rules(N)),
		N == 0.

	test(multifile_08) :-
		object_property(main, number_of_user_rules(N)),
		N == 0.

:- end_object.
