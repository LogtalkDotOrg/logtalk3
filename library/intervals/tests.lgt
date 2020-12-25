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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-05-21,
		comment is 'Unit tests for the "intervals" library.'
	]).

	:- uses(interval, [
		new/3, valid/1,
		before/2, after/2,
		meets/2, met_by/2,
		overlaps/2, overlapped_by/2,
		starts/2, started_by/2,
		during/2, contains/2,
		finishes/2, finished_by/2,
		equal/2
	]).

	cover(interval).

	% new/3 tests

	test(interval_new_3_01, deterministic) :-
		new(1, 3, _).

	test(interval_new_3_02, fail) :-
		new(1, 1, _).

	test(interval_new_3_03, fail) :-
		new(3, 1, _).

	% valid/1 tests

	test(interval_valid_1_01, deterministic) :-
		new(1, 3, Interval),
		valid(Interval).

	% before/2 tests

	test(interval_before_2_01, deterministic) :-
		new(1, 3, Interval1),
		new(5, 7, Interval2),
		before(Interval1, Interval2).

	test(interval_before_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		before(Interval1, Interval2).

	test(interval_before_2_03, fail) :-
		new(1, 3, Interval1),
		new(0, 1, Interval2),
		before(Interval1, Interval2).

	% after/2 tests

	test(interval_after_2_01, deterministic) :-
		new(5, 7, Interval1),
		new(1, 3, Interval2),
		after(Interval1, Interval2).

	test(interval_after_2_02, fail) :-
		new(2, 4, Interval1),
		new(1, 3, Interval2),
		after(Interval1, Interval2).

	test(interval_after_2_03, fail) :-
		new(0, 1, Interval1),
		new(1, 3, Interval2),
		after(Interval1, Interval2).

	% meets/2 tests

	test(interval_meets_2_01, deterministic) :-
		new(1, 3, Interval1),
		new(3, 5, Interval2),
		meets(Interval1, Interval2).

	test(interval_meets_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		meets(Interval1, Interval2).

	% met_by/2 tests

	test(interval_met_by_2_01, deterministic) :-
		new(3, 5, Interval1),
		new(1, 3, Interval2),
		met_by(Interval1, Interval2).

	test(interval_met_by_2_02, fail) :-
		new(2, 4, Interval1),
		new(1, 3, Interval2),
		met_by(Interval1, Interval2).

	% overlaps/2 tests

	test(interval_overlaps_2_01, deterministic) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		overlaps(Interval1, Interval2).

	test(interval_overlaps_2_02, fail) :-
		new(1, 3, Interval1),
		new(4, 6, Interval2),
		overlaps(Interval1, Interval2).

	test(interval_overlaps_2_03, fail) :-
		new(4, 6, Interval1),
		new(1, 3, Interval2),
		overlaps(Interval1, Interval2).

	% overlapped_by/2 tests

	test(interval_overlapped_by_2_01, deterministic) :-
		new(2, 4, Interval1),
		new(1, 3, Interval2),
		overlapped_by(Interval1, Interval2).

	test(interval_overlapped_by_2_02, fail) :-
		new(4, 6, Interval1),
		new(1, 3, Interval2),
		overlapped_by(Interval1, Interval2).

	test(interval_overlapped_by_2_03, fail) :-
		new(1, 3, Interval1),
		new(4, 6, Interval2),
		overlapped_by(Interval1, Interval2).

	% starts/2 tests

	test(interval_starts_2_01, deterministic) :-
		new(1, 2, Interval1),
		new(1, 3, Interval2),
		starts(Interval1, Interval2).

	test(interval_starts_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 3, Interval2),
		starts(Interval1, Interval2).

	% started_by/2 tests

	test(interval_started_by_2_01, deterministic) :-
		new(1, 3, Interval1),
		new(1, 2, Interval2),
		started_by(Interval1, Interval2).

	test(interval_started_by_2_02, fail) :-
		new(2, 3, Interval1),
		new(1, 3, Interval2),
		started_by(Interval1, Interval2).

	% during/2 tests

	test(interval_during_2_01, deterministic) :-
		new(2, 4, Interval1),
		new(1, 5, Interval2),
		during(Interval1, Interval2).

	test(interval_during_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		during(Interval1, Interval2).

	% contains/2 tests

	test(interval_contains_2_01, deterministic) :-
		new(1, 5, Interval1),
		new(2, 4, Interval2),
		contains(Interval1, Interval2).

	test(interval_contains_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		contains(Interval1, Interval2).

	% finishes/2 tests

	test(interval_finishes_2_01, deterministic) :-
		new(2, 3, Interval1),
		new(1, 3, Interval2),
		finishes(Interval1, Interval2).

	test(interval_finishes_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		finishes(Interval1, Interval2).

	% finished_by/2 tests

	test(interval_finished_by_2_01, deterministic) :-
		new(1, 3, Interval1),
		new(2, 3, Interval2),
		finished_by(Interval1, Interval2).

	test(interval_finished_by_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		finished_by(Interval1, Interval2).

	% equal/2 tests

	test(interval_equal_2_01, deterministic) :-
		new(1, 3, Interval1),
		new(1, 3, Interval2),
		equal(Interval1, Interval2).

	test(interval_equal_2_02, fail) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		equal(Interval1, Interval2).

:- end_object.
