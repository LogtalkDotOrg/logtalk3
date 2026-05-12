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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-12,
		comment is 'Unit tests for the "derangements" library.'
	]).

	:- uses(list, [
		length/2
	]).

	cover(derangements).

	test(derangements_2_empty, deterministic(Derangements == [[]])) :-
		derangements::derangements([], Derangements).

	test(derangements_2_singleton, deterministic(Derangements == [])) :-
		derangements::derangements([a], Derangements).

	test(derangement_2_singleton_none, deterministic(Derangements == [])) :-
		findall(Derangement, derangements::derangement([a], Derangement), Derangements).

	test(derangements_2_of_three, deterministic(Derangements == [[b,c,a],[c,a,b]])) :-
		derangements::derangements([a,b,c], Derangements).

	test(derangement_2_all_of_three, true(Derangements == [[b,c,a],[c,a,b]])) :-
		findall(Derangement, derangements::derangement([a,b,c], Derangement), Derangements).

	test(derangements_3_lexicographic, deterministic(Derangements == [[a,c,b],[b,a,c]])) :-
		derangements::derangements([c,b,a], lexicographic, Derangements).

	test(derangement_3_lexicographic_all, true(Derangements == [[a,c,b],[b,a,c]])) :-
		findall(Derangement, derangements::derangement([c,b,a], lexicographic, Derangement), Derangements).

	test(distinct_derangements_2_with_duplicates, deterministic(Derangements == [[b,b,a,a]])) :-
		derangements::distinct_derangements([a,a,b,b], Derangements).

	test(distinct_derangement_2_with_duplicates, true(Derangements == [[b,b,a,a]])) :-
		findall(Derangement, derangements::distinct_derangement([a,a,b,b], Derangement), Derangements).

	test(distinct_derangements_3_lexicographic, deterministic(Derangements == [[b,b,a,a]])) :-
		derangements::distinct_derangements([a,a,b,b], lexicographic, Derangements).

	test(distinct_derangement_3_default_exists, exists(Derangement == [b,b,a,a])) :-
		derangements::distinct_derangement([a,a,b,b], default, Derangement).

	test(nth_derangement_3_second, deterministic(Derangement == [c,a,b])) :-
		derangements::nth_derangement([a,b,c], 1, Derangement).

	test(nth_derangement_4_default_second, deterministic(Derangement == [c,a,b])) :-
		derangements::nth_derangement([a,b,c], default, 1, Derangement).

	test(nth_derangement_4_lexicographic_second, deterministic(Derangement == [b,a,c])) :-
		derangements::nth_derangement([c,b,a], lexicographic, 1, Derangement).

	test(derangement_index_3_second, deterministic(Index == 1)) :-
		derangements::derangement_index([a,b,c], [c,a,b], Index).

	test(derangement_index_4_default_second, deterministic(Index == 1)) :-
		derangements::derangement_index([a,b,c], default, [c,a,b], Index).

	test(derangement_index_4_lexicographic_second, deterministic(Index == 1)) :-
		derangements::derangement_index([c,b,a], lexicographic, [b,a,c], Index).

	test(count_derangements_2_empty, deterministic(Count == 1)) :-
		derangements::count_derangements([], Count).

	test(count_derangements_2_of_four, deterministic(Count == 9)) :-
		derangements::count_derangements([a,b,c,d], Count).

	test(count_derangements_2_with_duplicates, deterministic(Count == 4)) :-
		derangements::count_derangements([a,a,b,b], Count).

	test(count_partial_derangements_3_zero_fixed_points, deterministic(Count == 9)) :-
		derangements::count_partial_derangements(0, [a,b,c,d], Count).

	test(count_partial_derangements_3_one_fixed_point, deterministic(Count == 8)) :-
		derangements::count_partial_derangements(1, [a,b,c,d], Count).

	test(count_partial_derangements_3_all_fixed_points, deterministic(Count == 1)) :-
		derangements::count_partial_derangements(4, [a,b,c,d], Count).

	test(count_partial_derangements_3_out_of_range, deterministic(Count == 0)) :-
		derangements::count_partial_derangements(5, [a,b,c,d], Count).

	test(count_partial_derangements_3_duplicates_zero_fixed_points, deterministic(Count == 4)) :-
		derangements::count_partial_derangements(0, [a,a,b,b], Count).

	test(count_partial_derangements_3_duplicates_all_fixed_points, deterministic(Count == 4)) :-
		derangements::count_partial_derangements(4, [a,a,b,b], Count).

	test(count_distinct_derangements_2_with_duplicates, deterministic(Count == 1)) :-
		derangements::count_distinct_derangements([a,a,b,b], Count).

	test(nth_distinct_derangement_3_first, deterministic(Derangement == [b,b,a,a])) :-
		derangements::nth_distinct_derangement([a,a,b,b], 0, Derangement).

	test(distinct_derangement_index_3_first, deterministic(Index == 0)) :-
		derangements::distinct_derangement_index([a,a,b,b], [b,b,a,a], Index).

	test(nth_derangement_derangement_index_roundtrip, deterministic(Index == 1)) :-
		derangements::nth_derangement([a,b,c], 1, Derangement),
		derangements::derangement_index([a,b,c], Derangement, Index).

	test(next_derangement_3, deterministic(Next == [b,a,c])) :-
		derangements::next_derangement([c,b,a], [a,c,b], Next).

	test(next_derangement_3_last, fail) :-
		derangements::next_derangement([c,b,a], [b,a,c], _).

	test(previous_derangement_3, deterministic(Previous == [a,c,b])) :-
		derangements::previous_derangement([c,b,a], [b,a,c], Previous).

	test(previous_derangement_3_first, fail) :-
		derangements::previous_derangement([c,b,a], [a,c,b], _).

	test(random_derangement_2_exists, true(derangements::derangement([a,b,c,d], Derangement))) :-
		derangements::random_derangement([a,b,c,d], Derangement).

	test(random_derangement_2_duplicates_exists, true(derangements::derangement([a,a,b,b], Derangement))) :-
		derangements::random_derangement([a,a,b,b], Derangement).

	test(random_derangement_2_none, fail) :-
		derangements::random_derangement([a], _).

	test(sample_derangements_3_zero, deterministic(Samples == [])) :-
		derangements::sample_derangements([a,b,c], 0, Samples).

	test(sample_derangements_3_duplicates_zero, deterministic(Samples == [])) :-
		derangements::sample_derangements([a,a,b,b], 0, Samples).

	test(sample_derangements_3_none, fail) :-
		derangements::sample_derangements([a], 1, _).

	test(sample_derangements_3_count, true((length(Samples, 3), forall(list::member(Sample, Samples), derangements::derangement([a,b,c,d], Sample))))) :-
		derangements::sample_derangements([a,b,c,d], 3, Samples).

	test(sample_derangements_3_duplicates_count, true((length(Samples, 3), forall(list::member(Sample, Samples), derangements::derangement([a,a,b,b], Sample))))) :-
		derangements::sample_derangements([a,a,b,b], 3, Samples).

	test(random_distinct_derangement_2_exists, true(derangements::distinct_derangement([a,a,b,b], Derangement))) :-
		derangements::random_distinct_derangement([a,a,b,b], Derangement).

	test(sample_distinct_derangements_3_zero, deterministic(Samples == [])) :-
		derangements::sample_distinct_derangements([a,a,b,b], 0, Samples).

	test(sample_distinct_derangements_3_count, true((length(Samples, 3), forall(list::member(Sample, Samples), derangements::distinct_derangement([a,a,b,b], Sample))))) :-
		derangements::sample_distinct_derangements([a,a,b,b], 3, Samples).

:- end_object.
