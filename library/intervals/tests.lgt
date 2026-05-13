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
		date is 2026-05-13,
		comment is 'Unit tests for the "intervals" library.'
	]).

	:- uses(interval, [
		new/3, valid/1, relation/3,
		before/2, after/2,
		meets/2, met_by/2,
		overlaps/2, overlapped_by/2,
		starts/2, started_by/2,
		during/2, contains/2,
		finishes/2, finished_by/2,
		equal/2
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	cover(interval).
	cover(interval_algebra).
	cover(interval_relation_set).
	cover(interval_constraint_network).

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

	% relation/3 tests

	test(interval_relation_3_01, deterministic(Relation == before)) :-
		new(1, 3, Interval1),
		new(5, 7, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_02, deterministic(Relation == after)) :-
		new(5, 7, Interval1),
		new(1, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_03, deterministic(Relation == meets)) :-
		new(1, 3, Interval1),
		new(3, 5, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_04, deterministic(Relation == met_by)) :-
		new(3, 5, Interval1),
		new(1, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_05, deterministic(Relation == overlaps)) :-
		new(1, 3, Interval1),
		new(2, 4, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_06, deterministic(Relation == overlapped_by)) :-
		new(2, 4, Interval1),
		new(1, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_07, deterministic(Relation == starts)) :-
		new(1, 2, Interval1),
		new(1, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_08, deterministic(Relation == started_by)) :-
		new(1, 3, Interval1),
		new(1, 2, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_09, deterministic(Relation == during)) :-
		new(2, 4, Interval1),
		new(1, 5, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_10, deterministic(Relation == contains)) :-
		new(1, 5, Interval1),
		new(2, 4, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_11, deterministic(Relation == finishes)) :-
		new(2, 3, Interval1),
		new(1, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_12, deterministic(Relation == finished_by)) :-
		new(1, 3, Interval1),
		new(2, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_13, deterministic(Relation == equal)) :-
		new(1, 3, Interval1),
		new(1, 3, Interval2),
		relation(Interval1, Interval2, Relation).

	test(interval_relation_3_14, deterministic) :-
		findall(Interval, sample_interval(Interval), Intervals),
		forall(
			(member(Interval1, Intervals), member(Interval2, Intervals)),
			unique_relation(Interval1, Interval2)
		).

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

	% interval_algebra relation/1 tests

	test(interval_algebra_relation_1_01, deterministic(Relations == [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal])) :-
		findall(Relation, interval_algebra::relation(Relation), Relations).

	% interval_algebra converse/2 tests

	test(interval_algebra_converse_2_01, deterministic) :-
		forall(
			expected_converse(Relation, Converse),
			(interval_algebra::converse(Relation, Converse), interval_algebra::converse(Converse, Relation))
		).

	test(interval_algebra_converse_2_02, fail) :-
		interval_algebra::converse(before, before).

	% interval_algebra compose/3 tests

	test(interval_algebra_compose_3_01, deterministic(Relations == [before])) :-
		interval_algebra::compose(before, before, Relations).

	test(interval_algebra_compose_3_02, deterministic(Relations == [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal])) :-
		interval_algebra::compose(before, after, Relations).

	test(interval_algebra_compose_3_03, deterministic(Relations == [starts, started_by, equal])) :-
		interval_algebra::compose(met_by, meets, Relations).

	test(interval_algebra_compose_3_04, deterministic) :-
		findall(Relation, interval_algebra::relation(Relation), BaseRelations),
		forall(
			(member(Relation1, BaseRelations), member(Relation2, BaseRelations)),
			(	compose_is_well_formed(Relation1, Relation2),
				compose_respects_converse(Relation1, Relation2)
			)
		).

	test(interval_algebra_compose_3_05, deterministic) :-
		findall(Relation, interval_algebra::relation(Relation), BaseRelations),
		forall(
			member(Relation, BaseRelations),
			(	interval_algebra::compose(equal, Relation, [Relation]),
				interval_algebra::compose(Relation, equal, [Relation])
			)
		).

	% interval_relation_set relation_set/1 tests

	test(interval_relation_set_relation_set_1_01, deterministic) :-
		interval_relation_set::relation_set([]).

	test(interval_relation_set_relation_set_1_02, deterministic) :-
		interval_relation_set::relation_set([before, overlaps, during]).

	test(interval_relation_set_relation_set_1_03, fail) :-
		interval_relation_set::relation_set([overlaps, before]).

	test(interval_relation_set_relation_set_1_04, fail) :-
		interval_relation_set::relation_set([before, before]).

	% interval_relation_set empty/1 tests

	test(interval_relation_set_empty_1_01, deterministic(RelationSet == [])) :-
		interval_relation_set::empty(RelationSet).

	% interval_relation_set universal/1 tests

	test(interval_relation_set_universal_1_01, deterministic(RelationSet == [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal])) :-
		interval_relation_set::universal(RelationSet).

	% interval_relation_set singleton/2 tests

	test(interval_relation_set_singleton_2_01, deterministic(RelationSet == [before])) :-
		interval_relation_set::singleton(before, RelationSet).

	% interval_relation_set normalize/2 tests

	test(interval_relation_set_normalize_2_01, deterministic(RelationSet == [before, overlaps, during])) :-
		interval_relation_set::normalize([during, before, overlaps, during], RelationSet).

	test(interval_relation_set_normalize_2_02, fail) :-
		interval_relation_set::normalize([before, unknown], _).

	% interval_relation_set member/2 tests

	test(interval_relation_set_member_2_01, deterministic) :-
		interval_relation_set::member(overlaps, [before, overlaps, during]).

	test(interval_relation_set_member_2_02, fail) :-
		interval_relation_set::member(overlapped_by, [before, overlaps, during]).

	% interval_relation_set subset/2 tests

	test(interval_relation_set_subset_2_01, deterministic) :-
		interval_relation_set::subset([before, overlaps], [before, overlaps, during]).

	test(interval_relation_set_subset_2_02, fail) :-
		interval_relation_set::subset([before, overlapped_by], [before, overlaps, during]).

	% interval_relation_set intersection/3 tests

	test(interval_relation_set_intersection_3_01, deterministic(Intersection == [before])) :-
		interval_relation_set::intersection([before, during], [before, overlaps], Intersection).

	% interval_relation_set union/3 tests

	test(interval_relation_set_union_3_01, deterministic(Union == [before, overlaps, during])) :-
		interval_relation_set::union([before, during], [before, overlaps], Union).

	% interval_relation_set converse/2 tests

	test(interval_relation_set_converse_2_01, deterministic(ConverseSet == [after, overlapped_by, equal])) :-
		interval_relation_set::converse([before, overlaps, equal], ConverseSet).

	% interval_relation_set compose/3 tests

	test(interval_relation_set_compose_3_01, deterministic(Composition == [before])) :-
		interval_relation_set::compose([before], [before], Composition).

	test(interval_relation_set_compose_3_02, deterministic(Composition == [before, meets])) :-
		interval_relation_set::compose([before, meets], [starts, equal], Composition).

	test(interval_relation_set_compose_3_03, deterministic(Composition == [])) :-
		interval_relation_set::compose([], [before, meets], Composition).

	% interval_constraint_network network/1 tests

	test(interval_constraint_network_network_1_01, deterministic) :-
		interval_constraint_network::network(
			network([a, b], [constraint(a, b, [before, meets])])
		).

	test(interval_constraint_network_network_1_02, fail) :-
		interval_constraint_network::network(
			network([a, a], [])
		).

	% interval_constraint_network new/2 tests

	test(interval_constraint_network_new_2_01, deterministic) :-
		interval_constraint_network::new([a, b, c], Network),
		interval_constraint_network::network(Network).

	test(interval_constraint_network_new_2_02, fail) :-
		interval_constraint_network::new([a, a], _Network).

	% interval_constraint_network nodes/2 tests

	test(interval_constraint_network_nodes_2_01, deterministic(Nodes == [a, b, c])) :-
		interval_constraint_network::new([a, b, c], Network),
		interval_constraint_network::nodes(Network, Nodes).

	% interval_constraint_network relation/4 tests

	test(interval_constraint_network_relation_4_01, deterministic(RelationSet == [equal])) :-
		interval_constraint_network::new([a, b], Network),
		interval_constraint_network::relation(Network, a, a, RelationSet).

	test(interval_constraint_network_relation_4_02, deterministic(RelationSet == [before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal])) :-
		interval_constraint_network::new([a, b], Network),
		interval_constraint_network::relation(Network, a, b, RelationSet).

	% interval_constraint_network query tests

	test(interval_constraint_network_entails_4_01, deterministic) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::entails(Network1, a, b, [before, meets]).

	test(interval_constraint_network_entails_4_02, fail) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before, meets], Network1),
		interval_constraint_network::entails(Network1, a, b, [before]).

	test(interval_constraint_network_possible_4_01, deterministic) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before, meets], Network1),
		interval_constraint_network::possible(Network1, a, b, [meets, overlaps]).

	test(interval_constraint_network_possible_4_02, fail) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::possible(Network1, a, b, [overlaps]).

	test(interval_constraint_network_excluded_4_01, deterministic) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::excluded(Network1, a, b, [overlaps, during]).

	test(interval_constraint_network_contradiction_2_01, deterministic(Explanation == contradiction(a, c, propagated(b, [before], [before], [before])))) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::refine(Network2, a, c, [after], Network3),
		interval_constraint_network::path_consistency(Network3, Closure),
		interval_constraint_network::contradiction(Closure, Explanation).

	test(interval_constraint_network_contradiction_2_02, deterministic(Explanation == contradiction(a, b, direct([])))) :-
		interval_constraint_network::contradiction(network([a, b], [constraint(a, b, [])]), Explanation).

	test(interval_constraint_network_entails_5_01, deterministic(Explanation == direct([before]))) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::entails(Network1, a, b, [before, meets], Explanation).

	test(interval_constraint_network_entails_5_02, deterministic(Explanation == propagated(b, [before], [before], [before]))) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::path_consistency(Network2, Network3),
		interval_constraint_network::entails(Network3, a, c, [before, meets], Explanation).

	% interval_constraint_network refine/5 tests

	test(interval_constraint_network_refine_5_01, deterministic) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before, meets], Network1),
		interval_constraint_network::relation(Network1, a, b, [before, meets]),
		interval_constraint_network::relation(Network1, b, a, [after, met_by]).

	% interval_constraint_network consistent/1 tests

	test(interval_constraint_network_consistent_1_01, deterministic) :-
		interval_constraint_network::new([a, b], Network),
		interval_constraint_network::consistent(Network).

	test(interval_constraint_network_consistent_1_02, fail) :-
		interval_constraint_network::network(network([a, b], [constraint(a, b, [])])),
		interval_constraint_network::consistent(network([a, b], [constraint(a, b, [])])).

	% interval_constraint_network propagate/2 tests

	test(interval_constraint_network_propagate_2_01, deterministic(RelationSet == [before])) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::propagate(Network2, Network3),
		interval_constraint_network::relation(Network3, a, c, RelationSet).

	test(interval_constraint_network_propagate_2_02, fail) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::refine(Network2, a, c, [after], Network3),
		interval_constraint_network::propagate(Network3, _Closure).

	% interval_constraint_network propagate/3 tests

	test(interval_constraint_network_propagate_3_01, deterministic) :-
		interval_relation_set::universal(UniversalRelationSet),
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::propagate(Network2, _Closure, Changes),
		memberchk(change(a, c, UniversalRelationSet, [before], propagated(b, [before], [before], [before])), Changes).

	test(interval_constraint_network_propagation_triple_2_01, deterministic(Triple == triple(a, b, c))) :-
		interval_constraint_network::propagation_triple(change(a, c, [before, after], [before], propagated(b, [before], [before], [before])), Triple).

	test(interval_constraint_network_propagation_triples_2_01, deterministic(Triples == [triple(a, b, c)])) :-
		interval_relation_set::universal(UniversalRelationSet),
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::propagate(Network2, _Closure, Changes),
		memberchk(change(a, c, UniversalRelationSet, [before], propagated(b, [before], [before], [before])), Changes),
		interval_constraint_network::propagation_triples(Changes, Triples).

	% interval_constraint_network refine_propagate/5 tests

	test(interval_constraint_network_refine_propagate_5_01, deterministic(RelationSet == [before])) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine_propagate(Network0, a, b, [before], Network1),
		interval_constraint_network::refine_propagate(Network1, b, c, [before], Network2),
		interval_constraint_network::relation(Network2, a, c, RelationSet).

	test(interval_constraint_network_refine_propagate_5_02, fail) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine_propagate(Network0, a, b, [before], Network1),
		interval_constraint_network::refine_propagate(Network1, b, c, [before], Network2),
		interval_constraint_network::refine_propagate(Network2, a, c, [after], _Closure).

	% interval_constraint_network refine_propagate/6 tests

	test(interval_constraint_network_refine_propagate_6_01, deterministic) :-
		interval_relation_set::universal(UniversalRelationSet),
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine_propagate(Network0, a, b, [before], Network1, Changes1),
		memberchk(change(a, b, UniversalRelationSet, [before], refined([before])), Changes1),
		interval_constraint_network::refine_propagate(Network1, b, c, [before], _Closure, Changes2),
		memberchk(change(b, c, UniversalRelationSet, [before], refined([before])), Changes2),
		memberchk(change(a, c, UniversalRelationSet, [before], propagated(b, [before], [before], [before])), Changes2).

	% interval_constraint_network path_consistency/2 tests

	test(interval_constraint_network_path_consistency_2_01, deterministic(RelationSet == [before])) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::path_consistency(Network2, Network3),
		interval_constraint_network::relation(Network3, a, c, RelationSet).

	test(interval_constraint_network_path_consistency_2_02, deterministic) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::refine(Network2, a, c, [after], Network3),
		interval_constraint_network::path_consistency(Network3, Closure),
		interval_constraint_network::relation(Closure, a, c, []),
		\+ interval_constraint_network::consistent(Closure).

	% interval_constraint_network batch refine_propagate tests

	test(interval_constraint_network_refine_propagate_3_01, deterministic(RelationSet == [before])) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine_propagate(Network0, [constraint(a, b, [before]), constraint(b, c, [before])], Network1),
		interval_constraint_network::relation(Network1, a, c, RelationSet).

	test(interval_constraint_network_refine_propagate_4_01, deterministic) :-
		interval_relation_set::universal(UniversalRelationSet),
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine_propagate(Network0, [constraint(a, b, [before]), constraint(b, c, [before])], _Closure, Changes),
		memberchk(change(a, b, UniversalRelationSet, [before], refined([before])), Changes),
		memberchk(change(b, c, UniversalRelationSet, [before], refined([before])), Changes),
		memberchk(change(a, c, UniversalRelationSet, [before], propagated(b, [before], [before], [before])), Changes).

	test(interval_constraint_network_refine_propagate_3_02, fail) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine_propagate(Network0, [constraint(a, b, [before]), constraint(b, c, [before]), constraint(a, c, [after])], _Closure).

	% interval_constraint_network explanation list tests

	test(interval_constraint_network_entailment_explanations_5_01, deterministic(Explanations == [propagated(b, [before], [before], [before])])) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::path_consistency(Network2, Network3),
		interval_constraint_network::entailment_explanations(Network3, a, c, [before, meets], Explanations).

	test(interval_constraint_network_contradiction_explanations_2_01, deterministic) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::refine(Network2, a, c, [after], Network3),
		interval_constraint_network::path_consistency(Network3, Closure),
		interval_constraint_network::contradiction_explanations(Closure, Explanations),
		memberchk(contradiction(a, c, propagated(b, [before], [before], [before])), Explanations).

	% interval_constraint_network inspection and comparison tests

	test(interval_constraint_network_constraints_2_01, deterministic(Constraints == [constraint(a, b, [before]), constraint(a, c, [before]), constraint(b, c, [before])])) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::path_consistency(Network2, Network3),
		interval_constraint_network::constraints(Network3, Constraints).

	test(interval_constraint_network_subsumes_2_01, deterministic) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::subsumes(Network0, Network1).

	test(interval_constraint_network_subsumes_2_02, fail) :-
		interval_constraint_network::new([a, b], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::subsumes(Network1, Network0).

	test(interval_constraint_network_equivalent_2_01, deterministic) :-
		interval_constraint_network::new([a, b, c], Network0),
		interval_constraint_network::refine(Network0, a, b, [before], Network1),
		interval_constraint_network::refine(Network1, b, c, [before], Network2),
		interval_constraint_network::path_consistency(Network2, Network3),
		interval_constraint_network::new([a, b, c], Network4),
		interval_constraint_network::refine_propagate(Network4, [constraint(a, b, [before]), constraint(b, c, [before])], Network5),
		interval_constraint_network::equivalent(Network3, Network5).

	unique_relation(Interval1, Interval2) :-
		findall(Relation, relation(Interval1, Interval2, Relation), Relations),
		Relations = [_].

	expected_converse(before, after).
	expected_converse(meets, met_by).
	expected_converse(overlaps, overlapped_by).
	expected_converse(starts, started_by).
	expected_converse(during, contains).
	expected_converse(finishes, finished_by).
	expected_converse(equal, equal).

	compose_is_well_formed(Relation1, Relation2) :-
		interval_algebra::compose(Relation1, Relation2, Relations),
		Relations \== [],
		forall(member(Relation, Relations), interval_algebra::relation(Relation)),
		canonical_relations(Relations, CanonicalRelations),
		Relations == CanonicalRelations.

	compose_respects_converse(Relation1, Relation2) :-
		interval_algebra::compose(Relation1, Relation2, Relations),
		interval_algebra::converse(Relation1, Converse1),
		interval_algebra::converse(Relation2, Converse2),
		converse_relations(Relations, ConverseRelations0),
		canonical_relations(ConverseRelations0, ConverseRelations),
		interval_algebra::compose(Converse2, Converse1, ExpectedConverseRelations),
		ConverseRelations == ExpectedConverseRelations.

	converse_relations([], []).
	converse_relations([Relation| Relations], [Converse| Converses]) :-
		interval_algebra::converse(Relation, Converse),
		converse_relations(Relations, Converses).

	canonical_relations(Relations0, Relations) :-
		relation_order(Order),
		canonical_relations(Order, Relations0, Relations).

	canonical_relations([], _Relations0, []).
	canonical_relations([Relation| Order], Relations0, [Relation| Relations]) :-
		member(Relation, Relations0),
		!,
		canonical_relations(Order, Relations0, Relations).
	canonical_relations([_Relation| Order], Relations0, Relations) :-
		canonical_relations(Order, Relations0, Relations).

	relation_order([before, after, meets, met_by, overlaps, overlapped_by, starts, started_by, during, contains, finishes, finished_by, equal]).

	sample_interval(Interval) :-
		between(0, 7, Start),
		between(0, 7, End),
		Start < End,
		new(Start, End, Interval).

:- end_object.
