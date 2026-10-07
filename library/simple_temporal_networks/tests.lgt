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
		date is 2026-10-07,
		comment is 'Unit tests for the "simple_temporal_networks" library.'
	]).

	:- uses(integer, [
		between/3
	]).

	:- uses(list, [
		memberchk/2, reverse/2
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	cover(stn).

	test(stn_earliest_schedule_correlated_bounds, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [
			constraint(zero, a, 10), constraint(a, zero, -5),
			constraint(zero, b, 20), constraint(b, zero, -1),
			constraint(b, a, -2),
			constraint(zero, c, 3), constraint(c, zero, -1)
		], STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, 5), time(b, 7), time(c, 1)]),
		stn::schedule(STN, [time(zero, 0), time(a, 5), time(b, 7), time(c, 3)]).

	test(stn_earliest_schedule_empty, deterministic) :-
		stn::new([], STN),
		stn::earliest_schedule(STN, [time(zero, 0)]).

	test(stn_earliest_schedule_lower_only, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, zero, -5), constraint(b, a, -2)], STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, 5), time(b, 7)]),
		stn::latest(STN, b, positive_infinity).

	test(stn_earliest_schedule_negative_times, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, zero, 5), constraint(b, a, -2)], STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, -5), time(b, -3)]).

	test(stn_earliest_schedule_mixed_numbers, deterministic(Time =~= -1.5)) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, zero, 1.5), constraint(zero, a, -1.5), constraint(b, zero, -2)], STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, Time), time(b, 2)]).

	test(stn_earliest_schedule_large_integer, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		Start is 2 ^ 60,
		NegativeStart is -Start,
		stn::new([a], STN0),
		stn::add_constraint(STN0, a, zero, NegativeStart, STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, Start)]).

	test(stn_earliest_schedule_labels_and_order, deterministic) :-
		stn::new([point(b), 1.0, 1], STN0),
		stn::add_constraints(STN0, [constraint(point(b), zero, -2), constraint(1.0, zero, -1), constraint(1, zero, -3)], STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(point(b), 2), time(1.0, 1), time(1, 3)]).

	test(stn_earliest_schedule_unbounded, fail) :-
		stn::new([a], STN),
		stn::earliest_schedule(STN, _).

	test(stn_earliest_schedule_disconnected_failure, deterministic) :-
		stn::new([bounded, a, b], STN0),
		stn::add_constraints(STN0, [constraint(bounded, zero, -2), constraint(b, a, -3)], STN),
		\+ stn::earliest_schedule(STN, Schedule),
		var(Schedule),
		stn::earliest(STN, bounded, 2),
		stn::earliest(STN, a, negative_infinity).

	test(stn_earliest_schedule_upper_only, fail) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, zero, a, 3, STN),
		stn::earliest_schedule(STN, _).

	test(stn_earliest_schedule_invalid_inputs, deterministic) :-
		\+ stn::earliest_schedule(not_a_state, _),
		\+ stn::earliest_schedule(STN, Schedule),
		var(STN),
		var(Schedule),
		\+ stn::earliest_schedule(stn([zero, Point], 1, [], Matrix), _),
		var(Point),
		var(Matrix).

	test(stn_earliest_schedule_incompatible_output, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, a, zero, -2, STN),
		\+ stn::earliest_schedule(STN, [time(zero, 0), time(a, 3)]),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, 2)]).

	test(stn_earliest_schedule_repeated_and_immutable, deterministic(First == Second)) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, a, zero, -2, STN),
		stn::constraints(STN, Sources),
		stn::earliest_schedule(STN, First),
		stn::earliest_schedule(STN, Second),
		stn::constraints(STN, Sources),
		stn::distance(STN, a, zero, -2, Sources),
		\+ stn::earliest_schedule(STN0, _).

	test(stn_earliest_schedule_point_extension, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, a, zero, -2, STN1),
		stn::add_time_points(STN1, [b], STN2),
		\+ stn::earliest_schedule(STN2, _),
		stn::add_constraint(STN2, b, zero, -1, STN),
		stn::earliest_schedule(STN, [time(zero, 0), time(a, 2), time(b, 1)]),
		stn::earliest_schedule(STN1, [time(zero, 0), time(a, 2)]).

	test(stn_earliest_schedule_source_retraction, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraints(STN0, [constraint(a, zero, -5), constraint(a, zero, -2)], [Strong, Weak], STN1),
		stn::earliest_schedule(STN1, [time(zero, 0), time(a, 5)]),
		stn::remove_constraints(STN1, [Strong], STN2),
		stn::earliest_schedule(STN2, [time(zero, 0), time(a, 2)]),
		stn::remove_constraints(STN2, [Weak], STN),
		\+ stn::earliest_schedule(STN, _).

	test(stn_earliest_schedule_point_deletion, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, zero, -5), constraint(b, a, -2)], STN1),
		stn::earliest_schedule(STN1, [time(zero, 0), time(a, 5), time(b, 7)]),
		stn::remove_time_points(STN1, [a], Unbounded),
		\+ stn::earliest_schedule(Unbounded, _),
		stn::remove_time_points(STN1, [b], Remaining),
		stn::earliest_schedule(Remaining, [time(zero, 0), time(a, 5)]).

	test(stn_earliest_schedule_rounding_diagnostic, error(evaluation_error(stn_numerical_inconsistency))) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, zero, -1.0e16), constraint(b, a, -0.1)], STN),
		stn::earliest_schedule(STN, _).

	test(stn_schedule_anchor_and_disconnected_edge, deterministic) :-
		stn::new([anchor, a, b], STN0),
		stn::add_constraints(STN0, [constraint(anchor, zero, -5), constraint(zero, anchor, 10), constraint(a, b, -2)], STN),
		stn::schedule(STN, [time(zero, 0), time(anchor, 5), time(a, 5), time(b, 3)]),
		stn::bounds(STN, anchor, 5, 10),
		stn::bounds(STN, a, negative_infinity, positive_infinity).

	test(stn_schedule_empty, deterministic) :-
		stn::new([], STN),
		stn::schedule(STN, [time(zero, 0)]).

	test(stn_schedule_unconstrained, deterministic) :-
		stn::new([b, a], STN),
		stn::schedule(STN, [time(zero, 0), time(b, 0), time(a, 0)]).

	test(stn_schedule_negative_times, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(zero, a, -4), constraint(a, b, -3)], STN),
		stn::schedule(STN, [time(zero, 0), time(a, -4), time(b, -7)]).

	test(stn_schedule_fixed_float, deterministic(Time =~= 1.5)) :-
		stn::new([a], STN0),
		stn::add_constraints(STN0, [constraint(zero, a, 1.5), constraint(a, zero, -1.5)], STN),
		stn::schedule(STN, [time(zero, 0), time(a, Time)]).

	test(stn_schedule_labels_and_order, deterministic) :-
		stn::new([point(b), 1.0, 1], STN0),
		stn::add_constraint(STN0, 1, 1.0, -2, STN),
		stn::schedule(STN, [time(zero, 0), time(point(b), 0), time(1.0, -2), time(1, 0)]).

	test(stn_schedule_parallel_and_zero_cycles, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(zero, zero, 0), constraint(a, a, 0), constraint(a, b, 2), constraint(a, b, 1), constraint(b, a, -1)], STN),
		stn::schedule(STN, [time(zero, 0), time(a, -1), time(b, 0)]).

	test(stn_schedule_repeated_and_immutable, deterministic(First == Second)) :-
		stn::new([a, b], STN0),
		stn::add_constraint(STN0, a, b, -2, STN),
		stn::constraints(STN, Sources),
		stn::schedule(STN, First),
		stn::schedule(STN, Second),
		stn::constraints(STN, Sources),
		stn::distance(STN, a, b, -2, Sources),
		stn::schedule(STN0, [time(zero, 0), time(a, 0), time(b, 0)]).

	test(stn_schedule_point_deletion, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, -2), constraint(b, c, -3), constraint(a, c, 0)], STN1),
		stn::schedule(STN1, [time(zero, 0), time(a, 0), time(b, -2), time(c, -5)]),
		stn::remove_time_points(STN1, [b], STN),
		stn::schedule(STN, [time(zero, 0), time(a, 0), time(c, 0)]),
		stn::add_time_points(STN, [b], STN2),
		stn::schedule(STN2, [time(zero, 0), time(a, 0), time(c, 0), time(b, 0)]).

	test(stn_schedule_source_retraction, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraint(STN0, a, b, -2, Id, STN1),
		stn::remove_constraints(STN1, [Id], STN),
		stn::schedule(STN1, [time(zero, 0), time(a, 0), time(b, -2)]),
		stn::schedule(STN, [time(zero, 0), time(a, 0), time(b, 0)]).

	test(stn_schedule_invalid_state, fail) :-
		stn::schedule(not_a_state, _).

	test(stn_schedule_nonground_state, deterministic) :-
		\+ stn::schedule(STN, Schedule),
		var(STN),
		var(Schedule).

	test(stn_schedule_incompatible_output, deterministic) :-
		stn::new([a], STN),
		\+ stn::schedule(STN, [time(zero, 0), time(a, 1)]),
		stn::schedule(STN, [time(zero, 0), time(a, 0)]).

	test(stn_schedule_normalization_rounding, error(evaluation_error(stn_numerical_inconsistency))) :-
		stn::new([anchor, a, b], STN0),
		stn::add_constraints(STN0, [constraint(anchor, zero, -1.0e16), constraint(a, b, -0.1)], STN),
		stn::schedule(STN, _).

	test(stn_bounds_direction, deterministic((Lower == 5, Upper == 10))) :-
		stn::new([t], STN0),
		stn::add_constraints(STN0, [constraint(zero, t, 10), constraint(t, zero, -5)], STN),
		stn::bounds(STN, t, Lower, Upper),
		stn::earliest(STN, t, Lower),
		stn::latest(STN, t, Upper),
		stn::difference_bounds(STN, t, zero, -10, -5).

	test(stn_empty, deterministic(Nodes == [zero])) :-
		stn::new([], STN),
		stn::time_points(STN, Nodes),
		stn::constraints(STN, []),
		stn::bounds(STN, zero, 0, 0),
		stn::consistent(STN).

	test(stn_unbounded, deterministic) :-
		stn::new([t], STN),
		stn::bounds(STN, t, negative_infinity, positive_infinity),
		stn::distance(STN, t, t, 0).

	test(stn_one_sided, deterministic) :-
		stn::new([t], STN0),
		stn::add_constraint(STN0, zero, t, 7, STN),
		stn::bounds(STN, t, negative_infinity, 7).

	test(stn_transitive, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 3), constraint(b, c, -1), constraint(a, c, 8)], STN),
		stn::difference_bounds(STN, a, c, negative_infinity, 2).

	test(stn_disconnected_negative_cycle, fail) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 1), constraint(b, a, -2)], _).

	test(stn_negative_self_edge, fail) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, a, a, -1, _).

	test(stn_zero_cycle, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 1), constraint(b, a, -1)], STN),
		stn::difference_bounds(STN, a, b, 1, 1).

	test(stn_empty_batch, deterministic(STN == STN0)) :-
		stn::new([t], STN0),
		stn::add_constraints(STN0, [], STN).

	test(stn_duplicate_points, fail) :-
		stn::new([a, a], _).

	test(stn_reserved_zero, fail) :-
		stn::new([zero], _).

	test(stn_unknown_point, fail) :-
		stn::new([a], STN),
		stn::distance(STN, a, unknown, _).

	test(stn_invalid_weight, fail) :-
		stn::new([a], STN),
		stn::add_constraint(STN, zero, a, positive_infinity, _).

	test(stn_float_bounds, deterministic) :-
		stn::new([t], STN0),
		stn::add_constraints(STN0, [constraint(zero, t, 1.5), constraint(t, zero, -0.5)], STN),
		stn::bounds(STN, t, 0.5, 1.5).

	test(stn_source_ids, deterministic((Ids == [1, 2], Sources == [constraint(1, a, b, 2), constraint(2, a, b, 9)]))) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 2), constraint(a, b, 9)], Ids, STN),
		stn::constraints(STN, Sources).

	test(stn_supporting_path, deterministic(Path == [constraint(1, a, b, 3), constraint(2, b, c, -1)])) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 3), constraint(b, c, -1), constraint(a, c, 8)], STN),
		stn::distance(STN, a, c, 2, Path),
		stn::entails(STN, a, c, 2, Path),
		\+ stn::entails(STN, a, c, 1).

	test(stn_identity_path, deterministic(Path == [])) :-
		stn::new([a], STN),
		stn::distance(STN, a, a, 0, Path),
		stn::entails(STN, a, a, 0, Path).

	test(stn_unreachable_path, fail) :-
		stn::new([a, b], STN),
		stn::distance(STN, a, b, _, _).

	test(stn_unreachable_entailment, fail) :-
		stn::new([a, b], STN),
		stn::entails(STN, a, b, 100).

	test(stn_negative_cycle_witness, deterministic) :-
		stn::new([a, b, c], STN0),
		Batch = [constraint(a, b, 1), constraint(b, c, 2), constraint(c, a, -4)],
		stn::try_add_constraints(STN0, Batch, [1, 2, 3], inconsistent(cycle(Sources, Weight))),
		Sources = [constraint(_, Start, _, _)| _],
		check_path(Sources, Start, Start, 0, Total),
		Total =:= Weight,
		Weight < 0,
		stn::constraints(STN0, []).

	test(stn_negative_self_witness, deterministic(Sources == [constraint(1, a, a, -1)])) :-
		stn::new([a], STN0),
		stn::try_add_constraints(STN0, [constraint(a, a, -1)], [1], inconsistent(cycle(Sources, -1))).

	test(stn_try_empty_batch, deterministic(STN == STN0)) :-
		stn::new([], STN0),
		stn::try_add_constraints(STN0, [], [], consistent(STN)).

	test(stn_retraction_restores_bound, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 2), constraint(a, b, 9)], [1, 2], STN1),
		stn::remove_constraints(STN1, [1, 1, 99], STN2),
		stn::distance(STN2, a, b, 9, [constraint(2, a, b, 9)]),
		stn::distance(STN1, a, b, 2),
		stn::add_constraint(STN2, a, b, 1, 3, STN3),
		stn::distance(STN3, a, b, 1).

	test(stn_retraction_restores_equal_support, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 2), constraint(a, b, 2)], [1, 2], STN1),
		stn::distance(STN1, a, b, 2, [constraint(1, a, b, 2)]),
		stn::remove_constraints(STN1, [1], STN2),
		stn::distance(STN2, a, b, 2, [constraint(2, a, b, 2)]).

	test(stn_remove_all, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, zero, a, 3, Id, STN1),
		stn::remove_constraints(STN1, [Id], STN),
		stn::bounds(STN, a, negative_infinity, positive_infinity).

	test(stn_missing_retraction, deterministic(STN == STN0)) :-
		stn::new([], STN0),
		stn::remove_constraints(STN0, [1, 1], STN).

	test(stn_extend_points, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, zero, a, 3, 1, STN1),
		stn::add_time_points(STN1, [b, event(c)], STN2),
		stn::time_points(STN2, [zero, a, b, event(c)]),
		stn::bounds(STN2, b, negative_infinity, positive_infinity),
		stn::add_constraint(STN2, a, b, 2, 2, STN),
		stn::latest(STN, b, 5).

	test(stn_extend_empty, deterministic(STN == STN0)) :-
		stn::new([], STN0),
		stn::add_time_points(STN0, [], STN).

	test(stn_extend_existing, fail) :-
		stn::new([a], STN0),
		stn::add_time_points(STN0, [a], _).

	test(stn_precedence_ambiguous, deterministic) :-
		difference_network(-1, 1, STN),
		stn::can_precede(STN, a, b),
		stn::can_precede_or_equal(STN, a, b),
		\+ stn::must_precede(STN, a, b),
		\+ stn::must_precede_or_equal(STN, a, b).

	test(stn_remove_isolated_point, deterministic) :-
		stn::new([a, b], STN0),
		stn::remove_time_points(STN0, [b], STN),
		stn::time_points(STN, [zero, a]),
		stn::constraints(STN, []),
		stn::consistent(STN),
		stn::time_points(STN0, [zero, a, b]),
		\+ stn::distance(STN, a, b, _).

	test(stn_remove_point_weakens_bound, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 3), constraint(b, c, 2), constraint(a, c, 10)], STN1),
		stn::distance(STN1, a, c, 5, OldPath),
		stn::remove_time_points(STN1, [b], STN),
		stn::time_points(STN, [zero, a, c]),
		stn::constraints(STN, [constraint(3, a, c, 10)]),
		stn::distance(STN, a, c, 10, [constraint(3, a, c, 10)]),
		stn::distance(STN1, a, c, 5, OldPath).

	test(stn_precedence_equal, deterministic) :-
		difference_network(0, 0, STN),
		\+ stn::can_precede(STN, a, b),
		\+ stn::must_precede(STN, a, b),
		stn::can_precede_or_equal(STN, a, b),
		stn::must_precede_or_equal(STN, a, b).

	test(stn_remove_points_empty, deterministic(STN == STN0)) :-
		stn::new([a], STN0),
		stn::remove_time_points(STN0, [], STN).

	test(stn_remove_points_missing, deterministic(STN == STN0)) :-
		stn::new([a], STN0),
		stn::remove_time_points(STN0, [missing, missing], STN).

	test(stn_remove_points_zero_only_state, deterministic(STN == STN0)) :-
		stn::new([], STN0),
		stn::remove_time_points(STN0, [missing], STN),
		stn::bounds(STN, zero, 0, 0).

	test(stn_remove_points_reserved_zero, fail) :-
		stn::new([a], STN),
		stn::remove_time_points(STN, [zero], _).

	test(stn_remove_points_zero_batch_atomic, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, zero, a, 3, STN1),
		\+ stn::remove_time_points(STN1, [a, missing, zero], _),
		stn::time_points(STN1, [zero, a]),
		stn::constraints(STN1, [constraint(1, zero, a, 3)]),
		stn::add_constraint(STN1, a, zero, -1, 2, STN),
		stn::bounds(STN, a, 1, 3).

	test(stn_remove_points_nonground, deterministic) :-
		stn::new([a], STN),
		\+ stn::remove_time_points(STN, Points, _),
		var(Points),
		\+ stn::remove_time_points(STN, [Label], _),
		var(Label),
		\+ stn::remove_time_points(STN, [a| Tail], _),
		var(Tail).

	test(stn_remove_points_nonlist, fail) :-
		stn::new([a], STN),
		stn::remove_time_points(STN, a, _).

	test(stn_remove_points_improper_list, fail) :-
		stn::new([a], STN),
		stn::remove_time_points(STN, [a| invalid], _).

	test(stn_remove_points_incident_sources, deterministic) :-
		deletion_network(STN0),
		stn::remove_time_points(STN0, [b, missing, b], STN),
		stn::time_points(STN, [zero, a, c, d]),
		stn::constraints(STN, [constraint(7, a, c, 10), constraint(8, zero, zero, 0), constraint(9, d, a, 7)]),
		stn::distance(STN, d, c, 17, Path),
		check_path(Path, d, c, 0, 17),
		stn::consistent(STN),
		stn::time_points(STN0, [zero, a, b, c, d]).

	test(stn_remove_points_all, deterministic) :-
		deletion_network(STN0),
		stn::remove_time_points(STN0, [d, c, b, a], STN1),
		stn::time_points(STN1, [zero]),
		stn::constraints(STN1, [constraint(8, zero, zero, 0)]),
		stn::bounds(STN1, zero, 0, 0),
		stn::consistent(STN1),
		stn::add_constraint(STN1, zero, zero, 1, 10, STN),
		stn::constraints(STN, [constraint(8, zero, zero, 0), constraint(10, zero, zero, 1)]).

	test(stn_remove_points_bridge_unbounded, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 3), constraint(b, c, 2)], STN1),
		stn::remove_time_points(STN1, [b], STN),
		stn::distance(STN, a, c, positive_infinity),
		\+ stn::distance(STN, a, c, _, _),
		\+ stn::distance(STN, a, b, _),
		\+ stn::distance(STN, b, c, _).

	test(stn_remove_points_preserves_next_id, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, c, 10), constraint(a, b, 3), constraint(b, c, 2)], STN1),
		stn::remove_time_points(STN1, [b], STN2),
		stn::constraints(STN2, [constraint(1, a, c, 10)]),
		stn::add_constraint(STN2, a, c, 9, 4, STN),
		stn::distance(STN, a, c, 9, [constraint(4, a, c, 9)]).

	test(stn_remove_points_numeric_identity, deterministic) :-
		stn::new([1, 1.0, event(a)], STN0),
		stn::add_constraints(STN0, [constraint(zero, 1, 3), constraint(zero, 1.0, 4), constraint(1.0, event(a), 2)], STN1),
		stn::remove_time_points(STN1, [1], STN),
		stn::time_points(STN, [zero, 1.0, event(a)]),
		stn::constraints(STN, [constraint(2, zero, 1.0, 4), constraint(3, 1.0, event(a), 2)]),
		stn::latest(STN, event(a), 6),
		\+ stn::distance(STN, zero, 1, _).

	test(stn_remove_points_compound_label, deterministic) :-
		stn::new([a, event(a), event(b)], STN0),
		stn::remove_time_points(STN0, [event(a)], STN),
		stn::time_points(STN, [zero, a, event(b)]).

	test(stn_remove_points_readd_unconstrained, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(zero, a, 3), constraint(a, b, 2)], STN1),
		stn::remove_time_points(STN1, [b], STN2),
		stn::add_time_points(STN2, [b], STN3),
		stn::constraints(STN3, [constraint(1, zero, a, 3)]),
		stn::bounds(STN3, b, negative_infinity, positive_infinity),
		stn::add_constraint(STN3, a, b, 1, 3, STN),
		stn::latest(STN, b, 4).

	test(stn_remove_points_new_conflict_witness, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 3), constraint(b, c, 2), constraint(a, c, 10)], STN1),
		stn::remove_time_points(STN1, [b], STN2),
		stn::try_add_constraints(STN2, [constraint(c, a, -11)], [4], inconsistent(cycle(Cycle, Weight))),
		verify_cycle(Cycle, Weight, [constraint(3, a, c, 10), constraint(4, c, a, -11)]),
		stn::distance(STN1, a, c, 5).

	test(stn_remove_points_batch_matches_sequential, deterministic(Again == Batch)) :-
		deletion_network(STN0),
		stn::remove_time_points(STN0, [b, d, b, unknown], Batch),
		stn::remove_time_points(STN0, [b], STN1),
		stn::remove_time_points(STN1, [d], Sequential),
		stn::time_points(Batch, [zero, a, c]),
		stn::time_points(Sequential, [zero, a, c]),
		stn::constraints(Batch, Sources),
		stn::constraints(Sequential, Sources),
		compare_deletion_rows([zero, a, c], [zero, a, c], Batch, Sequential),
		stn::remove_time_points(Batch, [b, d], Again).

	test(stn_precedence_positive, deterministic) :-
		difference_network(1, 2, STN),
		stn::can_precede(STN, a, b),
		stn::must_precede(STN, a, b),
		stn::can_precede_or_equal(STN, a, b),
		stn::must_precede_or_equal(STN, a, b).

	test(stn_precedence_negative, deterministic) :-
		difference_network(-2, -1, STN),
		\+ stn::can_precede(STN, a, b),
		\+ stn::must_precede(STN, a, b),
		\+ stn::can_precede_or_equal(STN, a, b),
		\+ stn::must_precede_or_equal(STN, a, b).

	test(stn_precedence_unbounded, deterministic) :-
		stn::new([a, b], STN),
		stn::can_precede(STN, a, b),
		stn::can_precede_or_equal(STN, a, b),
		\+ stn::must_precede(STN, a, b),
		\+ stn::must_precede_or_equal(STN, a, b).

	test(stn_precedence_identity, deterministic) :-
		stn::new([a], STN),
		\+ stn::can_precede(STN, a, a),
		\+ stn::must_precede(STN, a, a),
		stn::can_precede_or_equal(STN, a, a),
		stn::must_precede_or_equal(STN, a, a).

	test(stn_failed_batch_preserves_state, deterministic) :-
		stn::new([a], STN0),
		stn::add_constraint(STN0, zero, a, 3, STN1),
		\+ stn::add_constraints(STN1, [constraint(a, zero, -4), constraint(zero, a, 10)], _),
		stn::constraints(STN1, [constraint(1, zero, a, 3)]),
		stn::add_constraint(STN1, a, zero, -2, 2, STN2),
		stn::bounds(STN2, a, 2, 3).

	test(stn_nonground_input_unmodified, deterministic) :-
		stn::new([a], STN),
		\+ stn::add_constraint(STN, Point, a, 2, _),
		var(Point),
		\+ stn::distance(STN, zero, Unknown, _),
		var(Unknown),
		\+ stn::new([Label], _),
		var(Label).

	test(stn_invalid_batch, fail) :-
		stn::new([a], STN),
		stn::try_add_constraints(STN, [constraint(zero, a, 1), invalid], _, _).

	test(stn_invalid_removal_id, fail) :-
		stn::new([], STN),
		stn::remove_constraints(STN, [0], _).

	test(stn_labels_use_identity, deterministic) :-
		stn::new([1, 1.0, event(a)], STN0),
		stn::add_constraint(STN0, 1, 1.0, -2, STN),
		stn::distance(STN, 1, 1.0, -2),
		stn::distance(STN, 1.0, 1, positive_infinity).

	test(stn_window_interval_mixed_endpoints, deterministic) :-
		window_network(1.0, 2, 3, 4, STN),
		stn::window_interval(STN, a, i(1.0, 2)),
		stn::window_relation(STN, a, b, before).

	test(stn_event_interval_mixed_endpoints, deterministic) :-
		fixed_events(1.0, 2, 3, 4, STN),
		stn::event_interval(STN, start_a, end_a, i(1.0, 2)),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, before).

	test(stn_window_interval_numeric_endpoints, deterministic) :-
		window_network(-3, 1, -2.5, 0.5, STN),
		stn::window_interval(STN, a, i(-3, 1)),
		stn::window_interval(STN, b, i(-2.5, 0.5)).

	test(stn_window_interval_large_integer, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		Start is 2 ^ 60,
		End is Start + 1,
		window_network(Start, End, 0, 1, STN),
		stn::window_interval(STN, a, i(Start, End)).

	test(stn_window_interval_singleton, fail) :-
		window_network(1.0, 1, 2, 3, STN),
		stn::window_interval(STN, a, _).

	test(stn_window_interval_unbounded, deterministic) :-
		stn::new([a], STN0),
		\+ stn::window_interval(STN0, a, _),
		stn::add_constraint(STN0, zero, a, 3, UpperOnly),
		\+ stn::window_interval(UpperOnly, a, _),
		stn::add_constraint(STN0, a, zero, -1, LowerOnly),
		\+ stn::window_interval(LowerOnly, a, _),
		\+ stn::window_interval(STN0, zero, _).

	test(stn_window_interval_invalid_inputs, deterministic) :-
		window_network(1, 2, 3, 4, STN),
		\+ stn::window_interval(STN, missing, _),
		\+ stn::window_interval(not_a_state, a, _),
		\+ stn::window_interval(State, a, _),
		var(State),
		\+ stn::window_interval(STN, Point, Interval),
		var(Point),
		var(Interval).

	test(stn_window_interval_repeated_and_immutable, deterministic(First == Second)) :-
		window_network(-2, 4, 0, 5, STN),
		stn::constraints(STN, Sources),
		stn::window_interval(STN, a, First),
		stn::window_interval(STN, a, Second),
		stn::constraints(STN, Sources),
		stn::bounds(STN, a, -2, 4),
		\+ stn::window_interval(STN, a, i(-2, 5)),
		stn::window_relation(STN, a, b, overlaps).

	test(stn_event_interval_numeric_endpoints, deterministic) :-
		fixed_events(-3, 1, -2.5, 0.5, STN),
		stn::event_interval(STN, start_a, end_a, i(-3, 1)),
		stn::event_interval(STN, start_b, end_b, i(-2.5, 0.5)).

	test(stn_event_interval_large_integer, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		Start is 2 ^ 60,
		Middle is Start + 1,
		End is Middle + 1,
		fixed_events(Start, Middle, Middle, End, STN),
		stn::event_interval(STN, start_a, end_a, i(Start, Middle)),
		stn::event_interval(STN, start_b, end_b, i(Middle, End)).

	test(stn_event_interval_zero_and_reversed, deterministic) :-
		fixed_events(1, 1, 3, 2, STN),
		\+ stn::event_interval(STN, start_a, end_a, _),
		\+ stn::event_interval(STN, start_b, end_b, _).

	test(stn_event_interval_uncertain, deterministic) :-
		stn::new([start, end], STN0),
		stn::add_constraints(STN0, [constraint(zero, start, 1), constraint(start, zero, -1), constraint(zero, end, 4), constraint(end, zero, -2)], STN),
		\+ stn::event_interval(STN, start, end, _),
		\+ stn::event_interval(STN, end, start, _).

	test(stn_event_interval_unbounded, fail) :-
		stn::new([start, end], STN),
		stn::event_interval(STN, start, end, _).

	test(stn_event_interval_reference_endpoint, deterministic) :-
		stn::new([end], STN0),
		stn::add_constraints(STN0, [constraint(zero, end, 2), constraint(end, zero, -2)], STN),
		stn::event_interval(STN, zero, end, i(0, 2)).

	test(stn_event_interval_invalid_inputs, deterministic) :-
		fixed_events(1, 2, 3, 4, STN),
		\+ stn::event_interval(STN, missing, end_a, _),
		\+ stn::event_interval(STN, start_a, missing, _),
		\+ stn::event_interval(not_a_state, start_a, end_a, _),
		\+ stn::event_interval(State, start_a, end_a, _),
		var(State),
		\+ stn::event_interval(STN, Start, end_a, _),
		var(Start),
		\+ stn::event_interval(STN, start_a, End, Interval),
		var(End),
		var(Interval).

	test(stn_event_interval_repeated_and_immutable, deterministic(First == Second)) :-
		fixed_events(1, 2, 3, 4, STN),
		stn::constraints(STN, Sources),
		stn::event_interval(STN, start_a, end_a, First),
		stn::event_interval(STN, start_a, end_a, Second),
		stn::constraints(STN, Sources),
		\+ stn::event_interval(STN, start_a, end_a, i(1, 3)),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, before).

	test(stn_event_allen_relations, deterministic) :-
		check_event_cases([
			case(6, 8, before), case(0, 1, after),
			case(5, 8, meets), case(0, 2, met_by),
			case(4, 7, overlaps), case(0, 3, overlapped_by),
			case(2, 7, starts), case(2, 4, started_by),
			case(1, 6, during), case(3, 4, contains),
			case(1, 5, finishes), case(3, 5, finished_by),
			case(2, 5, equal)
		]).

	test(stn_event_mixed_numeric_equality, deterministic(Relation == meets)) :-
		fixed_events(-2, 1, 1.0, 2.5, STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, Relation).

	test(stn_event_mixed_numeric_order, deterministic(Relation == before)) :-
		fixed_events(-3, -2, -1.5, 0.5, STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, Relation).

	test(stn_event_large_integer, deterministic(Relation == meets), [condition(current_prolog_flag(bounded, false))]) :-
		Start is 2 ^ 60,
		Middle is Start + 1,
		End is Middle + 1,
		fixed_events(Start, Middle, Middle, End, STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, Relation).

	test(stn_event_zero_duration, fail) :-
		fixed_events(1, 1, 2, 3, STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, _).

	test(stn_event_reversed, fail) :-
		fixed_events(2, 1, 3, 4, STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, _).

	test(stn_event_uncertain, fail) :-
		stn::new([start_a, end_a, start_b, end_b], STN0),
		stn::add_constraints(STN0, [constraint(zero, start_a, 3), constraint(start_a, zero, -1)], STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, _).

	test(stn_event_unbounded, fail) :-
		stn::new([start_a, end_a, start_b, end_b], STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, _).

	test(stn_window_relation, deterministic(Relation == overlaps)) :-
		window_network(-2, 1, 0.5, 2, STN),
		stn::window_relation(STN, a, b, Relation).

	test(stn_window_mixed_numeric_equality, deterministic(Relation == meets)) :-
		window_network(-2, 1, 1.0, 3, STN),
		stn::window_relation(STN, a, b, Relation).

	test(stn_window_singleton, fail) :-
		window_network(1, 1, 2, 3, STN),
		stn::window_relation(STN, a, b, _).

	test(stn_window_unbounded, fail) :-
		stn::new([a, b], STN),
		stn::window_relation(STN, a, b, _).

	test(stn_window_relation_is_not_point_order, deterministic) :-
		window_network(0, 2, 0, 2, STN0),
		stn::add_constraints(STN0, [constraint(a, b, 0), constraint(b, a, 0)], STN),
		stn::window_relation(STN, a, b, equal),
		\+ stn::can_precede(STN, a, b),
		stn::must_precede_or_equal(STN, a, b).

	test(stn_positive_cycle, deterministic) :-
		stn::new([a, b], STN0),
		stn::add_constraints(STN0, [constraint(a, b, 2), constraint(b, a, 3)], STN),
		stn::difference_bounds(STN, a, b, -3, 2).

	test(stn_schedule_example, deterministic) :-
		stn::new([start_a, end_a, start_b, end_b], STN0),
		stn::add_constraints(STN0, [
			constraint(start_a, end_a, 10), constraint(end_a, start_a, -5),
			constraint(zero, start_a, 2), constraint(start_a, zero, 0),
			constraint(start_b, end_a, 0), constraint(zero, start_b, 20)
		], STN),
		stn::bounds(STN, start_a, 0, 2),
		stn::bounds(STN, end_a, 5, 12),
		stn::bounds(STN, start_b, 5, 20),
		stn::must_precede_or_equal(STN, end_a, start_b),
		stn::window_relation(STN, start_a, start_b, before).

	test(stn_overflow_is_not_a_conflict, error(evaluation_error(float_overflow))) :-
		stn::new([a, b, c], STN0),
		stn::try_add_constraints(STN0, [constraint(a, b, 1.0e308), constraint(b, c, 1.0e308)], _, _).

	test(stn_invalid_state, fail) :-
		stn::consistent(not_a_state).

	test(stn_nonground_state, deterministic) :-
		\+ stn::consistent(State),
		var(State).

	test(stn_improper_points, fail) :-
		stn::new([a| invalid], _).

	test(stn_improper_batch, fail) :-
		stn::new([a], STN),
		stn::add_constraints(STN, [constraint(zero, a, 1)| invalid], _).

	test(stn_reserved_point_extension, fail) :-
		stn::new([], STN),
		stn::add_time_points(STN, [zero], _).

	test(stn_duplicate_point_extension, fail) :-
		stn::new([], STN),
		stn::add_time_points(STN, [a, a], _).

	test(stn_empty_retraction, deterministic(STN == STN0)) :-
		stn::new([a], STN0),
		stn::remove_constraints(STN0, [], STN).

	test(stn_mixed_numeric_fixed_point, deterministic(Relation == equal)) :-
		fixed_events(1, 2, 1.0, 2.0, STN),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, Relation).

	test(stn_cycle_contains_existing_sources, deterministic) :-
		stn::new([a, b, c], STN0),
		stn::add_constraints(STN0, [constraint(a, b, -1), constraint(b, c, -1)], STN1),
		stn::try_add_constraints(STN1, [constraint(c, a, 1)], [3], inconsistent(cycle(Cycle, Weight))),
		verify_cycle(Cycle, Weight, [constraint(1, a, b, -1), constraint(2, b, c, -1), constraint(3, c, a, 1)]).

	test(stn_parallel_negative_cycle, deterministic) :-
		stn::new([a, b], STN0),
		Batch = [constraint(a, b, 5), constraint(a, b, -1), constraint(b, a, 0)],
		stn::try_add_constraints(STN0, Batch, [1, 2, 3], inconsistent(cycle(Cycle, Weight))),
		verify_cycle(Cycle, Weight, [constraint(1, a, b, 5), constraint(2, a, b, -1), constraint(3, b, a, 0)]).

	test(stn_zero_cycle_paths, deterministic) :-
		stn::new([a, b, c, d], STN0),
		stn::add_constraints(STN0, [
			constraint(a, b, 0), constraint(b, a, 0),
			constraint(b, c, -1), constraint(c, d, 1), constraint(d, b, 0)
		], STN),
		stn::distance(STN, a, d, 0, Path),
		check_path(Path, a, d, 0, 0).

	test(stn_exhaustive_bounded_schedules, deterministic) :-
		check_oracle_cases(-4).

	quick_check(stn_generated_schedule_oracle, schedule_oracle(+byte, +byte), [n(100)]).

	quick_check(stn_generated_chain_properties, chain_properties(+byte, +byte, +byte), [n(100)]).

	quick_check(stn_generated_translation, translation_property(+byte), [n(50)]).

	% auxiliary predicates

	fixed_events(Start1, End1, Start2, End2, STN) :-
		stn::new([start_a, end_a, start_b, end_b], STN0),
		NegativeStart1 is -Start1,
		NegativeEnd1 is -End1,
		NegativeStart2 is -Start2,
		NegativeEnd2 is -End2,
		stn::add_constraints(STN0, [
			constraint(zero, start_a, Start1), constraint(start_a, zero, NegativeStart1),
			constraint(zero, end_a, End1), constraint(end_a, zero, NegativeEnd1),
			constraint(zero, start_b, Start2), constraint(start_b, zero, NegativeStart2),
			constraint(zero, end_b, End2), constraint(end_b, zero, NegativeEnd2)
		], STN).

	check_event_cases([]) :-
		!.
	check_event_cases([case(Start, End, Relation)| Cases]) :-
		fixed_events(2, 5, Start, End, STN),
		stn::event_interval(STN, start_a, end_a, i(2, 5)),
		stn::event_interval(STN, start_b, end_b, i(Start, End)),
		stn::event_relation(STN, start_a, end_a, start_b, end_b, Relation),
		check_event_cases(Cases).

	window_network(Start1, End1, Start2, End2, STN) :-
		stn::new([a, b], STN0),
		NegativeStart1 is -Start1,
		NegativeStart2 is -Start2,
		stn::add_constraints(STN0, [
			constraint(zero, a, End1), constraint(a, zero, NegativeStart1),
			constraint(zero, b, End2), constraint(b, zero, NegativeStart2)
		], STN).

	difference_network(Lower, Upper, STN) :-
		stn::new([a, b], STN0),
		Reverse is -Lower,
		stn::add_constraints(STN0, [constraint(a, b, Upper), constraint(b, a, Reverse)], STN).

	schedule_oracle(Byte1, Byte2) :-
		Upper is Byte1 mod 9 - 4,
		Reverse is Byte2 mod 9 - 4,
		verify_oracle(Upper, Reverse).

	check_oracle_cases(Upper) :-
		(	Upper > 4 ->
			true
		;	check_oracle_reverse(-4, Upper),
			NextUpper is Upper + 1,
			check_oracle_cases(NextUpper)
		).

	check_oracle_reverse(Reverse, Upper) :-
		(	Reverse > 4 ->
			true
		;	verify_oracle(Upper, Reverse),
			NextReverse is Reverse + 1,
			check_oracle_reverse(NextReverse, Upper)
		).

	verify_oracle(Upper, Reverse) :-
		findall(schedule(TimeA, TimeB), (
			between(-2, 2, TimeA),
			between(-2, 2, TimeB),
			TimeB - TimeA =< Upper,
			TimeA - TimeB =< Reverse
		), Schedules),
		Batch = [
			constraint(zero, a, 2), constraint(a, zero, 2),
			constraint(zero, b, 2), constraint(b, zero, 2),
			constraint(a, b, Upper), constraint(b, a, Reverse)
		],
		Known = [
			constraint(1, zero, a, 2), constraint(2, a, zero, 2),
			constraint(3, zero, b, 2), constraint(4, b, zero, 2),
			constraint(5, a, b, Upper), constraint(6, b, a, Reverse)
		],
		stn::new([a, b], STN0),
		stn::try_add_constraints(STN0, Batch, [1, 2, 3, 4, 5, 6], Outcome),
		(	Schedules == [] ->
			Outcome = inconsistent(cycle(Cycle, Weight)),
			verify_cycle(Cycle, Weight, Known)
		;	Outcome = consistent(STN),
			check_pair_rows([zero, a, b], STN, Schedules, Known),
			stn::schedule(STN, [time(zero, 0), time(a, TimeA), time(b, TimeB)]),
			memberchk(schedule(TimeA, TimeB), Schedules),
			stn::earliest_schedule(STN, [time(zero, 0), time(a, EarliestA), time(b, EarliestB)]),
			memberchk(schedule(EarliestA, EarliestB), Schedules),
			oracle_differences(Schedules, zero, a, ValuesA),
			oracle_differences(Schedules, zero, b, ValuesB),
			sort(ValuesA, [EarliestA| _]),
			sort(ValuesB, [EarliestB| _])
		).

	check_pair_rows([], _STN, _Schedules, _Known) :-
		!.
	check_pair_rows([From| Points], STN, Schedules, Known) :-
		check_pair_columns([zero, a, b], From, STN, Schedules, Known),
		check_pair_rows(Points, STN, Schedules, Known).

	check_pair_columns([], _From, _STN, _Schedules, _Known) :-
		!.
	check_pair_columns([To| Points], From, STN, Schedules, Known) :-
		oracle_differences(Schedules, From, To, Differences),
		sort(Differences, [Lower| Sorted]),
		reverse([Lower| Sorted], [Upper| _]),
		stn::difference_bounds(STN, From, To, Lower, Upper),
		stn::distance(STN, From, To, Upper, Path),
		check_path(Path, From, To, 0, Upper),
		check_source_members(Path, Known),
		check_pair_columns(Points, From, STN, Schedules, Known).

	oracle_differences([], _From, _To, []) :-
		!.
	oracle_differences([schedule(TimeA, TimeB)| Schedules], From, To, [Difference| Differences]) :-
		schedule_time(From, TimeA, TimeB, Left),
		schedule_time(To, TimeA, TimeB, Right),
		Difference is Right - Left,
		oracle_differences(Schedules, From, To, Differences).

	schedule_time(zero, _, _, 0) :-
		!.
	schedule_time(a, TimeA, _, TimeA) :-
		!.
	schedule_time(b, _, TimeB, TimeB).

	verify_cycle([Source| Sources], Weight, Known) :-
		Source = constraint(_, Start, _, _),
		check_path([Source| Sources], Start, Start, 0, Total),
		Total =:= Weight,
		Weight < 0,
		check_source_members([Source| Sources], Known).

	check_source_members([], _Known) :-
		!.
	check_source_members([Source| Sources], Known) :-
		memberchk(Source, Known),
		check_source_members(Sources, Known).

	chain_properties(Byte1, Byte2, Byte3) :-
		Weight1 is Byte1 mod 9 - 4,
		Weight2 is Byte2 mod 9 - 4,
		Weight3 is Byte3 mod 9 - 4,
		Batch = [constraint(a, b, Weight1), constraint(b, c, Weight2), constraint(c, d, Weight3)],
		stn::new([a, b, c, d], STN0),
		stn::add_constraints(STN0, Batch, STN1),
		stn::add_constraint(STN0, a, b, Weight1, Sequential1),
		stn::add_constraint(Sequential1, b, c, Weight2, Sequential2),
		stn::add_constraint(Sequential2, c, d, Weight3, Sequential),
		stn::constraints(STN1, Sources),
		stn::constraints(Sequential, Sources),
		Sum12 is Weight1 + Weight2,
		Total is Sum12 + Weight3,
		stn::distance(STN1, a, c, Sum12),
		stn::distance(STN1, a, d, Total, Sources),
		stn::distance(Sequential, a, d, Total, Sources),
		check_path(Sources, a, d, 0, Total),
		Stronger is Total - 1,
		stn::add_constraint(STN1, a, d, Stronger, Id, STN2),
		stn::distance(STN2, a, d, Stronger),
		stn::remove_constraints(STN2, [Id], STN3),
		stn::distance(STN3, a, d, Total, Sources),
		stn::difference_bounds(STN3, d, a, Lower, positive_infinity),
		Lower =:= -Total,
		check_feasible_schedule(STN1),
		check_feasible_schedule(Sequential),
		check_feasible_schedule(STN3).

	check_feasible_schedule(STN) :-
		stn::schedule(STN, Assignment),
		stn::constraints(STN, Sources),
		check_assignment_sources(Sources, Assignment).

	check_assignment_sources([], _Assignment) :-
		!.
	check_assignment_sources([constraint(_, X, Y, Weight)| Sources], Assignment) :-
		memberchk(time(X, Left), Assignment),
		memberchk(time(Y, Right), Assignment),
		Right - Left =< Weight,
		check_assignment_sources(Sources, Assignment).

	translation_property(Byte) :-
		Offset is Byte - 128,
		Start1 is Offset,
		End1 is Offset + 3,
		Start2 is Offset + 1,
		End2 is Offset + 4,
		window_network(0, 3, 1, 4, Base0),
		window_network(Start1, End1, Start2, End2, Shifted0),
		stn::add_constraint(Base0, a, b, 2, Base),
		stn::add_constraint(Shifted0, a, b, 2, Shifted),
		stn::difference_bounds(Base, a, b, Lower, Upper),
		stn::difference_bounds(Shifted, a, b, Lower, Upper),
		stn::window_relation(Base, a, b, Relation),
		stn::window_relation(Shifted, a, b, Relation).

	deletion_network(STN) :-
		stn::new([a, b, c, d], STN0),
		stn::add_constraints(STN0, [
			constraint(a, b, 3), constraint(b, c, 2), constraint(b, b, 0),
			constraint(b, c, 9), constraint(zero, b, 5), constraint(b, zero, 0),
			constraint(a, c, 10), constraint(zero, zero, 0), constraint(d, a, 7)
		], STN).

	compare_deletion_rows([], _Nodes, _Batch, _Sequential) :-
		!.
	compare_deletion_rows([From| Points], Nodes, Batch, Sequential) :-
		compare_deletion_columns(Nodes, From, Batch, Sequential),
		compare_deletion_rows(Points, Nodes, Batch, Sequential).

	compare_deletion_columns([], _From, _Batch, _Sequential) :-
		!.
	compare_deletion_columns([To| Points], From, Batch, Sequential) :-
		stn::difference_bounds(Batch, From, To, Lower, Upper),
		stn::difference_bounds(Sequential, From, To, Lower, Upper),
		compare_deletion_columns(Points, From, Batch, Sequential).

	check_path([], Point, End, Total, Total) :-
		!,
		Point == End.
	check_path([constraint(_, From, To, Weight)| Sources], Point, End, Total0, Total) :-
		From == Point,
		Total1 is Total0 + Weight,
		check_path(Sources, To, End, Total1, Total).

:- end_object.
