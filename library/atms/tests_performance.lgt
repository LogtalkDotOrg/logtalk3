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


:- object(tests_performance(_Representation_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-15,
		comment is 'Performance benchmarks for the "atms" library.',
		parameters is [
			'Representation' - 'ATMS environment representation under test.'
		]
	]).

	:- uses(atms, [
		new/2, create_node/4, create_assumption/4, create_contradiction/4, justify/5, label/3, nogood/2,
		interpretations/2, justifications/2, why/3, retract_justifications/3, retract_batch/4
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	test(interpretations_12_free_assumptions, true, [note(metrics(free_assumptions-12, cpu_seconds-Time))]) :-
		benchmark_interpretations(12, Time).

	test(interpretations_16_free_assumptions, true, [note(metrics(free_assumptions-16, cpu_seconds-Time))]) :-
		benchmark_interpretations(16, Time).

	test(interpretations_20_free_assumptions, true, [note(metrics(free_assumptions-20, cpu_seconds-Time))]) :-
		benchmark_interpretations(20, Time).

	test(unchanged_label_40_alternatives, true, [note(metrics(alternatives-40, environment_calls-Calls))]) :-
		benchmark_unchanged_label(40, Calls).

	test(unchanged_label_80_alternatives, true, [note(metrics(alternatives-80, environment_calls-Calls))]) :-
		benchmark_unchanged_label(80, Calls).

	test(retraction_batches, true, [note(metrics(rule_count-1500, removal_batches-100, missing_node_batches-1000, removal_cpu_seconds-GroupedTime, missing_node_cpu_seconds-MissingTime))]) :-
		benchmark_retractions(1500, 100, GroupedTime, MissingTime).

	test(duplicate_insertions, true, [note(metrics(unrelated_rules-1500, repetitions-1000, cpu_seconds-Time))]) :-
		benchmark_duplicates(1500, 1000, Time).

	test(nogood_batch, true, [note(metrics(alternatives-40, environment_calls-Calls, cpu_seconds-Time))]) :-
		benchmark_nogood_batch(40, Calls, Time).

	test(target_matching, true, [note(metrics(target_count-300, repetitions-100, indexed_cpu_seconds-IndexedTime, linear_cpu_seconds-LinearTime))]) :-
		benchmark_target_matching(300, 100, IndexedTime, LinearTime).

	test(retraction_index_ground, true, [note(metrics(rule_count-240, target_count-300, matching_targets-120, missing_targets-180, surviving_rules-120, cpu_seconds-Time))]) :-
		numbered_fixture(240, State0, Assumption, Head),
		numbered_targets(300, Assumption, Head, Targets),
		justifications(State0, Original),
		linear_rule_survivors(Original, Targets, Expected),
		length(Expected, 120),
		lgtunit::benchmark(retract_justifications(Targets, State0, State), Time),
		justifications(State, Rules),
		Rules == Expected,
		why(Head, State, Explanation),
		Explanation == Expected,
		label(Head, State, [[Assumption]]),
		justifications(State0, Unchanged),
		Unchanged == Original,
		label(Head, State0, [[Assumption]]).

	benchmark_target_matching(Count, Repetitions, IndexedTime, LinearTime) :-
		numbered_targets(Count, node(0), node(1), Targets),
		atms<<target_index(Targets, Index),
		atms<<remove_matching_justifications(Targets, Index, [], _),
		linear_rule_survivors(Targets, Targets, []),
		lgtunit::benchmark((atms<<target_index(Targets, TimedIndex), atms<<remove_matching_justifications(Targets, TimedIndex, [], _)), Repetitions, IndexedTime),
		lgtunit::benchmark(linear_rule_survivors(Targets, Targets, []), Repetitions, LinearTime).

	benchmark_retractions(Count, Repetitions, GroupedTime, MissingTime) :-
		numbered_fixture(Count, State0, Assumption, Head),
		Half is Count // 2,
		numbered_targets(Half, Assumption, Head, Targets),
		justifications(State0, Rules),
		linear_rule_survivors(Rules, Targets, Expected),
		retract_justifications(Targets, State0, Reduced),
		justifications(Reduced, Expected),
		label(Head, Reduced, [[Assumption]]),
		retract_batch([node(99999), node(99999)], [], State0, Same),
		Same == State0,
		lgtunit::benchmark(retract_justifications(Targets, State0, _), Repetitions, GroupedTime),
		MissingRepetitions is Repetitions * 10,
		lgtunit::benchmark(retract_batch([node(99999)], [], State0, _), MissingRepetitions, MissingTime).

	benchmark_duplicates(Count, Repetitions, Time) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		justify(Head, [Assumption], info(Info), State2, Initial),
		add_unrelated_rules(Count, Initial, State),
		justify(Head, [Assumption], info(Info), State, Same),
		Same == State,
		lgtunit::benchmark(justify(Head, [Assumption], info(Info), State, _), Repetitions, Time),
		var(Info).

	benchmark_nogood_batch(Count, Calls, Time) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_node(head, State0, Head, State1),
		add_label_alternatives(Count, Head, State1, State2),
		Counter::from_list([node(1), node(2)], Larger),
		Counter::from_list([node(1)], Smaller),
		Counter::reset,
		lgtunit::benchmark((atms<<record_nogoods([Larger, Smaller], State2, State)), Time),
		Counter::counts(Calls, _),
		findall(Environment, nogood(Environment, State), [[node(1)]]),
		label(Head, State, Label),
		Remaining is Count - 1,
		length(Label, Remaining),
		label(Head, State2, Original),
		length(Original, Count).

	benchmark_interpretations(Count, Time) :-
		new(_Representation_, State0),
		create_assumption(forbidden, State0, Forbidden, State1),
		add_free_assumptions(Count, State1, Free, State2),
		create_contradiction(false, State2, Bottom, State3),
		justify(Bottom, [Forbidden], clash, State3, State),
		lgtunit::benchmark(interpretations(State, Interpretations), Time),
		Interpretations == [Free].

	benchmark_unchanged_label(Count, Calls) :-
		Counter = counting_environment(_Representation_),
		Counter::reset,
		new(Counter, State0),
		create_node(h, State0, Head, State1),
		add_label_alternatives(Count, Head, State1, State2),
		Counter::reset,
		justify(Head, [node(Count)], extra, State2, State),
		Counter::counts(Calls, _),
		label(Head, State, Label),
		length(Label, Count),
		why(Head, State, Why),
		memberchk(justification(Head, [node(Count)], extra), Why).

	numbered_fixture(Count, State, Assumption, Head) :-
		new(_Representation_, State0),
		create_assumption(a, State0, Assumption, State1),
		create_node(h, State1, Head, State2),
		add_numbered_rules(Count, Assumption, Head, State2, State).

	add_numbered_rules(0, _, _, State, State) :-
		!.
	add_numbered_rules(Count, Assumption, Head, State0, State) :-
		justify(Head, [Assumption], index(Count), State0, State1),
		Next is Count - 1,
		add_numbered_rules(Next, Assumption, Head, State1, State).

	numbered_targets(0, _, _, []) :-
		!.
	numbered_targets(Count, Assumption, Head, [justification(Head, [Assumption], index(Id))| Targets]) :-
		Id is Count * 2,
		Next is Count - 1,
		numbered_targets(Next, Assumption, Head, Targets).

	linear_rule_survivors([], _, []).
	linear_rule_survivors([Rule| Rules], Targets, Remaining) :-
		(	linear_identity_member(Rule, Targets) ->
			Remaining = Tail
		;	Remaining = [Rule| Tail]
		),
		linear_rule_survivors(Rules, Targets, Tail).

	linear_identity_member(Rule, [Target| Targets]) :-
		(	Rule == Target ->
			true
		;	linear_identity_member(Rule, Targets)
		).

	add_unrelated_rules(0, State, State) :-
		!.
	add_unrelated_rules(Count, State0, State) :-
		create_node(unrelated, State0, Node, State1),
		justify(Node, [Node], unrelated, State1, State2),
		Next is Count - 1,
		add_unrelated_rules(Next, State2, State).

	add_free_assumptions(0, State, [], State) :-
		!.
	add_free_assumptions(Count, State0, [Node| Nodes], State) :-
		create_assumption(free, State0, Node, State1),
		Next is Count - 1,
		add_free_assumptions(Next, State1, Nodes, State).

	add_label_alternatives(0, _, State, State) :-
		!.
	add_label_alternatives(Count, Head, State0, State) :-
		create_assumption(alternative, State0, Assumption, State1),
		justify(Head, [Assumption], Count, State1, State2),
		Next is Count - 1,
		add_label_alternatives(Next, Head, State2, State).

:- end_object.
