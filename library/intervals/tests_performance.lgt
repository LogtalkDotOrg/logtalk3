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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-13,
		comment is 'Performance benchmarks for the "intervals" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2, benchmark/3
	]).

	:- uses(list, [
		length/2
	]).

	:- private(consistent_closure_/1).
	:- dynamic(consistent_closure_/1).

	:- private(inconsistent_closure_/1).
	:- dynamic(inconsistent_closure_/1).

	setup :-
		cleanup,
		consistent_seed_network(ConsistentSeedNetwork),
		interval_constraint_network::path_consistency(ConsistentSeedNetwork, ConsistentClosure),
		interval_constraint_network::consistent(ConsistentClosure),
		assertz(consistent_closure_(ConsistentClosure)),
		inconsistent_seed_network(InconsistentSeedNetwork),
		interval_constraint_network::path_consistency(InconsistentSeedNetwork, InconsistentClosure),
		\+ interval_constraint_network::consistent(InconsistentClosure),
		assertz(inconsistent_closure_(InconsistentClosure)).

	cleanup :-
		retractall(consistent_closure_(_)),
		retractall(inconsistent_closure_(_)).

	test(path_consistency_chain_12_nodes, true, [note(metrics(nodes-12, seed_constraints-11, seconds-Time))]) :-
		consistent_seed_network(SeedNetwork),
		interval_constraint_network::path_consistency(SeedNetwork, Closure),
		interval_constraint_network::consistent(Closure),
		benchmark(interval_constraint_network::path_consistency(SeedNetwork, _), Time).

	test(relation_queries_chain_12_nodes_8000, true, [note(metrics(nodes-12, queries-8000, seconds-Time))]) :-
		consistent_closure_(Closure),
		relation_query_batch(Closure),
		benchmark(relation_query_batch(Closure), 1000, Time).

	test(contradiction_query_chain_12_nodes_10000, true, [note(metrics(nodes-12, queries-10000, seconds-Time))]) :-
		inconsistent_closure_(Closure),
		interval_constraint_network::contradiction(Closure, contradiction(_Node1, _Node2, _Cause)),
		benchmark(interval_constraint_network::contradiction(Closure, _Explanation), 10000, Time).

	test(contradiction_explanations_chain_12_nodes_1000, true, [note(metrics(nodes-12, queries-1000, explanations-Count, seconds-Time))]) :-
		inconsistent_closure_(Closure),
		interval_constraint_network::contradiction_explanations(Closure, Explanations),
		length(Explanations, Count),
		Count > 0,
		benchmark(interval_constraint_network::contradiction_explanations(Closure, _), 1000, Time).

	consistent_seed_network(Network) :-
		seed_nodes(Nodes),
		interval_constraint_network::new(Nodes, Network0),
		chain_constraints(Constraints),
		apply_constraints(Constraints, Network0, Network).

	inconsistent_seed_network(Network) :-
		consistent_seed_network(Network0),
		interval_constraint_network::refine(Network0, a, l, [after], Network).

	seed_nodes([a, b, c, d, e, f, g, h, i, j, k, l]).

	chain_constraints([
		constraint(a, b, [before]),
		constraint(b, c, [before]),
		constraint(c, d, [before]),
		constraint(d, e, [before]),
		constraint(e, f, [before]),
		constraint(f, g, [before]),
		constraint(g, h, [before]),
		constraint(h, i, [before]),
		constraint(i, j, [before]),
		constraint(j, k, [before]),
		constraint(k, l, [before])
	]).

	apply_constraints([], Network, Network).
	apply_constraints([constraint(Node1, Node2, RelationSet)| Constraints], Network0, Network) :-
		interval_constraint_network::refine(Network0, Node1, Node2, RelationSet, Network1),
		apply_constraints(Constraints, Network1, Network).

	relation_query_batch(Closure) :-
		interval_constraint_network::relation(Closure, a, d, [before]),
		interval_constraint_network::relation(Closure, a, h, [before]),
		interval_constraint_network::relation(Closure, a, l, [before]),
		interval_constraint_network::relation(Closure, c, g, [before]),
		interval_constraint_network::relation(Closure, e, j, [before]),
		interval_constraint_network::relation(Closure, i, l, [before]),
		interval_constraint_network::relation(Closure, d, a, [after]),
		interval_constraint_network::relation(Closure, l, a, [after]).

:- end_object.
