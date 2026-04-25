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


:- category(grouped_strength_ranker_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-25,
		comment is 'Shared connectivity, positive-strength, and iterative-update helpers for grouped ranking models that learn one strength parameter per item.'
	]).

	:- protected(update_strengths/4).
	:- mode(update_strengths(+nonvar, +list(number), -list(number), -number), one).
	:- info(update_strengths/4, [
		comment is 'Hook predicate that importing rankers must define to perform one synchronous strength-update iteration.',
		argnames is ['Context', 'Strengths0', 'Strengths', 'MaximumDifference']
	]).

	:- protected(initial_strengths/2).
	:- mode(initial_strengths(+list, -list(float)), one).
	:- info(initial_strengths/2, [
		comment is 'Initializes a positive strength vector with a uniform value for each ordered item.',
		argnames is ['Items', 'Strengths']
	]).

	:- protected(strength_pairs/3).
	:- mode(strength_pairs(+list, +list(number), -list(pair)), one).
	:- info(strength_pairs/3, [
		comment is 'Pairs ordered items with their learned strength values.',
		argnames is ['Items', 'StrengthValues', 'Strengths']
	]).

	:- protected(optimize_strengths/8).
	:- mode(optimize_strengths(+integer, +number, +nonvar, +list(number), -list(number), -atom, -integer, -number), one).
	:- info(optimize_strengths/8, [
		comment is 'Runs the iterative strength-update loop until convergence or the maximum iteration bound is reached.',
		argnames is ['MaximumIterations', 'Tolerance', 'UpdateContext', 'Strengths0', 'Strengths', 'Status', 'Iterations', 'FinalDifference']
	]).

	:- protected(strength_dictionary/2).
	:- mode(strength_dictionary(+list(number), -compound), one).
	:- info(strength_dictionary/2, [
		comment is 'Builds an index-addressable dictionary from an ordered list of strength values.',
		argnames is ['Strengths', 'Dictionary']
	]).

	:- protected(require_strong_connectivity/4).
	:- mode(require_strong_connectivity(+integer, +compound, +compound, +atom), one).
	:- info(require_strong_connectivity/4, [
		comment is 'Checks that the directed graph is strongly connected and throws a domain error with the strongly connected components otherwise.',
		argnames is ['Count', 'DirectedAdjacency', 'ReverseAdjacency', 'ErrorDomain']
	]).

	:- protected(strongly_connected/3).
	:- mode(strongly_connected(+integer, +compound, +compound), zero_or_one).
	:- info(strongly_connected/3, [
		comment is 'True when all indexed items are mutually reachable in both the forward and reverse adjacency graphs.',
		argnames is ['Count', 'DirectedAdjacency', 'ReverseAdjacency']
	]).

	:- protected(strongly_connected_components/4).
	:- mode(strongly_connected_components(+integer, +compound, +compound, -list), one).
	:- info(strongly_connected_components/4, [
		comment is 'Computes the strongly connected components of the directed graph induced by the forward and reverse adjacency dictionaries.',
		argnames is ['Count', 'DirectedAdjacency', 'ReverseAdjacency', 'Components']
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		length/2
	]).

	initial_strengths(Items, Strengths) :-
		length(Items, Count),
		Strength is 1.0 / Count,
		fill_strengths(Count, Strength, Strengths).

	fill_strengths(0, _Strength, []) :-
		!.
	fill_strengths(Count, Strength, [Strength| Strengths]) :-
		Count > 0,
		NextCount is Count - 1,
		fill_strengths(NextCount, Strength, Strengths).

	strength_pairs([], [], []).
	strength_pairs([Item| Items], [Strength| Values], [Item-Strength| Strengths]) :-
		strength_pairs(Items, Values, Strengths).

	optimize_strengths(MaximumIterations, Tolerance, UpdateContext, Strengths0, Strengths, Status, Iterations, FinalDifference) :-
		optimize_strengths(0, MaximumIterations, Tolerance, UpdateContext, Strengths0, Strengths, Status, Iterations, FinalDifference).

	optimize_strengths(Iteration0, MaximumIterations, Tolerance, UpdateContext, Strengths0, Strengths, Status, Iterations, FinalDifference) :-
		::update_strengths(UpdateContext, Strengths0, Strengths1, MaximumDifference),
		Iteration is Iteration0 + 1,
		(	MaximumDifference =< Tolerance ->
			Strengths = Strengths1,
			Status = converged,
			Iterations = Iteration,
			FinalDifference = MaximumDifference
		;	Iteration >= MaximumIterations ->
			Strengths = Strengths1,
			Status = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDifference = MaximumDifference
		;	optimize_strengths(Iteration, MaximumIterations, Tolerance, UpdateContext, Strengths1, Strengths, Status, Iterations, FinalDifference)
		).

	strength_dictionary(Strengths, Dictionary) :-
		dictionary_new(Dictionary0),
		strength_dictionary(Strengths, 1, Dictionary0, Dictionary).

	strength_dictionary([], _Index, Dictionary, Dictionary).
	strength_dictionary([Strength| Strengths], Index, Dictionary0, Dictionary) :-
		dictionary_insert(Dictionary0, Index, Strength, Dictionary1),
		NextIndex is Index + 1,
		strength_dictionary(Strengths, NextIndex, Dictionary1, Dictionary).

	require_strong_connectivity(Count, DirectedAdjacency0, ReverseAdjacency0, ErrorDomain) :-
		(	strongly_connected(Count, DirectedAdjacency0, ReverseAdjacency0) ->
			true
		;	strongly_connected_components(Count, DirectedAdjacency0, ReverseAdjacency0, Components),
			domain_error(ErrorDomain, Components)
		).

	strongly_connected(Count, DirectedAdjacency, ReverseAdjacency) :-
		reachable_all(Count, DirectedAdjacency),
		reachable_all(Count, ReverseAdjacency).

	reachable_all(Count, Adjacency) :-
		dictionary_new(Visited0),
		dfs_reach(1, Adjacency, Visited0, Visited),
		all_visited(1, Count, Visited).

	dfs_reach(Index, Adjacency, Visited0, Visited) :-
		(	dictionary_lookup(Index, _Seen, Visited0) ->
			Visited = Visited0
		;	dictionary_insert(Visited0, Index, true, Visited1),
			adjacency_neighbors(Index, Adjacency, Neighbors),
			dfs_reach_neighbors(Neighbors, Adjacency, Visited1, Visited)
		).

	dfs_reach_neighbors([], _Adjacency, Visited, Visited).
	dfs_reach_neighbors([Neighbor| Neighbors], Adjacency, Visited0, Visited) :-
		dfs_reach(Neighbor, Adjacency, Visited0, Visited1),
		dfs_reach_neighbors(Neighbors, Adjacency, Visited1, Visited).

	all_visited(Index, Count, _Visited) :-
		Index > Count,
		!.
	all_visited(Index, Count, Visited) :-
		dictionary_lookup(Index, _Seen, Visited),
		NextIndex is Index + 1,
		all_visited(NextIndex, Count, Visited).

	strongly_connected_components(Count, DirectedAdjacency, ReverseAdjacency, Components) :-
		dictionary_new(ForwardVisited0),
		finish_order(1, Count, DirectedAdjacency, ForwardVisited0, [], _ForwardVisited, Order),
		dictionary_new(ReverseVisited0),
		collect_components(Order, ReverseAdjacency, ReverseVisited0, _ReverseVisited, Components).

	finish_order(Index, Count, _DirectedAdjacency, Visited, Order, Visited, Order) :-
		Index > Count,
		!.
	finish_order(Index, Count, DirectedAdjacency, Visited0, Order0, Visited, Order) :-
		(	dictionary_lookup(Index, _Seen, Visited0) ->
			Visited1 = Visited0,
			Order1 = Order0
		;	dfs_finish(Index, DirectedAdjacency, Visited0, Order0, Visited1, Order1)
		),
		NextIndex is Index + 1,
		finish_order(NextIndex, Count, DirectedAdjacency, Visited1, Order1, Visited, Order).

	dfs_finish(Index, DirectedAdjacency, Visited0, Order0, Visited, [Index| Order]) :-
		dictionary_insert(Visited0, Index, true, Visited1),
		adjacency_neighbors(Index, DirectedAdjacency, Neighbors),
		dfs_finish_neighbors(Neighbors, DirectedAdjacency, Visited1, Order0, Visited, Order).

	dfs_finish_neighbors([], _DirectedAdjacency, Visited, Order, Visited, Order).
	dfs_finish_neighbors([Neighbor| Neighbors], DirectedAdjacency, Visited0, Order0, Visited, Order) :-
		(	dictionary_lookup(Neighbor, _Seen, Visited0) ->
			Visited1 = Visited0,
			Order1 = Order0
		;	dfs_finish(Neighbor, DirectedAdjacency, Visited0, Order0, Visited1, Order1)
		),
		dfs_finish_neighbors(Neighbors, DirectedAdjacency, Visited1, Order1, Visited, Order).

	collect_components([], _ReverseAdjacency, Visited, Visited, []).
	collect_components([Index| Indices], ReverseAdjacency, Visited0, Visited, Components) :-
		(	dictionary_lookup(Index, _Seen, Visited0) ->
			collect_components(Indices, ReverseAdjacency, Visited0, Visited, Components)
		;	dfs_collect(Index, ReverseAdjacency, Visited0, [], Visited1, Component),
			collect_components(Indices, ReverseAdjacency, Visited1, Visited, Components0),
			Components = [Component| Components0]
		).

	dfs_collect(Index, ReverseAdjacency, Visited0, Component0, Visited, Component) :-
		dictionary_insert(Visited0, Index, true, Visited1),
		adjacency_neighbors(Index, ReverseAdjacency, Neighbors),
		dfs_collect_neighbors(Neighbors, ReverseAdjacency, Visited1, [Index| Component0], Visited, Component).

	dfs_collect_neighbors([], _ReverseAdjacency, Visited, Component, Visited, Component).
	dfs_collect_neighbors([Neighbor| Neighbors], ReverseAdjacency, Visited0, Component0, Visited, Component) :-
		(	dictionary_lookup(Neighbor, _Seen, Visited0) ->
			Visited1 = Visited0,
			Component1 = Component0
		;	dfs_collect(Neighbor, ReverseAdjacency, Visited0, Component0, Visited1, Component1)
		),
		dfs_collect_neighbors(Neighbors, ReverseAdjacency, Visited1, Component1, Visited, Component).

	adjacency_neighbors(Index, Adjacency, Neighbors) :-
		(	dictionary_lookup(Index, Neighbors, Adjacency) ->
			true
		;	Neighbors = []
		).

:- end_category.
