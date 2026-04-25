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


:- category(pairwise_strength_ranker_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-25,
		comment is 'Shared preprocessing, connectivity, and MM-iteration helpers for pairwise strength rankers.'
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

	:- protected(index_items/3).
	:- mode(index_items(+list, +integer, -list(pair)), one).
	:- info(index_items/3, [
		comment is 'Builds ``Item-Index`` pairs for the ordered training items.',
		argnames is ['Items', 'Index', 'Indices']
	]).

	:- protected(preprocess_matchups/6).
	:- mode(preprocess_matchups(+list, +compound, -compound, -compound, -compound, -compound), one).
	:- info(preprocess_matchups/6, [
		comment is 'Builds directed adjacency, reverse adjacency, per-item win totals, and undirected pair-adjacency structures from aggregated pairwise matchups.',
		argnames is ['Matchups', 'IndexDictionary', 'DirectedAdjacency', 'ReverseAdjacency', 'WinWeights', 'PairAdjacency']
	]).

	:- protected(build_dataset_model/5).
	:- mode(build_dataset_model(+integer, +compound, +compound, -list, -list(number)), one).
	:- info(build_dataset_model/5, [
		comment is 'Materializes dense per-item win totals and weighted undirected adjacency lists from the sparse preprocessing dictionaries.',
		argnames is ['Count', 'WinWeights', 'PairAdjacency', 'PairWeights', 'Wins']
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

	:- protected(item_denominator/5).
	:- mode(item_denominator(+list(pair), +compound, +number, +number, -number), one).
	:- info(item_denominator/5, [
		comment is 'Accumulates the pairwise MM denominator for one item using the current strength dictionary and weighted neighbors.',
		argnames is ['Neighbors', 'StrengthDictionary', 'CurrentStrength', 'Denominator0', 'Denominator']
	]).

	:- uses(avltree, [
		as_list/2 as dictionary_as_list/2,
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

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency, ReverseAdjacency, WinWeights, PairAdjacency) :-
		dictionary_new(DirectedAdjacency0),
		dictionary_new(ReverseAdjacency0),
		dictionary_new(WinWeights0),
		dictionary_new(PairAdjacency0),
		preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, WinWeights0, WinWeights, PairAdjacency0, PairAdjacency).

	preprocess_matchups([], _IndexDictionary, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, WinWeights, WinWeights, PairAdjacency, PairAdjacency).
	preprocess_matchups([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], IndexDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, WinWeights0, WinWeights, PairAdjacency0, PairAdjacency) :-
		dictionary_lookup(Item1, Item1Index, IndexDictionary),
		dictionary_lookup(Item2, Item2Index, IndexDictionary),
		TotalWeight is Item1Wins + Item2Wins,
		update_pair_adjacency_dictionary(PairAdjacency0, Item1Index, Item2Index, TotalWeight, PairAdjacency1),
		update_matchup_wins(Item1Wins, Item1Index, Item2Index, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1, WinWeights0, WinWeights1),
		update_matchup_wins(Item2Wins, Item2Index, Item1Index, DirectedAdjacency1, DirectedAdjacency2, ReverseAdjacency1, ReverseAdjacency2, WinWeights1, WinWeights2),
		preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency2, DirectedAdjacency, ReverseAdjacency2, ReverseAdjacency, WinWeights2, WinWeights, PairAdjacency1, PairAdjacency).

	update_matchup_wins(Wins, WinnerIndex, LoserIndex, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, WinWeights0, WinWeights) :-
		(	Wins > 0 ->
			update_neighbor_dictionary(DirectedAdjacency0, WinnerIndex, LoserIndex, DirectedAdjacency),
			update_neighbor_dictionary(ReverseAdjacency0, LoserIndex, WinnerIndex, ReverseAdjacency),
			update_weight_dictionary(WinWeights0, WinnerIndex, Wins, WinWeights)
		;	DirectedAdjacency = DirectedAdjacency0,
			ReverseAdjacency = ReverseAdjacency0,
			WinWeights = WinWeights0
		).

	update_neighbor_dictionary(Dictionary0, Key, Neighbor, Dictionary) :-
		(	dictionary_lookup(Key, Neighbors0, Dictionary0) ->
			Neighbors = [Neighbor| Neighbors0]
		;	Neighbors = [Neighbor]
		),
		dictionary_insert(Dictionary0, Key, Neighbors, Dictionary).

	update_weight_dictionary(Dictionary0, Key, Delta, Dictionary) :-
		(	dictionary_lookup(Key, Weight0, Dictionary0) ->
			Weight is Weight0 + Delta
		;	Weight = Delta
		),
		dictionary_insert(Dictionary0, Key, Weight, Dictionary).

	update_pair_adjacency_dictionary(Dictionary0, Left, Right, Delta, Dictionary) :-
		update_neighbor_weight_dictionary(Dictionary0, Left, Right, Delta, Dictionary1),
		update_neighbor_weight_dictionary(Dictionary1, Right, Left, Delta, Dictionary).

	update_neighbor_weight_dictionary(Dictionary0, Key, Neighbor, Delta, Dictionary) :-
		(	dictionary_lookup(Key, NeighborWeights0, Dictionary0) ->
			true
		;	dictionary_new(NeighborWeights0)
		),
		update_weight_dictionary(NeighborWeights0, Neighbor, Delta, NeighborWeights),
		dictionary_insert(Dictionary0, Key, NeighborWeights, Dictionary).

	build_dataset_model(Count, WinWeights0, PairAdjacency0, PairWeights, Wins) :-
		dictionary_as_list(WinWeights0, WinEntries),
		fill_weight_vector(1, Count, WinEntries, Wins),
		dictionary_as_list(PairAdjacency0, PairAdjacencyEntries),
		fill_weighted_adjacency(1, Count, PairAdjacencyEntries, PairWeights).

	fill_weight_vector(Index, Count, _Entries, []) :-
		Index > Count,
		!.
	fill_weight_vector(Index, Count, [Index-Weight| Entries], [Weight| Weights]) :-
		!,
		NextIndex is Index + 1,
		fill_weight_vector(NextIndex, Count, Entries, Weights).
	fill_weight_vector(Index, Count, Entries, [0.0| Weights]) :-
		NextIndex is Index + 1,
		fill_weight_vector(NextIndex, Count, Entries, Weights).

	fill_weighted_adjacency(Index, Count, _Entries, []) :-
		Index > Count,
		!.
	fill_weighted_adjacency(Index, Count, [Index-NeighborWeights| Entries], [Neighbors| PairWeights]) :-
		!,
		dictionary_as_list(NeighborWeights, Neighbors),
		NextIndex is Index + 1,
		fill_weighted_adjacency(NextIndex, Count, Entries, PairWeights).
	fill_weighted_adjacency(Index, Count, Entries, [[]| PairWeights]) :-
		NextIndex is Index + 1,
		fill_weighted_adjacency(NextIndex, Count, Entries, PairWeights).

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
			Components = [Component| Components0],
			collect_components(Indices, ReverseAdjacency, Visited1, Visited, Components0)
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

	item_denominator([], _StrengthDictionary, _CurrentStrength, Denominator, Denominator).
	item_denominator([NeighborIndex-PairWeight| Neighbors], StrengthDictionary, CurrentStrength, Denominator0, Denominator) :-
		dictionary_lookup(NeighborIndex, OtherStrength, StrengthDictionary),
		Denominator1 is Denominator0 + PairWeight / (CurrentStrength + OtherStrength),
		item_denominator(Neighbors, StrengthDictionary, CurrentStrength, Denominator1, Denominator).

:- end_category.
