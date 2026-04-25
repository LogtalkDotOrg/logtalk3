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


:- object(bradley_terry,
	imports([options, ranking_dataset_common, ranker_common])).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-04-24,
		comment is 'Bradley-Terry pairwise preference ranker. Learns one positive strength parameter per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol when the directed win graph admits a finite Bradley-Terry maximum-likelihood estimate, and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		remarks is [
			'Algorithm' - 'Uses a deterministic minorization-maximization update to estimate one relative strength parameter per item from weighted pairwise wins and losses.',
			'Dataset requirements' - 'The training dataset must declare each ranked item once, enumerate positive-weight pairwise preferences between distinct declared items, induce a connected undirected comparison graph, and induce a strongly connected directed win graph so that a finite Bradley-Terry maximum-likelihood estimate exists.',
			'Ranker representation' - 'The learned ranker is represented by default as ``bt_ranker(Items, Strengths, Diagnostics)`` where ``Strengths`` stores ``Item-Strength`` pairs and ``Diagnostics`` stores metadata such as convergence status, iteration count, and dataset summary.'
		],
		see_also is [pairwise_ranking_dataset_protocol, ranking_dataset_protocol, ranker_protocol]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a Bradley-Terry ranker from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Ranker', 'Options']
	]).

	:- public(strengths/2).
	:- mode(strengths(+compound, -list(pair)), one).
	:- info(strengths/2, [
		comment is 'Returns the learned item-strength pairs.',
		argnames is ['Ranker', 'Strengths']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_dictionary/2,
		as_list/2 as dictionary_as_list/2,
		insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		keysort/2, length/2, member/2, reverse/2, sort/4
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(pairs, [
		values/2 as pairs_values/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_preferences(Dataset, Preferences),
		length(Items, Count),
		index_items(Items, 1, IndexPairs),
		as_dictionary(IndexPairs, IndexDictionary),
		preprocess_preferences(Preferences, IndexDictionary, DirectedAdjacency0, ReverseAdjacency0, WinWeights0, PairAdjacency0),
		validate_bradley_terry_dataset(Count, DirectedAdjacency0, ReverseAdjacency0),
		build_dataset_model(Count, WinWeights0, PairAdjacency0, PairWeights, Wins),
		initial_strengths(Items, StrengthValues0),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		optimize_strengths(MaximumIterations, Tolerance, PairWeights, Wins, StrengthValues0, StrengthValues, Status, Iterations, FinalDifference),
		strength_pairs(Items, StrengthValues, Strengths),
		Ranker = bt_ranker(Items, Strengths, [
			model(bradley_terry),
			options(Options),
			convergence(Status),
			iterations(Iterations),
			final_delta(FinalDifference),
			dataset_summary(DatasetSummary)
		]).

	rank(Ranker, Candidates, Ranking) :-
		ranker_data(Ranker, _Items, Strengths, _Diagnostics),
		as_dictionary(Strengths, StrengthDictionary),
		dictionary_new(SeenCandidates),
		rank_candidate_pairs(Candidates, StrengthDictionary, SeenCandidates, Candidates, Pairs),
		sort(1, @=<, Pairs, SortedPairs),
		pairs_values(SortedPairs, Ranking).

	strengths(Ranker, Strengths) :-
		ranker_data(Ranker, _Items, Strengths, _Diagnostics).

	ranker_diagnostics_data(Ranker, Diagnostics) :-
		ranker_data(Ranker, _Items, _Strengths, Diagnostics).

	ranker_export_template(_Dataset, _Ranker, Functor, Template) :-
		Template =.. [Functor, 'Ranker'].

	ranker_term_template(bt_ranker(_Items, _Strengths, _Diagnostics), bt_ranker('Items', 'Strengths', 'Diagnostics')).

	export_to_clauses(_Dataset, Ranker, Functor, [Clause]) :-
		Clause =.. [Functor, Ranker].

	print_ranker(Ranker) :-
		ranker_data(Ranker, Items, Strengths, Diagnostics),
		length(Items, Count),
		format('Bradley-Terry ranker for ~d items~n', [Count]),
		^^print_ranker_template(Ranker),
		forall(
			member(Item-Strength, Strengths),
			format('  ~q: ~6f~n', [Item, Strength])
		),
		format('Diagnostics: ~q~n', [Diagnostics]).

	ranker_data(Ranker, Items, Strengths, Diagnostics) :-
		(   var(Ranker) ->
			instantiation_error
		;   Ranker = bt_ranker(Items, Strengths, Diagnostics),
			valid_ranker_data(Items, Strengths) ->
			true
		;   domain_error(bradley_terry_ranker, Ranker)
		).

	valid_ranker_data(Items, Strengths) :-
		proper_item_list(Items),
		strength_pairs_values(Items, Strengths, _Values).

	proper_item_list(Items) :-
		proper_item_list(Items, []).

	proper_item_list(Items, _Seen) :-
		var(Items),
		!,
		fail.
	proper_item_list([], _Seen).
	proper_item_list([Item| Items], Seen) :-
		nonvar(Item),
		\+ member(Item, Seen),
		proper_item_list(Items, [Item| Seen]).

	strength_pairs_values([], [], []).
	strength_pairs_values([Item| Items], [StrengthItem-Strength| Strengths], [Strength| Values]) :-
		Item == StrengthItem,
		number(Strength),
		Strength > 0.0,
		strength_pairs_values(Items, Strengths, Values).

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

	build_dataset_model(Count, WinWeights0, PairAdjacency0, PairWeights, Wins) :-
		dictionary_as_list(WinWeights0, WinEntries),
		fill_weight_vector(1, Count, WinEntries, Wins),
		dictionary_as_list(PairAdjacency0, PairAdjacencyEntries),
		fill_weighted_adjacency(1, Count, PairAdjacencyEntries, PairWeights).

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	preprocess_preferences(Preferences, IndexDictionary, DirectedAdjacency, ReverseAdjacency, WinWeights, PairAdjacency) :-
		dictionary_new(DirectedAdjacency0),
		dictionary_new(ReverseAdjacency0),
		dictionary_new(WinWeights0),
		dictionary_new(PairAdjacency0),
		preprocess_preferences(Preferences, IndexDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, WinWeights0, WinWeights, PairAdjacency0, PairAdjacency).

	preprocess_preferences([], _IndexDictionary, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, WinWeights, WinWeights, PairAdjacency, PairAdjacency).
	preprocess_preferences([p(Winner, Loser, Weight)| Preferences], IndexDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, WinWeights0, WinWeights, PairAdjacency0, PairAdjacency) :-
		dictionary_lookup(Winner, WinnerIndex, IndexDictionary),
		dictionary_lookup(Loser, LoserIndex, IndexDictionary),
		ordered_pair(WinnerIndex, LoserIndex, LeftIndex, RightIndex),
		update_neighbor_dictionary(DirectedAdjacency0, WinnerIndex, LoserIndex, DirectedAdjacency1),
		update_neighbor_dictionary(ReverseAdjacency0, LoserIndex, WinnerIndex, ReverseAdjacency1),
		update_weight_dictionary(WinWeights0, WinnerIndex, Weight, WinWeights1),
		update_pair_adjacency_dictionary(PairAdjacency0, LeftIndex, RightIndex, Weight, PairAdjacency1),
		preprocess_preferences(Preferences, IndexDictionary, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency, WinWeights1, WinWeights, PairAdjacency1, PairAdjacency).

	update_neighbor_dictionary(Dictionary0, Key, Neighbor, Dictionary) :-
		(   dictionary_lookup(Key, Neighbors0, Dictionary0) ->
			Neighbors = [Neighbor| Neighbors0]
		;   Neighbors = [Neighbor]
		),
		dictionary_insert(Dictionary0, Key, Neighbors, Dictionary).

	update_weight_dictionary(Dictionary0, Key, Delta, Dictionary) :-
		(   dictionary_lookup(Key, Weight0, Dictionary0) ->
			Weight is Weight0 + Delta
		;   Weight = Delta
		),
		dictionary_insert(Dictionary0, Key, Weight, Dictionary).

	update_pair_adjacency_dictionary(Dictionary0, Left, Right, Delta, Dictionary) :-
		update_neighbor_weight_dictionary(Dictionary0, Left, Right, Delta, Dictionary1),
		update_neighbor_weight_dictionary(Dictionary1, Right, Left, Delta, Dictionary).

	update_neighbor_weight_dictionary(Dictionary0, Key, Neighbor, Delta, Dictionary) :-
		(   dictionary_lookup(Key, NeighborWeights0, Dictionary0) ->
			true
		;   dictionary_new(NeighborWeights0)
		),
		update_weight_dictionary(NeighborWeights0, Neighbor, Delta, NeighborWeights),
		dictionary_insert(Dictionary0, Key, NeighborWeights, Dictionary).

	ordered_pair(Left, Right, Left, Right) :-
		Left < Right,
		!.
	ordered_pair(Left, Right, Right, Left).

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

	validate_bradley_terry_dataset(Count, DirectedAdjacency0, ReverseAdjacency0) :-
		(   strongly_connected(Count, DirectedAdjacency0, ReverseAdjacency0) ->
			true
		;   strongly_connected_components(Count, DirectedAdjacency0, ReverseAdjacency0, Components),
			domain_error(bradley_terry_regular_dataset, Components)
		).

	strongly_connected(Count, DirectedAdjacency, ReverseAdjacency) :-
		reachable_all(Count, DirectedAdjacency),
		reachable_all(Count, ReverseAdjacency).

	reachable_all(Count, Adjacency) :-
		dictionary_new(Visited0),
		dfs_reach(1, Adjacency, Visited0, Visited),
		all_visited(1, Count, Visited).

	dfs_reach(Index, Adjacency, Visited0, Visited) :-
		(   dictionary_lookup(Index, _Seen, Visited0) ->
			Visited = Visited0
		;   dictionary_insert(Visited0, Index, true, Visited1),
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
		collect_components(Order, ReverseAdjacency, ReverseVisited0, [], _ReverseVisited, Components0),
		reverse(Components0, Components).

	finish_order(Index, Count, _DirectedAdjacency, Visited, Order, Visited, Order) :-
		Index > Count,
		!.
	finish_order(Index, Count, DirectedAdjacency, Visited0, Order0, Visited, Order) :-
		(   dictionary_lookup(Index, _Seen, Visited0) ->
			Visited1 = Visited0,
			Order1 = Order0
		;   dfs_finish(Index, DirectedAdjacency, Visited0, Order0, Visited1, Order1)
		),
		NextIndex is Index + 1,
		finish_order(NextIndex, Count, DirectedAdjacency, Visited1, Order1, Visited, Order).

	dfs_finish(Index, DirectedAdjacency, Visited0, Order0, Visited, [Index| Order]) :-
		dictionary_insert(Visited0, Index, true, Visited1),
		adjacency_neighbors(Index, DirectedAdjacency, Neighbors),
		dfs_finish_neighbors(Neighbors, DirectedAdjacency, Visited1, Order0, Visited, Order).

	dfs_finish_neighbors([], _DirectedAdjacency, Visited, Order, Visited, Order).
	dfs_finish_neighbors([Neighbor| Neighbors], DirectedAdjacency, Visited0, Order0, Visited, Order) :-
		(   dictionary_lookup(Neighbor, _Seen, Visited0) ->
			Visited1 = Visited0,
			Order1 = Order0
		;   dfs_finish(Neighbor, DirectedAdjacency, Visited0, Order0, Visited1, Order1)
		),
		dfs_finish_neighbors(Neighbors, DirectedAdjacency, Visited1, Order1, Visited, Order).

	collect_components([], _ReverseAdjacency, Visited, Components, Visited, Components).
	collect_components([Index| Indices], ReverseAdjacency, Visited0, Components0, Visited, Components) :-
		(   dictionary_lookup(Index, _Seen, Visited0) ->
			Visited1 = Visited0,
			Components1 = Components0
		;   dfs_collect(Index, ReverseAdjacency, Visited0, [], Visited1, Component),
			Components1 = [Component| Components0]
		),
		collect_components(Indices, ReverseAdjacency, Visited1, Components1, Visited, Components).

	dfs_collect(Index, ReverseAdjacency, Visited0, Component0, Visited, Component) :-
		dictionary_insert(Visited0, Index, true, Visited1),
		adjacency_neighbors(Index, ReverseAdjacency, Neighbors),
		dfs_collect_neighbors(Neighbors, ReverseAdjacency, Visited1, [Index| Component0], Visited, Component).

	dfs_collect_neighbors([], _ReverseAdjacency, Visited, Component, Visited, Component).
	dfs_collect_neighbors([Neighbor| Neighbors], ReverseAdjacency, Visited0, Component0, Visited, Component) :-
		(   dictionary_lookup(Neighbor, _Seen, Visited0) ->
			Visited1 = Visited0,
			Component1 = Component0
		;   dfs_collect(Neighbor, ReverseAdjacency, Visited0, Component0, Visited1, Component1)
		),
		dfs_collect_neighbors(Neighbors, ReverseAdjacency, Visited1, Component1, Visited, Component).

	adjacency_neighbors(Index, Adjacency, Neighbors) :-
		(   dictionary_lookup(Index, Neighbors, Adjacency) ->
			true
		;   Neighbors = []
		).

	optimize_strengths(MaximumIterations, Tolerance, PairWeights, Wins, Strengths0, Strengths, Status, Iterations, FinalDifference) :-
		optimize_strengths(0, MaximumIterations, Tolerance, PairWeights, Wins, Strengths0, Strengths, Status, Iterations, FinalDifference).

	optimize_strengths(Iteration0, MaximumIterations, Tolerance, PairWeights, Wins, Strengths0, Strengths, Status, Iterations, FinalDifference) :-
		update_strengths(PairWeights, Wins, Strengths0, Strengths1, MaximumDifference),
		Iteration is Iteration0 + 1,
		(   MaximumDifference =< Tolerance ->
			Strengths = Strengths1,
			Status = converged,
			Iterations = Iteration,
			FinalDifference = MaximumDifference
		;   Iteration >= MaximumIterations ->
			Strengths = Strengths1,
			Status = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDifference = MaximumDifference
		;   optimize_strengths(Iteration, MaximumIterations, Tolerance, PairWeights, Wins, Strengths1, Strengths, Status, Iterations, FinalDifference)
		).

	update_strengths(PairWeights, Wins, Strengths0, Strengths, MaximumDifference) :-
		strength_dictionary(Strengths0, StrengthDictionary),
		update_strength_values(PairWeights, Wins, StrengthDictionary, Strengths0, RawStrengths, TotalRawStrength),
		normalize_strengths(RawStrengths, TotalRawStrength, Strengths0, Strengths, MaximumDifference).

	strength_dictionary(Strengths, Dictionary) :-
		dictionary_new(Dictionary0),
		strength_dictionary(Strengths, 1, Dictionary0, Dictionary).

	strength_dictionary([], _Index, Dictionary, Dictionary).
	strength_dictionary([Strength| Strengths], Index, Dictionary0, Dictionary) :-
		dictionary_insert(Dictionary0, Index, Strength, Dictionary1),
		NextIndex is Index + 1,
		strength_dictionary(Strengths, NextIndex, Dictionary1, Dictionary).

	update_strength_values([], [], _StrengthDictionary, [], [], 0.0).
	update_strength_values([Neighbors| PairWeights], [Wins| WinTotals], StrengthDictionary, [CurrentStrength| CurrentStrengths], [RawStrength| RawStrengths], TotalRawStrength) :-
		item_denominator(Neighbors, StrengthDictionary, CurrentStrength, 0.0, Denominator),
		RawStrength is Wins / Denominator,
		update_strength_values(PairWeights, WinTotals, StrengthDictionary, CurrentStrengths, RawStrengths, RemainingRawStrength),
		TotalRawStrength is RawStrength + RemainingRawStrength.

	item_denominator([], _StrengthDictionary, _CurrentStrength, Denominator, Denominator).
	item_denominator([NeighborIndex-PairWeight| Neighbors], StrengthDictionary, CurrentStrength, Denominator0, Denominator) :-
		dictionary_lookup(NeighborIndex, OtherStrength, StrengthDictionary),
		Denominator1 is Denominator0 + PairWeight / (CurrentStrength + OtherStrength),
		item_denominator(Neighbors, StrengthDictionary, CurrentStrength, Denominator1, Denominator).

	normalize_strengths(RawStrengths, TotalRawStrength, Strengths0, Strengths, MaximumDifference) :-
		(   TotalRawStrength =< 1.0e-12 ->
			normalize_strengths_identity(RawStrengths, Strengths0, Strengths, 0.0, MaximumDifference)
		;   normalize_strengths_scaled(RawStrengths, TotalRawStrength, Strengths0, Strengths, 0.0, MaximumDifference)
		).

	normalize_strengths_identity([], [], [], MaximumDifference, MaximumDifference).
	normalize_strengths_identity([RawStrength| RawStrengths], [Strength0| Strengths0], [RawStrength| Strengths], MaximumDifference0, MaximumDifference) :-
		Difference is abs(Strength0 - RawStrength),
		(   Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;   MaximumDifference1 = MaximumDifference0
		),
		normalize_strengths_identity(RawStrengths, Strengths0, Strengths, MaximumDifference1, MaximumDifference).

	normalize_strengths_scaled([], _TotalRawStrength, [], [], MaximumDifference, MaximumDifference).
	normalize_strengths_scaled([RawStrength| RawStrengths], TotalRawStrength, [Strength0| Strengths0], [Strength| Strengths], MaximumDifference0, MaximumDifference) :-
		Strength is RawStrength / TotalRawStrength,
		Difference is abs(Strength0 - Strength),
		(   Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;   MaximumDifference1 = MaximumDifference0
		),
		normalize_strengths_scaled(RawStrengths, TotalRawStrength, Strengths0, Strengths, MaximumDifference1, MaximumDifference).

	rank_candidate_pairs(Candidates, _StrengthDictionary, _SeenCandidates, _Original, _Pairs) :-
		var(Candidates),
		!,
		instantiation_error.
	rank_candidate_pairs([], _StrengthDictionary, _SeenCandidates, _Original, []) :-
		!.
	rank_candidate_pairs([Candidate| Candidates], StrengthDictionary, SeenCandidates0, Original, [pair(NegStrength, Candidate)-Candidate| Pairs]) :-
		!,
		(   var(Candidate) ->
			instantiation_error
		;   dictionary_lookup(Candidate, _Seen, SeenCandidates0) ->
			domain_error(unique_candidates, Original)
		;   dictionary_lookup(Candidate, Strength, StrengthDictionary) ->
			dictionary_insert(SeenCandidates0, Candidate, true, SeenCandidates),
			NegStrength is -Strength,
			rank_candidate_pairs(Candidates, StrengthDictionary, SeenCandidates, Original, Pairs)
		;   existence_error(item, Candidate)
		).
	rank_candidate_pairs(Candidates, _StrengthDictionary, _SeenCandidates, _Original, _Pairs) :-
		type_error(list, Candidates).

	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.

	default_option(maximum_iterations(5000)).
	default_option(tolerance(1.0e-6)).

:- end_object.
