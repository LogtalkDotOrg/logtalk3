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


:- object(rank_centrality,
	imports([ranking_dataset_common, score_ranker_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Rank Centrality pairwise preference ranker. Learns one stationary probability score per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by applying power iteration to the Rank Centrality Markov chain built from aggregated head-to-head outcomes, and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, bradley_terry, copeland]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_dictionary/2, as_list/2 as dictionary_as_list/2, insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, nth1/3, reverse/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		(	Items = [Item] ->
			singleton_ranker(Item, Options, DatasetSummary, Ranker)
		;	^^pairwise_dataset_matchups(Dataset, Matchups),
			length(Items, Count),
			index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency0, ReverseAdjacency0, IncomingAdjacency0, OutgoingMasses0, MaximumDegree),
			validate_rank_centrality_dataset(Items, Count, DirectedAdjacency0, ReverseAdjacency0),
			build_transition_model(Count, IncomingAdjacency0, OutgoingMasses0, IncomingAdjacency, OutgoingMasses),
			initial_scores(Items, ScoreValues0),
			^^option(maximum_iterations(MaximumIterations), Options),
			^^option(tolerance(Tolerance), Options),
			optimize_scores(MaximumIterations, Tolerance, IncomingAdjacency, OutgoingMasses, MaximumDegree, ScoreValues0, ScoreValues, Status, Iterations, FinalDifference),
			score_pairs(Items, ScoreValues, Scores),
			Ranker = rank_centrality_ranker(Items, Scores, [
				model(rank_centrality),
				options(Options),
				convergence(Status),
				iterations(Iterations),
				final_delta(FinalDifference),
				maximum_degree(MaximumDegree),
				dataset_summary(DatasetSummary)
			])
		).

	rank(Ranker, Candidates, Ranking) :-
		ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	ranker_scores_data(Ranker, Scores) :-
		ranker_data(Ranker, _Items, Scores, _Diagnostics).

	ranker_diagnostics_data(Ranker, Diagnostics) :-
		ranker_data(Ranker, _Items, _Scores, Diagnostics).

	ranker_export_template(_Dataset, _Ranker, Functor, Template) :-
		Template =.. [Functor, 'Ranker'].

	ranker_term_template(rank_centrality_ranker(_Items, _Scores, _Diagnostics), rank_centrality_ranker('Items', 'Scores', 'Diagnostics')).

	export_to_clauses(_Dataset, Ranker, Functor, [Clause]) :-
		Clause =.. [Functor, Ranker].

	print_ranker(Ranker) :-
		ranker_data(Ranker, Items, Scores, Diagnostics),
		length(Items, Count),
		format('Rank Centrality ranker for ~d items~n', [Count]),
		^^print_ranker_template(Ranker),
		forall(
			member(Item-Score, Scores),
			format('  ~q: ~8f~n', [Item, Score])
		),
		format('Diagnostics: ~q~n', [Diagnostics]).

	ranker_data(Ranker, Items, Scores, Diagnostics) :-
		(	var(Ranker) ->
			instantiation_error
		;	Ranker = rank_centrality_ranker(Items, Scores, Diagnostics),
			valid_ranker_data(Items, Scores, Diagnostics) ->
			true
		;	domain_error(rank_centrality_ranker, Ranker)
		).

	valid_ranker_data(Items, Scores, Diagnostics) :-
		^^valid_item_value_pairs(Items, Scores),
		valid_probability_values(Scores),
		^^valid_ranker_metadata(rank_centrality, Diagnostics),
		memberchk(convergence(Status), Diagnostics),
		atom(Status),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 0,
		memberchk(final_delta(FinalDifference), Diagnostics),
		number(FinalDifference),
		FinalDifference >= 0.0,
		memberchk(maximum_degree(MaximumDegree), Diagnostics),
		integer(MaximumDegree),
		MaximumDegree >= 0.

	valid_probability_values([]).
	valid_probability_values([_Item-Value| Pairs]) :-
		number(Value),
		Value > 0.0,
		Value =< 1.0,
		valid_probability_values(Pairs).

	singleton_ranker(Item, Options, DatasetSummary, rank_centrality_ranker([Item], [Item-1.0], [
		model(rank_centrality),
		options(Options),
		convergence(converged),
		iterations(0),
		final_delta(0.0),
		maximum_degree(0),
		dataset_summary(DatasetSummary)
	])).

	initial_scores(Items, Scores) :-
		length(Items, Count),
		Score is 1.0 / Count,
		fill_scores(Count, Score, Scores).

	fill_scores(0, _Score, []) :-
		!.
	fill_scores(Count, Score, [Score| Scores]) :-
		Count > 0,
		NextCount is Count - 1,
		fill_scores(NextCount, Score, Scores).

	score_pairs([], [], []).
	score_pairs([Item| Items], [Score| Values], [Item-Score| Scores]) :-
		score_pairs(Items, Values, Scores).

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency, ReverseAdjacency, IncomingAdjacency, OutgoingMasses, MaximumDegree) :-
		dictionary_new(DirectedAdjacency0),
		dictionary_new(ReverseAdjacency0),
		dictionary_new(IncomingAdjacency0),
		dictionary_new(OutgoingMasses0),
		dictionary_new(Degrees0),
		preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, IncomingAdjacency0, IncomingAdjacency, OutgoingMasses0, OutgoingMasses, Degrees0, _Degrees, 0, MaximumDegree).

	preprocess_matchups([], _IndexDictionary, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, IncomingAdjacency, IncomingAdjacency, OutgoingMasses, OutgoingMasses, Degrees, Degrees, MaximumDegree, MaximumDegree).
	preprocess_matchups([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], IndexDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, IncomingAdjacency0, IncomingAdjacency, OutgoingMasses0, OutgoingMasses, Degrees0, Degrees, MaximumDegree0, MaximumDegree) :-
		dictionary_lookup(Item1, Item1Index, IndexDictionary),
		dictionary_lookup(Item2, Item2Index, IndexDictionary),
		update_degree_dictionary(Degrees0, Item1Index, Degrees1, Degree1),
		update_degree_dictionary(Degrees1, Item2Index, Degrees2, Degree2),
		max_degree(Degree1, Degree2, MaximumDegree0, MaximumDegree1),
		TotalWeight is Item1Wins + Item2Wins,
		update_transition(Item2Wins, TotalWeight, Item1Index, Item2Index, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1, IncomingAdjacency0, IncomingAdjacency1, OutgoingMasses0, OutgoingMasses1),
		update_transition(Item1Wins, TotalWeight, Item2Index, Item1Index, DirectedAdjacency1, DirectedAdjacency2, ReverseAdjacency1, ReverseAdjacency2, IncomingAdjacency1, IncomingAdjacency2, OutgoingMasses1, OutgoingMasses2),
		preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency2, DirectedAdjacency, ReverseAdjacency2, ReverseAdjacency, IncomingAdjacency2, IncomingAdjacency, OutgoingMasses2, OutgoingMasses, Degrees2, Degrees, MaximumDegree1, MaximumDegree).

	update_transition(Wins, TotalWeight, SourceIndex, TargetIndex, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, IncomingAdjacency0, IncomingAdjacency, OutgoingMasses0, OutgoingMasses) :-
		(	Wins > 0 ->
			Probability is Wins / TotalWeight,
			update_neighbor_dictionary(DirectedAdjacency0, SourceIndex, TargetIndex, DirectedAdjacency),
			update_neighbor_dictionary(ReverseAdjacency0, TargetIndex, SourceIndex, ReverseAdjacency),
			update_neighbor_weight_dictionary(IncomingAdjacency0, TargetIndex, SourceIndex, Probability, IncomingAdjacency),
			update_weight_dictionary(OutgoingMasses0, SourceIndex, Probability, OutgoingMasses)
		;	DirectedAdjacency = DirectedAdjacency0,
			ReverseAdjacency = ReverseAdjacency0,
			IncomingAdjacency = IncomingAdjacency0,
			OutgoingMasses = OutgoingMasses0
		).

	update_neighbor_dictionary(Dictionary0, Key, Neighbor, Dictionary) :-
		(	dictionary_lookup(Key, Neighbors0, Dictionary0) ->
			Neighbors = [Neighbor| Neighbors0]
		;	Neighbors = [Neighbor]
		),
		dictionary_insert(Dictionary0, Key, Neighbors, Dictionary).

	update_neighbor_weight_dictionary(Dictionary0, Key, Neighbor, Delta, Dictionary) :-
		(	dictionary_lookup(Key, NeighborWeights0, Dictionary0) ->
			true
		;	dictionary_new(NeighborWeights0)
		),
		update_weight_dictionary(NeighborWeights0, Neighbor, Delta, NeighborWeights),
		dictionary_insert(Dictionary0, Key, NeighborWeights, Dictionary).

	update_weight_dictionary(Dictionary0, Key, Delta, Dictionary) :-
		(	dictionary_lookup(Key, Weight0, Dictionary0) ->
			Weight is Weight0 + Delta
		;	Weight = Delta
		),
		dictionary_insert(Dictionary0, Key, Weight, Dictionary).

	update_degree_dictionary(Dictionary0, Key, Dictionary, Degree) :-
		(	dictionary_lookup(Key, Degree0, Dictionary0) ->
			Degree is Degree0 + 1
		;	Degree = 1
		),
		dictionary_insert(Dictionary0, Key, Degree, Dictionary).

	max_degree(Degree1, Degree2, MaximumDegree0, MaximumDegree) :-
		(	Degree1 > MaximumDegree0 ->
			MaximumDegree1 = Degree1
		;	MaximumDegree1 = MaximumDegree0
		),
		(	Degree2 > MaximumDegree1 ->
			MaximumDegree = Degree2
		;	MaximumDegree = MaximumDegree1
		).

	build_transition_model(Count, IncomingAdjacency0, OutgoingMasses0, IncomingAdjacency, OutgoingMasses) :-
		dictionary_as_list(OutgoingMasses0, OutgoingEntries),
		fill_weight_vector(1, Count, OutgoingEntries, OutgoingMasses),
		dictionary_as_list(IncomingAdjacency0, IncomingEntries),
		fill_weighted_adjacency(1, Count, IncomingEntries, IncomingAdjacency).

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
	fill_weighted_adjacency(Index, Count, [Index-NeighborWeights| Entries], [Neighbors| IncomingAdjacency]) :-
		!,
		dictionary_as_list(NeighborWeights, Neighbors),
		NextIndex is Index + 1,
		fill_weighted_adjacency(NextIndex, Count, Entries, IncomingAdjacency).
	fill_weighted_adjacency(Index, Count, Entries, [[]| IncomingAdjacency]) :-
		NextIndex is Index + 1,
		fill_weighted_adjacency(NextIndex, Count, Entries, IncomingAdjacency).

	validate_rank_centrality_dataset(Items, Count, DirectedAdjacency0, ReverseAdjacency0) :-
		(	strongly_connected(Count, DirectedAdjacency0, ReverseAdjacency0) ->
			true
		;	strongly_connected_components(Count, DirectedAdjacency0, ReverseAdjacency0, Components0),
			components_items(Components0, Items, Components),
			domain_error(rank_centrality_regular_dataset, Components)
		).

	components_items([], _Items, []).
	components_items([Component0| Components0], Items, [Component| Components]) :-
		component_items(Component0, Items, Component),
		components_items(Components0, Items, Components).

	component_items([], _Items, []).
	component_items([Index| Indices], Items, [Item| Component]) :-
		nth1(Index, Items, Item),
		component_items(Indices, Items, Component).

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
		collect_components(Order, ReverseAdjacency, ReverseVisited0, [], _ReverseVisited, Components0),
		reverse(Components0, Components).

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

	collect_components([], _ReverseAdjacency, Visited, Components, Visited, Components).
	collect_components([Index| Indices], ReverseAdjacency, Visited0, Components0, Visited, Components) :-
		(	dictionary_lookup(Index, _Seen, Visited0) ->
			Visited1 = Visited0,
			Components1 = Components0
		;	dfs_collect(Index, ReverseAdjacency, Visited0, [], Visited1, Component),
			Components1 = [Component| Components0]
		),
		collect_components(Indices, ReverseAdjacency, Visited1, Components1, Visited, Components).

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

	optimize_scores(MaximumIterations, Tolerance, IncomingAdjacency, OutgoingMasses, MaximumDegree, Scores0, Scores, Status, Iterations, FinalDifference) :-
		optimize_scores(0, MaximumIterations, Tolerance, IncomingAdjacency, OutgoingMasses, MaximumDegree, Scores0, Scores, Status, Iterations, FinalDifference).

	optimize_scores(Iteration0, MaximumIterations, Tolerance, IncomingAdjacency, OutgoingMasses, MaximumDegree, Scores0, Scores, Status, Iterations, FinalDifference) :-
		update_scores(IncomingAdjacency, OutgoingMasses, MaximumDegree, Scores0, Scores1, MaximumDifference),
		Iteration is Iteration0 + 1,
		(	MaximumDifference =< Tolerance ->
			Scores = Scores1,
			Status = converged,
			Iterations = Iteration,
			FinalDifference = MaximumDifference
		;	Iteration >= MaximumIterations ->
			Scores = Scores1,
			Status = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDifference = MaximumDifference
		;	optimize_scores(Iteration, MaximumIterations, Tolerance, IncomingAdjacency, OutgoingMasses, MaximumDegree, Scores1, Scores, Status, Iterations, FinalDifference)
		).

	update_scores(IncomingAdjacency, OutgoingMasses, MaximumDegree, Scores0, Scores, MaximumDifference) :-
		score_dictionary(Scores0, ScoreDictionary),
		update_score_values(IncomingAdjacency, OutgoingMasses, MaximumDegree, ScoreDictionary, Scores0, RawScores, 0.0, TotalRawScore),
		normalize_scores(RawScores, TotalRawScore, Scores0, Scores, MaximumDifference).

	score_dictionary(Scores, Dictionary) :-
		dictionary_new(Dictionary0),
		score_dictionary(Scores, 1, Dictionary0, Dictionary).

	score_dictionary([], _Index, Dictionary, Dictionary).
	score_dictionary([Score| Scores], Index, Dictionary0, Dictionary) :-
		dictionary_insert(Dictionary0, Index, Score, Dictionary1),
		NextIndex is Index + 1,
		score_dictionary(Scores, NextIndex, Dictionary1, Dictionary).

	update_score_values([], [], _MaximumDegree, _ScoreDictionary, [], [], TotalRawScore, TotalRawScore).
	update_score_values([IncomingNeighbors| IncomingAdjacency], [OutgoingMass| OutgoingMasses], MaximumDegree, ScoreDictionary, [CurrentScore| CurrentScores], [RawScore| RawScores], TotalRawScore0, TotalRawScore) :-
		incoming_mass(IncomingNeighbors, ScoreDictionary, 0.0, IncomingMass0),
		StayWeight is 1.0 - OutgoingMass / MaximumDegree,
		RawScore is CurrentScore * StayWeight + IncomingMass0 / MaximumDegree,
		TotalRawScore1 is TotalRawScore0 + RawScore,
		update_score_values(IncomingAdjacency, OutgoingMasses, MaximumDegree, ScoreDictionary, CurrentScores, RawScores, TotalRawScore1, TotalRawScore).

	incoming_mass([], _ScoreDictionary, IncomingMass, IncomingMass).
	incoming_mass([SourceIndex-Weight| IncomingNeighbors], ScoreDictionary, IncomingMass0, IncomingMass) :-
		dictionary_lookup(SourceIndex, SourceScore, ScoreDictionary),
		IncomingMass1 is IncomingMass0 + SourceScore * Weight,
		incoming_mass(IncomingNeighbors, ScoreDictionary, IncomingMass1, IncomingMass).

	normalize_scores(RawScores, TotalRawScore, Scores0, Scores, MaximumDifference) :-
		(	abs(TotalRawScore - 1.0) =< 1.0e-12 ->
			normalize_scores_identity(RawScores, Scores0, Scores, 0.0, MaximumDifference)
		;	normalize_scores_scaled(RawScores, TotalRawScore, Scores0, Scores, 0.0, MaximumDifference)
		).

	normalize_scores_identity([], [], [], MaximumDifference, MaximumDifference).
	normalize_scores_identity([RawScore| RawScores], [Score0| Scores0], [RawScore| Scores], MaximumDifference0, MaximumDifference) :-
		Difference is abs(Score0 - RawScore),
		(	Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;	MaximumDifference1 = MaximumDifference0
		),
		normalize_scores_identity(RawScores, Scores0, Scores, MaximumDifference1, MaximumDifference).

	normalize_scores_scaled([], _TotalRawScore, [], [], MaximumDifference, MaximumDifference).
	normalize_scores_scaled([RawScore| RawScores], TotalRawScore, [Score0| Scores0], [Score| Scores], MaximumDifference0, MaximumDifference) :-
		Score is RawScore / TotalRawScore,
		Difference is abs(Score0 - Score),
		(	Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;	MaximumDifference1 = MaximumDifference0
		),
		normalize_scores_scaled(RawScores, TotalRawScore, Scores0, Scores, MaximumDifference1, MaximumDifference).

	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.

	default_option(maximum_iterations(5000)).
	default_option(tolerance(1.0e-8)).

:- end_object.
