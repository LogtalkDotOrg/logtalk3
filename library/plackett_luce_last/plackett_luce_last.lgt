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


:- object(plackett_luce_last,
	imports([ranking_dataset_common, grouped_strength_ranker_common, score_ranker_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Tie-aware Plackett-Luce-last grouped-ranking ranker. Learns one positive preference strength parameter per item from a dataset object implementing the ``ranking_dataset_protocol`` protocol by fitting a last-choice Plackett-Luce model to grouped tie blocks, and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		remarks is [
			'Algorithm' - 'Processes each group as a sequence of last-choice eliminations from lowest relevance to highest relevance, using grouped tie blocks and a deterministic fixed-point update on positive item strengths.',
			'Tie handling' - 'Equal relevance judgments inside a group are treated as unordered tie blocks. Each tie block contributes a size-constrained last-choice likelihood term against the remaining higher-relevance items.',
			'Dataset requirements' - 'The training dataset must declare each group once, use only declared groups and items in relevance judgments, assign non-negative integer relevance values, and induce a strongly connected directed strict-order graph across groups so that a finite Plackett-Luce-last maximum-likelihood estimate exists.',
			'Missing relevance semantics' - 'Missing relevance facts are treated as zero by default using the ``missing_relevance(zero)`` option and can be rejected using ``missing_relevance(error)``.',
			'Ranker representation' - 'The learned ranker is represented by default as ``plackett_luce_last_ranker(Items, Strengths, Diagnostics)`` where ``Strengths`` stores normalized ``Item-Strength`` pairs and ``Diagnostics`` stores metadata such as convergence status, iteration count, and dataset summary.'
		],
		see_also is [ranking_dataset_protocol, ranker_protocol, borda]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_dictionary/2, insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		append/3, drop/3, length/2, member/2, nth0/3, reverse/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_grouped_dataset(Dataset, DatasetSummary),
		^^grouped_dataset_items(Dataset, Items),
		^^grouped_dataset_groups(Dataset, Groups),
		^^option(missing_relevance(MissingRelevance), Options),
		(	Items = [Item] ->
			validate_singleton_groups(Groups, Dataset, MissingRelevance),
			singleton_ranker(Item, Options, DatasetSummary, Ranker)
		;
			length(Items, Count),
			index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			preprocess_groups(Groups, Dataset, MissingRelevance, IndexDictionary, Count, Steps, SelectionCounts, DirectedAdjacency, ReverseAdjacency),
			^^require_strong_connectivity(Count, DirectedAdjacency, ReverseAdjacency, plackett_luce_last_regular_dataset),
			^^initial_strengths(Items, StrengthValues0),
			^^option(maximum_iterations(MaximumIterations), Options),
			^^option(tolerance(Tolerance), Options),
			^^optimize_strengths(MaximumIterations, Tolerance, plackett_luce_last_context(Steps, SelectionCounts), StrengthValues0, StrengthValues, Status, Iterations, FinalDifference),
			^^strength_pairs(Items, StrengthValues, Strengths),
			Ranker = plackett_luce_last_ranker(Items, Strengths, [
				model(plackett_luce_last),
				options(Options),
				convergence(Status),
				iterations(Iterations),
				final_delta(FinalDifference),
				dataset_summary(DatasetSummary)
			])
		).

	rank(Ranker, Candidates, Ranking) :-
		ranker_data(Ranker, _Items, Strengths, _Diagnostics),
		^^rank_by_scores(Strengths, Candidates, Ranking).

	ranker_scores_data(Ranker, Scores) :-
		ranker_data(Ranker, _Items, Scores, _Diagnostics).

	ranker_diagnostics_data(Ranker, Diagnostics) :-
		ranker_data(Ranker, _Items, _Strengths, Diagnostics).

	ranker_export_template(_Dataset, _Ranker, Functor, Template) :-
		Template =.. [Functor, 'Ranker'].

	ranker_term_template(plackett_luce_last_ranker(_Items, _Strengths, _Diagnostics), plackett_luce_last_ranker('Items', 'Strengths', 'Diagnostics')).

	export_to_clauses(_Dataset, Ranker, Functor, [Clause]) :-
		Clause =.. [Functor, Ranker].

	print_ranker(Ranker) :-
		ranker_data(Ranker, Items, Strengths, Diagnostics),
		length(Items, Count),
		format('Plackett-Luce-last ranker for ~d items~n', [Count]),
		^^print_ranker_template(Ranker),
		forall(
			member(Item-Strength, Strengths),
			format('  ~q: ~6f~n', [Item, Strength])
		),
		format('Diagnostics: ~q~n', [Diagnostics]).

	ranker_data(Ranker, Items, Strengths, Diagnostics) :-
		(	var(Ranker) ->
			instantiation_error
		;	Ranker = plackett_luce_last_ranker(Items, Strengths, Diagnostics) ->
			true
		;	domain_error(plackett_luce_last_ranker, Ranker)
		).

	singleton_ranker(Item, Options, DatasetSummary, plackett_luce_last_ranker([Item], [Item-1.0], [
		model(plackett_luce_last),
		options(Options),
		convergence(converged),
		iterations(0),
		final_delta(0.0),
		dataset_summary(DatasetSummary)
	])).

	validate_singleton_groups([], _Dataset, _MissingRelevance).
	validate_singleton_groups([Group| Groups], Dataset, MissingRelevance) :-
		^^grouped_dataset_item_relevances(Dataset, Group, MissingRelevance, _ItemRelevances),
		validate_singleton_groups(Groups, Dataset, MissingRelevance).

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	preprocess_groups(Groups, Dataset, MissingRelevance, IndexDictionary, Count, Steps, SelectionCounts, DirectedAdjacency, ReverseAdjacency) :-
		dictionary_new(SelectionCounts0),
		dictionary_new(DirectedAdjacency0),
		dictionary_new(ReverseAdjacency0),
		preprocess_groups(Groups, Dataset, MissingRelevance, IndexDictionary, SelectionCounts0, SelectionCountsDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, Steps, []),
		count_vector(1, Count, SelectionCountsDictionary, SelectionCounts).

	preprocess_groups([], _Dataset, _MissingRelevance, _IndexDictionary, SelectionCounts, SelectionCounts, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, Steps, Steps).
	preprocess_groups([Group| Groups], Dataset, MissingRelevance, IndexDictionary, SelectionCounts0, SelectionCounts, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, Steps0, Steps) :-
		^^grouped_dataset_tie_blocks(Dataset, Group, MissingRelevance, TieBlocksDescending),
		reverse(TieBlocksDescending, TieBlocksAscending),
		tie_block_indices(TieBlocksAscending, IndexDictionary, BlocksAscending),
		blocks_indices(BlocksAscending, RemainingIndices),
		group_steps(BlocksAscending, RemainingIndices, SelectionCounts0, SelectionCounts1, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1, Steps0, Steps1),
		preprocess_groups(Groups, Dataset, MissingRelevance, IndexDictionary, SelectionCounts1, SelectionCounts, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency, Steps1, Steps).

	tie_block_indices([], _IndexDictionary, []).
	tie_block_indices([tie_block(_Relevance, Items)| TieBlocks], IndexDictionary, [Indices| Blocks]) :-
		block_indices(Items, IndexDictionary, Indices),
		tie_block_indices(TieBlocks, IndexDictionary, Blocks).

	block_indices([], _IndexDictionary, []).
	block_indices([Item| Items], IndexDictionary, [Index| Indices]) :-
		dictionary_lookup(Item, Index, IndexDictionary),
		block_indices(Items, IndexDictionary, Indices).

	blocks_indices([], []).
	blocks_indices([Block| Blocks], Indices) :-
		append(Block, Rest, Indices),
		blocks_indices(Blocks, Rest).

	group_steps([_TopBlock], _RemainingIndices, SelectionCounts, SelectionCounts, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, Steps, Steps) :-
		!.
	group_steps([Block| Blocks], RemainingIndices, SelectionCounts0, SelectionCounts, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, [step(Block, RemainingIndices, BlockSize)| Steps0], Steps) :-
		length(Block, BlockSize),
		increment_selection_counts(Block, SelectionCounts0, SelectionCounts1),
		drop(BlockSize, RemainingIndices, BetterIndices),
		add_directed_edges(Block, BetterIndices, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1),
		group_steps(Blocks, BetterIndices, SelectionCounts1, SelectionCounts, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency, Steps0, Steps).

	increment_selection_counts([], SelectionCounts, SelectionCounts).
	increment_selection_counts([Index| Indices], SelectionCounts0, SelectionCounts) :-
		update_weight_dictionary(SelectionCounts0, Index, 1, SelectionCounts1),
		increment_selection_counts(Indices, SelectionCounts1, SelectionCounts).

	add_directed_edges([], _BetterIndices, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency).
	add_directed_edges([Index| Indices], BetterIndices, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency) :-
		add_directed_neighbors(BetterIndices, Index, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1),
		add_directed_edges(Indices, BetterIndices, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency).

	add_directed_neighbors([], _Index, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency).
	add_directed_neighbors([BetterIndex| BetterIndices], Index, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency) :-
		update_neighbor_dictionary(DirectedAdjacency0, Index, BetterIndex, DirectedAdjacency1),
		update_neighbor_dictionary(ReverseAdjacency0, BetterIndex, Index, ReverseAdjacency1),
		add_directed_neighbors(BetterIndices, Index, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency).

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

	count_vector(Index, Count, _SelectionCountsDictionary, []) :-
		Index > Count,
		!.
	count_vector(Index, Count, SelectionCountsDictionary, [SelectionCount| SelectionCounts]) :-
		(	dictionary_lookup(Index, SelectionCount0, SelectionCountsDictionary) ->
			SelectionCount = SelectionCount0
		;	SelectionCount = 0
		),
		NextIndex is Index + 1,
		count_vector(NextIndex, Count, SelectionCountsDictionary, SelectionCounts).

	update_strengths(plackett_luce_last_context(Steps, SelectionCounts), Strengths0, Strengths, MaximumDifference) :-
		inverse_strengths(Strengths0, BadnessValues),
		^^strength_dictionary(BadnessValues, BadnessDictionary),
		dictionary_new(ExposureDictionary0),
		selection_exposures(Steps, BadnessDictionary, ExposureDictionary0, ExposureDictionary),
		exposure_vector(SelectionCounts, 1, ExposureDictionary, ExposureCounts),
		raw_strengths(SelectionCounts, ExposureCounts, RawStrengths, 0.0, TotalRawStrength),
		normalize_strengths(RawStrengths, TotalRawStrength, Strengths0, Strengths, 0.0, MaximumDifference).

	inverse_strengths([], []).
	inverse_strengths([Strength| Strengths], [Badness| Badnesses]) :-
		Badness is 1.0 / Strength,
		inverse_strengths(Strengths, Badnesses).

	selection_exposures([], _BadnessDictionary, ExposureDictionary, ExposureDictionary).
	selection_exposures([step(_Block, RemainingIndices, BlockSize)| Steps], BadnessDictionary, ExposureDictionary0, ExposureDictionary) :-
		remaining_pairs(RemainingIndices, BadnessDictionary, Pairs),
		pairs_badness_values(Pairs, BadnessValues),
		tie_block_denominator(BadnessValues, BlockSize, Denominator),
		selection_exposures(Pairs, Pairs, BlockSize, Denominator, ExposureDictionary0, ExposureDictionary1),
		selection_exposures(Steps, BadnessDictionary, ExposureDictionary1, ExposureDictionary).

	remaining_pairs([], _BadnessDictionary, []).
	remaining_pairs([Index| Indices], BadnessDictionary, [Index-Badness| Pairs]) :-
		dictionary_lookup(Index, Badness, BadnessDictionary),
		remaining_pairs(Indices, BadnessDictionary, Pairs).

	pairs_badness_values([], []).
	pairs_badness_values([_Index-Badness| Pairs], [Badness| BadnessValues]) :-
		pairs_badness_values(Pairs, BadnessValues).

	selection_exposures([], _AllPairs, _BlockSize, _Denominator, ExposureDictionary, ExposureDictionary).
	selection_exposures([Index-_Badness| Pairs], AllPairs, BlockSize, Denominator, ExposureDictionary0, ExposureDictionary) :-
		other_badness_values(AllPairs, Index, OtherBadnessValues),
		LowerBlockSize is BlockSize - 1,
		tie_block_denominator(OtherBadnessValues, LowerBlockSize, Numerator),
		Delta is Numerator / Denominator,
		update_weight_dictionary(ExposureDictionary0, Index, Delta, ExposureDictionary1),
		selection_exposures(Pairs, AllPairs, BlockSize, Denominator, ExposureDictionary1, ExposureDictionary).

	other_badness_values([], _Index, []).
	other_badness_values([OtherIndex-Badness| Pairs], Index, BadnessValues) :-
		(	Index == OtherIndex ->
			BadnessValues = Rest
		;	BadnessValues = [Badness| Rest]
		),
		other_badness_values(Pairs, Index, Rest).

	tie_block_denominator(_BadnessValues, 0, 1.0) :-
		!.
	tie_block_denominator(BadnessValues, BlockSize, Denominator) :-
		zero_coefficients(BlockSize, Zeroes),
		update_elementary_coefficients(BadnessValues, [1.0| Zeroes], Coefficients),
		nth0(BlockSize, Coefficients, Denominator).

	zero_coefficients(0, []) :-
		!.
	zero_coefficients(Count, [0.0| Zeroes]) :-
		NextCount is Count - 1,
		zero_coefficients(NextCount, Zeroes).

	update_elementary_coefficients([], Coefficients, Coefficients).
	update_elementary_coefficients([Badness| BadnessValues], [Coefficient0| Coefficients0], Coefficients) :-
		update_coefficients(Coefficients0, Badness, Coefficient0, UpdatedCoefficients),
		update_elementary_coefficients(BadnessValues, [Coefficient0| UpdatedCoefficients], Coefficients).

	update_coefficients([], _Badness, _PreviousCoefficient, []).
	update_coefficients([Coefficient0| Coefficients0], Badness, PreviousCoefficient, [Coefficient| Coefficients]) :-
		Coefficient is Coefficient0 + Badness * PreviousCoefficient,
		update_coefficients(Coefficients0, Badness, Coefficient0, Coefficients).

	exposure_vector([], _Index, _ExposureDictionary, []).
	exposure_vector([_SelectionCount| SelectionCounts], Index, ExposureDictionary, [Exposure| ExposureCounts]) :-
		dictionary_lookup(Index, Exposure, ExposureDictionary),
		NextIndex is Index + 1,
		exposure_vector(SelectionCounts, NextIndex, ExposureDictionary, ExposureCounts).

	raw_strengths([], [], [], TotalRawStrength, TotalRawStrength).
	raw_strengths([SelectionCount| SelectionCounts], [Exposure| ExposureCounts], [RawStrength| RawStrengths], TotalRawStrength0, TotalRawStrength) :-
		RawStrength is Exposure / SelectionCount,
		TotalRawStrength1 is TotalRawStrength0 + RawStrength,
		raw_strengths(SelectionCounts, ExposureCounts, RawStrengths, TotalRawStrength1, TotalRawStrength).

	normalize_strengths([], _TotalRawStrength, [], [], MaximumDifference, MaximumDifference).
	normalize_strengths([RawStrength| RawStrengths], TotalRawStrength, [CurrentStrength| CurrentStrengths], [Strength| Strengths], MaximumDifference0, MaximumDifference) :-
		Strength is RawStrength / TotalRawStrength,
		Difference is abs(CurrentStrength - Strength),
		(	Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;	MaximumDifference1 = MaximumDifference0
		),
		normalize_strengths(RawStrengths, TotalRawStrength, CurrentStrengths, Strengths, MaximumDifference1, MaximumDifference).

	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.
	valid_option(missing_relevance(Value)) :-
		once((Value == zero; Value == error)).

	default_option(maximum_iterations(5000)).
	default_option(tolerance(1.0e-6)).
	default_option(missing_relevance(zero)).

:- end_object.
