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


:- category(plackett_luce_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Shared helpers for grouped Plackett-Luce rankers.'
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		append/3, drop/3, length/2, nth0/3, reverse/2
	]).

	:- protected(index_items/3).
	:- mode(index_items(+list, +integer, -list), one).
	:- info(index_items/3, [
		comment is 'Builds ``Item-Index`` pairs for the ordered training items.',
		argnames is ['Items', 'Index', 'Indices']
	]).

	:- protected(preprocess_groups/10).
	:- mode(preprocess_groups(+atom, +list, +object_identifier, +atom, +compound, +integer, -list, -list, -compound, -compound), one).
	:- info(preprocess_groups/10, [
		comment is 'Builds per-group choice steps, selection counts, and forward and reverse adjacency dictionaries from grouped rankings under the selected traversal order.',
		argnames is ['Order', 'Groups', 'Dataset', 'MissingRelevance', 'IndexDictionary', 'Count', 'Steps', 'SelectionCounts', 'DirectedAdjacency', 'ReverseAdjacency']
	]).

	:- protected(selection_exposures/4).
	:- mode(selection_exposures(+list, +list, +compound, -list), one).
	:- info(selection_exposures/4, [
		comment is 'Computes the per-item exposure counts implied by the grouped choice steps and the current value dictionary.',
		argnames is ['Steps', 'SelectionCounts', 'ValueDictionary', 'ExposureCounts']
	]).

	:- protected(normalize_strengths/6).
	:- mode(normalize_strengths(+list(number), +number, +list(number), -list(number), +number, -number), one).
	:- info(normalize_strengths/6, [
		comment is 'Normalizes raw strength values to sum to one while tracking the maximum absolute change from the current strengths.',
		argnames is ['RawStrengths', 'TotalRawStrength', 'CurrentStrengths', 'Strengths', 'MaximumDifference0', 'MaximumDifference']
	]).

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	preprocess_groups(Order, Groups, Dataset, MissingRelevance, IndexDictionary, Count, Steps, SelectionCounts, DirectedAdjacency, ReverseAdjacency) :-
		dictionary_new(SelectionCounts0),
		dictionary_new(DirectedAdjacency0),
		dictionary_new(ReverseAdjacency0),
		preprocess_groups(Groups, Order, Dataset, MissingRelevance, IndexDictionary, SelectionCounts0, SelectionCountsDictionary, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, Steps, []),
		count_vector(1, Count, SelectionCountsDictionary, SelectionCounts).

	preprocess_groups([], _Order, _Dataset, _MissingRelevance, _IndexDictionary, SelectionCounts, SelectionCounts, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, Steps, Steps).
	preprocess_groups([Group| Groups], Order, Dataset, MissingRelevance, IndexDictionary, SelectionCounts0, SelectionCounts, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, Steps0, Steps) :-
		::grouped_dataset_tie_blocks(Dataset, Group, MissingRelevance, TieBlocksDescending),
		ordered_tie_blocks(Order, TieBlocksDescending, TieBlocks),
		tie_block_indices(TieBlocks, IndexDictionary, Blocks),
		blocks_indices(Blocks, RemainingIndices),
		group_steps(Blocks, RemainingIndices, SelectionCounts0, SelectionCounts1, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1, Steps0, Steps1),
		preprocess_groups(Groups, Order, Dataset, MissingRelevance, IndexDictionary, SelectionCounts1, SelectionCounts, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency, Steps1, Steps).

	ordered_tie_blocks(ascending, TieBlocksDescending, TieBlocksAscending) :-
		reverse(TieBlocksDescending, TieBlocksAscending).
	ordered_tie_blocks(descending, TieBlocksDescending, TieBlocksDescending).

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

	group_steps([_LastBlock], _RemainingIndices, SelectionCounts, SelectionCounts, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency, Steps, Steps) :-
		!.
	group_steps([Block| Blocks], RemainingIndices, SelectionCounts0, SelectionCounts, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency, [step(Block, RemainingIndices, BlockSize)| Steps0], Steps) :-
		length(Block, BlockSize),
		increment_selection_counts(Block, SelectionCounts0, SelectionCounts1),
		drop(BlockSize, RemainingIndices, NextIndices),
		add_directed_edges(Block, NextIndices, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1),
		group_steps(Blocks, NextIndices, SelectionCounts1, SelectionCounts, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency, Steps0, Steps).

	increment_selection_counts([], SelectionCounts, SelectionCounts).
	increment_selection_counts([Index| Indices], SelectionCounts0, SelectionCounts) :-
		update_weight_dictionary(SelectionCounts0, Index, 1, SelectionCounts1),
		increment_selection_counts(Indices, SelectionCounts1, SelectionCounts).

	add_directed_edges([], _NextIndices, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency).
	add_directed_edges([Index| Indices], NextIndices, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency) :-
		add_directed_neighbors(NextIndices, Index, DirectedAdjacency0, DirectedAdjacency1, ReverseAdjacency0, ReverseAdjacency1),
		add_directed_edges(Indices, NextIndices, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency).

	add_directed_neighbors([], _Index, DirectedAdjacency, DirectedAdjacency, ReverseAdjacency, ReverseAdjacency).
	add_directed_neighbors([NextIndex| NextIndices], Index, DirectedAdjacency0, DirectedAdjacency, ReverseAdjacency0, ReverseAdjacency) :-
		update_neighbor_dictionary(DirectedAdjacency0, Index, NextIndex, DirectedAdjacency1),
		update_neighbor_dictionary(ReverseAdjacency0, NextIndex, Index, ReverseAdjacency1),
		add_directed_neighbors(NextIndices, Index, DirectedAdjacency1, DirectedAdjacency, ReverseAdjacency1, ReverseAdjacency).

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

	count_vector(Index, Count, _Dictionary, []) :-
		Index > Count,
		!.
	count_vector(Index, Count, Dictionary, [Weight| Weights]) :-
		(	dictionary_lookup(Index, Weight0, Dictionary) ->
			Weight = Weight0
		;	Weight = 0
		),
		NextIndex is Index + 1,
		count_vector(NextIndex, Count, Dictionary, Weights).

	selection_exposures(Steps, SelectionCounts, ValueDictionary, ExposureCounts) :-
		dictionary_new(ExposureDictionary0),
		accumulate_selection_exposures(Steps, ValueDictionary, ExposureDictionary0, ExposureDictionary),
		exposure_vector(SelectionCounts, 1, ExposureDictionary, ExposureCounts).

	accumulate_selection_exposures([], _ValueDictionary, ExposureDictionary, ExposureDictionary).
	accumulate_selection_exposures([step(_Block, RemainingIndices, BlockSize)| Steps], ValueDictionary, ExposureDictionary0, ExposureDictionary) :-
		remaining_pairs(RemainingIndices, ValueDictionary, Pairs),
		pair_values(Pairs, Values),
		tie_block_denominator(Values, BlockSize, Denominator),
		selection_exposures(Pairs, Pairs, BlockSize, Denominator, ExposureDictionary0, ExposureDictionary1),
		accumulate_selection_exposures(Steps, ValueDictionary, ExposureDictionary1, ExposureDictionary).

	remaining_pairs([], _ValueDictionary, []).
	remaining_pairs([Index| Indices], ValueDictionary, [Index-Value| Pairs]) :-
		dictionary_lookup(Index, Value, ValueDictionary),
		remaining_pairs(Indices, ValueDictionary, Pairs).

	pair_values([], []).
	pair_values([_Index-Value| Pairs], [Value| Values]) :-
		pair_values(Pairs, Values).

	selection_exposures([], _AllPairs, _BlockSize, _Denominator, ExposureDictionary, ExposureDictionary).
	selection_exposures([Index-_Value| Pairs], AllPairs, BlockSize, Denominator, ExposureDictionary0, ExposureDictionary) :-
		other_pair_values(AllPairs, Index, OtherValues),
		LowerBlockSize is BlockSize - 1,
		tie_block_denominator(OtherValues, LowerBlockSize, Numerator),
		Delta is Numerator / Denominator,
		update_weight_dictionary(ExposureDictionary0, Index, Delta, ExposureDictionary1),
		selection_exposures(Pairs, AllPairs, BlockSize, Denominator, ExposureDictionary1, ExposureDictionary).

	other_pair_values([], _Index, []).
	other_pair_values([OtherIndex-Value| Pairs], Index, Values) :-
		(	Index == OtherIndex ->
			Values = Rest
		;	Values = [Value| Rest]
		),
		other_pair_values(Pairs, Index, Rest).

	tie_block_denominator(_Values, 0, 1.0) :-
		!.
	tie_block_denominator(Values, BlockSize, Denominator) :-
		zero_coefficients(BlockSize, Zeroes),
		update_elementary_coefficients(Values, [1.0| Zeroes], Coefficients),
		nth0(BlockSize, Coefficients, Denominator).

	zero_coefficients(0, []) :-
		!.
	zero_coefficients(Count, [0.0| Zeroes]) :-
		NextCount is Count - 1,
		zero_coefficients(NextCount, Zeroes).

	update_elementary_coefficients([], Coefficients, Coefficients).
	update_elementary_coefficients([Value| Values], [Coefficient0| Coefficients0], Coefficients) :-
		update_coefficients(Coefficients0, Value, Coefficient0, UpdatedCoefficients),
		update_elementary_coefficients(Values, [Coefficient0| UpdatedCoefficients], Coefficients).

	update_coefficients([], _Value, _PreviousCoefficient, []).
	update_coefficients([Coefficient0| Coefficients0], Value, PreviousCoefficient, [Coefficient| Coefficients]) :-
		Coefficient is Coefficient0 + Value * PreviousCoefficient,
		update_coefficients(Coefficients0, Value, Coefficient0, Coefficients).

	exposure_vector([], _Index, _ExposureDictionary, []).
	exposure_vector([_SelectionCount| SelectionCounts], Index, ExposureDictionary, [Exposure| ExposureCounts]) :-
		dictionary_lookup(Index, Exposure, ExposureDictionary),
		NextIndex is Index + 1,
		exposure_vector(SelectionCounts, NextIndex, ExposureDictionary, ExposureCounts).

	normalize_strengths([], _TotalRawStrength, [], [], MaximumDifference, MaximumDifference).
	normalize_strengths([RawStrength| RawStrengths], TotalRawStrength, [CurrentStrength| CurrentStrengths], [Strength| Strengths], MaximumDifference0, MaximumDifference) :-
		Strength is RawStrength / TotalRawStrength,
		Difference is abs(CurrentStrength - Strength),
		(	Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;	MaximumDifference1 = MaximumDifference0
		),
		normalize_strengths(RawStrengths, TotalRawStrength, CurrentStrengths, Strengths, MaximumDifference1, MaximumDifference).

:- end_category.
