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
	imports([ranking_dataset_common, pairwise_strength_ranker_common, score_ranker_common])).

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
		see_also is [pairwise_ranking_dataset_protocol, ranking_dataset_protocol, ranker_protocol, regularized_bradley_terry]
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
		as_dictionary/2
	]).

	:- uses(list, [
		keysort/2, length/2, member/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_matchups(Dataset, Matchups),
		length(Items, Count),
		^^index_items(Items, 1, IndexPairs),
		as_dictionary(IndexPairs, IndexDictionary),
		^^preprocess_matchups(Matchups, IndexDictionary, DirectedAdjacency0, ReverseAdjacency0, WinWeights0, PairAdjacency0),
		^^require_strong_connectivity(Count, DirectedAdjacency0, ReverseAdjacency0, bradley_terry_regular_dataset),
		^^build_dataset_model(Count, WinWeights0, PairAdjacency0, PairWeights, Wins),
		^^initial_strengths(Items, StrengthValues0),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		^^optimize_strengths(MaximumIterations, Tolerance, bradley_terry_context(PairWeights, Wins), StrengthValues0, StrengthValues, Status, Iterations, FinalDifference),
		^^strength_pairs(Items, StrengthValues, Strengths),
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
		^^rank_by_scores(Strengths, Candidates, Ranking).

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

	update_strengths(bradley_terry_context(PairWeights, Wins), Strengths0, Strengths, MaximumDifference) :-
		^^strength_dictionary(Strengths0, StrengthDictionary),
		update_strength_values(PairWeights, Wins, StrengthDictionary, Strengths0, RawStrengths, TotalRawStrength),
		normalize_strengths(RawStrengths, TotalRawStrength, Strengths0, Strengths, MaximumDifference).

	update_strength_values([], [], _StrengthDictionary, [], [], 0.0).
	update_strength_values([Neighbors| PairWeights], [Wins| WinTotals], StrengthDictionary, [CurrentStrength| CurrentStrengths], [RawStrength| RawStrengths], TotalRawStrength) :-
		^^item_denominator(Neighbors, StrengthDictionary, CurrentStrength, 0.0, Denominator),
		RawStrength is Wins / Denominator,
		update_strength_values(PairWeights, WinTotals, StrengthDictionary, CurrentStrengths, RawStrengths, RemainingRawStrength),
		TotalRawStrength is RawStrength + RemainingRawStrength.

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

	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.

	default_option(maximum_iterations(5000)).
	default_option(tolerance(1.0e-6)).

:- end_object.
