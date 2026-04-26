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
	imports([ranking_dataset_common, grouped_strength_ranker_common, score_ranker_common, plackett_luce_common])).

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
		as_dictionary/2
	]).

	:- uses(list, [
		length/2, member/2
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
			^^index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			^^preprocess_groups(ascending, Groups, Dataset, MissingRelevance, IndexDictionary, Count, Steps, SelectionCounts, DirectedAdjacency, ReverseAdjacency),
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

	update_strengths(plackett_luce_last_context(Steps, SelectionCounts), Strengths0, Strengths, MaximumDifference) :-
		inverse_strengths(Strengths0, BadnessValues),
		^^strength_dictionary(BadnessValues, BadnessDictionary),
		^^selection_exposures(Steps, SelectionCounts, BadnessDictionary, ExposureCounts),
		raw_strengths(SelectionCounts, ExposureCounts, RawStrengths, 0.0, TotalRawStrength),
		^^normalize_strengths(RawStrengths, TotalRawStrength, Strengths0, Strengths, 0.0, MaximumDifference).

	inverse_strengths([], []).
	inverse_strengths([Strength| Strengths], [Badness| Badnesses]) :-
		Badness is 1.0 / Strength,
		inverse_strengths(Strengths, Badnesses).

	raw_strengths([], [], [], TotalRawStrength, TotalRawStrength).
	raw_strengths([SelectionCount| SelectionCounts], [Exposure| ExposureCounts], [RawStrength| RawStrengths], TotalRawStrength0, TotalRawStrength) :-
		RawStrength is Exposure / SelectionCount,
		TotalRawStrength1 is TotalRawStrength0 + RawStrength,
		raw_strengths(SelectionCounts, ExposureCounts, RawStrengths, TotalRawStrength1, TotalRawStrength).

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
