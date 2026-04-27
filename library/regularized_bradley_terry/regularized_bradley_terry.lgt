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


:- object(regularized_bradley_terry,
	imports([ranking_dataset_common, pairwise_strength_ranker_common, score_ranker_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Regularized Bradley-Terry MAP ranker. Learns one positive strength parameter per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol using a Bradley-Terry likelihood with an explicit independent Gamma prior, and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		remarks is [
			'Algorithm' - 'Uses a deterministic MM-style posterior-mode update for a Bradley-Terry likelihood regularized by an explicit independent Gamma prior over item strengths.',
			'Dataset requirements' - 'The training dataset must declare each ranked item once, enumerate positive-weight pairwise preferences between distinct declared items, and induce a connected undirected comparison graph. Unlike the unregularized Bradley-Terry model, the directed win graph is not required to be strongly connected.',
			'Prior semantics' - 'The ``gamma_prior(gamma(Shape, Rate))`` option specifies the Gamma prior hyperparameters. The implementation requires ``Shape > 1`` and ``Rate > 0`` so the posterior mode remains strictly positive for all items.',
			'Ranker representation' - 'The learned ranker is represented by default as ``regularized_bt_ranker(Items, Strengths, Diagnostics)`` where ``Strengths`` stores ``Item-Strength`` pairs and ``Diagnostics`` stores metadata such as the effective Gamma prior, convergence status, iteration count, and dataset summary.'
		],
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, bradley_terry]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(avltree, [
		as_dictionary/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
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
		^^preprocess_matchups(Matchups, IndexDictionary, _DirectedAdjacency, _ReverseAdjacency, WinWeights0, PairAdjacency0),
		^^build_dataset_model(Count, WinWeights0, PairAdjacency0, PairWeights, Wins),
		^^initial_strengths(Items, StrengthValues0),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		^^option(gamma_prior(gamma(Shape, Rate)), Options),
		ShapeMinusOne is Shape - 1.0,
		^^optimize_strengths(MaximumIterations, Tolerance, regularized_bt_context(PairWeights, Wins, ShapeMinusOne, Rate), StrengthValues0, StrengthValues, Status, Iterations, FinalDifference),
		^^strength_pairs(Items, StrengthValues, Strengths),
		Ranker = regularized_bt_ranker(Items, Strengths, [
			model(regularized_bradley_terry),
			options(Options),
			prior(gamma(Shape, Rate)),
			convergence(Status),
			iterations(Iterations),
			final_delta(FinalDifference),
			dataset_summary(DatasetSummary)
		]).

	rank(Ranker, Candidates, Ranking) :-
		ranker_data(Ranker, _Items, Strengths, _Diagnostics),
		^^rank_by_scores(Strengths, Candidates, Ranking).

	ranker_scores_data(Ranker, Scores) :-
		ranker_data(Ranker, _Items, Scores, _Diagnostics).

	ranker_diagnostics_data(Ranker, Diagnostics) :-
		ranker_data(Ranker, _Items, _Strengths, Diagnostics).

	ranker_export_template(_Dataset, _Ranker, Functor, Template) :-
		Template =.. [Functor, 'Ranker'].

	ranker_term_template(regularized_bt_ranker(_Items, _Strengths, _Diagnostics), regularized_bt_ranker('Items', 'Strengths', 'Diagnostics')).

	export_to_clauses(_Dataset, Ranker, Functor, [Clause]) :-
		Clause =.. [Functor, Ranker].

	print_ranker(Ranker) :-
		ranker_data(Ranker, Items, Strengths, Diagnostics),
		length(Items, Count),
		format('Regularized Bradley-Terry ranker for ~d items~n', [Count]),
		^^print_ranker_template(Ranker),
		forall(
			member(Item-Strength, Strengths),
			format('  ~q: ~6f~n', [Item, Strength])
		),
		format('Diagnostics: ~q~n', [Diagnostics]).

	ranker_data(Ranker, Items, Strengths, Diagnostics) :-
		(   var(Ranker) ->
			instantiation_error
		;   Ranker = regularized_bt_ranker(Items, Strengths, Diagnostics),
			valid_ranker_data(Items, Strengths, Diagnostics) ->
			true
		;   domain_error(regularized_bradley_terry_ranker, Ranker)
		).

	valid_ranker_data(Items, Strengths, Diagnostics) :-
		^^valid_item_value_pairs(Items, Strengths),
		valid_positive_values(Strengths),
		^^valid_ranker_metadata(regularized_bradley_terry, Diagnostics),
		memberchk(prior(gamma(Shape, Rate)), Diagnostics),
		number(Shape),
		Shape > 1.0,
		number(Rate),
		Rate > 0.0,
		memberchk(convergence(Status), Diagnostics),
		atom(Status),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 0,
		memberchk(final_delta(FinalDifference), Diagnostics),
		number(FinalDifference),
		FinalDifference >= 0.0.

	valid_positive_values(Pairs) :-
		positive_values(Pairs).

	positive_values([]).
	positive_values([_Item-Value| Pairs]) :-
		number(Value),
		Value > 0.0,
		positive_values(Pairs).

	update_strengths(regularized_bt_context(PairWeights, Wins, ShapeMinusOne, Rate), Strengths0, Strengths, MaximumDifference) :-
		^^strength_dictionary(Strengths0, StrengthDictionary),
		update_strength_values(PairWeights, Wins, ShapeMinusOne, Rate, StrengthDictionary, Strengths0, Strengths, 0.0, MaximumDifference).

	update_strength_values([], [], _ShapeMinusOne, _Rate, _StrengthDictionary, [], [], MaximumDifference, MaximumDifference).
	update_strength_values([Neighbors| PairWeights], [Wins| WinTotals], ShapeMinusOne, Rate, StrengthDictionary, [CurrentStrength| CurrentStrengths], [Strength| Strengths], MaximumDifference0, MaximumDifference) :-
		^^item_denominator(Neighbors, StrengthDictionary, CurrentStrength, 0.0, Denominator),
		Strength is (Wins + ShapeMinusOne) / (Rate + Denominator),
		Difference is abs(CurrentStrength - Strength),
		(   Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;   MaximumDifference1 = MaximumDifference0
		),
		update_strength_values(PairWeights, WinTotals, ShapeMinusOne, Rate, StrengthDictionary, CurrentStrengths, Strengths, MaximumDifference1, MaximumDifference).

	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.
	valid_option(gamma_prior(gamma(Shape, Rate))) :-
		number(Shape),
		Shape > 1.0,
		number(Rate),
		Rate > 0.0.

	default_option(maximum_iterations(5000)).
	default_option(tolerance(1.0e-6)).
	default_option(gamma_prior(gamma(2.0, 2.0))).

:- end_object.
