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
		date is 2026-04-20,
		comment is 'Bradley-Terry pairwise preference ranker. Learns one positive strength parameter per item from a connected dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		remarks is [
			'Algorithm' - 'Uses a deterministic minorization-maximization update to estimate one relative strength parameter per item from weighted pairwise wins and losses.',
			'Dataset requirements' - 'The training dataset must declare each ranked item once, enumerate positive-weight pairwise preferences between distinct declared items, and induce a connected comparison graph.',
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

	:- uses(list, [
		length/2, member/2, memberchk/2, sort/4
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_preferences(Dataset, Preferences),
		initial_strengths(Items, Strengths0),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		optimize_strengths(MaximumIterations, Tolerance, Items, Preferences, Strengths0, Strengths, Status, Iterations, FinalDifference),
		Ranker = bt_ranker(Items, Strengths, [
			model(bradley_terry),
			options(Options),
			convergence(Status),
			iterations(Iterations),
			final_delta(FinalDifference),
			dataset_summary(DatasetSummary)
		]).

	rank(Ranker, Candidates, Ranking) :-
		ranker_data(Ranker, Items, Strengths, _Diagnostics),
		validate_candidates(Candidates, Items),
		rank_candidates(Strengths, Candidates, Ranking).

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
		Ranker =.. [_Functor, Items, Strengths, Diagnostics].

	initial_strengths(Items, Strengths) :-
		length(Items, Count),
		Strength is 1.0 / Count,
		findall(Item-Strength, member(Item, Items), Strengths).

	optimize_strengths(MaximumIterations, Tolerance, Items, Preferences, Strengths0, Strengths, Status, Iterations, FinalDifference) :-
		optimize_strengths(0, MaximumIterations, Tolerance, Items, Preferences, Strengths0, Strengths, Status, Iterations, FinalDifference).

	optimize_strengths(Iteration0, MaximumIterations, Tolerance, Items, Preferences, Strengths0, Strengths, Status, Iterations, FinalDifference) :-
		update_strengths(Items, Preferences, Strengths0, Strengths1, MaximumDifference),
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
		;   optimize_strengths(Iteration, MaximumIterations, Tolerance, Items, Preferences, Strengths1, Strengths, Status, Iterations, FinalDifference)
		).

	update_strengths(Items, Preferences, Strengths0, Strengths, MaximumDifference) :-
		findall(
			Item-RawStrength,
			(
				member(Item, Items),
				lookup_strength(Item, Strengths0, CurrentStrength),
				item_wins(Preferences, Item, 0.0, Wins),
				item_denominator(Items, Item, Preferences, Strengths0, 0.0, Denominator),
				(   Denominator =< 1.0e-12 ->
					RawStrength = CurrentStrength
				;   CandidateStrength is Wins / Denominator,
					(   CandidateStrength > 1.0e-12 ->
						RawStrength = CandidateStrength
					;   RawStrength = 1.0e-12
					)
				)
			),
			RawStrengths
		),
		normalize_strengths(RawStrengths, Strengths),
		max_difference(Strengths0, Strengths, 0.0, MaximumDifference).

	item_wins([], _Item, Wins, Wins).
	item_wins([p(Winner, _, Weight)| Preferences], Item, Wins0, Wins) :-
		(   Item == Winner ->
			Wins1 is Wins0 + Weight
		;   Wins1 = Wins0
		),
		item_wins(Preferences, Item, Wins1, Wins).

	item_denominator([], _Item, _Preferences, _Strengths, Denominator, Denominator).
	item_denominator([Other| Others], Item, Preferences, Strengths, Denominator0, Denominator) :-
		(   Item == Other ->
			Denominator1 = Denominator0
		;   comparison_weight(Item, Other, Preferences, TotalWeight),
			(   TotalWeight =< 1.0e-12 ->
				Denominator1 = Denominator0
			;   lookup_strength(Item, Strengths, ItemStrength),
				lookup_strength(Other, Strengths, OtherStrength),
				Denominator1 is Denominator0 + TotalWeight / (ItemStrength + OtherStrength)
			)
		),
		item_denominator(Others, Item, Preferences, Strengths, Denominator1, Denominator).

	comparison_weight(Item, Other, Preferences, TotalWeight) :-
		pairwise_weight(Item, Other, Preferences, ForwardWeight),
		pairwise_weight(Other, Item, Preferences, BackwardWeight),
		TotalWeight is ForwardWeight + BackwardWeight.

	pairwise_weight(_Winner, _Loser, [], 0.0) :-
		!.
	pairwise_weight(Winner, Loser, [p(Winner, Loser, Weight)| Preferences], TotalWeight) :-
		!,
		pairwise_weight(Winner, Loser, Preferences, RestWeight),
		TotalWeight is RestWeight + Weight.
	pairwise_weight(Winner, Loser, [_| Preferences], TotalWeight) :-
		pairwise_weight(Winner, Loser, Preferences, TotalWeight).

	lookup_strength(Item, [Item-Strength| _], Strength) :-
		!.
	lookup_strength(Item, [_| Strengths], Strength) :-
		lookup_strength(Item, Strengths, Strength).

	normalize_strengths(Strengths0, Strengths) :-
		sum_strengths(Strengths0, 0, Total),
		(   Total =< 1.0e-12 ->
			Strengths = Strengths0
		;   normalize_strengths(Strengths0, Total, Strengths)
		).

	normalize_strengths([], _Total, []).
	normalize_strengths([Item-Strength0| Strengths0], Total, [Item-Strength| Strengths]) :-
		Strength is Strength0 / Total,
		normalize_strengths(Strengths0, Total, Strengths).

	sum_strengths([], Total, Total).
	sum_strengths([_-Strength| Strengths], Total0, Total) :-
		Total1 is Total0 + Strength,
		sum_strengths(Strengths, Total1, Total).

	max_difference([], [], MaximumDifference, MaximumDifference).
	max_difference([Item-Strength0| Strengths0], [Item-Strength1| Strengths1], MaximumDifference0, MaximumDifference) :-
		Difference is abs(Strength0 - Strength1),
		(   Difference > MaximumDifference0 ->
			MaximumDifference1 = Difference
		;   MaximumDifference1 = MaximumDifference0
		),
		max_difference(Strengths0, Strengths1, MaximumDifference1, MaximumDifference).

	validate_candidates([], _Items).
	validate_candidates([Candidate| Candidates], Items) :-
		(   memberchk(Candidate, Items) ->
			true
		;   existence_error(item, Candidate)
		),
		validate_candidates(Candidates, Items).

	rank_candidates(Strengths, Candidates, Ranking) :-
		findall(
			pair(NegStrength, Item)-Item,
			(
				member(Item, Candidates),
				lookup_strength(Item, Strengths, Strength),
				NegStrength is -Strength
			),
			Pairs
		),
		sort(1, @=<, Pairs, SortedPairs),
		pairs_values(SortedPairs, Ranking).

	pairs_values([], []).
	pairs_values([_-Value| Pairs], [Value| Values]) :-
		pairs_values(Pairs, Values).

	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.

	default_option(maximum_iterations(5000)).
	default_option(tolerance(1.0e-6)).

:- end_object.
