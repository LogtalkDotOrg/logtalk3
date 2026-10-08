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


:- object(mrmr_feature_selector,
	imports([feature_selector_common, feature_redundancy])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Minimum redundancy maximum relevance selection using raw mutual information in bits and the MID difference criterion. Learning requires at least two joint complete observations; otherwise it throws ``domain_error(mrmr_usable_examples,Count)``.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- private(check_usable_examples/1).
	:- mode(check_usable_examples(+non_negative_integer), one_or_error).
	:- info(check_usable_examples/1, [
		comment is 'Requires at least two jointly complete observations.',
		argnames is ['Count'],
		exceptions is [
			'Fewer than two jointly complete observations remain' - domain_error(mrmr_usable_examples, 'Count')
		]
	]).

	learn(Dataset, mrmr_feature_selector(Scores, Selected, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_examples(Dataset, Features, Examples),
		^^prepare_feature_columns(Dataset, Features, Examples, Options, joint, Columns, Preparation),
		memberchk(usable_example_count(Usable), Preparation),
		check_usable_examples(Usable),
		relevance_candidates(Columns, Candidates, Unsorted),
		^^sort_by_decreasing_score(Unsorted, Scores),
		^^option(selection_strategy(Strategy), Options),
		length(Features, CandidateCount),
		selection_budget(Strategy, CandidateCount, K),
		greedy_selection(Candidates, K, Strategy, 0, Selected, Trace, 0, Evaluations, Rounds, Termination),
		length(Examples, ExampleCount),
		length(Selected, SelectedCount),
		Extra = [
			candidate_count(CandidateCount), selected_count(SelectedCount),
			selection_criterion(mid), scoring_metric(mutual_information), redundancy_metric(mutual_information),
			selection_trace(Trace), redundancy_evaluations(Evaluations),
			redundancy_update_rounds(Rounds), termination(Termination)| Preparation
		],
		^^base_selector_diagnostics(mrmr_feature_selector, ExampleCount, Options, Extra, Diagnostics).

	check_usable_examples(Count) :-
		(	Count >= 2 ->
			true
		;	domain_error(mrmr_usable_examples, Count)
		).

	relevance_candidates([], [], []).
	relevance_candidates([column(Feature, _Specification, Pairs, _Categories)| Columns],
		[candidate(Feature, Relevance, Pairs, 0.0)| Candidates], [Feature-Relevance| Scores]) :-
		^^contingency_counts(Pairs, Counts),
		^^contingency_score(mutual_information, Counts, Relevance),
		relevance_candidates(Columns, Candidates, Scores).

	selection_budget(top_k(K), _CandidateCount, K).
	selection_budget(positive_mid, CandidateCount, CandidateCount).

	greedy_selection([], _K, _Strategy, _Count, [], [], Evaluations, Evaluations, 0, candidates_exhausted) :-
		!.
	greedy_selection([Candidate| Candidates], K, Strategy, Count, Selected, Trace, Evaluations0, Evaluations, Rounds, Termination) :-
		best_candidate(Candidates, Count, Candidate, Best),
		Best = candidate(Feature, Relevance, Pairs, _Sum),
		candidate_mid(Best, Count, Mean, MID),
		(	Strategy == positive_mid, MID =< 0 ->
			Selected = [],
			Trace = [],
			Evaluations = Evaluations0,
			Rounds = Count,
			Termination = non_positive_mid(step(Feature, Relevance, Mean, MID))
		;	Selected = [Feature| RestSelected],
			Trace = [step(Feature, Relevance, Mean, MID)| RestTrace],
			remove_candidate([Candidate| Candidates], Feature, Remaining),
			NextK is K - 1,
			(	Remaining == [] ->
				RestSelected = [],
				RestTrace = [],
				Evaluations = Evaluations0,
				Rounds = Count,
				Termination = candidates_exhausted
			;	NextK =:= 0 ->
				RestSelected = [],
				RestTrace = [],
				Evaluations = Evaluations0,
				Rounds = Count,
				Termination = budget_reached
			;	update_redundancies(Remaining, Pairs, Updated, 0, Added),
				NextCount is Count + 1,
				Evaluations1 is Added + Evaluations0,
				greedy_selection(Updated, NextK, Strategy, NextCount, RestSelected, RestTrace, Evaluations1, Evaluations, Rounds, Termination)
			)
		).

	candidate_mid(candidate(_Feature, Relevance, _Pairs, Sum), Count, Mean, MID) :-
		(	Count =:= 0 ->
			Mean = 0.0
		;	Mean is Sum / Count
		),
		MID is Relevance - Mean.

	best_candidate([], _Count, Best, Best).
	best_candidate([Candidate| Candidates], Count, Best0, Best) :-
		candidate_mid(Candidate, Count, _Mean, Score),
		candidate_mid(Best0, Count, _BestMean, BestScore),
		(	Score > BestScore ->
			Best1 = Candidate
		;	Best1 = Best0
		),
		best_candidate(Candidates, Count, Best1, Best).

	remove_candidate([candidate(Feature, _Relevance, _Pairs, _Sum)| Candidates], Selected, Remaining) :-
		Feature == Selected,
		!,
		Remaining = Candidates.
	remove_candidate([Candidate| Candidates], Selected, [Candidate| Remaining]) :-
		remove_candidate(Candidates, Selected, Remaining).

	update_redundancies([], _Pairs, [], Evaluations, Evaluations).
	update_redundancies([candidate(Feature, Relevance, Values, Sum0)| Candidates], Pairs, [candidate(Feature, Relevance, Values, Sum)| Updated], Evaluations0, Evaluations) :-
		^^feature_pair_mutual_information(Pairs, Values, Redundancy),
		Sum is Sum0 + Redundancy,
		Evaluations1 is Evaluations0 + 1,
		update_redundancies(Candidates, Pairs, Updated, Evaluations1, Evaluations).

	selected_features(Selector, Features) :-
		::check_selector(Selector),
		Selector = mrmr_feature_selector(_Scores, Features, _Diagnostics).

	feature_scores(Selector, Scores) :-
		::check_selector(Selector),
		Selector = mrmr_feature_selector(Scores, _Features, _Diagnostics).

	check_selector(Selector) :-
		(	\+ ground(Selector) ->
			instantiation_error
		;	valid_model(Selector) ->
			true
		;	domain_error(selector, Selector)
		).

	valid_model(mrmr_feature_selector(Scores, Selected, Diagnostics)) :-
		valid(list(pair), Scores),
		valid(list(atomic), Selected),
		^^valid_selector_metadata(mrmr_feature_selector, Diagnostics),
		memberchk(options(Options), Diagnostics),
		^^valid_options(Options),
		^^option(selection_strategy(Strategy), Options),
		^^option(discretization(_Default), Options),
		memberchk(candidate_count(CandidateCount), Diagnostics),
		valid(non_negative_integer, CandidateCount),
		length(Scores, CandidateCount),
		memberchk(selected_count(SelectedCount), Diagnostics),
		valid(non_negative_integer, SelectedCount),
		length(Selected, SelectedCount),
		memberchk(example_count(Total), Diagnostics),
		memberchk(usable_example_count(Usable), Diagnostics),
		valid(positive_integer, Usable),
		Usable >= 2,
		memberchk(excluded_example_count(Excluded), Diagnostics),
		valid(non_negative_integer, Excluded),
		Total =:= Usable + Excluded,
		memberchk(selection_criterion(mid), Diagnostics),
		memberchk(scoring_metric(mutual_information), Diagnostics),
		memberchk(redundancy_metric(mutual_information), Diagnostics),
		memberchk(preparation_mode(joint), Diagnostics),
		memberchk(complete_cases(Counts), Diagnostics),
		valid(list(pair), Counts),
		length(Counts, CandidateCount),
		avltree::new(Empty),
		valid_score_dictionary(Scores, Empty, Vocabulary),
		valid_preparation_counts(Counts, Usable, Vocabulary, Empty, DeclarationScores),
		^^sort_by_decreasing_score(DeclarationScores, Sorted),
		Scores == Sorted,
		memberchk(discretization(Configurations), Diagnostics),
		memberchk(occupied_categories(Occupied), Diagnostics),
		valid(list(pair), Configurations),
		valid(list(pair), Occupied),
		valid_preparation_details(Counts, Configurations, Occupied, Usable, Options),
		valid_override_features(Options, Vocabulary),
		memberchk(selection_trace(Trace), Diagnostics),
		valid(list(compound), Trace),
		valid_trace(Selected, Trace, Vocabulary, Empty, 0),
		valid_first_selection(Selected, Scores),
		memberchk(redundancy_evaluations(Evaluations), Diagnostics),
		valid(non_negative_integer, Evaluations),
		memberchk(redundancy_update_rounds(Rounds), Diagnostics),
		valid(non_negative_integer, Rounds),
		memberchk(termination(Termination), Diagnostics),
		valid_termination(Strategy, CandidateCount, Selected, Trace, Vocabulary, Rounds, Termination),
		Evaluations =:= Rounds * CandidateCount - Rounds * (Rounds + 1) // 2.

	valid_termination(top_k(K), CandidateCount, Selected, _Trace, _Vocabulary, Rounds, Termination) :-
		length(Selected, Count),
		Count =:= min(K, CandidateCount),
		Rounds =:= max(0, Count - 1),
		(	Count =:= CandidateCount ->
			Termination == candidates_exhausted
		;	Termination == budget_reached
		).
	valid_termination(positive_mid, CandidateCount, Selected, Trace, Vocabulary, Rounds, Termination) :-
		positive_trace(Trace),
		length(Selected, Count),
		(	Termination == candidates_exhausted ->
			Count =:= CandidateCount,
			Rounds =:= max(0, Count - 1)
		;	Termination = non_positive_mid(step(Feature, Relevance, Mean, MID)),
			Count < CandidateCount,
			Rounds =:= Count,
			avltree::lookup(Feature, Score, Vocabulary),
			\+ member(Feature, Selected),
			number(Relevance),
			number(Mean),
			number(MID),
			Relevance =:= Score,
			Mean >= 0,
			(	Count =:= 0 ->
				Mean =:= 0
			;	true
			),
			MID =:= Relevance - Mean,
			MID =< 0
		).

	positive_trace([]).
	positive_trace([step(_, _, _, MID)| Trace]) :-
		MID > 0,
		positive_trace(Trace).

	valid_score_dictionary([], Vocabulary, Vocabulary).
	valid_score_dictionary([Feature-Score| Scores], Vocabulary0, Vocabulary) :-
		atomic(Feature),
		number(Score),
		Score >= 0,
		\+ avltree::lookup(Feature, _, Vocabulary0),
		avltree::insert(Vocabulary0, Feature, Score, Vocabulary1),
		valid_score_dictionary(Scores, Vocabulary1, Vocabulary).

	valid_preparation_counts([], _Usable, _Vocabulary, _Seen, []).
	valid_preparation_counts([Feature-Count| Counts], Usable, Vocabulary, Seen0, [Feature-Score| Scores]) :-
		valid(non_negative_integer, Count),
		Count =:= Usable,
		avltree::lookup(Feature, Score, Vocabulary),
		\+ avltree::lookup(Feature, _, Seen0),
		avltree::insert(Seen0, Feature, true, Seen),
		valid_preparation_counts(Counts, Usable, Vocabulary, Seen, Scores).

	valid_preparation_details([], [], [], _Usable, _Options).
	valid_preparation_details([Feature-_Count| Counts], [Feature-Specification| Configurations], [Feature-Categories| Occupied], Usable, Options) :-
		^^valid_feature_discretization(Specification),
		valid(positive_integer, Categories),
		Categories =< Usable,
		(	Specification == categorical ->
			true
		;	arg(1, Specification, Bins),
			Categories =< Bins
		),
		(	member(feature_discretization(Feature, Override), Options) ->
			Specification == Override
		;	(	Specification == categorical ->
				true
			;	^^option(discretization(Default), Options),
				Specification == Default
			)
		),
		valid_preparation_details(Counts, Configurations, Occupied, Usable, Options).

	valid_override_features([], _Vocabulary).
	valid_override_features([Option| Options], Vocabulary) :-
		(	Option = feature_discretization(Feature, _Specification) ->
			avltree::lookup(Feature, _, Vocabulary)
		;	true
		),
		valid_override_features(Options, Vocabulary).

	valid_trace([], [], _Vocabulary, _Seen, _Count).
	valid_trace([Feature| Selected], [step(Feature, Relevance, Mean, MID)| Trace], Vocabulary, Seen0, Count) :-
		avltree::lookup(Feature, Score, Vocabulary),
		\+ avltree::lookup(Feature, _, Seen0),
		number(Relevance),
		number(Mean),
		number(MID),
		Relevance =:= Score,
		Mean >= 0,
		(	Count =:= 0 ->
			Mean =:= 0
		;	true
		),
		MID =:= Relevance - Mean,
		avltree::insert(Seen0, Feature, true, Seen),
		NextCount is Count + 1,
		valid_trace(Selected, Trace, Vocabulary, Seen, NextCount).

	valid_first_selection([], []).
	valid_first_selection([], [_-Score| _]) :-
		Score =:= 0.
	valid_first_selection([Feature| _Selected], [Feature-_Score| _Scores]).

	export_to_clauses(_Dataset, Selector, Functor, [Clause]) :-
		::check_selector(Selector),
		Clause =.. [Functor, Selector].

	selector_export_template(_Dataset, _Selector, Functor, Template) :-
		Template =.. [Functor, 'Selector'].

	selector_term_template(mrmr_feature_selector(_Scores, _Selected, _Diagnostics),
		mrmr_feature_selector('FeatureScores', 'SelectedFeatures', 'Diagnostics')).

	print_selector(Selector) :-
		::check_selector(Selector),
		^^print_selector_template(Selector),
		writeq(Selector), nl.

	default_option(selection_strategy(top_k(10))).
	default_option(discretization(equal_frequency(10))).

	valid_option(selection_strategy(Strategy)) :-
		(	Strategy == positive_mid ->
			true
		;	Strategy = top_k(Count),
			integer(Count),
			Count > 0
		).
	valid_option(discretization(Specification)) :-
		^^valid_feature_discretization(Specification).
	valid_option(feature_discretization(Feature, Specification)) :-
		atomic(Feature),
		^^valid_feature_discretization(Specification).

:- end_object.
