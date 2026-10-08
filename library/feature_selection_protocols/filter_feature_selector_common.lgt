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


:- category(filter_feature_selector_common,
	extends(feature_selector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Shared learning, validation, selection, and export implementations for univariate feature selectors.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- protected(filter_model/1).
	:- mode(filter_model(-atom), one).
	:- info(filter_model/1, [
		comment is 'Returns the receiving filter implementation model name.',
		argnames is ['Model']
	]).

	:- protected(filter_scoring_metric/2).
	:- mode(filter_scoring_metric(+list(compound), -object_identifier), one).
	:- info(filter_scoring_metric/2, [
		comment is 'Returns the scoring metric for the effective options.',
		argnames is ['Options', 'Metric']
	]).

	:- protected(filter_validate_dataset/3).
	:- mode(filter_validate_dataset(+object_identifier, +list(atomic), +list(compound)), one_or_error).
	:- info(filter_validate_dataset/3, [
		comment is 'Checks algorithm-specific feature declarations. The default accepts all declarations.',
		argnames is ['Dataset', 'Features', 'Examples'],
		exceptions is [
			'A feature declaration is unsupported by the receiving filter' - domain_error(feature_type, 'Feature-Declaration')
		]
	]).

	:- protected(filter_feature_scores/6).
	:- mode(filter_feature_scores(+object_identifier, +list(atomic), +list(compound), +list(compound), -list(pair), -list(compound)), one_or_error).
	:- info(filter_feature_scores/6, [
		comment is 'Scores all features and reports complete-case counts. Implementations can override preprocessing and scoring.',
		argnames is ['Dataset', 'Features', 'Examples', 'Options', 'Scores', 'Diagnostics'],
		exceptions is [
			'A complete numeric feature value is not numeric' - type_error(number, 'Value'),
			'A complete categorical target is not atomic' - type_error(atomic, 'Target'),
			'A chi-square expectation is below the requested minimum' - domain_error(chi_square_expected_count, 'Feature-expected(ObservedMinimum,RequiredMinimum)')
		]
	]).

	:- protected(filter_validate_diagnostics/2).
	:- mode(filter_validate_diagnostics(+list(compound), +list(compound)), zero_or_one).
	:- info(filter_validate_diagnostics/2, [
		comment is 'Checks implementation-specific preparation metadata against effective options.',
		argnames is ['Options', 'Diagnostics']
	]).

	:- protected(filter_valid_preparation_diagnostics/2).
	:- mode(filter_valid_preparation_diagnostics(+atom, +list(compound)), zero_or_one).
	:- info(filter_valid_preparation_diagnostics/2, [
		comment is 'Checks the recorded preparation mode and joint complete-case counts.',
		argnames is ['Mode', 'Diagnostics']
	]).

	:- protected(filter_selection/3).
	:- mode(filter_selection(+term, +list(pair), -list(atomic)), one).
	:- info(filter_selection/3, [
		comment is 'Applies the receiving filter selection strategy to sorted feature scores.',
		argnames is ['Strategy', 'Scores', 'Selected']
	]).

	learn(Dataset, Selector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_examples(Dataset, Features, Examples),
		::filter_validate_dataset(Dataset, Features, Examples),
		::filter_feature_scores(Dataset, Features, Examples, Options, Scores, ScoreDiagnostics),
		^^option(selection_strategy(Strategy), Options),
		::filter_selection(Strategy, Scores, Selected),
		length(Examples, ExampleCount),
		length(Features, CandidateCount),
		length(Selected, SelectedCount),
		::filter_model(Model),
		^^base_selector_diagnostics(Model, ExampleCount, Options,
			[candidate_count(CandidateCount), selected_count(SelectedCount)| ScoreDiagnostics], Diagnostics),
		Selector =.. [Model, Scores, Selected, Diagnostics].

	filter_validate_dataset(_Dataset, _Features, _Examples).

	filter_validate_diagnostics(_Options, _Diagnostics).

	filter_valid_preparation_diagnostics(Mode, Diagnostics) :-
		memberchk(preparation_mode(Recorded), Diagnostics),
		Recorded == Mode,
		(	Mode == joint ->
			memberchk(example_count(Total), Diagnostics),
			memberchk(usable_example_count(Used), Diagnostics),
			memberchk(excluded_example_count(Excluded), Diagnostics),
			integer(Used),
			Used >= 0,
			integer(Excluded),
			Excluded >= 0,
			Total =:= Used + Excluded,
			memberchk(complete_cases(Counts), Diagnostics),
			filter_joint_counts(Counts, Used)
		;	true
		).

	filter_joint_counts([], _Used).
	filter_joint_counts([_-Count| Counts], Used) :-
		Count =:= Used,
		filter_joint_counts(Counts, Used).

	filter_feature_scores(_Dataset, Features, Examples, Options, Scores, [scoring_metric(Metric), complete_cases(Counts)]) :-
		::filter_scoring_metric(Options, Metric),
		^^score_features(Metric, Examples, Features, Scores),
		avltree::new(Empty),
		filter_example_counts(Examples, Empty, Dictionary),
		filter_feature_counts(Features, Dictionary, Counts).

	selected_features(Selector, Features) :-
		::check_selector(Selector),
		Selector =.. [_Model, _Scores, Features, _Diagnostics].

	feature_scores(Selector, Scores) :-
		::check_selector(Selector),
		Selector =.. [_Model, Scores, _Selected, _Diagnostics].

	check_selector(Selector) :-
		(	\+ ground(Selector) ->
			instantiation_error
		;	::filter_model(Model),
			Selector =.. [Model, Scores, Selected, Diagnostics],
			filter_valid_scores(Scores, Vocabulary),
			filter_valid_selected(Selected, Vocabulary),
			^^valid_selector_metadata(Model, Diagnostics),
			memberchk(options(Options), Diagnostics),
			^^valid_options(Options),
			::filter_scoring_metric(Options, Metric),
			memberchk(scoring_metric(RecordedMetric), Diagnostics),
			RecordedMetric == Metric,
			^^option(selection_strategy(Strategy), Options),
			::filter_selection(Strategy, Scores, Expected),
			Selected == Expected,
			memberchk(selected_count(SelectedCount), Diagnostics),
			valid(non_negative_integer, SelectedCount),
			length(Selected, SelectedCount),
			memberchk(candidate_count(CandidateCount), Diagnostics),
			valid(non_negative_integer, CandidateCount),
			length(Scores, CandidateCount),
			memberchk(example_count(ExampleCount), Diagnostics),
			memberchk(complete_cases(Counts), Diagnostics),
			valid(list(pair), Counts),
			length(Counts, CandidateCount),
			avltree::new(Empty),
			filter_valid_counts(Counts, Vocabulary, ExampleCount, Empty),
			::filter_validate_diagnostics(Options, Diagnostics) ->
			true
		;	domain_error(selector, Selector)
		).

	export_to_clauses(_Dataset, Selector, Functor, [Clause]) :-
		::check_selector(Selector),
		Clause =.. [Functor, Selector].

	selector_export_template(_Dataset, _Selector, Functor, Template) :-
		Template =.. [Functor, 'Selector'].

	selector_term_template(Selector, Template) :-
		::filter_model(Model),
		Selector =.. [Model, _Scores, _Selected, _Diagnostics],
		Template =.. [Model, 'FeatureScores', 'SelectedFeatures', 'Diagnostics'].

	print_selector(Selector) :-
		::check_selector(Selector),
		^^print_selector_template(Selector),
		writeq(Selector), nl.

	default_option(selection_strategy(top_k(10))).

	valid_option(selection_strategy(Strategy)) :-
		(	Strategy == all ->
			true
		;	Strategy = top_k(Count),
			integer(Count),
			Count > 0
		).
	valid_option(selection_strategy(threshold(Threshold))) :-
		number(Threshold).

	filter_selection(all, Scores, Features) :-
		filter_score_names(Scores, Features).
	filter_selection(top_k(Count), Scores, Features) :-
		^^select_top_k(Scores, Count, Features).
	filter_selection(threshold(Threshold), Scores, Features) :-
		^^select_above_threshold(Scores, Threshold, Features).

	filter_score_names([], []).
	filter_score_names([Feature-_Score| Scores], [Feature| Features]) :-
		filter_score_names(Scores, Features).

	filter_example_counts([], Counts, Counts).
	filter_example_counts([example(_Id, Features, Target)| Examples], Counts0, Counts) :-
		(	nonvar(Target) ->
			filter_row_counts(Features, Counts0, Counts1)
		;	Counts1 = Counts0
		),
		filter_example_counts(Examples, Counts1, Counts).

	filter_row_counts([], Counts, Counts).
	filter_row_counts([Feature-Value| Features], Counts0, Counts) :-
		(	nonvar(Value) ->
			(	avltree::lookup(Feature, Previous, Counts0) ->
				Count is Previous + 1
			;	Count = 1
			),
			avltree::insert(Counts0, Feature, Count, Counts1)
		;	Counts1 = Counts0
		),
		filter_row_counts(Features, Counts1, Counts).

	filter_feature_counts([], _Dictionary, []).
	filter_feature_counts([Feature| Features], Dictionary, [Feature-Count| Counts]) :-
		(	avltree::lookup(Feature, Known, Dictionary) ->
			Count = Known
		;	Count = 0
		),
		filter_feature_counts(Features, Dictionary, Counts).

	filter_valid_scores(Scores, Vocabulary) :-
		valid(list(pair), Scores),
		avltree::new(Empty),
		filter_score_vocabulary(Scores, Empty, Vocabulary),
		filter_decreasing_scores(Scores).

	filter_score_vocabulary([], Vocabulary, Vocabulary).
	filter_score_vocabulary([Feature-Score| Scores], Vocabulary0, Vocabulary) :-
		atomic(Feature),
		number(Score),
		\+ avltree::lookup(Feature, _, Vocabulary0),
		avltree::insert(Vocabulary0, Feature, Score, Vocabulary1),
		filter_score_vocabulary(Scores, Vocabulary1, Vocabulary).

	filter_decreasing_scores([]).
	filter_decreasing_scores([_Feature-Score| Scores]) :-
		filter_decreasing_scores_(Scores, Score).

	filter_decreasing_scores_([], _Previous).
	filter_decreasing_scores_([_Feature-Score| Scores], Previous) :-
		Previous >= Score,
		filter_decreasing_scores_(Scores, Score).

	filter_valid_selected(Selected, Vocabulary) :-
		valid(list(atomic), Selected),
		avltree::new(Empty),
		filter_selected_names(Selected, Vocabulary, Empty).

	filter_selected_names([], _Vocabulary, _Seen).
	filter_selected_names([Feature| Features], Vocabulary, Seen0) :-
		avltree::lookup(Feature, _, Vocabulary),
		\+ avltree::lookup(Feature, _, Seen0),
		avltree::insert(Seen0, Feature, true, Seen),
		filter_selected_names(Features, Vocabulary, Seen).

	filter_valid_counts([], _Vocabulary, _ExampleCount, _Seen).
	filter_valid_counts([Feature-Count| Counts], Vocabulary, ExampleCount, Seen0) :-
		avltree::lookup(Feature, _, Vocabulary),
		valid(non_negative_integer, Count),
		Count =< ExampleCount,
		\+ avltree::lookup(Feature, _, Seen0),
		avltree::insert(Seen0, Feature, true, Seen),
		filter_valid_counts(Counts, Vocabulary, ExampleCount, Seen).

:- end_category.
