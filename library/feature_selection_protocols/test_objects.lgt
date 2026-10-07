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


:- object(sample_selector,
	imports([options, feature_selector_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-02,
		comment is 'Minimal filter-based feature selector used to exercise the feature_selector_protocol and feature_selector_common shared code end-to-end: scores every feature using a pluggable scoring metric, then applies a top-k or threshold selection strategy.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, sample_selector(Metric, FeatureScores, SelectedFeatures, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(scoring_metric(Metric), Options),
		^^option(selection_strategy(Strategy), Options),
		^^dataset_examples(Dataset, FeatureNames, Examples),
		^^score_features(Metric, Examples, FeatureNames, FeatureScores),
		apply_strategy(Strategy, FeatureScores, SelectedFeatures),
		length(Examples, ExampleCount),
		^^base_selector_diagnostics(sample_selector, ExampleCount, Options, [selected_count(SelectedCount)], Diagnostics),
		length(SelectedFeatures, SelectedCount).

	apply_strategy(all, FeatureScores, Selected) :-
		score_feature_names(FeatureScores, Selected).
	apply_strategy(top_k(K), FeatureScores, Selected) :-
		^^select_top_k(FeatureScores, K, Selected).
	apply_strategy(threshold(Threshold), FeatureScores, Selected) :-
		^^select_above_threshold(FeatureScores, Threshold, Selected).

	selected_features(Selector, Features) :-
		check_selector(Selector),
		Selector = sample_selector(_Metric, _FeatureScores, Features, _Diagnostics).

	feature_scores(Selector, FeatureScores) :-
		check_selector(Selector),
		Selector = sample_selector(_Metric, FeatureScores, _Features, _Diagnostics).

	check_selector(Selector) :-
		(	var(Selector) ->
			instantiation_error
		;	Selector = sample_selector(Metric, FeatureScores, SelectedFeatures, Diagnostics),
			ground(Selector),
			valid_option(scoring_metric(Metric)),
			valid_feature_scores(FeatureScores, Vocabulary),
			valid_selected_features(SelectedFeatures, Vocabulary),
			^^valid_selector_metadata(sample_selector, Diagnostics),
			memberchk(options(Options), Diagnostics),
			^^valid_options(Options),
			^^option(scoring_metric(RecordedMetric), Options),
			RecordedMetric == Metric,
			^^option(selection_strategy(Strategy), Options),
			apply_strategy(Strategy, FeatureScores, ExpectedFeatures),
			SelectedFeatures == ExpectedFeatures,
			memberchk(selected_count(SelectedCount), Diagnostics),
			valid(non_negative_integer, SelectedCount),
			length(SelectedFeatures, SelectedCount) ->
			true
		;	domain_error(selector, Selector)
		).

	valid_feature_scores(FeatureScores, Vocabulary) :-
		valid(list(pair), FeatureScores),
		avltree::new(Empty),
		valid_score_pairs(FeatureScores, Empty, Vocabulary),
		decreasing_scores(FeatureScores).

	valid_score_pairs([], Vocabulary, Vocabulary).
	valid_score_pairs([Feature-Score| Scores], Vocabulary0, Vocabulary) :-
		atomic(Feature),
		number(Score),
		\+ avltree::lookup(Feature, _, Vocabulary0),
		avltree::insert(Vocabulary0, Feature, Score, Vocabulary1),
		valid_score_pairs(Scores, Vocabulary1, Vocabulary).

	decreasing_scores([]).
	decreasing_scores([_Feature-Score| Scores]) :-
		decreasing_scores_(Scores, Score).

	decreasing_scores_([], _Previous).
	decreasing_scores_([_Feature-Score| Scores], Previous) :-
		Previous >= Score,
		decreasing_scores_(Scores, Score).

	valid_selected_features(Features, Vocabulary) :-
		valid(list(atomic), Features),
		avltree::new(Empty),
		valid_selected_features_(Features, Vocabulary, Empty).

	valid_selected_features_([], _Vocabulary, _Seen).
	valid_selected_features_([Feature| Features], Vocabulary, Seen0) :-
		avltree::lookup(Feature, _, Vocabulary),
		\+ avltree::lookup(Feature, _, Seen0),
		avltree::insert(Seen0, Feature, true, Seen),
		valid_selected_features_(Features, Vocabulary, Seen).

	score_feature_names([], []).
	score_feature_names([Feature-_Score| Scores], [Feature| Features]) :-
		score_feature_names(Scores, Features).

	selector_export_template(_Dataset, _Selector, Functor, Template) :-
		Template =.. [Functor, 'Selector'].

	selector_term_template(
		sample_selector(_Metric, _FeatureScores, _SelectedFeatures, _Diagnostics),
		sample_selector('Metric', 'FeatureScores', 'SelectedFeatures', 'Diagnostics')
	).

	export_to_clauses(_Dataset, Selector, Functor, [Clause]) :-
		check_selector(Selector),
		Clause =.. [Functor, Selector].

	print_selector(Selector) :-
		check_selector(Selector),
		^^print_selector_template(Selector),
		writeq(Selector), nl.

	default_option(scoring_metric(variance_score)).
	default_option(selection_strategy(all)).

	valid_option(scoring_metric(Metric)) :-
		catch(^^check_scoring_metric(Metric), _Error, fail).
	valid_option(selection_strategy(Strategy)) :-
		(	Strategy == all ->
			true
		;	Strategy = top_k(K),
			integer(K),
			K > 0
		).
	valid_option(selection_strategy(threshold(Threshold))) :-
		number(Threshold).

:- end_object.


:- object(feature_selection_declared_metric,
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Declaration-only feature metric fixture.'
	]).

:- end_object.


:- object(feature_selection_inherited_metric,
	extends(variance_score)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Inherited feature metric fixture.'
	]).

:- end_object.


:- object(feature_selection_categorical_dataset,
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Categorical scoring fixture with informative, independent, and constant features.'
	]).

	attribute_values(signal, [a, b]).
	attribute_values(noise, [a, b]).
	attribute_values(constant, [a]).

	example_count(4).

	example(1, [signal-a, noise-a, constant-a], x).
	example(2, [signal-a, noise-b, constant-a], x).
	example(3, [signal-b, noise-a, constant-a], y).
	example(4, [signal-b, noise-b, constant-a], y).

:- end_object.


:- category(feature_selection_metric_category,
	implements(feature_scoring_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Category-provided metric returning the first feature value for sorting tests.'
	]).

	score([Value| _Values], _Targets, Value).

:- end_category.


:- object(feature_selection_category_metric,
	imports(feature_selection_metric_category)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Category-provided feature metric fixture.'
	]).

:- end_object.


:- object(feature_selection_invalid_dataset(_Kind_),
	implements(feature_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Invalid feature dataset fixtures for feature and count validation.'
	]).

	attribute_values(f1, continuous).
	attribute_values(f1, continuous) :-
		parameter(1, duplicate_declaration).

	example_count(Count) :-
		parameter(1, Kind),
		(	Kind == zero_count ->
			Count = 0
		;	(	Kind == non_integer_count ->
				Count = bad
			;	Count = 1
			)
		).

	example(1, Features, _Target) :-
		parameter(1, Kind),
		fixture_features(Kind, Features).

	fixture_features(duplicate_declaration, [f1-1.0]).
	fixture_features(duplicate_example, [f1-1.0, f1-100.0]).
	fixture_features(malformed_features, [not_a_pair]).
	fixture_features(variable_feature, [_Feature-1.0]).
	fixture_features(zero_count, [f1-1.0]).
	fixture_features(non_integer_count, [f1-1.0]).

:- end_object.
