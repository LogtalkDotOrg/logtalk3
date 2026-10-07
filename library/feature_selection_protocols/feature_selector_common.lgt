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


:- category(feature_selector_common,
	implements(feature_selector_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-06,
		comment is 'Shared predicates for selector diagnostics, dataset validation, feature-matrix utilities, and selection strategies (top-k and threshold).'
	]).

	:- uses(format, [
		format/2, format/3
	]).

	:- uses(list, [
		last/2, length/2, member/2, memberchk/2, msort/3
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	% hook predicates that concrete selector implementations must define

	:- protected(selector_diagnostics_data/2).
	:- mode(selector_diagnostics_data(+compound, -list(compound)), one).
	:- info(selector_diagnostics_data/2, [
		comment is 'Hook predicate that importing selector implementations must define in order to expose diagnostics metadata. A default implementation is provided that assumes the diagnostics list is the last argument of the selector term; concrete implementations following that convention do not need to override it.',
		argnames is ['Selector', 'Diagnostics']
	]).

	:- protected(selector_export_template/4).
	:- mode(selector_export_template(+object_identifier, +compound, +atom, -callable), one).
	:- info(selector_export_template/4, [
		comment is 'Hook predicate that importing selector implementations must define in order to expose the exported selector template for a given functor.',
		argnames is ['Dataset', 'Selector', 'Functor', 'Template']
	]).

	:- protected(selector_term_template/2).
	:- mode(selector_term_template(+compound, -callable), one).
	:- info(selector_term_template/2, [
		comment is 'Hook predicate that importing selector implementations must define in order to expose the learned selector term template used by pretty-printing helpers.',
		argnames is ['Selector', 'Template']
	]).

	% pretty-printing helper

	:- protected(print_selector_template/1).
	:- mode(print_selector_template(+compound), one).
	:- info(print_selector_template/1, [
		comment is 'Pretty-printing helper predicate used by importing selector implementations to show the learned selector term template.',
		argnames is ['Selector']
	]).

	% default protocol predicate implementations

	learn(Dataset, Selector) :-
		::learn(Dataset, Selector, []).

	check_selector(Selector) :-
		(	\+ ground(Selector) ->
			instantiation_error
		;	::selector_term_template(Selector, _Template),
			::selector_diagnostics_data(Selector, Diagnostics),
			valid(list(compound), Diagnostics),
			memberchk(model(Model), Diagnostics),
			atom(Model),
			valid_selector_metadata(Model, Diagnostics),
			memberchk(options(Options), Diagnostics),
			^^valid_options(Options) ->
			true
		;	domain_error(selector, Selector)
		).

	valid_selector(Selector) :-
		catch(::check_selector(Selector), _Error, fail).

	diagnostics(Selector, Diagnostics) :-
		::selector_diagnostics_data(Selector, Diagnostics).

	diagnostic(Selector, Diagnostic) :-
		::selector_diagnostics_data(Selector, Diagnostics),
		member(Diagnostic, Diagnostics).

	selector_options(Selector, Options) :-
		::selector_diagnostics_data(Selector, Diagnostics),
		memberchk(options(Options), Diagnostics).

	selector_diagnostics_data(Selector, Diagnostics) :-
		Selector =.. [_| Arguments],
		last(Arguments, Diagnostics).

	print_selector_template(Selector) :-
		::selector_term_template(Selector, Template),
		format('Template: ~w~n', [Template]).

	% dataset collection and validation

	:- protected(dataset_examples/2).
	:- mode(dataset_examples(+object_identifier, -list(compound)), one_or_error).
	:- info(dataset_examples/2, [
		comment is 'Collects and validates dataset examples. Rejects duplicate declarations, repeated features within examples, unknown features, and inconsistent example counts.',
		argnames is ['Dataset', 'Examples'],
		exceptions is [
			'A feature name, feature list, or declared count is a variable' - instantiation_error,
			'The dataset contains no examples' - domain_error(non_empty_examples, 'Dataset'),
			'An example names a feature not declared by attribute_values/2' - domain_error(unknown_feature, 'Feature'),
			'A feature is declared or supplied more than once' - domain_error(duplicate_feature, 'Feature'),
			'A feature name is not atomic' - type_error(atomic, 'Feature'),
			'An example feature list is not a list' - type_error(list, 'Features'),
			'An example feature entry is not a pair' - type_error(pair, 'Entry'),
			'The declared count is not an integer' - type_error(integer, 'DeclaredCount'),
			'The declared count is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'The declared and observed example counts differ' - consistency_error(example_count, 'DeclaredCount', 'ObservedCount')
		]
	]).

	dataset_examples(Dataset, Examples) :-
		dataset_examples(Dataset, _Features, Examples).

	:- protected(dataset_examples/3).
	:- mode(dataset_examples(+object_identifier, -list(atomic), -list(compound)), one_or_error).
	:- info(dataset_examples/3, [
		comment is 'Collects declared feature names and validated examples in one pass over the dataset predicates. Uses the same validation as dataset_examples/2.',
		argnames is ['Dataset', 'Features', 'Examples'],
		exceptions is [
			'A feature name, feature list, or declared count is a variable' - instantiation_error,
			'The dataset contains no examples' - domain_error(non_empty_examples, 'Dataset'),
			'A feature is declared or supplied more than once' - domain_error(duplicate_feature, 'Feature'),
			'An example names an undeclared feature' - domain_error(unknown_feature, 'Feature'),
			'A feature name is not atomic' - type_error(atomic, 'Feature'),
			'An example feature list is not a list' - type_error(list, 'Features'),
			'An example feature entry is not a pair' - type_error(pair, 'Entry'),
			'The declared count is not an integer' - type_error(integer, 'DeclaredCount'),
			'The declared count is not positive' - domain_error(positive_integer, 'DeclaredCount'),
			'The declared and observed example counts differ' - consistency_error(example_count, 'DeclaredCount', 'ObservedCount')
		]
	]).

	dataset_examples(Dataset, Features, Examples) :-
		feature_vocabulary(Dataset, Features, Vocabulary),
		findall(
			example(Id, ExampleFeatures, Target),
			Dataset::example(Id, ExampleFeatures, Target),
			Examples0
		),
		(	Examples0 == [] ->
			domain_error(non_empty_examples, Dataset)
		;	true
		),
		check_known_features(Examples0, Vocabulary),
		length(Examples0, ObservedCount),
		Dataset::example_count(DeclaredCount),
		context(Context),
		check(positive_integer, DeclaredCount, Context),
		(	DeclaredCount =:= ObservedCount ->
			Examples = Examples0
		;	consistency_error(example_count, DeclaredCount, ObservedCount)
		).

	feature_vocabulary(Dataset, Features, Vocabulary) :-
		findall(Feature, Dataset::attribute_values(Feature, _Values), Features),
		avltree::new(Empty),
		feature_vocabulary_(Features, Empty, Vocabulary).

	feature_vocabulary_([], Vocabulary, Vocabulary).
	feature_vocabulary_([Feature| Features], Vocabulary0, Vocabulary) :-
		context(Context),
		check(atomic, Feature, Context),
		(	avltree::lookup(Feature, _, Vocabulary0) ->
			domain_error(duplicate_feature, Feature)
		;	avltree::insert(Vocabulary0, Feature, true, Vocabulary1)
		),
		feature_vocabulary_(Features, Vocabulary1, Vocabulary).

	check_known_features([], _Features).
	check_known_features([example(_Id, ExampleFeatures, _Target)| Examples], Features) :-
		context(Context),
		check(list(pair), ExampleFeatures, Context),
		avltree::new(Empty),
		check_known_features_(ExampleFeatures, Features, Empty),
		check_known_features(Examples, Features).

	check_known_features_([], _Features, _Seen).
	check_known_features_([Feature-_Value| ExampleFeatures], Features, Seen0) :-
		context(Context),
		check(atomic, Feature, Context),
		(	avltree::lookup(Feature, _, Features) ->
			true
		;	domain_error(unknown_feature, Feature)
		),
		(	avltree::lookup(Feature, _, Seen0) ->
			domain_error(duplicate_feature, Feature)
		;	avltree::insert(Seen0, Feature, true, Seen)
		),
		check_known_features_(ExampleFeatures, Features, Seen).

	% feature-matrix utilities

	:- protected(feature_names/2).
	:- mode(feature_names(+object_identifier, -list(atomic)), one_or_error).
	:- info(feature_names/2, [
		comment is 'Returns unique, atomic candidate features in declaration order, rejecting invalid or duplicate declarations.',
		argnames is ['Dataset', 'FeatureNames'],
		exceptions is [
			'A declared feature name is a variable' - instantiation_error,
			'A declared feature name is not atomic' - type_error(atomic, 'Feature'),
			'A feature is declared more than once' - domain_error(duplicate_feature, 'Feature')
		]
	]).

	feature_names(Dataset, FeatureNames) :-
		feature_vocabulary(Dataset, FeatureNames, _Vocabulary).

	:- protected(feature_values/3).
	:- mode(feature_values(+list(compound), +atomic, -list), one).
	:- info(feature_values/3, [
		comment is 'Returns the value of one feature for every example, in example order. An example that does not mention the feature contributes an unbound variable (a missing value).',
		argnames is ['Examples', 'Feature', 'Values']
	]).

	feature_values([], _Feature, []).
	feature_values([example(_Id, ExampleFeatures, _Target)| Examples], Feature, [Value| Values]) :-
		(	memberchk(Feature-FoundValue, ExampleFeatures) ->
			Value = FoundValue
		;	true
		),
		feature_values(Examples, Feature, Values).

	:- protected(targets/2).
	:- mode(targets(+list(compound), -list), one).
	:- info(targets/2, [
		comment is 'Returns the target of every example, in example order (an unbound variable for an example with no known target).',
		argnames is ['Examples', 'Targets']
	]).

	targets([], []).
	targets([example(_Id, _Features, Target)| Examples], [Target| Targets]) :-
		targets(Examples, Targets).

	% selection strategies

	:- protected(score_features/4).
	:- mode(score_features(+object_identifier, +list(compound), +list(atomic), -list(pair)), one).
	:- info(score_features/4, [
		comment is 'Scores every feature using score/3 messages to the metric object. Returns ``Feature-Score`` pairs in decreasing score order, preserving input feature order for numeric ties.',
		argnames is ['Metric', 'Examples', 'FeatureNames', 'FeatureScores']
	]).

	score_features(Metric, Examples, FeatureNames, FeatureScores) :-
		targets(Examples, Targets),
		index_examples(Examples, Indexed),
		score_features_(FeatureNames, Metric, Indexed, Targets, Scored),
		sort_by_decreasing_score(Scored, FeatureScores).

	index_examples([], []).
	index_examples([example(_Id, Features, _Target)| Examples], [Dictionary| Indexed]) :-
		avltree::as_dictionary(Features, Dictionary),
		index_examples(Examples, Indexed).

	indexed_feature_values([], _Feature, []).
	indexed_feature_values([Dictionary| Indexed], Feature, [Value| Values]) :-
		(	avltree::lookup(Feature, FoundValue, Dictionary) ->
			Value = FoundValue
		;	true
		),
		indexed_feature_values(Indexed, Feature, Values).

	score_features_([], _Metric, _Examples, _Targets, []).
	score_features_([Feature| FeatureNames], Metric, Examples, Targets, [Feature-Score| Scored]) :-
		indexed_feature_values(Examples, Feature, Values),
		Metric::score(Values, Targets, Score),
		score_features_(FeatureNames, Metric, Examples, Targets, Scored).

	:- protected(sort_by_decreasing_score/2).
	:- mode(sort_by_decreasing_score(+list(pair), -list(pair)), one).
	:- info(sort_by_decreasing_score/2, [
		comment is 'Sorts feature scores numerically in decreasing order, preserving input order for ties.',
		argnames is ['FeatureScores', 'Sorted']
	]).

	sort_by_decreasing_score(FeatureScores, Sorted) :-
		score_keys(FeatureScores, 0, Keyed),
		msort(compare_feature_scores, Keyed, KeySorted),
		remove_score_keys(KeySorted, Sorted).

	score_keys([], _Index, []).
	score_keys([Feature-Score| Pairs], Index, [scored(Score, Index, Feature)| Keyed]) :-
		NextIndex is Index + 1,
		score_keys(Pairs, NextIndex, Keyed).

	:- private(compare_feature_scores/3).
	:- mode(compare_feature_scores(-atom, +compound, +compound), one).
	:- info(compare_feature_scores/3, [
		comment is 'Compares numerically decreasing scores, breaking ties by increasing input position.',
		argnames is ['Order', 'ScoredFeature1', 'ScoredFeature2']
	]).

	compare_feature_scores(Order, scored(Score1, Index1, _), scored(Score2, Index2, _)) :-
		(	Score1 > Score2 ->
			Order = (<)
		;	(	Score1 < Score2 ->
				Order = (>)
			;	compare(Order, Index1, Index2)
			)
		).

	remove_score_keys([], []).
	remove_score_keys([scored(Score, _Index, Feature)| Keyed], [Feature-Score| Pairs]) :-
		remove_score_keys(Keyed, Pairs).

	:- protected(select_top_k/3).
	:- mode(select_top_k(+list(pair), +positive_integer, -list(atomic)), one).
	:- info(select_top_k/3, [
		comment is 'Returns the feature names of the K highest-scoring features (``FeatureScores`` already sorted by decreasing score, as returned by score_features/4), preserving that order. Returns every feature name, still in that order, when fewer than K are given.',
		argnames is ['FeatureScores', 'K', 'Selected']
	]).

	select_top_k(FeatureScores, K, Selected) :-
		take_at_most(K, FeatureScores, TopK),
		feature_names_of(TopK, Selected).

	take_at_most(_K, [], []) :-
		!.
	take_at_most(0, _FeatureScores, []) :-
		!.
	take_at_most(K, [FeatureScore| FeatureScores], [FeatureScore| Taken]) :-
		K > 0,
		K1 is K - 1,
		take_at_most(K1, FeatureScores, Taken).

	:- protected(select_above_threshold/3).
	:- mode(select_above_threshold(+list(pair), +number, -list(atomic)), one).
	:- info(select_above_threshold/3, [
		comment is 'Returns the feature names of every feature whose score is at least Threshold (``FeatureScores`` already sorted by decreasing score, as returned by score_features/4), preserving that order.',
		argnames is ['FeatureScores', 'Threshold', 'Selected']
	]).

	select_above_threshold(FeatureScores, Threshold, Selected) :-
		above_threshold(FeatureScores, Threshold, Selected0),
		feature_names_of(Selected0, Selected).

	above_threshold([], _Threshold, []).
	above_threshold([Feature-Score| FeatureScores], Threshold, Selected) :-
		(	Score >= Threshold ->
			Selected = [Feature-Score| Selected0]
		;	Selected = Selected0
		),
		above_threshold(FeatureScores, Threshold, Selected0).

	feature_names_of([], []).
	feature_names_of([Feature-_Score| FeatureScores], [Feature| FeatureNames]) :-
		feature_names_of(FeatureScores, FeatureNames).

	% diagnostics helpers

	:- protected(base_selector_diagnostics/5).
	:- mode(base_selector_diagnostics(+atom, +positive_integer, +list(compound), +list(compound), -list(compound)), one).
	:- info(base_selector_diagnostics/5, [
		comment is 'Builds the common part of a selector diagnostics metadata list, combined with implementation-specific extra diagnostics terms.',
		argnames is ['Model', 'ExampleCount', 'Options', 'ExtraDiagnostics', 'Diagnostics']
	]).

	base_selector_diagnostics(Model, ExampleCount, Options, ExtraDiagnostics, Diagnostics) :-
		Diagnostics = [
			model(Model),
			example_count(ExampleCount),
			options(Options)
		| ExtraDiagnostics
		].

	:- protected(valid_selector_metadata/2).
	:- mode(valid_selector_metadata(+atom, +list(compound)), zero_or_one).
	:- info(valid_selector_metadata/2, [
		comment is 'True when ground diagnostics metadata contains the expected model, a positive example count, and a list of stored options.',
		argnames is ['Model', 'Diagnostics']
	]).

	valid_selector_metadata(Model, Diagnostics) :-
		ground(Diagnostics),
		valid(list(compound), Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(example_count(Count), Diagnostics),
		valid(positive_integer, Count),
		memberchk(options(Options), Diagnostics),
		valid(list(compound), Options).

	:- protected(valid_selector_metadata/3).
	:- mode(valid_selector_metadata(+atom, +list(compound), +list(compound)), zero_or_one).
	:- info(valid_selector_metadata/3, [
		comment is 'True when diagnostics metadata contains the expected model term and records the given effective options.',
		argnames is ['Model', 'Options', 'Diagnostics']
	]).

	valid_selector_metadata(Model, Options, Diagnostics) :-
		valid_selector_metadata(Model, Diagnostics),
		memberchk(options(Options), Diagnostics).

	% check helper

	:- protected(check_scoring_metric/1).
	:- mode(check_scoring_metric(@callable), one_or_error).
	:- info(check_scoring_metric/1, [
		comment is 'Checks that a scoring metric option value names an object implementing score/3 (feature_scoring_protocol).',
		argnames is ['Metric'],
		exceptions is [
			'``Metric`` is a variable' - instantiation_error,
			'``Metric`` is bound but does not implement score/3' - domain_error(scoring_metric, 'Metric')
		]
	]).

	check_scoring_metric(Metric) :-
		(	var(Metric) ->
			instantiation_error
		;	ground(Metric),
			catch(
				(	Metric::predicate_property(score(_, _, _), public),
					Metric::predicate_property(score(_, _, _), defined_in(_))
				),
				_Error,
				fail
			) ->
			true
		;	domain_error(scoring_metric, Metric)
		).

	% export

	export_to_file(Dataset, Selector, Functor, File) :-
		::export_to_clauses(Dataset, Selector, Functor, Clauses),
		open(File, write, Stream),
		(	catch(
				(	write_comment_header(Dataset, Functor, Selector, Stream),
					write_clauses(Clauses, Stream)
				),
				Error,
				(safe_close_stream(Stream), throw(Error))
			) ->
			close(Stream)
		;	safe_close_stream(Stream),
			fail
		).

	safe_close_stream(Stream) :-
		catch(close(Stream), _, true).

	write_comment_header(Dataset, Functor, Selector, Stream) :-
		::selector_export_template(Dataset, Selector, Functor, Template),
		functor(Template, _, Arity),
		format(Stream, '% exported selector predicate: ~q/~d~n', [Functor, Arity]),
		format(Stream, '% training dataset: ~q~n', [Dataset]),
		::dataset_examples(Dataset, Examples),
		length(Examples, Count),
		format(Stream, '% training example count: ~d~n', [Count]),
		(	::diagnostics(Selector, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;	true
		),
		format(Stream, '% ~w~n', [Template]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
