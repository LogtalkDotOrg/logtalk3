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


:- object(z_score,
	imports(anomaly_detector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Statistical Z-score anomaly detector for continuous datasets. Learns per-attribute population mean and standard deviation from a dataset object implementing the ``anomaly_dataset_protocol`` protocol and returns a detector term that can be used for scoring, prediction, and export.',
		remarks is [
			'Algorithm' - 'This is a statistical anomaly-detection method based on standard scores. The detector estimates a population mean and standard deviation for each continuous attribute and supports two learn-time score modes: ``root_mean_square`` aggregates all known Z-scores into a dense multivariate deviation score while ``any_feature_extreme`` uses the maximum absolute Z-score to emphasize sparse single-feature anomalies.',
			'Feature handling' - 'Supports continuous attributes only. Missing values are ignored when fitting attribute statistics. During scoring, queries must contain at least one known value. The ``root_mean_square`` score mode normalizes the raw score by the number of known values so that scores remain comparable across different missing-value patterns.',
			'Predict-time options' - 'The ``score_mode/1`` option is a learn-time option. ``predict/4`` always uses the score mode stored in the learned detector. If ``score_mode/1`` is passed to ``predict/4``, it is ignored.',
			'Score normalization' - 'The raw multivariate root-mean-square Z-score is mapped to the interval ``[0.0,1.0)`` using ``Score = Raw / (1 + Raw)``. With this normalization a raw score of ``3.0`` maps to ``0.75``.',
			'Detector representation' - 'The learned detector is represented as a ``z_score_detector(TrainingDataset, Encoders, Diagnostics)`` term where ``Diagnostics`` stores the learned metadata, including the effective options.'
		],
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, modified_z_score, isolation_forest, knn_distance, lof]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2, reverse/2
	]).

	:- uses(numberlist, [
		euclidean_norm/2
	]).

	:- uses(pairs, [
		keys/2, values/2
	]).

	:- uses(population, [
		arithmetic_mean/2, standard_deviation/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Detector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		check_attribute_names_non_empty(AttributeNames, Dataset),
		findall(
			Id-Class-AttributeValues,
			Dataset::example(Id, Class, AttributeValues),
			Examples
		),
		^^check_examples_non_empty(Dataset, Examples),
		check_example_values(Examples, AttributeNames),
		build_encoders(AttributeNames, Examples, Encoders),
		length(Examples, ExampleCount),
		build_diagnostics(Dataset, AttributeNames, ExampleCount, Options, Diagnostics),
		Detector = z_score_detector(Dataset, Encoders, Diagnostics).

	check_anomaly_detector(Detector) :-
		(	var(Detector) ->
			instantiation_error
		;	Detector = z_score_detector(TrainingDataset, Encoders, Diagnostics),
			valid(object_identifier, TrainingDataset),
			valid_zscore_encoders(Encoders),
			valid_detector_diagnostics(TrainingDataset, Encoders, Diagnostics) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(z_score_detector(_Dataset, _Encoders, Diagnostics), Diagnostics).

	score(Detector, Instance, Score) :-
		detector_data(Detector, _TrainingDataset, Encoders, Diagnostics),
		detector_options(Diagnostics, Options),
		raw_score_for_attribute_values(Encoders, Options, Instance, RawScore),
		normalize_raw_score(RawScore, Score).

	score_all(Dataset, Detector, Scores) :-
		detector_data(Detector, _TrainingDataset, Encoders, Diagnostics),
		detector_options(Diagnostics, Options),
		findall(
			Score-Id-Class,
			(
				Dataset::example(Id, Class, AttributeValues),
				raw_score_for_attribute_values(Encoders, Options, AttributeValues, RawScore),
				normalize_raw_score(RawScore, Score)
			),
			ScoredPairs
		),
		msort(ScoredPairs, SortedAscending),
		reverse(SortedAscending, SortedDescending),
		^^extract_scores(SortedDescending, Scores).

	export_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Detector) :-
		detector_data(Detector, _TrainingDataset, Encoders, Diagnostics),
		encoder_attribute_names(Encoders, AttributeNames),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		format('Z-Score Anomaly Detector~n', []),
		format('========================~n~n', []),
		format('Training examples:  ~w~n', [ExampleCount]),
		format('Features:           ~w~n', [AttributeNames]),
		format('Encoders:           ~w~n', [Encoders]),
		^^print_anomaly_detector_template(Detector),
		format('Options:            ~w~n', [Options]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(z_score_detector(_TrainingDataset, _Encoders, _Diagnostics), z_score_detector('TrainingDataset', 'Encoders', 'Diagnostics')).

	detector_data(Detector, TrainingDataset, Encoders, Diagnostics) :-
		Detector =.. [_Functor, TrainingDataset, Encoders, Diagnostics].

	detector_options(Diagnostics, Options) :-
		memberchk(options(Options), Diagnostics).

	build_diagnostics(Dataset, AttributeNames, ExampleCount, Options, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		Diagnostics = [
			model(z_score),
			training_dataset(Dataset),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			example_count(ExampleCount),
			options(Options)
		].

	check_continuous_attributes([]).
	check_continuous_attributes([Attribute-Values| Attributes]) :-
		(	Values == continuous ->
			true
		;	domain_error(continuous_attribute, Attribute)
		),
		check_continuous_attributes(Attributes).

	check_attribute_names_non_empty(AttributeNames, Dataset) :-
		(	AttributeNames == [] ->
			domain_error(non_empty_features, Dataset)
		;	true
		).

	check_example_values([], _AttributeNames).
	check_example_values([_Id-_Class-AttributeValues| Examples], AttributeNames) :-
		check_example_attributes(AttributeNames, AttributeValues),
		check_example_values(Examples, AttributeNames).

	check_example_attributes(AttributeNames, AttributeValues) :-
		checked_attribute_values(AttributeNames, AttributeValues, _OrderedValues).

	checked_attribute_values(AttributeNames, AttributeValues, OrderedValues) :-
		index_attribute_names(AttributeNames, IndexedAttributeNames),
		keysort(IndexedAttributeNames, SortedIndexedAttributeNames),
		keysort(AttributeValues, SortedAttributeValues),
		indexed_attribute_values(SortedIndexedAttributeNames, SortedAttributeValues, IndexedValues),
		keysort(IndexedValues, OrderedIndexedValues),
		values(OrderedIndexedValues, OrderedValues).

	index_attribute_names(AttributeNames, IndexedAttributeNames) :-
		index_attribute_names(AttributeNames, 1, IndexedAttributeNames).

	index_attribute_names([], _Index, []).
	index_attribute_names([Attribute| AttributeNames], Index, [Attribute-Index| IndexedAttributeNames]) :-
		NextIndex is Index + 1,
		index_attribute_names(AttributeNames, NextIndex, IndexedAttributeNames).

	indexed_attribute_values([], [], []) :-
		!.
	indexed_attribute_values([Attribute-_| _], [], _) :-
		existence_error(attribute, Attribute).
	indexed_attribute_values([], [Attribute-_Value| _], _) :-
		domain_error(declared_attribute, Attribute).
	indexed_attribute_values([Attribute-Index| IndexedAttributeNames], [Attribute-Value| AttributeValues], [Index-Value| IndexedValues]) :-
		!,
		check_attribute_value(Value),
		(	AttributeValues = [Attribute-_Duplicate| _] ->
			domain_error(attribute_occurrences, Attribute)
		;	indexed_attribute_values(IndexedAttributeNames, AttributeValues, IndexedValues)
		).
	indexed_attribute_values([Attribute-_| _], [OtherAttribute-_Value| _], _) :-
		compare(Order, Attribute, OtherAttribute),
		indexed_attribute_value_error(Order, Attribute, OtherAttribute).

	indexed_attribute_value_error(<, Attribute, _OtherAttribute) :-
		existence_error(attribute, Attribute).
	indexed_attribute_value_error(>, _Attribute, OtherAttribute) :-
		domain_error(declared_attribute, OtherAttribute).

	check_attribute_value(Value) :-
		(	var(Value) ->
			true
		;	number(Value) ->
			true
		;	type_error(number, Value)
		).

	attribute_value(Attribute, AttributeValues, Value) :-
		(	member(Attribute-Value, AttributeValues) ->
			true
		;	existence_error(attribute, Attribute)
		).

	build_encoders([], _Examples, []).
	build_encoders([Attribute| Attributes], Examples, [zscore(Attribute, Mean, Scale)| Encoders]) :-
		known_attribute_values(Examples, Attribute, Values),
		(	Values == [] ->
			domain_error(known_attribute_values, Attribute)
		;	population::arithmetic_mean(Values, Mean),
			population::standard_deviation(Values, Scale0),
			normalize_scale(Scale0, Scale)
		),
		build_encoders(Attributes, Examples, Encoders).

	known_attribute_values([], _Attribute, []).
	known_attribute_values([_Id-_Class-AttributeValues| Examples], Attribute, Values) :-
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			Values = Tail
		;	number(Value) ->
			Values = [Value| Tail]
		;	type_error(number, Value)
		),
		known_attribute_values(Examples, Attribute, Tail).

	normalize_scale(Scale0, Scale) :-
		(	Scale0 =< 0.0 ->
			Scale = 1.0
		;	Scale = Scale0
		).

	raw_score_for_attribute_values(Encoders, Options, AttributeValues, RawScore) :-
		encoder_attribute_names(Encoders, AttributeNames),
		checked_attribute_values(AttributeNames, AttributeValues, OrderedValues),
		known_zscores(Encoders, OrderedValues, ZScores, 0, KnownCount),
		(	KnownCount == 0 ->
			domain_error(non_empty_known_values, AttributeNames)
		;	raw_score_for_zscores(ZScores, KnownCount, Options, RawScore)
		).

	raw_score_for_zscores(ZScores, KnownCount, Options, RawScore) :-
		memberchk(score_mode(ScoreMode), Options),
		(	ScoreMode == root_mean_square ->
			euclidean_norm(ZScores, Norm),
			RawScore is float(Norm / sqrt(KnownCount))
		;	ScoreMode == any_feature_extreme,
			maximum_absolute_zscore(ZScores, RawScore)
		).

	maximum_absolute_zscore([ZScore| ZScores], Maximum) :-
		Maximum0 is abs(ZScore),
		maximum_absolute_zscore(ZScores, Maximum0, Maximum).

	maximum_absolute_zscore([], Maximum, Maximum).
	maximum_absolute_zscore([ZScore| ZScores], Maximum0, Maximum) :-
		Absolute is abs(ZScore),
		(	Absolute > Maximum0 ->
			Maximum1 = Absolute
		;	Maximum1 = Maximum0
		),
		maximum_absolute_zscore(ZScores, Maximum1, Maximum).

	known_zscores([], [], [], KnownCount, KnownCount).
	known_zscores([zscore(_Attribute, Mean, Scale)| Encoders], [Value| Values], ZScores, KnownCount0, KnownCount) :-
		(	var(Value) ->
			ZScores = Tail,
			KnownCount1 = KnownCount0
		;	number(Value) ->
			ZScore is float((Value - Mean) / Scale),
			ZScores = [ZScore| Tail],
			KnownCount1 is KnownCount0 + 1
		;	type_error(number, Value)
		),
		known_zscores(Encoders, Values, Tail, KnownCount1, KnownCount).

	normalize_raw_score(RawScore, Score) :-
		Score is float(RawScore / (1.0 + RawScore)).

	encoder_attribute_names([], []).
	encoder_attribute_names([zscore(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	valid_zscore_encoders(Encoders) :-
		valid(list(compound), Encoders),
		Encoders \== [],
		valid_zscore_encoders_(Encoders, []).

	valid_zscore_encoders_([], _SeenAttributes).
	valid_zscore_encoders_([zscore(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid(number, Mean),
		valid(positive_number, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_zscore_encoders_(Encoders, [Attribute| SeenAttributes]).

	valid_detector_diagnostics(Dataset, Encoders, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(z_score), Diagnostics),
		memberchk(training_dataset(Dataset), Diagnostics),
		encoder_attribute_names(Encoders, AttributeNames),
		memberchk(attribute_names(AttributeNames), Diagnostics),
		length(AttributeNames, FeatureCount),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(example_count(ExampleCount), Diagnostics),
		valid(positive_integer, ExampleCount),
		memberchk(options(Options), Diagnostics),
		valid_detector_options(Options).

	valid_detector_options(Options) :-
		valid(list(compound), Options),
		catch(^^check_options(Options), _Error, fail).

	default_option(anomaly_threshold(0.70)).
	default_option(score_mode(root_mean_square)).

	valid_option(anomaly_threshold(Threshold)) :-
		number(Threshold),
		Threshold >= 0.0,
		Threshold =< 1.0.
	valid_option(score_mode(ScoreMode)) :-
		once((ScoreMode == root_mean_square; ScoreMode == any_feature_extreme)).

:- end_object.
