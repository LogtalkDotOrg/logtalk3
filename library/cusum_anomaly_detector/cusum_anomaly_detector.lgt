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


:- object(cusum_anomaly_detector,
	imports(anomaly_detector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'CUSUM (Cumulative Sum Control Chart) anomaly detector for continuous sequence-like datasets. Learns per-step population mean and standard deviation from baseline training examples, or from the baseline subset of a labeled dataset selected by learn-time class-label options, using a dataset object implementing the ``anomaly_dataset_protocol`` protocol. Returns a detector term that can be used for scoring, prediction, and export.',
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, isolation_forest_anomaly_detector, knn_distance_anomaly_detector, lof_anomaly_detector, modified_z_score_anomaly_detector, z_score_anomaly_detector]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, msort/2, reverse/2
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
		prepare_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		check_attribute_names_non_empty(AttributeNames, Dataset),
		build_attribute_schema(AttributeNames, AttributeSchema),
		^^baseline_training_examples(Dataset, Examples, Options),
		check_example_values(Examples, AttributeSchema),
		build_encoders(AttributeNames, Examples, Encoders),
		length(Examples, ExampleCount),
		build_diagnostics(Dataset, AttributeNames, ExampleCount, Options, Diagnostics),
		Detector = cusum_detector(Dataset, AttributeSchema, Encoders, Diagnostics).

	check_anomaly_detector(Detector) :-
		(	var(Detector) ->
			instantiation_error
		;	Detector = cusum_detector(TrainingDataset, AttributeSchema, Encoders, Diagnostics),
			valid(object_identifier, TrainingDataset),
			valid_attribute_schema(AttributeSchema),
			valid_cusum_encoders(Encoders),
			schema_attribute_names(AttributeSchema, AttributeNames),
			encoder_attribute_names(Encoders, AttributeNames),
			valid_detector_diagnostics(TrainingDataset, Encoders, Diagnostics) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(cusum_detector(_Dataset, _AttributeSchema, _Encoders, Diagnostics), Diagnostics).

	score(Detector, Instance, Score) :-
		detector_data(Detector, _TrainingDataset, AttributeSchema, Encoders, Diagnostics),
		detector_options(Diagnostics, Options),
		raw_score_for_attribute_values(AttributeSchema, Encoders, Options, Instance, RawScore),
		normalize_raw_score(RawScore, Score).

	score_all(Dataset, Detector, Scores) :-
		detector_data(Detector, _TrainingDataset, AttributeSchema, Encoders, Diagnostics),
		detector_options(Diagnostics, Options),
		findall(
			Score-Id-Class,
			(
				Dataset::example(Id, Class, AttributeValues),
				raw_score_for_attribute_values(AttributeSchema, Encoders, Options, AttributeValues, RawScore),
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
		detector_data(Detector, _TrainingDataset, AttributeSchema, Encoders, Diagnostics),
		schema_attribute_names(AttributeSchema, AttributeNames),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		format('CUSUM Anomaly Detector~n', []),
		format('======================~n~n', []),
		format('Training examples:  ~w~n', [ExampleCount]),
		format('Steps:              ~w~n', [AttributeNames]),
		format('Encoders:           ~w~n', [Encoders]),
		^^print_anomaly_detector_template(Detector),
		format('Options:            ~w~n', [Options]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(cusum_detector(_TrainingDataset, _AttributeSchema, _Encoders, _Diagnostics), cusum_detector('TrainingDataset', 'AttributeSchema', 'Encoders', 'Diagnostics')).

	detector_data(Detector, TrainingDataset, AttributeSchema, Encoders, Diagnostics) :-
		Detector =.. [_Functor, TrainingDataset, AttributeSchema, Encoders, Diagnostics].

	detector_options(Diagnostics, Options) :-
		memberchk(options(Options), Diagnostics).

	build_attribute_schema(AttributeNames, attribute_schema(AttributeNames, SortedIndexedAttributeNames)) :-
		index_attribute_names(AttributeNames, IndexedAttributeNames),
		keysort(IndexedAttributeNames, SortedIndexedAttributeNames).

	schema_attribute_names(attribute_schema(AttributeNames, _SortedIndexedAttributeNames), AttributeNames).

	prepare_options(UserOptions, Options) :-
		^^check_options(UserOptions),
		(	member(anomaly_threshold(_), UserOptions) ->
			AugmentedUserOptions = UserOptions
		;	member(decision_interval(DecisionInterval), UserOptions) ->
			decision_interval_threshold(DecisionInterval, Threshold),
			AugmentedUserOptions = [anomaly_threshold(Threshold)| UserOptions]
		;	AugmentedUserOptions = UserOptions
		),
		^^merge_options(AugmentedUserOptions, Options).

	decision_interval_threshold(DecisionInterval, Threshold) :-
		Threshold is float(DecisionInterval / (1.0 + DecisionInterval)).

	build_diagnostics(Dataset, AttributeNames, ExampleCount, Options, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		Diagnostics = [
			model(cusum_anomaly_detector),
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

	check_example_values([], _AttributeSchema).
	check_example_values([_Id-_Class-AttributeValues| Examples], AttributeSchema) :-
		check_example_attributes(AttributeSchema, AttributeValues),
		check_example_values(Examples, AttributeSchema).

	check_example_attributes(AttributeSchema, AttributeValues) :-
		checked_attribute_values(AttributeSchema, AttributeValues, _OrderedValues).

	checked_attribute_values(attribute_schema(_AttributeNames, SortedIndexedAttributeNames), AttributeValues, OrderedValues) :-
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
	build_encoders([Attribute| Attributes], Examples, [cusum_encoder(Attribute, Mean, Scale)| Encoders]) :-
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

	raw_score_for_attribute_values(AttributeSchema, Encoders, Options, AttributeValues, RawScore) :-
		checked_attribute_values(AttributeSchema, AttributeValues, OrderedValues),
		memberchk(allowance(Allowance), Options),
		cusum_excursion(Encoders, OrderedValues, Allowance, RawScore, KnownCount),
		(	KnownCount == 0 ->
			schema_attribute_names(AttributeSchema, AttributeNames),
			domain_error(non_empty_known_values, AttributeNames)
		;	true
		).

	cusum_excursion(Encoders, OrderedValues, Allowance, RawScore, KnownCount) :-
		cusum_excursion(Encoders, OrderedValues, Allowance, 0.0, 0.0, 0.0, 0, RawScore, KnownCount).

	cusum_excursion([], [], _Allowance, _Positive0, _Negative0, Maximum, KnownCount, Maximum, KnownCount).
	cusum_excursion([cusum_encoder(_Attribute, Mean, Scale)| Encoders], [Value| Values], Allowance, Positive0, Negative0, Maximum0, KnownCount0, Maximum, KnownCount) :-
		(	var(Value) ->
			Positive1 = Positive0,
			Negative1 = Negative0,
			Maximum1 = Maximum0,
			KnownCount1 = KnownCount0
		;	number(Value) ->
			StandardizedDeviation is float((Value - Mean) / Scale),
			PositiveCandidate is Positive0 + StandardizedDeviation - Allowance,
			(	PositiveCandidate > 0.0 ->
				Positive1 = PositiveCandidate
			;	Positive1 = 0.0
			),
			NegativeCandidate is Negative0 - StandardizedDeviation - Allowance,
			(	NegativeCandidate > 0.0 ->
				Negative1 = NegativeCandidate
			;	Negative1 = 0.0
			),
			maximum_excursion(Maximum0, Positive1, Negative1, Maximum1),
			KnownCount1 is KnownCount0 + 1
		;	type_error(number, Value)
		),
		cusum_excursion(Encoders, Values, Allowance, Positive1, Negative1, Maximum1, KnownCount1, Maximum, KnownCount).

	maximum_excursion(Maximum0, Positive, Negative, Maximum) :-
		(	Positive > Maximum0 ->
			Maximum1 = Positive
		;	Maximum1 = Maximum0
		),
		(	Negative > Maximum1 ->
			Maximum = Negative
		;	Maximum = Maximum1
		).

	normalize_raw_score(RawScore, Score) :-
		Score is float(RawScore / (1.0 + RawScore)).

	encoder_attribute_names([], []).
	encoder_attribute_names([cusum_encoder(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	valid_attribute_schema(AttributeSchema) :-
		AttributeSchema = attribute_schema(AttributeNames, SortedIndexedAttributeNames),
		valid(list(atom), AttributeNames),
		AttributeNames \== [],
		build_attribute_schema(AttributeNames, attribute_schema(AttributeNames, SortedIndexedAttributeNames)).

	valid_cusum_encoders(Encoders) :-
		valid(list(compound), Encoders),
		Encoders \== [],
		valid_cusum_encoders_(Encoders, []).

	valid_cusum_encoders_([], _SeenAttributes).
	valid_cusum_encoders_([cusum_encoder(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid(number, Mean),
		valid(positive_number, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_cusum_encoders_(Encoders, [Attribute| SeenAttributes]).

	valid_detector_diagnostics(Dataset, Encoders, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(cusum_anomaly_detector), Diagnostics),
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

	default_option(anomaly_threshold(0.8333333333333334)).
	default_option(allowance(0.5)).
	default_option(baseline_class_values([normal])).
	default_option(baseline_selection_policy(reject)).
	default_option(decision_interval(5.0)).

	valid_option(anomaly_threshold(Threshold)) :-
		number(Threshold),
		Threshold >= 0.0,
		Threshold =< 1.0.
	valid_option(allowance(Allowance)) :-
		number(Allowance),
		Allowance >= 0.0.
	valid_option(baseline_class_values(BaselineClassValues)) :-
		^^valid_baseline_class_values(BaselineClassValues).
	valid_option(baseline_selection_policy(Policy)) :-
		once((Policy == reject; Policy == filter)).
	valid_option(decision_interval(DecisionInterval)) :-
		number(DecisionInterval),
		DecisionInterval > 0.0.

:- end_object.
