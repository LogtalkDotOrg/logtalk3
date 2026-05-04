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


:- object(ewma,
	imports(anomaly_detector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'EWMA (Exponentially Weighted Moving Average) anomaly detector for continuous sequence-like datasets. Learns per-step population mean and standard deviation from baseline training examples selected from a dataset object implementing the ``anomaly_dataset_protocol`` protocol and returns a detector term that can be used for scoring, prediction, and export.',
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, cusum, isolation_forest, knn_distance, lof, modified_z_score, z_score]
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
		Detector = ewma_detector(Dataset, AttributeSchema, Encoders, Diagnostics).

	check_anomaly_detector(Detector) :-
		(	var(Detector) ->
			instantiation_error
		;	Detector = ewma_detector(TrainingDataset, AttributeSchema, Encoders, Diagnostics),
			valid(object_identifier, TrainingDataset),
			valid_attribute_schema(AttributeSchema),
			valid_ewma_encoders(Encoders),
			schema_attribute_names(AttributeSchema, AttributeNames),
			encoder_attribute_names(Encoders, AttributeNames),
			valid_detector_diagnostics(TrainingDataset, Encoders, Diagnostics) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(ewma_detector(_Dataset, _AttributeSchema, _Encoders, Diagnostics), Diagnostics).

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
		format('EWMA Anomaly Detector~n', []),
		format('=====================~n~n', []),
		format('Training examples:  ~w~n', [ExampleCount]),
		format('Steps:              ~w~n', [AttributeNames]),
		format('Encoders:           ~w~n', [Encoders]),
		^^print_anomaly_detector_template(Detector),
		format('Options:            ~w~n', [Options]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(ewma_detector(_TrainingDataset, _AttributeSchema, _Encoders, _Diagnostics), ewma_detector('TrainingDataset', 'AttributeSchema', 'Encoders', 'Diagnostics')).

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
		^^merge_options(UserOptions, Options).

	build_diagnostics(Dataset, AttributeNames, ExampleCount, Options, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		Diagnostics = [
			model(ewma),
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

	build_encoders(AttributeNames, Examples, Encoders) :-
		build_encoders(AttributeNames, Examples, Encoders, []).

	build_encoders([], _Examples, [], _SeenAttributes).
	build_encoders([Attribute| Attributes], Examples, [ewma_encoder(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		known_attribute_values(Examples, Attribute, Values),
		(	Values == [] ->
			domain_error(known_attribute_values, Attribute)
		;	population::arithmetic_mean(Values, Mean),
			population::standard_deviation(Values, Scale0),
			normalize_scale(Scale0, Scale),
			\+ member(Attribute, SeenAttributes)
		),
		build_encoders(Attributes, Examples, Encoders, [Attribute| SeenAttributes]).

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

	control_limit_factor(Index, SmoothingFactor, LimitFactor) :-
		Retention is float(1.0 - SmoothingFactor),
		Decay is float(Retention ** (2 * Index)),
		VarianceFactor is float((SmoothingFactor / (2.0 - SmoothingFactor)) * (1.0 - Decay)),
		LimitFactor is float(sqrt(VarianceFactor)).

	normalize_scale(Scale0, Scale) :-
		(	Scale0 =< 0.0 ->
			Scale = 1.0
		;	Scale = Scale0
		).

	raw_score_for_attribute_values(AttributeSchema, Encoders, Options, AttributeValues, RawScore) :-
		schema_attribute_names(AttributeSchema, AttributeNames),
		checked_attribute_values(AttributeSchema, AttributeValues, OrderedValues),
		memberchk(control_limit_multiplier(ControlLimitMultiplier), Options),
		memberchk(smoothing_factor(SmoothingFactor), Options),
		ewma_excursion(Encoders, OrderedValues, SmoothingFactor, ControlLimitMultiplier, 0.0, 0.0, 0, RawScore, KnownCount),
		(	KnownCount == 0 ->
			domain_error(non_empty_known_values, AttributeNames)
		;	true
		).

	ewma_excursion(Encoders, OrderedValues, SmoothingFactor, ControlLimitMultiplier, RawState0, Maximum0, KnownCount0, Maximum, KnownCount) :-
		Retention is float(1.0 - SmoothingFactor),
		ewma_excursion_(Encoders, OrderedValues, SmoothingFactor, Retention, ControlLimitMultiplier, RawState0, Maximum0, KnownCount0, Maximum, KnownCount).

	ewma_excursion_([], [], _SmoothingFactor, _Retention, _ControlLimitMultiplier, _RawState0, Maximum, KnownCount, Maximum, KnownCount).
	ewma_excursion_([ewma_encoder(_Attribute, Mean, Scale)| Encoders], [Value| Values], SmoothingFactor, Retention, ControlLimitMultiplier, RawState0, Maximum0, KnownCount0, Maximum, KnownCount) :-
		(	var(Value) ->
			RawState1 = RawState0,
			Maximum1 = Maximum0,
			KnownCount1 = KnownCount0
		;	number(Value) ->
			StandardizedDeviation is float((Value - Mean) / Scale),
			RawState1 is float(SmoothingFactor * StandardizedDeviation + Retention * RawState0),
			KnownCount1 is KnownCount0 + 1,
			control_limit_factor(KnownCount1, SmoothingFactor, LimitFactor),
			ControlLimit is float(ControlLimitMultiplier * LimitFactor),
			NormalizedExcursion is float(abs(RawState1) / ControlLimit),
			(	NormalizedExcursion > Maximum0 ->
				Maximum1 = NormalizedExcursion
			;	Maximum1 = Maximum0
			)
		;	type_error(number, Value)
		),
		ewma_excursion_(Encoders, Values, SmoothingFactor, Retention, ControlLimitMultiplier, RawState1, Maximum1, KnownCount1, Maximum, KnownCount).

	normalize_raw_score(RawScore, Score) :-
		Score is float(RawScore / (1.0 + RawScore)).

	encoder_attribute_names([], []).
	encoder_attribute_names([ewma_encoder(Attribute, _Mean, _Scale)| Encoders], [Attribute| AttributeNames]) :-
		encoder_attribute_names(Encoders, AttributeNames).

	valid_attribute_schema(AttributeSchema) :-
		AttributeSchema = attribute_schema(AttributeNames, SortedIndexedAttributeNames),
		valid(list(atom), AttributeNames),
		AttributeNames \== [],
		build_attribute_schema(AttributeNames, attribute_schema(AttributeNames, SortedIndexedAttributeNames)).

	valid_ewma_encoders(Encoders) :-
		valid(list(compound), Encoders),
		Encoders \== [],
		valid_ewma_encoders_(Encoders, []).

	valid_ewma_encoders_([], _SeenAttributes).
	valid_ewma_encoders_([ewma_encoder(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid(number, Mean),
		valid(positive_number, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_ewma_encoders_(Encoders, [Attribute| SeenAttributes]).

	valid_detector_diagnostics(Dataset, Encoders, Diagnostics) :-
		valid(list(compound), Diagnostics),
		memberchk(model(ewma), Diagnostics),
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
		catch(^^check_options(Options), _Error, fail),
		unique_option_names(Options, []),
		catch(^^merge_options(Options, EffectiveOptions), _Error, fail),
		same_option_set(Options, EffectiveOptions),
		same_option_set(EffectiveOptions, Options).

	unique_option_names([], _SeenOptions).
	unique_option_names([Option| Options], SeenOptions) :-
		functor(Option, Name, Arity),
		\+ member(Name/Arity, SeenOptions),
		unique_option_names(Options, [Name/Arity| SeenOptions]).

	same_option_set([], _Options).
	same_option_set([Option| Options], OtherOptions) :-
		member(Option, OtherOptions),
		same_option_set(Options, OtherOptions).

	default_option(anomaly_threshold(0.5)).
	default_option(baseline_class_values([normal])).
	default_option(baseline_selection_policy(reject)).
	default_option(control_limit_multiplier(3.0)).
	default_option(smoothing_factor(0.2)).

	valid_option(anomaly_threshold(Threshold)) :-
		number(Threshold),
		Threshold >= 0.0,
		Threshold =< 1.0.
	valid_option(baseline_class_values(BaselineClassValues)) :-
		^^valid_baseline_class_values(BaselineClassValues).
	valid_option(baseline_selection_policy(Policy)) :-
		once((Policy == reject; Policy == filter)).
	valid_option(control_limit_multiplier(ControlLimitMultiplier)) :-
		number(ControlLimitMultiplier),
		ControlLimitMultiplier > 0.0.
	valid_option(smoothing_factor(SmoothingFactor)) :-
		number(SmoothingFactor),
		SmoothingFactor > 0.0,
		SmoothingFactor =< 1.0.

:- end_object.
