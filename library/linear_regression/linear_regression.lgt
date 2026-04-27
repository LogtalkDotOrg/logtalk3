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


:- object(linear_regression,
	imports([options, regressor_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Linear regression regressor supporting continuous and mixed-feature datasets using batch gradient descent. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses batch gradient descent to minimize mean squared error. Supports optional L2 regularization and optional feature scaling for continuous attributes.',
			'Feature handling' - 'Continuous features may be standardized using z-score scaling. Categorical features are one-hot encoded from the declared dataset attribute values.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features.',
			'Unknown values' - 'Prediction requests containing categorical values that are not declared by the dataset raise a domain error.',
			'Regressor representation' - 'The learned regressor is represented by default as ``linear_regressor(Encoders, Bias, Weights, Options)`` where ``Encoders`` stores the feature encoding metadata, ``Bias`` stores the intercept, and ``Weights`` stores one coefficient per encoded feature.'
		],
		see_also is [knn_regression, regression_tree, random_forest_regression, gradient_boosting_regression]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a regressor from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Regressor', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(_Target),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_encoders(Attributes, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		encoders_feature_count(Encoders, NumFeatures),
		train_model(Rows, NumFeatures, Options, Bias, Weights),
		Regressor = linear_regressor(Encoders, Bias, Weights, Options).

	predict(Regressor, Instance, Target) :-
		Regressor =.. [_, Encoders, Bias, Weights, _Options],
		encode_instance(Encoders, Instance, Features),
		dot_product(Weights, Features, Linear),
		Target is Bias + Linear.

	build_encoders([], _, _, []).
	build_encoders([Attribute-Values| Rest], Examples, Options, [Encoder| Encoders]) :-
		(   Values == continuous ->
			continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;   Encoder = categorical(Attribute, Values)
		),
		build_encoders(Rest, Examples, Options, Encoders).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == true ->
			known_attribute_values(Examples, Attribute, Values),
			(   Values == [] ->
				Mean = 0.0,
				Scale = 1.0
			;   arithmetic_mean(Values, Mean),
				length(Values, Count),
				(   Count > 1 ->
					variance(Values, Variance)
				;   Variance = 0.0
				),
				(   Variance > 0.0 ->
					Scale is sqrt(Variance)
				;   Scale = 1.0
				)
			)
		;   Mean = 0.0,
			Scale = 1.0
		).

	known_attribute_values([], _, []).
	known_attribute_values([example(_Id, _Target, AttributeValues)| Examples], Attribute, Values) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;   Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	examples_to_rows([], _, []).
	examples_to_rows([example(_Id, Target, AttributeValues)| Examples], Encoders, [Features-Target| Rows]) :-
		encode_instance(Encoders, AttributeValues, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature, Missing| Features]) :-
		!,
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			normalize_continuous(Value, Mean, Scale, Feature),
			Missing = 0.0
		;   Feature = 0.0,
			Missing = 1.0
		),
		encode_instance(Encoders, AttributeValues, Features).
	encode_instance([categorical(Attribute, Values)| Encoders], AttributeValues, Features) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			check_categorical_value(Attribute, Values, Value),
			one_hot_encode(Values, Value, Encoded)
		;   missing_one_hot_encode(Values, Encoded)
		),
		encode_instance(Encoders, AttributeValues, RestFeatures),
		append(Encoded, RestFeatures, Features).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	one_hot_encode(Values, Value, Encoded) :-
		one_hot_encode_(Values, Value, Encoded0),
		append(Encoded0, [0.0], Encoded).

	one_hot_encode_([], _, []).
	one_hot_encode_([Category| Categories], Value, [Feature| Features]) :-
		(   Value == Category ->
			Feature = 1.0
		;   Feature = 0.0
		),
		one_hot_encode_(Categories, Value, Features).

	missing_one_hot_encode(Values, Encoded) :-
		zero_vector_from_values(Values, Zeroes),
		append(Zeroes, [1.0], Encoded).

	check_categorical_value(Attribute, Values, Value) :-
		(   memberchk(Value, Values) ->
			true
		;   domain_error(attribute_value(Attribute, Values), Value)
		).

	zero_vector_from_values([], []).
	zero_vector_from_values([_| Values], [0.0| Zeroes]) :-
		zero_vector_from_values(Values, Zeroes).

	encoders_feature_count([], 0).
	encoders_feature_count([continuous(_, _, _)| Encoders], Count) :-
		!,
		encoders_feature_count(Encoders, RestCount),
		Count is RestCount + 2.
	encoders_feature_count([categorical(_, Values)| Encoders], Count) :-
		length(Values, ValueCount),
		encoders_feature_count(Encoders, RestCount),
		Count is RestCount + ValueCount + 1.

	train_model(Rows, NumFeatures, Options, Bias, Weights) :-
		zero_vector(NumFeatures, InitialWeights),
		optimize_model(Rows, Options, 0, 0.0, InitialWeights, Bias, Weights).

	optimize_model(Rows, Options, Iteration, Bias0, Weights0, Bias, Weights) :-
		^^option(maximum_iterations(MaxIterations), Options),
		(   Iteration >= MaxIterations ->
			Bias = Bias0,
			Weights = Weights0
		;   update_parameters(Rows, Bias0, Weights0, Options, Bias1, Weights1, MaxDelta),
			^^option(tolerance(Tolerance), Options),
			(   MaxDelta =< Tolerance ->
				Bias = Bias1,
				Weights = Weights1
			;   NextIteration is Iteration + 1,
				optimize_model(Rows, Options, NextIteration, Bias1, Weights1, Bias, Weights)
			)
		).

	update_parameters(Rows, Bias0, Weights0, Options, Bias1, Weights1, MaxDelta) :-
		length(Rows, Count),
		zero_vector_like(Weights0, InitialGradientWeights),
		accumulate_gradients(Rows, Bias0, Weights0, 0.0, InitialGradientWeights, GradientBias, GradientWeights),
		Scale is 1.0 / Count,
		^^option(learning_rate(LearningRate), Options),
		^^option(l2_regularization(Regularization), Options),
		MeanBiasGradient is GradientBias * Scale,
		Bias1 is Bias0 - LearningRate * MeanBiasGradient,
		BiasDelta is abs(Bias1 - Bias0),
		update_weights(Weights0, GradientWeights, Scale, Regularization, LearningRate, Weights1, 0.0, MaxWeightDelta),
		MaxDelta is max(BiasDelta, MaxWeightDelta).

	accumulate_gradients([], _, _, GradientBias, GradientWeights, GradientBias, GradientWeights).
	accumulate_gradients([Features-Target| Rows], Bias, Weights, GradientBias0, GradientWeights0, GradientBias, GradientWeights) :-
		dot_product(Weights, Features, Linear),
		Prediction is Bias + Linear,
		Error is Prediction - Target,
		GradientBias1 is GradientBias0 + Error,
		add_scaled_vector(Features, Error, GradientWeights0, GradientWeights1),
		accumulate_gradients(Rows, Bias, Weights, GradientBias1, GradientWeights1, GradientBias, GradientWeights).

	add_scaled_vector([], _, [], []).
	add_scaled_vector([Feature| Features], Scale, [Gradient| Gradients], [Updated| UpdatedGradients]) :-
		Updated is Gradient + Feature * Scale,
		add_scaled_vector(Features, Scale, Gradients, UpdatedGradients).

	update_weights([], [], _, _, _, [], MaxDelta, MaxDelta).
	update_weights([Weight0| Weights0], [Gradient| Gradients], Scale, Regularization, LearningRate, [Weight1| Weights1], MaxDelta0, MaxDelta) :-
		MeanGradient is Gradient * Scale + Regularization * Weight0,
		Weight1 is Weight0 - LearningRate * MeanGradient,
		Delta is abs(Weight1 - Weight0),
		MaxDelta1 is max(Delta, MaxDelta0),
		update_weights(Weights0, Gradients, Scale, Regularization, LearningRate, Weights1, MaxDelta1, MaxDelta).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Zeroes]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_vector(NextCount, Zeroes).

	zero_vector_like([], []).
	zero_vector_like([_| Weights], [0.0| Zeroes]) :-
		zero_vector_like(Weights, Zeroes).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Bias', 'Weights', 'Options'].

	regressor_term_template(linear_regressor(_Encoders, _Bias, _Weights, _Options), linear_regressor('Encoders', 'Bias', 'Weights', 'Options')).

	check_regressor(Regressor) :-
		(   Regressor = linear_regressor(Encoders, Bias, Weights, Options),
			^^valid_regression_encoders(Encoders),
			valid(float, Bias),
			encoders_feature_count(Encoders, FeatureCount),
			valid(list(float, FeatureCount), Weights),
			^^valid_regressor_options(Options) ->
			true
		;   domain_error(valid_regressor, Regressor)
		).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = linear_regressor(Encoders, Bias, Weights, Options),
		Clause =.. [Functor, Encoders, Bias, Weights, Options].

	print_regressor(Regressor) :-
		Regressor = linear_regressor(Encoders, Bias, Weights, Options),
		format('Linear Regression Regressor~n', []),
		format('===========================~n~n', []),
		^^print_regressor_template(Regressor),
		print_options(Options),
		format('Bias: ~4f~n', [Bias]),
		format('Weights: ~w coefficients~n~n', [Weights]),
		format('Encoders:~n', []),
		print_encoders(Encoders).

	print_options(Options) :-
		format('Options: ~w~n', [Options]).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		!,
		format('  ~w (continuous, mean=~4f, scale=~4f, missing-indicator=yes)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).
	print_encoders([categorical(Attribute, Values)| Encoders]) :-
		format('  ~w (categorical, values=~w, missing-indicator=yes)~n', [Attribute, Values]),
		print_encoders(Encoders).

	default_option(learning_rate(0.05)).
	default_option(maximum_iterations(2000)).
	default_option(tolerance(1.0e-7)).
	default_option(l2_regularization(0.0)).
	default_option(feature_scaling(true)).

	valid_option(learning_rate(Rate)) :-
		valid(positive_float, Rate).
	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		valid(non_negative_float, Tolerance).
	valid_option(l2_regularization(Regularization)) :-
		valid(non_negative_float, Regularization).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.
