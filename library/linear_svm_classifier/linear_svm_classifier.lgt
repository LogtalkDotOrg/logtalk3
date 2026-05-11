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


:- object(linear_svm_classifier,
	imports(classifier_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Linear support vector machine classifier supporting binary and multiclass classification using one-vs-rest margin models. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		see_also is [dataset_protocol, logistic_regression_classifier, c45_classifier, knn_classifier, naive_bayes_classifier, nearest_centroid_classifier, random_forest_classifier, adaptive_boosting_classifier]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(linear_algebra, [
		add_scaled_vector/4, new_vector/3, new_vector_like/2
	]).

	:- uses(numberlist, [
		scalar_product/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, _AttributeNames),
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples(Dataset, Examples),
		Dataset::class_values(Classes),
		build_encoders(Attributes, Examples, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		encoders_feature_count(Encoders, NumFeatures),
		train_models(Classes, Rows, NumFeatures, Options, Models),
		Classifier = linear_svm_classifier(Classes, Encoders, Models, Options).

	predict(Classifier, Instance, Class) :-
		Classifier =.. [_, _Classes, Encoders, Models, _Options],
		encode_instance(Encoders, Instance, Features),
		class_scores(Models, Features, ClassScores),
		max_score(ClassScores, Class, _).

	dataset_attributes(Dataset, Attributes) :-
		findall(
			Attribute-Values,
			Dataset::attribute_values(Attribute, Values),
			Attributes
		).

	build_encoders([], _, []).
	build_encoders([Attribute-Values| Rest], Examples, [Encoder| Encoders]) :-
		(   Values == continuous ->
			continuous_stats(Attribute, Examples, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;   Encoder = categorical(Attribute, Values)
		),
		build_encoders(Rest, Examples, Encoders).

	continuous_stats(Attribute, Examples, Mean, Scale) :-
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
		).

	known_attribute_values([], _, []).
	known_attribute_values([_-_-AttributeValues| Examples], Attribute, Values) :-
		(   memberchk(Attribute-Value, AttributeValues),
			nonvar(Value) ->
			Values = [Value| Rest]
		;   Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	examples_to_rows([], _, []).
	examples_to_rows([_-Class-AttributeValues| Examples], Encoders, [Features-Class| Rows]) :-
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
		append(Encoded, RestFeatures, Features),
		encode_instance(Encoders, AttributeValues, RestFeatures).

	normalize_continuous(Value, Mean, Scale, Feature) :-
		(   number(Value) ->
			true
		;   type_error(number, Value)
		),
		Feature is (Value - Mean) / Scale.

	one_hot_encode([], _, [0.0]).
	one_hot_encode([Category| Categories], Value, [Feature| Features]) :-
		(   Value == Category ->
			Feature = 1.0
		;   Feature = 0.0
		),
		one_hot_encode(Categories, Value, Features).

	missing_one_hot_encode([], [1.0]).
	missing_one_hot_encode([_| Values], [0.0| Zeroes]) :-
		missing_one_hot_encode(Values, Zeroes).

	check_categorical_value(Attribute, Values, Value) :-
		(   member(Value, Values) ->
			true
		;   domain_error(attribute_value(Attribute, Values), Value)
		).

	encoders_feature_count([], 0).
	encoders_feature_count([continuous(_, _, _)| Encoders], Count) :-
		!,
		encoders_feature_count(Encoders, RestCount),
		Count is RestCount + 2.
	encoders_feature_count([categorical(_, Values)| Encoders], Count) :-
		length(Values, ValueCount),
		encoders_feature_count(Encoders, RestCount),
		Count is RestCount + ValueCount + 1.

	train_models([], _, _, _, []).
	train_models([Class| Classes], Rows, NumFeatures, Options, [Model| Models]) :-
		initialize_model(Class, NumFeatures, InitialModel),
		optimize_model(Rows, Class, Options, 0, InitialModel, Model),
		train_models(Classes, Rows, NumFeatures, Options, Models).

	initialize_model(Class, NumFeatures, class_model(Class, 0.0, Weights)) :-
		new_vector(NumFeatures, 0.0, Weights).

	optimize_model(Rows, PositiveClass, Options, Iteration, Model0, Model) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Iteration >= MaximumIterations ->
			Model = Model0
		;   ^^option(learning_rate(LearningRate), Options),
			Step is LearningRate / sqrt(Iteration + 1.0),
			update_model(Rows, PositiveClass, Model0, Step, Options, Model1, MaxDelta),
			^^option(tolerance(Tolerance), Options),
			(   MaxDelta =< Tolerance ->
				Model = Model1
			;   NextIteration is Iteration + 1,
				optimize_model(Rows, PositiveClass, Options, NextIteration, Model1, Model)
			)
		).

	update_model(Rows, PositiveClass, class_model(Class, Bias0, Weights0), Step, Options, class_model(Class, Bias1, Weights1), MaxDelta) :-
		length(Rows, Count),
		new_vector_like(Weights0, ZeroGradients),
		accumulate_binary_gradients(Rows, PositiveClass, Bias0, Weights0, 0.0, BiasGradient, ZeroGradients, WeightGradients),
		Scale is 1.0 / Count,
		MeanBiasGradient is BiasGradient * Scale,
		Bias1 is Bias0 - Step * MeanBiasGradient,
		BiasDelta is abs(Bias1 - Bias0),
		^^option(l2_regularization(Regularization), Options),
		update_weights(Weights0, WeightGradients, Scale, Regularization, Step, Weights1, 0.0, MaxWeightDelta),
		MaxDelta is max(BiasDelta, MaxWeightDelta).

	accumulate_binary_gradients([], _PositiveClass, _Bias, _Weights, BiasGradient, BiasGradient, WeightGradients, WeightGradients).
	accumulate_binary_gradients([Features-Label| Rows], PositiveClass, Bias, Weights, BiasGradient0, BiasGradient, WeightGradients0, WeightGradients) :-
		target_sign(Label, PositiveClass, Sign),
		scalar_product(Weights, Features, Linear),
		Score is Bias + Linear,
		Margin is Sign * Score,
		(   Margin >= 1.0 ->
			BiasGradient1 = BiasGradient0,
			WeightGradients1 = WeightGradients0
		;   BiasGradient1 is BiasGradient0 - Sign,
			NegativeSign is -Sign,
			add_scaled_vector(Features, NegativeSign, WeightGradients0, WeightGradients1)
		),
		accumulate_binary_gradients(Rows, PositiveClass, Bias, Weights, BiasGradient1, BiasGradient, WeightGradients1, WeightGradients).

	target_sign(Label, PositiveClass, 1.0) :-
		Label == PositiveClass,
		!.
	target_sign(_, _, -1.0).

	update_weights([], [], _, _, _, [], MaxDelta, MaxDelta).
	update_weights([Weight0| Weights0], [Gradient| Gradients], Scale, Regularization, Step, [Weight1| Weights1], MaxDelta0, MaxDelta) :-
		MeanGradient is Gradient * Scale + Regularization * Weight0,
		Weight1 is Weight0 - Step * MeanGradient,
		Delta is abs(Weight1 - Weight0),
		MaxDelta1 is max(Delta, MaxDelta0),
		update_weights(Weights0, Gradients, Scale, Regularization, Step, Weights1, MaxDelta1, MaxDelta).

	class_scores([], _, []).
	class_scores([class_model(Class, Bias, Weights)| Models], Features, [Class-Score| Scores]) :-
		scalar_product(Weights, Features, Linear),
		Score is Bias + Linear,
		class_scores(Models, Features, Scores).

	max_score([Class-Score], Class, Score) :-
		!.
	max_score([Class1-Score1, Class2-Score2| Scores], Class, Score) :-
		(   Score1 >= Score2 ->
			max_score([Class1-Score1| Scores], Class, Score)
		;   max_score([Class2-Score2| Scores], Class, Score)
		).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		classifier_data(Classifier, Classes, Encoders, Models, Options),
		encoders_feature_count(Encoders, EncodedFeatures),
		length(Models, ModelCount),
		Diagnostics = [
			model(linear_svm_classifier),
			classes(Classes),
			encoded_features(EncodedFeatures),
			models(ModelCount),
			options(Options)
		].

	check_classifier(Classifier) :-
		(   classifier_data(Classifier, Classes, Encoders, Models, Options),
			^^valid_class_values(Classes),
			^^valid_linear_encoders(Encoders),
			catch(::check_options(Options), _Error, fail),
			encoders_feature_count(Encoders, EncodedFeatures),
			length(Classes, ModelCount),
			length(Models, ModelCount),
			valid_models(Models, Classes, EncodedFeatures, []) ->
			true
		;   domain_error(classifier, Classifier)
		).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(linear_svm_classifier(_Classes, _Encoders, _Models, _Options), linear_svm_classifier('Classes', 'Encoders', 'Models', 'Options')).

	valid_models([], _Classes, _EncodedFeatures, _SeenClasses).
	valid_models([class_model(Class, Bias, Weights)| Models], Classes, EncodedFeatures, SeenClasses) :-
		memberchk(Class, Classes),
		\+ member(Class, SeenClasses),
		valid(float, Bias),
		valid(list(float, EncodedFeatures), Weights),
		valid_models(Models, Classes, EncodedFeatures, [Class| SeenClasses]).

	classifier_data(Classifier, Classes, Encoders, Models, Options) :-
		Classifier =.. [_Functor, Classes, Encoders, Models, Options].

	print_classifier(Classifier) :-
		classifier_data(Classifier, Classes, Encoders, Models, Options),
		format('Linear SVM Classifier~n', []),
		format('=====================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n', [Classes]),
		format('Options: ~w~n~n', [Options]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nModels:~n', []),
		print_models(Models).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		!,
		format('  ~w (continuous, mean=~4f, scale=~4f, missing-indicator=yes)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).
	print_encoders([categorical(Attribute, Values)| Encoders]) :-
		format('  ~w (categorical, values=~w, missing-indicator=yes)~n', [Attribute, Values]),
		print_encoders(Encoders).

	print_models([]).
	print_models([class_model(Class, Bias, Weights)| Models]) :-
		length(Weights, NumWeights),
		format('  ~w: bias=~4f, weights=~w coefficients~n', [Class, Bias, NumWeights]),
		print_models(Models).

	default_option(learning_rate(0.1)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-6)).
	default_option(l2_regularization(0.01)).

	valid_option(learning_rate(Rate)) :-
		number(Rate),
		Rate > 0.0.
	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(l2_regularization(Regularization)) :-
		number(Regularization),
		Regularization >= 0.0.

:- end_object.
