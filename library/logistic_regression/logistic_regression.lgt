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


:- object(logistic_regression,
	imports(classifier_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Logistic regression classifier supporting binary and multiclass classification using joint softmax training. Learns from a dataset object implementing the ``dataset_protocol`` protocol and returns a classifier term that can be used for prediction and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses batch gradient descent to train a single multiclass softmax model. Binary classification is treated as a two-class special case of the same objective.',
			'Feature handling' - 'Continuous features are standardized using z-score scaling. Categorical features are one-hot encoded from the declared dataset attribute values.',
			'Missing values' - 'Missing feature values represented using anonymous variables are encoded using explicit missing-value indicator features instead of being conflated with baseline feature values.',
			'Unknown values' - 'Prediction requests containing categorical values that are not declared by the dataset raise a domain error instead of being silently mapped into an existing feature bucket.',
			'Classifier representation' - 'The learned classifier is represented by default as ``lr_classifier(Classes, Encoders, Models, Options)`` where ``Encoders`` stores the feature encoding metadata and ``Models`` stores one parameter vector per class in the joint softmax model.'
		],
		see_also is [dataset_protocol, c45, knn, naive_bayes, nearest_centroid, random_forest, ada_boost]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a classifier from the given dataset object using the specified options.',
		argnames is ['Dataset', 'Classifier', 'Options']
	]).

	:- public(predict_probabilities/3).
	:- mode(predict_probabilities(+compound, +list, -list), one).
	:- info(predict_probabilities/3, [
		comment is 'Predicts class probabilities for a new instance using the learned classifier. Returns a list of ``Class-Probability`` pairs.',
		argnames is ['Classifier', 'Instance', 'Probabilities']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		scalar_product/3 as dot_product/3
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

	learn(Dataset, Classifier) :-
		learn(Dataset, Classifier, []).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		dataset_attributes(Dataset, Attributes),
		keys(Attributes, _AttributeNames),
		findall(
			Id-Class-AttributeValues,
			Dataset::example(Id, Class, AttributeValues),
			Examples
		),
		Dataset::class_values(Classes),
		build_encoders(Attributes, Examples, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		encoders_feature_count(Encoders, 0, NumFeatures),
		train_models(Classes, Rows, NumFeatures, Options, Models),
		Classifier = lr_classifier(Classes, Encoders, Models, Options).

	predict(Classifier, Instance, Class) :-
		predict_probabilities(Classifier, Instance, Probabilities),
		max_probability(Probabilities, Class, _).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier =.. [_, _Classes, Encoders, Models, _Options],
		encode_instance(Encoders, Instance, Features),
		class_logits(Models, Features, ClassLogits),
		stable_softmax(ClassLogits, Probabilities).

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

	encoders_feature_count([], Count, Count).
	encoders_feature_count([continuous(_, _, _)| Encoders], Count0, Count) :-
		!,
		Count1 is Count0 + 2,
		encoders_feature_count(Encoders, Count1, Count).
	encoders_feature_count([categorical(_, Values)| Encoders], Count0, Count) :-
		length(Values, ValueCount),
		Count1 is Count0 + ValueCount + 1,
		encoders_feature_count(Encoders, Count1, Count).

	train_models([], _, _, _, []).
	train_models([Class| Classes], Rows, NumFeatures, Options, Models) :-
		initialize_models([Class| Classes], NumFeatures, InitialModels),
		optimize_models(Rows, Options, 0, InitialModels, Models).

	initialize_models([], _, []).
	initialize_models([Class| Classes], NumFeatures, [class_model(Class, 0.0, Weights)| Models]) :-
		zero_vector(NumFeatures, Weights),
		initialize_models(Classes, NumFeatures, Models).

	optimize_models(Rows, Options, Iteration, Models0, Models) :-
		^^option(maximum_iterations(MaxIterations), Options),
		(   Iteration >= MaxIterations ->
			Models = Models0
		;   update_models(Rows, Models0, Options, Models1, MaxDelta),
			^^option(tolerance(Tolerance), Options),
			(   MaxDelta =< Tolerance ->
				Models = Models1
			;   NextIteration is Iteration + 1,
				optimize_models(Rows, Options, NextIteration, Models1, Models)
			)
		).

	update_models(Rows, Models0, Options, Models1, MaxDelta) :-
		length(Rows, Count),
		zero_gradient_models(Models0, InitialGradientModels),
		accumulate_model_gradients(Rows, Models0, InitialGradientModels, GradientModels),
		Scale is 1.0 / Count,
		^^option(learning_rate(LearningRate), Options),
		^^option(l2_regularization(Regularization), Options),
		update_model_weights(Models0, GradientModels, Scale, Regularization, LearningRate, Models1, 0.0, MaxDelta).

	zero_gradient_models([], []).
	zero_gradient_models([class_model(Class, _Bias, Weights)| Models], [class_gradient(Class, 0.0, ZeroWeights)| GradientModels]) :-
		zero_vector_like(Weights, ZeroWeights),
		zero_gradient_models(Models, GradientModels).

	accumulate_model_gradients([], _, GradientModels, GradientModels).
	accumulate_model_gradients([Features-Label| Rows], Models, GradientModels0, GradientModels) :-
		class_logits(Models, Features, ClassLogits),
		stable_softmax(ClassLogits, Probabilities),
		accumulate_row_gradients(Models, Probabilities, Features, Label, GradientModels0, GradientModels1),
		accumulate_model_gradients(Rows, Models, GradientModels1, GradientModels).

	accumulate_row_gradients([], [], _, _, [], []).
	accumulate_row_gradients(
		[class_model(Class, _Bias, _Weights)| Models],
		[Class-Probability| Probabilities],
		Features,
		Label,
		[class_gradient(Class, BiasGradient0, WeightGradients0)| GradientModels0],
		[class_gradient(Class, BiasGradient, WeightGradients)| GradientModels]
	) :-
		target_value(Label, Class, Target),
		Error is Probability - Target,
		BiasGradient is BiasGradient0 + Error,
		add_scaled_vector(Features, Error, WeightGradients0, WeightGradients),
		accumulate_row_gradients(Models, Probabilities, Features, Label, GradientModels0, GradientModels).

	update_model_weights([], [], _, _, _, [], MaxDelta, MaxDelta).
	update_model_weights(
		[class_model(Class, Bias0, Weights0)| Models0],
		[class_gradient(Class, BiasGradient, WeightGradients)| GradientModels],
		Scale,
		Regularization,
		LearningRate,
		[class_model(Class, Bias1, Weights1)| Models1],
		MaxDelta0,
		MaxDelta
	) :-
		MeanBiasGradient is BiasGradient * Scale,
		Bias1 is Bias0 - LearningRate * MeanBiasGradient,
		BiasDelta is abs(Bias1 - Bias0),
		update_weights(Weights0, WeightGradients, Scale, Regularization, LearningRate, Weights1, 0.0, MaxWeightDelta),
		ModelDelta is max(BiasDelta, MaxWeightDelta),
		MaxDelta1 is max(ModelDelta, MaxDelta0),
		update_model_weights(Models0, GradientModels, Scale, Regularization, LearningRate, Models1, MaxDelta1, MaxDelta).

	target_value(Label, Class, 1.0) :-
		Label == Class,
		!.
	target_value(_, _, 0.0).

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

	class_logits([], _, []).
	class_logits([class_model(Class, Bias, Weights)| Models], Features, [Class-Score| ClassLogits]) :-
		dot_product(Weights, Features, Linear),
		Score is Bias + Linear,
		class_logits(Models, Features, ClassLogits).

	stable_softmax([ClassLogit| ClassLogits], Probabilities) :-
		ClassLogit = _-MaxLogit0,
		max_logit(ClassLogits, MaxLogit0, MaxLogit),
		exponentiate_shifted([ClassLogit| ClassLogits], MaxLogit, Shifted, 0.0, SumExp),
		normalize_shifted(Shifted, SumExp, Probabilities).

	max_logit([], MaxLogit, MaxLogit).
	max_logit([_-Logit| ClassLogits], MaxLogit0, MaxLogit) :-
		MaxLogit1 is max(Logit, MaxLogit0),
		max_logit(ClassLogits, MaxLogit1, MaxLogit).

	exponentiate_shifted([], _, [], SumExp, SumExp).
	exponentiate_shifted([Class-Logit| ClassLogits], MaxLogit, [Class-ShiftedExp| Shifted], SumExp0, SumExp) :-
		ShiftedExp is exp(Logit - MaxLogit),
		SumExp1 is SumExp0 + ShiftedExp,
		exponentiate_shifted(ClassLogits, MaxLogit, Shifted, SumExp1, SumExp).

	normalize_shifted([], _, []).
	normalize_shifted([Class-ShiftedExp| Shifted], SumExp, [Class-Probability| Probabilities]) :-
		Probability is ShiftedExp / SumExp,
		normalize_shifted(Shifted, SumExp, Probabilities).

	max_probability([Class-Probability], Class, Probability) :-
		!.
	max_probability([Class1-Probability1, Class2-Probability2| Probabilities], Class, Probability) :-
		(   Probability1 >= Probability2 ->
			max_probability([Class1-Probability1| Probabilities], Class, Probability)
		;   max_probability([Class2-Probability2| Probabilities], Class, Probability)
		).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		classifier_data(Classifier, Classes, Encoders, Models, Options),
		encoders_feature_count(Encoders, 0, EncodedFeatures),
		length(Models, ModelCount),
		Diagnostics = [
			model(logistic_regression),
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
			encoders_feature_count(Encoders, 0, EncodedFeatures),
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

	classifier_term_template(lr_classifier(_Classes, _Encoders, _Models, _Options), lr_classifier('Classes', 'Encoders', 'Models', 'Options')).

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
		format('Logistic Regression Classifier~n', []),
		format('==============================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n', [Classes]),
		print_options(Options),
		format('~nEncoders:~n', []),
		print_encoders(Encoders),
		format('~nModels:~n', []),
		print_models(Models).

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

	print_models([]).
	print_models([class_model(Class, Bias, Weights)| Models]) :-
		length(Weights, NumWeights),
		format('  ~w: bias=~4f, weights=~w coefficients~n', [Class, Bias, NumWeights]),
		print_models(Models).

	default_option(learning_rate(0.1)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-6)).
	default_option(l2_regularization(0.0)).

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
