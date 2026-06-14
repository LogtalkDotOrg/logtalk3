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


:- object(sgd_classifier,
	imports(probabilistic_classifier_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-11,
		comment is 'Stochastic gradient descent classifier supporting one-vs-rest linear models with configurable losses including ``log_loss``, ``hinge``, ``squared_hinge``, ``modified_huber``, and ``perceptron``.',
		see_also is [dataset_protocol, logistic_regression_classifier, linear_svm_classifier, knn_classifier, nearest_centroid_classifier]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(linear_algebra, [
		dot_product/3, new_vector/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		softmax/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		Dataset::class_values(Classes),
		^^option(feature_scaling(FeatureScaling), Options),
		^^build_linear_encoders(Attributes, Examples, FeatureScaling, Encoders),
		^^examples_to_linear_rows(Examples, Encoders, Rows),
		^^linear_encoders_feature_count(Encoders, FeatureCount),
		train_models(Classes, Rows, FeatureCount, Options, Models),
		^^option(loss(Loss), Options),
		Classifier = sgd_classifier(Classes, Encoders, Loss, Models, Options).

	predict(Classifier, Instance, Class) :-
		^^predict_from_probabilities(Classifier, Instance, Class).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier = sgd_classifier(Classes, Encoders, _Loss, Models, _Options),
		^^encode_linear_instance(Encoders, Instance, Features),
		class_scores(Models, Features, Scores),
		score_pairs(Classes, Scores, ClassScores),
		scores_to_probabilities(ClassScores, Probabilities).

	train_models([], _Rows, _FeatureCount, _Options, []).
	train_models([Class| Classes], Rows, FeatureCount, Options, [Model| Models]) :-
		new_vector(FeatureCount, 0.0, InitialWeights),
		optimize_model(Rows, Class, Options, 0, class_model(Class, 0.0, InitialWeights), Model),
		train_models(Classes, Rows, FeatureCount, Options, Models).

	optimize_model(Rows, PositiveClass, Options, Epoch, Model0, Model) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Epoch >= MaximumIterations ->
			Model = Model0
		;	learning_rate_for_epoch(Options, Epoch, Step),
			process_rows(Rows, PositiveClass, Options, Step, Model0, Model1, 0.0, MaxDelta),
			^^option(tolerance(Tolerance), Options),
			(	MaxDelta =< Tolerance ->
				Model = Model1
			;	NextEpoch is Epoch + 1,
				optimize_model(Rows, PositiveClass, Options, NextEpoch, Model1, Model)
			)
		).

	learning_rate_for_epoch(Options, Epoch, Step) :-
		^^option(learning_rate(LearningRate), Options),
		^^option(learning_schedule(Schedule), Options),
		(	Schedule == constant ->
			Step = LearningRate
		;	Schedule = inverse_scaling(Power) ->
			Step is LearningRate / ((Epoch + 1.0) ** Power)
		;	domain_error(learning_schedule, Schedule)
		).

	process_rows([], _PositiveClass, _Options, _Step, Model, Model, MaxDelta, MaxDelta).
	process_rows([Features-Label| Rows], PositiveClass, Options, Step, Model0, Model, MaxDelta0, MaxDelta) :-
		update_model(Options, PositiveClass, Features, Label, Step, Model0, Model1, Delta),
		MaxDelta1 is max(MaxDelta0, Delta),
		process_rows(Rows, PositiveClass, Options, Step, Model1, Model, MaxDelta1, MaxDelta).

	update_model(Options, PositiveClass, Features, Label, Step, class_model(Class, Bias0, Weights0), class_model(Class, Bias, Weights), Delta) :-
		target_sign(Label, PositiveClass, Sign),
		dot_product(Weights0, Features, Linear),
		Score is Bias0 + Linear,
		^^option(loss(Loss), Options),
		loss_gradient(Loss, Sign, Score, BiasGradient, WeightScale),
		^^option(l2_regularization(Regularization), Options),
		update_weights(Weights0, Features, Step, Regularization, WeightScale, Weights, 0.0, WeightDelta),
		Bias is Bias0 - Step * BiasGradient,
		BiasDelta is abs(Bias - Bias0),
		Delta is max(BiasDelta, WeightDelta),
		Class = PositiveClass.

	loss_gradient(log_loss, Sign, Score, BiasGradient, WeightScale) :-
		(	Sign > 0.0 ->
			Target = 1.0
		;	Target = 0.0
		),
		Probability is 1.0 / (1.0 + exp(-Score)),
		BiasGradient is Probability - Target,
		WeightScale = BiasGradient.
	loss_gradient(hinge, Sign, Score, BiasGradient, WeightScale) :-
		Margin is Sign * Score,
		(	Margin < 1.0 ->
			BiasGradient is -Sign,
			WeightScale is -Sign
		;	BiasGradient = 0.0,
			WeightScale = 0.0
		).
	loss_gradient(squared_hinge, Sign, Score, BiasGradient, WeightScale) :-
		Margin is Sign * Score,
		(	Margin < 1.0 ->
			Factor is -2.0 * (1.0 - Margin) * Sign,
			BiasGradient = Factor,
			WeightScale = Factor
		;	BiasGradient = 0.0,
			WeightScale = 0.0
		).
	loss_gradient(perceptron, Sign, Score, BiasGradient, WeightScale) :-
		Activation is Sign * Score,
		(	Activation =< 0.0 ->
			BiasGradient is -Sign,
			WeightScale is -Sign
		;	BiasGradient = 0.0,
			WeightScale = 0.0
		).
	loss_gradient(modified_huber, Sign, Score, BiasGradient, WeightScale) :-
		Margin is Sign * Score,
		(	Margin >= 1.0 ->
			BiasGradient = 0.0,
			WeightScale = 0.0
		;	Margin >= -1.0 ->
			Factor is -2.0 * (1.0 - Margin) * Sign,
			BiasGradient = Factor,
			WeightScale = Factor
		;	BiasGradient is -4.0 * Sign,
			WeightScale is -4.0 * Sign
		).

	update_weights([], [], _Step, _Regularization, _WeightScale, [], MaxDelta, MaxDelta).
	update_weights([Weight0| Weights0], [Feature| Features], Step, Regularization, WeightScale, [Weight| Weights], MaxDelta0, MaxDelta) :-
		Gradient is Regularization * Weight0 + WeightScale * Feature,
		Weight is Weight0 - Step * Gradient,
		Delta is abs(Weight - Weight0),
		MaxDelta1 is max(MaxDelta0, Delta),
		update_weights(Weights0, Features, Step, Regularization, WeightScale, Weights, MaxDelta1, MaxDelta).

	target_sign(Label, PositiveClass, 1.0) :-
		Label == PositiveClass,
		!.
	target_sign(_, _, -1.0).

	class_scores([], _Features, []).
	class_scores([class_model(_Class, Bias, Weights)| Models], Features, [Score| Scores]) :-
		dot_product(Weights, Features, Linear),
		Score is Bias + Linear,
		class_scores(Models, Features, Scores).

	score_pairs([], [], []).
	score_pairs([Class| Classes], [Score| Scores], [Class-Score| ClassScores]) :-
		score_pairs(Classes, Scores, ClassScores).

	scores_to_probabilities(ClassScores, Probabilities) :-
		score_values(ClassScores, Classes, Scores),
		softmax(Scores, Probabilities0),
		zip_probabilities(Classes, Probabilities0, Probabilities).

	score_values([], [], []).
	score_values([Class-Score| ClassScores], [Class| Classes], [Score| Scores]) :-
		score_values(ClassScores, Classes, Scores).

	zip_probabilities([], [], []).
	zip_probabilities([Class| Classes], [Probability| Probabilities0], [Class-Probability| Probabilities]) :-
		zip_probabilities(Classes, Probabilities0, Probabilities).

	classifier_diagnostics_data(Classifier, Diagnostics) :-
		classifier_data(Classifier, Classes, Encoders, Loss, Models, Options),
		^^linear_encoders_feature_count(Encoders, EncodedFeatures),
		length(Models, ModelCount),
		Diagnostics = [
			model(sgd_classifier),
			loss(Loss),
			classes(Classes),
			encoded_features(EncodedFeatures),
			models(ModelCount),
			options(Options)
		].

	check_classifier(Classifier) :-
		(	classifier_data(Classifier, Classes, Encoders, Loss, Models, Options),
			^^valid_class_values(Classes),
			^^valid_linear_encoders(Encoders),
			valid_loss(Loss),
			catch(::check_options(Options), _Error, fail),
			^^linear_encoders_feature_count(Encoders, EncodedFeatures),
			length(Classes, ModelCount),
			length(Models, ModelCount),
			valid_models(Models, Classes, EncodedFeatures, []) ->
			true
		;	domain_error(classifier, Classifier)
		).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(sgd_classifier(_Classes, _Encoders, _Loss, _Models, _Options), sgd_classifier('Classes', 'Encoders', 'Loss', 'Models', 'Options')).

	valid_models([], _Classes, _EncodedFeatures, _SeenClasses).
	valid_models([class_model(Class, Bias, Weights)| Models], Classes, EncodedFeatures, SeenClasses) :-
		memberchk(Class, Classes),
		\+ member(Class, SeenClasses),
		valid(float, Bias),
		valid(list(float, EncodedFeatures), Weights),
		valid_models(Models, Classes, EncodedFeatures, [Class| SeenClasses]).

	valid_loss(log_loss).
	valid_loss(hinge).
	valid_loss(squared_hinge).
	valid_loss(modified_huber).
	valid_loss(perceptron).

	classifier_data(Classifier, Classes, Encoders, Loss, Models, Options) :-
		Classifier =.. [_Functor, Classes, Encoders, Loss, Models, Options].

	print_classifier(Classifier) :-
		classifier_data(Classifier, Classes, Encoders, Loss, Models, Options),
		format('SGD Classifier~n', []),
		format('==============~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n', [Classes]),
		format('Loss: ~w~n', [Loss]),
		format('Options: ~w~n~n', [Options]),
		format('Encoders: ~w~n', [Encoders]),
		format('Models: ~w~n', [Models]).

	default_option(loss(log_loss)).
	default_option(learning_rate(0.05)).
	default_option(learning_schedule(constant)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-5)).
	default_option(l2_regularization(0.0001)).
	default_option(feature_scaling(true)).

	valid_option(loss(Loss)) :-
		valid_loss(Loss).
	valid_option(learning_rate(Rate)) :-
		number(Rate),
		Rate > 0.0.
	valid_option(learning_schedule(constant)).
	valid_option(learning_schedule(inverse_scaling(Power))) :-
		number(Power),
		Power > 0.0.
	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(l2_regularization(Regularization)) :-
		number(Regularization),
		Regularization >= 0.0.
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.
