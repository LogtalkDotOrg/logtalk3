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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-09,
		comment is 'Unit tests for the "mlp_classifier" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- private(xor_classifier_/1).
	:- dynamic(xor_classifier_/1).

	cover(mlp_classifier).

	cleanup :-
		retractall(xor_classifier_(_)),
		^^clean_file('test_output.pl').

	test(mlp_classifier_learn_2_xor, deterministic(ground(Classifier))) :-
		mlp_classifier::learn(xor, Classifier).

	test(mlp_classifier_valid_classifier_1, deterministic) :-
		xor_options(Options),
		mlp_classifier::learn(xor, Classifier, Options),
		mlp_classifier::check_classifier(Classifier).

	test(mlp_classifier_invalid_classifier_1, fail) :-
		mlp_classifier::valid_classifier(mlp_classifier([false, true], [], tanh, [], [])).

	test(mlp_classifier_xor_00, deterministic(Prediction == false)) :-
		xor_classifier(Classifier),
		mlp_classifier::predict(Classifier, [x1-0.0, x2-0.0], Prediction).

	test(mlp_classifier_xor_01, deterministic(Prediction == true)) :-
		xor_classifier(Classifier),
		mlp_classifier::predict(Classifier, [x1-0.0, x2-1.0], Prediction).

	test(mlp_classifier_xor_10, deterministic(Prediction == true)) :-
		xor_classifier(Classifier),
		mlp_classifier::predict(Classifier, [x1-1.0, x2-0.0], Prediction).

	test(mlp_classifier_xor_11, deterministic(Prediction == false)) :-
		xor_classifier(Classifier),
		mlp_classifier::predict(Classifier, [x1-1.0, x2-1.0], Prediction).

	test(mlp_classifier_probabilities_sum, deterministic(Total =~= 1.0)) :-
		xor_classifier(Classifier),
		mlp_classifier::predict_probabilities(Classifier, [x1-0.0, x2-1.0], Probabilities),
		memberchk(false-False, Probabilities),
		memberchk(true-True, Probabilities),
		Total is False + True.

	test(mlp_classifier_learn_3_custom_options, deterministic([HiddenLayers, Activation, Schedule, Seed, Shuffle] == [[5, 3], sigmoid, inverse_scaling(0.5), 17, false])) :-
		mlp_classifier::learn(weather, Classifier, [hidden_layers([5, 3]), activation(sigmoid), learning_schedule(inverse_scaling(0.5)), maximum_iterations(1), random_seed(17), shuffle(false)]),
		mlp_classifier::classifier_options(Classifier, Options),
		memberchk(hidden_layers(HiddenLayers), Options),
		memberchk(activation(Activation), Options),
		memberchk(learning_schedule(Schedule), Options),
		memberchk(random_seed(Seed), Options),
		memberchk(shuffle(Shuffle), Options).

	test(mlp_classifier_no_hidden_layers, deterministic((HiddenLayers == [], ground(Prediction)))) :-
		mlp_classifier::learn(weather, Classifier, [hidden_layers([]), maximum_iterations(20)]),
		mlp_classifier::check_classifier(Classifier),
		mlp_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(hidden_layers(HiddenLayers), Diagnostics),
		mlp_classifier::predict(Classifier, [outlook-overcast, temperature-hot, humidity-normal, wind-weak], Prediction).

	test(mlp_classifier_class_weights, deterministic((ClassWeights == [yes-2.0], UnweightedLayers \== WeightedLayers))) :-
		mlp_classifier::learn(weather, mlp_classifier(_Classes1, _Encoders1, _Activation1, UnweightedLayers, _Diagnostics1), [class_weights([]), maximum_iterations(1), random_seed(17), shuffle(false)]),
		mlp_classifier::learn(weather, WeightedClassifier, [class_weights([yes-2.0]), maximum_iterations(1), random_seed(17), shuffle(false)]),
		WeightedClassifier = mlp_classifier(_Classes2, _Encoders2, _Activation2, WeightedLayers, _Diagnostics2),
		mlp_classifier::check_classifier(WeightedClassifier),
		mlp_classifier::classifier_options(WeightedClassifier, Options),
		memberchk(class_weights(ClassWeights), Options).

	test(mlp_classifier_unknown_class_weight, error(domain_error(class_weight_class, maybe))) :-
		mlp_classifier::learn(weather, _Classifier, [class_weights([maybe-2.0])]).

	test(mlp_classifier_momentum, deterministic((Momentum == 0.9, PlainLayers \== MomentumLayers))) :-
		mlp_classifier::learn(weather, mlp_classifier(_Classes1, _Encoders1, _Activation1, PlainLayers, _Diagnostics1), [maximum_iterations(1), momentum(0.0), random_seed(17), shuffle(false)]),
		mlp_classifier::learn(weather, MomentumClassifier, [maximum_iterations(1), momentum(0.9), random_seed(17), shuffle(false)]),
		MomentumClassifier = mlp_classifier(_Classes2, _Encoders2, _Activation2, MomentumLayers, _Diagnostics2),
		mlp_classifier::classifier_options(MomentumClassifier, Options),
		memberchk(momentum(Momentum), Options).

	test(mlp_classifier_loss_convergence, deterministic((Criterion == loss, Iterations == 2))) :-
		mlp_classifier::learn(weather, Classifier, [convergence_criterion(loss), maximum_iterations(10), tolerance(1.0e6), shuffle(false)]),
		mlp_classifier::classifier_options(Classifier, Options),
		memberchk(convergence_criterion(Criterion), Options),
		mlp_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(iterations(Iterations), Diagnostics).

	test(mlp_classifier_multiclass_iris, deterministic(ground(Prediction))) :-
		mlp_classifier::learn(iris_small, Classifier, [maximum_iterations(20)]),
		mlp_classifier::predict(Classifier, [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2], Prediction).

	test(mlp_classifier_export_to_clauses_4, deterministic(Prediction == true)) :-
		xor_classifier(Classifier),
		mlp_classifier::export_to_clauses(xor, Classifier, classifier, [classifier(ExportedClassifier)]),
		mlp_classifier::predict(ExportedClassifier, [x1-0.0, x2-1.0], Prediction).

	test(mlp_classifier_export_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		xor_classifier(Classifier),
		mlp_classifier::export_to_file(xor, Classifier, classifier, File).

	test(mlp_classifier_diagnostics_2, deterministic((Model == mlp_classifier, HiddenLayers == [4], Iterations == 500))) :-
		xor_classifier(Classifier),
		mlp_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(hidden_layers(HiddenLayers), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics).

	test(mlp_classifier_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		xor_classifier(Classifier),
		mlp_classifier::print_classifier(Classifier).

	% auxiliary predicates

	xor_options([
		hidden_layers([4]),
		activation(tanh),
		learning_rate(0.1),
		learning_schedule(constant),
		maximum_iterations(500),
		momentum(0.9),
		tolerance(0.0),
		l2_regularization(0.0),
		random_seed(42)
	]).

	xor_classifier(Classifier) :-
		(	xor_classifier_(Classifier) ->
			true
		;	xor_options(Options),
			mlp_classifier::learn(xor, Classifier, Options),
			assertz(xor_classifier_(Classifier))
		).

:- end_object.
