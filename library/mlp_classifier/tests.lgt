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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Unit tests for the "mlp_classifier" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1, tolerance_equal/4
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- set_logtalk_flag(arithmetic_expressions, silent).

	:- private(xor_classifier_/1).
	:- dynamic(xor_classifier_/1).

	cover(mlp_classifier).

	cleanup :-
		retractall(xor_classifier_(_)),
		^^clean_file('test_output.pl').

	test(mlp_classifier_learn_2_xor, deterministic(ground(Classifier))) :-
		mlp_classifier::learn(xor, Classifier).

	test(mlp_classifier_valid_classifier_1, deterministic) :-
		xor_classifier(Classifier),
		mlp_classifier::check_classifier(Classifier).

	test(mlp_classifier_invalid_classifier_1, fail) :-
		mlp_classifier::valid_classifier(mlp_classifier([false, true], [], tanh, [], [])).

	test(mlp_classifier_validation_fixtures, deterministic) :-
		findall(Model, mlp_classifier_fixtures::invalid_model(Model), Models),
		assertion(Models \== []),
		forall(
			list::member(Model, Models),
			assertion(\+ mlp_classifier::valid_classifier(Model))
		).

	test(mlp_classifier_check_incomplete_fixture, error(instantiation_error)) :-
		mlp_classifier_fixtures::incomplete_model(Model),
		mlp_classifier::check_classifier(Model).

	test(mlp_classifier_valid_incomplete_fixture, fail) :-
		mlp_classifier_fixtures::incomplete_model(Model),
		mlp_classifier::valid_classifier(Model).

	test(mlp_classifier_check_variable, error(instantiation_error)) :-
		mlp_classifier::check_classifier(_Model).

	test(mlp_classifier_check_partial, error(instantiation_error)) :-
		mlp_classifier::check_classifier(mlp_classifier([a, b], [], tanh, [], [hidden_layers(_)])).

	test(mlp_classifier_valid_partial, fail) :-
		mlp_classifier::valid_classifier(mlp_classifier([a, b], [], _Activation, [], [])).

	test(mlp_classifier_custom_functor_validation, deterministic) :-
		mlp_classifier_fixtures::custom_functor(Model),
		mlp_classifier::check_classifier(Model),
		assertion(mlp_classifier::valid_classifier(Model)),
		mlp_classifier::predict_probabilities(Model, [], [a-First, b-Second]),
		assertion([First, Second] =~= [0.5, 0.5]).

	test(mlp_classifier_check_wrong_arity, error(domain_error(classifier, custom_classifier([a, b], [])))) :-
		mlp_classifier::check_classifier(custom_classifier([a, b], [])).

	test(mlp_classifier_custom_functor_prediction_diagnostics_export, deterministic) :-
		xor_classifier(Classifier),
		Classifier =.. [_| Arguments],
		Custom =.. [my_classifier| Arguments],
		mlp_classifier::check_classifier(Custom),
		mlp_classifier::predict(Custom, [x1-0.0, x2-1.0], Prediction),
		assertion(Prediction == true),
		mlp_classifier::predict_probabilities(Classifier, [x1-0.0, x2-1.0], Expected),
		mlp_classifier::predict_probabilities(Custom, [x1-0.0, x2-1.0], Actual),
		assertion(Expected == Actual),
		mlp_classifier::diagnostics(Classifier, Diagnostics),
		mlp_classifier::diagnostics(Custom, CustomDiagnostics),
		assertion(Diagnostics == CustomDiagnostics),
		mlp_classifier::classifier_options(Custom, Options),
		assertion(list::memberchk(maximum_iterations(50), Options)),
		mlp_classifier::export_to_clauses(xor, Custom, saved_model, [saved_model(Exported)]),
		assertion(Exported == Custom).

	test(mlp_classifier_custom_functor_print, deterministic) :-
		^^suppress_text_output,
		mlp_classifier_fixtures::custom_functor(Model),
		mlp_classifier << classifier_term_template(Model, Template),
		assertion(Template == custom_classifier('Classes', 'Encoders', 'Activation', 'Layers', 'Diagnostics')),
		mlp_classifier::print_classifier(Model).

	test(mlp_classifier_duplicate_option_precedence, deterministic) :-
		mlp_classifier::learn(xor, Classifier, [maximum_iterations(1), maximum_iterations(2), activation(tanh), activation(sigmoid), hidden_layers([]), hidden_layers([3])]),
		mlp_classifier::check_classifier(Classifier),
		mlp_classifier::diagnostics(Classifier, Diagnostics),
		assertion(memberchk(iterations(1), Diagnostics)),
		assertion(memberchk(hidden_layers([]), Diagnostics)),
		assertion(memberchk(activation(tanh), Diagnostics)).

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
		mlp_classifier::learn(weather, Classifier, [hidden_layers([]), maximum_iterations(1)]),
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
		mlp_classifier::learn(iris_small, Classifier, [maximum_iterations(1)]),
		Instance = [sepal_length-5.0, sepal_width-3.4, petal_length-1.4, petal_width-0.2],
		mlp_classifier::predict(Classifier, Instance, Prediction),
		mlp_classifier::predict_probabilities(Classifier, Instance, Probabilities),
		iris_small::class_values(Classes),
		probability_classes(Probabilities, ActualClasses, 0.0, Total),
		assertion(Classes == ActualClasses),
		assertion(list::memberchk(Prediction, Classes)),
		assertion(Total =~= 1.0).

	test(mlp_classifier_export_to_clauses_4, deterministic(Prediction == true)) :-
		xor_classifier(Classifier),
		mlp_classifier::export_to_clauses(xor, Classifier, classifier, [classifier(ExportedClassifier)]),
		mlp_classifier::predict(ExportedClassifier, [x1-0.0, x2-1.0], Prediction).

	test(mlp_classifier_export_to_file_4, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		xor_classifier(Classifier),
		mlp_classifier::export_to_file(xor, Classifier, classifier, File).

	test(mlp_classifier_diagnostics_2, deterministic((Model == mlp_classifier, HiddenLayers == [4], Iterations == 50))) :-
		xor_classifier(Classifier),
		mlp_classifier::diagnostics(Classifier, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(hidden_layers(HiddenLayers), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics).

	test(mlp_classifier_print_classifier_1, deterministic) :-
		^^suppress_text_output,
		xor_classifier(Classifier),
		mlp_classifier::print_classifier(Classifier).

	test(mlp_classifier_bias_only_configurations, all(check_bias_only(Activation, Hidden))) :-
		list::member(Activation, [relu, tanh, sigmoid]),
		list::member(Hidden, [[], [2], [2, 2]]).

	test(mlp_classifier_bias_only_update, deterministic) :-
		mlp_classifier::learn(mlp_no_features, Classifier, [hidden_layers([]), maximum_iterations(1), learning_rate(0.1), shuffle(false)]),
		Classifier = mlp_classifier([a, b], [], _, [layer([[], []], Biases)], _),
		assertion(Biases =~= [0.05, -0.05]),
		mlp_classifier::predict(Classifier, [], Prediction),
		assertion(Prediction == a).

	test(mlp_classifier_empty_dataset, error(domain_error(non_empty_dataset, mlp_empty_dataset))) :-
		mlp_classifier::learn(mlp_empty_dataset, _).

	test(mlp_classifier_saturated_loss, deterministic) :-
		Options = [class_weights([]), convergence_criterion(loss), tolerance(0.0)],
		Rows = [[]-a],
		Layers0 = [layer([[], []], [-100.0, 100.0])],
		Layers1 = [layer([[], []], [-99.0, 99.0])],
		network_loss(Rows, [a, b], relu, Options, Layers0, Loss0),
		network_loss(Rows, [a, b], relu, Options, Layers1, Loss1),
		assertion(Loss0 =~= 200.0),
		assertion(Loss1 =~= 198.0),
		mlp_classifier << convergence_status(loss, 0.0, [row([], 1, 1.0)], relu, Layers1, loss(Loss0), 1.0, _, Continued),
		assertion(Continued == false),
		mlp_classifier << convergence_status(loss, 0.0, [row([], 1, 1.0)], relu, Layers1, loss(Loss1), 1.0, _, Stopped),
		assertion(Stopped == true).

	test(mlp_classifier_loss_shift_weight_average, deterministic) :-
		Options = [class_weights([b-2.0])],
		Layers0 = [layer([[], [], []], [0.0, 0.0, 0.0])],
		Layers1 = [layer([[], [], []], [10000.0, 10000.0, 10000.0])],
		Rows = [[]-a, []-b, []-c],
		Expected is 4.0 * log(3.0) / 3.0,
		network_loss(Rows, [a, b, c], tanh, Options, Layers0, Loss0),
		network_loss(Rows, [a, b, c], tanh, Options, Layers1, Loss1),
		assertion(Loss0 =~= Expected),
		assertion(Loss1 =~= Expected).

	test(mlp_classifier_loss_correct_extreme, deterministic(Loss =~= 0.0)) :-
		network_loss([[]-b], [a, b], sigmoid, [class_weights([])], [layer([[], []], [-1000.0, 1000.0])], Loss).

	test(mlp_classifier_convergence_branches, deterministic) :-
		Layers = [layer([[], []], [0.0, 0.0])],
		mlp_classifier << convergence_status(parameter_update, 0.5, [row([], 1, 1.0)], relu, Layers, none, 0.5, none, Boundary),
		assertion(Boundary == true),
		mlp_classifier << convergence_status(parameter_update, 0.5, [row([], 1, 1.0)], relu, Layers, none, 0.6, none, Continued),
		assertion(Continued == false),
		mlp_classifier::learn(mlp_no_features, Model, [hidden_layers([]), convergence_criterion(loss), tolerance(0.0), maximum_iterations(3), shuffle(false)]),
		mlp_classifier::diagnostics(Model, Diagnostics),
		assertion(list::memberchk(iterations(3), Diagnostics)).

	test(mlp_classifier_finite_difference_gradients, all(check_gradients(Activation, Layers))) :-
		Hidden = layer([[0.2, -0.3], [0.4, 0.1]], [0.5, 0.6]),
		Output = layer([[0.3, -0.2], [-0.1, 0.4], [0.2, 0.1]], [0.0, 0.1, -0.1]),
		list::member(Activation, [relu, tanh, sigmoid]),
		list::member(Layers, [[Output], [Hidden, Output], [Hidden, Hidden, Output]]).

	test(mlp_classifier_activation_boundaries, deterministic) :-
		mlp_classifier << activate_vector([-1000.0, 0.0, 1000.0], sigmoid, Sigmoid),
		assertion(Sigmoid =~= [0.0, 0.5, 1.0]),
		mlp_classifier << activate_vector([-1.0, 0.0, 1.0], relu, Relu),
		assertion(Relu =~= [0.0, 0.0, 1.0]),
		mlp_classifier << activation_derivative(relu, 0.0, Zero),
		assertion(Zero =~= 0.0),
		mlp_classifier << activate_vector([-1000.0, 1000.0], tanh, Tanh),
		assertion(Tanh =~= [-1.0, 1.0]).

	test(mlp_classifier_optimizer_reference, all(check_optimizer(Momentum, Regularization))) :-
		list::member(Momentum, [0.0, 0.9]),
		list::member(Regularization, [0.0, 0.2]).

	test(mlp_classifier_softmax_shift, deterministic) :-
		mlp_classifier << softmax([-1.0, 0.0, 1.0], First),
		mlp_classifier << softmax([9999.0, 10000.0, 10001.0], Second),
		assertion(First =~= Second).

	test(mlp_classifier_training_option_equivalence, all(check_training_options(Schedule, Shuffle, Criterion))) :-
		list::member(Schedule, [constant, inverse_scaling(0.5)]),
		list::member(Shuffle, [true, false]),
		list::member(Criterion, [loss, parameter_update]).

	test(mlp_classifier_prepared_rows, deterministic) :-
		mlp_classifier << prepare_training_rows([[1.0]-a, [2.0]-b, [3.0]-c], [a, b, c], [b-2.0], Rows),
		assertion(Rows == [row([1.0], 1, 1.0), row([2.0], 2, 2.0), row([3.0], 3, 1.0)]),
		mlp_classifier << output_delta(3, [0.2, 0.3, 0.5], 2.0, Deltas),
		assertion(Deltas =~= [0.4, 0.6, -1.0]).

	test(mlp_classifier_forward_only_equivalence, all(check_forward(Activation, Layers))) :-
		Hidden = layer([[0.2, -0.3], [0.4, 0.1]], [0.5, 0.6]),
		Output = layer([[0.3, -0.2], [-0.1, 0.4], [0.2, 0.1]], [0.0, 0.1, -0.1]),
		list::member(Activation, [relu, tanh, sigmoid]),
		list::member(Layers, [[Output], [Hidden, Output], [Hidden, Hidden, Output]]).

	test(mlp_classifier_affine_reference, deterministic) :-
		mlp_classifier << linear_output([[0.2, -0.3], [0.4, 0.1]], [0.5, 0.6], [0.7, -0.2], Output),
		assertion(Output =~= [0.7, 0.86]).

	test(mlp_classifier_transpose_product_reference, all(check_transpose_product(Matrix, Vector))) :-
		Cases = [
			[]-[],
			[[], []]-[1.0, 2.0],
			[[1.0]]-[2.0],
			[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]]-[0.2, -0.5],
			[[1.0e12, 1.0], [-1.0e12, 2.0], [1.0, 3.0]]-[1.0, 1.0, 1.0]
		],
		list::member(Matrix-Vector, Cases).

	test(mlp_classifier_update_policy_equivalence, all(check_update_policy(Momentum, Regularization))) :-
		list::member(Momentum, [0, 0.0, 0.9]),
		list::member(Regularization, [0, 0.0, 0.2]).

	test(mlp_classifier_tiny_parameter_update, deterministic) :-
		mlp_classifier << update_layers([layer([[1.0e-8]], [0.0])], none, [[1.0e-8]], [[1.0e-8]], 0.1, 0, 0, [layer([[Weight]], [Bias])], none, 0.0, Largest),
		assertion(Weight < 1.0e-8),
		assertion(Bias < 0.0),
		assertion(Largest > 0.0).

	% auxiliary predicates

	check_update_policy(Momentum, Regularization) :-
		Options = [maximum_iterations(3), tolerance(0.0), momentum(Momentum), l2_regularization(Regularization), shuffle(false), hidden_layers([2, 2])],
		mlp_classifier::learn(xor, mlp_classifier(_, _, _, Tracked, _), [convergence_criterion(parameter_update)| Options]),
		mlp_classifier::learn(xor, mlp_classifier(_, _, _, Untracked, _), [convergence_criterion(loss)| Options]),
		assertion(Tracked == Untracked).

	check_transpose_product(Matrix, Vector) :-
		linear_algebra::transpose_matrix(Matrix, Transpose),
		linear_algebra::matrix_vector_product(Transpose, Vector, Expected),
		mlp_classifier << transpose_product(Matrix, Vector, Actual),
		assertion(Actual == Expected).

	check_forward(Activation, Layers) :-
		mlp_classifier << forward(Layers, Activation, [0.7, -0.2], _, Traced),
		mlp_classifier << forward_only(Layers, Activation, [0.7, -0.2], Plain),
		assertion(Traced == Plain).

	network_loss(Rows, Classes, Activation, Options, Layers, Loss) :-
		list::memberchk(class_weights(Weights), Options),
		mlp_classifier << prepare_training_rows(Rows, Classes, Weights, Prepared),
		mlp_classifier << mean_loss(Prepared, Activation, Layers, Loss).

	check_training_options(Schedule, Shuffle, Criterion) :-
		Options = [maximum_iterations(2), random_seed(17), learning_schedule(Schedule), shuffle(Shuffle), convergence_criterion(Criterion), tolerance(0.0)],
		list::append(Options, [maximum_iterations(7), shuffle(false), learning_schedule(inverse_scaling(2.0))], Repeated),
		mlp_classifier::learn(xor, mlp_classifier(Classes, Encoders, Activation, FirstLayers, _), Options),
		mlp_classifier::learn(xor, Second, Repeated),
		Second = mlp_classifier(Classes, Encoders, Activation, SecondLayers, _),
		assertion(FirstLayers == SecondLayers),
		mlp_classifier::check_classifier(Second).

	probability_classes([], [], Total, Total).
	probability_classes([Class-Probability| Probabilities], [Class| Classes], Total0, Total) :-
		assertion(Probability >= 0.0),
		assertion(Probability =< 1.0),
		Total1 is Total0 + Probability,
		probability_classes(Probabilities, Classes, Total1, Total).

	check_gradients(Activation, Layers) :-
		Features = [0.7, -0.2],
		mlp_classifier << forward(Layers, Activation, Features, Inputs, Logits),
		mlp_classifier << softmax(Logits, Probabilities),
		mlp_classifier << output_delta(2, Probabilities, 2.0, OutputDelta),
		mlp_classifier << layer_deltas(Layers, Activation, Inputs, OutputDelta, Deltas),
		gradient_layers(Inputs, Deltas, Gradients),
		layer_parameters(Layers, Parameters),
		layer_parameters(Gradients, GradientParameters),
		assertion(Parameters \== []),
		list::length(Parameters, Count),
		assertion(list::length(GradientParameters, Count)),
		forall(
			perturb_parameter(Parameters, GradientParameters, 1.0e-6, PlusParameters, MinusParameters, Analytic),
			check_numeric_gradient(Activation, Layers, Features, PlusParameters, MinusParameters, Analytic)
		).

	gradient_layers([], [], []).
	gradient_layers([Input| Inputs], [Delta| Deltas], [layer(Weights, Delta)| Layers]) :-
		linear_algebra::outer_product(Delta, Input, Weights),
		gradient_layers(Inputs, Deltas, Layers).

	layer_parameters([], []).
	layer_parameters([layer(Weights, Biases)| Layers], Parameters) :-
		matrix_parameters(Weights, WeightParameters),
		list::append(WeightParameters, Biases, First),
		list::append(First, Rest, Parameters),
		layer_parameters(Layers, Rest).

	matrix_parameters([], []).
	matrix_parameters([Row| Rows], Parameters) :-
		list::append(Row, Rest, Parameters),
		matrix_parameters(Rows, Rest).

	perturb_parameter([Value| Values], [Gradient| _], Step, [Plus| Values], [Minus| Values], Gradient) :-
		Plus is Value + Step,
		Minus is Value - Step.
	perturb_parameter([Value| Values], [_| Gradients], Step, [Value| Plus], [Value| Minus], Gradient) :-
		perturb_parameter(Values, Gradients, Step, Plus, Minus, Gradient).

	parameter_layers([], [], []).
	parameter_layers([layer(Weights, Biases)| Layers], Parameters, [layer(NewWeights, NewBiases)| NewLayers]) :-
		parameter_matrix(Weights, Parameters, Rest0, NewWeights),
		parameter_vector(Biases, Rest0, Rest, NewBiases),
		parameter_layers(Layers, Rest, NewLayers).

	parameter_matrix([], Parameters, Parameters, []).
	parameter_matrix([Row| Rows], Parameters, Rest, [NewRow| NewRows]) :-
		parameter_vector(Row, Parameters, Rest0, NewRow),
		parameter_matrix(Rows, Rest0, Rest, NewRows).

	parameter_vector([], Parameters, Parameters, []).
	parameter_vector([_| Values], [Parameter| Parameters], Rest, [Parameter| NewValues]) :-
		parameter_vector(Values, Parameters, Rest, NewValues).

	check_numeric_gradient(Activation, Shape, Features, PlusParameters, MinusParameters, Analytic) :-
		parameter_layers(Shape, PlusParameters, PlusLayers),
		parameter_layers(Shape, MinusParameters, MinusLayers),
		network_loss([Features-b], [a, b, c], Activation, [class_weights([b-2.0])], PlusLayers, PlusLoss),
		network_loss([Features-b], [a, b, c], Activation, [class_weights([b-2.0])], MinusLayers, MinusLoss),
		Numeric is (PlusLoss - MinusLoss) / 2.0e-6,
		assertion(lgtunit::tolerance_equal(Analytic, Numeric, 1.0e-5, 1.0e-7)).

	check_optimizer(Momentum, Regularization) :-
		Layers0 = [layer([[0.5]], [0.1])],
		Velocities0 = [layer([[0.2]], [-0.3])],
		WeightVelocity1 is Momentum * 0.2 - 0.1 * (0.4 * 2.0 + Regularization * 0.5),
		BiasVelocity1 is Momentum * (-0.3) - 0.1 * 0.4,
		Weight1 is 0.5 + WeightVelocity1,
		Bias1 is 0.1 + BiasVelocity1,
		Delta1 is max(abs(WeightVelocity1), abs(BiasVelocity1)),
		mlp_classifier << update_layers(Layers0, Velocities0, [[2.0]], [[0.4]], 0.1, Regularization, Momentum, Layers1, Velocities1, 0.0, ActualDelta1),
		Layers1 = [layer([[ActualWeight1]], [ActualBias1])],
		assertion([ActualWeight1, ActualBias1, ActualDelta1] =~= [Weight1, Bias1, Delta1]),
		mlp_classifier << update_layers(Layers0, Velocities0, [[2.0]], [[0.4]], 0.1, Regularization, Momentum, UntrackedLayers, UntrackedVelocities, none, none),
		assertion(UntrackedLayers == Layers1),
		assertion(UntrackedVelocities == Velocities1),
		(	Momentum =:= 0 ->
			mlp_classifier << update_layers(Layers0, none, [[2.0]], [[0.4]], 0.1, Regularization, Momentum, PlainLayers, none, 0.0, PlainDelta),
			assertion(PlainLayers == Layers1),
			assertion(PlainDelta =~= Delta1),
			mlp_classifier << update_layers(PlainLayers, none, [[2.0]], [[0.4]], 0.1, Regularization, Momentum, PlainLayers2, none, none, none)
		;	PlainLayers2 = unused
		),
		WeightVelocity2 is Momentum * WeightVelocity1 - 0.1 * (0.4 * 2.0 + Regularization * Weight1),
		BiasVelocity2 is Momentum * BiasVelocity1 - 0.1 * 0.4,
		Weight2 is Weight1 + WeightVelocity2,
		Bias2 is Bias1 + BiasVelocity2,
		Delta2 is max(abs(WeightVelocity2), abs(BiasVelocity2)),
		mlp_classifier << update_layers(Layers1, Velocities1, [[2.0]], [[0.4]], 0.1, Regularization, Momentum, [layer([[ActualWeight2]], [ActualBias2])], [layer([[ActualVelocity2]], [ActualBiasVelocity2])], 0.0, ActualDelta2),
		assertion([ActualWeight2, ActualBias2, ActualVelocity2, ActualBiasVelocity2, ActualDelta2] =~= [Weight2, Bias2, WeightVelocity2, BiasVelocity2, Delta2]),
		(	Momentum =:= 0 ->
			assertion(PlainLayers2 == [layer([[ActualWeight2]], [ActualBias2])])
		;	true
		).

	check_bias_only(Activation, Hidden) :-
		mlp_classifier::learn(mlp_no_features, Classifier, [activation(Activation), hidden_layers(Hidden), maximum_iterations(1)]),
		mlp_classifier::check_classifier(Classifier),
		Classifier = mlp_classifier([a, b], [], Activation, _, _),
		mlp_classifier::predict_probabilities(Classifier, [], [a-First, b-Second]),
		Total is First + Second,
		assertion(Total =~= 1.0),
		assertion(First >= 0.0),
		assertion(Second >= 0.0).

	xor_options([
		hidden_layers([4]),
		activation(tanh),
		learning_rate(0.1),
		learning_schedule(constant),
		maximum_iterations(50),
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
