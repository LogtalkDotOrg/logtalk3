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


:- object(mlp_classifier,
	imports(probabilistic_classifier_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-09,
		comment is 'Multi-layer perceptron classifier with configurable hidden layers and activation function, softmax output, and backpropagation training.',
		see_also is [dataset_protocol, logistic_regression_classifier, sgd_classifier]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		permutation/2, randomize/1, standard_normal/1
	]).

	:- uses(linear_algebra, [
		matrix_vector_product/3, outer_product/3, transpose_matrix/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_complete_examples(Dataset, Examples),
		Dataset::class_values(Classes),
		^^option(class_weights(ClassWeights), Options),
		check_class_weight_classes(ClassWeights, Classes),
		^^option(feature_scaling(FeatureScaling), Options),
		^^build_linear_encoders(Attributes, Examples, FeatureScaling, Encoders),
		^^examples_to_linear_rows(Examples, Encoders, Rows),
		^^linear_encoders_feature_count(Encoders, FeatureCount),
		length(Classes, ClassCount),
		^^option(hidden_layers(HiddenLayers), Options),
		^^option(activation(Activation), Options),
		^^option(random_seed(RandomSeed), Options),
		randomize(RandomSeed),
		append([FeatureCount| HiddenLayers], [ClassCount], LayerSizes),
		initialize_layers(LayerSizes, Activation, Layers0),
		initialize_velocities(Layers0, Velocities0),
		optimize(Rows, Classes, Activation, Options, 0, none, Layers0, Velocities0, Layers, Iterations),
		Diagnostics = [
			model(mlp_classifier),
			classes(Classes),
			encoded_features(FeatureCount),
			hidden_layers(HiddenLayers),
			activation(Activation),
			iterations(Iterations),
			options(Options)
		],
		Classifier = mlp_classifier(Classes, Encoders, Activation, Layers, Diagnostics).

	predict(Classifier, Instance, Class) :-
		^^predict_from_probabilities(Classifier, Instance, Class).

	predict_probabilities(mlp_classifier(Classes, Encoders, Activation, Layers, _Diagnostics), Instance, Probabilities) :-
		^^encode_linear_instance(Encoders, Instance, Features),
		forward(Layers, Activation, Features, _Inputs, Output),
		softmax(Output, Values),
		pair_probabilities(Classes, Values, Probabilities).

	initialize_layers([_], _Activation, []) :-
		!.
	initialize_layers([InputSize, OutputSize| Sizes], Activation, [layer(Weights, Biases)| Layers]) :-
		initialization_scale(Activation, InputSize, Scale),
		initialize_matrix(OutputSize, InputSize, Scale, Weights),
		zero_vector(OutputSize, Biases),
		initialize_layers([OutputSize| Sizes], Activation, Layers).

	initialization_scale(relu, InputSize, Scale) :-
		Scale is sqrt(2.0 / InputSize).
	initialization_scale(tanh, InputSize, Scale) :-
		Scale is sqrt(1.0 / InputSize).
	initialization_scale(sigmoid, InputSize, Scale) :-
		Scale is sqrt(1.0 / InputSize).

	initialize_matrix(0, _Columns, _Scale, []) :-
		!.
	initialize_matrix(Rows, Columns, Scale, [Row| Matrix]) :-
		initialize_vector(Columns, Scale, Row),
		RemainingRows is Rows - 1,
		initialize_matrix(RemainingRows, Columns, Scale, Matrix).

	initialize_vector(0, _Scale, []) :-
		!.
	initialize_vector(Count, Scale, [Value| Values]) :-
		standard_normal(Normal),
		Value is Normal * Scale,
		Remaining is Count - 1,
		initialize_vector(Remaining, Scale, Values).

	zero_vector(0, []) :-
		!.
	zero_vector(Count, [0.0| Values]) :-
		Remaining is Count - 1,
		zero_vector(Remaining, Values).

	initialize_velocities([], []).
	initialize_velocities([layer(Weights, Biases)| Layers], [layer(WeightVelocities, BiasVelocities)| Velocities]) :-
		zero_matrix_like(Weights, WeightVelocities),
		length(Biases, BiasCount),
		zero_vector(BiasCount, BiasVelocities),
		initialize_velocities(Layers, Velocities).

	zero_matrix_like([], []).
	zero_matrix_like([Row| Rows], [ZeroRow| ZeroRows]) :-
		length(Row, ColumnCount),
		zero_vector(ColumnCount, ZeroRow),
		zero_matrix_like(Rows, ZeroRows).

	optimize(Rows, Classes, Activation, Options, Epoch, PreviousLoss, Layers0, Velocities0, Layers, Iterations) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Epoch >= MaximumIterations ->
			Layers = Layers0,
			Iterations = Epoch
		;	learning_rate(Options, Epoch, Step),
			training_rows(Options, Rows, EpochRows),
			train_epoch(EpochRows, Classes, Activation, Options, Step, Layers0, Velocities0, Layers1, Velocities1, 0.0, MaxDelta),
			NextEpoch is Epoch + 1,
			convergence_status(Options, Rows, Classes, Activation, Layers1, PreviousLoss, MaxDelta, LossState, Converged),
			(	Converged == true ->
				Layers = Layers1,
				Iterations = NextEpoch
			;	optimize(Rows, Classes, Activation, Options, NextEpoch, LossState, Layers1, Velocities1, Layers, Iterations)
			)
		).

	convergence_status(Options, Rows, Classes, Activation, Layers, PreviousLoss, MaxDelta, LossState, Converged) :-
		^^option(convergence_criterion(Criterion), Options),
		^^option(tolerance(Tolerance), Options),
		(	Criterion == parameter_update ->
			LossState = none,
			(	MaxDelta =< Tolerance ->
				Converged = true
			;	Converged = false
			)
		;	mean_loss(Rows, Classes, Activation, Options, Layers, Loss),
			LossState = loss(Loss),
			(	PreviousLoss = loss(PreviousLossValue) ->
				Difference is abs(PreviousLossValue - Loss),
				(	Difference =< Tolerance ->
					Converged = true
				;	Converged = false
				)
			;	Converged = false
			)
		).

	mean_loss(Rows, Classes, Activation, Options, Layers, Loss) :-
		mean_loss(Rows, Classes, Activation, Options, Layers, 0.0, TotalLoss),
		length(Rows, Count),
		Loss is TotalLoss / Count.

	mean_loss([], _Classes, _Activation, _Options, _Layers, TotalLoss, TotalLoss).
	mean_loss([Features-Label| Rows], Classes, Activation, Options, Layers, TotalLoss0, TotalLoss) :-
		forward(Layers, Activation, Features, _Inputs, Output),
		softmax(Output, Probabilities),
		class_probability(Classes, Label, Probabilities, Probability),
		class_weight(Options, Label, ClassWeight),
		SafeProbability is max(Probability, 1.0e-15),
		TotalLoss1 is TotalLoss0 - ClassWeight * log(SafeProbability),
		mean_loss(Rows, Classes, Activation, Options, Layers, TotalLoss1, TotalLoss).

	class_probability([Class| _Classes], Label, [Probability| _Probabilities], Probability) :-
		Class == Label,
		!.
	class_probability([_Class| Classes], Label, [_Probability| Probabilities], Probability) :-
		class_probability(Classes, Label, Probabilities, Probability).

	training_rows(Options, Rows, EpochRows) :-
		^^option(shuffle(Shuffle), Options),
		(	Shuffle == true ->
			permutation(Rows, EpochRows)
		;	EpochRows = Rows
		).

	learning_rate(Options, Epoch, Step) :-
		^^option(learning_rate(Rate), Options),
		^^option(learning_schedule(Schedule), Options),
		(	Schedule == constant ->
			Step = Rate
		;	Schedule = inverse_scaling(Power),
			Step is Rate / ((Epoch + 1.0) ** Power)
		).

	train_epoch([], _Classes, _Activation, _Options, _Step, Layers, Velocities, Layers, Velocities, MaxDelta, MaxDelta).
	train_epoch([Features-Label| Rows], Classes, Activation, Options, Step, Layers0, Velocities0, Layers, Velocities, MaxDelta0, MaxDelta) :-
		forward(Layers0, Activation, Features, Inputs, Output),
		softmax(Output, Probabilities),
		class_weight(Options, Label, ClassWeight),
		output_delta(Classes, Label, Probabilities, ClassWeight, OutputDelta),
		layer_deltas(Layers0, Activation, Inputs, OutputDelta, Deltas),
		^^option(l2_regularization(Regularization), Options),
		^^option(momentum(Momentum), Options),
		update_layers(Layers0, Velocities0, Inputs, Deltas, Step, Regularization, Momentum, Layers1, Velocities1, 0.0, RowDelta),
		MaxDelta1 is max(MaxDelta0, RowDelta),
		train_epoch(Rows, Classes, Activation, Options, Step, Layers1, Velocities1, Layers, Velocities, MaxDelta1, MaxDelta).

	forward([layer(Weights, Biases)], _Activation, Input, [Input], Output) :-
		!,
		linear_output(Weights, Biases, Input, Output).
	forward([layer(Weights, Biases)| Layers], Activation, Input, [Input| Inputs], Output) :-
		linear_output(Weights, Biases, Input, Linear),
		activate_vector(Linear, Activation, Activated),
		forward(Layers, Activation, Activated, Inputs, Output).

	linear_output(Weights, Biases, Input, Output) :-
		matrix_vector_product(Weights, Input, Products),
		add_vectors(Products, Biases, Output).

	add_vectors([], [], []).
	add_vectors([Value1| Values1], [Value2| Values2], [Value| Values]) :-
		Value is Value1 + Value2,
		add_vectors(Values1, Values2, Values).

	activate_vector([], _Activation, []).
	activate_vector([Value| Values], Activation, [Activated| ActivatedValues]) :-
		activate(Activation, Value, Activated),
		activate_vector(Values, Activation, ActivatedValues).

	activate(relu, Value, Activated) :-
		Activated is max(0.0, Value).
	activate(tanh, Value, Activated) :-
		Activated is tanh(Value).
	activate(sigmoid, Value, Activated) :-
		(	Value >= 0.0 ->
			Activated is 1.0 / (1.0 + exp(-Value))
		;	Exponential is exp(Value),
			Activated is Exponential / (1.0 + Exponential)
		).

	activation_derivative(relu, Activated, Derivative) :-
		(	Activated > 0.0 ->
			Derivative = 1.0
		;	Derivative = 0.0
		).
	activation_derivative(tanh, Activated, Derivative) :-
		Derivative is 1.0 - Activated * Activated.
	activation_derivative(sigmoid, Activated, Derivative) :-
		Derivative is Activated * (1.0 - Activated).

	class_weight(Options, Label, ClassWeight) :-
		^^option(class_weights(ClassWeights), Options),
		(	member(Label-ClassWeight, ClassWeights) ->
			true
		;	ClassWeight = 1.0
		).

	output_delta([], _Label, [], _ClassWeight, []).
	output_delta([Class| Classes], Label, [Probability| Probabilities], ClassWeight, [Delta| Deltas]) :-
		(	Class == Label ->
			Target = 1.0
		;	Target = 0.0
		),
		Delta is ClassWeight * (Probability - Target),
		output_delta(Classes, Label, Probabilities, ClassWeight, Deltas).

	layer_deltas([_OutputLayer], _Activation, [_OutputInput], OutputDelta, [OutputDelta]) :-
		!.
	layer_deltas([_Layer, NextLayer| Layers], Activation, [_Input, Activated| Inputs], OutputDelta, [Delta| Deltas]) :-
		layer_deltas([NextLayer| Layers], Activation, [Activated| Inputs], OutputDelta, Deltas),
		Deltas = [NextDelta| _],
		NextLayer = layer(NextWeights, _NextBiases),
		transpose_matrix(NextWeights, Transpose),
		matrix_vector_product(Transpose, NextDelta, Propagated),
		activation_delta(Activated, Activation, Propagated, Delta).

	activation_delta([], _Activation, [], []).
	activation_delta([Activated| ActivatedValues], Activation, [Value| Values], [Delta| Deltas]) :-
		activation_derivative(Activation, Activated, Derivative),
		Delta is Value * Derivative,
		activation_delta(ActivatedValues, Activation, Values, Deltas).

	update_layers([], [], [], [], _Step, _Regularization, _Momentum, [], [], MaxDelta, MaxDelta).
	update_layers([layer(Weights0, Biases0)| Layers0], [layer(WeightVelocities0, BiasVelocities0)| Velocities0], [Input| Inputs], [Delta| Deltas], Step, Regularization, Momentum, [layer(Weights, Biases)| Layers], [layer(WeightVelocities, BiasVelocities)| Velocities], MaxDelta0, MaxDelta) :-
		outer_product(Delta, Input, WeightGradients),
		update_matrix(Weights0, WeightVelocities0, WeightGradients, Step, Regularization, Momentum, Weights, WeightVelocities, MaxDelta0, MaxDelta1),
		update_biases(Biases0, BiasVelocities0, Delta, Step, Momentum, Biases, BiasVelocities, MaxDelta1, MaxDelta2),
		update_layers(Layers0, Velocities0, Inputs, Deltas, Step, Regularization, Momentum, Layers, Velocities, MaxDelta2, MaxDelta).

	update_matrix([], [], [], _Step, _Regularization, _Momentum, [], [], MaxDelta, MaxDelta).
	update_matrix([Row0| Rows0], [VelocityRow0| VelocityRows0], [GradientRow| GradientRows], Step, Regularization, Momentum, [Row| Rows], [VelocityRow| VelocityRows], MaxDelta0, MaxDelta) :-
		update_weights(Row0, VelocityRow0, GradientRow, Step, Regularization, Momentum, Row, VelocityRow, MaxDelta0, MaxDelta1),
		update_matrix(Rows0, VelocityRows0, GradientRows, Step, Regularization, Momentum, Rows, VelocityRows, MaxDelta1, MaxDelta).

	update_weights([], [], [], _Step, _Regularization, _Momentum, [], [], MaxDelta, MaxDelta).
	update_weights([Weight0| Weights0], [Velocity0| Velocities0], [Gradient| Gradients], Step, Regularization, Momentum, [Weight| Weights], [Velocity| Velocities], MaxDelta0, MaxDelta) :-
		Velocity is Momentum * Velocity0 - Step * (Gradient + Regularization * Weight0),
		Weight is Weight0 + Velocity,
		Delta is abs(Velocity),
		MaxDelta1 is max(MaxDelta0, Delta),
		update_weights(Weights0, Velocities0, Gradients, Step, Regularization, Momentum, Weights, Velocities, MaxDelta1, MaxDelta).

	update_biases([], [], [], _Step, _Momentum, [], [], MaxDelta, MaxDelta).
	update_biases([Bias0| Biases0], [Velocity0| Velocities0], [Gradient| Gradients], Step, Momentum, [Bias| Biases], [Velocity| Velocities], MaxDelta0, MaxDelta) :-
		Velocity is Momentum * Velocity0 - Step * Gradient,
		Bias is Bias0 + Velocity,
		Delta is abs(Velocity),
		MaxDelta1 is max(MaxDelta0, Delta),
		update_biases(Biases0, Velocities0, Gradients, Step, Momentum, Biases, Velocities, MaxDelta1, MaxDelta).

	softmax([Value| Values], Probabilities) :-
		maximum(Values, Value, Maximum),
		exponentials([Value| Values], Maximum, Exponentials, 0.0, Sum),
		normalize(Exponentials, Sum, Probabilities).

	maximum([], Maximum, Maximum).
	maximum([Value| Values], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, Value),
		maximum(Values, Maximum1, Maximum).

	exponentials([], _Maximum, [], Sum, Sum).
	exponentials([Value| Values], Maximum, [Exponential| Exponentials], Sum0, Sum) :-
		Exponential is exp(Value - Maximum),
		Sum1 is Sum0 + Exponential,
		exponentials(Values, Maximum, Exponentials, Sum1, Sum).

	normalize([], _Sum, []).
	normalize([Value| Values], Sum, [Probability| Probabilities]) :-
		Probability is Value / Sum,
		normalize(Values, Sum, Probabilities).

	pair_probabilities([], [], []).
	pair_probabilities([Class| Classes], [Probability| Values], [Class-Probability| Probabilities]) :-
		pair_probabilities(Classes, Values, Probabilities).

	classifier_diagnostics_data(mlp_classifier(_Classes, _Encoders, _Activation, _Layers, Diagnostics), Diagnostics).

	check_classifier(Classifier) :-
		(	classifier_data(Classifier, Classes, Encoders, Activation, Layers, Diagnostics),
			^^valid_class_values(Classes),
			^^valid_linear_encoders(Encoders),
			valid_activation(Activation),
			memberchk(options(Options), Diagnostics),
			catch(::check_options(Options), _Error, fail),
			memberchk(class_weights(ClassWeights), Options),
			valid_class_weight_classes(ClassWeights, Classes),
			^^linear_encoders_feature_count(Encoders, FeatureCount),
			length(Classes, ClassCount),
			memberchk(hidden_layers(HiddenLayers), Diagnostics),
			valid_layers(Layers, [FeatureCount| HiddenLayers], ClassCount) ->
			true
		;	domain_error(classifier, Classifier)
		).

	valid_layers([layer(Weights, Biases)], [InputSize], OutputSize) :-
		!,
		valid_layer(Weights, Biases, InputSize, OutputSize).
	valid_layers([layer(Weights, Biases)| Layers], [InputSize, OutputSize| Sizes], ClassCount) :-
		valid_layer(Weights, Biases, InputSize, OutputSize),
		valid_layers(Layers, [OutputSize| Sizes], ClassCount).

	valid_layer(Weights, Biases, InputSize, OutputSize) :-
		valid(list(float, OutputSize), Biases),
		valid(list(list(float, InputSize), OutputSize), Weights).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(mlp_classifier(_Classes, _Encoders, _Activation, _Layers, _Diagnostics), mlp_classifier('Classes', 'Encoders', 'Activation', 'Layers', 'Diagnostics')).

	classifier_data(Classifier, Classes, Encoders, Activation, Layers, Diagnostics) :-
		Classifier =.. [_Functor, Classes, Encoders, Activation, Layers, Diagnostics].

	print_classifier(Classifier) :-
		classifier_data(Classifier, Classes, Encoders, Activation, Layers, Diagnostics),
		format('Multi-Layer Perceptron Classifier~n', []),
		format('=================================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n', [Classes]),
		format('Activation: ~w~n', [Activation]),
		format('Encoders: ~w~n', [Encoders]),
		format('Layers: ~w~n', [Layers]),
		format('Diagnostics: ~w~n', [Diagnostics]).

	default_option(hidden_layers([8])).
	default_option(activation(relu)).
	default_option(class_weights([])).
	default_option(convergence_criterion(parameter_update)).
	default_option(learning_rate(0.05)).
	default_option(learning_schedule(inverse_scaling(0.25))).
	default_option(maximum_iterations(500)).
	default_option(momentum(0.0)).
	default_option(tolerance(1.0e-6)).
	default_option(l2_regularization(0.0001)).
	default_option(feature_scaling(true)).
	default_option(random_seed(42)).
	default_option(shuffle(true)).

	valid_option(hidden_layers(HiddenLayers)) :-
		valid(list(positive_integer), HiddenLayers).
	valid_option(activation(Activation)) :-
		ground(Activation),
		valid_activation(Activation).
	valid_option(class_weights(ClassWeights)) :-
		ground(ClassWeights),
		valid_class_weights(ClassWeights, []).
	valid_option(convergence_criterion(ConvergenceCriterion)) :-
		once((ConvergenceCriterion == parameter_update; ConvergenceCriterion == loss)).
	valid_option(learning_rate(Rate)) :-
		number(Rate),
		Rate > 0.0.
	valid_option(learning_schedule(LearningSchedule)) :-
		(	LearningSchedule == constant ->
			true
		;	LearningSchedule = inverse_scaling(Power),
			number(Power),
			Power > 0.0
		).
	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(momentum(Momentum)) :-
		number(Momentum),
		Momentum >= 0.0,
		Momentum < 1.0.
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(l2_regularization(Regularization)) :-
		number(Regularization),
		Regularization >= 0.0.
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(random_seed(RandomSeed)) :-
		valid(positive_integer, RandomSeed).
	valid_option(shuffle(Shuffle)) :-
		valid(boolean, Shuffle).

	valid_class_weights([], _SeenClasses).
	valid_class_weights([Class-Weight| ClassWeights], SeenClasses) :-
		atom(Class),
		number(Weight),
		Weight > 0.0,
		\+ member(Class, SeenClasses),
		valid_class_weights(ClassWeights, [Class| SeenClasses]).

	check_class_weight_classes([], _Classes).
	check_class_weight_classes([Class-_Weight| ClassWeights], Classes) :-
		(	member(Class, Classes) ->
			true
		;	domain_error(class_weight_class, Class)
		),
		check_class_weight_classes(ClassWeights, Classes).

	valid_class_weight_classes([], _Classes).
	valid_class_weight_classes([Class-_Weight| ClassWeights], Classes) :-
		memberchk(Class, Classes),
		valid_class_weight_classes(ClassWeights, Classes).

	valid_activation(relu).
	valid_activation(tanh).
	valid_activation(sigmoid).

:- end_object.
