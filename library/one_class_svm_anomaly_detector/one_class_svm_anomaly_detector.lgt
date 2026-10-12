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


:- object(one_class_svm_anomaly_detector,
	imports(anomaly_detector_common)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'One-class support vector machine anomaly detector with linear, polynomial, and radial basis function kernels. Learns from baseline training examples selected from a dataset object implementing the ``anomaly_dataset_protocol`` protocol.',
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, kernel_svm_classifier, linear_svm_classifier]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(linear_algebra, [
		dot_product/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, msort/2, nth1/3, reverse/2
	]).

	:- uses(numberlist, [
		max/2, min/2, sum/2
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

	learn(Dataset, Detector, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^baseline_training_examples(Dataset, Examples, Options),
		^^check_examples_non_empty(Dataset, Examples),
		^^dataset_attributes(Dataset, Attributes),
		^^option(feature_scaling(FeatureScaling), Options),
		build_encoders(Attributes, Examples, FeatureScaling, Encoders),
		examples_to_rows(Examples, Encoders, Rows),
		keys(Rows, TrainingRows),
		^^option(kernel(Kernel), Options),
		learn_coefficients(Kernel, TrainingRows, Options, Coefficients0, Iterations, FinalDelta, OptimizationDiagnostics, Problem),
		^^option(support_vector_tolerance(SupportVectorTolerance), Options),
		select_support_vectors(TrainingRows, Coefficients0, SupportVectorTolerance, SupportVectors, Coefficients),
		effective_coefficient_vector(Coefficients0, SupportVectorTolerance, EffectiveCoefficients),
		build_scorer(Kernel, SupportVectors, Coefficients, Scorer),
		training_references(Scorer, Problem, TrainingRows, EffectiveCoefficients, ReferenceScores),
		length(TrainingRows, ExampleCount),
		length(SupportVectors, SupportVectorCount),
		score_geometry(Kernel, SupportVectors, Coefficients, ReferenceScores, Geometry),
		Diagnostics0 = [
			model(one_class_svm_anomaly_detector),
			training_dataset(Dataset),
			kernel(Kernel),
			example_count(ExampleCount),
			support_vectors(SupportVectorCount),
			iterations(Iterations),
			final_delta(FinalDelta),
			decision_geometry(Geometry),
			options(Options)
		],
		append(Diagnostics0, OptimizationDiagnostics, Diagnostics),
		build_rank_index(ReferenceScores, RankIndex),
		Cache = svm_cache(Scorer, ExampleCount, RankIndex),
		Detector = one_class_svm_detector(Encoders, Kernel, SupportVectors, Coefficients, ReferenceScores, Cache, Diagnostics).

	check_anomaly_detector(Detector) :-
		(	\+ ground(Detector) ->
			instantiation_error
		;	detector_data(Detector, Encoders, Kernel, SupportVectors, Coefficients, ReferenceScores, Cache, Diagnostics),
			valid_encoders(Encoders),
			valid_kernel(Kernel),
			valid_training_rows(SupportVectors, Encoders),
			length(SupportVectors, SupportVectorCount),
			SupportVectorCount > 0,
			valid(list(float, SupportVectorCount), Coefficients),
			length(ReferenceScores, ExampleCount),
			ExampleCount > 0,
			valid(list(float, ExampleCount), ReferenceScores),
			valid_diagnostics(Diagnostics, Kernel, ExampleCount, SupportVectorCount),
			valid_coefficients(Coefficients, Diagnostics, ExampleCount),
			valid_cache(Cache, Kernel, SupportVectors, Coefficients, ReferenceScores, ExampleCount) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(one_class_svm_detector(_Encoders, _Kernel, _TrainingRows, _Coefficients, _ReferenceScores, _Cache, Diagnostics), Diagnostics).

	score(Detector, Instance, Score) :-
		detector_data(Detector, Encoders, Kernel, TrainingRows, Coefficients, _ReferenceScores, Cache, _Diagnostics),
		Cache = svm_cache(Scorer, Count, RankIndex),
		encode_instance(Encoders, Instance, Features),
		cached_raw_score(Scorer, TrainingRows, Coefficients, Features, Kernel, RawScore),
		empirical_anomaly_score(RankIndex, Count, RawScore, Score).

	score_all(Dataset, Detector, Scores) :-
		findall(
			Score-Id-Class,
			(
				Dataset::example(Id, Class, Instance),
				score(Detector, Instance, Score)
			),
			Pairs
		),
		msort(Pairs, Ascending),
		reverse(Ascending, Descending),
		^^extract_scores(Descending, Scores).

	export_to_clauses(_Dataset, Detector, Functor, [Clause]) :-
		Clause =.. [Functor, Detector].

	print_anomaly_detector(Detector) :-
		detector_data(Detector, Encoders, Kernel, SupportVectors, Coefficients, _ReferenceScores, Cache, Diagnostics),
		length(SupportVectors, SupportVectorCount),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		format('One-Class SVM Anomaly Detector~n', []),
		format('==============================~n~n', []),
		^^print_anomaly_detector_template(Detector),
		format('Kernel:            ~w~n', [Kernel]),
		format('Training examples: ~w~n', [ExampleCount]),
		format('Support vectors:   ~w~n', [SupportVectorCount]),
		format('Options:           ~w~n', [Options]),
		format('Encoders:          ~w~n', [Encoders]),
		format('Coefficients:      ~w~n', [Coefficients]),
		format('Cache:             ~w~n', [Cache]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(one_class_svm_detector(_Encoders, _Kernel, _TrainingRows, _Coefficients, _ReferenceScores, _Cache, _Diagnostics), one_class_svm_detector('Encoders', 'Kernel', 'TrainingRows', 'Coefficients', 'ReferenceScores', 'Cache', 'Diagnostics')).

	detector_data(one_class_svm_detector(Encoders, Kernel, TrainingRows, Coefficients, ReferenceScores, Cache, Diagnostics), Encoders, Kernel, TrainingRows, Coefficients, ReferenceScores, Cache, Diagnostics).

	learn_coefficients(linear, Rows, Options, Coefficients, Iterations, Delta, Diagnostics, linear(Rows)) :-
		!,
		length(Rows, Count),
		linear_spectral_bound(Rows, 0.0, Bound),
		train_problem(linear(Rows), Bound, Count, Options, Coefficients, Iterations, Delta, Diagnostics).
	learn_coefficients(Kernel, Rows, Options, Coefficients, Iterations, Delta, Diagnostics, dense(Matrix)) :-
		build_kernel_matrix(Rows, Kernel, Matrix),
		train_model(Matrix, Options, Coefficients, Iterations, Delta, Diagnostics).

	linear_spectral_bound([], Bound, Bound).
	linear_spectral_bound([Row| Rows], Bound0, Bound) :-
		dot_product(Row, Row, SquaredNorm),
		Bound1 is Bound0 + SquaredNorm,
		linear_spectral_bound(Rows, Bound1, Bound).

	build_scorer(linear, Rows, Coefficients, linear_weights(Weights)) :-
		!,
		linear_weights(Rows, Coefficients, Weights).
	build_scorer(_Kernel, _Rows, _Coefficients, kernel_sum).

	training_references(linear_weights(Weights), _Problem, Rows, _Coefficients, References) :-
		matrix_vector_product(Rows, Weights, References).
	training_references(kernel_sum, dense(Matrix), _Rows, Coefficients, References) :-
		matrix_vector_product(Matrix, Coefficients, References).

	cached_raw_score(linear_weights(Weights), _Rows, _Coefficients, Features, _Kernel, Score) :-
		dot_product(Weights, Features, Score).
	cached_raw_score(kernel_sum, Rows, Coefficients, Features, Kernel, Score) :-
		weighted_kernel_score(Rows, Coefficients, Features, Kernel, 0.0, Score).

	valid_cache(svm_cache(Scorer, Count, RankIndex), Kernel, Rows, Coefficients, References, Count) :-
		build_scorer(Kernel, Rows, Coefficients, Expected),
		Scorer == Expected,
		build_rank_index(References, ExpectedIndex),
		RankIndex == ExpectedIndex.

	build_encoders([], _Examples, _FeatureScaling, []).
	build_encoders([Attribute-Values| Attributes], Examples, FeatureScaling, [Encoder| Encoders]) :-
		(	Values == continuous ->
			continuous_encoder(Attribute, Examples, FeatureScaling, Encoder)
		;	Encoder = categorical(Attribute, Values)
		),
		build_encoders(Attributes, Examples, FeatureScaling, Encoders).

	continuous_encoder(Attribute, Examples, true, continuous(Attribute, Mean, Scale)) :-
		!,
		known_attribute_values(Examples, Attribute, Values),
		(	Values == [] ->
			Mean = 0.0,
			Scale = 1.0
		;	arithmetic_mean(Values, Mean),
			length(Values, Count),
			(	Count > 1 ->
				variance(Values, Variance)
			;	Variance = 0.0
			),
			(	Variance > 0.0 ->
				Scale is sqrt(Variance)
			;	Scale = 1.0
			)
		).
	continuous_encoder(Attribute, _Examples, false, continuous(Attribute, 0.0, 1.0)).

	known_attribute_values([], _Attribute, []).
	known_attribute_values([_-_-AttributeValues| Examples], Attribute, Values) :-
		(	memberchk(Attribute-Value, AttributeValues),
			nonvar(Value),
			number(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		known_attribute_values(Examples, Attribute, Rest).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([_-Class-Instance| Examples], Encoders, [Features-Class| Rows]) :-
		encode_instance(Encoders, Instance, Features),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance([], _Instance, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], Instance, [Feature, Missing| Features]) :-
		!,
		(	member(Attribute-Value, Instance), nonvar(Value) ->
			(	number(Value) ->
				true
			;	type_error(number, Value)
			),
			Feature is (Value - Mean) / Scale,
			Missing = 0.0
		;	Feature = 0.0,
			Missing = 1.0
		),
		encode_instance(Encoders, Instance, Features).
	encode_instance([categorical(Attribute, Values)| Encoders], Instance, Features) :-
		(	member(Attribute-Value, Instance), nonvar(Value) ->
			(	member(Value, Values) ->
				one_hot_encode(Values, Value, Encoded)
			; missing_one_hot_encode(Values, Encoded)
			)
		;	missing_one_hot_encode(Values, Encoded)
		),
		append(Encoded, RestFeatures, Features),
		encode_instance(Encoders, Instance, RestFeatures).

	one_hot_encode([], _Value, [0.0]).
	one_hot_encode([Category| Categories], Value, [Feature| Features]) :-
		(	Value == Category ->
			Feature = 1.0
		;	Feature = 0.0
		),
		one_hot_encode(Categories, Value, Features).

	missing_one_hot_encode([], [1.0]).
	missing_one_hot_encode([_| Values], [0.0| Features]) :-
		missing_one_hot_encode(Values, Features).

	build_kernel_matrix(Rows, Kernel, Matrix) :-
		length(Rows, Count),
		matrix_skeleton(Count, Count, Matrix),
		fill_symmetric_rows(Rows, Matrix, Kernel).

	matrix_skeleton(0, _Columns, []) :-
		!.
	matrix_skeleton(Count, Columns, [Row| Rows]) :-
		length(Row, Columns),
		Remaining is Count - 1,
		matrix_skeleton(Remaining, Columns, Rows).

	fill_symmetric_rows([], [], _Kernel).
	fill_symmetric_rows([Row| Rows], [[Diagonal| Upper]| Matrix], Kernel) :-
		kernel_value(Kernel, Row, Row, Diagonal),
		fill_symmetric_pairs(Rows, Row, Upper, Matrix, Tails, Kernel),
		fill_symmetric_rows(Rows, Tails, Kernel).

	fill_symmetric_pairs([], _Row, [], [], [], _Kernel).
	fill_symmetric_pairs([Other| Rows], Row, [Value| Upper], [[Value| Tail]| Matrix], [Tail| Tails], Kernel) :-
		kernel_value(Kernel, Row, Other, Value),
		fill_symmetric_pairs(Rows, Row, Upper, Matrix, Tails, Kernel).

	kernel_value(linear, Row1, Row2, Value) :-
		dot_product(Row1, Row2, Value).
	kernel_value(polynomial(Degree, Gamma, Coef0), Row1, Row2, Value) :-
		dot_product(Row1, Row2, DotProduct),
		Base is Gamma * DotProduct + Coef0,
		Value is Base ** Degree.
	kernel_value(rbf(Gamma), Row1, Row2, Value) :-
		squared_distance(Row1, Row2, 0.0, SquaredDistance),
		Value is exp(-Gamma * SquaredDistance).

	squared_distance([], [], Distance, Distance).
	squared_distance([Value1| Values1], [Value2| Values2], Distance0, Distance) :-
		Difference is Value1 - Value2,
		Distance1 is Distance0 + Difference * Difference,
		squared_distance(Values1, Values2, Distance1, Distance).

	weighted_kernel_score([], [], _Features, _Kernel, Score, Score).
	weighted_kernel_score([Row| Rows], [Coefficient| Coefficients], Features, Kernel, Score0, Score) :-
		kernel_value(Kernel, Row, Features, Value),
		Score1 is Score0 + Coefficient * Value,
		weighted_kernel_score(Rows, Coefficients, Features, Kernel, Score1, Score).

	train_model(KernelMatrix, Options, Coefficients, Iterations, FinalDelta, Diagnostics) :-
		length(KernelMatrix, Count),
		matrix_spectral_bound(KernelMatrix, 0.0, Bound),
		train_problem(dense(KernelMatrix), Bound, Count, Options, Coefficients, Iterations, FinalDelta, Diagnostics).

	train_problem(Problem, Bound, Count, Options, Coefficients, Iterations, FinalDelta, Diagnostics) :-
		Initial is 1.0 / Count,
		constant_vector(Count, Initial, Coefficients0),
		^^option(nu(Nu), Options),
		UpperBound is 1.0 / (Nu * Count),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		^^option(optimality_tolerance(OptimalityTolerance), Options),
		^^option(learning_rate(BaseLearningRate), Options),
		^^option(learning_schedule(Schedule), Options),
		(	Bound > 0.0 ->
			StepLimit is 1.0 / Bound
		;	StepLimit = BaseLearningRate
		),
		Config = optimizer(MaximumIterations, Tolerance, OptimalityTolerance, BaseLearningRate, Schedule),
		problem_gradient(Problem, Coefficients0, Gradient0),
		optimize_model(Problem, UpperBound, Config, StepLimit, 0, Coefficients0, Gradient0, Coefficients, Iterations, FinalDelta, Residual, Reason),
		(	Reason == converged ->
			Converged = true
		;	Converged = false
		),
		Diagnostics = [converged(Converged), stopping_reason(Reason), optimality_residual(Residual), step_limit(StepLimit)].

	optimize_model(Problem, UpperBound, Config, StepLimit, Iteration, Coefficients0, Gradient0, Coefficients, Iterations, FinalDelta, Residual, Reason) :-
		Config = optimizer(MaximumIterations, Tolerance, OptimalityTolerance, _, _),
		learning_rate_for_iteration(Config, Iteration, RequestedRate),
		LearningRate is min(RequestedRate, StepLimit),
		gradient_step(Coefficients0, Gradient0, LearningRate, Candidate),
		project_capped_simplex(Candidate, UpperBound, Coefficients1),
		problem_gradient(Problem, Coefficients1, Gradient1),
		maximum_delta(Coefficients0, Coefficients1, 0.0, Delta),
		optimality_residual(Coefficients1, Gradient1, UpperBound, Residual1),
		NextIteration is Iteration + 1,
		(	(Delta =< Tolerance, Residual1 =< OptimalityTolerance) ->
			Coefficients = Coefficients1,
			Iterations = NextIteration,
			FinalDelta = Delta,
			Residual = Residual1,
			Reason = converged
		;	NextIteration >= MaximumIterations ->
			Coefficients = Coefficients1,
			Iterations = NextIteration,
			FinalDelta = Delta,
			Residual = Residual1,
			Reason = iteration_limit
		;	optimize_model(Problem, UpperBound, Config, StepLimit, NextIteration, Coefficients1, Gradient1, Coefficients, Iterations, FinalDelta, Residual, Reason)
		).

	learning_rate_for_iteration(optimizer(_, _, _, BaseLearningRate, Schedule), Iteration, LearningRate) :-
		(	Schedule == constant ->
			LearningRate = BaseLearningRate
		;	Schedule = inverse_scaling(Power),
			LearningRate is BaseLearningRate / ((Iteration + 1.0) ** Power)
		).

	problem_gradient(dense(Matrix), Coefficients, Gradient) :-
		matrix_vector_product(Matrix, Coefficients, Gradient).
	problem_gradient(linear(Rows), Coefficients, Gradient) :-
		linear_weights(Rows, Coefficients, Weights),
		matrix_vector_product(Rows, Weights, Gradient).

	matrix_spectral_bound([], Bound, Bound).
	matrix_spectral_bound([Row| Rows], Bound0, Bound) :-
		absolute_sum(Row, 0.0, Total),
		Bound1 is max(Bound0, Total),
		matrix_spectral_bound(Rows, Bound1, Bound).

	absolute_sum([], Total, Total).
	absolute_sum([Value| Values], Total0, Total) :-
		Total1 is Total0 + abs(Value),
		absolute_sum(Values, Total1, Total).

	optimality_residual(Coefficients, Gradient, UpperBound, Residual) :-
		gradient_extrema(Coefficients, Gradient, UpperBound, none, none, Donor, Recipient),
		(	(Donor == none; Recipient == none) ->
			Residual = 0.0
		;	Residual is max(0.0, Donor - Recipient)
		).

	gradient_extrema([], [], _UpperBound, Donor, Recipient, Donor, Recipient).
	gradient_extrema([Coefficient| Coefficients], [Gradient| Gradients], UpperBound, Donor0, Recipient0, Donor, Recipient) :-
		(	Coefficient > 1.0e-12 ->
			(	Donor0 == none ->
				Donor1 = Gradient
			;	Donor1 is max(Donor0, Gradient)
			)
		;	Donor1 = Donor0
		),
		(	Coefficient < UpperBound - 1.0e-12 ->
			(	Recipient0 == none ->
				Recipient1 = Gradient
			;	Recipient1 is min(Recipient0, Gradient)
			)
		;	Recipient1 = Recipient0
		),
		gradient_extrema(Coefficients, Gradients, UpperBound, Donor1, Recipient1, Donor, Recipient).

	score_geometry(linear, Rows, Coefficients, References, Geometry) :-
		!,
		linear_weights(Rows, Coefficients, Weights),
		(	zero_vector(Weights) ->
			Geometry = zero_linear
		;	reference_geometry(References, Geometry)
		).
	score_geometry(_Kernel, _Rows, _Coefficients, References, Geometry) :-
		reference_geometry(References, Geometry).

	zero_vector([]).
	zero_vector([Value| Values]) :-
		Value =:= 0,
		zero_vector(Values).

	reference_geometry(References, Geometry) :-
		min(References, Minimum),
		max(References, Maximum),
		(	Minimum =:= Maximum ->
			Geometry = constant_reference
		;	Geometry = varying_reference
		).

	linear_weights([Row| Rows], [Coefficient| Coefficients], Weights) :-
		length(Row, Count),
		constant_vector(Count, 0.0, Zero),
		weighted_add(Row, Coefficient, Zero, Initial),
		linear_weights(Rows, Coefficients, Initial, Weights).

	linear_weights([], [], Weights, Weights).
	linear_weights([Row| Rows], [Coefficient| Coefficients], Weights0, Weights) :-
		weighted_add(Row, Coefficient, Weights0, Weights1),
		linear_weights(Rows, Coefficients, Weights1, Weights).

	weighted_add([], _Coefficient, [], []).
	weighted_add([Value| Values], Coefficient, [Weight| Weights], [Updated| Updates]) :-
		Updated is Weight + Coefficient * Value,
		weighted_add(Values, Coefficient, Weights, Updates).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	gradient_step([], [], _LearningRate, []).
	gradient_step([Coefficient| Coefficients], [Gradient| Gradients], LearningRate, [Candidate| Candidates]) :-
		Candidate is Coefficient - LearningRate * Gradient,
		gradient_step(Coefficients, Gradients, LearningRate, Candidates).

	project_capped_simplex(Values, UpperBound, Projection) :-
		length(Values, Count),
		(	Count * UpperBound =< 1.0 + 1.0e-14 ->
			Uniform is 1.0 / Count,
			constant_vector(Count, Uniform, Projection)
		;	Cap is min(UpperBound, 1.0),
			ActiveCount is min(Count, ceiling(1.0 / Cap)),
			AnchorIndex is Count - ActiveCount + 1,
			msort(Values, Sorted),
			nth1(AnchorIndex, Sorted, Anchor),
			center_projection_values(Values, Anchor, Centered),
			Low is -Cap,
			project_bisection(Centered, Cap, Low, 0.0, 0, Projection)
		).

	center_projection_values([], _Anchor, []).
	center_projection_values([Value| Values], Anchor, [Centered| Rest]) :-
		Centered is Value - Anchor,
		center_projection_values(Values, Anchor, Rest).

	project_bisection(Values, UpperBound, Low, High, Iteration, Projection) :-
		Lambda is Low / 2.0 + High / 2.0,
		projection_total(Values, Lambda, UpperBound, 0.0, Total),
		Residual is 1.0 - Total,
		(	(abs(Residual) =< 1.0e-12; Iteration >= 80; Lambda =:= Low; Lambda =:= High) ->
			clamp_projection(Values, Lambda, UpperBound, Candidate),
			repair_projection(Values, UpperBound, Lambda, Candidate, Residual, Projection)
		;	NextIteration is Iteration + 1,
			(	Total > 1.0 ->
				project_bisection(Values, UpperBound, Lambda, High, NextIteration, Projection)
			;	project_bisection(Values, UpperBound, Low, Lambda, NextIteration, Projection)
			)
		).

	projection_total([], _Lambda, _UpperBound, Total, Total).
	projection_total([Value| Values], Lambda, UpperBound, Total0, Total) :-
		Projected is max(0.0, min(UpperBound, Value - Lambda)),
		Total1 is Total0 + Projected,
		projection_total(Values, Lambda, UpperBound, Total1, Total).

	repair_projection(Values, UpperBound, Lambda, Projection0, Residual, Projection) :-
		(	abs(Residual) =< 1.0e-12 ->
			Projection = Projection0
		;	closest_adjustable(Values, Projection0, UpperBound, Lambda, Residual, 1, none, _Distance-Index),
			adjust_projection(Projection0, Index, UpperBound, Residual, Projection1, Applied),
			Remaining is Residual - Applied,
			repair_projection(Values, UpperBound, Lambda, Projection1, Remaining, Projection)
		).

	closest_adjustable([], [], _UpperBound, _Lambda, _Residual, _Index, Best, Best).
	closest_adjustable([Value| Values], [Coefficient| Coefficients], UpperBound, Lambda, Residual, Index, Best0, Best) :-
		(	(Residual > 0.0, Coefficient < UpperBound; Residual < 0.0, Coefficient > 0.0) ->
			Distance is abs(Value - Lambda),
			(	Best0 == none ->
				Best1 = Distance-Index
			;	Best0 = BestDistance-_,
				(	Distance < BestDistance ->
					Best1 = Distance-Index
				;	Best1 = Best0
				)
			)
		;	Best1 = Best0
		),
		NextIndex is Index + 1,
		closest_adjustable(Values, Coefficients, UpperBound, Lambda, Residual, NextIndex, Best1, Best).

	adjust_projection([Coefficient| Coefficients], 1, UpperBound, Residual, [Adjusted| Coefficients], Applied) :-
		!,
		Adjusted is max(0.0, min(UpperBound, Coefficient + Residual)),
		Applied is Adjusted - Coefficient.
	adjust_projection([Coefficient| Coefficients0], Index, UpperBound, Residual, [Coefficient| Coefficients], Applied) :-
		NextIndex is Index - 1,
		adjust_projection(Coefficients0, NextIndex, UpperBound, Residual, Coefficients, Applied).

	clamp_projection([], _Lambda, _UpperBound, []).
	clamp_projection([Value| Values], Lambda, UpperBound, [Projected| Projection]) :-
		Shifted is Value - Lambda,
		(	Shifted < 0.0 ->
			Projected = 0.0
		;	Shifted > UpperBound ->
			Projected = UpperBound
		;	Projected = Shifted
		),
		clamp_projection(Values, Lambda, UpperBound, Projection).

	maximum_delta([], [], Delta, Delta).
	maximum_delta([Value0| Values0], [Value1| Values1], Delta0, Delta) :-
		Difference is abs(Value1 - Value0),
		Delta1 is max(Delta0, Difference),
		maximum_delta(Values0, Values1, Delta1, Delta).

	constant_vector(0, _Value, []) :-
		!.
	constant_vector(Count, Value, [Value| Values]) :-
		NextCount is Count - 1,
		constant_vector(NextCount, Value, Values).

	select_support_vectors(TrainingRows, Coefficients, Tolerance, SupportVectors, SupportCoefficients) :-
		collect_support_vectors(TrainingRows, Coefficients, Tolerance, SupportVectors0, SupportCoefficients0),
		(	SupportVectors0 == [] ->
			maximum_support_vector(TrainingRows, Coefficients, SupportVector, SupportCoefficient),
			SupportVectors = [SupportVector],
			SupportCoefficients = [SupportCoefficient]
		;	SupportVectors = SupportVectors0,
			SupportCoefficients = SupportCoefficients0
		).

	collect_support_vectors([], [], _Tolerance, [], []).
	collect_support_vectors([TrainingRow| TrainingRows], [Coefficient| Coefficients], Tolerance, SupportVectors, SupportCoefficients) :-
		(	Coefficient > Tolerance ->
			SupportVectors = [TrainingRow| RestSupportVectors],
			SupportCoefficients = [Coefficient| RestSupportCoefficients]
		;	SupportVectors = RestSupportVectors,
			SupportCoefficients = RestSupportCoefficients
		),
		collect_support_vectors(TrainingRows, Coefficients, Tolerance, RestSupportVectors, RestSupportCoefficients).

	maximum_support_vector([TrainingRow| TrainingRows], [Coefficient| Coefficients], SupportVector, SupportCoefficient) :-
		maximum_support_vector(TrainingRows, Coefficients, TrainingRow, Coefficient, SupportVector, SupportCoefficient).

	maximum_support_vector([], [], SupportVector, SupportCoefficient, SupportVector, SupportCoefficient).
	maximum_support_vector([TrainingRow| TrainingRows], [Coefficient| Coefficients], SupportVector0, SupportCoefficient0, SupportVector, SupportCoefficient) :-
		(	Coefficient > SupportCoefficient0 ->
			SupportVector1 = TrainingRow,
			SupportCoefficient1 = Coefficient
		;	SupportVector1 = SupportVector0,
			SupportCoefficient1 = SupportCoefficient0
		),
		maximum_support_vector(TrainingRows, Coefficients, SupportVector1, SupportCoefficient1, SupportVector, SupportCoefficient).

	effective_coefficient_vector(Coefficients, Tolerance, Effective) :-
		prune_coefficient_vector(Coefficients, Tolerance, Pruned),
		sum(Pruned, Total),
		(	Total > 0.0 ->
			Effective = Pruned
		;	max(Coefficients, Maximum),
			retain_first_maximum(Coefficients, Maximum, Effective)
		).

	prune_coefficient_vector([], _Tolerance, []).
	prune_coefficient_vector([Coefficient| Coefficients], Tolerance, [Effective| Rest]) :-
		(	Coefficient > Tolerance ->
			Effective = Coefficient
		;	Effective = 0.0
		),
		prune_coefficient_vector(Coefficients, Tolerance, Rest).

	retain_first_maximum([Coefficient| Coefficients], Maximum, [Coefficient| Zeroes]) :-
		Coefficient =:= Maximum,
		!,
		length(Coefficients, Count),
		constant_vector(Count, 0.0, Zeroes).
	retain_first_maximum([_| Coefficients], Maximum, [0.0| Effective]) :-
		retain_first_maximum(Coefficients, Maximum, Effective).

	empirical_anomaly_score(RankIndex, Count, RawScore, Score) :-
		rank_greater(RankIndex, RawScore, 0, GreaterCount),
		Score is float(GreaterCount / Count).

	build_rank_index(References, Index) :-
		msort(References, Sorted),
		group_reference_scores(Sorted, Groups),
		length(Groups, Count),
		rank_tree(Count, Groups, [], Index).

	group_reference_scores([], []).
	group_reference_scores([Value| Values], [Value-Count| Groups]) :-
		count_equal_values(Values, Value, 1, Count, Rest),
		group_reference_scores(Rest, Groups).

	count_equal_values([Value| Values], Key, Count0, Count, Rest) :-
		Value =:= Key,
		!,
		Count1 is Count0 + 1,
		count_equal_values(Values, Key, Count1, Count, Rest).
	count_equal_values(Values, _Key, Count, Count, Values).

	rank_tree(0, Groups, Groups, rank_empty) :-
		!.
	rank_tree(Count, Groups0, Groups, rank_node(Key, Multiplicity, Total, Left, Right)) :-
		LeftCount is (Count - 1) div 2,
		RightCount is Count - 1 - LeftCount,
		rank_tree(LeftCount, Groups0, [Key-Multiplicity| Groups1], Left),
		rank_tree(RightCount, Groups1, Groups, Right),
		rank_size(Left, LeftSize),
		rank_size(Right, RightSize),
		Total is LeftSize + Multiplicity + RightSize.

	rank_size(rank_empty, 0).
	rank_size(rank_node(_Key, _Multiplicity, Total, _Left, _Right), Total).

	rank_greater(rank_empty, _Raw, Count, Count).
	rank_greater(rank_node(Key, Multiplicity, _Total, Left, Right), Raw, Count0, Count) :-
		(	Key > Raw ->
			rank_size(Right, RightSize),
			Count1 is Count0 + Multiplicity + RightSize,
			rank_greater(Left, Raw, Count1, Count)
		;	rank_greater(Right, Raw, Count0, Count)
		).

	valid_encoders(Encoders) :-
		valid(list(compound), Encoders),
		Encoders \== [],
		valid_encoders(Encoders, []).

	valid_encoders([], _SeenAttributes).
	valid_encoders([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		atom(Attribute),
		\+ member(Attribute, SeenAttributes),
		valid(float, Mean),
		valid(float, Scale),
		Scale > 0.0,
		valid_encoders(Encoders, [Attribute| SeenAttributes]).
	valid_encoders([categorical(Attribute, Values)| Encoders], SeenAttributes) :-
		atom(Attribute),
		\+ member(Attribute, SeenAttributes),
		valid(list(atom), Values),
		Values \== [],
		unique_values(Values),
		valid_encoders(Encoders, [Attribute| SeenAttributes]).

	unique_values([]).
	unique_values([Value| Values]) :-
		\+ member(Value, Values),
		unique_values(Values).

	valid_coefficients(Coefficients, Diagnostics, Count) :-
		memberchk(options(UserOptions), Diagnostics),
		^^merge_options(UserOptions, Options),
		^^option(nu(Nu), Options),
		UpperBound is 1.0 / (Nu * Count),
		valid_coefficient_bounds(Coefficients, UpperBound),
		sum(Coefficients, Total),
		Total > 0.0,
		Total =< 1.0 + 1.0e-10.

	valid_coefficient_bounds([], _UpperBound).
	valid_coefficient_bounds([Coefficient| Coefficients], UpperBound) :-
		Coefficient >= 0.0,
		Coefficient =< UpperBound + 1.0e-10,
		valid_coefficient_bounds(Coefficients, UpperBound).

	valid_training_rows(TrainingRows, Encoders) :-
		encoded_feature_count(Encoders, FeatureCount),
		valid(list(list(float, FeatureCount)), TrainingRows).

	encoded_feature_count([], 0).
	encoded_feature_count([continuous(_, _, _)| Encoders], Count) :-
		!,
		encoded_feature_count(Encoders, Rest),
		Count is Rest + 2.
	encoded_feature_count([categorical(_, Values)| Encoders], Count) :-
		length(Values, ValueCount),
		encoded_feature_count(Encoders, Rest),
		Count is Rest + ValueCount + 1.

	valid_diagnostics(Diagnostics, Kernel, ExampleCount, SupportVectorCount) :-
		valid(list(compound), Diagnostics),
		SupportVectorCount =< ExampleCount,
		memberchk(model(one_class_svm_anomaly_detector), Diagnostics),
		memberchk(kernel(Kernel), Diagnostics),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(support_vectors(SupportVectorCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(^^check_options(Options), _Error, fail),
		^^merge_options(Options, EffectiveOptions),
		^^option(kernel(RecordedKernel), EffectiveOptions),
		RecordedKernel == Kernel,
		memberchk(training_dataset(Dataset), Diagnostics),
		valid(object_identifier, Dataset),
		memberchk(iterations(Iterations), Diagnostics),
		valid(positive_integer, Iterations),
		^^option(maximum_iterations(MaximumIterations), EffectiveOptions),
		Iterations =< MaximumIterations,
		memberchk(final_delta(Delta), Diagnostics),
		valid(non_negative_number, Delta),
		memberchk(optimality_residual(Residual), Diagnostics),
		valid(non_negative_number, Residual),
		memberchk(step_limit(StepLimit), Diagnostics),
		valid(positive_number, StepLimit),
		memberchk(converged(Converged), Diagnostics),
		memberchk(stopping_reason(Reason), Diagnostics),
		(	Converged == true ->
			Reason == converged,
			^^option(tolerance(Tolerance), EffectiveOptions),
			^^option(optimality_tolerance(OptimalityTolerance), EffectiveOptions),
			Delta =< Tolerance,
			Residual =< OptimalityTolerance
		;	Converged == false,
			Reason == iteration_limit,
			Iterations =:= MaximumIterations
		),
		memberchk(decision_geometry(Geometry), Diagnostics),
		(	Geometry == zero_linear ->
			Kernel == linear
		;	member(Geometry, [constant_reference, varying_reference])
		).

	valid_kernel(Kernel) :-
		ground(Kernel),
		valid_ground_kernel(Kernel).

	valid_ground_kernel(linear).
	valid_ground_kernel(polynomial(Degree, Gamma, Coef0)) :-
		integer(Degree),
		Degree > 0,
		number(Gamma),
		Gamma > 0,
		number(Coef0),
		Coef0 >= 0.
	valid_ground_kernel(rbf(Gamma)) :-
		number(Gamma),
		Gamma > 0.

	default_option(kernel(rbf(0.5))).
	default_option(nu(0.1)).
	default_option(learning_rate(0.1)).
	default_option(learning_schedule(constant)).
	default_option(support_vector_tolerance(0.0)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-6)).
	default_option(optimality_tolerance(1.0e-6)).
	default_option(feature_scaling(true)).
	default_option(anomaly_threshold(0.95)).
	default_option(baseline_class_values([normal])).
	default_option(baseline_selection_policy(reject)).

	valid_option(kernel(Kernel)) :-
		valid_kernel(Kernel).
	valid_option(nu(Nu)) :-
		number(Nu),
		Nu > 0.0,
		Nu =< 1.0.
	valid_option(learning_rate(LearningRate)) :-
		number(LearningRate),
		LearningRate > 0.0.
	valid_option(learning_schedule(LearningSchedule)) :-
		(	LearningSchedule == constant ->
			true
		;	LearningSchedule = inverse_scaling(Power),
			number(Power),
			Power > 0.0
		).
	valid_option(support_vector_tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(maximum_iterations(Iterations)) :-
		valid(positive_integer, Iterations).
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(optimality_tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance > 0.0.
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == true; FeatureScaling == false)).
	valid_option(anomaly_threshold(Threshold)) :-
		valid(probability, Threshold).
	valid_option(baseline_class_values(BaselineClassValues)) :-
		^^valid_baseline_class_values(BaselineClassValues).
	valid_option(baseline_selection_policy(Policy)) :-
		once((Policy == reject; Policy == filter)).

:- end_object.
