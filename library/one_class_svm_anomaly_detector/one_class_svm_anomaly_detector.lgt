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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-09,
		comment is 'One-class support vector machine anomaly detector with linear, polynomial, and radial basis function kernels. Learns from baseline training examples selected from a dataset object implementing the ``anomaly_dataset_protocol`` protocol.',
		see_also is [anomaly_dataset_protocol, anomaly_detector_protocol, kernel_svm_classifier, linear_svm_classifier]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(linear_algebra, [
		dot_product/3, subtract_vectors/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, msort/2, reverse/2
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
		build_kernel_matrix(TrainingRows, Kernel, KernelMatrix),
		train_model(KernelMatrix, Options, Coefficients0, Iterations, FinalDelta),
		^^option(support_vector_tolerance(SupportVectorTolerance), Options),
		select_support_vectors(TrainingRows, Coefficients0, SupportVectorTolerance, SupportVectors, Coefficients),
		training_raw_scores(TrainingRows, SupportVectors, Kernel, Coefficients, ReferenceScores),
		length(TrainingRows, ExampleCount),
		length(SupportVectors, SupportVectorCount),
		Diagnostics = [
			model(one_class_svm_anomaly_detector),
			training_dataset(Dataset),
			kernel(Kernel),
			example_count(ExampleCount),
			support_vectors(SupportVectorCount),
			iterations(Iterations),
			final_delta(FinalDelta),
			options(Options)
		],
		Detector = one_class_svm_detector(Encoders, Kernel, SupportVectors, Coefficients, ReferenceScores, Diagnostics).

	check_anomaly_detector(Detector) :-
		(	detector_data(Detector, Encoders, Kernel, SupportVectors, Coefficients, ReferenceScores, Diagnostics),
			valid_encoders(Encoders),
			valid_kernel(Kernel),
			valid_training_rows(SupportVectors, Encoders),
			length(SupportVectors, SupportVectorCount),
			SupportVectorCount > 0,
			valid(list(float, SupportVectorCount), Coefficients),
			length(ReferenceScores, ExampleCount),
			ExampleCount > 0,
			valid(list(float, ExampleCount), ReferenceScores),
			valid_diagnostics(Diagnostics, Kernel, ExampleCount, SupportVectorCount) ->
			true
		;	domain_error(anomaly_detector, Detector)
		).

	anomaly_detector_diagnostics_data(one_class_svm_detector(_Encoders, _Kernel, _TrainingRows, _Coefficients, _ReferenceScores, Diagnostics), Diagnostics).

	score(Detector, Instance, Score) :-
		detector_data(Detector, Encoders, Kernel, TrainingRows, Coefficients, ReferenceScores, _Diagnostics),
		encode_instance(Encoders, Instance, Features),
		kernel_vector(TrainingRows, Features, Kernel, KernelValues),
		dot_product(Coefficients, KernelValues, RawScore),
		empirical_anomaly_score(ReferenceScores, RawScore, Score).

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
		detector_data(Detector, Encoders, Kernel, SupportVectors, Coefficients, _ReferenceScores, Diagnostics),
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
		format('Coefficients:      ~w~n', [Coefficients]).

	anomaly_detector_export_template(Functor, Template) :-
		Template =.. [Functor, 'Detector'].

	anomaly_detector_term_template(one_class_svm_detector(_Encoders, _Kernel, _TrainingRows, _Coefficients, _ReferenceScores, _Diagnostics), one_class_svm_detector('Encoders', 'Kernel', 'TrainingRows', 'Coefficients', 'ReferenceScores', 'Diagnostics')).

	detector_data(Detector, Encoders, Kernel, TrainingRows, Coefficients, ReferenceScores, Diagnostics) :-
		Detector =.. [_Functor, Encoders, Kernel, TrainingRows, Coefficients, ReferenceScores, Diagnostics].

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
		build_kernel_rows(Rows, Rows, Kernel, Matrix).

	build_kernel_rows([], _Rows, _Kernel, []).
	build_kernel_rows([Row| Rows], AllRows, Kernel, [KernelRow| Matrix]) :-
		kernel_vector(AllRows, Row, Kernel, KernelRow),
		build_kernel_rows(Rows, AllRows, Kernel, Matrix).

	kernel_vector([], _Features, _Kernel, []).
	kernel_vector([TrainingRow| TrainingRows], Features, Kernel, [Value| Values]) :-
		kernel_value(Kernel, TrainingRow, Features, Value),
		kernel_vector(TrainingRows, Features, Kernel, Values).

	kernel_value(linear, Row1, Row2, Value) :-
		dot_product(Row1, Row2, Value).
	kernel_value(polynomial(Degree, Gamma, Coef0), Row1, Row2, Value) :-
		dot_product(Row1, Row2, DotProduct),
		Base is Gamma * DotProduct + Coef0,
		Value is Base ** Degree.
	kernel_value(rbf(Gamma), Row1, Row2, Value) :-
		subtract_vectors(Row1, Row2, Difference),
		dot_product(Difference, Difference, SquaredDistance),
		Value is exp(-Gamma * SquaredDistance).

	train_model(KernelMatrix, Options, Coefficients, Iterations, FinalDelta) :-
		length(KernelMatrix, Count),
		Initial is 1.0 / Count,
		constant_vector(Count, Initial, Coefficients0),
		^^option(nu(Nu), Options),
		UpperBound is 1.0 / (Nu * Count),
		optimize_model(KernelMatrix, UpperBound, Options, 0, Coefficients0, Coefficients, Iterations, FinalDelta).

	optimize_model(KernelMatrix, UpperBound, Options, Iteration, Coefficients0, Coefficients, Iterations, FinalDelta) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Iteration >= MaximumIterations ->
			Coefficients = Coefficients0,
			Iterations = Iteration,
			FinalDelta = 0.0
		;	matrix_vector_product(KernelMatrix, Coefficients0, Gradient),
			learning_rate_for_iteration(Options, Iteration, LearningRate),
			gradient_step(Coefficients0, Gradient, LearningRate, Candidate),
			project_capped_simplex(Candidate, UpperBound, Coefficients1),
			maximum_delta(Coefficients0, Coefficients1, 0.0, Delta),
			NextIteration is Iteration + 1,
			^^option(tolerance(Tolerance), Options),
			(	Delta =< Tolerance ->
				Coefficients = Coefficients1,
				Iterations = NextIteration,
				FinalDelta = Delta
			;	NextIteration >= MaximumIterations ->
				Coefficients = Coefficients1,
				Iterations = NextIteration,
				FinalDelta = Delta
			;	optimize_model(KernelMatrix, UpperBound, Options, NextIteration, Coefficients1, Coefficients, Iterations, FinalDelta)
			)
		).

	learning_rate_for_iteration(Options, Iteration, LearningRate) :-
		^^option(learning_rate(BaseLearningRate), Options),
		^^option(learning_schedule(Schedule), Options),
		( Schedule == constant ->
			LearningRate = BaseLearningRate
		; Schedule = inverse_scaling(Power),
			LearningRate is BaseLearningRate / ((Iteration + 1.0) ** Power)
		).

	matrix_vector_product([], _Vector, []).
	matrix_vector_product([Row| Rows], Vector, [Value| Values]) :-
		dot_product(Row, Vector, Value),
		matrix_vector_product(Rows, Vector, Values).

	gradient_step([], [], _LearningRate, []).
	gradient_step([Coefficient| Coefficients], [Gradient| Gradients], LearningRate, [Candidate| Candidates]) :-
		Candidate is Coefficient - LearningRate * Gradient,
		gradient_step(Coefficients, Gradients, LearningRate, Candidates).

	project_capped_simplex(Values, UpperBound, Projection) :-
		min(Values, Minimum),
		max(Values, Maximum),
		Low is Minimum - UpperBound,
		project_bisection(Values, UpperBound, Low, Maximum, 0, Projection).

	project_bisection(Values, UpperBound, Low, High, Iteration, Projection) :-
		(	Iteration >= 50 ->
			Lambda is (Low + High) / 2.0,
			clamp_projection(Values, Lambda, UpperBound, Projection)
		;	Lambda is (Low + High) / 2.0,
			clamp_projection(Values, Lambda, UpperBound, Candidate),
			sum(Candidate, Total),
			NextIteration is Iteration + 1,
			(	Total > 1.0 ->
				project_bisection(Values, UpperBound, Lambda, High, NextIteration, Projection)
			;	project_bisection(Values, UpperBound, Low, Lambda, NextIteration, Projection)
			)
		).

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

	training_raw_scores([], _SupportVectors, _Kernel, _Coefficients, []).
	training_raw_scores([TrainingRow| TrainingRows], SupportVectors, Kernel, Coefficients, [RawScore| RawScores]) :-
		kernel_vector(SupportVectors, TrainingRow, Kernel, KernelValues),
		dot_product(Coefficients, KernelValues, RawScore),
		training_raw_scores(TrainingRows, SupportVectors, Kernel, Coefficients, RawScores).

	empirical_anomaly_score(ReferenceScores, RawScore, Score) :-
		count_greater(ReferenceScores, RawScore, 0, GreaterCount),
		length(ReferenceScores, Count),
		Score is float(GreaterCount / Count).

	count_greater([], _RawScore, Count, Count).
	count_greater([ReferenceScore| ReferenceScores], RawScore, Count0, Count) :-
		(	ReferenceScore > RawScore ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		count_greater(ReferenceScores, RawScore, Count1, Count).

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
		valid_encoders(Encoders, [Attribute| SeenAttributes]).

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
		memberchk(model(one_class_svm_anomaly_detector), Diagnostics),
		memberchk(kernel(Kernel), Diagnostics),
		memberchk(example_count(ExampleCount), Diagnostics),
		memberchk(support_vectors(SupportVectorCount), Diagnostics),
		memberchk(options(Options), Diagnostics),
		catch(^^check_options(Options), _Error, fail).

	valid_kernel(linear).
	valid_kernel(polynomial(Degree, Gamma, Coef0)) :-
		valid(positive_integer, Degree),
		valid(positive_number, Gamma),
		valid(non_negative_number, Coef0).
	valid_kernel(rbf(Gamma)) :-
		valid(positive_number, Gamma).

	default_option(kernel(rbf(0.5))).
	default_option(nu(0.1)).
	default_option(learning_rate(0.1)).
	default_option(learning_schedule(constant)).
	default_option(support_vector_tolerance(0.0)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-6)).
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
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(anomaly_threshold(Threshold)) :-
		valid(probability, Threshold).
	valid_option(baseline_class_values(BaselineClassValues)) :-
		^^valid_baseline_class_values(BaselineClassValues).
	valid_option(baseline_selection_policy(Policy)) :-
		once((Policy == reject; Policy == filter)).

:- end_object.
