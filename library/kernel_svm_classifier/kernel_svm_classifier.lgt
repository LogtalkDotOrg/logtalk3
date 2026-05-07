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


:- object(kernel_svm_classifier,
	imports(probabilistic_classifier_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Kernel support vector machine classifier using one-vs-rest dual margin models with linear, polynomial, and radial basis function kernels.',
		see_also is [dataset_protocol, linear_svm_classifier, logistic_regression_classifier, kernel_pca_projection]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(linear_algebra, [
		dot_product/3, new_vector/3, subtract_vectors/3
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

	:- uses(pairs, [
		keys/2
	]).

	learn(Dataset, Classifier, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		Dataset::class_values(Classes),
		^^option(feature_scaling(FeatureScaling), Options),
		^^build_linear_encoders(Attributes, Examples, FeatureScaling, Encoders),
		^^examples_to_linear_rows(Examples, Encoders, Rows),
		keys(Rows, TrainingRows),
		^^option(kernel(Kernel), Options),
		build_kernel_matrix(TrainingRows, Kernel, GramMatrix),
		train_models(Classes, Rows, GramMatrix, Options, Models),
		Classifier = kernel_svm_classifier(Classes, Encoders, Kernel, TrainingRows, Models, Options).

	predict(Classifier, Instance, Class) :-
		^^predict_from_probabilities(Classifier, Instance, Class).

	predict_probabilities(Classifier, Instance, Probabilities) :-
		Classifier = kernel_svm_classifier(Classes, Encoders, Kernel, TrainingRows, Models, _Options),
		^^encode_linear_instance(Encoders, Instance, Features),
		kernel_vector(TrainingRows, Features, Kernel, KernelValues),
		class_scores(Models, KernelValues, Scores),
		score_pairs(Classes, Scores, ClassScores),
		scores_to_probabilities(ClassScores, Probabilities).

	build_kernel_matrix(Rows, Kernel, KernelMatrix) :-
		build_kernel_rows(Rows, Rows, Kernel, KernelMatrix).

	build_kernel_rows([], _AllRows, _Kernel, []).
	build_kernel_rows([Row| Rows], AllRows, Kernel, [KernelRow| KernelMatrix]) :-
		build_kernel_row(AllRows, Row, Kernel, KernelRow),
		build_kernel_rows(Rows, AllRows, Kernel, KernelMatrix).

	build_kernel_row([], _Row, _Kernel, []).
	build_kernel_row([OtherRow| OtherRows], Row, Kernel, [KernelValue| KernelValues]) :-
		kernel_value(Kernel, Row, OtherRow, KernelValue),
		build_kernel_row(OtherRows, Row, Kernel, KernelValues).

	kernel_vector([], _Features, _Kernel, []).
	kernel_vector([TrainingRow| TrainingRows], Features, Kernel, [KernelValue| KernelValues]) :-
		kernel_value(Kernel, TrainingRow, Features, KernelValue),
		kernel_vector(TrainingRows, Features, Kernel, KernelValues).

	kernel_value(linear, Row1, Row2, KernelValue) :-
		dot_product(Row1, Row2, KernelValue).
	kernel_value(polynomial(Degree, Gamma, Coef0), Row1, Row2, KernelValue) :-
		dot_product(Row1, Row2, DotProduct),
		Base is Gamma * DotProduct + Coef0,
		KernelValue is Base ** Degree.
	kernel_value(rbf(Gamma), Row1, Row2, KernelValue) :-
		subtract_vectors(Row1, Row2, Difference),
		dot_product(Difference, Difference, SquaredDistance),
		KernelValue is exp(-Gamma * SquaredDistance).

	train_models([], _Rows, _GramMatrix, _Options, []).
	train_models([Class| Classes], Rows, GramMatrix, Options, [Model| Models]) :-
		length(Rows, RowCount),
		new_vector(RowCount, 0.0, Coefficients),
		optimize_model(Rows, GramMatrix, Class, Options, 0, class_model(Class, 0.0, Coefficients), Model),
		train_models(Classes, Rows, GramMatrix, Options, Models).

	optimize_model(Rows, GramMatrix, PositiveClass, Options, Epoch, Model0, Model) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Epoch >= MaximumIterations ->
			Model = Model0
		;   learning_rate_for_epoch(Options, Epoch, Step),
			process_rows(Rows, GramMatrix, PositiveClass, Options, Step, 1, Model0, Model1, 0.0, MaxDelta),
			^^option(tolerance(Tolerance), Options),
			(   MaxDelta =< Tolerance ->
				Model = Model1
			;   NextEpoch is Epoch + 1,
				optimize_model(Rows, GramMatrix, PositiveClass, Options, NextEpoch, Model1, Model)
			)
		).

	learning_rate_for_epoch(Options, Epoch, Step) :-
		^^option(learning_rate(LearningRate), Options),
		^^option(learning_schedule(Schedule), Options),
		(   Schedule == constant ->
			Step = LearningRate
		;   Schedule = inverse_scaling(Power) ->
			Step is LearningRate / ((Epoch + 1.0) ** Power)
		;   domain_error(learning_schedule, Schedule)
		).

	process_rows([], _GramMatrix, _PositiveClass, _Options, _Step, _Index, Model, Model, MaxDelta, MaxDelta).
	process_rows([_Features-Label| Rows], [KernelRow| GramMatrix], PositiveClass, Options, Step, Index, Model0, Model, MaxDelta0, MaxDelta) :-
		update_model(Options, PositiveClass, Label, KernelRow, Index, Step, Model0, Model1, Delta),
		MaxDelta1 is max(MaxDelta0, Delta),
		NextIndex is Index + 1,
		process_rows(Rows, GramMatrix, PositiveClass, Options, Step, NextIndex, Model1, Model, MaxDelta1, MaxDelta).

	update_model(Options, PositiveClass, Label, KernelRow, Index, Step, class_model(Class, Bias0, Coefficients0), class_model(Class, Bias, Coefficients), Delta) :-
		target_sign(Label, PositiveClass, Sign),
		dot_product(Coefficients0, KernelRow, DualScore),
		Score is Bias0 + DualScore,
		Margin is Sign * Score,
		^^option(l2_regularization(Regularization), Options),
		decay_coefficients(Coefficients0, Step, Regularization, Coefficients1, 0.0, DecayDelta),
		(   Margin < 1.0 ->
			add_dual_update(Coefficients1, Index, Step * Sign, Coefficients, 0.0, UpdateDelta),
			Bias is Bias0 + Step * Sign,
			BiasDelta is abs(Bias - Bias0),
			Delta is max(BiasDelta, max(DecayDelta, UpdateDelta))
		;   Coefficients = Coefficients1,
			Bias = Bias0,
			Delta = DecayDelta
		),
		Class = PositiveClass.

	decay_coefficients([], _Step, _Regularization, [], MaxDelta, MaxDelta).
	decay_coefficients([Coefficient0| Coefficients0], Step, Regularization, [Coefficient| Coefficients], MaxDelta0, MaxDelta) :-
		Coefficient is Coefficient0 * (1.0 - Step * Regularization),
		Delta is abs(Coefficient - Coefficient0),
		MaxDelta1 is max(MaxDelta0, Delta),
		decay_coefficients(Coefficients0, Step, Regularization, Coefficients, MaxDelta1, MaxDelta).

	add_dual_update([Coefficient0| Coefficients0], 1, Adjustment, [Coefficient| Coefficients0], MaxDelta0, MaxDelta) :-
		!,
		Coefficient is Coefficient0 + Adjustment,
		Delta is abs(Coefficient - Coefficient0),
		MaxDelta is max(MaxDelta0, Delta).
	add_dual_update([Coefficient0| Coefficients0], Index, Adjustment, [Coefficient0| Coefficients], MaxDelta0, MaxDelta) :-
		NextIndex is Index - 1,
		add_dual_update(Coefficients0, NextIndex, Adjustment, Coefficients, MaxDelta0, MaxDelta).

	target_sign(Label, PositiveClass, 1.0) :-
		Label == PositiveClass,
		!.
	target_sign(_, _, -1.0).

	class_scores([], _KernelValues, []).
	class_scores([class_model(_Class, Bias, Coefficients)| Models], KernelValues, [Score| Scores]) :-
		dot_product(Coefficients, KernelValues, DualScore),
		Score is Bias + DualScore,
		class_scores(Models, KernelValues, Scores).

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
		classifier_data(Classifier, Classes, Encoders, Kernel, TrainingRows, Models, Options),
		^^linear_encoders_feature_count(Encoders, EncodedFeatures),
		length(TrainingRows, TrainingCount),
		length(Models, ModelCount),
		Diagnostics = [
			model(kernel_svm_classifier),
			kernel(Kernel),
			classes(Classes),
			encoded_features(EncodedFeatures),
			training_examples(TrainingCount),
			models(ModelCount),
			options(Options)
		].

	check_classifier(Classifier) :-
		(   classifier_data(Classifier, Classes, Encoders, Kernel, TrainingRows, Models, Options),
			^^valid_class_values(Classes),
			^^valid_linear_encoders(Encoders),
			valid_kernel(Kernel),
			catch(::check_options(Options), _Error, fail),
			^^linear_encoders_feature_count(Encoders, EncodedFeatures),
			valid_training_rows(TrainingRows, EncodedFeatures),
			length(TrainingRows, TrainingCount),
			length(Classes, ModelCount),
			length(Models, ModelCount),
			valid_models(Models, Classes, TrainingCount, []) ->
			true
		;   domain_error(classifier, Classifier)
		).

	export_to_clauses(_Dataset, Classifier, Functor, [Clause]) :-
		Clause =.. [Functor, Classifier].

	classifier_export_template(_Dataset, _Classifier, Functor, Template) :-
		Template =.. [Functor, 'Classifier'].

	classifier_term_template(kernel_svm_classifier(_Classes, _Encoders, _Kernel, _TrainingRows, _Models, _Options), kernel_svm_classifier('Classes', 'Encoders', 'Kernel', 'TrainingRows', 'Models', 'Options')).

	valid_training_rows([], _EncodedFeatures).
	valid_training_rows([TrainingRow| TrainingRows], EncodedFeatures) :-
		valid(list(float, EncodedFeatures), TrainingRow),
		valid_training_rows(TrainingRows, EncodedFeatures).

	valid_models([], _Classes, _TrainingCount, _SeenClasses).
	valid_models([class_model(Class, Bias, Coefficients)| Models], Classes, TrainingCount, SeenClasses) :-
		memberchk(Class, Classes),
		\+ member(Class, SeenClasses),
		valid(float, Bias),
		valid(list(float, TrainingCount), Coefficients),
		valid_models(Models, Classes, TrainingCount, [Class| SeenClasses]).

	valid_kernel(linear).
	valid_kernel(polynomial(Degree, Gamma, Coef0)) :-
		valid(positive_integer, Degree),
		valid(positive_number, Gamma),
		valid(non_negative_number, Coef0).
	valid_kernel(rbf(Gamma)) :-
		valid(positive_number, Gamma).

	classifier_data(Classifier, Classes, Encoders, Kernel, TrainingRows, Models, Options) :-
		Classifier =.. [_Functor, Classes, Encoders, Kernel, TrainingRows, Models, Options].

	print_classifier(Classifier) :-
		classifier_data(Classifier, Classes, Encoders, Kernel, TrainingRows, Models, Options),
		format('Kernel SVM Classifier~n', []),
		format('=====================~n~n', []),
		^^print_classifier_template(Classifier),
		format('Classes: ~w~n', [Classes]),
		format('Kernel: ~w~n', [Kernel]),
		length(TrainingRows, TrainingCount),
		format('Training rows: ~w~n', [TrainingCount]),
		format('Options: ~w~n', [Options]),
		format('Encoders: ~w~n', [Encoders]),
		format('Models: ~w~n', [Models]).

	default_option(kernel(linear)).
	default_option(learning_rate(0.5)).
	default_option(learning_schedule(constant)).
	default_option(maximum_iterations(25)).
	default_option(tolerance(1.0e-5)).
	default_option(l2_regularization(0.001)).
	default_option(feature_scaling(true)).

	valid_option(kernel(Kernel)) :-
		valid_kernel(Kernel).
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
