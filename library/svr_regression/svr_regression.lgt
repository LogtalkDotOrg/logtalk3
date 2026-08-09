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


:- object(svr_regression,
	imports(regressor_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-09,
		comment is 'Support vector regression (SVR) using an epsilon-insensitive dual margin model with linear, polynomial, and radial basis function kernels, trained with a Pegasos-style dual subgradient descent loop. Learns from a dataset object implementing the ``regression_dataset_protocol`` protocol and returns a regressor term that can be used for prediction and exported as predicate clauses.',
		see_also is [
			regression_dataset_protocol, kernel_svm_classifier, linear_svm_classifier, linear_regression, ridge_regression, lasso_regression
		]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(linear_algebra, [
		dot_product/3, new_vector/3, subtract_vectors/3
	]).

	:- uses(list, [
		append/3, length/2, memberchk/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Regressor, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(Target),
		fit_svr_model(Dataset, Options, Encoders, Kernel, TrainingRows, TrainingExampleCount, Bias, Coefficients, TrainingDiagnostics),
		build_diagnostics(Target, Encoders, Kernel, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics),
		Regressor = svr_regressor(Encoders, Kernel, TrainingRows, Bias, Coefficients, Diagnostics).

	predict(Regressor, Instance, Prediction) :-
		Regressor =.. [_, Encoders, Kernel, TrainingRows, Bias, Coefficients, _Diagnostics],
		^^encode_instance(Encoders, Instance, Features),
		kernel_vector(TrainingRows, Features, Kernel, KernelValues),
		dot_product(Coefficients, KernelValues, DualScore),
		Prediction is Bias + DualScore.

	build_diagnostics(Target, Encoders, Kernel, TrainingExampleCount, Options, TrainingDiagnostics, Diagnostics) :-
		^^encoded_feature_count(Encoders, FeatureCount),
		append(TrainingDiagnostics, [kernel(Kernel), encoded_feature_count(FeatureCount)], ExtraDiagnostics),
		^^base_regressor_diagnostics(svr_regression, Target, TrainingExampleCount, Options, ExtraDiagnostics, Diagnostics).

	fit_svr_model(Dataset, Options, Encoders, Kernel, TrainingRows, TrainingExampleCount, Bias, Coefficients, TrainingDiagnostics) :-
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		build_svr_encoders(Attributes, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		keys(Rows, TrainingRows),
		^^option(kernel(Kernel), Options),
		build_kernel_matrix(TrainingRows, Kernel, GramMatrix),
		length(Rows, TrainingExampleCount),
		train_svr_model(Rows, GramMatrix, Options, Bias, Coefficients, TrainingDiagnostics).

	build_svr_encoders([], _Examples, _Options, []).
	build_svr_encoders([Attribute-Values| Attributes], Examples, Options, [Encoder| Encoders]) :-
		(	Values == continuous ->
			^^continuous_stats(Attribute, Examples, Options, Mean, Scale),
			Encoder = continuous(Attribute, Mean, Scale)
		;	Encoder = categorical(Attribute, Values)
		),
		build_svr_encoders(Attributes, Examples, Options, Encoders).

	% kernel machinery (shared shape with kernel_svm_classifier)

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

	% epsilon-insensitive dual training (Pegasos-style subgradient descent)
	%
	% Each epoch visits every training row once. For row i with target y and
	% current prediction f(x_i) = Bias + dot(Coefficients, KernelRow_i), the
	% residual y - f(x_i) is compared against the epsilon-insensitive tube:
	% inside the tube (|residual| =< epsilon) only the L2 weight-decay applies;
	% outside it, the i-th dual coefficient and the bias are nudged by one
	% learning-rate step in the direction that shrinks the residual. This
	% mirrors kernel_svm_classifier's per-row dual update, with the one-sided
	% hinge condition replaced by this two-sided epsilon-tube condition.

	train_svr_model(Rows, GramMatrix, Options, Bias, Coefficients, TrainingDiagnostics) :-
		length(Rows, RowCount),
		new_vector(RowCount, 0.0, Coefficients0),
		optimize_svr_model(Rows, GramMatrix, Options, 0, -1.0, 0.0, Coefficients0, Bias, Coefficients, Convergence, Iterations, FinalDelta),
		TrainingDiagnostics = [convergence(Convergence), iterations(Iterations), final_delta(FinalDelta)].

	optimize_svr_model(Rows, GramMatrix, Options, Epoch, PreviousDelta, Bias0, Coefficients0, Bias, Coefficients, Convergence, Iterations, FinalDelta) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(	Epoch >= MaximumIterations ->
			Bias = Bias0,
			Coefficients = Coefficients0,
			Convergence = maximum_iterations_exhausted,
			Iterations = Epoch,
			FinalDelta = PreviousDelta
		;	learning_rate_for_epoch(Options, Epoch, Step),
			process_svr_rows(Rows, GramMatrix, Options, Step, 1, Bias0, Coefficients0, Bias1, Coefficients1, 0.0, MaxDelta),
			^^option(tolerance(Tolerance), Options),
			NextEpoch is Epoch + 1,
			(	MaxDelta =< Tolerance ->
				Bias = Bias1,
				Coefficients = Coefficients1,
				Convergence = tolerance,
				Iterations = NextEpoch,
				FinalDelta = MaxDelta
			;	optimize_svr_model(Rows, GramMatrix, Options, NextEpoch, MaxDelta, Bias1, Coefficients1, Bias, Coefficients, Convergence, Iterations, FinalDelta)
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

	process_svr_rows([], _GramMatrix, _Options, _Step, _Index, Bias, Coefficients, Bias, Coefficients, MaxDelta, MaxDelta).
	process_svr_rows([_Features-Target| Rows], [KernelRow| GramMatrix], Options, Step, Index, Bias0, Coefficients0, Bias, Coefficients, MaxDelta0, MaxDelta) :-
		update_svr_model(Options, Target, KernelRow, Index, Step, Bias0, Coefficients0, Bias1, Coefficients1, Delta),
		MaxDelta1 is max(MaxDelta0, Delta),
		NextIndex is Index + 1,
		process_svr_rows(Rows, GramMatrix, Options, Step, NextIndex, Bias1, Coefficients1, Bias, Coefficients, MaxDelta1, MaxDelta).

	update_svr_model(Options, Target, KernelRow, Index, Step, Bias0, Coefficients0, Bias, Coefficients, Delta) :-
		dot_product(Coefficients0, KernelRow, DualScore),
		Score is Bias0 + DualScore,
		Residual is Target - Score,
		^^option(l2_regularization(Regularization), Options),
		decay_coefficients(Coefficients0, Step, Regularization, Coefficients1, 0.0, DecayDelta),
		^^option(epsilon(Epsilon), Options),
		(	Residual > Epsilon ->
			add_dual_update(Coefficients1, Index, Step, Coefficients, 0.0, UpdateDelta),
			Bias is Bias0 + Step,
			BiasDelta is abs(Bias - Bias0),
			Delta is max(BiasDelta, max(DecayDelta, UpdateDelta))
		;	Residual < -Epsilon ->
			NegativeStep is -Step,
			add_dual_update(Coefficients1, Index, NegativeStep, Coefficients, 0.0, UpdateDelta),
			Bias is Bias0 - Step,
			BiasDelta is abs(Bias - Bias0),
			Delta is max(BiasDelta, max(DecayDelta, UpdateDelta))
		;	Coefficients = Coefficients1,
			Bias = Bias0,
			Delta = DecayDelta
		).

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

	% validation, export, pretty-printing

	check_regressor(Regressor) :-
		(	Regressor = svr_regressor(Encoders, Kernel, TrainingRows, Bias, Coefficients, Diagnostics),
			^^valid_regression_encoders(Encoders),
			valid_kernel(Kernel),
			^^encoded_feature_count(Encoders, FeatureCount),
			valid_training_rows(TrainingRows, FeatureCount),
			length(TrainingRows, TrainingCount),
			valid(float, Bias),
			valid(list(float, TrainingCount), Coefficients),
			^^valid_regressor_metadata(svr_regression, Diagnostics),
			^^valid_linear_model_diagnostics(Diagnostics),
			^^valid_diagnostic_count(encoded_feature_count, Diagnostics, FeatureCount),
			memberchk(kernel(Kernel), Diagnostics) ->
			true
		;	domain_error(regressor, Regressor)
		).

	valid_training_rows([], _FeatureCount).
	valid_training_rows([TrainingRow| TrainingRows], FeatureCount) :-
		valid(list(float, FeatureCount), TrainingRow),
		valid_training_rows(TrainingRows, FeatureCount).

	valid_kernel(linear).
	valid_kernel(polynomial(Degree, Gamma, Coef0)) :-
		valid(positive_integer, Degree),
		valid(positive_number, Gamma),
		valid(non_negative_number, Coef0).
	valid_kernel(rbf(Gamma)) :-
		valid(positive_number, Gamma).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Regressor = svr_regressor(Encoders, Kernel, TrainingRows, Bias, Coefficients, Diagnostics),
		Clause =.. [Functor, Encoders, Kernel, TrainingRows, Bias, Coefficients, Diagnostics].

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Encoders', 'Kernel', 'TrainingRows', 'Bias', 'Coefficients', 'Diagnostics'].

	regressor_term_template(svr_regressor(_Encoders, _Kernel, _TrainingRows, _Bias, _Coefficients, _Diagnostics), svr_regressor('Encoders', 'Kernel', 'TrainingRows', 'Bias', 'Coefficients', 'Diagnostics')).

	print_regressor(Regressor) :-
		Regressor = svr_regressor(Encoders, Kernel, TrainingRows, Bias, Coefficients, Diagnostics),
		format('Support Vector Regression Regressor~n', []),
		format('====================================~n~n', []),
		^^print_regressor_template(Regressor),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Kernel: ~w~n', [Kernel]),
		length(TrainingRows, TrainingCount),
		format('Training rows: ~w~n', [TrainingCount]),
		format('Bias: ~4f~n', [Bias]),
		format('Coefficients: ~w support values~n~n', [Coefficients]),
		format('Encoders: ~w~n', [Encoders]).

	default_option(kernel(linear)).
	default_option(epsilon(0.1)).
	default_option(learning_rate(0.5)).
	default_option(learning_schedule(inverse_scaling(0.5))).
	default_option(maximum_iterations(200)).
	default_option(tolerance(1.0e-5)).
	default_option(l2_regularization(0.001)).
	default_option(feature_scaling(true)).

	valid_option(kernel(Kernel)) :-
		ground(Kernel),
		valid_kernel(Kernel).
	valid_option(epsilon(Epsilon)) :-
		valid(non_negative_number, Epsilon).
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
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(l2_regularization(Regularization)) :-
		number(Regularization),
		Regularization >= 0.0.
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).

:- end_object.
