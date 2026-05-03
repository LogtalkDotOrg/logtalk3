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


:- object(duplicate_inputs,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Regression dataset fixture containing repeated feature vectors to exercise covariance stabilization logic.'
	]).

	attribute_values(x, continuous).

	target(y).

	example(1, 3, [x-1]).
	example(2, 3, [x-1]).
	example(3, 5, [x-2]).

:- end_object.


:- object(categorical_only_signal_order1,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Categorical-only regression dataset fixture used to test invariance to categorical declaration order.'
	]).

	attribute_values(plan, [basic, premium, deluxe]).

	target(score).

	example(1, 10, [plan-basic]).
	example(2, 20, [plan-premium]).
	example(3, 30, [plan-deluxe]).

:- end_object.


:- object(categorical_only_signal_order2,
	implements(regression_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Categorical-only regression dataset fixture with a different declaration order for the same categories.'
	]).

	attribute_values(plan, [premium, basic, deluxe]).

	target(score).

	example(1, 10, [plan-basic]).
	example(2, 20, [plan-premium]).
	example(3, 30, [plan-deluxe]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Unit tests for the "gaussian_process_regression" library.'
	]).

	:- uses(lgtunit, [
		assertion/1, op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	cover(gaussian_process_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	gp_exact_options([feature_scaling(false), optimize_hyperparameters(false), length_scale(2.0), signal_variance(16.0), noise_variance(1.0e-6), jitter(1.0e-8)]).

	test(gaussian_process_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		gaussian_process_regression::learn(simple_line, Regressor).

	test(gaussian_process_regression_valid_regressor_1, deterministic(gaussian_process_regression::valid_regressor(Regressor))) :-
		gaussian_process_regression::learn(simple_line, Regressor).

	test(gaussian_process_regression_invalid_regressor_1, fail) :-
		gaussian_process_regression::learn(simple_line, gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, _Alpha, CholeskyFactor, Kernel, Diagnostics)),
		gaussian_process_regression::valid_regressor(gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, [1.0], CholeskyFactor, Kernel, Diagnostics)).

	test(gaussian_process_regression_invalid_regressor_1_kernel_length_scale_dimensions, fail) :-
		gaussian_process_regression::learn(simple_line, gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, squared_exponential_kernel(FeatureLayout, _LengthScales, CategoricalPenalties, SignalVariance, NoiseVariance, Jitter), Diagnostics)),
		Kernel = squared_exponential_kernel(FeatureLayout, [1.0], CategoricalPenalties, SignalVariance, NoiseVariance, Jitter),
		gaussian_process_regression::valid_regressor(gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics)).

	test(gaussian_process_regression_invalid_regressor_1_diagnostics_length_scale_dimensions, fail) :-
		gaussian_process_regression::learn(simple_line, gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics0)),
		replace_diagnostic(length_scales, Diagnostics0, length_scales([1.0]), Diagnostics),
		gaussian_process_regression::valid_regressor(gaussian_process_regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics)).

	test(gaussian_process_regression_learn_2_structure, deterministic(functor(Regressor, gaussian_process_regressor, 7))) :-
		gaussian_process_regression::learn(simple_line, Regressor).

	test(gaussian_process_regression_predict_3_simple_line_training_point, deterministic(abs(Prediction - 7.0) < 1.0e-4)) :-
		gp_exact_options(Options),
		gaussian_process_regression::learn(simple_line, Regressor, Options),
		gaussian_process_regression::predict(Regressor, [x-3], Prediction).

	test(gaussian_process_regression_predict_distribution_3_simple_line_uncertainty, deterministic) :-
		gp_exact_options(Options),
		gaussian_process_regression::learn(simple_line, Regressor, Options),
		gaussian_process_regression::predict_distribution(Regressor, [x-3], gaussian(Mean, TrainingVariance)),
		gaussian_process_regression::predict_distribution(Regressor, [x-20], gaussian(_FarMean, FarVariance)),
		assertion(abs(Mean - 7.0) < 1.0e-4),
		assertion(TrainingVariance >= 0.0),
		assertion(FarVariance > TrainingVariance).

	test(gaussian_process_regression_predict_3_plane_training_point, deterministic(abs(Prediction - 8.0) < 1.0e-4)) :-
		gaussian_process_regression::learn(plane, Regressor, [feature_scaling(false), optimize_hyperparameters(false), length_scale(2.0), signal_variance(25.0), noise_variance(1.0e-6)]),
		gaussian_process_regression::predict(Regressor, [x1-2, x2-1.5], Prediction).

	test(gaussian_process_regression_predict_3_mixed_signal_training_point, deterministic(abs(Prediction - 175.0) < 1.0e-4)) :-
		gaussian_process_regression::learn(mixed_signal, Regressor, [optimize_hyperparameters(false), length_scale(2.0), signal_variance(64.0), noise_variance(1.0e-6)]),
		gaussian_process_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(gaussian_process_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		gaussian_process_regression::learn(sparse_mixed_signal, Regressor, [optimize_hyperparameters(false), length_scale(2.0), signal_variance(64.0), noise_variance(1.0e-6)]),
		gaussian_process_regression::predict(Regressor, [age-20], Prediction).

	test(gaussian_process_regression_predict_distribution_3_intercept_only, deterministic) :-
		gaussian_process_regression::learn(intercept_only, Regressor, [optimize_hyperparameters(false), signal_variance(1.0), noise_variance(1.0e-6)]),
		gaussian_process_regression::predict_distribution(Regressor, [dummy-0], gaussian(Mean, Variance)),
		assertion(abs(Mean - 7.0) < 1.0e-4),
		assertion(Variance >= 0.0).

	test(gaussian_process_regression_learn_3_custom_options, deterministic) :-
		UserOptions = [
			feature_scaling(false),
			optimize_hyperparameters(false),
			length_scale(2.0),
			signal_variance(16.0),
			noise_variance(1.0e-6),
			relative_improvement_factor(1.0e-3),
			hyperparameter_minimum(1.0e-5),
			maximum_continuous_length_scale(16.0),
			maximum_categorical_penalty(8.0),
			max_factorization_attempts(8),
			jitter_scale_factor(4.0)
		],
		gaussian_process_regression::learn(simple_line, Regressor, UserOptions),
		gaussian_process_regression::regressor_options(Regressor, Options),
		assertion(member(feature_scaling(false), Options)),
		assertion(member(optimize_hyperparameters(false), Options)),
		assertion(member(length_scale(2.0), Options)),
		assertion(member(signal_variance(16.0), Options)),
		assertion(member(noise_variance(1.0e-6), Options)),
		assertion(member(relative_improvement_factor(1.0e-3), Options)),
		assertion(member(hyperparameter_minimum(1.0e-5), Options)),
		assertion(member(maximum_continuous_length_scale(16.0), Options)),
		assertion(member(maximum_categorical_penalty(8.0), Options)),
		assertion(member(max_factorization_attempts(8), Options)),
		assertion(member(jitter_scale_factor(4.0), Options)),
		assertion(member(kernel(squared_exponential), Options)).

	test(gaussian_process_regression_diagnostics_2, deterministic) :-
		gaussian_process_regression::learn(simple_line, Regressor),
		gaussian_process_regression::diagnostics(Regressor, Diagnostics),
		memberchk(length_scales(LengthScales), Diagnostics),
		assertion(length(LengthScales, 2)),
		memberchk(categorical_penalties(CategoricalPenalties), Diagnostics),
		assertion(CategoricalPenalties == []),
		memberchk(signal_variance(SignalVariance), Diagnostics),
		assertion(SignalVariance > 0.0),
		memberchk(noise_variance(NoiseVariance), Diagnostics),
		assertion(NoiseVariance >= 0.0),
		memberchk(jitter(Jitter), Diagnostics),
		assertion(Jitter > 0.0),
		assertion(member(continuous_feature_count(2), Diagnostics)),
		assertion(member(categorical_feature_count(0), Diagnostics)),
		memberchk(jitter_attempts(JitterAttempts), Diagnostics),
		assertion(JitterAttempts >= 0),
		memberchk(iterations(Iterations), Diagnostics),
		assertion(integer(Iterations)),
		assertion(member(model(gaussian_process_regression), Diagnostics)),
		assertion(member(training_example_count(5), Diagnostics)),
		assertion(member(kernel(squared_exponential), Diagnostics)),
		assertion(integer(JitterAttempts)),
		assertion(member(log_marginal_likelihood(_), Diagnostics)),
		assertion(member(convergence(_), Diagnostics)),
		assertion(member(encoded_feature_count(2), Diagnostics)).

	test(gaussian_process_regression_learn_3_length_scale_dimension_mismatch, error(domain_error(length_scale_dimensions(2), [1.0]))) :-
		gaussian_process_regression::learn(simple_line, _Regressor, [feature_scaling(false), optimize_hyperparameters(false), length_scale([1.0]), signal_variance(16.0), noise_variance(1.0e-6)]).

	test(gaussian_process_regression_learn_3_duplicate_inputs_jitter_retry, deterministic) :-
		gaussian_process_regression::learn(duplicate_inputs, Regressor, [feature_scaling(false), optimize_hyperparameters(false), length_scale([1.0, 1.0]), signal_variance(1.0), noise_variance(0.0), jitter(1.0e-16)]),
		gaussian_process_regression::diagnostics(Regressor, Diagnostics),
		memberchk(jitter(Jitter), Diagnostics),
		memberchk(jitter_attempts(JitterAttempts), Diagnostics),
		assertion(Jitter > 1.0e-16),
		assertion(JitterAttempts > 0).

	test(gaussian_process_regression_predict_3_categorical_order_invariance, deterministic) :-
		Options = [optimize_hyperparameters(false), categorical_penalty(1.0), signal_variance(16.0), noise_variance(1.0e-6)],
		gaussian_process_regression::learn(categorical_only_signal_order1, Regressor1, Options),
		gaussian_process_regression::learn(categorical_only_signal_order2, Regressor2, Options),
		gaussian_process_regression::predict(Regressor1, [plan-premium], Prediction1),
		gaussian_process_regression::predict(Regressor2, [plan-premium], Prediction2),
		assertion(abs(Prediction1 - Prediction2) < 1.0e-8).

	test(gaussian_process_regression_export_to_clauses_4, deterministic(abs(Prediction - 7.0) < 1.0e-4)) :-
		gp_exact_options(Options),
		gaussian_process_regression::learn(simple_line, Regressor, Options),
		gaussian_process_regression::export_to_clauses(simple_line, Regressor, regressor, [Clause]),
		gaussian_process_regression::predict(Clause, [x-3], Prediction).

	test(gaussian_process_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		gaussian_process_regression::learn(simple_line, Regressor),
		gaussian_process_regression::export_to_file(simple_line, Regressor, regressor, File).

	test(gaussian_process_regression_export_to_file_4_loaded, deterministic(abs(Prediction - 7.0) < 1.0e-4)) :-
		^^file_path('test_output.pl', File),
		gp_exact_options(Options),
		gaussian_process_regression::learn(simple_line, Regressor, Options),
		gaussian_process_regression::export_to_file(simple_line, Regressor, regressor, File),
		logtalk_load(File),
		{regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics)},
		gaussian_process_regression::predict(regressor(Encoders, TrainingFeatures, TargetMean, Alpha, CholeskyFactor, Kernel, Diagnostics), [x-3], Prediction).

	test(gaussian_process_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		gaussian_process_regression::learn(simple_line, Regressor),
		gaussian_process_regression::print_regressor(Regressor).

	test(gaussian_process_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		gaussian_process_regression::learn(invalid_target, _Regressor).

	test(gaussian_process_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		gaussian_process_regression::learn(duplicate_attribute_declaration, _Regressor).

	test(gaussian_process_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		gaussian_process_regression::learn(simple_line, Regressor),
		gaussian_process_regression::predict(Regressor, [x-3, typo-1], _Prediction).

	test(gaussian_process_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		gaussian_process_regression::learn(simple_line, Regressor),
		gaussian_process_regression::predict(Regressor, [x-3, x-4], _Prediction).

	test(gaussian_process_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		gaussian_process_regression::learn(mixed_signal, Regressor),
		gaussian_process_regression::predict(Regressor, [age-20, student-yes, plan-deluxe], _Prediction).

	% auxiliary predicates

	replace_diagnostic(Functor, [Diagnostic| Diagnostics], Replacement, [Replacement| Diagnostics]) :-
		functor(Diagnostic, Functor, 1),
		!.
	replace_diagnostic(Functor, [Diagnostic| Diagnostics], Replacement, [Diagnostic| UpdatedDiagnostics]) :-
		replace_diagnostic(Functor, Diagnostics, Replacement, UpdatedDiagnostics).

:- end_object.
