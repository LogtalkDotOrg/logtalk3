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
		date is 2026-05-03,
		comment is 'Unit tests for the "bayesian_ridge_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(bayesian_ridge_regression).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_simple_line.pl'),
		^^clean_file('test_output_mixed_signal.pl').

	test(bayesian_ridge_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		bayesian_ridge_regression::learn(simple_line, Regressor).

	test(bayesian_ridge_regression_valid_regressor_1, deterministic(bayesian_ridge_regression::valid_regressor(Regressor))) :-
		bayesian_ridge_regression::learn(simple_line, Regressor).

	test(bayesian_ridge_regression_invalid_regressor_1, fail) :-
		bayesian_ridge_regression::learn(simple_line, bayesian_ridge_regressor(Encoders, Bias, Weights, _ActiveFlags, _PosteriorCovariance, NoiseVariance, Diagnostics)),
		bayesian_ridge_regression::valid_regressor(bayesian_ridge_regressor(Encoders, Bias, Weights, [active], [[1.0]], NoiseVariance, Diagnostics)).

	test(bayesian_ridge_regression_predict_3_simple_line, true(abs(Prediction - 13.0) < 0.05)) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		bayesian_ridge_regression::predict(Regressor, [x-6], Prediction).

	test(bayesian_ridge_regression_predict_3_plane, true(abs(Prediction - 3.0) < 0.10)) :-
		bayesian_ridge_regression::learn(plane, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		bayesian_ridge_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(bayesian_ridge_regression_predict_3_collinear_line, true(abs(Prediction - 27.0) < 0.10)) :-
		bayesian_ridge_regression::learn(collinear_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		bayesian_ridge_regression::predict(Regressor, [x1-6, x2-12], Prediction).

	test(bayesian_ridge_regression_predict_3_mixed_signal, true(abs(Prediction - 175.0) < 1.5)) :-
		bayesian_ridge_regression::learn(mixed_signal, Regressor),
		bayesian_ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(bayesian_ridge_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		bayesian_ridge_regression::learn(sparse_mixed_signal, Regressor, [maximum_iterations(500), tolerance(1.0e-8)]),
		bayesian_ridge_regression::predict(Regressor, [age-20], Prediction).

	test(bayesian_ridge_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		bayesian_ridge_regression::learn(intercept_only, Regressor),
		bayesian_ridge_regression::predict(Regressor, [dummy-0], Prediction).

	test(bayesian_ridge_regression_predict_3_no_attribute_intercept, deterministic(Prediction =~= 7.0)) :-
		bayesian_ridge_regression::learn(no_attribute_intercept, Regressor),
		bayesian_ridge_regression::predict(Regressor, [], Prediction).

	test(bayesian_ridge_regression_predict_distribution_3_simple_line_uncertainty, deterministic) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		Regressor = bayesian_ridge_regressor(_Encoders, _Bias, _Weights, _ActiveFlags, [[BiasVariance, BiasWeightCovariance], [WeightBiasCovariance, WeightVariance]], NoiseVariance, _Diagnostics),
		bayesian_ridge_regression::predict_distribution(Regressor, [x-3], gaussian(Mean, NearVariance)),
		bayesian_ridge_regression::predict_distribution(Regressor, [x-20], gaussian(_FarMean, FarVariance)),
		assertion(abs(Mean - 7.0) < 0.05),
		assertion(BiasVariance =~= 0.0),
		assertion(BiasWeightCovariance =~= 0.0),
		assertion(WeightBiasCovariance =~= 0.0),
		assertion(abs(NearVariance - (NoiseVariance + 9.0 * WeightVariance)) < 1.0e-10),
		assertion(NearVariance >= 0.0),
		assertion(FarVariance > NearVariance).

	test(bayesian_ridge_regression_predict_distribution_3_intercept_only, deterministic) :-
		bayesian_ridge_regression::learn(intercept_only, Regressor),
		Regressor = bayesian_ridge_regressor(_Encoders, _Bias, _Weights, _ActiveFlags, [[BiasVariance]], NoiseVariance, _Diagnostics),
		bayesian_ridge_regression::predict_distribution(Regressor, [dummy-0], gaussian(Mean, Variance)),
		assertion(Mean =~= 7.0),
		assertion(BiasVariance =~= 0.0),
		assertion(Variance =~= NoiseVariance),
		assertion(Variance >= 0.0).

	test(bayesian_ridge_regression_predict_distribution_3_no_attribute_intercept, deterministic) :-
		bayesian_ridge_regression::learn(no_attribute_intercept, Regressor),
		Regressor = bayesian_ridge_regressor(_Encoders, _Bias, _Weights, _ActiveFlags, [[BiasVariance]], NoiseVariance, _Diagnostics),
		bayesian_ridge_regression::predict_distribution(Regressor, [], gaussian(Mean, Variance)),
		assertion(Mean =~= 7.0),
		assertion(BiasVariance =~= 0.0),
		assertion(Variance =~= NoiseVariance),
		assertion(Variance >= 0.0).

	test(bayesian_ridge_regression_weight_variances_2, deterministic) :-
		bayesian_ridge_regression::learn(mixed_signal, Regressor),
		bayesian_ridge_regression::weight_variances(Regressor, Variances),
		length(Variances, 6),
		assertion(memberchk(0.0, Variances)),
		memberchk(Variance, Variances),
		assertion(Variance >= 0.0).

	test(bayesian_ridge_regression_learn_3_custom_options, deterministic) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [maximum_iterations(25), tolerance(1.0e-4), initial_weight_precision(2.0), initial_noise_precision(3.0), alpha_1(0.1), alpha_2(0.2), lambda_1(0.3), lambda_2(0.4), feature_scaling(false), precision_bounds(1.0e-9, 1.0e9)]),
		bayesian_ridge_regression::regressor_options(Regressor, Options),
		assertion(memberchk(maximum_iterations(25), Options)),
		assertion(memberchk(tolerance(1.0e-4), Options)),
		assertion(memberchk(initial_weight_precision(2.0), Options)),
		assertion(memberchk(initial_noise_precision(3.0), Options)),
		assertion(memberchk(alpha_1(0.1), Options)),
		assertion(memberchk(alpha_2(0.2), Options)),
		assertion(memberchk(lambda_1(0.3), Options)),
		assertion(memberchk(lambda_2(0.4), Options)),
		assertion(memberchk(feature_scaling(false), Options)),
		assertion(memberchk(precision_bounds(1.0e-9, 1.0e9), Options)).

	test(bayesian_ridge_regression_diagnostics_2, deterministic) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		bayesian_ridge_regression::diagnostics(Regressor, Diagnostics),
		memberchk(model(bayesian_ridge_regression), Diagnostics),
		memberchk(training_example_count(5), Diagnostics),
		memberchk(stabilization_attempts(StabilizationAttempts), Diagnostics),
		memberchk(stabilization_jitter(StabilizationJitter), Diagnostics),
		memberchk(precision_bounds(MinimumPrecision, MaximumPrecision), Diagnostics),
		memberchk(weight_precision_hyperprior(gamma(LambdaShape, LambdaRate)), Diagnostics),
		memberchk(noise_precision_hyperprior(gamma(AlphaShape, AlphaRate)), Diagnostics),
		memberchk(weight_precision(Alpha), Diagnostics),
		memberchk(noise_precision(Beta), Diagnostics),
		memberchk(noise_variance(NoiseVariance), Diagnostics),
		memberchk(log_evidence(LogEvidence), Diagnostics),
		memberchk(scores(Scores), Diagnostics),
		memberchk(active_feature_count(1), Diagnostics),
		memberchk(weight_prior(isotropic_zero_mean_gaussian), Diagnostics),
		memberchk(intercept_treatment(non_probabilistic), Diagnostics),
		memberchk(bias_variance(BiasVariance), Diagnostics),
		memberchk(weight_variances(WeightVariances), Diagnostics),
		memberchk(convergence_metric(coefficient_l1), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics),
		memberchk(encoded_feature_count(2), Diagnostics),
		memberchk(options(Options), Diagnostics),
		assertion(Alpha > 0.0),
		assertion(Beta > 0.0),
		assertion(StabilizationAttempts >= 0),
		assertion(StabilizationJitter >= 0.0),
		assertion(MinimumPrecision =:= 1.0e-12),
		assertion(MaximumPrecision =:= 1.0e12),
		assertion(LambdaShape =:= 1.0e-6),
		assertion(LambdaRate =:= 1.0e-6),
		assertion(AlphaShape =:= 1.0e-6),
		assertion(AlphaRate =:= 1.0e-6),
		assertion(NoiseVariance >= 0.0),
		assertion(LogEvidence =< 1.0e12),
		assertion(BiasVariance =~= 0.0),
		assertion(length(WeightVariances, 2)),
		assertion(Convergence == tolerance),
		assertion(Iterations >= 1),
		ScoreCount is Iterations + 1,
		assertion(length(Scores, ScoreCount)),
		last_score(Scores, LogEvidence),
		assertion(FinalDelta =< 1.0e-10),
		assertion(FinalDelta >= 0.0),
		assertion(memberchk(initial_noise_precision(auto), Options)),
		assertion(memberchk(alpha_1(1.0e-6), Options)),
		assertion(memberchk(alpha_2(1.0e-6), Options)),
		assertion(memberchk(lambda_1(1.0e-6), Options)),
		assertion(memberchk(lambda_2(1.0e-6), Options)),
		assertion(memberchk(precision_bounds(1.0e-12, 1.0e12), Options)).

	test(bayesian_ridge_regression_final_delta_2_tracks_coefficient_l1_change, deterministic) :-
		Options = [feature_scaling(false), maximum_iterations(1), tolerance(0.0)],
		bayesian_ridge_regression::learn(simple_line, Regressor1, Options),
		regressor_weights(Regressor1, Weights1),
		bayesian_ridge_regression::learn(simple_line, Regressor2, [feature_scaling(false), maximum_iterations(2), tolerance(0.0)]),
		regressor_weights(Regressor2, Weights2),
		bayesian_ridge_regression::diagnostics(Regressor2, Diagnostics),
		memberchk(convergence(maximum_iterations_exhausted), Diagnostics),
		memberchk(iterations(2), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics),
		vector_l1_distance(Weights1, Weights2, ExpectedDelta),
		assertion(abs(FinalDelta - ExpectedDelta) < 1.0e-10).

	test(bayesian_ridge_regression_log_evidence_2_matches_closed_form_simple_line, deterministic) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		Regressor = bayesian_ridge_regressor(_Encoders, Bias, Weights, _ActiveFlags, [[_BiasVariance, _BiasWeightCovariance], [_WeightBiasCovariance, WeightVariance]], _NoiseVariance, Diagnostics),
		bayesian_ridge_regression::diagnostics(Regressor, Diagnostics),
		memberchk(weight_precision(Alpha), Diagnostics),
		memberchk(noise_precision(Beta), Diagnostics),
		memberchk(log_evidence(LogEvidence), Diagnostics),
		residual_sum_of_squares(simple_line, Bias, Weights, ResidualSumOfSquares),
		weight_square_norm(Weights, WeightSquaredNorm),
		DefaultShape = 1.0e-6,
		DefaultRate = 1.0e-6,
		ExpectedLogEvidence is DefaultShape * log(Alpha) - DefaultRate * Alpha + DefaultShape * log(Beta) - DefaultRate * Beta + 0.5 * (log(Alpha) + 5.0 * log(Beta) - Alpha * WeightSquaredNorm - Beta * ResidualSumOfSquares + log(WeightVariance) - 5.0 * log(2.0 * pi)),
		assertion(abs(LogEvidence - ExpectedLogEvidence) < 1.0e-8).

	test(bayesian_ridge_regression_learn_3_maximum_iterations_diagnostics, deterministic) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(1), tolerance(1.0e-12)]),
		bayesian_ridge_regression::diagnostics(Regressor, Diagnostics),
		memberchk(convergence(maximum_iterations_exhausted), Diagnostics),
		memberchk(iterations(1), Diagnostics),
		memberchk(scores(Scores), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics),
		assertion(length(Scores, 2)),
		assertion(FinalDelta >= 0.0).

	test(bayesian_ridge_regression_export_to_clauses_4, true(abs(Prediction - 13.0) < 0.05)) :-
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		bayesian_ridge_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		bayesian_ridge_regression::predict(Clause, [x-6], Prediction).

	test(bayesian_ridge_regression_export_to_clauses_4_default, deterministic(ClausePrediction =~= ModelPrediction)) :-
		bayesian_ridge_regression::learn(mixed_signal, Regressor),
		bayesian_ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		bayesian_ridge_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		bayesian_ridge_regression::predict(Clause, [age-20, student-yes, plan-premium], ClausePrediction).

	test(bayesian_ridge_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		bayesian_ridge_regression::learn(simple_line, Regressor),
		bayesian_ridge_regression::export_to_file(simple_line, Regressor, regress, File).

	test(bayesian_ridge_regression_export_to_file_4_loaded, true(abs(Prediction - 13.0) < 0.05)) :-
		^^file_path('test_output_simple_line.pl', File),
		bayesian_ridge_regression::learn(simple_line, Regressor, [feature_scaling(false), maximum_iterations(500), tolerance(1.0e-10)]),
		bayesian_ridge_regression::export_to_file(simple_line, Regressor, regress_simple_line, File),
		logtalk_load(File),
		{regress_simple_line(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics)},
		bayesian_ridge_regression::predict(regress(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics), [x-6], Prediction).

	test(bayesian_ridge_regression_export_to_file_4_loaded_default, deterministic(LoadedPrediction =~= ModelPrediction)) :-
		^^file_path('test_output_mixed_signal.pl', File),
		bayesian_ridge_regression::learn(mixed_signal, Regressor),
		bayesian_ridge_regression::predict(Regressor, [age-20, student-yes, plan-premium], ModelPrediction),
		bayesian_ridge_regression::export_to_file(mixed_signal, Regressor, regress_mixed_signal, File),
		logtalk_load(File),
		{regress_mixed_signal(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics)},
		bayesian_ridge_regression::predict(regress(Encoders, Bias, Weights, ActiveFlags, PosteriorCovariance, NoiseVariance, Diagnostics), [age-20, student-yes, plan-premium], LoadedPrediction).

	test(bayesian_ridge_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		bayesian_ridge_regression::learn(mixed_signal, Regressor),
		bayesian_ridge_regression::print_regressor(Regressor).

	test(bayesian_ridge_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		bayesian_ridge_regression::learn(invalid_target, _Regressor).

	test(bayesian_ridge_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		bayesian_ridge_regression::learn(duplicate_attribute_declaration, _Regressor).

	test(bayesian_ridge_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		bayesian_ridge_regression::learn(simple_line, Regressor),
		bayesian_ridge_regression::predict(Regressor, [x-6, typo-1], _Prediction).

	test(bayesian_ridge_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		bayesian_ridge_regression::learn(simple_line, Regressor),
		bayesian_ridge_regression::predict(Regressor, [x-6, x-7], _Prediction).

	test(bayesian_ridge_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		bayesian_ridge_regression::learn(mixed_signal, Regressor),
		bayesian_ridge_regression::predict(Regressor, [age-10, student-no, plan-deluxe], _Prediction).

	regressor_weights(bayesian_ridge_regressor(_Encoders, _Bias, Weights, _ActiveFlags, _PosteriorCovariance, _NoiseVariance, _Diagnostics), Weights).

	vector_l1_distance([], [], 0.0).
	vector_l1_distance([Value1| Values1], [Value2| Values2], Distance) :-
		vector_l1_distance(Values1, Values2, RestDistance),
		Distance is RestDistance + abs(Value1 - Value2).

	residual_sum_of_squares(Dataset, Bias, Weights, ResidualSumOfSquares) :-
		findall(Target-Attributes, Dataset::example(_, Target, Attributes), Examples),
		residual_sum_of_squares(Examples, Bias, Weights, 0.0, ResidualSumOfSquares).

	residual_sum_of_squares([], _Bias, _Weights, ResidualSumOfSquares, ResidualSumOfSquares).
	residual_sum_of_squares([Target-[x-Value]| Examples], Bias, [Weight, 0.0], ResidualSumOfSquares0, ResidualSumOfSquares) :-
		Prediction is Bias + Weight * Value,
		Residual is Prediction - Target,
		ResidualSumOfSquares1 is ResidualSumOfSquares0 + Residual * Residual,
		residual_sum_of_squares(Examples, Bias, [Weight, 0.0], ResidualSumOfSquares1, ResidualSumOfSquares).

	weight_square_norm([], 0.0).
	weight_square_norm([Weight| Weights], WeightSquaredNorm) :-
		weight_square_norm(Weights, RestSquaredNorm),
		WeightSquaredNorm is RestSquaredNorm + Weight * Weight.

	last_score([Score], Score) :-
		!.
	last_score([_| Scores], Score) :-
		last_score(Scores, Score).

:- end_object.
