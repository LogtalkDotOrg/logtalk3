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
		comment is 'Unit tests for the svr_regression library, exercised against the real regression_protocols test dataset fixtures.'
	]).

	:- uses(svr_regression, [
		learn/2, learn/3, predict/3, diagnostics/2, valid_regressor/1,
		export_to_clauses/4, export_to_file/4, print_regressor/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(os, [
		delete_file/1, file_exists/1
	]).

	cover(svr_regression).

	cleanup :-
		^^clean_file('svr_regression_test_export.lgt').

	% learn/2, default options: datasets where the default schedule already
	% converges to a tight fit within the default iteration budget

	test(svr_regression_learn_2_simple_line, true(Error < 0.01)) :-
		learn(simple_line, Regressor),
		predict(Regressor, [x-6], Prediction),
		Error is abs(Prediction - 13.0).

	test(svr_regression_learn_2_plane, true(Error < 0.5)) :-
		learn(plane, Regressor),
		predict(Regressor, [x1-2, x2-1], Prediction),
		Error is abs(Prediction - 9.0).

	test(svr_regression_learn_2_sparse_signal, true(Error < 0.5)) :-
		% the noise attribute carries no signal; the model should still track
		% y = 3*signal + 5 reasonably well using signal alone.
		learn(sparse_signal, Regressor),
		predict(Regressor, [signal-0, noise-1], Prediction),
		Error is abs(Prediction - 5.0).

	test(svr_regression_learn_2_grouped_categorical_signal, true(Error < 0.5)) :-
		learn(grouped_categorical_signal, Regressor),
		predict(Regressor, [signal-1, noise-a], Prediction),
		Error is abs(Prediction - 10.0).

	test(svr_regression_learn_2_intercept_only, true(Error < 0.5)) :-
		learn(intercept_only, Regressor),
		predict(Regressor, [dummy-0], Prediction),
		Error is abs(Prediction - 7.0).

	test(svr_regression_learn_2_no_attribute_intercept, true(Error < 0.5)) :-
		% a dataset with no declared attributes at all: every encoded feature
		% vector is empty, every kernel value is 0, and the fit reduces to
		% the bias term alone (mathematically the same fit as intercept_only).
		learn(no_attribute_intercept, Regressor),
		predict(Regressor, [], Prediction),
		Error is abs(Prediction - 7.0).

	test(svr_regression_learn_3_no_attribute_intercept_polynomial, true(Error < 0.5)) :-
		learn(no_attribute_intercept, Regressor, [kernel(polynomial(2, 0.5, 1.0))]),
		predict(Regressor, [], Prediction),
		Error is abs(Prediction - 7.0).

	test(svr_regression_learn_3_no_attribute_intercept_rbf, true(Error < 0.5)) :-
		learn(no_attribute_intercept, Regressor, [kernel(rbf(0.5))]),
		predict(Regressor, [], Prediction),
		Error is abs(Prediction - 7.0).

	test(svr_regression_learn_2_collinear_line, true(Error < 0.5)) :-
		% x2 = 2*x1 exactly: the dual/kernel formulation does not require
		% inverting a design matrix, so exact collinearity between features
		% is not a numerical hazard here the way it is for some closed-form
		% linear solvers.
		learn(collinear_line, Regressor),
		predict(Regressor, [x1-2, x2-4], Prediction),
		Error is abs(Prediction - 11.0).

	% learn/2, default options: datasets that only reach a loose fit within
	% the default iteration budget (see NOTES.md); sanity-checked here for
	% successful, well-formed training rather than tight accuracy

	test(svr_regression_learn_2_mixed_signal, deterministic) :-
		learn(mixed_signal, Regressor),
		valid_regressor(Regressor),
		predict(Regressor, [age-10, student-no, plan-basic], Prediction),
		number(Prediction).

	test(svr_regression_learn_2_sparse_mixed_signal, deterministic) :-
		% exercises the missing-value indicator features (some examples omit
		% attribute-value pairs) as well as mixed continuous/categorical
		% encoding; only checked for a successful, well-formed fit.
		learn(sparse_mixed_signal, Regressor),
		valid_regressor(Regressor),
		predict(Regressor, [age-10, student-no], Prediction),
		number(Prediction).

	% learn/3, custom options: demonstrates that the harder mixed-feature
	% datasets above reach a tight fit given a larger iteration budget

	test(svr_regression_learn_3_custom_options_mixed_signal, true(Error < 1.0)) :-
		learn(mixed_signal, Regressor, [maximum_iterations(3000)]),
		predict(Regressor, [age-10, student-no, plan-basic], Prediction),
		mixed_signal::example(_, Target, [age-10, student-no, plan-basic]),
		Error is abs(Prediction - Target).

	% kernel selection: step_signal is a piecewise-constant (nonlinear)
	% target that a linear kernel cannot fit well; an rbf kernel should
	% fit it substantially better, over the same iteration budget

	test(svr_regression_learn_3_kernel_linear_vs_rbf_step_signal, true(RmseRbf < RmseLinear)) :-
		learn(step_signal, LinearRegressor, [maximum_iterations(500)]),
		learn(step_signal, RbfRegressor, [kernel(rbf(1.0)), maximum_iterations(500)]),
		dataset_rmse(step_signal, LinearRegressor, RmseLinear),
		dataset_rmse(step_signal, RbfRegressor, RmseRbf).

	test(svr_regression_learn_3_kernel_polynomial, deterministic) :-
		learn(plane, Regressor, [kernel(polynomial(2, 0.5, 1.0)), maximum_iterations(300)]),
		valid_regressor(Regressor),
		predict(Regressor, [x1-2, x2-1], Prediction),
		number(Prediction).

	% validity checks

	test(svr_regression_valid_regressor_1, deterministic) :-
		learn(simple_line, Regressor),
		valid_regressor(Regressor).

	test(svr_regression_invalid_regressor_1, fail) :-
		valid_regressor(not_a_regressor(1, 2, 3)).

	% diagnostics

	test(svr_regression_diagnostics_2, deterministic(Model-Target-Kernel == svr_regression-y-linear)) :-
		learn(simple_line, Regressor),
		diagnostics(Regressor, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(target(Target), Diagnostics),
		memberchk(kernel(Kernel), Diagnostics).

	test(svr_regression_learn_3_maximum_iterations_diagnostics, deterministic(Convergence-Iterations == maximum_iterations_exhausted-5)) :-
		learn(simple_line, Regressor, [maximum_iterations(5)]),
		diagnostics(Regressor, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics).

	test(svr_regression_learn_3_tolerance_diagnostics, deterministic(Convergence == tolerance)) :-
		% a loose enough tolerance that the optimizer settles before the
		% (generous) iteration cap is reached.
		learn(simple_line, Regressor, [tolerance(0.05), maximum_iterations(5000)]),
		diagnostics(Regressor, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics).

	% prediction

	test(svr_regression_predict_3_simple_line, deterministic) :-
		learn(simple_line, Regressor),
		predict(Regressor, [x-0], Prediction),
		number(Prediction).

	% export

	test(svr_regression_export_to_clauses_4, deterministic(Functor-Arity == svr_predict-6)) :-
		learn(simple_line, Regressor),
		export_to_clauses(simple_line, Regressor, svr_predict, [Clause]),
		functor(Clause, Functor, Arity).

	test(svr_regression_export_to_file_4_written, deterministic) :-
		^^file_path('svr_regression_test_export.lgt', Path),
		learn(simple_line, Regressor),
		export_to_file(simple_line, Regressor, svr_predict, Path),
		file_exists(Path).

	test(svr_regression_export_to_file_4_loaded, deterministic(Functor-Arity == svr_predict-6)) :-
		^^file_path('svr_regression_test_export.lgt', Path),
		open(Path, read, Stream),
		read_term(Stream, Term, []),
		close(Stream),
		functor(Term, Functor, Arity).

	% pretty printing (smoke test: succeeds without error)

	test(svr_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		learn(simple_line, Regressor),
		print_regressor(Regressor).

	% schema- and example-level negative tests (learn/2)

	test(svr_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		learn(invalid_target, _).

	test(svr_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		learn(duplicate_attribute_declaration, _).

	test(svr_regression_learn_2_duplicate_attribute_example, error(domain_error(attribute_occurrences, x))) :-
		learn(duplicate_attribute_example, _).

	test(svr_regression_learn_2_undeclared_attribute_example, error(domain_error(declared_attribute, typo))) :-
		learn(undeclared_attribute_example, _).

	% prediction-time negative tests (predict/3)

	test(svr_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		learn(simple_line, Regressor),
		predict(Regressor, [x-1, typo-9], _).

	test(svr_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		learn(simple_line, Regressor),
		predict(Regressor, [x-1, x-2], _).

	test(svr_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), ultra))) :-
		learn(mixed_signal, Regressor),
		predict(Regressor, [age-10, student-no, plan-ultra], _).

	% auxiliary predicates

	dataset_rmse(Dataset, Regressor, RMSE) :-
		findall(
			SquaredError,
			(	Dataset::example(_, Target, AttributeValues),
				predict(Regressor, AttributeValues, Prediction),
				SquaredError is (Prediction - Target) ** 2
			),
			SquaredErrors
		),
		length(SquaredErrors, Count),
		sum(SquaredErrors, Sum),
		MeanSquaredError is Sum / Count,
		RMSE is sqrt(MeanSquaredError).

:- end_object.
