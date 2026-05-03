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
		date is 2026-05-02,
		comment is 'Unit tests for the "linear_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		member/2
	]).

	cover(linear_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(linear_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		linear_regression::learn(simple_line, Regressor).

	test(linear_regression_valid_regressor_1, deterministic(linear_regression::valid_regressor(Regressor))) :-
		linear_regression::learn(simple_line, Regressor).

	test(linear_regression_invalid_regressor_1, fail) :-
		linear_regression::learn(simple_line, linear_regressor(Encoders, _Bias, _Weights, Diagnostics)),
		linear_regression::valid_regressor(linear_regressor(Encoders, 0.0, [1.0], Diagnostics)).

	test(linear_regression_predict_3_simple_line, deterministic(Prediction =~= 13.0)) :-
		linear_regression::learn(simple_line, Regressor),
		linear_regression::predict(Regressor, [x-6], Prediction).

	test(linear_regression_simple_line_matches_numberlist, deterministic(([Weight, Bias] =~= [Slope, Intercept]))) :-
		numberlist::linear_regression([1,2,3,4,5], [3,5,7,9,11], Slope, Intercept),
		linear_regression::learn(simple_line, linear_regressor([continuous(x, 0.0, 1.0)], Bias, [Weight, _MissingWeight], _Diagnostics), [feature_scaling(false)]).

	test(linear_regression_predict_3_plane, deterministic(Prediction =~= 3.0)) :-
		linear_regression::learn(plane, Regressor),
		linear_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(linear_regression_predict_3_mixed_signal, deterministic(Prediction =~= 170.0)) :-
		linear_regression::learn(mixed_signal, Regressor),
		linear_regression::predict(Regressor, [age-15, student-yes, plan-premium], Prediction).

	test(linear_regression_predict_3_sparse_mixed_signal_missing_attributes, true(Prediction > 150.0)) :-
		linear_regression::learn(sparse_mixed_signal, Regressor),
		linear_regression::predict(Regressor, [age-20], Prediction).

	test(linear_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		linear_regression::learn(intercept_only, Regressor),
		linear_regression::predict(Regressor, [dummy-0], Prediction).

	test(linear_regression_learn_3_custom_options, deterministic((member(feature_scaling(false), Options), \+ member(learning_rate(_), Options), \+ member(maximum_iterations(_), Options), \+ member(tolerance(_), Options), \+ member(regularization(_), Options)))) :-
		linear_regression::learn(simple_line, Regressor, [feature_scaling(false)]),
		linear_regression::regressor_options(Regressor, Options).

	test(linear_regression_diagnostics_2, deterministic((member(model(linear_regression), Diagnostics), member(training_example_count(5), Diagnostics), member(solver(modified_gram_schmidt_column_pivoting), Diagnostics), member(residual_sum_of_squares(ResidualSumOfSquares), Diagnostics), ResidualSumOfSquares =< 1.0e-8, member(effective_rank(2), Diagnostics), member(active_feature_count(1), Diagnostics), member(encoded_feature_count(2), Diagnostics), member(options(Options), Diagnostics), member(feature_scaling(true), Options)))) :-
		linear_regression::learn(simple_line, Regressor),
		linear_regression::diagnostics(Regressor, Diagnostics).

	test(linear_regression_mixed_signal_encoded_feature_count, deterministic(member(encoded_feature_count(6), Diagnostics))) :-
		linear_regression::learn(mixed_signal, Regressor),
		linear_regression::diagnostics(Regressor, Diagnostics).

	test(linear_regression_collinear_line_rank_handling, deterministic((Prediction =~= 27.0, member(residual_sum_of_squares(ResidualSumOfSquares), Diagnostics), ResidualSumOfSquares =< 1.0e-8, member(effective_rank(2), Diagnostics), member(active_feature_count(1), Diagnostics), member(encoded_feature_count(4), Diagnostics)))) :-
		linear_regression::learn(collinear_line, Regressor, [feature_scaling(false)]),
		linear_regression::predict(Regressor, [x1-6, x2-12], Prediction),
		linear_regression::diagnostics(Regressor, Diagnostics).

	test(linear_regression_intercept_only_diagnostics, deterministic((member(solver(modified_gram_schmidt_column_pivoting), Diagnostics), member(residual_sum_of_squares(ResidualSumOfSquares), Diagnostics), ResidualSumOfSquares =< 1.0e-8, member(effective_rank(1), Diagnostics), member(active_feature_count(0), Diagnostics), member(encoded_feature_count(2), Diagnostics)))) :-
		linear_regression::learn(intercept_only, Regressor),
		linear_regression::diagnostics(Regressor, Diagnostics).

	test(linear_regression_export_to_clauses_4, deterministic(Prediction =~= 13.0)) :-
		linear_regression::learn(simple_line, Regressor),
		linear_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		linear_regression::predict(Clause, [x-6], Prediction).

	test(linear_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		linear_regression::learn(simple_line, Regressor),
		linear_regression::export_to_file(simple_line, Regressor, regress, File).

	test(linear_regression_export_to_file_4_loaded, deterministic(Prediction =~= 13.0)) :-
		^^file_path('test_output.pl', File),
		linear_regression::learn(simple_line, Regressor),
		linear_regression::export_to_file(simple_line, Regressor, regress, File),
		logtalk_load(File),
		{regress(Encoders, Bias, Weights, Diagnostics)},
		linear_regression::predict(regress(Encoders, Bias, Weights, Diagnostics), [x-6], Prediction).

	test(linear_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		linear_regression::learn(mixed_signal, Regressor),
		linear_regression::print_regressor(Regressor).

	test(linear_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		linear_regression::learn(invalid_target, _Regressor).

	test(linear_regression_learn_2_duplicate_attribute_declaration, error(domain_error(attribute_declarations, x))) :-
		linear_regression::learn(duplicate_attribute_declaration, _Regressor).

	test(linear_regression_predict_3_undeclared_attribute, error(domain_error(declared_attribute, typo))) :-
		linear_regression::learn(simple_line, Regressor),
		linear_regression::predict(Regressor, [x-6, typo-1], _Prediction).

	test(linear_regression_predict_3_duplicate_attribute, error(domain_error(attribute_occurrences, x))) :-
		linear_regression::learn(simple_line, Regressor),
		linear_regression::predict(Regressor, [x-6, x-7], _Prediction).

	test(linear_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		linear_regression::learn(mixed_signal, Regressor),
		linear_regression::predict(Regressor, [age-10, student-no, plan-deluxe], _Prediction).

:- end_object.
