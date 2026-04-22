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
		date is 2026-04-22,
		comment is 'Unit tests for the "linear_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(linear_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(linear_regression_learn_2_simple_line, deterministic(ground(Regressor))) :-
		linear_regression::learn(simple_line, Regressor).

	test(linear_regression_predict_3_simple_line, deterministic(Prediction =~= 13.0)) :-
		linear_regression::learn(simple_line, Regressor, [learning_rate(0.05), maximum_iterations(5000), tolerance(1.0e-9)]),
		linear_regression::predict(Regressor, [x-6], Prediction).

	test(linear_regression_simple_line_matches_numberlist, deterministic(([Weight, Bias] =~= [Slope, Intercept]))) :-
		numberlist::linear_regression([1,2,3,4,5], [3,5,7,9,11], Slope, Intercept),
		linear_regression::learn(simple_line, linear_regressor([continuous(x, 0.0, 1.0)], Bias, [Weight, _MissingWeight], _Options), [feature_scaling(false), learning_rate(0.01), maximum_iterations(8000), tolerance(1.0e-10)]).

	test(linear_regression_predict_3_plane, deterministic(Prediction =~= 3.0)) :-
		linear_regression::learn(plane, Regressor, [learning_rate(0.05), maximum_iterations(8000), tolerance(1.0e-9)]),
		linear_regression::predict(Regressor, [x1-2, x2-4], Prediction).

	test(linear_regression_predict_3_mixed_signal, deterministic(Prediction =~= 170.0)) :-
		linear_regression::learn(mixed_signal, Regressor, [learning_rate(0.05), maximum_iterations(8000), tolerance(1.0e-9)]),
		linear_regression::predict(Regressor, [age-15, student-yes, plan-premium], Prediction).

	test(linear_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		linear_regression::learn(intercept_only, Regressor, [learning_rate(0.05), maximum_iterations(5000), tolerance(1.0e-9)]),
		linear_regression::predict(Regressor, [dummy-0], Prediction).

	test(linear_regression_learn_3_custom_options, deterministic((memberchk(learning_rate(0.1), Options), memberchk(maximum_iterations(1500), Options), memberchk(tolerance(1.0e-6), Options), memberchk(l2_regularization(0.02), Options), memberchk(feature_scaling(false), Options)))) :-
		linear_regression::learn(simple_line, linear_regressor(_Encoders, _Bias, _Weights, Options), [learning_rate(0.1), maximum_iterations(1500), tolerance(1.0e-6), l2_regularization(0.02), feature_scaling(false)]).

	test(linear_regression_export_to_clauses_4, deterministic(Prediction =~= 13.0)) :-
		linear_regression::learn(simple_line, Regressor, [learning_rate(0.05), maximum_iterations(5000), tolerance(1.0e-9)]),
		linear_regression::export_to_clauses(_Dataset, Regressor, regress, [Clause]),
		linear_regression::predict(Clause, [x-6], Prediction).

	test(linear_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		linear_regression::learn(simple_line, Regressor),
		linear_regression::export_to_file(simple_line, Regressor, regress, File).

	test(linear_regression_export_to_file_4_loaded, deterministic(Prediction =~= 13.0)) :-
		^^file_path('test_output.pl', File),
		linear_regression::learn(simple_line, Regressor, [learning_rate(0.05), maximum_iterations(5000), tolerance(1.0e-9)]),
		linear_regression::export_to_file(simple_line, Regressor, regress, File),
		logtalk_load(File),
		{regress(Encoders, Bias, Weights, Options)},
		linear_regression::predict(regress(Encoders, Bias, Weights, Options), [x-6], Prediction).

	test(linear_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		linear_regression::learn(mixed_signal, Regressor),
		linear_regression::print_regressor(Regressor).

	test(linear_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		linear_regression::learn(invalid_target, _Regressor).

	test(linear_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		linear_regression::learn(mixed_signal, Regressor),
		linear_regression::predict(Regressor, [age-10, student-no, plan-deluxe], _Prediction).

:- end_object.
