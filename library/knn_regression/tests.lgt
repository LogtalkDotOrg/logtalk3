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
		date is 2026-05-06,
		comment is 'Unit tests for the "knn_regression" library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).
	:- uses(list, [
		member/2
	]).

	cover(knn_regression).

	cleanup :-
		^^clean_file('test_output.pl').

	test(knn_regression_learn_2_step_signal, deterministic(ground(Regressor))) :-
		knn_regression::learn(step_signal, Regressor).

	test(knn_regression_valid_regressor_1, deterministic(knn_regression::valid_regressor(Regressor))) :-
		knn_regression::learn(step_signal, Regressor).

	test(knn_regression_invalid_regressor_1, fail) :-
		knn_regression::valid_regressor(knn_regressor(
			[continuous(x, 0.0, 1.0)],
			[[1.0]-7],
			[model(knn_regression), target(y), training_example_count(1), options([k(1), distance_metric(euclidean), weight_scheme(uniform), minkowski_power(3.0), feature_scaling(true)]), encoded_feature_count(2)]
		)).

	test(knn_regression_predict_3_step_signal_left_band, deterministic(Prediction =~= 10.0)) :-
		knn_regression::learn(step_signal, Regressor, [k(1), feature_scaling(false)]),
		knn_regression::predict(Regressor, [x-1.5], Prediction).

	test(knn_regression_predict_3_step_signal_right_band, deterministic(Prediction =~= 20.0)) :-
		knn_regression::learn(step_signal, Regressor, [k(1), feature_scaling(false)]),
		knn_regression::predict(Regressor, [x-8.5], Prediction).

	test(knn_regression_predict_3_intercept_only, deterministic(Prediction =~= 7.0)) :-
		knn_regression::learn(intercept_only, Regressor, [k(3)]),
		knn_regression::predict(Regressor, [dummy-0], Prediction).

	test(knn_regression_predict_3_step_signal_minkowski, deterministic(Prediction =~= 10.0)) :-
		knn_regression::learn(step_signal, Regressor, [k(1), distance_metric(minkowski), minkowski_power(3.0), feature_scaling(false)]),
		knn_regression::predict(Regressor, [x-1.5], Prediction).

	test(knn_regression_predict_3_mixed_signal_exact_neighbor, deterministic(Prediction =~= 175.0)) :-
		knn_regression::learn(mixed_signal, Regressor, [k(1)]),
		knn_regression::predict(Regressor, [age-20, student-yes, plan-premium], Prediction).

	test(knn_regression_predict_3_sparse_mixed_signal_missing_attributes, deterministic(Prediction =~= 200.0)) :-
		knn_regression::learn(sparse_mixed_signal, Regressor, [k(1), feature_scaling(false)]),
		knn_regression::predict(Regressor, [age-20], Prediction).

	test(knn_regression_learn_3_custom_options, deterministic((member(k(5), Options), member(distance_metric(manhattan), Options), member(weight_scheme(distance), Options), member(minkowski_power(4.0), Options), member(feature_scaling(false), Options)))) :-
		knn_regression::learn(step_signal, Regressor, [k(5), distance_metric(manhattan), weight_scheme(distance), minkowski_power(4.0), feature_scaling(false)]),
		knn_regression::regressor_options(Regressor, Options).

	test(knn_regression_diagnostics_2, deterministic((member(model(knn_regression), Diagnostics), member(encoded_feature_count(2), Diagnostics), member(options(Options), Diagnostics), member(k(3), Options)))) :-
		knn_regression::learn(step_signal, Regressor),
		knn_regression::diagnostics(Regressor, Diagnostics).

	test(knn_regression_export_to_clauses_4, deterministic(Prediction =~= 20.0)) :-
		knn_regression::learn(step_signal, Regressor, [k(1), feature_scaling(false)]),
		knn_regression::export_to_clauses(step_signal, Regressor, regress, [Clause]),
		knn_regression::predict(Clause, [x-9], Prediction).

	test(knn_regression_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		knn_regression::learn(step_signal, Regressor),
		knn_regression::export_to_file(step_signal, Regressor, regress, File).

	test(knn_regression_export_to_file_4_loaded, deterministic(Prediction =~= 10.0)) :-
		^^file_path('test_output.pl', File),
		knn_regression::learn(step_signal, Regressor, [k(1), feature_scaling(false)]),
		knn_regression::export_to_file(step_signal, Regressor, regress, File),
		logtalk_load(File),
		{regress(Encoders, Rows, Diagnostics)},
		knn_regression::predict(regress(Encoders, Rows, Diagnostics), [x-2], Prediction).

	test(knn_regression_print_regressor_1, deterministic) :-
		^^suppress_text_output,
		knn_regression::learn(step_signal, Regressor),
		knn_regression::print_regressor(Regressor).

	test(knn_regression_learn_2_invalid_target, error(type_error(number, bad))) :-
		knn_regression::learn(invalid_target, _Regressor).

	test(knn_regression_predict_3_unknown_category, error(domain_error(attribute_value(plan, [basic, premium]), deluxe))) :-
		knn_regression::learn(mixed_signal, Regressor),
		knn_regression::predict(Regressor, [age-20, student-yes, plan-deluxe], _Prediction).

:- end_object.
