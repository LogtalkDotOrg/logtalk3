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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-03,
		comment is 'Performance and reference-fit benchmarks for the "gaussian_process_regression" library, including the default optimizer-enabled training path.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	test(simple_line_reference_fit, true, [note(metrics(default_train_seconds-DefaultTrainTime, reference_train_seconds-ReferenceTrainTime, rmse-RMSE, mae-MAE))]) :-
		performance_profile(simple_line, [feature_scaling(false), optimize_hyperparameters(false), length_scale(2.0), signal_variance(16.0), noise_variance(1.0e-6)], 0.01, 0.01, DefaultTrainTime, ReferenceTrainTime, RMSE, MAE).

	test(plane_reference_fit, true, [note(metrics(default_train_seconds-DefaultTrainTime, reference_train_seconds-ReferenceTrainTime, rmse-RMSE, mae-MAE))]) :-
		performance_profile(plane, [feature_scaling(false), optimize_hyperparameters(false), length_scale(2.0), signal_variance(25.0), noise_variance(1.0e-6)], 0.01, 0.01, DefaultTrainTime, ReferenceTrainTime, RMSE, MAE).

	test(mixed_signal_reference_fit, true, [note(metrics(default_train_seconds-DefaultTrainTime, reference_train_seconds-ReferenceTrainTime, rmse-RMSE, mae-MAE))]) :-
		performance_profile(mixed_signal, [optimize_hyperparameters(false), length_scale(2.0), signal_variance(64.0), noise_variance(1.0e-6)], 0.01, 0.01, DefaultTrainTime, ReferenceTrainTime, RMSE, MAE).

	test(wide_mixed_signal_reference_fit, true, [note(metrics(default_train_seconds-DefaultTrainTime, reference_train_seconds-ReferenceTrainTime, rmse-RMSE, mae-MAE))]) :-
		performance_profile(wide_mixed_signal, [optimize_hyperparameters(false), length_scale(2.5), signal_variance(64.0), noise_variance(1.0e-6)], 0.10, 0.10, DefaultTrainTime, ReferenceTrainTime, RMSE, MAE).

	test(intercept_only_reference_fit, true, [note(metrics(default_train_seconds-DefaultTrainTime, reference_train_seconds-ReferenceTrainTime, rmse-RMSE, mae-MAE))]) :-
		performance_profile(intercept_only, [optimize_hyperparameters(false), signal_variance(1.0), noise_variance(1.0e-6)], 0.01, 0.01, DefaultTrainTime, ReferenceTrainTime, RMSE, MAE).

	% auxiliary predicates

	performance_profile(Dataset, ReferenceOptions, MaximumRMSE, MaximumMAE, DefaultTrainTime, ReferenceTrainTime, RMSE, MAE) :-
		benchmark(gaussian_process_regression::learn(Dataset, _DefaultRegressor), DefaultTrainTime),
		reference_fit(Dataset, ReferenceOptions, MaximumRMSE, MaximumMAE, ReferenceTrainTime, RMSE, MAE).

	reference_fit(Dataset, Options, MaximumRMSE, MaximumMAE, ReferenceTrainTime, RMSE, MAE) :-
		benchmark(gaussian_process_regression::learn(Dataset, _Regressor, Options), ReferenceTrainTime),
		gaussian_process_regression::learn(Dataset, Regressor, Options),
		dataset_metrics(Dataset, Regressor, RMSE, MAE),
		RMSE =< MaximumRMSE,
		MAE =< MaximumMAE.

	dataset_metrics(Dataset, Regressor, RMSE, MAE) :-
		findall(
			Target-AttributeValues,
			Dataset::example(_, Target, AttributeValues),
			Examples
		),
		length(Examples, Count),
		evaluate_examples(Examples, Regressor, 0.0, SumSquaredError, 0.0, SumAbsoluteError),
		MeanSquaredError is SumSquaredError / Count,
		RMSE is sqrt(MeanSquaredError),
		MAE is SumAbsoluteError / Count.

	evaluate_examples([], _, SumSquaredError, SumSquaredError, SumAbsoluteError, SumAbsoluteError).
	evaluate_examples([Target-AttributeValues| Examples], Regressor, SumSquaredError0, SumSquaredError, SumAbsoluteError0, SumAbsoluteError) :-
		gaussian_process_regression::predict(Regressor, AttributeValues, Prediction),
		Error is Prediction - Target,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		SumAbsoluteError1 is SumAbsoluteError0 + abs(Error),
		evaluate_examples(Examples, Regressor, SumSquaredError1, SumSquaredError, SumAbsoluteError1, SumAbsoluteError).

:- end_object.
