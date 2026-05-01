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
		date is 2026-05-01,
		comment is 'Performance and reference-fit benchmarks for the "random_forest_regression" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	test(step_signal_reference_fit, true, [note(metrics(train_seconds-TrainTime, rmse-RMSE, mae-MAE))]) :-
		reference_fit(step_signal, [number_of_trees(11), maximum_features_per_split(1), minimum_samples_leaf(2)], 0.75, 0.50, TrainTime, RMSE, MAE).

	test(mixed_signal_reference_fit, true, [note(metrics(train_seconds-TrainTime, rmse-RMSE, mae-MAE))]) :-
		reference_fit(mixed_signal, [number_of_trees(21), maximum_features_per_split(3)], 5.0, 4.0, TrainTime, RMSE, MAE).

	test(intercept_only_reference_fit, true, [note(metrics(train_seconds-TrainTime, rmse-RMSE, mae-MAE))]) :-
		reference_fit(intercept_only, [number_of_trees(5)], 0.01, 0.01, TrainTime, RMSE, MAE).

	reference_fit(Dataset, Options, MaximumRMSE, MaximumMAE, TrainTime, RMSE, MAE) :-
		benchmark(benchmark_learn(Dataset, Options), TrainTime),
		learn_regressor(Dataset, Options, Regressor),
		dataset_metrics(Dataset, Regressor, RMSE, MAE),
		RMSE =< MaximumRMSE,
		MAE =< MaximumMAE.

	benchmark_learn(Dataset, Options) :-
		fast_random::randomize(12345),
		random_forest_regression::learn(Dataset, _Regressor, Options).

	learn_regressor(Dataset, Options, Regressor) :-
		fast_random::randomize(12345),
		random_forest_regression::learn(Dataset, Regressor, Options).

	dataset_metrics(Dataset, Regressor, RMSE, MAE) :-
		findall(Target-AttributeValues, Dataset::example(_, Target, AttributeValues), Examples),
		length(Examples, Count),
		evaluate_examples(Examples, Regressor, 0.0, SumSquaredError, 0.0, SumAbsoluteError),
		MeanSquaredError is SumSquaredError / Count,
		RMSE is sqrt(MeanSquaredError),
		MAE is SumAbsoluteError / Count.

	evaluate_examples([], _, SumSquaredError, SumSquaredError, SumAbsoluteError, SumAbsoluteError).
	evaluate_examples([Target-AttributeValues| Examples], Regressor, SumSquaredError0, SumSquaredError, SumAbsoluteError0, SumAbsoluteError) :-
		random_forest_regression::predict(Regressor, AttributeValues, Prediction),
		Error is Prediction - Target,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		SumAbsoluteError1 is SumAbsoluteError0 + abs(Error),
		evaluate_examples(Examples, Regressor, SumSquaredError1, SumSquaredError, SumAbsoluteError1, SumAbsoluteError).

:- end_object.
