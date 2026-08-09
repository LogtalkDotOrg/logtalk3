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
		date is 2026-08-09,
		comment is 'Performance and reference-fit benchmarks for the "svr_regression" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2
	]).

	test(simple_line_reference_fit, true, [note(metrics(train_seconds-TrainTime, rmse-RMSE, mae-MAE))]) :-
		reference_fit(simple_line, [], 0.01, 0.01, TrainTime, RMSE, MAE).

	% The mixed_signal and sparse_mixed_signal fixtures do not reach a tight
	% fit within the default 200 iterations (RMSE around 35). The unit tests
	% check only for a well-formed default fit; this benchmark uses the 3000
	% iterations needed to reduce the RMSE to around 0.08.
	test(mixed_signal_reference_fit, true, [note(metrics(train_seconds-TrainTime, rmse-RMSE, mae-MAE))]) :-
		reference_fit(mixed_signal, [maximum_iterations(3000)], 0.25, 0.25, TrainTime, RMSE, MAE).

	test(wide_mixed_signal_reference_fit, true, [note(metrics(train_seconds-TrainTime, rmse-RMSE, mae-MAE))]) :-
		reference_fit(wide_mixed_signal, [maximum_iterations(1000)], 0.25, 0.25, TrainTime, RMSE, MAE).

	% The unit suite uses step_signal to compare the RBF and linear kernels
	% over the same iteration budget. That relative-fit assertion belongs
	% there instead of imposing an isolated RMSE threshold in this suite.

	% auxiliary predicates

	reference_fit(Dataset, Options, MaximumRMSE, MaximumMAE, TrainTime, RMSE, MAE) :-
		benchmark(svr_regression::learn(Dataset, _Regressor, Options), TrainTime),
		svr_regression::learn(Dataset, Regressor, Options),
		dataset_metrics(Dataset, Regressor, RMSE, MAE),
		RMSE =< MaximumRMSE,
		MAE =< MaximumMAE.

	dataset_metrics(Dataset, Regressor, RMSE, MAE) :-
		findall(Target-AttributeValues, Dataset::example(_, Target, AttributeValues), Examples),
		length(Examples, Count),
		evaluate_examples(Examples, Regressor, 0.0, SumSquaredError, 0.0, SumAbsoluteError),
		MeanSquaredError is SumSquaredError / Count,
		RMSE is sqrt(MeanSquaredError),
		MAE is SumAbsoluteError / Count.

	evaluate_examples([], _, SumSquaredError, SumSquaredError, SumAbsoluteError, SumAbsoluteError).
	evaluate_examples([Target-AttributeValues| Examples], Regressor, SumSquaredError0, SumSquaredError, SumAbsoluteError0, SumAbsoluteError) :-
		svr_regression::predict(Regressor, AttributeValues, Prediction),
		Error is Prediction - Target,
		SumSquaredError1 is SumSquaredError0 + Error * Error,
		SumAbsoluteError1 is SumAbsoluteError0 + abs(Error),
		evaluate_examples(Examples, Regressor, SumSquaredError1, SumSquaredError, SumAbsoluteError1, SumAbsoluteError).

:- end_object.