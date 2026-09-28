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


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(types(loader)),
	logtalk_load(deques(loader)),
	logtalk_load(format(loader)),
	logtalk_load(options(loader)),
	logtalk_load(statistics(loader)),
	logtalk_load(random(loader)),
	logtalk_load(local_optimization(loader)),
	logtalk_load(differential_evolution(loader)),
	logtalk_load(time_series_protocols(loader)),
	logtalk_load([
		time_series_protocols('test_datasets/gap_index'),
		time_series_protocols('test_datasets/linear_trend'),
		time_series_protocols('test_datasets/non_numeric_value'),
		time_series_protocols('test_datasets/short_series'),
		'test_datasets/constant_series',
		'test_datasets/seasonal_additive',
		'test_datasets/seasonal_edge_cases',
		'test_datasets/seasonal_multiplicative',
		'test_datasets/seasonal_invalid',
		'test_datasets/missing_observations',
		'test_datasets/online_updates',
		exponential_smoothing_common,
		exponential_smoothing_problem,
		exponential_smoothing
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
