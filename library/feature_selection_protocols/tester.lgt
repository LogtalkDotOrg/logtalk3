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
	logtalk_load(format(loader)),
	logtalk_load(options(loader)),
	logtalk_load(dictionaries(loader)),
	logtalk_load(random(loader)),
	logtalk_load([
		feature_dataset_protocol,
		feature_scoring_protocol,
		feature_scoring_common,
		feature_discretization,
		feature_redundancy,
		variance_score,
		correlation_score,
		anova_f_score,
		fisher_score,
		mutual_information_score,
		chi_square_score,
		chi_square_yates_score,
		symmetrical_uncertainty_score,
		cramers_v_score,
		cramers_v_bias_corrected_score,
		feature_selector_protocol,
		feature_selector_common,
		filter_feature_selector_common,
		relief_feature_selector_common
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load([
		'test_datasets/bad_feature_dataset',
		'test_datasets/feature_demo',
		'test_datasets/inconsistent_example_count',
		'test_datasets/no_examples',
		'test_datasets/regression_demo',
		test_objects
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
