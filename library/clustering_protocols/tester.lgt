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
	logtalk_load(dictionaries(loader)),
	logtalk_load(options(loader)),
	logtalk_load(reader(loader)),
	logtalk_load(statistics(loader)),
	logtalk_load([
		clusterer_protocol,
		clustering_dataset_protocol,
		clusterer_common,
		search_indexing
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load([
		'test_datasets/all_noise',
		'test_datasets/bridge_noise',
		'test_datasets/dead_component_blobs',
		'test_datasets/duplicate_points',
		'test_datasets/imbalanced_three_modes',
		'test_datasets/two_blobs',
		'test_datasets/iris_unlabeled',
		'test_datasets/large_two_blobs',
		'test_datasets/scaling_bands',
		'test_datasets/single_blob',
		'test_datasets/mixed_profiles'
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
