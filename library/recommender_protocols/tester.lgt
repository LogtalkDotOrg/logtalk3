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
	logtalk_load([
		rating_dataset_protocol,
		item_content_dataset_protocol,
		similarity_metric_protocol,
		similarity_metric_common,
		cosine_similarity,
		pearson_similarity,
		jaccard_similarity,
		msd_similarity,
		spearman_similarity,
		recommender_protocol,
		recommender_common
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load([
		'test_datasets/duplicate_rating',
		'test_datasets/inconsistent_rating_count',
		'test_datasets/movie_ratings',
		'test_datasets/no_ratings',
		'test_datasets/non_numeric_rating',
		'test_datasets/out_of_scale_rating',
		test_objects
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
