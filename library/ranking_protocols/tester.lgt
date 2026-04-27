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
	logtalk_load([
		ranker_protocol,
		ranker_common,
		score_ranker_common,
		glicko2_common,
		condorcet_victory_common,
		ranking_dataset_common,
		ranking_dataset_protocol,
		pairwise_ranking_dataset_protocol,
		temporal_pairwise_ranking_dataset_protocol
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load([
		'test_datasets/head_to_head',
		'test_datasets/regular_head_to_head',
		'test_datasets/condorcet_divergence_pairwise',
		'test_datasets/sparse_preferences',
		'test_datasets/disconnected_pairwise',
		'test_datasets/cyclic_pairwise',
		'test_datasets/search_results',
		'test_datasets/tied_grouped',
		'test_datasets/sparse_grouped_relevance',
		'test_datasets/temporal_two_period_chain',
		'test_datasets/temporal_draws',
		'test_datasets/temporal_idle_periods',
		'test_datasets/temporal_late_activity',
		'test_datasets/disconnected_temporal_pairwise',
		'test_datasets/malformed_duplicate_periods',
		'test_datasets/malformed_temporal_unknown_period',
		'test_datasets/malformed_temporal_unknown_item',
		'test_datasets/malformed_temporal_self_game',
		'test_datasets/malformed_temporal_illegal_score',
		'test_datasets/malformed_pairwise',
		'test_datasets/malformed_grouped',
		'test_datasets/malformed_duplicate_items',
		'test_datasets/malformed_self_preference',
		'test_datasets/malformed_non_positive_weight'
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
