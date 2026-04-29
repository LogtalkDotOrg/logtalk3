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
	logtalk_load(basic_types(loader)),
	logtalk_load(pattern_mining_protocols(loader)),
	logtalk_load([
		sequence_dataset_protocol,
		sequential_pattern_mining_common
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load([
		'test_datasets/border_threshold_sequences',
		'test_datasets/branching_sequences',
		'test_datasets/clickstream_sequences',
		'test_datasets/closure_sequences',
		'test_datasets/dense_overlap_sequences',
		'test_datasets/invalid_duplicate_id_sequences',
		'test_datasets/invalid_duplicate_item_in_event_sequences',
		'test_datasets/invalid_empty_event_sequences',
		'test_datasets/invalid_undeclared_item_sequences',
		'test_datasets/invalid_unsorted_itemset_sequences',
		'test_datasets/prefix_ladder_sequences',
		'test_datasets/repeated_embedding_sequences',
		'test_datasets/same_event_vs_next_event_sequences'
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
