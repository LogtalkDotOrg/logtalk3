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
	logtalk_load(dictionaries(loader)),
	logtalk_load(format(loader)),
	logtalk_load(options(loader)),
	logtalk_load(combinations(loader)),
	logtalk_load(frequent_pattern_mining_protocols(loader)),
	logtalk_load([
		frequent_pattern_mining_protocols('test_datasets/dense_shared_prefix_baskets'),
		frequent_pattern_mining_protocols('test_datasets/invalid_duplicate_id_baskets'),
		frequent_pattern_mining_protocols('test_datasets/deep_intersection_baskets'),
		frequent_pattern_mining_protocols('test_datasets/invalid_duplicate_item_baskets'),
		frequent_pattern_mining_protocols('test_datasets/invalid_empty_baskets'),
		frequent_pattern_mining_protocols('test_datasets/invalid_item_domain_baskets'),
		frequent_pattern_mining_protocols('test_datasets/invalid_undeclared_item_baskets'),
		frequent_pattern_mining_protocols('test_datasets/invalid_unsorted_transaction_baskets'),
		frequent_pattern_mining_protocols('test_datasets/market_basket_basics'),
		frequent_pattern_mining_protocols('test_datasets/layered_baskets'),
		frequent_pattern_mining_protocols('test_datasets/non_monotone_id_baskets')
	], [
		source_data(on),
		debug(on)
	]),
	logtalk_load(eclat_pattern_miner, [source_data(on), debug(on)]),
	logtalk_load(apriori_pattern_miner(loader)),
	logtalk_load(fp_growth_pattern_miner(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
