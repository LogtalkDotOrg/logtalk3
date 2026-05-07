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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "fp_growth_pattern_miner" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(fp_growth_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(fp_growth_mine_2_market_basket_basics, deterministic(ground(PatternMiner))) :-
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner).

	test(fp_growth_mine_2_structure, deterministic(functor(PatternMiner, fp_growth_pattern_miner, 3))) :-
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner).

	test(fp_growth_mine_3_default_patterns, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, butter], 3), itemset([bread, milk], 4), itemset([butter, milk], 3)])) :-
		fp_growth_pattern_miner::mine(market_basket_basics, fp_growth_pattern_miner(_ItemDomain, Patterns, _Options)).

	test(fp_growth_mine_3_support_count_threshold, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, milk], 4)])) :-
		fp_growth_pattern_miner::mine(market_basket_basics, fp_growth_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]).

	test(fp_growth_mine_3_pattern_length_filters, deterministic(Patterns == [itemset([bread, milk], 4)])) :-
		fp_growth_pattern_miner::mine(market_basket_basics, fp_growth_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(2), maximum_pattern_length(2)]).

	test(fp_growth_mine_3_layered_baskets_pair, deterministic(Support == 4)) :-
		fp_growth_pattern_miner::mine(layered_baskets, fp_growth_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]),
		memberchk(itemset([bread, diapers], Support), Patterns).

	test(fp_growth_mine_3_dense_shared_prefix_quads, deterministic([Support1, Support2, Support3] == [5, 5, 5])) :-
		fp_growth_pattern_miner::mine(dense_shared_prefix_baskets, fp_growth_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(5)]),
		memberchk(itemset([alpha, beta, delta, epsilon], Support1), Patterns),
		memberchk(itemset([alpha, beta, delta, gamma], Support2), Patterns),
		memberchk(itemset([alpha, beta, epsilon, gamma], Support3), Patterns).

	test(fp_growth_matches_apriori_dense_shared_prefix, deterministic(FPGrowthPatterns == AprioriPatterns)) :-
		Options = [minimum_support_count(5)],
		fp_growth_pattern_miner::mine(dense_shared_prefix_baskets, fp_growth_pattern_miner(_ItemDomain, FPGrowthPatterns, _Options), Options),
		apriori_pattern_miner::mine(dense_shared_prefix_baskets, apriori_pattern_miner(_AprioriDomain, AprioriPatterns, _AprioriOptions), Options).

	test(fp_growth_diagnostics_2, deterministic([Model, Compression, SupportLayout, ProjectionAccess] == [fp_growth_pattern_miner, prefix_tree_sharing, fp_tree, header_table_parent_links])) :-
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		fp_growth_pattern_miner::diagnostics(PatternMiner, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(compression(Compression), Diagnostics),
		memberchk(support_layout(SupportLayout), Diagnostics),
		memberchk(projection_access(ProjectionAccess), Diagnostics).

	test(fp_growth_valid_pattern_miner_1, deterministic) :-
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		fp_growth_pattern_miner::valid_pattern_miner(PatternMiner).

	test(fp_growth_invalid_pattern_miner_1, fail) :-
		PatternMiner = fp_growth_pattern_miner([bread], [itemset([bread], foo)], [minimum_support(0.5)]),
		fp_growth_pattern_miner::valid_pattern_miner(PatternMiner).

	test(fp_growth_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		fp_growth_pattern_miner::export_to_clauses(market_basket_basics, PatternMiner, mined_patterns, [Clause]).

		test(fp_growth_export_to_file_4, deterministic(Support == 4)) :-
		^^file_path('test_output.pl', File),
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		fp_growth_pattern_miner::export_to_file(market_basket_basics, PatternMiner, fp_growth_exported_patterns, File),
		logtalk_load(File),
		{fp_growth_exported_patterns(_ItemDomain, Patterns, _Options)},
		memberchk(itemset([bread, milk], Support), Patterns).

	test(fp_growth_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		fp_growth_pattern_miner::mine(market_basket_basics, PatternMiner),
		fp_growth_pattern_miner::print_pattern_miner(PatternMiner).

	test(fp_growth_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, butter))) :-
		fp_growth_pattern_miner::mine(invalid_undeclared_item_baskets, _PatternMiner).

	test(fp_growth_mine_2_invalid_unsorted_transaction_dataset, error(domain_error(canonical_transaction, [milk, bread]))) :-
		fp_growth_pattern_miner::mine(invalid_unsorted_transaction_baskets, _PatternMiner).

	test(fp_growth_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_transaction, [bread, bread, milk]))) :-
		fp_growth_pattern_miner::mine(invalid_duplicate_item_baskets, _PatternMiner).

	test(fp_growth_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_transaction_ids, [1, 1]))) :-
		fp_growth_pattern_miner::mine(invalid_duplicate_id_baskets, _PatternMiner).

	test(fp_growth_mine_2_invalid_empty_dataset, error(domain_error(non_empty_dataset, invalid_empty_baskets))) :-
		fp_growth_pattern_miner::mine(invalid_empty_baskets, _PatternMiner).

	test(fp_growth_mine_2_invalid_item_domain_dataset, error(domain_error(canonical_item_domain, [milk, bread, bread]))) :-
		fp_growth_pattern_miner::mine(invalid_item_domain_baskets, _PatternMiner).

:- end_object.
