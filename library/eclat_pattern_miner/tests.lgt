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
		comment is 'Unit tests for the "eclat_pattern_miner" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(eclat_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(eclat_mine_2_market_basket_basics, deterministic(ground(PatternMiner))) :-
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner).

	test(eclat_mine_2_structure, deterministic(functor(PatternMiner, eclat_pattern_miner, 3))) :-
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner).

	test(eclat_mine_3_default_patterns, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, butter], 3), itemset([bread, milk], 4), itemset([butter, milk], 3)])) :-
		eclat_pattern_miner::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, Patterns, _Options)).

	test(eclat_mine_3_support_count_threshold, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, milk], 4)])) :-
		eclat_pattern_miner::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]).

	test(eclat_mine_3_pattern_length_filters, deterministic(Patterns == [itemset([bread, milk], 4)])) :-
		eclat_pattern_miner::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(2), maximum_pattern_length(2)]).

		test(eclat_mine_3_layered_baskets_pair, deterministic(Support == 4)) :-
		eclat_pattern_miner::mine(layered_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]),
				memberchk(itemset([bread, diapers], Support), Patterns).

	test(eclat_mine_3_non_monotone_transaction_ids, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, butter], 3), itemset([bread, milk], 4), itemset([butter, milk], 3)])) :-
		eclat_pattern_miner::mine(non_monotone_id_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options)).

		test(eclat_mine_3_deep_intersection_quadruple, deterministic(Support == 4)) :-
		eclat_pattern_miner::mine(deep_intersection_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]),
				memberchk(itemset([alpha, beta, delta, gamma], Support), Patterns).

	test(eclat_mine_3_deep_intersection_maximum_length, deterministic(Patterns == [itemset([alpha, beta, delta, gamma], 4)])) :-
		eclat_pattern_miner::mine(deep_intersection_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(4), maximum_pattern_length(4)]).

	test(eclat_mine_3_dense_shared_prefix_quads, deterministic([Support1, Support2, Support3] == [5, 5, 5])) :-
		eclat_pattern_miner::mine(dense_shared_prefix_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(5)]),
		memberchk(itemset([alpha, beta, delta, epsilon], Support1), Patterns),
		memberchk(itemset([alpha, beta, delta, gamma], Support2), Patterns),
		memberchk(itemset([alpha, beta, epsilon, gamma], Support3), Patterns).

	test(eclat_matches_apriori, deterministic(EclatPatterns == AprioriPatterns)) :-
		eclat_pattern_miner::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, EclatPatterns, _Options), [minimum_support_count(3)]),
		apriori_pattern_miner::mine(market_basket_basics, apriori_pattern_miner(_AprioriDomain, AprioriPatterns, _AprioriOptions), [minimum_support_count(3)]).

	test(eclat_matches_apriori_non_monotone_transaction_ids, deterministic(EclatPatterns == AprioriPatterns)) :-
		Options = [minimum_support_count(3)],
		eclat_pattern_miner::mine(non_monotone_id_baskets, eclat_pattern_miner(_ItemDomain, EclatPatterns, _Options), Options),
		apriori_pattern_miner::mine(non_monotone_id_baskets, apriori_pattern_miner(_AprioriDomain, AprioriPatterns, _AprioriOptions), Options).

	test(eclat_matches_fp_growth_dense_shared_prefix, deterministic(EclatPatterns == FPGrowthPatterns)) :-
		Options = [minimum_support_count(5)],
		eclat_pattern_miner::mine(dense_shared_prefix_baskets, eclat_pattern_miner(_ItemDomain, EclatPatterns, _Options), Options),
		fp_growth_pattern_miner::mine(dense_shared_prefix_baskets, fp_growth_pattern_miner(_FPGrowthDomain, FPGrowthPatterns, _FPGrowthOptions), Options).

	test(eclat_matches_fp_growth, deterministic(EclatPatterns == FPGrowthPatterns)) :-
		eclat_pattern_miner::mine(layered_baskets, eclat_pattern_miner(_ItemDomain, EclatPatterns, _Options), [minimum_support_count(3)]),
		fp_growth_pattern_miner::mine(layered_baskets, fp_growth_pattern_miner(_FPGrowthDomain, FPGrowthPatterns, _FPGrowthOptions), [minimum_support_count(3)]).

		test(eclat_diagnostics_2, deterministic([Model, ExtensionOperator, SupportLayout] == [eclat_pattern_miner, tidset_intersection, vertical_tidsets])) :-
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		eclat_pattern_miner::diagnostics(PatternMiner, Diagnostics),
				memberchk(model(Model), Diagnostics),
				memberchk(extension_operator(ExtensionOperator), Diagnostics),
				memberchk(support_layout(SupportLayout), Diagnostics).

	test(eclat_valid_pattern_miner_1, deterministic) :-
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		eclat_pattern_miner::valid_pattern_miner(PatternMiner).

	test(eclat_invalid_pattern_miner_1, fail) :-
		PatternMiner = eclat_pattern_miner([bread], [itemset([bread], foo)], [minimum_support(0.5)]),
		eclat_pattern_miner::valid_pattern_miner(PatternMiner).

	test(eclat_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		eclat_pattern_miner::export_to_clauses(market_basket_basics, PatternMiner, mined_patterns, [Clause]).

		test(eclat_export_to_file_4, deterministic(Support == 4)) :-
		^^file_path('test_output.pl', File),
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		eclat_pattern_miner::export_to_file(market_basket_basics, PatternMiner, eclat_exported_patterns, File),
		logtalk_load(File),
		{eclat_exported_patterns(_ItemDomain, Patterns, _Options)},
				memberchk(itemset([bread, milk], Support), Patterns).

	test(eclat_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		eclat_pattern_miner::mine(market_basket_basics, PatternMiner),
		eclat_pattern_miner::print_pattern_miner(PatternMiner).

	test(eclat_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, butter))) :-
		eclat_pattern_miner::mine(invalid_undeclared_item_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_unsorted_transaction_dataset, error(domain_error(canonical_transaction, [milk, bread]))) :-
		eclat_pattern_miner::mine(invalid_unsorted_transaction_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_transaction, [bread, bread, milk]))) :-
		eclat_pattern_miner::mine(invalid_duplicate_item_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_transaction_ids, [1, 1]))) :-
		eclat_pattern_miner::mine(invalid_duplicate_id_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_empty_dataset, error(domain_error(non_empty_dataset, invalid_empty_baskets))) :-
		eclat_pattern_miner::mine(invalid_empty_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_item_domain_dataset, error(domain_error(canonical_item_domain, [milk, bread, bread]))) :-
		eclat_pattern_miner::mine(invalid_item_domain_baskets, _PatternMiner).

:- end_object.
