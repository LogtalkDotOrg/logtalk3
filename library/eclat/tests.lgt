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
		date is 2026-04-22,
		comment is 'Unit tests for the "eclat" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(eclat).

	cleanup :-
		^^clean_file('test_output.pl').

	test(eclat_mine_2_market_basket_basics, deterministic(ground(PatternMiner))) :-
		eclat::mine(market_basket_basics, PatternMiner).

	test(eclat_mine_2_structure, deterministic(functor(PatternMiner, eclat_pattern_miner, 3))) :-
		eclat::mine(market_basket_basics, PatternMiner).

	test(eclat_mine_3_default_patterns, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, butter], 3), itemset([bread, milk], 4), itemset([butter, milk], 3)])) :-
		eclat::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, Patterns, _Options)).

	test(eclat_mine_3_support_count_threshold, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, milk], 4)])) :-
		eclat::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]).

	test(eclat_mine_3_pattern_length_filters, deterministic(Patterns == [itemset([bread, milk], 4)])) :-
		eclat::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(2), maximum_pattern_length(2)]).

	test(eclat_mine_3_layered_baskets_pair, deterministic(memberchk(itemset([bread, diapers], 4), Patterns))) :-
		eclat::mine(layered_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]).

	test(eclat_mine_3_deep_intersection_quadruple, deterministic(memberchk(itemset([alpha, beta, delta, gamma], 4), Patterns))) :-
		eclat::mine(deep_intersection_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]).

	test(eclat_mine_3_deep_intersection_maximum_length, deterministic(Patterns == [itemset([alpha, beta, delta, gamma], 4)])) :-
		eclat::mine(deep_intersection_baskets, eclat_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(4), maximum_pattern_length(4)]).

	test(eclat_matches_apriori, deterministic(EclatPatterns == AprioriPatterns)) :-
		eclat::mine(market_basket_basics, eclat_pattern_miner(_ItemDomain, EclatPatterns, _Options), [minimum_support_count(3)]),
		apriori::mine(market_basket_basics, apriori_pattern_miner(_AprioriDomain, AprioriPatterns, _AprioriOptions), [minimum_support_count(3)]).

	test(eclat_matches_fp_growth, deterministic(EclatPatterns == FPGrowthPatterns)) :-
		eclat::mine(layered_baskets, eclat_pattern_miner(_ItemDomain, EclatPatterns, _Options), [minimum_support_count(3)]),
		fp_growth::mine(layered_baskets, fp_growth_pattern_miner(_FPGrowthDomain, FPGrowthPatterns, _FPGrowthOptions), [minimum_support_count(3)]).

	test(eclat_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		eclat::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		eclat::export_to_clauses(market_basket_basics, PatternMiner, mined_patterns, [Clause]).

	test(eclat_export_to_file_4, deterministic(memberchk(itemset([bread, milk], 4), Patterns))) :-
		^^file_path('test_output.pl', File),
		eclat::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		eclat::export_to_file(market_basket_basics, PatternMiner, eclat_exported_patterns, File),
		logtalk_load(File),
		{eclat_exported_patterns(_ItemDomain, Patterns, _Options)}.

	test(eclat_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		eclat::mine(market_basket_basics, PatternMiner),
		eclat::print_pattern_miner(PatternMiner).

	test(eclat_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, butter))) :-
		eclat::mine(invalid_undeclared_item_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_unsorted_transaction_dataset, error(domain_error(canonical_transaction, [milk, bread]))) :-
		eclat::mine(invalid_unsorted_transaction_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_transaction, [bread, bread, milk]))) :-
		eclat::mine(invalid_duplicate_item_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_empty_dataset, error(domain_error(non_empty_dataset, invalid_empty_baskets))) :-
		eclat::mine(invalid_empty_baskets, _PatternMiner).

	test(eclat_mine_2_invalid_item_domain_dataset, error(domain_error(canonical_item_domain, [milk, bread, bread]))) :-
		eclat::mine(invalid_item_domain_baskets, _PatternMiner).

:- end_object.
