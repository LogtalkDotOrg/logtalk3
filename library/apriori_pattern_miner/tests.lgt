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
		comment is 'Unit tests for the "apriori_pattern_miner" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(apriori_pattern_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(apriori_mine_2_market_basket_basics, deterministic(ground(PatternMiner))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner).

	test(apriori_mine_2_structure, deterministic(functor(PatternMiner, apriori_pattern_miner, 3))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner).

	test(apriori_mine_3_default_patterns, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, butter], 3), itemset([bread, milk], 4), itemset([butter, milk], 3)])) :-
		apriori_pattern_miner::mine(market_basket_basics, apriori_pattern_miner(_ItemDomain, Patterns, _Options)).

	test(apriori_mine_3_support_count_threshold, deterministic(Patterns == [itemset([bread], 5), itemset([butter], 4), itemset([milk], 5), itemset([bread, milk], 4)])) :-
		apriori_pattern_miner::mine(market_basket_basics, apriori_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]).

	test(apriori_mine_3_pattern_length_filters, deterministic(Patterns == [itemset([bread, milk], 4)])) :-
		apriori_pattern_miner::mine(market_basket_basics, apriori_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(2), maximum_pattern_length(2)]).

		test(apriori_mine_3_layered_baskets_pair, deterministic(Support == 4)) :-
		apriori_pattern_miner::mine(layered_baskets, apriori_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(3), maximum_pattern_length(2)]),
				memberchk(itemset([bread, diapers], Support), Patterns).

		test(apriori_mine_3_deep_intersection_quadruple, deterministic(Support == 4)) :-
		apriori_pattern_miner::mine(deep_intersection_baskets, apriori_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4)]),
				memberchk(itemset([alpha, beta, delta, gamma], Support), Patterns).

	test(apriori_mine_3_deep_intersection_maximum_length, deterministic(Patterns == [itemset([alpha, beta, delta, gamma], 4)])) :-
		apriori_pattern_miner::mine(deep_intersection_baskets, apriori_pattern_miner(_ItemDomain, Patterns, _Options), [minimum_support_count(4), minimum_pattern_length(4), maximum_pattern_length(4)]).

	test(apriori_matches_eclat_deep_intersection, deterministic(AprioriPatterns == EclatPatterns)) :-
		Options = [minimum_support_count(4)],
		apriori_pattern_miner::mine(deep_intersection_baskets, apriori_pattern_miner(_ItemDomain, AprioriPatterns, _Options), Options),
		eclat_pattern_miner::mine(deep_intersection_baskets, eclat_pattern_miner(_EclatDomain, EclatPatterns, _EclatOptions), Options).

		test(apriori_diagnostics_2, deterministic([Model, CandidateGeneration, CandidateCounting, PatternLengthHistogram] == [apriori_pattern_miner, join_prune, hash_tree, [1-3, 2-1]])) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		apriori_pattern_miner::diagnostics(PatternMiner, Diagnostics),
				memberchk(model(Model), Diagnostics),
				memberchk(candidate_generation(CandidateGeneration), Diagnostics),
				memberchk(candidate_counting(CandidateCounting), Diagnostics),
				memberchk(pattern_length_histogram(PatternLengthHistogram), Diagnostics).

	test(apriori_valid_pattern_miner_1, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		apriori_pattern_miner::valid_pattern_miner(PatternMiner).

	test(apriori_export_to_clauses_4, deterministic(functor(Clause, mined_patterns, 3))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		apriori_pattern_miner::export_to_clauses(market_basket_basics, PatternMiner, mined_patterns, [Clause]).

		test(apriori_export_to_file_4, deterministic(Support == 4)) :-
		^^file_path('test_output.pl', File),
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4)]),
		apriori_pattern_miner::export_to_file(market_basket_basics, PatternMiner, apriori_exported_patterns, File),
		logtalk_load(File),
		{apriori_exported_patterns(_ItemDomain, Patterns, _Options)},
				memberchk(itemset([bread, milk], Support), Patterns).

	test(apriori_print_pattern_miner_1, deterministic) :-
		^^suppress_text_output,
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		apriori_pattern_miner::print_pattern_miner(PatternMiner).

	test(apriori_mine_2_invalid_undeclared_item_dataset, error(domain_error(item, butter))) :-
		apriori_pattern_miner::mine(invalid_undeclared_item_baskets, _PatternMiner).

	test(apriori_mine_2_invalid_unsorted_transaction_dataset, error(domain_error(canonical_transaction, [milk, bread]))) :-
		apriori_pattern_miner::mine(invalid_unsorted_transaction_baskets, _PatternMiner).

	test(apriori_mine_2_invalid_duplicate_item_dataset, error(domain_error(canonical_transaction, [bread, bread, milk]))) :-
		apriori_pattern_miner::mine(invalid_duplicate_item_baskets, _PatternMiner).

	test(apriori_mine_2_invalid_duplicate_id_dataset, error(domain_error(unique_transaction_ids, [1, 1]))) :-
		apriori_pattern_miner::mine(invalid_duplicate_id_baskets, _PatternMiner).

	test(apriori_mine_2_invalid_empty_dataset, error(domain_error(non_empty_dataset, invalid_empty_baskets))) :-
		apriori_pattern_miner::mine(invalid_empty_baskets, _PatternMiner).

	test(apriori_mine_2_invalid_item_domain_dataset, error(domain_error(canonical_item_domain, [milk, bread, bread]))) :-
		apriori_pattern_miner::mine(invalid_item_domain_baskets, _PatternMiner).

:- end_object.
