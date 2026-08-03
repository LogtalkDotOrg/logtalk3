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
		date is 2026-08-03,
		comment is 'Unit tests for the "association_rule_miner" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	cover(association_rule_miner).

	cleanup :-
		^^clean_file('test_output.pl').

	test(mine_3_itemset_structure, deterministic(functor(AssociationRuleMiner, association_rule_miner, 6))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner).

	test(itemset_rule_metrics, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options)),
		memberchk(association_rule([bread], [milk], 4, 5, 5, Confidence, Lift), Rules),
		^^approximately_equal(Confidence, 0.8, 1.0e-12),
		^^approximately_equal(Lift, 0.96, 1.0e-12).

	test(itemset_directional_rules, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options)),
		memberchk(association_rule([bread], [butter], 3, 5, 4, _Confidence1, _Lift1), Rules),
		memberchk(association_rule([butter], [bread], 3, 4, 5, _Confidence2, _Lift2), Rules).

	test(itemset_all_proper_partitions, deterministic(CandidateRuleCount == 14)) :-
		PatternMiner = apriori_pattern_miner([alpha, beta, delta, epsilon, gamma], [itemset([alpha, beta, delta, gamma], 4)], []),
		association_rule_miner::mine(deep_intersection_baskets, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, CandidateRuleCount, _Rules, _Options), [minimum_confidence(0.0)]).

	test(itemset_filtered_source_supports, deterministic(memberchk(association_rule([bread], [milk], 4, 5, 5, _Confidence, _Lift), Rules))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner, [minimum_support_count(4), minimum_pattern_length(2), maximum_pattern_length(2)]),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options)).

	test(itemset_miner_parity, deterministic([AprioriRules, EclatRules, FpGrowthRules] == [EclatRules, FpGrowthRules, AprioriRules])) :-
		MiningOptions = [minimum_support_count(4)],
		apriori_pattern_miner::mine(market_basket_basics, AprioriPatternMiner, MiningOptions),
		eclat_pattern_miner::mine(market_basket_basics, EclatPatternMiner, MiningOptions),
		fp_growth_pattern_miner::mine(market_basket_basics, FpGrowthPatternMiner, MiningOptions),
		association_rule_miner::mine(market_basket_basics, AprioriPatternMiner, association_rule_miner(_AprioriSource, _AprioriDomain, _AprioriSize, _AprioriCount, AprioriRules, _AprioriOptions)),
		association_rule_miner::mine(market_basket_basics, EclatPatternMiner, association_rule_miner(_EclatSource, _EclatDomain, _EclatSize, _EclatCount, EclatRules, _EclatOptions)),
		association_rule_miner::mine(market_basket_basics, FpGrowthPatternMiner, association_rule_miner(_FpGrowthSource, _FpGrowthDomain, _FpGrowthSize, _FpGrowthCount, FpGrowthRules, _FpGrowthOptions)).

	test(sequence_repeated_embeddings_count_once, deterministic) :-
		prefix_span_pattern_miner::mine(repeated_embedding_sequences, PatternMiner, [minimum_support_count(2)]),
		association_rule_miner::mine(repeated_embedding_sequences, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, 4, _CandidateRuleCount, Rules, _Options)),
		memberchk(association_rule([[a]], [[b]], 2, 3, 3, Confidence, Lift), Rules),
		ExpectedConfidence is 2.0 / 3.0,
		ExpectedLift is 8.0 / 9.0,
		^^approximately_equal(Confidence, ExpectedConfidence, 1.0e-12),
		^^approximately_equal(Lift, ExpectedLift, 1.0e-12).

	test(sequence_event_boundary_splits_only, deterministic(CandidateRuleCount == 1)) :-
		PatternMiner = prefix_span_pattern_miner([a, b, c], [sequence_pattern([[a, b], [c]], 1)], []),
		association_rule_miner::mine(same_event_vs_next_event_sequences, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, CandidateRuleCount, Rules, _Options), [minimum_confidence(0.0)]),
		Rules = [association_rule([[a, b]], [[c]], 1, 3, 2, _Confidence, _Lift)].

	test(sequence_miner_parity, deterministic([GspRules, PrefixSpanRules, SpadeRules] == [PrefixSpanRules, SpadeRules, GspRules])) :-
		MiningOptions = [minimum_support_count(2), maximum_pattern_length(3)],
		gsp_pattern_miner::mine(repeated_embedding_sequences, GspPatternMiner, MiningOptions),
		prefix_span_pattern_miner::mine(repeated_embedding_sequences, PrefixSpanPatternMiner, MiningOptions),
		spade_pattern_miner::mine(repeated_embedding_sequences, SpadePatternMiner, MiningOptions),
		association_rule_miner::mine(repeated_embedding_sequences, GspPatternMiner, association_rule_miner(_GspSource, _GspDomain, _GspSize, _GspCount, GspRules, _GspOptions)),
		association_rule_miner::mine(repeated_embedding_sequences, PrefixSpanPatternMiner, association_rule_miner(_PrefixSpanSource, _PrefixSpanDomain, _PrefixSpanSize, _PrefixSpanCount, PrefixSpanRules, _PrefixSpanOptions)),
		association_rule_miner::mine(repeated_embedding_sequences, SpadePatternMiner, association_rule_miner(_SpadeSource, _SpadeDomain, _SpadeSize, _SpadeCount, SpadeRules, _SpadeOptions)).

	test(clo_span_closed_patterns, deterministic) :-
		clo_span_pattern_miner::mine(closure_sequences, PatternMiner, [minimum_support_count(2)]),
		association_rule_miner::mine(closure_sequences, PatternMiner, AssociationRuleMiner, [minimum_confidence(0.0)]),
		association_rule_miner::valid_association_rule_miner(AssociationRuleMiner).

	test(minimum_confidence_inclusive, deterministic(memberchk(association_rule([bread], [milk], 4, 5, 5, _Confidence, _Lift), Rules))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options), [minimum_confidence(0.8)]).

	test(minimum_lift_filters, deterministic(\+ member(association_rule([bread], [milk], 4, 5, 5, _Confidence, _Lift), Rules))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options), [minimum_lift(1.0)]).

	test(maximum_rule_length_filters, deterministic(Rules == [])) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options), [maximum_rule_length(1)]).

	test(consequent_length_filters, deterministic(CandidateRuleCount == 3)) :-
		PatternMiner = apriori_pattern_miner([alpha, beta, delta, epsilon, gamma], [itemset([alpha, beta, delta], 5)], []),
		association_rule_miner::mine(deep_intersection_baskets, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, CandidateRuleCount, _Rules, _Options), [minimum_confidence(0.0), minimum_consequent_length(2), maximum_consequent_length(2)]).

	test(empty_filtered_output, deterministic(Rules == [])) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options), [minimum_lift(10.0)]).

	test(diagnostics_2, deterministic([Model, SourceModel, RuleKind, DatasetSize] == [association_rule_miner, apriori_pattern_miner, itemset, 6])) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		association_rule_miner::diagnostics(AssociationRuleMiner, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(source_model(SourceModel), Diagnostics),
		memberchk(rule_kind(RuleKind), Diagnostics),
		memberchk(dataset_size(DatasetSize), Diagnostics).

	test(options_2, deterministic(memberchk(minimum_confidence(0.7), Options))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner, [minimum_confidence(0.7)]),
		association_rule_miner::association_rule_miner_options(AssociationRuleMiner, Options).

	test(rule_metric_3_leverage, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(association_rule([bread], [milk], 4, 5, 5, Confidence, Lift), Rules),
		Rule = association_rule([bread], [milk], 4, 5, 5, Confidence, Lift),
		association_rule_miner::rule_metric(AssociationRuleMiner, Rule, leverage(Leverage)),
		ExpectedLeverage is -1.0 / 36.0,
		^^approximately_equal(Leverage, ExpectedLeverage, 1.0e-12).

	test(rule_metric_3_jaccard, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(association_rule([bread], [milk], 4, 5, 5, Confidence, Lift), Rules),
		Rule = association_rule([bread], [milk], 4, 5, 5, Confidence, Lift),
		association_rule_miner::rule_metric(AssociationRuleMiner, Rule, jaccard(Jaccard)),
		ExpectedJaccard is 2.0 / 3.0,
		^^approximately_equal(Jaccard, ExpectedJaccard, 1.0e-12).

	test(rule_metric_3_cosine, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(association_rule([bread], [milk], 4, 5, 5, Confidence, Lift), Rules),
		Rule = association_rule([bread], [milk], 4, 5, 5, Confidence, Lift),
		association_rule_miner::rule_metric(AssociationRuleMiner, Rule, cosine(Cosine)),
		^^approximately_equal(Cosine, 0.8, 1.0e-12).

	test(rule_metric_3_kulczynski, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(association_rule([bread], [butter], 3, 5, 4, Confidence, Lift), Rules),
		Rule = association_rule([bread], [butter], 3, 5, 4, Confidence, Lift),
		association_rule_miner::rule_metric(AssociationRuleMiner, Rule, kulczynski(Kulczynski)),
		^^approximately_equal(Kulczynski, 0.675, 1.0e-12).

	test(rule_metric_3_enumeration_leverage, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(Rule, Rules),
		findall(Metric, association_rule_miner::rule_metric(AssociationRuleMiner, Rule, Metric), Metrics),
		memberchk(leverage(Leverage), Metrics),
		Rule = association_rule(_Antecedent, _Consequent, Support, AntecedentSupport, ConsequentSupport, _Confidence, _Lift),
		ExpectedLeverage is (DatasetSize * Support - AntecedentSupport * ConsequentSupport) / (DatasetSize * DatasetSize),
		^^approximately_equal(Leverage, ExpectedLeverage, 1.0e-12).

	test(rule_metric_3_enumeration_jaccard, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(Rule, Rules),
		findall(Metric, association_rule_miner::rule_metric(AssociationRuleMiner, Rule, Metric), Metrics),
		memberchk(jaccard(Jaccard), Metrics),
		Rule = association_rule(_Antecedent, _Consequent, Support, AntecedentSupport, ConsequentSupport, _Confidence, _Lift),
		ExpectedJaccard is Support / (AntecedentSupport + ConsequentSupport - Support),
		^^approximately_equal(Jaccard, ExpectedJaccard, 1.0e-12).

	test(rule_metric_3_enumeration_cosine, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(Rule, Rules),
		findall(Metric, association_rule_miner::rule_metric(AssociationRuleMiner, Rule, Metric), Metrics),
		memberchk(cosine(Cosine), Metrics),
		Rule = association_rule(_Antecedent, _Consequent, Support, AntecedentSupport, ConsequentSupport, _Confidence, _Lift),
		ExpectedCosine is Support / sqrt(AntecedentSupport * ConsequentSupport),
		^^approximately_equal(Cosine, ExpectedCosine, 1.0e-12).

	test(rule_metric_3_enumeration_kulczynski, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(Rule, Rules),
		findall(Metric, association_rule_miner::rule_metric(AssociationRuleMiner, Rule, Metric), Metrics),
		memberchk(kulczynski(Kulczynski), Metrics),
		Rule = association_rule(_Antecedent, _Consequent, Support, AntecedentSupport, ConsequentSupport, _Confidence, _Lift),
		ExpectedKulczynski is 0.5 * (Support / AntecedentSupport + Support / ConsequentSupport),
		^^approximately_equal(Kulczynski, ExpectedKulczynski, 1.0e-12).

	test(rule_metric_3_rule_not_found, fail) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		association_rule_miner::rule_metric(AssociationRuleMiner, association_rule([bread], [eggs], 1, 5, 2, 0.2, 0.6), leverage(_Leverage)).

	test(rule_metric_3_unsupported_metric, fail) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(Rule, Rules),
		association_rule_miner::rule_metric(AssociationRuleMiner, Rule, conviction(_Conviction)).

	test(valid_association_rule_miner_1, deterministic) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		association_rule_miner::valid_association_rule_miner(AssociationRuleMiner).

	test(check_association_rule_miner_1_variable, error(instantiation_error)) :-
		association_rule_miner::check_association_rule_miner(_AssociationRuleMiner).

	test(check_association_rule_miner_1_inconsistent_metrics, error(domain_error(association_rule_miner, AssociationRuleMiner))) :-
		AssociationRuleMiner = association_rule_miner(apriori_pattern_miner, [bread, milk], 6, 1, [association_rule([bread], [milk], 4, 5, 5, 0.9, 0.96)], [minimum_confidence(0.5), minimum_lift(0.0), maximum_rule_length(1000), minimum_consequent_length(1), maximum_consequent_length(1000)]),
		association_rule_miner::check_association_rule_miner(AssociationRuleMiner).

	test(source_support_mismatch, error(domain_error(pattern_support, [bread, milk]-99))) :-
		PatternMiner = apriori_pattern_miner([bread, butter, cereal, eggs, milk], [itemset([bread, milk], 99)], []),
		association_rule_miner::mine(market_basket_basics, PatternMiner, _AssociationRuleMiner).

	test(invalid_consequent_length_range, error(domain_error(consequent_length_range, 3-2))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, _AssociationRuleMiner, [minimum_consequent_length(3), maximum_consequent_length(2)]).

	test(export_to_clauses_4, deterministic(functor(Clause, mined_rules, 6))) :-
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		association_rule_miner::export_to_clauses(market_basket_basics, AssociationRuleMiner, mined_rules, [Clause]).

	test(export_to_file_4, deterministic(memberchk(association_rule([bread], [milk], 4, 5, 5, _Confidence, _Lift), Rules))) :-
		^^file_path('test_output.pl', File),
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		association_rule_miner::export_to_file(market_basket_basics, AssociationRuleMiner, association_rules, File),
		logtalk_load(File),
		{association_rules(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, Rules, _Options)}.

	test(print_association_rule_miner_1, deterministic) :-
		^^suppress_text_output,
		apriori_pattern_miner::mine(market_basket_basics, PatternMiner),
		association_rule_miner::mine(market_basket_basics, PatternMiner, AssociationRuleMiner),
		association_rule_miner::print_association_rule_miner(AssociationRuleMiner).

:- end_object.
