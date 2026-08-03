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


:- category(association_rule_miner_common,
	implements(association_rule_miner_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Shared predicates for association rule miner diagnostics, options, validation, and export.'
	]).

	:- protected(rule_kind/2).
	:- mode(rule_kind(+atom, -atom), one).
	:- info(rule_kind/2, [
		comment is 'Returns the rule kind implied by a supported source pattern miner.',
		argnames is ['SourceMiner', 'RuleKind']
	]).

	:- protected(check_rule_options/1).
	:- mode(check_rule_options(+list(compound)), one).
	:- info(check_rule_options/1, [
		comment is 'Checks cross-option constraints for effective association rule mining options.',
		argnames is ['Options']
	]).

	:- uses(format, [
		format/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	mine(Dataset, PatternMiner, AssociationRuleMiner) :-
		::mine(Dataset, PatternMiner, AssociationRuleMiner, []).

	diagnostics(association_rule_miner(SourceMiner, ItemDomain, DatasetSize, CandidateRuleCount, Rules, Options), Diagnostics) :-
		::rule_kind(SourceMiner, RuleKind),
		length(ItemDomain, ItemDomainSize),
		length(Rules, RuleCount),
		rule_metric_ranges(Rules, MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift),
		Diagnostics = [
			model(association_rule_miner),
			source_model(SourceMiner),
			rule_kind(RuleKind),
			options(Options),
			item_domain_size(ItemDomainSize),
			dataset_size(DatasetSize),
			candidate_rule_count(CandidateRuleCount),
			rule_count(RuleCount),
			confidence_range(MinimumConfidence, MaximumConfidence),
			lift_range(MinimumLift, MaximumLift)
		].

	diagnostic(AssociationRuleMiner, Diagnostic) :-
		diagnostics(AssociationRuleMiner, Diagnostics),
		member(Diagnostic, Diagnostics).

	association_rule_miner_options(association_rule_miner(_SourceMiner, _ItemDomain, _DatasetSize, _CandidateRuleCount, _Rules, Options), Options).

	rule_metric(AssociationRuleMiner, Rule, Metric) :-
		::check_association_rule_miner(AssociationRuleMiner),
		AssociationRuleMiner = association_rule_miner(_SourceMiner, _ItemDomain, DatasetSize, _CandidateRuleCount, Rules, _Options),
		memberchk(Rule, Rules),
		Rule = association_rule(_Antecedent, _Consequent, Support, AntecedentSupport, ConsequentSupport, _Confidence, _Lift),
		rule_metric_value(Metric, DatasetSize, Support, AntecedentSupport, ConsequentSupport).

	rule_metric_value(leverage(Leverage), DatasetSize, Support, AntecedentSupport, ConsequentSupport) :-
		Leverage is (DatasetSize * Support - AntecedentSupport * ConsequentSupport) / (DatasetSize * DatasetSize).
	rule_metric_value(jaccard(Jaccard), _DatasetSize, Support, AntecedentSupport, ConsequentSupport) :-
		Jaccard is Support / (AntecedentSupport + ConsequentSupport - Support).
	rule_metric_value(cosine(Cosine), _DatasetSize, Support, AntecedentSupport, ConsequentSupport) :-
		Cosine is Support / sqrt(AntecedentSupport * ConsequentSupport).
	rule_metric_value(kulczynski(Kulczynski), _DatasetSize, Support, AntecedentSupport, ConsequentSupport) :-
		Kulczynski is 0.5 * (Support / AntecedentSupport + Support / ConsequentSupport).

	valid_association_rule_miner(AssociationRuleMiner) :-
		catch(::check_association_rule_miner(AssociationRuleMiner), _Error, fail).

	export_to_clauses(_Dataset, association_rule_miner(SourceMiner, ItemDomain, DatasetSize, CandidateRuleCount, Rules, Options), Functor, [Clause]) :-
		Clause =.. [Functor, SourceMiner, ItemDomain, DatasetSize, CandidateRuleCount, Rules, Options].

	export_to_file(Dataset, AssociationRuleMiner, Functor, File) :-
		::export_to_clauses(Dataset, AssociationRuleMiner, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Clauses, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	check_rule_options(Options) :-
		^^option(minimum_consequent_length(MinimumConsequentLength), Options),
		^^option(maximum_consequent_length(MaximumConsequentLength), Options),
		( MinimumConsequentLength =< MaximumConsequentLength ->
			true
		; domain_error(consequent_length_range, MinimumConsequentLength-MaximumConsequentLength)
		).

	rule_kind(apriori_pattern_miner, itemset).
	rule_kind(eclat_pattern_miner, itemset).
	rule_kind(fp_growth_pattern_miner, itemset).
	rule_kind(gsp_pattern_miner, sequence).
	rule_kind(prefix_span_pattern_miner, sequence).
	rule_kind(spade_pattern_miner, sequence).
	rule_kind(clo_span_pattern_miner, sequence).

	rule_metric_ranges([], 0.0, 0.0, 0.0, 0.0).
	rule_metric_ranges([association_rule(_Antecedent, _Consequent, _Support, _AntecedentSupport, _ConsequentSupport, Confidence, Lift)| Rules], MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift) :-
		rule_metric_ranges(Rules, Confidence, Confidence, Lift, Lift, MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift).

	rule_metric_ranges([], MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift, MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift).
	rule_metric_ranges([association_rule(_Antecedent, _Consequent, _Support, _AntecedentSupport, _ConsequentSupport, Confidence, Lift)| Rules], MinimumConfidence0, MaximumConfidence0, MinimumLift0, MaximumLift0, MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift) :-
		MinimumConfidence1 is min(MinimumConfidence0, Confidence),
		MaximumConfidence1 is max(MaximumConfidence0, Confidence),
		MinimumLift1 is min(MinimumLift0, Lift),
		MaximumLift1 is max(MaximumLift0, Lift),
		rule_metric_ranges(Rules, MinimumConfidence1, MaximumConfidence1, MinimumLift1, MaximumLift1, MinimumConfidence, MaximumConfidence, MinimumLift, MaximumLift).

	write_comment_header([Clause| _Clauses], Stream) :-
		format(Stream, '% ~q~n', [Clause]).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

	default_option(minimum_confidence(0.5)).
	default_option(minimum_lift(0.0)).
	default_option(maximum_rule_length(1000)).
	default_option(minimum_consequent_length(1)).
	default_option(maximum_consequent_length(1000)).

	valid_option(minimum_confidence(MinimumConfidence)) :-
		number(MinimumConfidence),
		MinimumConfidence >= 0.0,
		MinimumConfidence =< 1.0.
	valid_option(minimum_lift(MinimumLift)) :-
		number(MinimumLift),
		MinimumLift >= 0.0.
	valid_option(maximum_rule_length(MaximumRuleLength)) :-
		valid(positive_integer, MaximumRuleLength).
	valid_option(minimum_consequent_length(MinimumConsequentLength)) :-
		valid(positive_integer, MinimumConsequentLength).
	valid_option(maximum_consequent_length(MaximumConsequentLength)) :-
		valid(positive_integer, MaximumConsequentLength).

:- end_category.
