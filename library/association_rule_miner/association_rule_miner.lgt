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


:- object(association_rule_miner,
	imports(association_rule_miner_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Association rule miner deriving confidence- and lift-scored rules from frequent itemsets and sequential patterns.',
		see_also is [association_rule_miner_protocol, pattern_miner_protocol, transaction_dataset_protocol, sequence_dataset_protocol]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	:- uses(type, [
		valid/2
	]).

	mine(Dataset, PatternMiner, AssociationRuleMiner, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^check_rule_options(Options),
		check_source_pattern_miner(PatternMiner, SourceMiner, ItemDomain, Patterns),
		^^rule_kind(SourceMiner, RuleKind),
		check_item_domain(ItemDomain),
		mine_rules(RuleKind, Dataset, ItemDomain, Patterns, Options, DatasetSize, Candidates),
		length(Candidates, CandidateRuleCount),
		score_candidates(Candidates, DatasetSize, Options, Rules0),
		sort_rules(Rules0, Rules),
		AssociationRuleMiner = association_rule_miner(SourceMiner, ItemDomain, DatasetSize, CandidateRuleCount, Rules, Options).

	mine_rules(itemset, Dataset, ItemDomain, Patterns, Options, DatasetSize, Candidates) :-
		check_dataset_item_domain(Dataset, ItemDomain),
		findall(Id-Transaction, Dataset::transaction(Id, Transaction), Transactions),
		check_transactions(Dataset, ItemDomain, Transactions),
		length(Transactions, DatasetSize),
		itemset_candidates(Patterns, Options, RawCandidates),
		itemset_support_index(RawCandidates, Patterns, Transactions, SupportIndex),
		verify_itemset_source_supports(Patterns, SupportIndex),
		resolve_candidates(RawCandidates, SupportIndex, Candidates).
	mine_rules(sequence, Dataset, ItemDomain, Patterns, Options, DatasetSize, Candidates) :-
		check_dataset_item_domain(Dataset, ItemDomain),
		findall(Id-Sequence, Dataset::sequence(Id, Sequence), Sequences),
		check_sequences(Dataset, ItemDomain, Sequences),
		length(Sequences, DatasetSize),
		sequence_candidates(Patterns, Options, RawCandidates),
		sequence_support_index(RawCandidates, Patterns, Sequences, SupportIndex),
		verify_sequence_source_supports(Patterns, SupportIndex),
		resolve_candidates(RawCandidates, SupportIndex, Candidates).

	check_source_pattern_miner(PatternMiner, SourceMiner, ItemDomain, Patterns) :-
		(	nonvar(PatternMiner),
			PatternMiner =.. [SourceMiner, ItemDomain, Patterns, PatternOptions],
			valid(list(compound), PatternOptions) ->
			true
		;	domain_error(pattern_miner, PatternMiner)
		).

	check_dataset_item_domain(Dataset, ItemDomain) :-
		Dataset::items(DatasetItemDomain),
		check_item_domain(DatasetItemDomain),
		(	DatasetItemDomain == ItemDomain ->
			true
		;	domain_error(pattern_miner_item_domain, ItemDomain)
		).

	itemset_candidates([], _Options, []).
	itemset_candidates([itemset(Items, Support)| Patterns], Options, Candidates) :-
		check_source_support(Support),
		check_canonical_items(Items),
		itemset_pattern_candidates(Items, Support, Options, PatternCandidates),
		append(PatternCandidates, RestCandidates, Candidates),
		itemset_candidates(Patterns, Options, RestCandidates).

	itemset_pattern_candidates(Items, Support, Options, Candidates) :-
		length(Items, RuleLength),
		^^option(maximum_rule_length(MaximumRuleLength), Options),
		(	RuleLength >= 2, RuleLength =< MaximumRuleLength ->
			proper_itemset_partitions(Items, Partitions),
			filter_itemset_partitions(Partitions, Items, Support, Options, Candidates)
		;	Candidates = []
		).

	proper_itemset_partitions(Items, Partitions) :-
		findall(Antecedent-Consequent, nonempty_itemset_partition(Items, Antecedent, Consequent), Partitions).

	nonempty_itemset_partition(Items, Antecedent, Consequent) :-
		itemset_partition(Items, Antecedent, Consequent),
		Antecedent = [_| _],
		Consequent = [_| _].

	itemset_partition([], [], []).
	itemset_partition([Item| Items], [Item| Antecedent], Consequent) :-
		itemset_partition(Items, Antecedent, Consequent).
	itemset_partition([Item| Items], Antecedent, [Item| Consequent]) :-
		itemset_partition(Items, Antecedent, Consequent).

	filter_itemset_partitions([], _Items, _Support, _Options, []).
	filter_itemset_partitions([Antecedent-Consequent| Partitions], Items, Support, Options, Candidates) :-
		length(Consequent, ConsequentLength),
		(	consequent_length_allowed(ConsequentLength, Options) ->
			Candidates = [raw_candidate(Antecedent, Consequent, Items, Support)| RestCandidates]
		;	Candidates = RestCandidates
		),
		filter_itemset_partitions(Partitions, Items, Support, Options, RestCandidates).

	sequence_candidates([], _Options, []).
	sequence_candidates([sequence_pattern(Pattern, Support)| Patterns], Options, Candidates) :-
		check_source_support(Support),
		check_canonical_sequence(Pattern),
		sequence_pattern_candidates(Pattern, Support, Options, PatternCandidates),
		append(PatternCandidates, RestCandidates, Candidates),
		sequence_candidates(Patterns, Options, RestCandidates).

	sequence_pattern_candidates(Pattern, Support, Options, Candidates) :-
		pattern_length(Pattern, RuleLength),
		^^option(maximum_rule_length(MaximumRuleLength), Options),
		(	Pattern = [_FirstEvent, _SecondEvent| _Events], RuleLength =< MaximumRuleLength ->
			sequence_splits(Pattern, Splits),
			filter_sequence_splits(Splits, Pattern, Support, Options, Candidates)
		;	Candidates = []
		).

	sequence_splits(Pattern, Splits) :-
		findall(Antecedent-Consequent, event_boundary_split(Pattern, Antecedent, Consequent), Splits).

	event_boundary_split(Pattern, Antecedent, Consequent) :-
		append(Antecedent, Consequent, Pattern),
		Antecedent = [_| _],
		Consequent = [_| _].

	filter_sequence_splits([], _Pattern, _Support, _Options, []).
	filter_sequence_splits([Antecedent-Consequent| Splits], Pattern, Support, Options, Candidates) :-
		pattern_length(Consequent, ConsequentLength),
		(	consequent_length_allowed(ConsequentLength, Options) ->
			Candidates = [raw_candidate(Antecedent, Consequent, Pattern, Support)| RestCandidates]
		;	Candidates = RestCandidates
		),
		filter_sequence_splits(Splits, Pattern, Support, Options, RestCandidates).

	consequent_length_allowed(ConsequentLength, Options) :-
		^^option(minimum_consequent_length(MinimumConsequentLength), Options),
		^^option(maximum_consequent_length(MaximumConsequentLength), Options),
		ConsequentLength >= MinimumConsequentLength,
		ConsequentLength =< MaximumConsequentLength.

	itemset_support_index(Candidates, Patterns, Transactions, SupportIndex) :-
		required_patterns(Candidates, RequiredPatterns0),
		itemset_source_patterns(Patterns, SourcePatterns),
		append(SourcePatterns, RequiredPatterns0, AllRequiredPatterns),
		sort(AllRequiredPatterns, RequiredPatterns),
		count_itemset_patterns(RequiredPatterns, Transactions, SupportIndex).

	sequence_support_index(Candidates, Patterns, Sequences, SupportIndex) :-
		required_patterns(Candidates, RequiredPatterns0),
		sequence_source_patterns(Patterns, SourcePatterns),
		append(SourcePatterns, RequiredPatterns0, AllRequiredPatterns),
		sort(AllRequiredPatterns, RequiredPatterns),
		count_sequence_patterns(RequiredPatterns, Sequences, SupportIndex).

	itemset_source_patterns([], []).
	itemset_source_patterns([itemset(Items, _Support)| Patterns], [Items| SourcePatterns]) :-
		itemset_source_patterns(Patterns, SourcePatterns).

	sequence_source_patterns([], []).
	sequence_source_patterns([sequence_pattern(Pattern, _Support)| Patterns], [Pattern| SourcePatterns]) :-
		sequence_source_patterns(Patterns, SourcePatterns).

	required_patterns([], []).
	required_patterns([raw_candidate(Antecedent, Consequent, Pattern, _Support)| Candidates], [Antecedent, Consequent, Pattern| Patterns]) :-
		required_patterns(Candidates, Patterns).

	verify_itemset_source_supports([], _SupportIndex).
	verify_itemset_source_supports([itemset(Items, RecordedSupport)| Patterns], SupportIndex) :-
		memberchk(Items-Support, SupportIndex),
		(	Support =:= RecordedSupport ->
			true
		;	domain_error(pattern_support, Items-RecordedSupport)
		),
		verify_itemset_source_supports(Patterns, SupportIndex).

	verify_sequence_source_supports([], _SupportIndex).
	verify_sequence_source_supports([sequence_pattern(Pattern, RecordedSupport)| Patterns], SupportIndex) :-
		memberchk(Pattern-Support, SupportIndex),
		(	Support =:= RecordedSupport ->
			true
		;	domain_error(pattern_support, Pattern-RecordedSupport)
		),
		verify_sequence_source_supports(Patterns, SupportIndex).

	count_itemset_patterns(Patterns, Transactions, SupportIndex) :-
		initialize_pattern_supports(Patterns, SupportIndex0),
		count_itemset_transactions(Transactions, SupportIndex0, SupportIndex).

	count_itemset_transactions([], SupportIndex, SupportIndex).
	count_itemset_transactions([_Id-Transaction| Transactions], SupportIndex0, SupportIndex) :-
		update_itemset_supports(SupportIndex0, Transaction, SupportIndex1),
		count_itemset_transactions(Transactions, SupportIndex1, SupportIndex).

	update_itemset_supports([], _Transaction, []).
	update_itemset_supports([Pattern-Support0| Supports0], Transaction, [Pattern-Support| Supports]) :-
		(	ordered_subset(Pattern, Transaction) ->
			Support is Support0 + 1
		;	Support = Support0
		),
		update_itemset_supports(Supports0, Transaction, Supports).

	ordered_subset([], _Items).
	ordered_subset([Item| Items], [OtherItem| OtherItems]) :-
		compare(Order, Item, OtherItem),
		(	Order == (=) ->
			ordered_subset(Items, OtherItems)
		;	Order == (>) ->
			ordered_subset([Item| Items], OtherItems)
		;	fail
		).

	count_sequence_patterns(Patterns, Sequences, SupportIndex) :-
		initialize_pattern_supports(Patterns, SupportIndex0),
		count_dataset_sequences(Sequences, SupportIndex0, SupportIndex).

	count_dataset_sequences([], SupportIndex, SupportIndex).
	count_dataset_sequences([_Id-Sequence| Sequences], SupportIndex0, SupportIndex) :-
		update_sequence_supports(SupportIndex0, Sequence, SupportIndex1),
		count_dataset_sequences(Sequences, SupportIndex1, SupportIndex).

	update_sequence_supports([], _Sequence, []).
	update_sequence_supports([Pattern-Support0| Supports0], Sequence, [Pattern-Support| Supports]) :-
		(	pattern_in_sequence(Pattern, Sequence) ->
			Support is Support0 + 1
		;	Support = Support0
		),
		update_sequence_supports(Supports0, Sequence, Supports).

	initialize_pattern_supports([], []).
	initialize_pattern_supports([Pattern| Patterns], [Pattern-0| Supports]) :-
		initialize_pattern_supports(Patterns, Supports).

	pattern_in_sequence([], _Sequence).
	pattern_in_sequence([Itemset| Pattern], Sequence) :-
		select_matching_itemset(Itemset, Sequence, RestSequence),
		pattern_in_sequence(Pattern, RestSequence).

	select_matching_itemset(Itemset, [OtherItemset| Sequence], Sequence) :-
		ordered_subset(Itemset, OtherItemset),
		!.
	select_matching_itemset(Itemset, [_OtherItemset| Sequence0], Sequence) :-
		select_matching_itemset(Itemset, Sequence0, Sequence).

	resolve_candidates([], _SupportIndex, []).
	resolve_candidates([raw_candidate(Antecedent, Consequent, Pattern, RecordedSupport)| RawCandidates], SupportIndex, [candidate(Antecedent, Consequent, Support, AntecedentSupport, ConsequentSupport)| Candidates]) :-
		memberchk(Pattern-Support, SupportIndex),
		(	Support =:= RecordedSupport ->
			true
		;	domain_error(pattern_support, Pattern-RecordedSupport)
		),
		memberchk(Antecedent-AntecedentSupport, SupportIndex),
		memberchk(Consequent-ConsequentSupport, SupportIndex),
		resolve_candidates(RawCandidates, SupportIndex, Candidates).

	score_candidates([], _DatasetSize, _Options, []).
	score_candidates([candidate(Antecedent, Consequent, Support, AntecedentSupport, ConsequentSupport)| Candidates], DatasetSize, Options, Rules) :-
		Confidence is Support / AntecedentSupport,
		Lift is DatasetSize * Support / (AntecedentSupport * ConsequentSupport),
		^^option(minimum_confidence(MinimumConfidence), Options),
		^^option(minimum_lift(MinimumLift), Options),
		(	Confidence >= MinimumConfidence, Lift >= MinimumLift ->
			Rules = [association_rule(Antecedent, Consequent, Support, AntecedentSupport, ConsequentSupport, Confidence, Lift)| RestRules]
		;	Rules = RestRules
		),
		score_candidates(Candidates, DatasetSize, Options, RestRules).

	sort_rules(Rules0, Rules) :-
		decorate_rules(Rules0, DecoratedRules),
		keysort(DecoratedRules, SortedDecoratedRules),
		undecorate_rules(SortedDecoratedRules, Rules).

	decorate_rules([], []).
	decorate_rules([Rule| Rules], [key(RuleLength, Antecedent, Consequent)-Rule| DecoratedRules]) :-
		Rule = association_rule(Antecedent, Consequent, _Support, _AntecedentSupport, _ConsequentSupport, _Confidence, _Lift),
		pattern_length(Antecedent, AntecedentLength),
		pattern_length(Consequent, ConsequentLength),
		RuleLength is AntecedentLength + ConsequentLength,
		decorate_rules(Rules, DecoratedRules).

	undecorate_rules([], []).
	undecorate_rules([_Key-Rule| DecoratedRules], [Rule| Rules]) :-
		undecorate_rules(DecoratedRules, Rules).

	check_association_rule_miner(AssociationRuleMiner) :-
		(	var(AssociationRuleMiner) ->
			instantiation_error
		;	valid_association_rule_miner_term(AssociationRuleMiner) ->
			true
		;	domain_error(association_rule_miner, AssociationRuleMiner)
		).

	valid_association_rule_miner_term(association_rule_miner(SourceMiner, ItemDomain, DatasetSize, CandidateRuleCount, Rules, Options)) :-
		^^rule_kind(SourceMiner, RuleKind),
		catch(check_item_domain(ItemDomain), _Error, fail),
		integer(DatasetSize), DatasetSize > 0,
		integer(CandidateRuleCount), CandidateRuleCount >= 0,
		catch(^^check_options(Options), _Error, fail),
		catch(^^check_rule_options(Options), _Error, fail),
		valid_rules(Rules, RuleKind, ItemDomain, DatasetSize, Options),
		length(Rules, RuleCount),
		RuleCount =< CandidateRuleCount,
		sort_rules(Rules, Rules).

	valid_rules([], _RuleKind, _ItemDomain, _DatasetSize, _Options).
	valid_rules([association_rule(Antecedent, Consequent, Support, AntecedentSupport, ConsequentSupport, Confidence, Lift)| Rules], RuleKind, ItemDomain, DatasetSize, Options) :-
		valid_rule_side(RuleKind, Antecedent, ItemDomain),
		valid_rule_side(RuleKind, Consequent, ItemDomain),
		valid_rule_relation(RuleKind, Antecedent, Consequent),
		integer(Support), Support > 0, Support =< DatasetSize,
		integer(AntecedentSupport), AntecedentSupport >= Support, AntecedentSupport =< DatasetSize,
		integer(ConsequentSupport), ConsequentSupport >= Support, ConsequentSupport =< DatasetSize,
		number(Confidence), Confidence >= 0.0, Confidence =< 1.0,
		number(Lift), Lift >= 0.0,
		ExpectedConfidence is Support / AntecedentSupport,
		ExpectedLift is DatasetSize * Support / (AntecedentSupport * ConsequentSupport),
		Confidence =:= ExpectedConfidence,
		Lift =:= ExpectedLift,
		^^option(minimum_confidence(MinimumConfidence), Options),
		^^option(minimum_lift(MinimumLift), Options),
		Confidence >= MinimumConfidence,
		Lift >= MinimumLift,
		pattern_length(Antecedent, AntecedentLength),
		pattern_length(Consequent, ConsequentLength),
		RuleLength is AntecedentLength + ConsequentLength,
		^^option(maximum_rule_length(MaximumRuleLength), Options),
		RuleLength =< MaximumRuleLength,
		consequent_length_allowed(ConsequentLength, Options),
		valid_rules(Rules, RuleKind, ItemDomain, DatasetSize, Options).

	valid_rule_side(itemset, Items, ItemDomain) :-
		Items = [_| _],
		catch(check_canonical_items(Items), _Error, fail),
		all_declared_items(Items, ItemDomain).
	valid_rule_side(sequence, Pattern, ItemDomain) :-
		Pattern = [_| _],
		catch(check_canonical_sequence(Pattern), _Error, fail),
		all_declared_events(Pattern, ItemDomain).

	valid_rule_relation(itemset, Antecedent, Consequent) :-
		disjoint_items(Antecedent, Consequent).
	valid_rule_relation(sequence, _Antecedent, _Consequent).

	disjoint_items([], _Items).
	disjoint_items([Item| Items], OtherItems) :-
		\+ member(Item, OtherItems),
		disjoint_items(Items, OtherItems).

	check_transactions(Dataset, _ItemDomain, []) :-
		domain_error(non_empty_dataset, Dataset).
	check_transactions(_Dataset, ItemDomain, Transactions) :-
		check_unique_ids(Transactions),
		check_transaction_list(Transactions, ItemDomain).

	check_transaction_list([], _ItemDomain).
	check_transaction_list([_Id-Transaction| Transactions], ItemDomain) :-
		check_canonical_items(Transaction),
		all_declared_items(Transaction, ItemDomain),
		check_transaction_list(Transactions, ItemDomain).

	check_sequences(Dataset, _ItemDomain, []) :-
		domain_error(non_empty_dataset, Dataset).
	check_sequences(_Dataset, ItemDomain, Sequences) :-
		check_unique_ids(Sequences),
		check_sequence_list(Sequences, ItemDomain).

	check_sequence_list([], _ItemDomain).
	check_sequence_list([_Id-Sequence| Sequences], ItemDomain) :-
		(	Sequence == [] ->
			domain_error(non_empty_sequence, [])
		;	true
		),
		check_canonical_sequence(Sequence),
		all_declared_events(Sequence, ItemDomain),
		check_sequence_list(Sequences, ItemDomain).

	check_unique_ids(Pairs) :-
		findall(Id, member(Id-_, Pairs), Ids),
		sort(Ids, UniqueIds),
		length(Ids, IdCount),
		length(UniqueIds, UniqueIdCount),
		(	IdCount =:= UniqueIdCount ->
			true
		;	domain_error(unique_dataset_ids, Ids)
		).

	check_item_domain(ItemDomain) :-
		check_canonical_items(ItemDomain).

	check_canonical_items(Items) :-
		sort(Items, SortedItems),
		(	Items == SortedItems ->
			true
		;	domain_error(canonical_items, Items)
		),
		check_atoms(Items).

	check_atoms([]).
	check_atoms([Item| Items]) :-
		(	var(Item) ->
			instantiation_error
		;	atom(Item) ->
			true
		;	type_error(atom, Item)
		),
		check_atoms(Items).

	check_canonical_sequence([]).
	check_canonical_sequence([Itemset| Sequence]) :-
		(	Itemset == [] ->
			domain_error(non_empty_itemset, [])
		;	true
		),
		check_canonical_items(Itemset),
		check_canonical_sequence(Sequence).

	all_declared_events([], _ItemDomain).
	all_declared_events([Itemset| Events], ItemDomain) :-
		all_declared_items(Itemset, ItemDomain),
		all_declared_events(Events, ItemDomain).

	all_declared_items([], _ItemDomain).
	all_declared_items([Item| Items], ItemDomain) :-
		(	member(Item, ItemDomain) ->
			true
		;	domain_error(item, Item)
		),
		all_declared_items(Items, ItemDomain).

	check_source_support(Support) :-
		(	integer(Support),
			Support > 0 ->
			true
		;	domain_error(pattern_support, Support)
		).

	pattern_length([], 0).
	pattern_length([Item| Items], PatternLength) :-
		(	Item = [_| _] ->
			length(Item, ItemLength)
		;	ItemLength = 1
		),
		pattern_length(Items, RestLength),
		PatternLength is ItemLength + RestLength.

	print_association_rule_miner(association_rule_miner(SourceMiner, ItemDomain, DatasetSize, CandidateRuleCount, Rules, Options)) :-
		format('Association Rule Miner~n', []),
		format('======================~n~n', []),
		format('Source miner: ~w~n', [SourceMiner]),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Dataset size: ~w~n', [DatasetSize]),
		format('Candidate rule count: ~w~n', [CandidateRuleCount]),
		format('Options: ~w~n', [Options]),
		format('Rules: ~w~n', [Rules]).

:- end_object.
