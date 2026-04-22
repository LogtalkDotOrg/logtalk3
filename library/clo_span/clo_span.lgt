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


:- object(clo_span,
	imports(sequential_pattern_mining_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-22,
		comment is 'Closed sequential pattern miner that filters frequent sequential patterns to the closed subset.',
		remarks is [
			'Algorithm' - 'Mines the frequent sequential patterns and retains only those patterns that have no strict superpattern with the same support.',
			'Dataset handling' - 'Requires a dataset implementing ``sequence_dataset_protocol`` with sequences represented as ordered lists of canonical sorted itemsets over a declared item domain.',
			'Support thresholds' - 'Supports minimum support specified either as a relative proportion or as an absolute count. When both are given, the absolute-count threshold takes precedence.',
			'Pattern miner representation' - 'The mined result is represented by default as ``clo_span_pattern_miner(ItemDomain, Patterns, Options)`` where ``Patterns`` stores closed ``sequence_pattern(Pattern, SupportCount)`` terms ordered first by total item count and then lexicographically.'
		],
		see_also is [pattern_miner_protocol, sequence_dataset_protocol, prefix_span, gsp, spade]
	]).

	:- public(mine/3).
	:- mode(mine(+object_identifier, -compound, +list(compound)), one).
	:- info(mine/3, [
		comment is 'Mines closed frequent sequential patterns from the given sequence dataset using the specified options.',
		argnames is ['Dataset', 'PatternMiner', 'Options']
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	mine(Dataset, PatternMiner, UserOptions) :-
		prefix_span::mine(Dataset, prefix_span_pattern_miner(ItemDomain, FrequentPatterns, Options), UserOptions),
		closed_patterns(FrequentPatterns, ClosedPatterns),
		PatternMiner = clo_span_pattern_miner(ItemDomain, ClosedPatterns, Options),
		!.

	closed_patterns([], []).
	closed_patterns([sequence_pattern(Pattern, Support)| Patterns], ClosedPatterns) :-
		(   pattern_is_closed(Pattern, Support, Patterns) ->
			ClosedPatterns = [sequence_pattern(Pattern, Support)| RestClosedPatterns]
		;   ClosedPatterns = RestClosedPatterns
		),
		closed_patterns(Patterns, RestClosedPatterns).

	pattern_is_closed(_Pattern, _Support, []).
	pattern_is_closed(Pattern, Support, [sequence_pattern(OtherPattern, OtherSupport)| Patterns]) :-
		(   Support =:= OtherSupport,
			Pattern \== OtherPattern,
			strict_superpattern(OtherPattern, Pattern) ->
			fail
		;   pattern_is_closed(Pattern, Support, Patterns)
		).

	strict_superpattern(Superpattern, Pattern) :-
		^^pattern_length(Superpattern, SuperpatternLength),
		^^pattern_length(Pattern, PatternLength),
		SuperpatternLength > PatternLength,
		pattern_in_pattern(Pattern, Superpattern).

	pattern_in_pattern([], _Superpattern).
	pattern_in_pattern([Itemset| Pattern], Superpattern) :-
		select_matching_itemset(Itemset, Superpattern, RestSuperpattern),
		pattern_in_pattern(Pattern, RestSuperpattern).

	select_matching_itemset(Itemset, [OtherItemset| Superpattern], Superpattern) :-
		itemset_subset(Itemset, OtherItemset),
		!.
	select_matching_itemset(Itemset, [_OtherItemset| Superpattern0], Superpattern) :-
		select_matching_itemset(Itemset, Superpattern0, Superpattern).

	itemset_subset([], _OtherItemset).
	itemset_subset([Item| Items], OtherItemset) :-
		memberchk(Item, OtherItemset),
		itemset_subset(Items, OtherItemset).

	pattern_miner_export_template(_Dataset, clo_span_pattern_miner(ItemDomain, Patterns, Options), Functor, Template) :-
		Template =.. [Functor, ItemDomain, Patterns, Options].

	print_pattern_miner(clo_span_pattern_miner(ItemDomain, Patterns, Options)) :-
		format('CloSpan Pattern Miner~n', []),
		format('====================~n~n', []),
		format('Item domain: ~w~n', [ItemDomain]),
		format('Options: ~w~n', [Options]),
		format('Patterns: ~w~n', [Patterns]).

:- end_object.
