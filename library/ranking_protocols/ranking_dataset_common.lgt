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


:- category(ranking_dataset_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-18,
		comment is 'Shared predicates for collecting, analyzing, and validating ranking datasets.'
	]).

	:- public(pairwise_dataset_declared_items/2).
	:- mode(pairwise_dataset_declared_items(+object_identifier, -list), one).
	:- info(pairwise_dataset_declared_items/2, [
		comment is 'Collects the pairwise dataset declared items preserving declaration order.',
		argnames is ['Dataset', 'Items']
	]).

	:- public(pairwise_dataset_items/2).
	:- mode(pairwise_dataset_items(+object_identifier, -list), one).
	:- info(pairwise_dataset_items/2, [
		comment is 'Collects the unique pairwise dataset items preserving their first declaration order.',
		argnames is ['Dataset', 'Items']
	]).

	:- public(pairwise_dataset_preferences/2).
	:- mode(pairwise_dataset_preferences(+object_identifier, -list), one).
	:- info(pairwise_dataset_preferences/2, [
		comment is 'Collects the pairwise dataset preferences as ``p(Winner,Loser,Weight)`` terms.',
		argnames is ['Dataset', 'Preferences']
	]).

	:- public(pairwise_dataset_connected_components/2).
	:- mode(pairwise_dataset_connected_components(+object_identifier, -list(list)), one).
	:- info(pairwise_dataset_connected_components/2, [
		comment is 'Returns the connected components induced by the pairwise preference graph.',
		argnames is ['Dataset', 'Components']
	]).

	:- public(pairwise_dataset_summary/2).
	:- mode(pairwise_dataset_summary(+object_identifier, -list(compound)), one).
	:- info(pairwise_dataset_summary/2, [
		comment is 'Returns a summary of a pairwise dataset, including item, preference, component, and isolated-item counts.',
		argnames is ['Dataset', 'Summary']
	]).

	:- public(pairwise_dataset_win_totals/2).
	:- mode(pairwise_dataset_win_totals(+object_identifier, -list(pair)), one).
	:- info(pairwise_dataset_win_totals/2, [
		comment is 'Returns per-item total win weights for a pairwise dataset.',
		argnames is ['Dataset', 'Totals']
	]).

	:- public(pairwise_dataset_matchups/2).
	:- mode(pairwise_dataset_matchups(+object_identifier, -list(compound)), one).
	:- info(pairwise_dataset_matchups/2, [
		comment is 'Returns aggregated pairwise matchups as ``matchup(Item1,Item2,Item1Wins,Item2Wins)`` terms preserving item declaration order inside each pair.',
		argnames is ['Dataset', 'Matchups']
	]).

	:- public(grouped_dataset_groups/2).
	:- mode(grouped_dataset_groups(+object_identifier, -list), one).
	:- info(grouped_dataset_groups/2, [
		comment is 'Collects the unique grouped-dataset groups preserving their first declaration order.',
		argnames is ['Dataset', 'Groups']
	]).

	:- public(grouped_dataset_items/2).
	:- mode(grouped_dataset_items(+object_identifier, -list), one).
	:- info(grouped_dataset_items/2, [
		comment is 'Collects the unique items declared across all groups in a grouped dataset preserving their first declaration order.',
		argnames is ['Dataset', 'Items']
	]).

	:- public(grouped_dataset_items/3).
	:- mode(grouped_dataset_items(+object_identifier, +term, -list), one).
	:- info(grouped_dataset_items/3, [
		comment is 'Collects the unique items declared for a grouped-dataset group.',
		argnames is ['Dataset', 'Group', 'Items']
	]).

	:- public(grouped_dataset_summary/2).
	:- mode(grouped_dataset_summary(+object_identifier, -list(compound)), one).
	:- info(grouped_dataset_summary/2, [
		comment is 'Returns a summary of a grouped dataset, including group, item, and relevance-judgment counts.',
		argnames is ['Dataset', 'Summary']
	]).

	:- protected(grouped_dataset_item_relevances/4).
	:- mode(grouped_dataset_item_relevances(+object_identifier, +term, +atom, -list(pair)), one).
	:- info(grouped_dataset_item_relevances/4, [
		comment is 'Collects the declared items for a grouped-dataset group paired with their relevance values under the given missing-relevance policy.',
		argnames is ['Dataset', 'Group', 'MissingRelevance', 'ItemRelevances']
	]).

	:- protected(grouped_dataset_relevance_frequencies/2).
	:- mode(grouped_dataset_relevance_frequencies(+list(pair), -compound), one).
	:- info(grouped_dataset_relevance_frequencies/2, [
		comment is 'Aggregates grouped item relevances into a relevance-frequency dictionary.',
		argnames is ['ItemRelevances', 'FrequencyDictionary']
	]).

	:- protected(grouped_dataset_item_relevance_scores/3).
	:- mode(grouped_dataset_item_relevance_scores(+list(pair), +compound, -list(pair)), one).
	:- info(grouped_dataset_item_relevance_scores/3, [
		comment is 'Maps grouped item relevances to per-item score deltas using a relevance-points dictionary.',
		argnames is ['ItemRelevances', 'PointsDictionary', 'ItemScores']
	]).

	:- public(validate_pairwise_dataset/1).
	:- mode(validate_pairwise_dataset(+object_identifier), one).
	:- info(validate_pairwise_dataset/1, [
		comment is 'Validates a pairwise ranking dataset and throws an error if the dataset is malformed or disconnected.',
		argnames is ['Dataset']
	]).

	:- public(validate_pairwise_dataset/2).
	:- mode(validate_pairwise_dataset(+object_identifier, -list(compound)), one).
	:- info(validate_pairwise_dataset/2, [
		comment is 'Validates a pairwise ranking dataset and returns its dataset summary when validation succeeds.',
		argnames is ['Dataset', 'Summary']
	]).

	:- public(validate_grouped_dataset/1).
	:- mode(validate_grouped_dataset(+object_identifier), one).
	:- info(validate_grouped_dataset/1, [
		comment is 'Validates a grouped ranking dataset and throws an error if the dataset is malformed.',
		argnames is ['Dataset']
	]).

	:- public(validate_grouped_dataset/2).
	:- mode(validate_grouped_dataset(+object_identifier, -list(compound)), one).
	:- info(validate_grouped_dataset/2, [
		comment is 'Validates a grouped ranking dataset and returns its dataset summary when validation succeeds.',
		argnames is ['Dataset', 'Summary']
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, reverse/2
	]).

	:- uses(avltree, [
		as_dictionary/2,
		as_list/2 as dictionary_as_list/2,
		insert/4 as dictionary_insert/4,
		lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(numberlist, [
		sum/2
	]).

	pairwise_dataset_declared_items(Dataset, Items) :-
		findall(Item, Dataset::item(Item), Items).

	pairwise_dataset_items(Dataset, Items) :-
		pairwise_dataset_declared_items(Dataset, RawItems),
		unique_list(RawItems, Items).

	pairwise_dataset_preferences(Dataset, Preferences) :-
		findall(p(Winner, Loser, Weight), Dataset::preference(Winner, Loser, Weight), Preferences).

	pairwise_dataset_connected_components(Dataset, Components) :-
		pairwise_dataset_items(Dataset, Items),
		pairwise_dataset_preferences(Dataset, Preferences),
		connected_components(Items, Preferences, Components).

	pairwise_dataset_summary(Dataset, Summary) :-
		pairwise_dataset_items(Dataset, Items),
		pairwise_dataset_preferences(Dataset, Preferences),
		connected_components(Items, Preferences, Components),
		length(Items, NumberOfItems),
		length(Preferences, NumberOfPreferences),
		length(Components, NumberOfComponents),
		isolated_items(Components, Preferences, IsolatedItems),
		Summary = [
			items(NumberOfItems),
			preferences(NumberOfPreferences),
			connected_components(NumberOfComponents),
			isolated_items(IsolatedItems)
		].

	pairwise_dataset_win_totals(Dataset, Totals) :-
		pairwise_dataset_items(Dataset, Items),
		pairwise_dataset_preferences(Dataset, Preferences),
		findall(Item-Wins, (member(Item, Items), item_wins(Item, Preferences, Wins)), Totals).

	pairwise_dataset_matchups(Dataset, Matchups) :-
		pairwise_dataset_items(Dataset, Items),
		pairwise_dataset_preferences(Dataset, Preferences),
		index_pairs(Items, 1, ItemIndexPairs, IndexItemPairs),
		as_dictionary(ItemIndexPairs, ItemIndices),
		as_dictionary(IndexItemPairs, IndexedItems),
		dictionary_new(Matchups0),
		aggregate_matchups(Preferences, ItemIndices, Matchups0, Matchups1),
		dictionary_as_list(Matchups1, MatchupEntries),
		matchup_entries(MatchupEntries, IndexedItems, Matchups).

	grouped_dataset_groups(Dataset, Groups) :-
		findall(Group, Dataset::group(Group), RawGroups),
		unique_list(RawGroups, Groups).

	grouped_dataset_items(Dataset, Items) :-
		findall(Item, Dataset::item(_Group, Item), RawItems),
		unique_list(RawItems, Items).

	grouped_dataset_items(Dataset, Group, Items) :-
		findall(Item, Dataset::item(Group, Item), RawItems),
		unique_list(RawItems, Items).

	grouped_dataset_summary(Dataset, Summary) :-
		grouped_dataset_groups(Dataset, Groups),
		length(Groups, NumberOfGroups),
		count_grouped_items(Groups, Dataset, 0, NumberOfItems),
		findall(Relevance, Dataset::relevance(_Group, _Item, Relevance), Relevances),
		length(Relevances, NumberOfJudgments),
		Summary = [
			groups(NumberOfGroups),
			items(NumberOfItems),
			relevance_judgments(NumberOfJudgments)
		].

	grouped_dataset_item_relevances(Dataset, Group, MissingRelevance, ItemRelevances) :-
		::grouped_dataset_items(Dataset, Group, Items),
		grouped_dataset_item_relevances(Items, Dataset, Group, MissingRelevance, ItemRelevances).

	grouped_dataset_relevance_frequencies(ItemRelevances, FrequencyDictionary) :-
		dictionary_new(FrequencyDictionary0),
		grouped_dataset_relevance_frequencies(ItemRelevances, FrequencyDictionary0, FrequencyDictionary).

	grouped_dataset_item_relevance_scores([], _PointsDictionary, []).
	grouped_dataset_item_relevance_scores([Item-Relevance| ItemRelevances], PointsDictionary, [Item-Points| ItemScores]) :-
		dictionary_lookup(Relevance, Points, PointsDictionary),
		grouped_dataset_item_relevance_scores(ItemRelevances, PointsDictionary, ItemScores).

	validate_pairwise_dataset(Dataset) :-
		validate_pairwise_dataset(Dataset, _Summary).

	validate_pairwise_dataset(Dataset, Summary) :-
		::pairwise_dataset_declared_items(Dataset, RawItems),
		RawItems \== [],
		check_unique_items(RawItems),
		::pairwise_dataset_items(Dataset, Items),
		::pairwise_dataset_preferences(Dataset, Preferences),
		forall(
			member(p(Winner, Loser, Weight), Preferences),
			validate_preference(Items, Winner, Loser, Weight)
		),
		::pairwise_dataset_connected_components(Dataset, Components),
		(   Components = [_] ->
			true
		;   domain_error(connected_pairwise_dataset, Components)
		),
		::pairwise_dataset_summary(Dataset, Summary).

	validate_grouped_dataset(Dataset) :-
		validate_grouped_dataset(Dataset, _Summary).

	validate_grouped_dataset(Dataset, Summary) :-
		findall(Group, Dataset::group(Group), RawGroups),
		RawGroups \== [],
		check_unique_groups(RawGroups),
		::grouped_dataset_groups(Dataset, Groups),
		forall(
			Dataset::item(Group, _Item),
			memberchk(Group, Groups)
		),
		forall(
			member(Group, Groups),
			validate_group_items(Dataset, Group)
		),
		forall(
			Dataset::relevance(Group, Item, Relevance),
			validate_relevance(Dataset, Groups, Group, Item, Relevance)
		),
		::grouped_dataset_summary(Dataset, Summary).

	connected_components([], _Preferences, []).
	connected_components([Item| Items], Preferences, [Component| Components]) :-
		reachable_items([Item], Preferences, [Item], Reachable),
		reverse(Reachable, Component0),
		unique_list(Component0, Component),
		remove_items(Items, Component, RemainingItems),
		connected_components(RemainingItems, Preferences, Components).

	reachable_items([], _Preferences, Reachable, Reachable).
	reachable_items([Item| Queue], Preferences, Reachable0, Reachable) :-
		neighbors(Preferences, Item, Neighbors),
		new_items(Neighbors, Reachable0, NewItems),
		append(Queue, NewItems, NextQueue),
		append(NewItems, Reachable0, Reachable1),
		reachable_items(NextQueue, Preferences, Reachable1, Reachable).

	neighbors([], _Item, []).
	neighbors([p(Winner, Loser, _)| Preferences], Item, Neighbors) :-
		(   Item == Winner ->
			Neighbors = [Loser| Rest]
		;   Item == Loser ->
			Neighbors = [Winner| Rest]
		;   Neighbors = Rest
		),
		neighbors(Preferences, Item, Rest).

	new_items([], _Visited, []).
	new_items([Item| Items], Visited, NewItems) :-
		(   member(Item, Visited) ->
			new_items(Items, Visited, NewItems)
		;   NewItems = [Item| Rest],
			new_items(Items, [Item| Visited], Rest)
		).

	remove_items([], _ToRemove, []).
	remove_items([Item| Items], ToRemove, RemainingItems) :-
		(   member(Item, ToRemove) ->
			remove_items(Items, ToRemove, RemainingItems)
		;   RemainingItems = [Item| Rest],
			remove_items(Items, ToRemove, Rest)
		).

	isolated_items([], _Preferences, []).
	isolated_items([Component| Components], Preferences, IsolatedItems) :-
		(   Component = [Item],
		    neighbors(Preferences, Item, []) ->
			IsolatedItems = [Item| Rest]
		;   IsolatedItems = Rest
		),
		isolated_items(Components, Preferences, Rest).

	count_grouped_items([], _Dataset, Count, Count).
	count_grouped_items([Group| Groups], Dataset, Count0, Count) :-
		grouped_dataset_items(Dataset, Group, Items),
		length(Items, GroupCount),
		Count1 is Count0 + GroupCount,
		count_grouped_items(Groups, Dataset, Count1, Count).

	grouped_dataset_item_relevances([], _Dataset, _Group, _MissingRelevance, []).
	grouped_dataset_item_relevances([Item| Items], Dataset, Group, MissingRelevance, [Item-Relevance| ItemRelevances]) :-
		grouped_dataset_item_relevance(Dataset, Group, Item, MissingRelevance, Relevance),
		grouped_dataset_item_relevances(Items, Dataset, Group, MissingRelevance, ItemRelevances).

	grouped_dataset_item_relevance(Dataset, Group, Item, MissingRelevance, Relevance) :-
		(   Dataset::relevance(Group, Item, Relevance0) ->
			Relevance = Relevance0
		;   MissingRelevance == zero ->
			Relevance = 0
		;   existence_error(relevance, Group-Item)
		).

	grouped_dataset_relevance_frequencies([], FrequencyDictionary, FrequencyDictionary).
	grouped_dataset_relevance_frequencies([_Item-Relevance| ItemRelevances], FrequencyDictionary0, FrequencyDictionary) :-
		update_frequency_dictionary(FrequencyDictionary0, Relevance, FrequencyDictionary1),
		grouped_dataset_relevance_frequencies(ItemRelevances, FrequencyDictionary1, FrequencyDictionary).

	update_frequency_dictionary(FrequencyDictionary0, Relevance, FrequencyDictionary) :-
		(   dictionary_lookup(Relevance, Count0, FrequencyDictionary0) ->
			Count is Count0 + 1
		;   Count = 1
		),
		dictionary_insert(FrequencyDictionary0, Relevance, Count, FrequencyDictionary).

	item_wins(Item, Preferences, Wins) :-
		findall(Weight, member(p(Item, _, Weight), Preferences), Weights),
		sum(Weights, Wins).

	index_pairs([], _Index, [], []).
	index_pairs([Item| Items], Index, [Item-Index| ItemIndexPairs], [Index-Item| IndexItemPairs]) :-
		NextIndex is Index + 1,
		index_pairs(Items, NextIndex, ItemIndexPairs, IndexItemPairs).

	aggregate_matchups([], _ItemIndices, Matchups, Matchups).
	aggregate_matchups([p(Winner, Loser, Weight)| Preferences], ItemIndices, Matchups0, Matchups) :-
		dictionary_lookup(Winner, WinnerIndex, ItemIndices),
		dictionary_lookup(Loser, LoserIndex, ItemIndices),
		(   WinnerIndex < LoserIndex ->
			LeftIndex = WinnerIndex,
			RightIndex = LoserIndex,
			LeftDelta = Weight,
			RightDelta = 0
		;   LeftIndex = LoserIndex,
			RightIndex = WinnerIndex,
			LeftDelta = 0,
			RightDelta = Weight
		),
		update_matchup_dictionary(Matchups0, LeftIndex, RightIndex, LeftDelta, RightDelta, Matchups1),
		aggregate_matchups(Preferences, ItemIndices, Matchups1, Matchups).

	update_matchup_dictionary(Matchups0, LeftIndex, RightIndex, LeftDelta, RightDelta, Matchups) :-
		Key = pair(LeftIndex, RightIndex),
		(   dictionary_lookup(Key, totals(Left0, Right0), Matchups0) ->
			Left is Left0 + LeftDelta,
			Right is Right0 + RightDelta
		;   Left = LeftDelta,
			Right = RightDelta
		),
		dictionary_insert(Matchups0, Key, totals(Left, Right), Matchups).

	matchup_entries([], _IndexedItems, []).
	matchup_entries([pair(LeftIndex, RightIndex)-totals(LeftWins, RightWins)| MatchupEntries], IndexedItems, [matchup(LeftItem, RightItem, LeftWins, RightWins)| Matchups]) :-
		dictionary_lookup(LeftIndex, LeftItem, IndexedItems),
		dictionary_lookup(RightIndex, RightItem, IndexedItems),
		matchup_entries(MatchupEntries, IndexedItems, Matchups).

	check_unique_items([]).
	check_unique_items([Item| Items]) :-
		(   member(Item, Items) ->
			domain_error(unique_items, [Item| Items])
		;   check_unique_items(Items)
		).

	check_unique_groups([]).
	check_unique_groups([Group| Groups]) :-
		(   member(Group, Groups) ->
			domain_error(unique_groups, [Group| Groups])
		;   check_unique_groups(Groups)
		).

	validate_group_items(Dataset, Group) :-
		findall(Item, Dataset::item(Group, Item), RawItems),
		check_unique_items(RawItems).

	validate_preference(Items, Winner, Loser, Weight) :-
		(   member(Winner, Items) ->
			true
		;   existence_error(item, Winner)
		),
		(   member(Loser, Items) ->
			true
		;   existence_error(item, Loser)
		),
		(   Winner == Loser ->
			domain_error(distinct_items, Winner-Loser)
		;   true
		),
		(   number(Weight), Weight > 0 ->
			true
		;   domain_error(positive_number, Weight)
		).

	validate_relevance(Dataset, Groups, Group, Item, Relevance) :-
		(   member(Group, Groups) ->
			true
		;   existence_error(group, Group)
		),
		(   Dataset::item(Group, Item) ->
			true
		;   existence_error(item, Item)
		),
		(   integer(Relevance), Relevance >= 0 ->
			true
		;   domain_error(non_negative_integer, Relevance)
		).

	unique_list(Items, UniqueItems) :-
		dictionary_new(Seen0),
		unique_list(Items, Seen0, UniqueItems).

	unique_list([], _Seen, []).
	unique_list([Item| Items], Seen0, UniqueItems) :-
		(   dictionary_lookup(Item, _Seen, Seen0) ->
			unique_list(Items, Seen0, UniqueItems)
		;   dictionary_insert(Seen0, Item, true, Seen),
			UniqueItems = [Item| Rest],
			unique_list(Items, Seen, Rest)
		).

:- end_category.
