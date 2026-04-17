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


:- category(ranking_dataset_analysis).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-17,
		comment is 'Shared predicates for collecting and analyzing ranking datasets.'
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2
	]).

	:- uses(numberlist, [
		sum/2
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

	:- public(grouped_dataset_groups/2).
	:- mode(grouped_dataset_groups(+object_identifier, -list), one).
	:- info(grouped_dataset_groups/2, [
		comment is 'Collects the unique grouped-dataset groups preserving their first declaration order.',
		argnames is ['Dataset', 'Groups']
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

	grouped_dataset_groups(Dataset, Groups) :-
		findall(Group, Dataset::group(Group), RawGroups),
		unique_list(RawGroups, Groups).

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

	item_wins(Item, Preferences, Wins) :-
		findall(Weight, member(p(Item, _, Weight), Preferences), Weights),
		sum(Weights, Wins).

	unique_list([], []).
	unique_list([Item| Items], UniqueItems) :-
		(   member(Item, Items) ->
			unique_list(Items, UniqueItems)
		;   UniqueItems = [Item| Rest],
			unique_list(Items, Rest)
		).

:- end_category.
