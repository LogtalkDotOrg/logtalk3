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


:- category(ranking_dataset_validation).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-17,
		comment is 'Shared predicates for validating ranking datasets.'
	]).

	:- uses(list, [
		member/2, memberchk/2
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

:- end_category.
