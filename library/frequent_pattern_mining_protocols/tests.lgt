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
		date is 2026-04-29,
		comment is 'Smoke tests for the "frequent_pattern_mining_protocols" library.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	test(market_basket_basics_items, deterministic(Items == [bread, butter, cereal, eggs, milk])) :-
		market_basket_basics::items(Items).

	test(market_basket_basics_transactions_count, deterministic(Count == 6)) :-
		findall(Id, market_basket_basics::transaction(Id, _Transaction), Ids),
		length(Ids, Count).

	test(deep_intersection_baskets_transactions_count, deterministic(Count == 8)) :-
		findall(Id, deep_intersection_baskets::transaction(Id, _Transaction), Ids),
		length(Ids, Count).

	test(deep_intersection_baskets_deep_shape, deterministic(transaction_is_declared(deep_intersection_baskets, Transaction))) :-
		deep_intersection_baskets::transaction(1, Transaction).

	test(market_basket_basics_transaction_shape, deterministic((Transaction == [bread, butter, milk], transaction_is_declared(market_basket_basics, Transaction)))) :-
		market_basket_basics::transaction(2, Transaction).

	test(layered_baskets_declared_transactions, deterministic) :-
		forall(layered_baskets::transaction(_Id, Transaction), transaction_is_declared(layered_baskets, Transaction)).

	test(invalid_undeclared_item_baskets_contains_undeclared_item, deterministic(\+ transaction_is_declared(invalid_undeclared_item_baskets, Transaction))) :-
		invalid_undeclared_item_baskets::transaction(1, Transaction).

	test(invalid_unsorted_transaction_baskets_not_canonical, deterministic(\+ canonical_transaction(Transaction))) :-
		invalid_unsorted_transaction_baskets::transaction(1, Transaction).

	test(invalid_duplicate_item_baskets_not_canonical, deterministic(\+ canonical_transaction(Transaction))) :-
		invalid_duplicate_item_baskets::transaction(1, Transaction).

	test(invalid_duplicate_id_baskets_has_non_unique_ids, deterministic(\+ unique_transaction_ids(Ids))) :-
		findall(Id, invalid_duplicate_id_baskets::transaction(Id, _Transaction), Ids).

	test(invalid_empty_baskets_has_no_transactions, deterministic(Count == 0)) :-
		findall(Id, invalid_empty_baskets::transaction(Id, _Transaction), Ids),
		length(Ids, Count).

	test(invalid_item_domain_baskets_not_canonical, deterministic(\+ canonical_item_domain(Items))) :-
		invalid_item_domain_baskets::items(Items).

	transaction_is_declared(Dataset, Transaction) :-
		Dataset::items(Items),
		sort(Transaction, Sorted),
		Transaction == Sorted,
		forall(member(Item, Transaction), memberchk(Item, Items)).

	canonical_transaction(Transaction) :-
		sort(Transaction, SortedTransaction),
		Transaction == SortedTransaction.

	canonical_item_domain(ItemDomain) :-
		sort(ItemDomain, SortedItemDomain),
		ItemDomain == SortedItemDomain.

	unique_transaction_ids(Ids) :-
		sort(Ids, UniqueIds),
		length(Ids, IdsCount),
		length(UniqueIds, UniqueIdsCount),
		IdsCount =:= UniqueIdsCount.

:- end_object.
