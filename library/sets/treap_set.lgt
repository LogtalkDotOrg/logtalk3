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


:- object(treap_set,
	implements(setp),
	extends(compound)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-28,
		comment is 'Set predicates implemented using treaps (tree heaps). A treap is a binary search tree with randomly assigned priorities. Uses ``==/2`` for element comparison and standard term ordering.',
		see_also is [set, set(_)]
	]).

	:- uses(list, [
		msort/2, length/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		between/3
	]).

	% Treap node structure: node(Key, Priority, Left, Right)
	% Empty treap is represented as empty

	as_set(List, Treap) :-
		msort(List, Sorted),
		list_to_treap(Sorted, Treap).

	list_to_treap([], empty) :-
		!.
	list_to_treap([Element], node(Element, Priority, empty, empty)) :-
		!,
		generate_random_priority(Priority).
	list_to_treap(List, Treap) :-
		insert_list(List, empty, Treap).

	insert_list([], Treap, Treap).
	insert_list([Head| Tail], Treap0, Treap) :-
		treap_insert(Treap0, Head, Treap1),
		insert_list(Tail, Treap1, Treap).

	treap_insert(empty, Key, node(Key, Priority, empty, empty)) :-
		generate_random_priority(Priority).
	treap_insert(node(NodeKey, NodePriority, Left, Right), Key, Treap) :-
		compare(Order, Key, NodeKey),
		(	Order == (=) ->
			 % duplicate
			Treap = node(NodeKey, NodePriority, Left, Right)
		;	Order == (<) ->
			treap_insert(Left, Key, NewLeft),
			Treap0 = node(NodeKey, NodePriority, NewLeft, Right),
			bubble_up(Treap0, Treap)
		;	treap_insert(Right, Key, NewRight),
			Treap0 = node(NodeKey, NodePriority, Left, NewRight),
			bubble_up(Treap0, Treap)
		).

	% Bubble up a node that may violate the heap property
	% Check if left or right child has higher priority and rotate if needed
	bubble_up(Treap, Result) :-
		Treap = node(_, NodePriority, Left, Right),
		(	Left = node(_, LeftPriority, _, _),
			LeftPriority > NodePriority ->
			rotate_right(Treap, Treap1),
			bubble_up(Treap1, Result)
		;	Right = node(_, RightPriority, _, _),
			RightPriority > NodePriority ->
			rotate_left(Treap, Treap1),
			bubble_up(Treap1, Result)
		;	Result = Treap
		).

	rotate_right(
		node(NodeKey, NodePriority, node(LeftKey, LeftPriority, LeftLeft, LeftRight), Right),
		node(LeftKey, LeftPriority, LeftLeft, node(NodeKey, NodePriority, LeftRight, Right))
	).

	rotate_left(
		node(NodeKey, NodePriority, Left, node(RightKey, RightPriority, RightLeft, RightRight)),
		node(RightKey, RightPriority, node(NodeKey, NodePriority, Left, RightLeft), RightRight)
	).

	as_list(Treap, List) :-
		as_list(Treap, [], List).

	as_list(empty, List, List).
	as_list(node(Key, _, Left, Right), List0, List) :-
		as_list(Left, [Key| List1], List),
		as_list(Right, List0, List1).

	delete(empty, _, empty).
	delete(node(Key, Priority, Left, Right), Element, Treap) :-
		compare(Order, Key, Element),
		(	Order == (=) ->
			merge_treaps(Left, Right, Treap)
		;	Order == (>) ->
			delete(Left, Element, LeftRes),
			Treap = node(Key, Priority, LeftRes, Right)
		;	delete(Right, Element, RightRes),
			Treap = node(Key, Priority, Left, RightRes)
		).

	merge_treaps(empty, Right, Right) :- !.
	merge_treaps(Left, empty, Left) :- !.
	merge_treaps(node(K1, P1, L1, R1), node(K2, P2, L2, R2), Treap) :-
		(	P1 >= P2 ->
			merge_treaps(R1, node(K2, P2, L2, R2), RightMerged),
			Treap = node(K1, P1, L1, RightMerged)
		;	merge_treaps(node(K1, P1, L1, R1), L2, LeftMerged),
			Treap = node(K2, P2, LeftMerged, R2)
		).

	disjoint(Treap1, Treap2) :-
		\+ intersect(Treap1, Treap2).

	intersect(empty, _) :- !, fail.
	intersect(_, empty) :- !, fail.
	intersect(node(Key1, _, Left1, Right1), Treap2) :-
		(	memberchk(Key1, Treap2) -> true
		;	intersect(Left1, Treap2) -> true
		;	intersect(Right1, Treap2)
		).

	equal(Treap1, Treap2) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		msort(List1, Sorted1),
		msort(List2, Sorted2),
		Sorted1 == Sorted2.

	empty(Treap) :-
		Treap == empty.

	insert(Treap, Element, Result) :-
		treap_insert(Treap, Element, Result).

	insert_all([], Treap, Treap).
	insert_all([Head| Tail], TreapIn, TreapOut) :-
		insert(TreapIn, Head, TreapMid),
		insert_all(Tail, TreapMid, TreapOut).

	intersection(Treap1, Treap2, Result) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::intersection(List1, List2, ResultList),
		as_set(ResultList, Result).

	intersection(Treap1, Treap2, Result, Difference) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::intersection(List1, List2, ResultList, DifferenceList),
		as_set(ResultList, Result),
		as_set(DifferenceList, Difference).

	size(Treap, Size) :-
		as_list(Treap, List),
		length(List, Size).

	member(Element, node(Element, _, _, _)).
	member(Element, node(_, _, Left, _)) :-
		member(Element, Left).
	member(Element, node(_, _, _, Right)) :-
		member(Element, Right).

	memberchk(Element, node(Key, _, Left, Right)) :-
		(	Element = Key ->
			true
		;	compare(Order, Element, Key),
			(	Order == (<) -> memberchk(Element, Left)
			;	Order == (>) -> memberchk(Element, Right)
			;	fail
			)
		).

	powerset(Treap, PowerSet) :-
		as_list(Treap, List),
		set::powerset(List, PSets),
		powerset_convert_to_lists(PSets, PowerSet).

	powerset_convert_to_lists([], []).
	powerset_convert_to_lists([Set| Sets], [Set| ConvertedSets]) :-
		powerset_convert_to_lists(Sets, ConvertedSets).

	product(Treap1, Treap2, Result) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::product(List1, List2, ProductList),
		as_set(ProductList, Result).

	select(Element, node(Element, _, Left, Right), Remaining) :-
		merge_treaps(Left, Right, Remaining).
	select(Element, node(Key, Priority, Left, Right), node(Key, Priority, LeftRes, Right)) :-
		select(Element, Left, LeftRes).
	select(Element, node(Key, Priority, Left, Right), node(Key, Priority, Left, RightRes)) :-
		select(Element, Right, RightRes).

	selectchk(Element, node(Element, _, Left, Right), Remaining) :- !,
		merge_treaps(Left, Right, Remaining).
	selectchk(Element, node(Key, Priority, Left, Right), node(Key, Priority, LeftRes, Right)) :-
		compare(<, Element, Key),
		!,
		selectchk(Element, Left, LeftRes).
	selectchk(Element, node(Key, Priority, Left, Right), node(Key, Priority, Left, RightRes)) :-
		selectchk(Element, Right, RightRes).

	subset(Treap1, Treap2) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::subset(List1, List2).

	subtract(Treap1, Treap2, Result) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::subtract(List1, List2, ResultList),
		as_set(ResultList, Result).

	symdiff(Treap1, Treap2, Result) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::symdiff(List1, List2, ResultList),
		as_set(ResultList, Result).

	union(Treap1, Treap2, Result) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::union(List1, List2, ResultList),
		as_set(ResultList, Result).

	union(Treap1, Treap2, Result, Difference) :-
		as_list(Treap1, List1),
		as_list(Treap2, List2),
		set::union(List1, List2, ResultList, DifferenceList),
		as_set(ResultList, Result),
		as_set(DifferenceList, Difference).

	valid(Treap) :-
		nonvar(Treap),
		valid_treap(Treap).

	valid_treap(empty).
	valid_treap(node(Key, Priority, Left, Right)) :-
		valid_treap(Key, Priority, Left, Right).

	valid_treap(Key, Priority, Left, Right) :-
		valid_treap(Left),
		valid_treap(Right),
		check_all_left_less(Left, Key),
		check_all_right_greater(Right, Key),
		check_all_left_priorities(Left, Priority),
		check_all_right_priorities(Right, Priority).

	check_all_left_less(empty, _).
	check_all_left_less(node(LeftKey, _, LeftLeft, LeftRight), Key) :-
		LeftKey @< Key,
		check_all_left_less(LeftLeft, Key),
		check_all_left_less(LeftRight, Key).

	check_all_right_greater(empty, _).
	check_all_right_greater(node(RightKey, _, RightLeft, RightRight), Key) :-
		RightKey @> Key,
		check_all_right_greater(RightLeft, Key),
		check_all_right_greater(RightRight, Key).

	check_all_left_priorities(empty, _).
	check_all_left_priorities(node(_, Priority, LeftLeft, LeftRight), NodePriority) :-
		Priority =< NodePriority,
		check_all_left_priorities(LeftLeft, NodePriority),
		check_all_left_priorities(LeftRight, NodePriority).

	check_all_right_priorities(empty, _).
	check_all_right_priorities(node(_, Priority, RightLeft, RightRight), NodePriority) :-
		Priority =< NodePriority,
		check_all_right_priorities(RightLeft, NodePriority),
		check_all_right_priorities(RightRight, NodePriority).

	new(empty).

	generate_random_priority(Priority) :-
		between(0, 1000000000, Priority).

:- end_object.
