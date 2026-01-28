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


/*  Pairing heaps implementation.

    A pairing heap is a type of heap data structure with relatively simple
    implementation and excellent amortized performance. In a pairing heap,
    the heap is represented as a tree where each node has a value and a
    list of child heaps.

    Representation: empty or p(Key, Value, Children) where Children is a list
    of child pairing heaps.
*/


:- object(pairing_heap(_Order_),
	implements(heap_protocol),
	extends(compound)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-28,
		comment is 'Pairing heap implementation, parameterized by the order to be used to compare keys (``<`` or ``>``).',
		parameters is [
			'Order' - 'Either ``<`` for a min heap or ``>`` for a max heap.'
		],
		see_also is [pairing_heap_min, pairing_heap_max]
	]).

	% Empty heap representation
	empty(Heap) :-
		Heap == empty.

	% Create a new empty heap
	new(empty).

	% Insert a key-value pair into the heap
	insert(Key, Value, Heap0, Heap) :-
		merge(p(Key, Value, []), Heap0, Heap).

	% Insert all pairs from a list
	insert_all([], Heap, Heap).
	insert_all([Key-Value| Pairs], Heap0, Heap) :-
		insert(Key, Value, Heap0, Heap1),
		insert_all(Pairs, Heap1, Heap).

	% Delete the top element from the heap
	delete(p(Key, Value, Children), Key, Value, Heap) :-
		merge_children(Children, Heap).

	% Merge two heaps
	merge(empty, Heap, Heap) :- !.
	merge(Heap, empty, Heap) :- !.
	merge(p(Key1, Value1, Children1), p(Key2, Value2, Children2), Heap) :-
		compare(_Order_, Key1, Key2),
		!,
		Heap = p(Key1, Value1, [p(Key2, Value2, Children2)| Children1]).
	merge(p(Key1, Value1, Children1), p(Key2, Value2, Children2), Heap) :-
		Heap = p(Key2, Value2, [p(Key1, Value1, Children1)| Children2]).

	% Merge a list of child heaps using the two-pass algorithm
	merge_children([], empty).
	merge_children([Child], Child) :- !.
	merge_children(Children0, Children) :-
		first_pass(Children0, Children1),
		second_pass(Children1, Children).

	% First pass: merge adjacent pairs
	first_pass([], []).
	first_pass([Child], [Child]) :- !.
	first_pass([Child1, Child2| Children0], [Child| Children]) :- !,
		merge(Child1, Child2, Child),
		first_pass(Children0, Children).

	% Second pass: merge all pairs from left to right
	second_pass([], empty).
	second_pass([Child], Child) :- !.
	second_pass([Child1, Child2| Children0], Children) :- !,
		merge(Child1, Child2, Child),
		second_pass([Child| Children0], Children).

	% Get the size of the heap
	size(Heap, Size) :-
		size_helper(Heap, Size).

	size_helper(empty, 0).
	size_helper(p(_, _, Children), Size) :-
		size_list(Children, 1, Size).

	size_list([], Size, Size).
	size_list([Child| Children], Size0, Size) :-
		size_helper(Child, HeadSize),
		Size1 is Size0 + HeadSize,
		size_list(Children, Size1, Size).

	% Convert heap to sorted list
	as_list(Heap, List) :-
		as_list_helper(Heap, List).

	as_list_helper(empty, []).
	as_list_helper(p(Key, Value, Children), [Key-Value| Pairs]) :-
		delete(p(Key, Value, Children), Key, Value, NewHeap),
		as_list_helper(NewHeap, Pairs).

	% Create a heap from a list of pairs
	as_heap(List, Heap) :-
		as_heap_helper(List, empty, Heap).

	as_heap_helper([], Heap, Heap).
	as_heap_helper([Key-Value| Rest], Heap0, Heap) :-
		insert(Key, Value, Heap0, Heap1),
		as_heap_helper(Rest, Heap1, Heap).

	% Get the top element
	top(p(Key, Value, _), Key, Value).

	% Get the top and second element
	top_next(p(Key1, Value1, [Child| Children]), Key1, Value1, Key2, Value2) :-
		find_minimum_child(Children, Child, Key2, Value2).

	% Find the child with the minimum (or maximum) key
	find_minimum_child([], p(Key, Value, _), Key, Value).
	find_minimum_child([Child| Children], p(Key1, Value1, _), Key, Value) :-
		find_minimum_child(Children, Child, Key2, Value2),
		(	compare(_Order_, Key1, Key2) ->
			Key = Key1,
			Value = Value1
		;	Key = Key2,
			Value = Value2
		).

	% Validate heap structure and ordering
	valid(Heap) :-
		nonvar(Heap),
		valid_helper(Heap).

	valid_helper(empty).
	valid_helper(p(ParentKey, _, Children)) :-
		valid_children(Children, ParentKey).

	valid_children([], _).
	valid_children([p(ChildKey, _, _)| Children], ParentKey) :-
		compare(_Order_, ParentKey, ChildKey),
		valid_children(Children, ParentKey).

:- end_object.


:- object(pairing_heap_min,
	extends(pairing_heap(<))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-28,
		comment is 'Min-pairing heap implementation. Uses standard order to compare keys.',
		see_also is [pairing_heap_max, pairing_heap(_)]
	]).

:- end_object.


:- object(pairing_heap_max,
	extends(pairing_heap(>))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-28,
		comment is 'Max-pairing heap implementation. Uses standard order to compare keys.',
		see_also is [pairing_heap_min, pairing_heap(_)]
	]).

:- end_object.
