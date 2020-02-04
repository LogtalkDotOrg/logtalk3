%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests(_HeapObject_),
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2019-05-24,
		comment is 'Unit tests for the "heaps" library.',
		parnames is ['HeapObject']
	]).

	:- uses(_HeapObject_, [
		as_heap/2, as_list/2,
		insert/4, insert_all/3, delete/4, merge/3,
		empty/1, size/2, top/3, top_next/5,
		valid/1, new/1
	]).

	cover(_HeapObject_).

	% as_heap/2 and as_list/2 tests

	test(heap_as_list_2_01) :-
		as_heap([], Heap),
		as_list(Heap, Pairs),
		Pairs == [].

	test(heap_as_list_2_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		as_list(Heap, Pairs),
		(	arg(1, _HeapObject_, (<)) ->
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% insert/4 tests

	test(heap_insert_4_01) :-
		as_heap([], Heap),
		insert(b, 2, Heap, NewHeap),
		as_list(NewHeap, Pairs),
		Pairs == [b-2].

	% insert top pair
	test(heap_insert_4_02) :-
		(	arg(1, _HeapObject_, (<)) ->
			as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,d-4], Heap),
			insert(a, 1, Heap, NewHeap),
			as_list(NewHeap, Pairs),
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	as_heap([b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
			insert(j, 0, Heap, NewHeap),
			as_list(NewHeap, Pairs),
			Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% insert middle pair
	test(heap_insert_4_03) :-
		as_heap([j-0,b-2,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		insert(e, 5, Heap, NewHeap),
		as_list(NewHeap, Pairs),
		(	arg(1, _HeapObject_, (<)) ->
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% insert bottom pair
	test(heap_insert_4_04) :-
		(	arg(1, _HeapObject_, (<)) ->
			as_heap([b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
			insert(j, 0, Heap, NewHeap),
			as_list(NewHeap, Pairs),
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,d-4], Heap),
			insert(a, 1, Heap, NewHeap),
			as_list(NewHeap, Pairs),
			Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% insert_all/3 tests

	% inserting into an empty heap
	test(heap_insert_all_3_01) :-
		as_heap([], Heap),
		insert_all([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap, NewHeap),
		as_list(NewHeap, Pairs),
		(	arg(1, _HeapObject_, (<)) ->
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% inserting into a non-empty heap
	test(heap_insert_all_3_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7], Heap),
		insert_all([i-9,h-8,f-6,a-1,d-4], Heap, NewHeap),
		as_list(NewHeap, Pairs),
		(	arg(1, _HeapObject_, (<)) ->
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% delete/4 tests

	% cannot delete from an empty heap
	test(heap_delete_4_01) :-
		as_heap([], Heap),
		\+ delete(Heap, b, 2, _).

	% delete top pair
	test(heap_delete_4_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		(	arg(1, _HeapObject_, (<)) ->
			delete(Heap, a, 1, NewHeap),
			as_list(NewHeap, Pairs),
			Pairs == [b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	delete(Heap, j, 0, NewHeap),
			as_list(NewHeap, Pairs),
			Pairs == [i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% merge/3 tests

	test(heap_merge_3_01) :-
		as_heap([j-0,b-2,e-5,c-3,g-7], Heap1),
		as_heap([i-9,h-8,f-6,a-1,d-4], Heap2),
		merge(Heap1, Heap2, Heap),
		as_list(Heap, Pairs),
		(	arg(1, _HeapObject_, (<)) ->
			Pairs == [a-1,b-2,c-3,d-4,e-5,f-6,g-7,h-8,i-9,j-0]
		;	Pairs == [j-0,i-9,h-8,g-7,f-6,e-5,d-4,c-3,b-2,a-1]
		).

	% empty/1 tests

	test(heap_empty_1_01) :-
		as_heap([], Heap),
		empty(Heap).

	test(heap_empty_1_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		\+ empty(Heap).

	% size/2 tests

	test(heap_size_2_01) :-
		as_heap([], Heap),
		size(Heap, Size),
		Size == 0.

	test(heap_size_2_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		size(Heap, Size),
		Size == 10.

	% top/3 tests

	test(heap_top_3_01) :-
		as_heap([], Heap),
		\+ top(Heap, _, _).

	test(heap_top_3_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		top(Heap, Key, Value),
		(	arg(1, _HeapObject_, (<)) ->
			Key-Value == a-1
		;	Key-Value == j-0
		).

	% top_next/5 tests

	test(heap_top_next_5_01) :-
		as_heap([], Heap),
		\+ top_next(Heap, _, _, _, _).

	test(heap_top_next_5_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		top_next(Heap, Key1, Value1, Key2, Value2),
		(	arg(1, _HeapObject_, (<)) ->
			Key1-Value1 == a-1, Key2-Value2 == b-2
		;	Key1-Value1 == j-0, Key2-Value2 == i-9
		).

	% valid/1 tests

	test(heap_valid_1_01) :-
		as_heap([], Heap),
		valid(Heap).

	test(heap_valid_1_02) :-
		as_heap([j-0,b-2,e-5,c-3,g-7,i-9,h-8,f-6,a-1,d-4], Heap),
		valid(Heap).

	% new/1 tests

	test(heap_new_1_01) :-
		new(Heap),
		valid(Heap).

:- end_object.
