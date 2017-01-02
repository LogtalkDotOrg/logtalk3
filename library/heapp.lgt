%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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



:- protocol(heapp).

	:- info([
		version is 1.01,
		author is 'Richard O''Keefe; adapted to Logtalk by Paulo Moura and Victor Lagerkvist.',
		date is 2010/11/13,
		comment is 'Heap protocol.'
	]).

	:- public(insert/4).
	:- mode(insert(+key, +value, +heap, -heap), one).
	:- info(insert/4, [
		comment is 'Inserts the new Key-Value pair into a heap, returning the updated heap.',
		argnames is ['Key', 'Value', 'Heap', 'NewHeap']
	]).

	:- public(insert_all/3).
	:- mode(insert_all(@list(pairs), +heap, -heap), one).
	:- info(insert_all/3, [
		comment is 'Inserts a list of Key-Value pairs into a heap, returning the updated heap.',
		argnames is ['List', 'Heap', 'NewHeap']
	]).

	:- public(delete/4).
	:- mode(delete(+heap, ?key, ?value, -heap), zero_or_one).
	:- info(delete/4, [
		comment is 'Deletes and returns the top Key-Value pair in OldHeap and the resulting NewHeap.',
		argnames is ['Heap', 'Key', 'Value', 'NewHeap']
	]).

	:- public(merge/3).
	:- mode(merge(+heap, +heap, -heap), one).
	:- info(merge/3, [
		comment is 'Merges two heaps.',
		argnames is ['Heap1', 'Heap2', 'NewHeap']
	]).

	:- public(empty/1).
	:- mode(empty(@heap), zero_or_one).
	:- info(empty/1, [
		comment is 'True if the heap is empty.',
		argnames is ['Heap']
	]).

	:- public(size/2).
	:- mode(size(+heap, ?integer), zero_or_one).
	:- info(size/2, [
		comment is 'Returns the number of heap elements.',
		argnames is ['Heap', 'Size']
	]).

	:- public(as_list/2).
	:- mode(as_list(+heap, -list), one).
	:- info(as_list/2, [
		comment is 'Returns the current set of Key-Value pairs in the Heap as a List, sorted into ascending order of Keys.',
		argnames is ['Heap', 'List']
	]).

	:- public(as_heap/2).
	:- mode(as_heap(+list, -heap), one).
	:- info(as_heap/2, [
		comment is 'Constructs a Heap from a list of Key-Value pairs.',
		argnames is ['List', 'Heap']
	]).

	:- public(top/3).
	:- mode(top(+heap, ?key, ?value), zero_or_one).
	:- info(top/3, [
		comment is 'Returns the top Key-Value pair in Heap. Fails if the heap is empty.',
		argnames is ['Heap', 'Key', 'Value']
	]).

	:- public(top_next/5).
	:- mode(top_next(+heap, ?key, ?value, ?key, ?value), zero_or_one).
	:- info(top_next/5, [
		comment is 'Returns the top pair, Key1-Value1, and the next pair, Key2-Value2, in Heap. Fails if the heap does not have at least two elements.',
		argnames is ['Heap', 'Key1', 'Value1', 'Key2', 'Value2']
	]).

:- end_protocol.
