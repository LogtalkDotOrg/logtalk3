%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
