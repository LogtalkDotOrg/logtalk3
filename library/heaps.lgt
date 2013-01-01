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



%   code adapted from:

%   File   : HEAPS.PL
%   Author : R.A.O'Keefe
%   Updated: 29 November 1983
%   Purpose: Implement heaps in Prolog.

/*  A heap is a labelled binary tree where the key of each node is less
    than or equal to the keys of its sons.  The point of a heap is that
    we can keep on adding new elements to the heap and we can keep on
    taking out the minimum element.  If there are N elements total, the
    total time is O(NlgN).  If you know all the elements in advance, you
    are better off doing a merge-sort, but this file is for when you
    want to do say a best-first search, and have no idea when you start
    how many elements there will be, let alone what they are.

    A heap is represented as a triple t(N, Free, Tree) where N is the
    number of elements in the tree, Free is a list of integers which
    specifies unused positions in the tree, and Tree is a tree made of
	t			terms for empty subtrees and
	t(Key,Value,Left,Right)	terms for the rest
    The nodes of the tree are notionally numbered like this:
				    1
		     2				    3
	     4               6               5               7
	 8      12      10     14       9       13      11     15
      ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..
    The idea is that if the maximum number of elements that have been in
    the heap so far is M, and the tree currently has K elements, the tree
    is some subtreee of the tree of this form having exactly M elements,
    and the Free list is a list of K-M integers saying which of the 
    positions in the M-element tree are currently unoccupied.  This free
    list is needed to ensure that the cost of passing N elements through
    the heap is O(NlgM) instead of O(NlgN).  For M say 100 and N say 10^4
    this means a factor of two.  The cost of the free list is slight.
    The storage cost of a heap in a copying Prolog (which Dec-10 Prolog is
    not) is 2K+3M words.
*/


:- object(heap(_Order),
	implements(heapp),
	extends(compound)).

	:- info([
		version is 1.0,
		author is 'Richard O''Keefe; adapted to Logtalk by Paulo Moura and Victor Lagerkvist.',
		date is 2010/02/19,
		comment is 'Heap implementation, parameterized by the order to be used to compare keys ("<" or ">").',
		parnames is ['Order']]).

	insert(Key, Value, t(M,[],OldTree), t(N,[],NewTree)) :- !,
		N is M + 1,
		insert(N, Key, Value, OldTree, NewTree).
	insert(Key, Value, t(M,[H|T],OldTree), t(N,T,NewTree)) :-
		N is M + 1,
		insert(H, Key, Value, OldTree, NewTree).

	insert(1, Key, Value, _, t(Key,Value,t,t)) :-
		!.
	insert(N, Key, Value, t(K1,V1,L1,R1), t(K2,V2,L2,R2)) :-
		E is N mod 2,
		M is N // 2,
    	%   M > 0,		%  only called from as_heap/4, insert/4
		sort(Key, Value, K1, V1, K2, V2, K3, V3),
		insert(E, M, K3, V3, L1, R1, L2, R2).

	insert(0, N, Key, Value, L1, R, L2, R) :- !,
		insert(N, Key, Value, L1, L2).
	insert(1, N, Key, Value, L, R1, L, R2) :- !,
		insert(N, Key, Value, R1, R2).

	% sort/8 *assumes* the last four arguments are not instantiated;
	% this holds when sort/8 is called from insert/5
	sort(Key1, Value1, Key2, Value2, Key1, Value1, Key2, Value2) :-
		parameter(1, Order),
		compare(Order, Key1, Key2),
		!.
	sort(Key1, Value1, Key2, Value2, Key2, Value2, Key1, Value1).

	insert_all([], Heap, Heap).
	insert_all([Key-Value| Pairs], Heap0, Heap) :-
		insert(Key, Value, Heap0, Heap1),
		insert_all(Pairs, Heap1, Heap).

	delete(t(N,Free,t(Key,Value,Left,Right)), Key, Value, t(M,[Hole|Free],Tree)) :-
		M is N - 1,
		repair(Left, Right, Tree, Hole).

	repair(t(K1,V1,L1,R1), t(K2,V2,L2,R2), t(K2,V2,t(K1,V1,L1,R1),R3), N) :-
		parameter(1, Order),
		compare(Order, K2, K1),
		!,
		repair(L2, R2, R3, M),
		N is 2*M+1.
	repair(t(K1,V1,L1,R1), t(K2,V2,L2,R2), t(K1,V1,L3,t(K2,V2,L2,R2)), N) :- !,
		repair(L1, R1, L3, M),
		N is 2*M.
	repair(t(K1,V1,L1,R1), t, t(K1,V1,L3,t), N) :- !,
		repair(L1, R1, L3, M),
		N is 2*M.
	repair(t, t(K2,V2,L2,R2), t(K2,V2,t,R3), N) :- !,
		repair(L2, R2, R3, M),
		N is 2*M+1.
	repair(t, t, t, 1) :- !.

	empty(Heap) :-
		Heap == t(0,[],t).

	new(t(0,[],t)).

	size(t(Size,_,_), Size).

	as_list(t(_,_,Tree), List) :-
		tree_to_list(Tree, List).

	tree_to_list(t, []) :-
		!.
	tree_to_list(t(Key,Value,Left,Right), [Key-Value| Pairs]) :-
		tree_to_list(Left, LeftPairs),
		tree_to_list(Right, RighPairs),
		merge_pairs(LeftPairs, RighPairs, Pairs).

	merge_pairs([Pair1| Pairs1], [Pair2| Pairs2], [Pair2| Pairs]) :-
		parameter(1, Order),
		compare(Order, Pair2, Pair1),
		!,
		merge_pairs([Pair1| Pairs1], Pairs2, Pairs).
	merge_pairs([Pair1| Pairs1], Pairs2, [Pair1| Pairs]) :-
		!,
		merge_pairs(Pairs1, Pairs2, Pairs).
	merge_pairs([], Pairs, Pairs) :- !.
	merge_pairs(Pairs, [], Pairs).

	as_heap(List, Heap) :-
		as_heap(List, 0, t, Heap).

	as_heap([], N, Tree, t(N,[],Tree)).
	as_heap([Key-Value|Rest], M, OldTree, Heap) :-
		N is M + 1,
		insert(N, Key, Value, OldTree, MidTree),
		as_heap(Rest, N, MidTree, Heap).

	merge(t(M, Holes1, Tree1), t(N, Holes2, Tree2), Heap) :-
		(	M > N ->
			merge_tree(Tree2, t(M, Holes1, Tree1), Heap)
		;	merge_tree(Tree1, t(N, Holes2, Tree2), Heap)
		).

	merge_tree(t, Heap, Heap).
	merge_tree(t(Key,Value,Left,Right), Heap0, Heap) :-
		insert(Heap0, Key, Value, Heap1),
		merge_tree(Left, Heap1, Heap2),
		merge_tree(Right, Heap2, Heap).

	top(t(_,_,t(Key,Value,_,_)), Key, Value).

	top_next(t(_,_,t(Key1,Value1,Left,Right)), Key1, Value1, Key2, Value2) :-
		top_next(Left, Right, Key2, Value2).

	top_next(t(Ka,_,_,_), t(Kb,Vb,_,_), Kb, Vb) :-
		parameter(1, Order),
		compare(Order, Kb, Ka),
		!.
	top_next(t(Ka,Va,_,_), _, Ka, Va).
	top_next(t, t(Kb,Vb,_,_), Kb, Vb).

	valid(Heap) :-
		nonvar(Heap),
		Heap = t(N, _, Tree),
		valid_(Tree, 0, N).

	valid_(t, N, N).
	valid_(t(_, _, Left, Right), N0, N) :-
		N1 is N0 + 1,
		valid_(Left, N1, N2),
		valid_(Right, N2, N).

:- end_object.


:- object(minheap,
	extends(heap(<))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura.',
		date is 2010/02/19,
		comment is 'Min-heap implementation. Uses standard order to compare keys.']).

:- end_object.


:- object(maxheap,
	extends(heap(>))).

	:- info([
		version is 1.0,
		author is 'Paulo Moura.',
		date is 2010/02/19,
		comment is 'Max-heap implementation. Uses standard order to compare keys.']).

:- end_object.
