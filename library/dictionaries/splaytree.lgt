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


:- object(splaytree,
	implements(dictionaryp),
	extends(term)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-10,
		comment is 'Splay tree implementation of the dictionary protocol. A splay tree is a self-adjusting binary search tree with the property that recently accessed elements are quick to access again. Uses standard order to compare keys.',
		see_also is [avltree, bintree, rbtree]
	]).

	% Tree representation:
	% - Empty tree: t
	% - Node: t(Key, Value, Left, Right)

	new(t).

	empty(Tree) :-
		Tree == t.

	as_dictionary(Pairs, Tree) :-
		keysort(Pairs, SortedPairs),
		as_dictionary_(SortedPairs, Tree).

	as_dictionary_([], t) :-
		!.
	as_dictionary_([Key-Value], t(Key, Value, t, t)) :-
		!.
	as_dictionary_([Key1-Value1, Key2-Value2], t(Key1, Value1, t, t(Key2, Value2, t, t))) :-
		!.
	as_dictionary_(Pairs, t(Key, Value, Left, Right)) :-
		list::length(Pairs, N),
		N1 is floor(N/2),
		split(Pairs, N1, Pairs1, [Key-Value| Pairs2]),
		as_dictionary_(Pairs1, Left),
		as_dictionary_(Pairs2, Right).

	split(Pairs, 0, [], Pairs) :-
		!.
	split([Pair| Pairs], N, [Pair| Pairs1], Pairs2) :-
		N1 is N - 1,
		split(Pairs, N1, Pairs1, Pairs2).

	as_list(Tree, Pairs) :-
		as_list_(Tree, [], Pairs).

	as_list_(t, Pairs, Pairs).
	as_list_(t(Key, Value, Left, Right), Pairs0, Pairs) :-
		as_list_(Right, Pairs0, Pairs1),
		as_list_(Left, [Key-Value| Pairs1], Pairs).

	as_curly_bracketed(Tree, Curly) :-
		as_list_(Tree, [], Pairs),
		pairs_to_curly(Pairs, Curly).

	pairs_to_curly([], {}).
	pairs_to_curly([Pair| Pairs], {Term}) :-
		pairs_to_curly(Pairs, Pair, Term).

	pairs_to_curly([], Pair, Pair).
	pairs_to_curly([NextPair| Pairs], Pair, (Pair, RestPairs)) :-
		pairs_to_curly(Pairs, NextPair, RestPairs).

	clone(Tree, Clone, Pairs) :-
		clone_3(Tree, Clone, [], Pairs).

	clone_3(t, t, Pairs, Pairs).
	clone_3(t(Key, _, Left, Right), t(Key, NewValue, CloneLeft, CloneRight), Pairs0, Pairs) :-
		clone_3(Left, CloneLeft, [Key-NewValue| Pairs1], Pairs),
		clone_3(Right, CloneRight, Pairs0, Pairs1).

	clone(Tree, Pairs, Clone, ClonePairs) :-
		clone_4(Tree, [], Pairs, Clone, [], ClonePairs).

	clone_4(t, Pairs, Pairs, t, ClonePairs, ClonePairs).
	clone_4(t(Key, Value, Left, Right), Pairs0, Pairs, t(Key, NewValue, CloneLeft, CloneRight), ClonePairs0, ClonePairs) :-
		clone_4(Left, [Key-Value| Pairs1], Pairs, CloneLeft, [Key-NewValue| ClonePairs1], ClonePairs),
		clone_4(Right, Pairs0, Pairs1, CloneRight, ClonePairs0, ClonePairs1).

	% Splay operation: brings the node with key Key to the root
	% splay(+Tree, +Key, -SplayedTree)
	% If Key is not in Tree, splays to the closest key

	splay(t, _, t).
	splay(t(Key, Value, Left, Right), SearchKey, SplayedTree) :-
		compare(Order, SearchKey, Key),
		splay_(Order, Key, Value, Left, Right, SearchKey, SplayedTree).

	% Key found at root
	splay_(=, Key, Value, Left, Right, _, t(Key, Value, Left, Right)).

	% Zig: Key is in left subtree
	splay_(<, Key, Value, Left, Right, SearchKey, SplayedTree) :-
		(	Left == t ->
			% Key not found, tree unchanged
			SplayedTree = t(Key, Value, Left, Right)
		;	Left = t(LeftKey, LeftValue, LeftLeft, LeftRight),
			compare(LeftOrder, SearchKey, LeftKey),
			splay_left(LeftOrder, Key, Value, LeftKey, LeftValue, LeftLeft, LeftRight, Right, SearchKey, SplayedTree)
		).

	% Zig: Key is in right subtree
	splay_(>, Key, Value, Left, Right, SearchKey, SplayedTree) :-
		(	Right == t ->
			% Key not found, tree unchanged
			SplayedTree = t(Key, Value, Left, Right)
		;	Right = t(RightKey, RightValue, RightLeft, RightRight),
			compare(RightOrder, SearchKey, RightKey),
			splay_right(RightOrder, Key, Value, Left, RightKey, RightValue, RightLeft, RightRight, SearchKey, SplayedTree)
		).

	% Zig step: node is left child of root
	splay_left(=, Key, Value, LeftKey, LeftValue, LeftLeft, LeftRight, Right, _,
			   t(LeftKey, LeftValue, LeftLeft, t(Key, Value, LeftRight, Right))).

	% Zig-zig step: node is in left-left subtree
	splay_left(<, Key, Value, LeftKey, LeftValue, LeftLeft, LeftRight, Right, SearchKey, SplayedTree) :-
		(	LeftLeft == t ->
			% Key not found, do single rotation
			SplayedTree = t(LeftKey, LeftValue, LeftLeft, t(Key, Value, LeftRight, Right))
		;	splay(LeftLeft, SearchKey, t(NewKey, NewValue, NewLeft, NewRight)),
			SplayedTree = t(NewKey, NewValue, NewLeft, t(LeftKey, LeftValue, NewRight, t(Key, Value, LeftRight, Right)))
		).

	% Zig-zag step: node is in left-right subtree
	splay_left(>, Key, Value, LeftKey, LeftValue, LeftLeft, LeftRight, Right, SearchKey, SplayedTree) :-
		(	LeftRight == t ->
			% Key not found, do single rotation
			SplayedTree = t(LeftKey, LeftValue, LeftLeft, t(Key, Value, LeftRight, Right))
		;	splay(LeftRight, SearchKey, t(NewKey, NewValue, NewLeft, NewRight)),
			SplayedTree = t(NewKey, NewValue, t(LeftKey, LeftValue, LeftLeft, NewLeft), t(Key, Value, NewRight, Right))
		).

	% Zig step: node is right child of root
	splay_right(=, Key, Value, Left, RightKey, RightValue, RightLeft, RightRight, _,
				t(RightKey, RightValue, t(Key, Value, Left, RightLeft), RightRight)).

	% Zig-zig step: node is in right-right subtree
	splay_right(>, Key, Value, Left, RightKey, RightValue, RightLeft, RightRight, SearchKey, SplayedTree) :-
		(	RightRight == t ->
			% Key not found, do single rotation
			SplayedTree = t(RightKey, RightValue, t(Key, Value, Left, RightLeft), RightRight)
		;	splay(RightRight, SearchKey, t(NewKey, NewValue, NewLeft, NewRight)),
			SplayedTree = t(NewKey, NewValue, t(RightKey, RightValue, t(Key, Value, Left, RightLeft), NewLeft), NewRight)
		).

	% Zig-zag step: node is in right-left subtree
	splay_right(<, Key, Value, Left, RightKey, RightValue, RightLeft, RightRight, SearchKey, SplayedTree) :-
		(	RightLeft == t ->
			% Key not found, do single rotation
			SplayedTree = t(RightKey, RightValue, t(Key, Value, Left, RightLeft), RightRight)
		;	splay(RightLeft, SearchKey, t(NewKey, NewValue, NewLeft, NewRight)),
			SplayedTree = t(NewKey, NewValue, t(Key, Value, Left, NewLeft), t(RightKey, RightValue, NewRight, RightRight))
		).

	% Insert: insert key-value pair, then splay to root
	% For splay trees, we can insert without splaying for simplicity,
	% since the protocol doesn't return the splayed tree for lookups
	insert(t, Key, Value, t(Key, Value, t, t)) :-
		nonvar(Key).
	insert(t(Key1, Value1, Left1, Right1), Key, Value, NewTree) :-
		compare(Order, Key, Key1),
		insert_(Order, Key1, Value1, Left1, Right1, Key, Value, NewTree).

	insert_(=, _, _, Left, Right, Key, Value, t(Key, Value, Left, Right)).
	insert_(<, Key1, Value1, Left1, Right1, Key, Value, t(Key1, Value1, Left2, Right1)) :-
		insert(Left1, Key, Value, Left2).
	insert_(>, Key1, Value1, Left1, Right1, Key, Value, t(Key1, Value1, Left1, Right2)) :-
		insert(Right1, Key, Value, Right2).

	lookup(Key, Value, Tree) :-
		(	var(Key) ->
			lookup_var(Key, Value, Tree)
		;	lookup_nonvar(Key, Value, Tree)
		).

	lookup_nonvar(Key, Value, t(Key1, Value1, Left1, Right1)) :-
		compare(Order, Key, Key1),
		lookup_nonvar_(Order, Key, Value, Value1, Left1, Right1).

	lookup_nonvar_(=, _, Value, Value, _, _).
	lookup_nonvar_(<, Key, Value, _, Left, _) :-
		lookup_nonvar(Key, Value, Left).
	lookup_nonvar_(>, Key, Value, _, _, Right) :-
		lookup_nonvar(Key, Value, Right).

	lookup_var(Key, Value, t(_, _, Left, _)) :-
		lookup_var(Key, Value, Left).
	lookup_var(Key, Value, t(Key, Value, _, _)).
	lookup_var(Key, Value, t(_, _, _, Right)) :-
		lookup_var(Key, Value, Right).

	lookup([], _).
	lookup([Key-Value| Pairs], Tree) :-
		lookup(Key, Value, Tree),
		lookup(Pairs, Tree).

	lookup(Key, Value, Tree, SplayedTree) :-
		splay(Tree, Key, SplayedTree),
		SplayedTree = t(Key, Value, _, _).

	% Previous key in standard order
	previous(Tree, Key, Previous, Value) :-
		previous_(Tree, Key, Previous, Value, []).

	previous_(t(Key0, Value0, Left, Right), Key, Previous, Value, Candidate) :-
		compare(Order, Key0, Key),
		previous__(Order, Key0, Value0, Left, Right, Key, Previous, Value, Candidate).

	previous__(>, _, _, Left, _, Key, Previous, Value, Candidate) :-
		previous_(Left, Key, Previous, Value, Candidate).
	previous__(<, Key0, Value0, _, Right, Key, Previous, Value, _) :-
		previous_(Right, Key, Previous, Value, Key0-Value0).
	previous__(=, _, _, Left, _, _, Previous, Value, Candidate) :-
		(	max(Left, Previous, Value) ->
			true
		;	Candidate = (Previous-Value)
		).

	% Next key in standard order
	next(Tree, Key, Next, Value) :-
		next_(Tree, Key, Next, Value, []).

	next_(t(Key0, Value0, Left, Right), Key, Next, Value, Candidate) :-
		compare(Order, Key0, Key),
		next__(Order, Key0, Value0, Left, Right, Key, Next, Value, Candidate).

	next__(>, Key0, Value0, Left, _, Key, Next, Value, _) :-
		next_(Left, Key, Next, Value, Key0-Value0).
	next__(<, _, _, _, Right, Key, Next, Value, Candidate) :-
		next_(Right, Key, Next, Value, Candidate).
	next__(=, _, _, _, Right, _, Next, Value, Candidate) :-
		(	min(Right, Next, Value) ->
			true
		;	Candidate = (Next-Value)
		).

	% Minimum key
	min(t(Key, Value, t, _), MinKey, MinValue) :-
		!,
		MinKey = Key,
		MinValue = Value.
	min(t(_, _, Left, _), Key, Value) :-
		min(Left, Key, Value).

	% Maximum key
	max(t(Key, Value, _, t), MaxKey, MaxValue) :-
		!,
		MaxKey = Key,
		MaxValue = Value.
	max(t(_, _, _, Right), Key, Value) :-
		max(Right, Key, Value).

	% Delete minimum
	delete_min(t(Key, Value, t, Right), MinKey, MinValue, NewTree) :-
		!,
		MinKey = Key,
		MinValue = Value,
		NewTree = Right.
	delete_min(t(Key0, Value0, Left, Right), Key, Value, t(Key0, Value0, NewLeft, Right)) :-
		delete_min(Left, Key, Value, NewLeft).

	% Delete maximum
	delete_max(t(Key, Value, Left, t), MaxKey, MaxValue, NewTree) :-
		!,
		MaxKey = Key,
		MaxValue = Value,
		NewTree = Left.
	delete_max(t(Key0, Value0, Left, Right), Key, Value, t(Key0, Value0, Left, NewRight)) :-
		delete_max(Right, Key, Value, NewRight).

	% Keys in ascending order
	keys(Tree, Keys) :-
		keys_(Tree, [], Keys).

	keys_(t, Keys, Keys).
	keys_(t(Key, _, Left, Right), Keys0, Keys) :-
		keys_(Right, Keys0, Keys1),
		keys_(Left, [Key| Keys1], Keys).

	% Values in ascending order of keys
	values(Tree, Values) :-
		values_(Tree, [], Values).

	values_(t, Values, Values).
	values_(t(_, Value, Left, Right), Values0, Values) :-
		values_(Right, Values0, Values1),
		values_(Left, [Value| Values1], Values).

	% Delete
	delete(t, _, _, t) :-
		fail.
	delete(t(Key1, Value1, Left1, Right1), Key, Value, NewTree) :-
		compare(Order, Key, Key1),
		delete_(Order, Key1, Value1, Left1, Right1, Key, Value, NewTree).

	delete_(=, Key1, Value1, Left1, Right1, Key1, Value1, NewTree) :-
		join(Left1, Right1, NewTree).
	delete_(<, Key1, Value1, Left1, Right1, Key, Value, t(Key1, Value1, Left2, Right1)) :-
		delete(Left1, Key, Value, Left2).
	delete_(>, Key1, Value1, Left1, Right1, Key, Value, t(Key1, Value1, Left1, Right2)) :-
		delete(Right1, Key, Value, Right2).

	join(t, Right, Right) :-
		!.
	join(Left, t, Left) :-
		!.
	join(t(Key, Value, Left, Right), Tree, t(Key, Value, Left, Right2)) :-
		join(Right, Tree, Right2).

	% Update value for existing key
	update(OldTree, Key, NewValue, NewTree) :-
		update(OldTree, Key, _, NewValue, NewTree).

	update(t(Key1, Value1, Left1, Right1), Key, OldValue, NewValue, NewTree) :-
		compare(Order, Key, Key1),
		update_(Order, Key1, Value1, Left1, Right1, Key, OldValue, NewValue, NewTree).

	update_(=, Key, OldValue, Left, Right, Key, OldValue, NewValue, t(Key, NewValue, Left, Right)).
	update_(<, Key1, Value1, Left1, Right1, Key, OldValue, NewValue, t(Key1, Value1, Left2, Right1)) :-
		update(Left1, Key, OldValue, NewValue, Left2).
	update_(>, Key1, Value1, Left1, Right1, Key, OldValue, NewValue, t(Key1, Value1, Left1, Right2)) :-
		update(Right1, Key, OldValue, NewValue, Right2).

	update(OldTree, Pairs, NewTree) :-
		update_pairs(Pairs, OldTree, NewTree).

	update_pairs([], NewTree, NewTree).
	update_pairs([Key-NewValue| Pairs], OldTree, NewTree) :-
		update(OldTree, Key, _, NewValue, NewTree0),
		update_pairs(Pairs, NewTree0, NewTree).

	% Intersection
	intersection(Tree1, Tree2) :-
		as_list_(Tree1, [], Pairs),
		intersection_check(Pairs, Tree2).

	intersection_check([], _).
	intersection_check([Key-Value| Pairs], Tree) :-
		(	lookup_nonvar(Key, Value0, Tree) ->
			Value = Value0
		;	true
		),
		intersection_check(Pairs, Tree).

	intersection(Tree1, Tree2, Intersection) :-
		as_list_(Tree1, [], Pairs),
		new(Intersection0),
		intersection_build(Pairs, Tree2, Intersection0, Intersection).

	intersection_build([], _, Intersection, Intersection).
	intersection_build([Key-Value| Pairs], Tree, Intersection0, Intersection) :-
		(	lookup_nonvar(Key, Value0, Tree) ->
			Value = Value0,
			insert(Intersection0, Key, Value, Intersection1)
		;	Intersection1 = Intersection0
		),
		intersection_build(Pairs, Tree, Intersection1, Intersection).

	% Map a closure over all key-value pairs
	:- meta_predicate(map(1, *)).
	map(Closure, Tree) :-
		map_1(Tree, Closure).

	:- meta_predicate(map_1(*, 1)).
	map_1(t, _).
	map_1(t(Key, Value, Left, Right), Closure) :-
		call(Closure, Key-Value),
		map_1(Left, Closure),
		map_1(Right, Closure).

	% Map a closure over all key-value pairs, producing a new tree
	:- meta_predicate(map(2, *, *)).
	map(Closure, Old, New) :-
		map_2(Old, Closure, New).

	:- meta_predicate(map_2(*, 2, *)).
	map_2(t, _, t).
	map_2(t(Key, Value1, Left1, Right1), Closure, t(Key, Value2, Left2, Right2)) :-
		call(Closure, Key-Value1, Key-Value2),
		map_2(Left1, Closure, Left2),
		map_2(Right1, Closure, Right2).

	% Apply a closure to a specific key
	:- meta_predicate(apply(2, *, *, *)).
	apply(Closure, OldTree, Key, NewTree) :-
		apply_(OldTree, Key, Closure, NewTree).

	:- meta_predicate(apply_(*, *, 2, *)).
	apply_(t(Key0, Value0, Left, Right), Key, Closure, NewTree) :-
		compare(Order, Key0, Key),
		apply__(Order, Key0, Value0, Left, Right, Key, Closure, NewTree).

	:- meta_predicate(apply__(*, *, *, *, *, *, 2, *)).
	apply__(=, _, Value0, Left, Right, Key, Closure, t(Key, Value, Left, Right)) :-
		call(Closure, Key-Value0, Key-Value).
	apply__(>, Key0, Value0, Left, Right, Key, Closure, t(Key0, Value0, NewLeft, Right)) :-
		apply_(Left, Key, Closure, NewLeft).
	apply__(<, Key0, Value0, Left, Right, Key, Closure, t(Key0, Value0, Left, NewRight)) :-
		apply_(Right, Key, Closure, NewRight).

	% Size of the tree
	size(Tree, Size) :-
		size_(Tree, 0, Size).

	size_(t, Size, Size).
	size_(t(_, _, Left, Right), Acc, Size) :-
		size_(Left, Acc, Acc2),
		Acc3 is Acc2 + 1,
		size_(Right, Acc3, Size).

	% Validity check
	valid(Tree) :-
		nonvar(Tree),
		valid_(Tree).

	valid_(t).
	valid_(t(Key, _, t, t)) :-
		!,
		ground(Key).
	valid_(t(Key, _, t, t(RightKey, _, RightLeft, RightRight))) :-
		!,
		ground((Key, RightKey)),
		Key @< RightKey,
		valid_(t(RightKey, _, RightLeft, RightRight)).
	valid_(t(Key, _, t(LeftKey, _, LeftLeft, LeftRight), t)) :-
		!,
		ground((LeftKey, Key)),
		LeftKey @< Key,
		valid_(t(LeftKey, _, LeftLeft, LeftRight)).
	valid_(t(Key, _, Left, Right)) :-
		ground(Key),
		valid_(Left),
		valid_(Right),
		Left = t(LeftKey, _, _, _),
		Right = t(RightKey, _, _, _),
		ground((LeftKey, RightKey)),
		LeftKey @< Key,
		Key @< RightKey.

	check(Term) :-
		(	valid(Term) ->
			true
		;	var(Term) ->
			instantiation_error
		;	type_error(splaytree, Term)
		).

:- end_object.
