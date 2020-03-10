%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%
%  Author: R.A.O'Keefe, L.Damas, V.S.Costa, Glenn Burgess,
%          Jiri Spitz, and Jan Wielemaker
%  Logtalk port and additional predicates: Paulo Moura
%
%  Copyright 2004-2018 various people and institutions
%  Copyright 2019 Paulo Moura
%  All rights reserved.
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions
%  are met:
%
%  1. Redistributions of source code must retain the above copyright
%     notice, this list of conditions and the following disclaimer.
%
%  2. Redistributions in binary form must reproduce the above copyright
%     notice, this list of conditions and the following disclaimer in
%     the documentation and/or other materials provided with the
%     distribution.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(avltree,
	implements(dictionaryp),
	extends(compound)).

	:- info([
		version is 1:3:0,
		author is 'R.A.O''Keefe, L.Damas, V.S.Costa, Glenn Burgess, Jiri Spitz, and Jan Wielemaker; Logtalk port and additional predicates by Paulo Moura',
		date is 2020-03-10,
		comment is 'AVL tree implementation of the dictionary protocol. Uses standard order to compare keys.',
		see_also is [bintree, rbtree]
	]).

	as_dictionary([], t).
	as_dictionary([Pair| Pairs], Tree) :-
		keysort([Pair| Pairs], SortedPairs),
		list::length(SortedPairs, Length),
		as_dictionary(Length, SortedPairs, [], _, Tree).

	as_dictionary(1, [Key-Value| Pairs], Pairs, 1, t(Key,Value,-,t,t)) :-
		!.
	as_dictionary(2, [Key1-Value1,Key2-Value2| Pairs], Pairs, 2, t(Key2,Value2,<,t(Key1,Value1,-,t,t),t)) :-
		!.
	as_dictionary(N, List, Pairs, Depth, t(Key,Value,Balance,Left,Right)) :-
		N0 is N - 1,
		RN is N0 div 2,
		Rem is N0 mod 2,
		LN is RN + Rem,
		as_dictionary(LN, List, [Key-Value|Upper], LeftDepth, Left),
		as_dictionary(RN, Upper, Pairs, RightDepth, Right),
		Depth is LeftDepth + 1,
		compare(Order, RightDepth, LeftDepth),
		balance(Order, Balance).

	as_list(Tree, Pairs) :-
		as_list(Tree, [], Pairs).

	as_list(t, Pairs, Pairs).
	as_list(t(Key,Value,_,Left,Right), Pairs0, Pairs) :-
		as_list(Right, Pairs0, Pairs1),
		as_list(Left, [Key-Value| Pairs1], Pairs).

	clone(Tree, Clone, Pairs) :-
		clone_3(Tree, Clone, [], Pairs).

	clone_3(t, t, Pairs, Pairs).
	clone_3(t(Key,_,Balance,Left,Right), t(Key,NewValue,Balance,CloneLeft,CloneRight), Pairs0, Pairs) :-
		clone_3(Left, CloneLeft, [Key-NewValue| Pairs1], Pairs),
		clone_3(Right, CloneRight, Pairs0, Pairs1).

	clone(Tree, Pairs, Clone, ClonePairs) :-
		clone_4(Tree, [], Pairs, Clone, [], ClonePairs).

	clone_4(t, Pairs, Pairs, t, ClonePairs, ClonePairs).
	clone_4(t(Key,Value,Balance,Left,Right), Pairs0, Pairs, t(Key,NewValue,Balance,CloneLeft,CloneRight), ClonePairs0, ClonePairs) :-
		clone_4(Left, [Key-Value| Pairs1], Pairs, CloneLeft, [Key-NewValue| ClonePairs1], ClonePairs),
		clone_4(Right, Pairs0, Pairs1, CloneRight, ClonePairs0, ClonePairs1).

	empty(Tree) :-
		Tree == t.

	insert(Tree, Key, Value, NewTree) :-
		insert(Tree, Key, Value, NewTree, _).

	insert(t, Key, Value, t(Key,Value,-,t,t), yes) :-
		nonvar(Key).
	insert(t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, Changed) :-
		compare(Order, Key, Key1),
		insert(Order, t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, Changed).

	insert(=, t(Key,_,Balance,Left,Right), _, Value, t(Key,Value,Balance,Left,Right), no).
	insert(<, t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, Changed) :-
		insert(Left1, Key, Value, NewLeft, LeftChanged),
		adjust(LeftChanged, t(Key1,Value1,Balance1,NewLeft,Right1), left, NewTree, Changed).
	insert(>, t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, Changed) :-
		insert(Right1, Key, Value, NewRight, RightChanged),
		adjust(RightChanged, t(Key1,Value1,Balance1,Left1,NewRight), right, NewTree, Changed).

	adjust(no, Oldree, _, Oldree, no).
	adjust(yes, t(Key,Value,Balance0,Left,Right), LeftOrRight, NewTree, Changed) :-
		table(Balance0, LeftOrRight, Balance1, Changed, ToBeRebalanced),
		rebalance(ToBeRebalanced, t(Key,Value,Balance0,Left,Right), Balance1, NewTree, _, _).

	%     balance  where     balance  whole tree  to be
	%     before   inserted  after    increased   rebalanced
	table((-)    , left    , (<)     , yes       , no    ) :- !.
	table((-)    , right   , (>)     , yes       , no    ) :- !.
	table((<)    , left    , (-)     , no        , yes   ) :- !.
	table((<)    , right   , (-)     , no        , no    ) :- !.
	table((>)    , left    , (-)     , no        , no    ) :- !.
	table((>)    , right   , (-)     , no        , yes   ) :- !.

	next(Tree, Key, Next, Value) :-
		next(Tree, Key, Next, Value, []).

	next(t(Key0,Value0,_,Left,Right), Key, Next, Value, Candidate) :-
		compare(Order, Key0, Key),
		next(Order, Key0, Value0, Left, Right, Key, Next, Value, Candidate).

	next(>, Key0, Value0, Left, _, Key, Next, Value, _) :-
		next(Left, Key, Next, Value, Key0-Value0).
	next(<, _, _, _, Right, Key, Next, Value, Candidate) :-
		next(Right, Key, Next, Value, Candidate).
	next(=, _, _, _, Right, _, Next, Value, Candidate) :-
		(	min(Right, Next, Value) ->
			true
		;	Candidate = (Next-Value)
		).

	previous(Tree, Key, Previous, Value) :-
		previous(Tree, Key, Previous, Value, []).

	previous(t(Key0,Value0,_,Left,Right), Key, Previous, Value, Candidate) :-
		compare(Order, Key0, Key),
		previous(Order, Key0, Value0, Left, Right, Key, Previous, Value, Candidate).

	previous(>, _, _, Left, _, Key, Previous, Value, Candidate) :-
		previous(Left, Key, Previous, Value, Candidate).
	previous(<, Key0, Value0, _, Right, Key, Previous, Value, _) :-
		previous(Right, Key, Previous, Value, Key0-Value0).
	previous(=, _, _, Left, _, _, Previous, Value, Candidate) :-
		(	max(Left, Previous, Value) ->
			true
		;	Candidate = (Previous-Value)
		).

	lookup(Key, Value, Tree) :-
		(	var(Key) ->
			lookup_var(Key, Value, Tree)
		;	lookup_nonvar(Key, Value, Tree)
		).

	lookup_nonvar(Key, Value, t(Key1,Value1,_,Left1,Right1)) :-
		compare(Order, Key, Key1),
		lookup_nonvar(Order, Key, Value, Value1, Left1, Right1).

	lookup_nonvar(=, _, Value, Value, _, _).
	lookup_nonvar(<, Key, Value, _, Left, _) :-
		lookup_nonvar(Key, Value, Left).
	lookup_nonvar(>, Key, Value, _, _, Right) :-
		lookup_nonvar(Key, Value, Right).

	lookup_var(Key, Value, t(_,_,_,Left,_)) :-
		lookup_var(Key, Value, Left).
	lookup_var(Key, Value, t(Key, Value,_,_,_)).
	lookup_var(Key, Value, t(_,_,_,_,Right)) :-
		lookup_var(Key, Value, Right).

	lookup([], _).
	lookup([Key-Value| Pairs], Tree) :-
		lookup(Key, Value, Tree),
		lookup(Pairs, Tree).

	min(t(Key,Value,_,t,_), MinKey, MinValue) :-
		!,
		MinKey = Key,
		MinValue = Value.
	min(t(_,_,_,Left,_), Key, Value) :-
		min(Left, Key, Value).

	max(t(Key,Value,_,_,t), MaxKey, MaxValue) :-
		!,
		MaxKey = Key,
		MaxValue = Value.
	max(t(_,_,_,_,Right), Key, Value) :-
		max(Right, Key, Value).

	delete_min(Tree, MinKey, MinValue, NewTree) :-
		delete_min(Tree, MinKey, MinValue, NewTree, _).

	delete_min(t(Key,Value,_,t,Right), Key, Value, Right, yes) :-
		!.
	delete_min(t(Key,Value,Balance,Left,Right), MinKey, MinValue, NewTree, Changed) :-
		delete_min(Left, MinKey, MinValue, NewLeft, LeftChanged),
		deladjust(LeftChanged, t(Key,Value,Balance,NewLeft,Right), left, NewTree, Changed).

	delete_max(Tree, MaxKey, MaxValue, NewTree) :-
		delete_max(Tree, MaxKey, MaxValue, NewTree, _).

	delete_max(t(MaxKey,MaxValue,_,Left,t), MaxKey, MaxValue, Left, yes) :-
		!.
	delete_max(t(Key,Value,Balance,Left,Right), MaxKey, MaxValue, NewTree, Changed) :-
		delete_max(Right, MaxKey, MaxValue, NewRight, RightChanged),
		deladjust(RightChanged, t(Key,Value,Balance,Left,NewRight), right, NewTree, Changed).

	keys(Tree, Keys) :-
		keys(Tree, [], Keys).

	keys(t, Keys, Keys).
	keys(t(Key,_,_,Left,Right), Keys0, Keys) :-
		keys(Right, Keys0, Keys1),
		keys(Left, [Key| Keys1], Keys).

	values(Tree, Values) :-
		values(Tree, [], Values).

	values(t, Values, Values).
	values(t(_,Value,_,Left,Right), Values0, Values) :-
		values(Right, Values0, Values1),
		values(Left, [Value| Values1], Values).

	delete(t, _, _, t) :-
		fail.
	delete(t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree) :-
		delete(t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, _).

	delete(t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, WhatHasChanged) :-
		compare(Order, Key, Key1),
		delete(Order, t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, WhatHasChanged).

	delete(=, t(Key,Value,_,t,Right), Key, Value, Right, yes) :-
		!.
	delete(=, t(Key,Value,_,Left,t), Key, Value, Left, yes) :-
		!.
	delete(=, t(Key,Value,>,Left,Right), Key, Value, NewTree, WhatHasChanged) :-
		% Rh tree is deeper, so rotate from R to L
		delete_min(Right, MinKey, MinValue, NewRight, RightHasChanged),
		deladjust(RightHasChanged, t(MinKey,MinValue,>,Left,NewRight), right, NewTree, WhatHasChanged),
		!.
	delete(=, t(Key,Value,Balance,Left,Right), Key, Value, NewTree, WhatHasChanged) :-
		% Rh tree is not deeper, so rotate from L to R
		delete_max(Left, MaxKey, MaxValue, NewLeft, LeftHasChanged),
		deladjust(LeftHasChanged, t(MaxKey,MaxValue,Balance,NewLeft,Right), left, NewTree, WhatHasChanged),
		!.
	delete(<, t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, WhatHasChanged) :-
		delete(Left1, Key, Value, NewLeft, LeftHasChanged),
		deladjust(LeftHasChanged, t(Key1,Value1,Balance1,NewLeft,Right1), left, NewTree, WhatHasChanged).
	delete(>, t(Key1,Value1,Balance1,Left1,Right1), Key, Value, NewTree, WhatHasChanged) :-
		delete(Right1, Key, Value, NewRight, RightHasChanged),
		deladjust(RightHasChanged, t(Key1,Value1,Balance1,Left1,NewRight), right, NewTree, WhatHasChanged).

	deladjust(no, OldTree, _, OldTree, no).
	deladjust(yes, t(Key,Value,B0,L,R), LoR, NewTree, RealChange) :-
		deltable(B0, LoR, B1, WhatHasChanged, ToBeRebalanced),
		rebalance(ToBeRebalanced, t(Key,Value,B0,L,R), B1, NewTree, WhatHasChanged, RealChange).

	%     balance  where     balance  whole tree  to be
	%     before   deleted   after    changed   rebalanced
	deltable(-      , right   , <      , no        , no    ) :- !.
	deltable(-      , left    , >      , no        , no    ) :- !.
	deltable(<      , right   , -      , yes       , yes   ) :- !.
	deltable(<      , left    , -      , yes       , no    ) :- !.
	deltable(>      , right   , -      , yes       , no    ) :- !.
	deltable(>      , left    , -      , yes       , yes   ) :- !.
	% It depends on the tree pattern in avl_geq whether it really decreases.

	% Single and double tree rotations - these are common for insert and delete.
	/* The patterns (>)-(>), (>)-( <), ( <)-( <) and ( <)-(>) on the LHS
	   always change the tree height and these are the only patterns which can
	   happen after an insertion. That's the reason why we can use a table only to
	   decide the needed changes.

	   The patterns (>)-( -) and ( <)-( -) do not change the tree height. After a
	   deletion any pattern can occur and so we return yes or no as a flag of a
	   height change.  */

	rebalance(no, t(Key,Value,_,Left,Right), Balance, t(Key,Value,Balance,Left,Right), Changed, Changed).
	rebalance(yes, OldTree, _, NewTree, _, RealChange) :-
		avl_geq(OldTree, NewTree, RealChange).

	avl_geq(t(A,VA,>,Alpha,t(B,VB,>,Beta,Gamma)),
	        t(B,VB,-,t(A,VA,-,Alpha,Beta),Gamma), yes) :- !.
	avl_geq(t(A,VA,>,Alpha,t(B,VB,-,Beta,Gamma)),
	        t(B,VB,<,t(A,VA,>,Alpha,Beta),Gamma), no) :- !.
	avl_geq(t(B,VB,<,t(A,VA,<,Alpha,Beta),Gamma),
	        t(A,VA,-,Alpha,t(B,VB,-,Beta,Gamma)), yes) :- !.
	avl_geq(t(B,VB,<,t(A,VA,-,Alpha,Beta),Gamma),
	        t(A,VA,>,Alpha,t(B,VB,<,Beta,Gamma)), no) :- !.
	avl_geq(t(A,VA,>,Alpha,t(B,VB,<,t(X,VX,B1,Beta,Gamma),Delta)),
	        t(X,VX,-,t(A,VA,B2,Alpha,Beta),t(B,VB,B3,Gamma,Delta)), yes) :-
		!,
		table2(B1, B2, B3).
	avl_geq(t(B,VB,<,t(A,VA,>,Alpha,t(X,VX,B1,Beta,Gamma)),Delta),
	        t(X,VX,-,t(A,VA,B2,Alpha,Beta),t(B,VB,B3,Gamma,Delta)), yes) :-
		!,
		table2(B1, B2, B3).

	table2((<), (-), (>)).
	table2((>), (<), (-)).
	table2((-), (-), (-)).

	update(OldTree, Key, NewValue, NewTree) :-
		update(OldTree, Key, _, NewValue, NewTree).

	update(t(Key1,Value1,Balance1,Left1,Right1), Key, OldValue, NewValue, NewTree) :-
		compare(Order, Key, Key1),
		update(Order, Key1, Value1, Balance1, Left1, Right1, Key, OldValue, NewValue, NewTree).

	update(=, Key, OldValue, Balance, Left, Right, Key, OldValue, NewValue, t(Key,NewValue,Balance,Left,Right)).
	update(<, Key1, Value1, Balance1, Left1, Right1, Key, OldValue, NewValue, t(Key1,Value1,Balance1,Left2,Right1)) :-
		update(Left1, Key, OldValue, NewValue, Left2).
	update(>, Key1, Value1, Balance1, Left1, Right1, Key, OldValue, NewValue, t(Key1,Value1,Balance1,Left1,Right2)) :-
		update(Right1, Key, OldValue, NewValue, Right2).

	update(OldTree, Pairs, NewTree) :-
		update_(Pairs, OldTree, NewTree).

	update_([], NewTree, NewTree).
	update_([Key-NewValue| Pairs], OldTree, NewTree) :-
		update(OldTree, Key, _, NewValue, NewTree0),
		update_(Pairs, NewTree0, NewTree).

	:- meta_predicate(map_(*, 1)).
	map_(t(Key,Value,_,Left,Right), Closure) :-
		call(Closure, Key-Value),
		map_(Left, Closure),
		map_(Right, Closure).
	map_(t, _).

	:- meta_predicate(map(1, *)).
	map(Closure, Tree) :-
		map_(Tree, Closure).

	:- meta_predicate(map_(*, 2, *)).
	map_(t(Key,Value1,Balance,Left1,Right1), Closure, t(Key,Value2,Balance,Left2,Right2)) :-
		call(Closure, Key-Value1, Key-Value2),
		map_(Left1, Closure, Left2),
		map_(Right1, Closure, Right2).
	map_(t, _, t).

	:- meta_predicate(map(2, *, *)).
	map(Closure, Old, New) :-
		map_(Old, Closure, New).

	:- meta_predicate(apply_(*, *, 2, *)).
	apply_(t(Key0,Value0,Balance,Left,Right), Key, Closure, NewTree) :-
		compare(Order, Key0, Key),
		apply_(Order, Key0, Value0, Balance, Left, Right, Key, Closure, NewTree).

	:- meta_predicate(apply_(*, *, *, *, *, *, *, 2, *)).
	apply_(=,    _, Value0, Balance, Left, Right, Key, Closure, t(Key,Value,Balance,Left,Right)) :-
		call(Closure, Key-Value0, Key-Value).
	apply_(>, Key0, Value0, Balance, Left, Right, Key, Closure, t(Key0,Value0,Balance,NewLeft,Right)) :-
		apply_(Left, Key, Closure, NewLeft).
	apply_(<, Key0, Value0, Balance, Left, Right, Key, Closure, t(Key0,Value0,Balance,Left,NewRight)) :-
		apply_(Right, Key, Closure, NewRight).

	:- meta_predicate(apply(2, *, *, *)).
	apply(Closure, OldTree, Key, NewTree) :-
		apply_(OldTree, Key, Closure, NewTree).

	new(t).

	valid(Tree) :-
		nonvar(Tree),
		valid(Tree, _Min, _Max, _Depth).

	valid(t, X, X, 0).
	valid(t(Key,_,-,t,t), Key, Key, 1) :-
		!,
		ground(Key).
	valid(t(Key,_,>,t,t(RightKey,_,-,t,t)), Key, RightKey, 2) :-
		% Ensure right side Key is 'greater' than K
		!,
		ground((Key,RightKey)),
		Key @< RightKey.
	valid(t(Key,_,<,t(LeftKey,_,-,t,t),t), LeftKey, Key, 2) :-
		% Ensure left side Key is 'less' than K
		!,
		ground((LeftKey,Key)),
		LeftKey @< Key.
	valid(t(Key,_,Balance,Left,Right), Min, Max, Depth) :-
		valid(Left, Min, LeftMax, LeftDepth),
		valid(Right, RightMin, Max, RightDepth),
		% Ensure Balance matches depth
		compare(Order, RightDepth, LeftDepth),
		balance(Order, Balance),
		% Ensure ordering
		ground((LeftMax,Key,RightMin)),
		LeftMax @< Key,
		Key @< RightMin,
		Depth is max(LeftDepth, RightDepth) + 1.

	% Private lookup table matching comparison operators to Balance operators used in tree
	balance((=), (-)).
	balance((<), (<)).
	balance((>), (>)).

	size(Tree, Size) :-
		size(Tree, 0, Size).

	size(t, Size, Size).
	size(t(_, _, _, Left, Right), Size0, Size) :-
		size(Left, Size0, Size1),
		Size2 is Size1 + 1,
		size(Right, Size2, Size).

:- end_object.
