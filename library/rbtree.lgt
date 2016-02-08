%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 2010 Vitor Santos Costa
%
%  This code implements Red-Black trees as described in:
%	 "Introduction to Algorithms", Second Edition
%	 Cormen, Leiserson, Rivest, and Stein,
%	 MIT Press
%
%  This file is dual-licensed under the Artistic License 2.0 (as
%  published by The Perl Foundation) and the GNU General Public
%  License 3 (as published by the Free Software Foundation) as follows.
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



:- object(rbtree,
	implements(dictionaryp),
	extends(compound)).

	:- info([
		version is 1.01,
		author is 'Vitor Santos Costa; adapted to Logtalk by Paulo Moura.',
		date is 2010/05/08,
		comment is 'Red-Black trees. Uses standard order to compare keys.'
	]).

	:- public(partial_map/4).
	:- meta_predicate(partial_map(*, *, 2, *)).
	:- mode(partial_map(+tree, +list, @closure, -tree), zero_or_one).
	:- info(partial_map/4, [
		comment is 'Applies a closure to the tree pairs identified by a set of keys.',
		argnames is ['Tree', 'Keys', 'Closure', 'NewTree']
	]).

	new(t(Nil, Nil)) :-
		Nil = black('', _, _, '').

	empty(t(Nil, Tree)) :-
		Nil == Tree,
		compound(Nil),
		functor(Nil, black, 4),
		arg(1, Nil, Arg1), Arg1 == '',
		arg(4, Nil, Arg4), Arg4 == ''.

	lookup(Key, Value, t(_, Tree)) :-
		(	var(Key) ->
			lookup_var(Key, Value, Tree)
		;	lookup_nonvar(Key, Value, Tree)
		).

	lookup_var(Key, Value, black(Left,CurrentKey,CurrentValue,Right)) :-
		Left \= '',
		enum_cases(Key, Value, Left, CurrentKey, CurrentValue, Right).
	lookup_var(Key, Value, red(Left,CurrentKey,CurrentValue,Right)) :-
		enum_cases(Key, Value, Left, CurrentKey, CurrentValue, Right).

	enum_cases(Key, Value, Left, _, _, _) :-
		lookup_var(Key, Value, Left).
	enum_cases(Key, Value, _, Key, Value, _).
	enum_cases(Key, Value, _, _, _, Right) :-
		lookup_var(Key, Value, Right).

	lookup_nonvar(_, _, black('',_,_,'')) :-
		!,
		fail.
	lookup_nonvar(Key, Value, Tree) :-
		arg(2, Tree, Key0),
		compare(Order, Key0, Key),
		lookup_nonvar(Order, Key, Value, Tree).

	lookup_nonvar(>, Key, Value, Tree) :-
		arg(1, Tree, NewTree),
		lookup_nonvar(Key, Value, NewTree).
	lookup_nonvar(<, Key, Value, Tree) :-
		arg(4, Tree, NewTree),
		lookup_nonvar(Key, Value, NewTree).
	lookup_nonvar(=, _, Value, Tree) :-
		arg(3, Tree, Value).

	min(t(_,Tree), Key, Value) :-
		min_key(Tree, Key, Value).

	min_key(red(black('', _, _, _), Key, Value, _), Key, Value) :-
		!.
	min_key(black(black('', _, _, _), Key, Value, _), Key, Value) :-
		!.
	min_key(red(Right, _, _, _), Key, Value) :-
		min_key(Right, Key, Value).
	min_key(black(Right, _, _, _), Key, Value) :-
		min_key(Right, Key, Value).

	max(t(_,Tree), Key, Value) :-
		max_key(Tree, Key, Value).

	max_key(red(_,Key,Value,black('',_,_,_)), Key, Value) :-
		!.
	max_key(black(_,Key,Value,black('',_,_,_)), Key, Value) :-
		!.
	max_key(red(_,_,_,Left), Key, Value) :-
		max_key(Left, Key, Value).
	max_key(black(_,_,_,Left), Key, Value) :-
		max_key(Left, Key, Value).

	next(t(_,Tree), Key, Next, Value) :-
		next(Tree, Key, Next, Value, []).

	next(black('',_,_,''), _, _, _, _) :-
		!,
		fail.
	next(Tree, Key, Next, Value, Candidate) :-
		arg(2, Tree, Key0),
		arg(3, Tree, Value0),
		compare(Order, Key0, Key),
		next(Order, Key, Key0, Value0, Next, Value, Tree, Candidate).

	next(>, Key, Key0, Value0, Next, Value, Tree, _) :-
		arg(1, Tree, NewTree),
		next(NewTree, Key, Next, Value, Key0-Value0).
	next(<, Key, _, _, Next, Value, Tree, Candidate) :-
		arg(4, Tree, NewTree),
		next(NewTree, Key, Next, Value, Candidate).
	next(=, _, _, _, Next, Value, Tree, Candidate) :-
		arg(4, Tree, NewTree),
		(	min_key(NewTree, Next, Value) ->
			true
		;	Candidate = (Next-Value)
		).

	previous(t(_,Tree), Key, Previous, Value) :-
		previous(Tree, Key, Previous, Value, []).

	previous(black('',_,_,''), _, _, _, _) :-
		!,
		fail.
	previous(Tree, Key, Previous, Value, Candidate) :-
		arg(2, Tree, Key0),
		arg(3, Tree, Value0),
		compare(Order, Key0, Key),
		previous(Order, Key, Key0, Value0, Previous, Value, Tree, Candidate).

	previous(>, Key, _, _, Previous, Value, Tree, Candidate) :-
		arg(1, Tree, NewTree),
		previous(NewTree, Key, Previous, Value, Candidate).
	previous(<, Key, Key0, Value0, Previous, Value, Tree, _) :-
		arg(4, Tree, NewTree),
		previous(NewTree, Key, Previous, Value, Key0-Value0).
	previous(=, _, _, _, Previous, Value, Tree, Candidate) :-
		arg(1, Tree, NewTree),
		(	max_key(NewTree, Previous, Value) ->
			true
		;	Candidate = (Previous-Value)
		).

	update(t(Nil,OldTree), Key, Value, t(Nil,NewTree)) :-
		update(OldTree, Key, _, Value, NewTree).

	update(t(Nil,OldTree), Key, OldValue, Value, t(Nil,NewTree)) :-
		update(OldTree, Key, OldValue, Value, NewTree).

	update(black(Left,Key0,Value0,Right), Key, OldValue, Value, NewTree) :-
		Left \= [],
		compare(Order, Key0, Key),
		update_black(Order, black(Left,Key0,Value0,Right), Key, OldValue, Value, NewTree).
	update(red(Left,Key0,Value0,Right), Key, OldValue, Value, NewTree) :-
		compare(Order, Key0, Key),
		update_red(Order, red(Left,Key0,Value0,Right), Key, OldValue, Value, NewTree).

	update_black(=, black(Left,Key0,Value0,Right), _, Value0, Value, black(Left,Key0,Value,Right)).
	update_black(>, black(Left,Key0,Value0,Right), Key, OldValue, Value, black(NewLeft,Key0,Value0,Right)) :-
		update(Left, Key, OldValue, Value, NewLeft).
	update_black(<, black(Left,Key0,Value0,Right), Key, OldValue, Value, black(Left,Key0,Value0,NewRight)) :-
		update(Right, Key, OldValue, Value, NewRight).

	update_red(=, red(Left,Key0,Value0,Right), _, Value0, Value, red(Left,Key0,Value,Right)).
	update_red(>, red(Left,Key0,Value0,Right), Key, OldValue, Value, red(NewLeft,Key0,Value0,Right)) :-
		update(Left, Key, OldValue, Value, NewLeft).
	update_red(<, red(Left,Key0,Value0,Right), Key, OldValue, Value, red(Left,Key0,Value0,NewRight)) :-
		update(Right, Key, OldValue, Value, NewRight).

	:- meta_predicate(apply(*, *, 2, *)).
	%apply(black('',_,_,''), _, _, _) :- !, fail.
	apply_(black(Left,Key0,Value0,Right), Key, Closure, black(NewLeft,Key0,Value,NewRight)) :-
		Left \= [],
		compare(Order, Key0, Key),
		apply_(Order, Left, Value0, Right, Key, Closure, t(NewLeft,Value,NewRight)).
	apply_(red(Left,Key0,Value0,Right), Key, Closure, red(NewLeft,Key0,Value,NewRight)) :-
		compare(Order, Key0, Key),
		apply_(Order, Left, Value0, Right, Key, Closure, t(NewLeft,Value,NewRight)).

	apply_(=, Left, Value0, Right, Key, Closure, t(Left,Value,Right)) :-
		call(Closure, Key-Value0, Key-Value).
	apply_(>, Left, Value0, Right, Key, Closure, t(NewLeft,Value0,Right)) :-
		apply_(Left, Key, Closure, NewLeft).
	apply_(<, Left, Value0, Right, Key, Closure, t(Left,Value0,NewRight)) :-
		apply_(Right, Key, Closure, NewRight).

	:- meta_predicate(apply(2, *, *, *)).
	apply(Closure, t(Nil,OldTree), Key, t(Nil,NewTree)) :-
		apply_(OldTree, Key, Closure, NewTree).

	/*******************************
	*	 TREE INSERTION		*
	*******************************/

	% We don't use parent nodes, so we may have to fix the root.

	%%	insert(+T0, +Key, ?Value, -TN)
	%
	%	Add an element with key Key and Value to the tree T0 creating a
	%	new red-black tree TN. If Key is a key in T0, the associated
	%	value is replaced by Value.

	insert(t(Nil, Tree0), Key, Value, t(Nil, Tree)) :-
		insert(Tree0, Key, Value, Nil, Tree1, _),
		fix_root(Tree1, Tree).

	%
	% Cormen et al present the algorithm as
	% (1) standard tree insertion;
	% (2) from the viewpoint of the newly inserted node:
	%     partially fix the tree;
	%     move upwards
	% until reaching the root.
	%
	% We do it a little bit different:
	%
	% (1) standard tree insertion;
	% (2) move upwards:
	%      when reaching a black node;
	%        if the tree below may be broken, fix it.
	% We take advantage of Prolog unification
	% to do several operations in a single go.
	%

	%
	% actual insertion
	%
	insert(black('',_,_,''), Key, Value, Nil, Tree, Status) :-
		!,
		Tree = red(Nil,Key,Value,Nil),
		Status = not_done.
	insert(red(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		compare(Order, Key, Key0),
		insert_red(Order, red(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag).
	insert(black(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		compare(Order, Key, Key0),
		insert_black(Order, black(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag).

	insert_red(<, red(Left,Key0,Value0,Right), Key, Value, Nil, red(NewLeft,Key0,Value0,Right), Flag) :-
		insert(Left, Key, Value, Nil, NewLeft, Flag).
	insert_red(=, red(Left,Key0,_,Right), _, Value, _, red(Left,Key0,Value,Right), done).
	insert_red(>, red(Left,Key0,Value0,Right), Key, Value, Nil, red(Left,Key0,Value0,NewRight), Flag) :-
		insert(Right, Key, Value, Nil, NewRight, Flag).

	insert_black(<, black(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		insert(Left, Key, Value, Nil, IL, Flag0),
		fix_left(Flag0, black(IL,Key0,Value0,Right), NewTree, Flag).
	insert_black(=, black(Left,Key0,_,Right), _, Value, _, black(Left,Key0,Value,Right), done).
	insert_black(>, black(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		insert(Right, Key, Value, Nil, IR, Flag0),
		fix_right(Flag0, black(Left,Key0,Value0,IR), NewTree, Flag).

	%
	% make sure the root is always black.
	%
	fix_root(black(Left,Key,Value,Right), black(Left,Key,Value,Right)).
	fix_root(red(Left,Key,Value,Right), black(Left,Key,Value,Right)).

	%
	% How to fix if we have inserted on the left
	%
	fix_left(done, Tree, Tree, done) :-
		!.
	fix_left(not_done, Tmp, Final, Done) :-
		fix_left(Tmp, Final, Done).

	%
	% case 1 of RB: just need to change colors.
	%
	fix_left(black(red(Al,AK,AV,red(Be,BK,BV,Ga)),KC,VC,red(De,KD,VD,Ep)),
		red(black(Al,AK,AV,red(Be,BK,BV,Ga)),KC,VC,black(De,KD,VD,Ep)),
		not_done) :- !.
	fix_left(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,red(De,KD,VD,Ep)),
		red(black(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,Ep)),
		not_done) :- !.
	%
	% case 2 of RB: got a knee so need to do rotations
	%
	fix_left(black(red(Al,KA,VA,red(Be,KB,VB,Ga)),KC,VC,De),
		black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
		done) :- !.
	%
	% case 3 of RB: got a line
	%
	fix_left(black(red(red(Al,KA,VA,Be),KB,VB,Ga),KC,VC,De),
		black(red(Al,KA,VA,Be),KB,VB,red(Ga,KC,VC,De)),
		done) :- !.
	%
	% case 4 of RB: nothing to do
	%
	fix_left(Tree, Tree, done).

	%
	% How to fix if we have inserted on the right
	%
	fix_right(done, Tree, Tree, done) :-
		!.
	fix_right(not_done, Tmp, Final, Done) :-
		fix_right(Tmp, Final, Done).

	%
	% case 1 of RB: just need to change colors.
	%
	fix_right(black(red(Ep,KD,VD,De),KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
		red(black(Ep,KD,VD,De),KC,VC,black(red(Ga,KB,VB,Be),KA,VA,Al)),
		not_done) :- !.
	fix_right(black(red(Ep,KD,VD,De),KC,VC,red(Ga,Ka,Va,red(Be,KB,VB,Al))),
		red(black(Ep,KD,VD,De),KC,VC,black(Ga,Ka,Va,red(Be,KB,VB,Al))),
		not_done) :- !.
	%
	% case 2 of RB: got a knee so need to do rotations
	%
	fix_right(black(De,KC,VC,red(red(Ga,KB,VB,Be),KA,VA,Al)),
		black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
		done) :- !.
	%
	% case 3 of RB: got a line
	%
	fix_right(black(De,KC,VC,red(Ga,KB,VB,red(Be,KA,VA,Al))),
		black(red(De,KC,VC,Ga),KB,VB,red(Be,KA,VA,Al)),
		done) :- !.
	%
	% case 4 of RB: nothing to do.
	%
	fix_right(Tree, Tree, done).

	delete(t(Nil,Tree), Key, Value, t(Nil,NewTree)) :-
		delete(Tree, Key, Value0, NewTree, _),
		Value = Value0.

	delete(red(Left,Key0,Value0,Right), Key, Value, NewTree, Flag) :-
		compare(Order, Key, Key0),
		delete_red(Order, red(Left,Key0,Value0,Right), Key, Value, NewTree, Flag).
	delete(black(Left,Key0,Value0,Right), Key, Value, NewTree, Flag) :-
		compare(Order, Key, Key0),
		delete_black(Order, black(Left,Key0,Value0,Right), Key, Value, NewTree, Flag).

	delete_red(<, red(Left,Key0,Value0,Right), Key, Value, NewTree, Flag) :-
		delete(Left, Key, Value, NewLeft, Flag0),
		fixup_left(Flag0, red(NewLeft,Key0,Value0,Right), NewTree, Flag).
	delete_red(>, red(Left,Key0,Value0,Right), Key, Value, NewTree, Flag) :-
		delete(Right, Key, Value, NewRight, Flag0),
		fixup_right(Flag0, red(Left,Key0,Value0,NewRight), NewTree, Flag).
	delete_red(=, red(Left,_,Value,Right), _, Value, OUT, Flag) :-
		delete_red_node(Left, Right, OUT, Flag).

	delete_black(<, black(Left,Key0,Value0,Right), Key, Value, NewTree, Flag) :-
		delete(Left, Key, Value, NewLeft, Flag0),
		fixup_left(Flag0, black(NewLeft,Key0,Value0,Right), NewTree, Flag).
	delete_black(>, black(Left,Key0,Value0,Right), Key, Value, NewTree, Flag) :-
		delete(Right, Key, Value, NewRight, Flag0),
		fixup_right(Flag0, black(Left,Key0,Value0,NewRight), NewTree, Flag).
	delete_black(=, black(Left,_,Value,Right), _, Value, OUT, Flag) :-
		delete_black_node(Left, Right, OUT, Flag).

	delete_min(t(Nil,Tree), Key, Value, t(Nil,NewTree)) :-
		delete_min(Tree, Key, Value, Nil, NewTree, _).

	delete_min(red(black('',_,_,_),Key,Value,Right), Key, Value, Nil, OUT, Flag) :-
		!,
		delete_red_node(Nil, Right, OUT, Flag).
	delete_min(red(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		delete_min(Left, Key, Value, Nil, NewLeft, Flag0),
		fixup_left(Flag0, red(NewLeft,Key0,Value0,Right), NewTree, Flag).
	delete_min(black(black('',_,_,_),Key,Value,Right), Key, Value, Nil, OUT, Flag) :-
		!,
		delete_black_node(Nil, Right, OUT, Flag).
	delete_min(black(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		delete_min(Left, Key, Value, Nil, NewLeft, Flag0),
		fixup_left(Flag0, black(NewLeft,Key0,Value0,Right), NewTree, Flag).

	delete_max(t(Nil,Tree), Key, Value, t(Nil,NewTree)) :-
		delete_max(Tree, Key, Value, Nil, NewTree, _).

	delete_max(red(Left,Key,Value,black('',_,_,_)), Key, Value, Nil, OUT, Flag) :-
		!,
		delete_red_node(Left, Nil, OUT, Flag).
	delete_max(red(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		delete_max(Right, Key, Value, Nil, NewRight, Flag0),
		fixup_right(Flag0, red(Left,Key0,Value0,NewRight), NewTree, Flag).
	delete_max(black(Left,Key,Value,black('',_,_,_)), Key, Value, Nil, OUT, Flag) :-
		!,
		delete_black_node(Left, Nil, OUT, Flag).
	delete_max(black(Left,Key0,Value0,Right), Key, Value, Nil, NewTree, Flag) :-
		delete_max(Right, Key, Value, Nil, NewRight, Flag0),
		fixup_right(Flag0, black(Left,Key0,Value0,NewRight), NewTree, Flag).

	delete_red_node(L1, L2, L1, done) :- L1 == L2, !.
	delete_red_node(black('',_,_,''), Right, Right, done) :-  !.
	delete_red_node(Left, black('',_,_,''), Left, done) :-  !.
	delete_red_node(Left, Right, OUT, Done) :-
		delete_next(Right, NewKey, NewValue, NewRight, Done0),
		fixup_right(Done0, red(Left,NewKey,NewValue,NewRight), OUT, Done).

	delete_black_node(L1, L2, L1, not_done) :- L1 == L2, !.
	delete_black_node(black('',_,_,''), red(Left,Key,Value,Right), black(Left,Key,Value,Right), done) :- !.
	delete_black_node(black('',_,_,''), Right, Right, not_done) :- !.
	delete_black_node(red(Left,Key,Value,Right), black('',_,_,''), black(Left,Key,Value,Right), done) :- !.
	delete_black_node(Left, black('',_,_,''), Left, not_done) :- !.
	delete_black_node(Left, Right, OUT, Done) :-
		delete_next(Right, NewKey, NewValue, NewRight, Done0),
		fixup_right(Done0, black(Left,NewKey,NewValue,NewRight), OUT, Done).

	delete_next(red(black('',_,_,''),Key,Value,Right), Key, Value, Right, done) :- !.
	delete_next(black(black('',_,_,''),Key,Value,red(L1,K1,V1,R1)), Key, Value, black(L1,K1,V1,R1), done) :- !.
	delete_next(black(black('',_,_,''),Key,Value,Right), Key, Value, Right, not_done) :- !.
	delete_next(red(Left,Key,Value,Right), Key0, Value0, OUT, Done) :-
		delete_next(Left, Key0, Value0, NewLeft, Done0),
		fixup_left(Done0, red(NewLeft,Key,Value,Right), OUT, Done).
	delete_next(black(Left,Key,Value,Right), Key0, Value0, OUT, Done) :-
		delete_next(Left, Key0, Value0, NewLeft, Done0),
		fixup_left(Done0, black(NewLeft,Key,Value,Right), OUT, Done).

	fixup_left(done, Tree, Tree, done).
	fixup_left(not_done, Tree, NewTree, Done) :-
		fixup2(Tree, NewTree, Done).

	%
	% case 1: x moves down, so we have to try to fix it again.
	% case 1 -> 2,3,4 -> done
	%
	fixup2(black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
		black(T1,KD,VD,black(Ep,KE,VE,Fi)),done) :- !,
		fixup2(red(black(Al,KA,VA,Be),KB,VB,black(Ga,KC,VC,De)),
			T1,
	                _).
	%
	% case 2: x moves up, change one to red
	%
	fixup2(red(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
		black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),done) :- !.
	fixup2(black(black(Al,KA,VA,Be),KB,VB,black(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
		black(black(Al,KA,VA,Be),KB,VB,red(black(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),not_done) :- !.
	%
	% case 3: x stays put, shift left and do a 4
	%
	fixup2(red(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
		red(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
		done) :- !.
	fixup2(black(black(Al,KA,VA,Be),KB,VB,black(red(Ga,KC,VC,De),KD,VD,black(Ep,KE,VE,Fi))),
		black(black(black(Al,KA,VA,Be),KB,VB,Ga),KC,VC,black(De,KD,VD,black(Ep,KE,VE,Fi))),
		done) :- !.
	%
	% case 4: rotate left, get rid of red
	%
	fixup2(red(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
		red(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
		done).
	fixup2(black(black(Al,KA,VA,Be),KB,VB,black(C,KD,VD,red(Ep,KE,VE,Fi))),
		black(black(black(Al,KA,VA,Be),KB,VB,C),KD,VD,black(Ep,KE,VE,Fi)),
		done).

	fixup_right(done, Tree, Tree, done).
	fixup_right(not_done, Tree, NewTree, Done) :-
		fixup3(Tree, NewTree, Done).

	%
	% case 1: x moves down, so we have to try to fix it again.
	% case 1 -> 2,3,4 -> done
	%
	fixup3(black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		black(black(Fi,KE,VE,Ep),KD,VD,T1),done) :- !,
		fixup3(red(black(De,KC,VC,Ga),KB,VB,black(Be,KA,VA,Al)),T1,_).
	%
	% case 2: x moves up, change one to red
	%
	fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		done) :- !.
	fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		black(red(black(Fi,KE,VE,Ep),KD,VD,black(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		not_done) :- !.
	%
	% case 3: x stays put, shift left and do a 4
	%
	fixup3(red(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		red(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
		done) :- !.
	fixup3(black(black(black(Fi,KE,VE,Ep),KD,VD,red(De,KC,VC,Ga)),KB,VB,black(Be,KA,VA,Al)),
		black(black(black(Fi,KE,VE,Ep),KD,VD,De),KC,VC,black(Ga,KB,VB,black(Be,KA,VA,Al))),
		done) :- !.
	%
	% case 4: rotate right, get rid of red
	%
	fixup3(red(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
		red(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
		done).
	fixup3(black(black(red(Fi,KE,VE,Ep),KD,VD,C),KB,VB,black(Be,KA,VA,Al)),
		black(black(Fi,KE,VE,Ep),KD,VD,black(C,KB,VB,black(Be,KA,VA,Al))),
		done).

	as_list(t(_,Tree), Pairs) :-
		as_list(Tree, [], Pairs).

	as_list(t(_,Tree), Pairs0, Pairs) :-
		as_list(Tree, Pairs0, Pairs).

	as_list(black('',_,_,_), Pairs, Pairs) :-
		!.
	as_list(red(Left,Key,Value,Right), Pairs0, Pairs) :-
		as_list(Left, [Key-Value| Pairs1], Pairs),
		as_list(Right, Pairs0, Pairs1).
	as_list(black(Left,Key,Value,Right), Pairs0, Pairs) :-
		as_list(Left, [Key-Value| Pairs1], Pairs),
		as_list(Right, Pairs0, Pairs1).

	:- meta_predicate(map(*, 2, *, *)).
	map_(black('',_,_,''), _, Nil, Nil) :-
		!.
	map_(red(Left,Key,Value,Right), Closure, red(NewLeft,Key,NewValue,NewRight), Nil) :-
		call(Closure, Key-Value, Key-NewValue),
		map_(Left, Closure, NewLeft, Nil),
		map_(Right, Closure, NewRight, Nil).
	map_(black(Left,Key,Value,Right), Closure, black(NewLeft,Key,NewValue,NewRight), Nil) :-
		call(Closure, Key-Value, NewValue),
		map_(Left, Closure, NewLeft, Nil),
		map_(Right, Closure, NewRight, Nil).

	:- meta_predicate(map(2, *, *)).
	map(Closure, t(Nil,Tree), t(Nil,NewTree)) :-
		map_(Tree, Closure, NewTree, Nil).

	:- meta_predicate(map_(*, 1)).
	map_(black('',_,_,''), _) :-
		!.
	map_(red(Left,Key,Value,Right), Closure) :-
		call(Closure, Key-Value),
		map_(Left, Closure),
		map_(Right, Closure).
	map_(black(Left,Key,Value,Right), Closure) :-
		call(Closure, Key-Value),
		map_(Left, Closure),
		map_(Right, Closure).

	:- meta_predicate(map(1, *)).
	map(Closure, t(_,Tree)) :-
		map_(Tree, Closure).

	clone(t(_,Tree), t(NewNil,NewTree), Pairs) :-
		NewNil = black('', _, _, ''),
		clone(Tree, NewNil, NewTree, [], Pairs).

	clone(black('',_,_,''), Nil, Nil, Pairs, Pairs) :-
		!.
	clone(red(Left,Key,_,Right), Nil, red(NewLeft,Key,NewValue,NewRight), Pairs0, Pairs) :-
		clone(Left, Nil, NewLeft, [Key-NewValue| Pairs1], Pairs),
		clone(Right, Nil, NewRight, Pairs0, Pairs1).
	clone(black(Left,Key,_,Right), Nil, black(NewLeft,Key,NewValue,NewRight), Pairs0, Pairs) :-
		clone(Left, Nil, NewLeft, [Key-NewValue| Pairs1], Pairs),
		clone(Right, Nil, NewRight, Pairs0, Pairs1).

	clone(t(Nil,Tree), OldPairs, t(Nil,NewTree), Pairs) :-
		clone(Tree, Nil, [], OldPairs, NewTree, [], Pairs).

	clone(black('',_,_,''), Nil, OldPairs, OldPairs, Nil, Pairs, Pairs) :-
		!.
	clone(red(Left,Key,Value,Right), Nil, OldPairs0, OldPairs, red(NewLeft,Key,NewValue,NewRight), Pairs0, Pairs) :-
		clone(Left, Nil, [Key-Value| OldPairs1], OldPairs, NewLeft, [Key-NewValue| Pairs1], Pairs),
		clone(Right, Nil, OldPairs0, OldPairs1, NewRight, Pairs0, Pairs1).
	clone(black(Left,Key,Value,Right), Nil, OldPairs0, OldPairs, black(NewLeft,Key,NewValue,NewRight), Pairs0, Pairs) :-
		clone(Left, Nil, [Key-Value| OldPairs1], OldPairs, NewLeft, [Key-NewValue| Pairs1], Pairs),
		clone(Right, Nil, OldPairs0, OldPairs1, NewRight, Pairs0, Pairs1).

%%	partial_map(+T, +Keys, :G, -TN)
%
%	For all nodes Key in Keys, if  the value associated with key Key
%	is Val0 in tree T,  and   if  call(G,Value0,ValF)  holds, then the
%	value associated with Key  in  TN  is   ValF.  Fails  if  or  if
%	call(G,Value0,ValF) is not satisfiable for  all Var0. Assumes keys
%	are not repeated.

	:- meta_predicate(partial_map(*, *, *, *, 2, *)).

	partial_map(t(Nil,Tree), Map, Closure, t(Nil,NewTree)) :-
		partial_map(Tree, Map, [], Nil, Closure, NewTree).

	partial_map(t(Nil,Tree), Map, Map0, Closure, t(Nil,NewTree)) :-
		partial_map(Tree, Map, Map0, Nil, Closure, NewTree).

	partial_map(T, [], [], _, _, T) :-
		!.
	partial_map(black('',_,_,_), Map, Map, Nil, _, Nil) :-
		!.
	partial_map(red(Left,Key,Value,Right), Map, MapF, Nil, Closure, red(NewLeft,Key,NewValue,NewRight)) :-
		partial_map(Left, Map, MapI, Nil, Closure, NewLeft),
		(	MapI == [] ->
			NewRight = Right, NewValue = Value, MapF = []
		;	MapI = [K1| MapR],
			(	Key == K1 ->
				(	call(Closure, Key-Value, Key-NewValue) ->
					true
				;	NewValue = Value
				),
				MapN = MapR
			;	NewValue = Value,
				MapN = MapI
			),
			partial_map(Right, MapN, MapF, Nil, Closure, NewRight)
		).
	partial_map(black(Left,Key,Value,Right), Map, MapF, Nil, Closure, black(NewLeft,Key,NewValue,NewRight)) :-
		partial_map(Left, Map, MapI, Nil, Closure, NewLeft),
		(	MapI == [] ->
			NewRight = Right, NewValue = Value, MapF = []
		;	MapI = [K1| MapR],
			(	Key == K1 ->
				(	call(Closure, Key-Value, Key-NewValue) ->
					true
				;	NewValue = Value
				),
				MapN = MapR
			;	NewValue = Value,
				MapN = MapI
			),
			partial_map(Right, MapN, MapF, Nil, Closure, NewRight)
		).

	keys(t(_,Tree), Keys) :-
		keys(Tree, [], Keys).

	keys(t(_,Tree), Keys0, Keys) :-
		keys(Tree, Keys0, Keys).
	keys(black('',_,_,''), Keys, Keys) :-
		!.
	keys(red(Left,Key,_,Right), Keys0, Keys) :-
		keys(Left, [Key| Keys1], Keys),
		keys(Right, Keys0, Keys1).
	keys(black(Left,Key,_,Right), Keys0, Keys) :-
		keys(Left, [Key| Keys1], Keys),
		keys(Right, Keys0, Keys1).

	as_dictionary(List, Tree) :-
		{sort(List, Sorted)},
		ord_list_to_rbtree(Sorted, Tree).

	ord_list_to_rbtree([], t(Nil,Nil)) :-
		!,
		Nil = black('', _, _, '').
	ord_list_to_rbtree([Key-Value], t(Nil,black(Nil,Key,Value,Nil))) :-
		!,
		Nil = black('', _, _, '').
	ord_list_to_rbtree(List, t(Nil,Tree)) :-
		Nil = black('', _, _, ''),
		Ar =.. [seq| List],
		functor(Ar, _, L),
		Height is truncate(log(L)/log(2)),
		construct_rbtree(1, L, Ar, Height, Nil, Tree).

	construct_rbtree(L, M, _, _, Nil, Nil) :-
		M < L,
		!.
	construct_rbtree(L, L, Ar, Depth, Nil, Node) :-
		!,
		arg(L, Ar, Key-Value),
		build_node(Depth, Nil, Key, Value, Nil, Node).
	construct_rbtree(I0, Max, Ar, Depth, Nil, Node) :-
		I is (I0 + Max) // 2,
		arg(I, Ar, Key-Value),
		build_node(Depth, Left, Key, Value, Right, Node),
		I1 is I - 1,
		NewDepth is Depth - 1,
		construct_rbtree(I0, I1, Ar, NewDepth, Nil, Left),
		I2 is I + 1,
		construct_rbtree(I2, Max, Ar, NewDepth, Nil, Right).

	build_node(0, Left, Key, Value, Right, red(Left, Key, Value, Right)) :-
		!.
	build_node(_, Left, Key, Value, Right, black(Left, Key, Value, Right)).

	size(t(_,Tree), Size) :-
		size(Tree, 0, Size).

	size(black('',_,_,_), Size, Size) :-
		!.
	size(red(Left,_,_,Right), Size0, Size) :-
		Size1 is Size0 + 1,
		size(Left, Size1, Size2),
		size(Right, Size2, Size).
	size(black(Left,_,_,Right), Size0, Size) :-
		Size1 is Size0 + 1,
		size(Left, Size1, Size2),
		size(Right, Size2, Size).

	valid(X) :-
		var(X),
		!,
		fail.
	valid(t(Nil,Nil)) :-
		!.
	valid(t(_,Tree)) :-
		catch(check(Tree), _, fail).

	check(black(Left,Key,_,Right)) :-
		find_path_blacks(Left, 0, Bls),
		check_rbtree(Left, -inf, Key, Bls),
		check_rbtree(Right, Key, +inf, Bls).
	check(red(_,_,_,_)) :-
		throw(root_should_be_black).

	find_path_blacks(black('',_,_,''), Bls, Bls) :-
		!.
	find_path_blacks(black(Left,_,_,_), Bls0, Bls) :-
		Bls1 is Bls0 + 1,
		find_path_blacks(Left, Bls1, Bls).
	find_path_blacks(red(Left,_,_,_), Bls0, Bls) :-
		find_path_blacks(Left, Bls0, Bls).

	check_rbtree(black('',_,_,''), Min, Max, Bls0) :-
		!,
		check_height(Bls0, Min, Max).
	check_rbtree(red(Left,Key,_,Right), Min, Max, Bls) :-
		check_val(Key, Min, Max),
		check_red_child(Left),
		check_red_child(Right),
		check_rbtree(Left, Min, Key, Bls),
		check_rbtree(Right, Key, Max, Bls).
	check_rbtree(black(Left,Key,_,Right), Min, Max, Bls0) :-
		check_val(Key, Min, Max),
		Bls is Bls0 - 1,
		check_rbtree(Left, Min, Key, Bls),
		check_rbtree(Right, Key, Max, Bls).

	check_height(0, _, _) :-
		!.
	check_height(Bls0, Min, Max) :-
		throw(unbalance_between(Bls0,Min,Max)).

	check_val(Key, Min, Max) :-
		(Key @> Min ; Min == -inf),
		(Key @< Max ; Max == +inf),
		!.
	check_val(Key, Min, Max) :-
		throw(not_ordered_between(Key,Min,Max)).

	check_red_child(black(_,_,_,_)).
	check_red_child(red(_,Key,_,_)) :-
		throw(must_be_red(Key)).

:- end_object.
