%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1990 Peter Van Roy and Regents of the University of California.
% All rights reserved.  This program may be freely used and modified for
% non-commercial purposes provided this copyright notice is kept unchanged.
% Written by Peter Van Roy as a part of the Aquarius project.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Benchmark based on the Aquarius compiler flow analyzer version 1.
% This program does a dataflow analysis of quicksort using abstract
% interpretation.  The lattice has two useful values: uninit and ground.
% Analysis takes three passes (it prints three 'x' characters).
% Builtins used: compare/3, arg/3, functor/3, sort/2, keysort/2, ==/2, \==/2.

top :- main(_).
% main :- main(Table), write(Table), nl.

main(Table) :-
	analyze_strees(
	  [stree(main/0,
	      (main:-
	       (qsort([1,2],L,[]),true
               ;fail
	       )),
              (main:-true),[],1),
	   stree(qsort/3,
	      (qsort(U,P,Q):-
	       (U=[N|O],part(O,N,R,S),qsort(S,T,Q),qsort(R,P,[N|T]),true
	       ;U=[],Q=P,true
	       ;fail
	       )),
	      (qsort(_,_,_):-true),[],1),
	   stree(part/4,
	      (part(W,X,Y,Z):-
	       ('$cut_load'(A1),'$cut_part/4_1'(W,X,Y,Z,A1),true
	       ;fail
	       )),
	      (part(_,_,_,_):-true),
	   [stree('$cut_part/4_1'/5,
	      ('$cut_part/4_1'(I1,E1,F1,G1,H1):-
	       (I1=[C1|D1],'$fac_$cut_part/4_1/5_2'(D1,E1,F1,G1,H1,C1),true
	       ;I1=[],F1=[],G1=[],true
	       ;fail
	       )),
	      ('$cut_part/4_1'(_,_,_,_,_):-true),
	    [stree('$fac_$cut_part/4_1/5_2'/6,
	      ('$fac_$cut_part/4_1/5_2'(K1,L1,Q1,O1,P1,M1):-
	       (Q1=[M1|N1],M1=<L1,'$cut_shallow'(P1),part(K1,L1,N1,O1),true
	       ;O1=[M1|R1],part(K1,L1,Q1,R1),true
	       ;fail
	       )),
	      ('$fac_$cut_part/4_1/5_2'(_,_,_,_,_,_):-true),[],1)
	    ],1)
	   ],1)
	  ], Table).

analyze_strees(Strees, OutTable) :-
	init_strees(Strees, _, Table),
	seal(Table),
	analyze_closure(Strees, Table, OutTable).

% Repeat traversal step until there are no more changes:
analyze_closure(Strees, InTable, OutTable) :-
	traverse_strees(Strees, InTable, MidTable, 0, Changes),
	% Mark an analysis pass:
	% put("x"), nl,
	analyze_closure(Strees, MidTable, OutTable, Changes).

analyze_closure(Strees, InTable, InTable, N) :- N=<0, !.
analyze_closure(Strees, InTable, OutTable, N) :- N>0, !,
	analyze_closure(Strees, InTable, OutTable).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initialize the table of call lattice values:

init_strees([],_4,_4) :-
   true.
init_strees([_12|_13],_4,_5) :-
   _12=stree(_14,(_15:-_16),_17,_18,_19),
   bottom_call(_14,_20),
   table_command(get(_14,_20),_4,_23),
   init_disj(_16,_23,_24),
   init_strees(_18,_24,_25),
   init_strees(_13,_25,_5),
   true.

init_conj(true,_4,_4) :-
   true.
init_conj((_12,_13),_4,_5) :-
   init_goal(_12,_4,_16),
   init_conj(_13,_16,_5),
   true.

init_disj(fail,_4,_4) :-
   true.
init_disj((_12;_13),_4,_5) :-
   init_conj(_12,_4,_16),
   init_disj(_13,_16,_5),
   true.

init_goal(_3,_4,_5) :-
   call_p(_3),
   !,
   functor(_3,_12,_13),
   bottom_call(_12/_13,_14),
   table_command(get(_12/_13,_14),_4,_5),
   true.
init_goal(_3,_4,_4) :-
   unify_p(_3),
   !,
   true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

traverse_strees([],_4,_4,_6,_6) :-
   true.
traverse_strees([_14|_15],_4,_5,_6,_7) :-
   _14=stree(_16,(_17:-_18),_19,_20,_21),
   traverse_disj(_17,_18,_4,_26,_6,_27),
   traverse_strees(_20,_26,_28,_27,_29),
   traverse_strees(_15,_28,_5,_29,_7),
   true.

traverse_disj(_3,fail,_5,_5,_7,_7) :-
   true.
traverse_disj(_3,(_15;_16),_5,_6,_7,_8) :-
   traverse_conj(_3,_15,_5,_22,_7,_23),
   traverse_disj(_3,_16,_22,_6,_23,_8),
   true.

traverse_conj(_3,_4,_5,_6,_7,_8) :-
   varset(_3,_24),
   functor(_3,_15,_16),
   table_command(get(_15/_16,_17),_5,_25),
   get_entry_modes(uninit,_3,_17,_26),
   get_entry_modes(ground,_3,_17,_27),
   traverse_conj(_4,_25,_6,_7,_8,_27,_28,_26,_29,_24,_30),
   true.

traverse_conj(true,_4,_4,_6,_6,_8,_8,_10,_10,_12,_12) :-
   true.
traverse_conj((_20,_21),_4,_5,_6,_7,_8,_9,_10,_11,_12,_13) :-
   varset(_20,_32),
   update_goal(_20,_32,_4,_33,_6,_34,_8,_35,_10,_36,_12,_37),
   unionv(_32,_37,_38),
   traverse_conj(_21,_33,_5,_34,_7,_35,_9,_36,_11,_38,_13),
   true.

update_goal(_3,_4,_5,_5,_7,_7,_9,_10,_11,_12,_13,_13) :-
   split_unify(_3,_21,_27),
   var(_21),
   nonvar(_27),
   varset(_27,_28),
   subsetv(_28,_9),
   !,
   set_command(add(_21),_9,_10),
   set_command(sub(_21),_11,_12),
   true.
update_goal(_3,_4,_5,_5,_7,_7,_9,_9,_11,_12,_13,_13) :-
   split_unify(_3,_21,_30),
   var(_21),
   nonvar(_30),
   inv(_21,_11),
   !,
   diffv(_4,_13,_31),
   diffv(_31,_9,_22),
   set_command(add_set(_22),_11,_32),
   set_command(sub(_21),_32,_33),
   intersectv(_4,_13,_23),
   set_command(sub_set(_23),_33,_12),
   true.
update_goal(_3,_4,_5,_5,_7,_7,_9,_10,_11,_12,_13,_13) :-
   split_unify(_3,_27,_28),
   var(_27),
   inv(_27,_9),
   !,
   set_command(add_set(_4),_9,_10),
   set_command(sub_set(_4),_11,_12),
   true.
update_goal(_3,_4,_5,_5,_7,_7,_9,_9,_11,_12,_13,_13) :-
   unify_p(_3),
   !,
   set_command(sub_set(_4),_11,_12),
   true.
update_goal(_3,_4,_5,_6,_7,_8,_9,_9,_11,_12,_13,_13) :-
   call_p(_3),
   !,
   goal_dupset(_3,_33),
   var_args(_3,_34),
   functor(_3,_22,_23),
   functor(_35,_22,_23),
   create_new_call(1,_23,_9,_34,_33,_11,_13,_3,_35),
   update_table(_22/_23,_35,_5,_6,_7,_8),
   set_command(sub_set(_4),_11,_12),
   true.

update_table(_15/_16,_4,_5,_6,_7,_8) :-
   table_command(get(_15/_16,_18),_5,_24),
   lub_call(_18,_4,_19),
   _18\==_19,
   !,
   table_command(set(_15/_16,_19),_24,_6),
   _8 is _7+1,
   true.
update_table(_15/_16,_4,_5,_5,_7,_7).

create_new_call(I, Ar, _, _, _, _, _, _, _) :- I>Ar, !.
create_new_call(I, Ar, Gnd, VarArgs, DupVars, Uni, SoFar, Goal, Call) :-
	I=<Ar,
	!,
	arg(I, Goal, X),
	arg(I, Call, Y),
	ground_flag(X, Gnd, Gf),
	membership_flag(X, VarArgs, Vf),
	membership_flag(X, DupVars, Df),
	membership_flag(X, Uni, Uf),
	membership_flag(X, SoFar, Sf),
	create_argument(Gf, Vf, Df, Uf, Sf, Y),
	I1 is I+1,
	create_new_call(I1, Ar, Gnd, VarArgs, DupVars, Uni, SoFar, Goal, Call).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lattice utilities:

lub(unknown,       X,      X) :- !.
lub(      X, unknown,      X) :- !.
lub(    any,       _,    any) :- !.
lub(      _,     any,    any) :- !.
lub( uninit,  uninit, uninit) :- !.
lub( ground,  ground, ground) :- !.
lub( uninit,  ground,    any) :- !.
lub( ground,  uninit,    any) :- !.

bottom(unknown).

create_argument(yes,   _,   _,   _,   _, ground) :- !. % Ground argument.
create_argument( no, yes,  no, yes,   _, uninit) :- !. % Non-duplicated uninit.
create_argument( no, yes,  no,   _,  no, uninit) :- !. % First occurrence.
create_argument( no, yes,   _,  no, yes, any) :- !.    % Already initialized.
create_argument( no, yes, yes,   _,   _, any) :- !.    % Duplicated argument.
create_argument( no,  no,   _,   _,   _, any) :- !.    % Non-variable argument.

lub_call(Call1, Call2, Lub) :-
	functor(Call1, Na, Ar),
	functor(Call2, Na, Ar),
	functor(Lub, Na, Ar),
	lub_call(1, Ar, Call1, Call2, Lub).

lub_call(I, Ar, _, _, _) :- I>Ar, !.
lub_call(I, Ar, Call1, Call2, Lub) :- I=<Ar, !,
	arg(I, Call1, X1),
	arg(I, Call2, X2),
	arg(I, Lub, X),
	lub(X1, X2, X),
	I1 is I+1,
	lub_call(I1, Ar, Call1, Call2, Lub).

bottom_call(Na/Ar, Bottom) :-
	functor(Bottom, Na, Ar),
	bottom_call(1, Ar, Bottom).

bottom_call(I, Ar, Bottom) :- I>Ar, !.
bottom_call(I, Ar, Bottom) :- I=<Ar, !,
	bottom(B),
	arg(I, Bottom, B),
	I1 is I+1,
	bottom_call(I1, Ar, Bottom).

lattice_modes_call(Na/Ar, Table, (Head:-Formula)) :-
	functor(Head, Na, Ar),
	get(Table, Na/Ar, Value),
	lattice_modes_call(1, Ar, Value, Head, Formula, true).

lattice_modes_call(I, Ar, _, _, Link, Link) :- I>Ar, !.
lattice_modes_call(I, Ar, Value, Head, Formula, Link) :- I=<Ar, !,
	arg(I, Value, T),
	arg(I, Head, X),
	lattice_modes_arg(T, X, Formula, Mid),
	I1 is I+1,
	lattice_modes_call(I1, Ar, Value, Head, Mid, Link).

lattice_modes_arg(uninit, X, (uninit(X),Link), Link) :- !.
lattice_modes_arg(ground, X, (ground(X),Link), Link) :- !.
lattice_modes_arg( Other, X, Link, Link).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Table utilities:

% This code implements a mutable array, represented as a binary tree.

% Access a value in logarithmic time and constant space:
% This predicate can be used to create the array incrementally.
get(node(N,W,L,R), I, V) :- get(N, W, L, R, I, V).

get(N, V, _, _, I, V) :- I=N, !.
get(N, _, L, R, I, V) :-
	compare(Order, I, N),
	get(Order, I, V, L, R).

get(<, I, V, L, _) :- get(L, I, V).
get(>, I, V, _, R) :- get(R, I, V).

set(leaf,          I, V, node(I,V,leaf,leaf)).
set(node(N,W,L,R), I, V, node(N,NW,NL,NR)) :-
	compare(Order, I, N),
	set_2(Order, I, V, W, L, R, NW, NL, NR).

set_2(<, I, V, W, L, R, W, NL, R) :- set(L, I, V, NL).
set_2(=, I, V, _, L, R, V, L, R).
set_2(>, I, V, W, L, R, W, L, NR) :- set(R, I, V, NR).

% Prevent any further insertions in the array:
seal(leaf).
seal(node(_,_,L,R)) :- seal(L), seal(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% General utilities:

membership_flag(X, Set, yes) :- inv(X, Set), !.
membership_flag(X, Set,  no).

ground_flag(X, Ground, yes) :- varset(X, Set), subsetv(Set, Ground), !.
ground_flag(X, Ground,  no).

get_entry_modes(Type, Head, Value, TypeSet) :-
	functor(Head, Na, Ar),
	get_entry_modes(Type, 1, Ar, Head, Value, Bag),
	sort(Bag, TypeSet).

get_entry_modes(_, I, Ar, _, _, []) :- I>Ar, !.
get_entry_modes(T, I, Ar, Head, Value, [X|Bag]) :- I=<Ar, arg(I, Value, T), !,
	arg(I, Head, X),
	I1 is I+1,
	get_entry_modes(T, I1, Ar, Head, Value, Bag).
get_entry_modes(T, I, Ar, Head, Value, Bag) :- I=<Ar, !,
	I1 is I+1,
	get_entry_modes(T, I1, Ar, Head, Value, Bag).

var_args(Goal, Set) :-
	functor(Goal, _, Ar),
	filter_vars(Ar, Goal, Bag),
	sort(Bag, Set).

filter_vars(Ar, Goal, Vs) :- filter_vars(Ar, Goal, Vs, []).

filter_vars(N, Goal) --> {N=<0}, !.
filter_vars(N, Goal) --> {N>0}, !,
	{arg(N, Goal, V)},
	filter_vars_arg(N, Goal, V).

filter_vars_arg(N, Goal, V) --> {var(V)}, !, [V],
	{N1 is N-1},
	filter_vars(N1, Goal).
filter_vars_arg(N, Goal, V) --> {nonvar(V)}, !,
	{N1 is N-1},
	filter_vars(N1, Goal).

goal_dupset(Goal, DupSet) :-
	goal_dupset_varbag(Goal, DupSet, _).

goal_dupset_varset(Goal, DupSet, VarSet) :-
	goal_dupset_varbag(Goal, DupSet, VarBag),
	sort(VarBag, VarSet).

goal_dupset_varbag(Goal, DupSet, VarBag) :-
	varbag(Goal, VarBag),
	make_key(VarBag, KeyBag),
	keysort(KeyBag, KeySet),
	filter_dups(KeySet, DupSet).

make_key([], []).
make_key([V|Bag], [V-dummy|KeyBag]) :- make_key(Bag, KeyBag).

filter_dups(KeySet, Set) :- filter_dups(KeySet, Set, []).

filter_dups([]) --> !.
filter_dups([V1-_,V2-_,V3-_|KeySet]) --> {V1==V2,V2==V3}, !,
	filter_dups([V2-_,V3-_|KeySet]).
filter_dups([V1-_,V2-_|KeySet]) --> {V1==V2}, !,
	[V1], filter_dups(KeySet).
filter_dups([V1-_|KeySet]) --> !,
	filter_dups(KeySet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Low-level utilities:

set_command(sub(X), In, Out) :- diffv(In, [X], Out).
set_command(add(X), In, Out) :- includev(X, In, Out).
set_command(sub_set(X), In, Out) :- diffv(In, X, Out).
set_command(add_set(X), In, Out) :- unionv(X, In, Out).

table_command(get(I,Val), In,  In) :- get(In, I, Val).
table_command(set(I,Val), In, Out) :- set(In, I, Val, Out).

% Set utilities inspired by R. O'Keefe in Practical Prolog:
inv(A, [B|S]) :-
	compare(Order, A, B),
	inv_2(Order, A, S).

inv_2(=, _, _).
inv_2(>, A, S) :- inv(A, S).

intersectv([], _, []).
intersectv([A|S1], S2, S) :- intersectv_2(S2, A, S1, S).

intersectv_2([], A, S1, []).
intersectv_2([B|S2], A, S1, S) :-
	compare(Order, A, B),
	intersectv_3(Order, A, S1, B, S2, S).

intersectv_3(<, A, S1, B, S2,     S) :- intersectv_2(S1, B, S2, S).
intersectv_3(=, A, S1, _, S2, [A|S]) :- intersectv(S1, S2, S).
intersectv_3(>, A, S1, B, S2,     S) :- intersectv_2(S2, A, S1, S).

diffv([], _, []).
diffv([A|S1], S2, S) :- diffv_2(S2, A, S1, S).

diffv_2([], A, S1, [A]).
diffv_2([B|S2], A, S1, S) :-
	compare(Order, A, B),
	diffv_3(Order, A, S1, B, S2, S).

diffv_3(<, A, S1, B, S2, [A|S]) :- diffv(S1, [B|S2], S).
diffv_3(=, A, S1, _, S2,     S) :- diffv(S1, S2, S).
diffv_3(>, A, S1, _, S2,     S) :- diffv_2(S2, A, S1, S).

unionv([], S2, S2).
unionv([A|S1], S2, S) :- unionv_2(S2, A, S1, S).

unionv_2([], A, S1, [A|S1]).
unionv_2([B|S2], A, S1, S) :-
	compare(Order, A, B),
	unionv_3(Order, A, S1, B, S2, S).

unionv_3(<, A, S1, B, S2, [A|S]) :- unionv_2(S1, B, S2, S).
unionv_3(=, A, S1, _, S2, [A|S]) :- unionv(S1, S2, S).
unionv_3(>, A, S1, B, S2, [B|S]) :- unionv_2(S2, A, S1, S).

includev(A, S1, S) :- includev_2(S1, A, S).

includev_2([], A, [A]).
includev_2([B|S1], A, S) :-
	compare(Order, A, B),
	includev_3(Order, A, B, S1, S).

includev_3(<, A, B, S1, [A,B|S1]).
includev_3(=, _, B, S1, [B|S1]).
includev_3(>, A, B, S1, [B|S]) :- includev_2(S1, A, S).

subsetv([], _).
subsetv([A|S1], [B|S2]) :-
	compare(Order, A, B),
	subsetv_2(Order, A, S1, S2).

subsetv_2(=, A, S1, S2) :- subsetv(S1, S2).
subsetv_2(>, A, S1, S2) :- subsetv([A|S1], S2).

varset(Term, VarSet) :- varbag(Term, VB), sort(VB, VarSet).
varbag(Term, VarBag) :- varbag(Term, VarBag, []).

varbag(Var) --> {var(Var)}, !, [Var].
varbag(Str) --> {nonvar(Str), !, functor(Str,_,Arity)}, varbag(Str, 1, Arity).

varbag(_Str, N, Arity) --> {N>Arity}, !.
varbag(Str, N, Arity) --> {N=<Arity}, !,
        {arg(N, Str, Arg)}, varbag(Arg),
        {N1 is N+1},
        varbag(Str, N1, Arity).

unify_p(_=_).

call_p(G) :- \+unify_p(G).

split_unify(X=Y, X, Y).
split_unify(Y=X, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
