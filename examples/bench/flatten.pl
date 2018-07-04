% preprocessing phase to eliminate disjunctions from the code

% takes a list of clauses of the form source(Name,Clause)
% returns these clauses with disjunctions replaced by dummy calls
% and a list of NewClauses corresponding to those dummy calls
% Link is the uninstantiated last cdr of this list

top:-
	eliminate_disjunctions([(a(A,B,C):-(b(A);c(C)))],X,Y,[]),
    inst_vars((X,Y)).
%	write((X,Y)), nl,
	% (X,Y) == ([(a:-'_dummy_0')],[('_dummy_0':-b),('_dummy_0':-c)]),
%	write(ok), nl.
top :- true.
%write(wrong), nl.

eliminate_disjunctions(OneProc,NewProc,NewClauses,Link) :-
	gather_disj(OneProc,NewProc,Disj,[]),
    treat_disj(Disj,NewClauses,Link).

gather_disj([],[],Link,Link).
gather_disj([C|Cs],NewProc,Disj,Link) :-
	extract_disj(C, NewC, Disj, Rest),
	NewProc = [NewC|NewCs],
	gather_disj(Cs,NewCs,Rest,Link).

% given a clause, find in Disj the list of disj((A;B),N,X,C)
% where N is a unique ID, X is a var that takes the place of
% (A;B) in the code, NewC is the clause modified in such a way that
% the disjunctions are replaced by the corresponding vars
% Link is the last (uninstantiated) cdr of the list Disj.
% do the work of pretrans for nots, -> etc...
% put all those guys inside disjunctions
extract_disj(C, (Head:-NewBody), Disj, Link) :-
	C = (Head:-Body), !,
	CtrIn = 0,
	extract_disj(Body, NewBody, Disj, Link, C, CtrIn, CtrOut).
extract_disj(Head, Head, Link, Link).

extract_disj((C1,C2), (NewC1,NewC2), Disj, Link, C, CtrIn, CtrOut) :-
	extract_disj(C1, NewC1, Disj, Link1, C, CtrIn, Ctr),
	extract_disj(C2, NewC2, Link1, Link, C, Ctr, CtrOut).

extract_disj(Goal, X, Disj, Link, C, CtrIn, CtrOut) :-
	is_disj(Goal,NewGoal), !,
	Disj = [disj(NewGoal,CtrIn,X,C)|Link],
	CtrOut is CtrIn + 1.
extract_disj(Goal, Goal, Link, Link, _, CtrIn, CtrIn).

is_disj(((C1 -> C2); C3),((C1, !, C2); C3)) :- !.
is_disj((C1;C2),(C1;C2)).
is_disj(not(C),((C,!,fail);true)).
is_disj(\+(C),((C,!,fail);true)).
is_disj(\=(C1,C2),((C1 = C2,!,fail);true)).

% given a list of disj((A;B),N,X,C), for each, do the following:
% 1) find vars in (A;B)
% 2) find the vars in C
% 3) intersect the two sets of vars into one list
% 4) make a predicate name using N as a part of it ('dummy_disjN')
% 5) put a structure with that name and those vars as args
% 6) binds X to this call
% 7) add new clauses [(dummy:-A)),(dummy:-B))]
treat_disj([], Link, Link).
treat_disj([disj((A;B),N,X,C)|Disjs], DummyClauses, Link) :-
	find_vars((A;B),Vars),
	find_vars(C,CVars),
	intersect_vars(Vars,CVars,Args),
	make_dummy_name(N,Name),
	X =.. [Name|Args],
	make_dummy_clauses((A;B),X,DummyClauses,Rest),
	treat_disj(Disjs, Rest, Link).

make_dummy_clauses((A;B),X,[NewC|Cs],Link) :-
	!,
	copy((X:-A), NewC),
	make_dummy_clauses(B,X,Cs,Link).
make_dummy_clauses(A,X,[NewC|Link],Link) :- copy((X:-A),NewC).

find_vars(X,Y) :- find_vars(X,Y,Link), Link = [].

find_vars(Var,[Var|Link],Link) :- var(Var), !.
find_vars(Cst,Link,Link) :- atomic(Cst), !.
find_vars([T|Ts],Vars,NewLink) :- !,
	find_vars(T,Vars,Link),
	find_vars(Ts,Link,NewLink).
find_vars(Term,Vars,Link) :-
	Term =.. [_|Args],
	find_vars(Args,Vars,Link).

intersect_vars(V1,V2,Out) :-
	sort_vars(V1,Sorted1),
	sort_vars(V2,Sorted2),
	intersect_sorted_vars(Sorted1,Sorted2,Out).

make_dummy_name(N,Name) :-
	name('_dummy_',L1),
	name(N,L2),
	append(L1,L2,L),
	name(Name,L).

append([], L, L).
append([H|L1], L2, [H|Res]) :- append(L1, L2, Res).

% copy_term using a symbol table.
copy(Term1, Term2) :-
        varset(Term1, Set), make_sym(Set, Sym),
        copy2(Term1, Term2, Sym), !.

copy2(V1, V2, Sym) :- var(V1), !, retrieve_sym(V1, Sym, V2).
copy2(X1, X2, Sym) :- nonvar(X1), !,
        functor(X1,Name,Arity),
        functor(X2,Name,Arity),
        copy2(X1, X2, Sym, 1, Arity).

copy2(_X1,_X2,_Sym, N, Arity) :- N>Arity, !.
copy2(X1, X2, Sym, N, Arity) :- N=<Arity, !,
        arg(N, X1, Arg1),
        arg(N, X2, Arg2),
        copy2(Arg1, Arg2, Sym),
        N1 is N+1,
        copy2(X1, X2, Sym, N1, Arity).

retrieve_sym(V, [p(W,X)|_Sym], X) :- V==W, !.
retrieve_sym(V, [_|Sym], X) :- retrieve_sym(V, Sym, X).

make_sym([], []).
make_sym([V|L], [p(V,_)|S]) :- make_sym(L, S).

% *** Gather all variables used in a term: (in a set or a bag)
varset(Term, VarSet) :- varbag(Term, VB),
    sort(VB, VarSet).
varbag(Term, VarBag) :- varbag(Term, VarBag, []).

varbag(Var) --> {var(Var)}, !, [Var].
varbag(Str) --> {nonvar(Str), !, functor(Str,_,Arity)}, varbag(Str, 1, Arity).

varbag(_Str, N, Arity) --> {N>Arity}, !.
varbag(Str, N, Arity) --> {N=<Arity}, !,
        {arg(N, Str, Arg)}, varbag(Arg),
        {N1 is N+1},
        varbag(Str, N1, Arity).

inst_vars(Term) :-
	varset(Term, Vars),
        [A]=`A`,
	inst_vars_list(Vars, A).

inst_vars_list([], _).
inst_vars_list([T|L], N) :-
	name(T, [N]),
	N1 is N+1,
	inst_vars_list(L, N1).

sort_vars(V,Out) :- sort_vars(V,Out,[]).
sort_vars([],Link,Link).
sort_vars([V|Vs],Result,Link) :-
	split_vars(Vs,V,Smaller,Bigger),
	sort_vars(Smaller,Result,[V|SLink]),
	sort_vars(Bigger,SLink,Link).

intersect_sorted_vars([],_,[]) :- !.
intersect_sorted_vars(_,[],[]).
intersect_sorted_vars([X|Xs],[Y|Ys],[X|Rs]) :-
	X == Y, !,
	intersect_sorted_vars(Xs,Ys,Rs).
intersect_sorted_vars([X|Xs],[Y|Ys],Rs) :-
	X @< Y, !,
	intersect_sorted_vars(Xs,[Y|Ys],Rs).
intersect_sorted_vars([X|Xs],[Y|Ys],Rs) :-
	X @> Y, !,
	intersect_sorted_vars([X|Xs],Ys,Rs).


split_vars([],_,[],[]).
split_vars([V|Vs],A,[V|Ss],Bs) :-
	V @< A, !,
	split_vars(Vs,A,Ss,Bs).
split_vars([V|Vs],A,Ss,Bs) :-
	V == A, !,
	split_vars(Vs,A,Ss,Bs).
split_vars([V|Vs],A,Ss,[V|Bs]) :-
	V @> A, !,
	split_vars(Vs,A,Ss,Bs).
