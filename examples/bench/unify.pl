%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (C) 1990 Regents of the University of California.
% All rights reserved.  This program may be freely used and modified for
% non-commercial purposes provided this copyright notice is kept unchanged.
% Written by Peter Van Roy as a part of the Aquarius project.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Benchmark based on part of Aquarius Prolog compiler
% Compiling unification into abstract machine code.

top :- main(X).
%, write(X), nl.

main(Size) :- u(X, [1,Y], [X], Code), size(Code, 0, Size).

% Unify variable X with term T and write the result:
u(X, T, In, Code) :- unify(X, T, In, _, Code, []).
 
% Unify the variable X with the term T, given that
% In = set of variables initialized before the unification.
% Returns the intermediate code for the unification and
% Out = set of variables initialized after the unification.
unify(X, T, In, Out) --> {\+myin(X, In)}, !, uninit(X, T, In, Out).
unify(X, T, In, Out) -->   {myin(X, In)}, !,   init(X, T, In, Out, nonlast, _).

%**** Uninit assumes X has not yet been initialized:
uninit(X, T, In, Out) --> {my_compound(T)}, !, [move(Tag^h, X)],
        {termtag(T, Tag)}, unify_block(nonlast, T, _, In, Mid, _), {incl(X, Mid, Out)}.
uninit(X, T, In, Out) --> {atomic(T)}, !, [move(tatm^T, X)], {incl(X, In, Out)}.
uninit(X, T, In, Out) --> {var(T)}, !, unify_var(X, T, In, Out).

%**** Init assumes X has already been initialized:
init(X, T, In, Out, Last, LLbls) --> {nonvar(T)}, !,
        {termtag(T,Tag)}, [deref(X), switch(Tag,X,[trail(X) | Write],Read,fail)],
        {unify_writemode(X, T, In,      Last, LLbls, Write, [])},
        {unify_readmode(X, T, In, Out,       LLbls, Read, [])}.
init(X, T, In, Out,    _,     _) --> {var(T)}, !, unify_var(X, T, In, Out).

%**** Unifying two variables together:
unify_var(X, Y, In,  In) --> {  myin(X, In),   myin(Y, In)}, !, [unify(X,Y,fail)].
unify_var(X, Y, In, Out) --> {  myin(X, In), \+myin(Y, In)}, !, [move(X,Y)], {incl(Y, In, Out)}.
unify_var(X, Y, In, Out) --> {\+myin(X, In),   myin(Y, In)}, !, [move(Y,X)], {incl(X, In, Out)}.
unify_var(X, Y, In, Out) --> {\+myin(X, In), \+myin(Y, In)}, !,
        [move(tvar^h,X), move(tvar^h,Y), add(1,h), move(Y,[h-1])],
        {incl(X, In, Mid), incl(Y, Mid, Out)}.

%**** Unify_readmode assumes X is a dereferenced nonvariable
% at run-time and T is a nonvariable at compile-time.
unify_readmode(X, T, In, Out, LLbls) --> {structure(T)}, !, [equal([X],tatm^(F/N),fail)],
        {functor(T, F, N)}, unify_args(1, N, T, In, Out,  0, X, LLbls).
unify_readmode(X, T, In, Out, LLbls) --> {cons(T)}, !,
        unify_args(1, 2, T, In, Out, -1, X, LLbls).
unify_readmode(X, T, In,  In,     _) --> {atomic(T)}, !, [equal(X,tatm^T,fail)].

unify_args(I, N, _, In,  In, _, _,         _) --> {I>N}, !.
unify_args(I, N, T, In, Out, D, X, [ _ | LLbls]) --> {I=N}, !,
        unify_arg(I, T, In, Out, D, X, last, LLbls). 
unify_args(I, N, T, In, Out, D, X,     LLbls) --> {I<N}, !,
        unify_arg(I, T, In, Mid, D, X, nonlast, _),
        {I1 is I+1}, unify_args(I1, N, T, Mid, Out, D, X, LLbls).

unify_arg(I, T, In, Out, D, X, Last, LLbls) --> [move([X+ID],Y)],
        {ID is I+D, incl(Y, In, Mid), arg(I, T, A)},
        init(Y, A, Mid, Out, Last, LLbls).



%**** Unify_writemode assumes X is a dereferenced unbound
% variable at run-time and T is a nonvariable at compile-time.
unify_writemode(X, T, In, Last, LLbls) --> {my_compound(T)}, !, [move(Tag^h,[X])],
        {termtag(T, Tag)}, unify_block(Last, T, _, In, _, LLbls).
unify_writemode(X, T,  _,    _,     _) --> {atomic(T)}, !, [move(tatm^T,[X])].


%**** Generate a minimal sequence of moves to create T on the heap:
unify_block(   last, T, Size, In,  In, [Lbl | _ ]) --> !, [add(Size,h), jump(Lbl)],
        {size(T, 0, Size)}.
unify_block(nonlast, T, Size, In, Out, [ _ | LLbls]) --> !, [add(Size,h)],
        {size(T, 0, Size), Offset is -Size}, block(T, Offset, 0, In, Out, LLbls).

block(T, Inf, Outf, In, Out, LLbls) --> {structure(T)}, !, [move(tatm^(F/N), [h+Inf])],
        {functor(T, F, N), Midf is Inf+N+1, S is Inf+1},
        make_slots(1, N, T, S, Offsets, In, Mid),
        block_args(1, N, T, Midf, Outf, Offsets, Mid, Out, LLbls).
block(T, Inf, Outf, In, Out, LLbls) --> {cons(T)}, !,
        {Midf is Inf+2},
        make_slots(1, 2, T, Inf, Offsets, In, Mid),
        block_args(1, 2, T, Midf, Outf, Offsets, Mid, Out, LLbls).
block(T, Inf,  Inf, In,  In,    []) --> {atomic(T)}, !.
block(T, Inf,  Inf, In,  In,    []) --> {var(T)}, !.

block_args(I, N, _, Inf,  Inf,    [], In,  In,          []) --> {I>N}, !.
block_args(I, N, T, Inf, Outf, [Inf], In, Out, [Lbl | LLbls]) --> {I=N}, !, [label(Lbl)],
        {arg(I, T, A)}, block(A, Inf, Outf, In, Out, LLbls).
block_args(I, N, T, Inf, Outf, [Inf | Offsets], In,Out,LLbls) --> {I<N}, !,
        {arg(I, T, A)}, block(A, Inf, Midf, In, Mid, _), {I1 is I+1},
        block_args(I1, N, T, Midf, Outf, Offsets, Mid, Out, LLbls).

make_slots(I, N, _, _,            [], In,  In) --> {I>N}, !.
make_slots(I, N, T, S, [Off | Offsets], In, Out) --> {I=<N}, !,
        {arg(I, T, A)}, init_var(A, S, In),
        {incl(A, In, Mid), make_word(A, Off, Word)}, [move(Word,[h+S])],
        {S1 is S+1, I1 is I+1},
        make_slots(I1, N, T, S1, Offsets, Mid, Out).


% Initialize first-time variables in write mode:
init_var(V, I, In) --> {var(V), \+myin(V, In)}, !, [move(tvar^(h+I),V)].
init_var(V, _, In) --> {var(V),   myin(V, In)}, !.
init_var(V, _,  _) --> {nonvar(V)}, !.

make_word(C, Off, Tag^(h+Off)) :- my_compound(C), !, termtag(C, Tag).
make_word(V,   _, V)           :- var(V), !.
make_word(A,   _, tatm^A)      :- atomic(A), !.

% Calculate the size of T on the heap:
size(T) --> {structure(T)}, !, {functor(T, _, N)}, add(1), add(N), size_args(1, N, T).
size(T) --> {cons(T)}, !, add(2), size_args(1, 2, T).
size(T) --> {atomic(T)}, !.
size(T) --> {var(T)}, !.

size_args(I, N, _) -->  {I>N}, !.
size_args(I, N, T) --> {I=<N}, !, {arg(I, T, A)}, size(A), {I1 is I+1}, size_args(I1, N, T).

%**** Utility routines:

add(I, X, Y) :- Y is X+I.

myin(A, [B|S]) :-
        compare(Order, A, B),
        in_2(Order, A, S).

in_2(=, _, _).
in_2(>, A, S) :- myin(A, S).

incl(A, S1, S) :- incl_2(S1, A, S).

incl_2([], A, [A]).
incl_2([B|S1], A, S) :-
        compare(Order, A, B),
        incl_3(Order, A, B, S1, S).

incl_3(<, A, B, S1, [A,B|S1]).
incl_3(=, _, B, S1, [B|S1]).
incl_3(>, A, B, S1, [B|S]) :- incl_2(S1, A, S).

my_compound(X)  :- nonvar(X), \+atomic(X).
cons(X)      :- nonvar(X), X=[_|_].
structure(X) :- my_compound(X), \+X=[_|_].

termtag(T, tstr) :- structure(T).
termtag(T, tlst) :- cons(T).
termtag(T, tatm) :- atomic(T).
termtag(T, tvar) :- var(T).




