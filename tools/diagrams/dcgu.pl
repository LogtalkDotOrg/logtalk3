:- module(dcgu, [
		writedcg/1

	,	nop/2
	,	out//1
	,	(>>)//2
	,	(\<)//1
	,	(\>)//1
	,	(\#)//2
	,	run_left//3
	,	run_right//3
	,	trans//2

	,	maybe//1
	,	opt//1
	,	if//3, 	if//2
	,	parmap//2, parmap//3, parmap//4, parmap//5, parmap//6
	,	seqmap//2, seqmap//3, seqmap//4, seqmap//5, seqmap//6
	,	seqmap_n//3, seqmap_n//4, seqmap_n//5
	,	seqmap_with_sep//3
	,	seqmap_with_sep//4
	,	seqmap_with_sep//5
	,	seqmap_with_progress//3
	,	seqmap_with_progress//4
	,	seqmap_ints//3
	,	seqmap_args//4
	,	seqmap_args//5
	,	seqmap_args//6
	,	iterate//3
	%,	apply/4, apply/5
	,	seq//1,	seq//2, seq_n//3
	,	smap//2
	,	rep//2, rep_nocopy//2
	,	at//1,	   wr//1,		str//1, fmt//2
	,	brace//1,	paren//1,	sqbr//1
	,	q//1,		qq//1
	,	escape//2, escape_with//3
	,	null//0,	cr//0, sp//0, fs//0
	,	fssp/2,	tb/2,	comma/2, commasp/2
	,	padint/5

	,	do_then_call/5
	,	do_then_call/6
	,	do_then_call/7

	,	any/3, notany/3, arb/2, arbno/3, bal/2
	,	span/3, break/3, len/3
	,	exhaust/3
	,	set/3, get/3, set_with/3
	,	with/4, iso/3
	,	once/3
	,	repeat/2
	,	(//)//2
	,	until//2

	,	findall//3
	,	setof//3

	,  op(900,fy,\<)
	,	op(900,fy,\>)
	,  op(900,xfy,\#)

	,	lift//1
	,	lift//2
	,	lift//3

	,	stats/0
	,	stats/1

	,	select_def_option//2	% like select_option/4 but for DCGs
]).

/** <module> DCG utilities
 
This module contains predicates for working with definite clause
grammars and the related stateful programming style where state 
arguments are automatically threaded through sequences
of calls. Some useful DCG procedures are also included.

When a predicate is declared with type =|foo(...)// is Det|=,
any requirements on the type of the DCG state are hidden, i.e. the
types of the two extra arguments are hidden. In these cases,
the documentation below will sometimes state that the predicate
'runs in the =|S|= DCG'.

---+++ Types used in this module

We use the following to denote types of terms that can
be interpreted as DCG phrases with or without further
arguments.  
	* phrase(S)
	  If P is a term of type =|phrase(S)|=, then P is a valid DCG phrase
	  when the DCG state is of type =|S|=, i.e. =|phrase(P,S1,S2)|= is
	  valid Prolog goal when S1 and S2 are of type =|S|=. N.B. the type
	  =|phrase(S)|= is almost but not quite equivalent to the binary
	  predicate type =|pred(S,S)|=. All such predicates are valid phrases,
	  but phrases involving braces (e.g. {Goal}), commas, semicolons,
	  and if-then constructs (->) are not equivalent to predicates
	  with two extra arguments.
	* phrase(A,S)
	  If P is of type =|phrase(A,S)|= and X has type A, then =|call(P,X)|= 
	  is a valid DCG phrase when the DCG is of type S. This type _|is|_
	  equivalent to =|pred(A,S,S)|= because the only way to call it
	  is with call//1 inside a DCG or call/3 outside it.
	* phrase(A,B,S)
	  If P is of type =|phrase(A,B)|= and =|X|= and =|Y|= are of types
	  =|A|= and =|B|= respectively, then =|call(P,X,Y)|=
	  is a valid DCG phrase. And so on. You get the idea.

The type =|pair(A,B)|= will be used to denote the type of terms
with functor (,)/2 and arguments of types =|A|= and =|B|= respectively:
==
pair(A,B) ---> (A,B).
==
This type is used to support a set of general purpose predicates
for combining commands in two distinct DCGs into a single DCG 
over a product space of states.
*/

:- module_transparent 	seq/3,	seq/4,	smap/4.

:- meta_predicate 
		writedcg(2)
	,	if(0,//,//,?,?) 
	,	if(0,//,?,?) 
	,	maybe(//,?,?)
	,	opt(//,?,?)
	,	once(//,?,?)
	,	repeat(?,?)
	,	>>(//,//,?,?)
	,	//(//,?,?,?)
	,	\<(//,?,?)
	,	\>(//,?,?)
	,	\#(?,//,?,?)
	,	brace(//,?,?)
	,	paren(//,?,?)
	,	sqbr(//,?,?)
	,	qq(//,?,?)
	,	q(//,?,?)
	,	arbno(//,?,?)
	,	rep(?,//,?,?)
	,	rep_nocopy(+,//,?,?)
	,	exhaust(//,?,?)
	,	with(?,//,?,?)
	,	iso(//,?,?)
	,	set_with(1,?,?)
	,	run_left(//,?,?,?,?)
	,	run_right(//,?,?,?,?)
	,	iterate(4,?,?,?,?)
	,	parmap(3,?,?,?)
	,	parmap(4,?,?,?,?)
	,	parmap(5,?,?,?,?,?)
	,	parmap(6,?,?,?,?,?,?)
	,	parmap(7,?,?,?,?,?,?,?)
	,	seqmap(3,?,?,?)
	,	seqmap(4,?,?,?,?)
	,	seqmap(5,?,?,?,?,?)
	,	seqmap(6,?,?,?,?,?,?)
	,	seqmap(7,?,?,?,?,?,?,?)
	,	seqmap_n(+,3,?,?,?)
	,	seqmap_n(+,4,?,?,?,?)
	,	seqmap_n(+,5,?,?,?,?,?)
	,	seqmap_ints(3,+,+,?,?)
	,	seqmap_with_sep(//,3,?,?,?)
	,	seqmap_with_sep(//,4,?,?,?,?)
	,	seqmap_with_sep(//,5,?,?,?,?,?)
	,	seqmap_args(3,+,+,?,?,?)
	,	seqmap_args(4,+,+,?,?,?,?)
	,	seqmap_args(5,+,+,?,?,?,?,?)
	,	do_then_call(//,3,?,?,?)
	,	do_then_call(//,4,?,?,?,?)
	,	do_then_call(//,5,?,?,?,?,?)
	,	until(0,//,?,?)
	.

:- op(900,fy,\<).
:- op(900,fy,\>).
:- op(900,xfy,\#).
:- op(550,xfx,..).


%%%
%%% The first lot of stuff is completely general for any stateful system.
%%%


%% trans( ?Old:S, ?New:S, ?S1:int, ?S2:S) is det.
%
%  Unifies Old and New with the states S1 and S2 respectively.
trans(X,Y,X,Y).

% these will be useful for seq (they define a sort of generalised
% lazy mapping over sequences of DCG terms)
empty([]).
empty(_:[]).
empty(map(_,L)) :- empty(L).
empty(_:map(_,L)) :- empty(L).
empty(M..N) :- N<M.

singleton([H|T],H) :- empty(T).
singleton(M:[H|T],M:H) :- empty(T).
singleton(map(F,L),call(F,H)) :- singleton(L,H).
singleton(M:map(F,L),call(M:F,H)) :- singleton(L,H).
singleton(M..M,M).

properlist([H|T],H,T) :- \+empty(T).
properlist(M:[H|T],M:H,M:T) :- \+empty(T).
properlist(map(F,L),call(F,H),map(F,T)) :- properlist(L,H,T).
properlist(M:map(F,L),call(M:F,H),M:map(F,T)) :- properlist(L,H,T).
properlist(M..N,M,M1..N) :- N>M, succ(M,M1).

%% nop// is det.
%
%  Do nothing. (More neutral than []).
nop(X,X).

%% set(S:A, S1:_, S2:A) is det.
%  Set state to S. Implemented by goal expansion.
set(S,_,S).

%% get(S:A, S1:A, S2:A) is det.
%  Get state to S. Implemented by goal expansion.
get(S,S,S).

%% with(S:A, P:phrase(A), S1:B, S2:B) is nondet.
%
%  Run phrase P starting from state S and discarding
%  the final state, meanwhile preserving the state
%  of the current system, i.e. guarantees S1=S2.
with(S,G) --> {phrase(G,S,_)}.

%% iso(P:phrase(A), S1:A, S2:A) is nondet.
%
%  Run phrase P starting with current state but discarding
%  its final state and preserving the current state, so
%  that S1=S2.
iso(G)    --> get(S), {phrase(G,S,_)}.

%% set_with(+G:pred(A), S1:_, S2:A) is det.
%
%  Set current state using a given callable goal G, which should accept one argument.
%  should be of type pred( -S:A), ie it should set S to the new desired
%  state, which is installed in the DCG state.
set_with(G,_,S) :- call(G,S).

%% \<(P:phrase(A), ?S1:pair(A,B), ?S2:pair(A,B)) is nondet.
%
%  Apply phrase P to left part of a paired state.
%  Implemented by goal expansion so incurs only very small
%  speed penalty.
\<(P,(A1,B),(A2,B)) :- phrase(P,A1,A2).

%% \>(P:phrase(B), ?S1:pair(A,B), ?S2:pair(A,B)) is nondet.
%
%  Apply phrase P which must be of type pred(B,B) to right
%  part of a paired state.
%  Implemented by goal expansion so incurs only very small
%  speed penalty.
\>(P,(A,B1),(A,B2)) :- phrase(P,B1,B2).

%% run_left(P:phrase(pair(A,B)), ?A1:A, ?A2:A, ?B1:B, ?B2:B) is multi.
%
%  Applies DCG phrase P to state formed by pairing A1 and A2 with
%  current DCG states B1 and B2. Phrase can use (\<) to access the
%  A state and (\>) to access the underlying B state.
run_left(P,S1,S2,T1,T2) :- phrase(P,(S1,T1),(S2,T2)).

%% run_right(P:phrase(pair(A,B)), ?B1:B, ?B2:B, ?A1:A, ?A2:A) is multi.
%
%  Applies DCG phrase P to state formed by pairing A1 and A2 with
%  current DCG states B1 and B2. Phrase can use (\<) to access the
%  A state and (\>) to access the underlying B state.
run_right(P,S1,S2,T1,T2) :- phrase(P,(T1,S1),(T2,S2)).

%% \#(N:natural, P:phrase(A), ?S1, ?S2) is nondet.
%
%  Apply phrase P to the Nth argument of state which must
%  be a compound term (with arbitrary functor), with the 
%  Nth argument of type A.
\#(N, P, S1, S2) :- with_nth_arg(N,P,S1,S2).


system:goal_expansion( run_left(P,S1,S2,T1,T2), phrase(P,(S1,T1),(S2,T2))).
system:goal_expansion( run_right(P,S1,S2,T1,T2), phrase(P,(T1,S1),(T2,S2))).
system:goal_expansion( \<(P,S1,S2), (S1=(L1,R),S2=(L2,R),phrase(P,L1,L2)) ).
system:goal_expansion( \>(P,S1,S2), (S1=(L,R1),S2=(L,R2),phrase(P,R1,R2)) ).
system:goal_expansion( nop(S1,S2), (S1=S2) ).
system:goal_expansion( out(X,S1,S2), (S1=[X|S2]) ).
system:goal_expansion( get(S,S1,S2), (S=S1,S1=S2) ). 
system:goal_expansion( set(S,_,S2), (S=S2) ). 
system:goal_expansion( A >> B, (A,B) ). 
system:goal_expansion( set_with(C,_,S2), Call) :- mk_call(C,[S2],Call).
system:goal_expansion( trans(A1,A2,S1,S2), (S1=A1,S2=A2) ). 
system:goal_expansion( //(P1,P2,S1,S2), (G1,G2)) :- 
	nonvar(P1), P1=..[F1|A1], append(A1,[S1,S2],B1), G1=..[F1|B1],
	nonvar(P2), P2=..[F2|A2], append(A2,[S1,S2],B2), G2=..[F2|B2].

mk_call(C,XX,Call) :- var(C), !, mk_call(call(C),XX,Call).
mk_call(M:C,XX,M:Call) :- !, mk_call(C,XX,Call).
mk_call(C,XX,Call) :- C =.. CL, append(CL,XX,CL2), Call =.. CL2.


%% pushl(S:A,S1:B,S2:pair(A,B)) is det.
%  Create a paired state by putting S on the left and the
%  old state on the right.
pushl(S,S0,(S,S0)).

%% pushr(S:A,S1:B,S2:pair(B,A)) is det.
%  Create a paired state by putting S on the right and the
%  old state on the left.
pushr(S,S0,(S0,S)).

%% popl(S:A,S1:pair(A,B),S2:B) is det.
%  Unpair state by removing left state and unifying it with S.
popl(S,(S,S0),S0).

%% popr(S:A,S1:(B,A),S2:B) is det.
%  Unpair state by removing right state and unifying it with S.
popr(S,(S0,S),S0).

%% >>(G1:phrase(S), G2:phrase(S))// is nondet.
% Sequential conjuction of phrases G1 and G2, equivalent to (G1,G2),
% but sometimes more convenient in terms of operator priorities.
% Implemented by goal expansion.
A >> B --> A, B.

%% once(G:phrase(_))// is semidet.
%  Call DCG phrase G succeeding at most once.
once(G,A,B) :- once(phrase(G,A,B)).

%% repeat// is nondet.
%  Create an infinite number of choice points.
repeat(A,A) :- repeat.

%% maybe(P:phrase(_))// is det.
%  Try P, if it fails, then do nothing. If it succeeds,
%  cut choicepoints and continue.
maybe(P)  --> P -> nop; nop.

%% opt(P:phrase(_))// is nondet.
%  P or nothing. Like maybe but does not cut if P succeeds.
opt(P)  --> P; nop.

%% if(G:pred,P,Q)// is det.
%% if(G:pred,P)// is det.
%
%  If Prolog goal =|call(G)|= succeeds, do P, otherwise, do Q.
%  if(G,P) is equivalent to if(G,P,nop), i.e. does nothing
%  if P fails.
if(A,B,C) --> {call(A)} -> B; C. % used to have nonvar(A)
if(A,B)   --> {call(A)} -> B; nop.


%% exhaust( P:phrase(_))// is det.
%
%  Run phrase sequentially as many times as possible until it fails.
%  Any choice points left by G are cut.
exhaust(G) --> G -> exhaust(G); nop.


%% until( +Q:pred, +P:phrase(_))// is det.
%
%	Repeatedly call phrase P and test ordinary Prolog goal
%	Q until Q fails. P and Q are copied together before each
%	iteration, so variables can be shared between them, but
%	are not shared between iterations.
until( Pred, Op) -->
	{copy_term(Pred/Op,Pred1/Op1)},
	call(Op1),
	(	{call(Pred1)} 
	-> {Pred/Op=Pred1/Op1}
	;	until(Pred, Op)
	).

%% iterate( +P:phrase(A,A,S), +X:A, -Y:A)// is nondet.
%
%  Sequentially call P zero or more times, passing in X on
%  the first call and threading the result through subsequent calls,
%  (as well as threading the DCG state in the normal way)
%  ending in Y.

iterate(_,A,A) --> [].
iterate(F,A1,A3) --> call(F,A1,A2), iterate(F,A2,A3).


%% rep( +N:natural, +P:phrase(_))// is nondet.
%% rep( -N:natural, +P:phrase(_))// is nondet.
%
%  Equivalent to N sequential copies of phrase P.
%  Free variables in P are *not* shared between copies.
%  If N is unbound on entry, rep//2 is _cautious_: it tries
%  gradually increasing N from 0 on backtracking.

rep(N,G,S1,S2) :- 
	(	var(N) 
	->	rep_var(N,G,S1,S2)
	;	rep_nonvar(N,G,S1,S2)
	).

rep_var(0,_,S,S).
rep_var(N,G,S1,S3) :- 
	copy_term(G,G1), phrase(G1,S1,S2), 
	rep_var(M,G,S2,S3), succ(M,N).

rep_nonvar(0,_,S,S) :- !.
rep_nonvar(N,G,S1,S3) :- 
	copy_term(G,G1), phrase(G1,S1,S2), 
	succ(M,N), rep_nonvar(M,G,S2,S3).


%% rep_nocopy( +N:natural, +P:phrase(_))// is nondet.
%
%	Like rep//2 but does not copy P before calling, so
%	any variables in P are shared between all calls.
%	Also, N cannot be a variable in this implementation.
rep_nocopy(0,_) --> !.
rep_nocopy(N,P) --> call(P), {succ(M,N)}, rep_nocopy(M,P).


%% seq( +L:plist, +Sep)// is nondet.
%% seq( +L:plist)// is nondet.
% Sequence list of phrases with separator. L can be a sort of _generalised_
% list of phrases, which can be:
% ==
% plist ---> list(A)               % ordinary list
%          ; map(phrase(B),plist)  % map phrase head P over list
%          .
% ==
% Sep is inserted strictly betweened elements of L. seq(L) is equivalent
% to seq(L,nop).

seq(L,_)     --> {dcgu:empty(L)}.
seq(L,_)     --> {dcgu:singleton(L,H)}, H.
seq(L,S)     --> {dcgu:properlist(L,H,T)}, H, S, seq(T,S).
seq(L)       --> seq(L,nop).         % if no separator specified, use nop.


%% seq_n( N:natural, +L:plist, +Sep)// is nondet.
% Sequence list of phrases with separator and counting.
%
% @see seq//2.

seq_n(0,L,_)   --> {dcgu:empty(L)}.
seq_n(1,L,_)   --> {dcgu:singleton(L,H)}, H.
seq_n(N,L,S)   --> {dcgu:properlist(L,H,T)}, H, S, seq_n(M,T,S), {succ(M,N)}.

%% smap(+F,+L:list)// is nondet.
%  Equivalent to seq(map(F,L),nop).
smap(F,L) --> seq(map(F,L),nop).



%% seqmap( +P:phrase(A,S), X:list(A))// is nondet.
%% seqmap( +P:phrase(A,B,S), X:list(A), Y:list(B))// is nondet.
%% seqmap( +P:phrase(A,B,C,S), X:list(A), Y:list(B), Z:list(C))// is nondet.
%% seqmap( +P:phrase(A,B,C,D,S), X:list(A), Y:list(B), Z:list(C), W:list(D))// is nondet.
%% seqmap( +P:phrase(A,B,C,D,E,S), X:list(A), Y:list(B), Z:list(C), W:list(D), V:list(E))// is nondet.
%
%  seqmap//N is like maplist/N except that P is an incomplete _phrase_
%  rather an ordinary goal, which is applied to the elements of the supplied
%  lists _|in order|_, while threading the DCG state correctly through all
%  the calls.
%
%  seqmap//N is very powerful - it is like =foldl= and =mapaccum= in functional 
%  languages, but with the added flexibility of bidirectional Prolog variables.
%  
%  @see maplist/2.

seqmap(_,[])             --> [].
seqmap(P,[A|AX])         --> call(P,A), seqmap(P,AX).
seqmap(_,[],[])          --> [].
seqmap(P,[A|AX],[B|BX])  --> call(P,A,B), seqmap(P,AX,BX).
seqmap(_,[],[],[])          --> [].
seqmap(P,[A|AX],[B|BX],[C|CX])  --> call(P,A,B,C), seqmap(P,AX,BX,CX).
seqmap(_,[],[],[],[])          --> [].
seqmap(P,[A|AX],[B|BX],[C|CX],[D|DX])  --> call(P,A,B,C,D), seqmap(P,AX,BX,CX,DX).
seqmap(_,[],[],[],[],[])          --> [].
seqmap(P,[A|AX],[B|BX],[C|CX],[D|DX],[E|EX])  --> call(P,A,B,C,D,E), seqmap(P,AX,BX,CX,DX,EX).

true(_,_).
parmap(_,[])             --> true.
parmap(P,[A|AX])         --> call(P,A) // parmap(P,AX).
parmap(_,[],[])          --> true.
parmap(P,[A|AX],[B|BX])  --> call(P,A,B) // parmap(P,AX,BX).
parmap(_,[],[],[])          --> true.
parmap(P,[A|AX],[B|BX],[C|CX])  --> call(P,A,B,C) // parmap(P,AX,BX,CX).
parmap(_,[],[],[],[])          --> true.
parmap(P,[A|AX],[B|BX],[C|CX],[D|DX])  --> call(P,A,B,C,D) // parmap(P,AX,BX,CX,DX).
parmap(_,[],[],[],[],[])          --> true.
parmap(P,[A|AX],[B|BX],[C|CX],[D|DX],[E|EX])  --> call(P,A,B,C,D,E) // parmap(P,AX,BX,CX,DX,EX).

%% seqmap_n( +N:natural, +P:phrase(A), X:list(A))// is nondet.
%% seqmap_n( +N:natural, +P:phrase(A,B), X:list(A), Y:list(B))// is nondet.
%% seqmap_n( +N:natural, +P:phrase(A,B,C), X:list(A), Y:list(B), Z:list(C))// is nondet.
%
%  seqmap_n//.. is like seqmap/N except that the lists of arguments are of lenght N.

seqmap_n(0,_,[])             --> [].
seqmap_n(N,P,[A|AX])         --> {succ(M,N)}, call(P,A), seqmap_n(M,P,AX).
seqmap_n(0,_,[],[])          --> [].
seqmap_n(N,P,[A|AX],[B|BX])  --> {succ(M,N)}, call(P,A,B), seqmap_n(M,P,AX,BX).
seqmap_n(0,_,[],[],[])          --> [].
seqmap_n(N,P,[A|AX],[B|BX],[C|CX])  --> {succ(M,N)}, call(P,A,B,C), seqmap_n(M,P,AX,BX,CX).


/*
 * Goal expansions 
 */

cons(A,B,[A|B]).

expand_seqmap_with_prefix(Sep0, Callable0, SeqmapArgs, Goal) :-
	(   Callable0 = M:Callable
	->  NextGoal = M:NextCall
	;   Callable = Callable0,
	    NextGoal = NextCall
	),

	append(Lists, [St1,St2], SeqmapArgs),

	Callable =.. [Pred|Args],
	length(Args, Argc),
	length(Argv, Argc),
	length(Lists, N),
	length(Vars, N),
	MapArity is N + 4,
	format(atom(AuxName), '__aux_seqmap/~d_~w_~w+~d', [MapArity, Sep0, Pred, Argc]),
	build_term(AuxName, Lists, Args, St1, St2, Goal),

	AuxArity is N+Argc+2,
	prolog_load_context(module, Module),
	(   current_predicate(Module:AuxName/AuxArity)
	->  true
	;   rep(N,[[]],BaseLists,[]),
	    length(Anon, Argc),
	    build_term(AuxName, BaseLists, Anon, S0, S0, BaseClause),

       length(Vars,N), 
		 maplist(cons, Vars, Tails, NextArgs),
       (  Sep0=_:Sep -> true; Sep=Sep0 ),
		 (  is_list(Sep) -> append(Sep,S2,S1), NextThing=NextGoal
		 ;  build_term(phrase, [Sep0], [], S1, S2, NextSep),
			 NextThing = (NextSep,NextGoal)
		 ),
	    build_term(Pred,    Argv,     Vars, S2, S3, NextCall1),
	    build_term(AuxName, Tails,    Argv, S3, S4, NextIterate),
	    build_term(AuxName, NextArgs, Argv, S1, S4, NextHead), 

		 (  goal_expansion(NextCall1,NextCall) -> true
		 ;  NextCall1=NextCall),

	    NextClause = (NextHead :- NextThing, NextIterate),

	    (	predicate_property(Module:NextGoal, transparent)
	    ->	compile_aux_clauses([ (:- module_transparent(Module:AuxName/AuxArity)),
				      BaseClause,
				      NextClause
				    ])
	    ;   compile_aux_clauses([BaseClause, NextClause])
	    )
	).

expand_call_with_prefix(Sep0, Callable0, InArgs, (SepGoal,CallGoal)) :-
	append(CallArgs, [S1,S3], InArgs),

	(  Sep0=_:Sep -> true; Sep=Sep0 ),
	(  is_list(Sep) -> append(Sep,S2,SS), SepGoal=(S1=SS)
	;  build_term(phrase, [Sep0], [], S1, S2, SepGoal)
	),

	(	var(Callable0)
	->	build_term(call,[Callable0], CallArgs, S2, S3, CallGoal1)
	;	(	Callable0 = M:Callable
		->  CallGoal1 = M:NextCall
		;   Callable = Callable0,
			 CallGoal1 = NextCall
		),
		Callable =.. [Pred|Args],
		build_term(Pred, Args, CallArgs, S2, S3, NextCall)
	),
	(	goal_expansion(CallGoal1,CallGoal) -> true
	;	CallGoal1=CallGoal
	).

seqmap_with_sep_first_call(P,[A1|AX],AX) --> call(P,A1).
seqmap_with_sep_first_call(P,[A1|AX],[B1|BX],AX,BX) --> call(P,A1,B1).
seqmap_with_sep_first_call(P,[A1|AX],[B1|BX],[C1|CX],AX,BX,CX) --> call(P,A1,B1,C1).

expand_seqmap_with_sep(Sep, Pred, SeqmapArgs, (dcgu:FirstCall,dcgu:SeqmapCall)) :-
	prolog_load_context(module,Context),
	(Sep=SMod:Sep1 -> true; SMod=Context, Sep1=Sep),
	(Pred=CMod:Pred1 -> true; CMod=Context, Pred1=Pred),
	append(Lists, [St1,St3], SeqmapArgs),
	length(Lists, N),
	length(Tails, N),
	build_term(seqmap_with_sep_first_call, [CMod:Pred1|Lists], Tails, St1, St2, FirstCall),
	build_term(seqmap_with_prefix, [SMod:Sep1,CMod:Pred1], Tails, St2, St3, SeqmapCall).

build_term(H,L1,L2,S1,S2,Term) :-
	append(L2,[S1,S2],L23), 
	append(L1,L23,L123),
	Term =.. [H | L123].


expand_dcgu(Term, Goal) :-
	functor(Term, seqmap, N), N >= 4,
	Term =.. [seqmap, Callable | Args],
	callable(Callable), !,
	expand_seqmap_with_prefix([],Callable, Args, Goal).

expand_dcgu(Term, Goal) :-
	functor(Term, seqmap_with_sep, N), N >= 5, 
	Term =.. [seqmap_with_sep, Sep, Callable | Args],
	nonvar(Sep), callable(Callable), !,
	expand_seqmap_with_sep(Sep, Callable, Args, Goal).

expand_dcgu(Term, Goal) :-
	functor(Term, seqmap_with_prefix, N), N >= 5, 
	Term =.. [seqmap_with_prefix, Sep, Callable | Args],
	callable(Callable), nonvar(Sep), !,
	expand_seqmap_with_prefix(Sep, Callable, Args, Goal).

expand_dcgu(Term, Goal) :-
	functor(Term, do_then_call, N), N >= 2, 
	Term =.. [do_then_call, Prefix, Callable | Args],
	nonvar(Prefix), !,
	expand_call_with_prefix(Prefix, Callable, Args, Goal).

system:goal_expansion(GoalIn, GoalOut) :-
	\+current_prolog_flag(xref, true),
	expand_dcgu(GoalIn, GoalOut).
%	prolog_load_context(module,Mod),
%	writeln(expanded(Mod:GoalIn)).


%% seqmap_with_sep(+S:phrase, +P:phrase(A), X:list(A))// is nondet.
%% seqmap_with_sep(+S:phrase, +P:phrase(A,B), X:list(A), Y:list(B))// is nondet.
%% seqmap_with_sep(+S:phrase, +P:phrase(A,B,C), X:list(A), Y:list(B), Z:list(C))// is nondet.
%
%  As seqmap//2.. but inserting the separator phrase S between each call to P.
%  NB: *Fails* for empty lists.
%
%  @see seqmap//2
%seqmap_with_sep(S,P,[A|AX]) --> call(P,A), seqmap_with_prefix(S,P,AX).
%seqmap_with_sep(S,P,[A|AX],[B|BX]) --> call(P,A,B), seqmap_with_prefix(S,P,AX,BX).
%seqmap_with_sep(S,P,[A|AX],[B|BX],[C|CX]) --> call(P,A,B,C), seqmap_with_prefix(S,P,AX,BX,CX).
seqmap_with_sep(S,P,[A|AX]) --> call(P,A), seqmap(do_then_call(S,P),AX).
seqmap_with_sep(S,P,[A|AX],[B|BX]) --> call(P,A,B), seqmap(do_then_call(S,P),AX,BX).
seqmap_with_sep(S,P,[A|AX],[B|BX],[C|CX]) --> call(P,A,B,C), seqmap(do_then_call(S,P),AX,BX,CX).

%seqmap_with_prefix(_,_,[])             --> [].
%seqmap_with_prefix(S,P,[A|AX])         --> S, call(P,A), seqmap_with_prefix(S,P,AX).
%seqmap_with_prefix(_,_,[],[])          --> [].
%seqmap_with_prefix(S,P,[A|AX],[B|BX])  --> S, call(P,A,B), seqmap_with_prefix(S,P,AX,BX).
%seqmap_with_prefix(_,_,[],[],[])       --> [].
%seqmap_with_prefix(S,P,[A|AX],[B|BX],[C|CX])  --> S, call(P,A,B,C), seqmap_with_prefix(S,P,AX,BX,CX).


% do_then_call( +S:phrase, +P:phrase(A), X:A)// is nondet.
% do_then_call( +S:phrase, +P:phrase(A,B), X:A, Y:B)// is nondet.
% do_then_call( +S:phrase, +P:phrase(A,B,C), X:A, Y:B, Z:C)// is nondet.
%
%  Call phrase S, then call phrase P with arguments A, B, C etc.
do_then_call(S,P,A) --> S, call(P,A).
do_then_call(S,P,A,B) --> S, call(P,A,B).
do_then_call(S,P,A,B,C) --> S, call(P,A,B,C).


%% seqmap_ints( +P:phrase(integer), +I:integer, +J:integer)// is nondet.
%
%  Equivalent to seqmap(P) applied to the list of integers from I to J inclusive.
%
%  @see seqmap//2.
seqmap_ints(P,L,N) --> 
	(	{L>N} -> []
	;	{M is L+1}, call(P,L), seqmap_ints(P,M,N)
	).


%% seqmap_args( +P:phrase(integer), +I:integer, +J:integer, X:term)// is nondet.
%% seqmap_args( +P:phrase(integer), +I:integer, +J:integer, X:term, Y:term)// is nondet.
%% seqmap_args( +P:phrase(integer), +I:integer, +J:integer, X:term, Y:term, Z:term)// is nondet.
%
%  Like seqmap//N, but applied to the arguments of term X, Y and Z, from the I th to the
%  J th inclusive.
%
%  @see seqmap//2.

seqmap_args(P,L,N,A) --> 
	(	{L>N} -> []
	;	{succ(L,M), arg(L,A,AA)},
		call(P,AA), seqmap_args(P,M,N,A)
	).

seqmap_args(P,L,N,A,B) --> 
	(	{L>N} -> []
	;	{succ(L,M), arg(L,A,AA), arg(L,B,BB)},
		call(P,AA,BB), seqmap_args(P,M,N,A,B)
	).

seqmap_args(P,L,N,A,B,C) --> 
	(	{L>N} -> []
	;	{succ(L,M), arg(L,A,AA), arg(L,B,BB), arg(L,C,CC)},
		call(P,AA,BB,CC), seqmap_args(P,M,N,A,B,C)
	).



%%% ------------------------------------------------------------------
%%% These are for sequence building DCGs.
%%% ------------------------------------------------------------------



%% out(?X)// is det.
%
%  Equivalent to [X]. prepends X to the difference list represented by
%  the DCG state variables.
out(L,[L|L0],L0).


% SNOBOL4ish rules
%
%	Others:
%		maxarb
%		pos rpos
%		tab rtab
%		rem


%% any(+L:list(_))// is nondet.
%  Matches any element of L.
any(L)    --> [X], {member(X,L)}.

%% notany(+L:list(_))// is nondet.
%  Matches anything not in L.
notany(L) --> [X], {maplist(dif(X),L)}.

%% arb// is nondet.
%  Matches an arbitrary sequence. Proceeds cautiously.
arb       --> []; [_], arb.

%% arbno(+P:phrase)// is nondet.
%  Matches an arbitrary number of P. Proceeds cautiously.
%  Any variables in P are shared across calls.
arbno(P)  --> []; P, arbno(P).

%% bal// is nondet.
%  Matches any expression with balanced parentheses. 
bal       --> balexp, arbno(balexp).
balexp    --> "(", bal, ")".
balexp    --> notany("()").

%% span(+L:list(_))// is nondet.
%  Matches the longest possible sequence of symbols from L.
span(L,A,[]) :- any(L,A,[]).
span(L)      --> any(L), span(L).
span(L), [N] --> any(L), [N], { maplist( dif( N), L) }.

%% break(+L:list(_))// is nondet.
%  Matches the longest possible sequence of symbols not in L.
break(L,A,[]) :- notany(L,A,[]).
break(L)      --> notany(L), break(L).
break(L), [N] --> notany(L), [N], { member(N,L) }.

%% len(N:natural)// is nondet.
%  Matches any N symbols.
len(0)    --> []. 
len(N)    --> [_], ({var(N)} -> len(M), {succ(M,N)}; {succ(M,N)}, len(M)).


%% //(+P:phrase(A), ?C:list(A), ?S1:list(A), ?S2:list(A)) is nondet.
%% //(+P:phrase(A), +C:phrase(A), ?S1:list(A), ?S2:list(A)) is nondet.
%
%  Sequence capture operator - captures the matching sequence C of any
%  phrase P, eg.
%  ==
%  ?- phrase(paren(arb)//C,"(hello)world",_)
%  C = "(hello)".
%  true
%  ==
%  If nonvar(C) and C is a phrase, it is called after calling P.

//(H,C,L,T) :- 
	(	var(C) 
	-> phrase(H,L,T), append(C,T,L)
	;	phrase(H,L,T), phrase(C,L,T)
	).

%%% ------------------------------------------------------------------
%%% These are for character sequences DCGs.

%% writedcg(+P:phrase) is nondet.
%
%  Run the phrase P, which must be a standard list-of-codes DCG,
%  and print the output.
writedcg(Phrase) :-
	phrase(Phrase,Codes),
	format('~s',[Codes]).
		
%% null// is det.
%  Empty string.
null  --> "".

%% cr// is det.
%  Carriage return "\n".
cr    --> "\n".

%% sp// is det.
%  Space " ".
sp    --> " ".

%% fs// is det.
%  Full stop (period) ".".
fs    --> ".".

%% fssp// is det.
%  Full stop (period) followed by space.
fssp  --> ". ".

%% tb// is det.
%  Tab "\t".
tb    --> "\t".

%% comma// is det.
%  Comma ",".
comma   --> ",".

%% commasp// is det.
%  Comma and space ", ".
commasp --> ", ".

%% at(X:atom)// is det.
%  Generate code list for textual representation of atom X.
at(A,C,T) :- atomic(A), with_output_to(codes(C,T),write(A)).

%% wr(X:term)// is det.
%  Generate the list of codes for term X, as produced by write/1.
wr(X,C,T) :- ground(X), with_output_to(codes(C,T),write(X)).

%% wq(X:term)// is det.
%  Generate the list of codes for term X, as produced by writeq/1.
wq(X,C,T) :- ground(X), with_output_to(codes(C,T),writeq(X)).

%% str(X:term)// is det.
%  Generate the list of codes for string X, as produced by writeq/1.
str(X,C,T):- string(X), with_output_to(codes(C,T),write(X)).

%% fmt(+F:atom,+Args:list)// is det
%  Generate list of codes using format/3.
fmt(F,A,C,T) :- format(codes(C,T),F,A).

%% brace(P:phrase)// is nondet.
%  Generate "{" before and "}" after the phrase P.
brace(A) --> "{", A, "}".

%% paren(P:phrase)// is nondet.
%  Generate "(" before and ")" after the phrase P.
paren(A) --> "(", A, ")".

%% sqbr(P:phrase)// is nondet.
%  Generate "[" before and "]" after the phrase P.
sqbr(A)  --> "[", A, "]".

%% q(P:phrase(list(code)))// is nondet.
%  Generate list of codes from phrase P, surrounds it with single quotes,
%  and escapes (by doubling up) any internal quotes so that the
%  generated string is a valid quoted string. Must be list of codes DCG.
q(X,[39|C],T)  :- T1=[39|T], escape_with(39,39,X,C,T1). % 39 is '

%% qq(P:phrase(list(code)))// is nondet.
%  Generate list of codes from phrase P, surrounds it with double quotes,
%  and escapes (by doubling up) any double quotes so that the
%  generated string is a valid double quoted string.
qq(X,[34|C],T) :- T1=[34|T], escape_with(34,34,X,C,T1). % 34 is "

% escape difference list of codes with given escape character
escape_codes(_,_,A,A,A).
escape_codes(E,Q,[Q|X],[E,Q|Y],T) :-escape_codes(E,Q,X,Y,T).
escape_codes(E,Q,[A|X],[A|Y],T)   :- Q\=A, escape_codes(E,Q,X,Y,T).

%% escape_with(E:C, Q:C, P:phrase(list(C)))// is nondet.
%
%  Runs phrase P to generate a list of elements of type C and
%  then escapes any occurrences of Q by prefixing them with E, e.g.,
%  =|escape_with(92,39,"some 'text' here")|= escapes the single quotes
%  with backslashes, yielding =|"some \'text\' here"|=.
escape_with(E,Q,Phrase,L1,L2) :-
	phrase(Phrase,L0,L2),
	escape_codes(E,Q,L0,L1,L2).

%% escape(Q:C, P:phrase(list(C)))// is nondet.
%
%  Runs phrase P to generate a list of elements of type C and
%  then escapes any occurrences of Q by doubling them up, e.g.,
%  =|escape(39,"some 'text' here")|= doubles up the single quotes
%  yielding =|"some ''text'' here"|=.
escape(Q,A) --> escape_with(Q,Q,A).

%% padint( +N:integer, +Range, +X:integer)// is nondet.
%
%  Write integer X padded with zeros ("0") to width N.
padint(N,L..H,X,C,T) :- 
	between(L,H,X), 
	format(atom(Format),'~~`0t~~d~~~d|',[N]),
	format(codes(C,T),Format,[X]).

difflength(A-B,N) :- unify_with_occurs_check(A,B) -> N=0; A=[_|T], difflength(T-B,M), succ(M,N).

% tail recursive version
difflength_x(A-B,M)       :- difflength_x(A-B,0,M).
difflength_x(A-B,M,M)     :- unify_with_occurs_check(A,B).
difflength_x([_|T]-A,M,N) :- succ(M,L), difflength_x(T-A,L,N).


%term_codes(T,C) :- with_output_to(codes(C),write(T)).




% try these?
%setof(X,Q,XS,S1,S2) :- setof(X,phrase(Q,S1,S2),XS).
%findall(X,Q,XS,S1,S2) :- findall(X,phrase(Q,S1,S2),XS).

with_nth_arg(K,P,T1,T2) :- 
	functor(T1,F,N),
	functor(T2,F,N),
	with_nth_arg(N,K,P,T1,T2).

with_nth_arg(K,K,P,T1,T2) :- 
	arg(K,T1,C1), phrase(P,C1,C2),
	arg(K,T2,C2), succ(N,K),
	copy_args(N,T1,T2).

with_nth_arg(N,K,P,T1,T2) :- 
	arg(N,T1,C), 
	arg(N,T2,C),
	succ(M,N), 
	with_nth_arg(M,K,P,T1,T2).

copy_args(0,_,_) :- !.
copy_args(N,T1,T2) :-
	succ(M,N), arg(N,T1,X), arg(N,T2,X), 
	copy_args(M,T1,T2).


%% setof( Template:X, Phrase:phrase(S), Results:list(X), S1:S, S2:S) is nondet.
setof(X,Q,XS,S1,S2) :- setof(X,phrase(Q,S1,S2),XS).

%% findall( Template:X, Phrase:phrase(S), Results:list(X), S1:S, S2:S) is nondet.
findall(X,Q,XS,S1,S2) :- findall(X,phrase(Q,S1,S2),XS).


:- meta_predicate lift(0,?,?), lift(1,?,?), lift(2,?,?).

lift(P) --> { call(P) }.
lift(P,X) --> { call(P,X) }.
lift(P,X,Y) --> { call(P,X,Y) }.



%% seqmap_with_progress( +Period:natural, +Pred:pred(A,S,S), +X:list(A))// is nondet.
%% seqmap_with_progress( +Period:natural, +Pred:pred(A,B,S,S), +X:list(A), ?Y:list(B))// is nondet.
%
%  Just like seqmap//2 and seqmap//3 but prints progress and memory usage statistics while running.
%  Information is printed every Period iterations. The first input list must be
%  valid list skeleton with a definite length, so that a percentage progress indicator
%  can be printed.
seqmap_with_progress(E,P,X) --> {progress_init(E,X,Pr0)}, smp(X,P,Pr0).
seqmap_with_progress(E,P,X,Y) --> {progress_init(E,X,Pr0)}, smp(X,Y,P,Pr0).

smp([],_,Pr) --> !, {progress_finish(Pr)}.
smp([X|XX],P,Pr1) --> {progress_next(Pr1,Pr2)}, call(P,X), !, smp(XX,P,Pr2).

smp([],_,_,Pr) --> !, {progress_finish(Pr)}.
smp([X|XX],[Y|YY],P,Pr1) --> {progress_next(Pr1,Pr2)}, call(P,X,Y), !, smp(XX,YY,P,Pr2).


progress_init(E,X,pr(T0,T,E,0,0)) :- length(X,T), get_time(T0).
progress_finish(Pr) :-
	progress_next(Pr,_),
	get_time(T1), Pr=pr(T0,N,_,_,_), 
	format('\nFinished ~w items in ~3g minutes.\n',[N,(T1-T0)/60]).

progress_next(pr(T0,Total,E,N,E),pr(T0,Total,E,M,1)) :- !, 
	succ(N,M),
	stats(Codes),
	get_time(T1), 
	format('~s | done ~0f% in ~3g s    \r', [Codes,100*N/Total,T1-T0]),
	flush_output.

progress_next(pr(T0,T,E,N,C),pr(T0,T,E,M,D)) :- succ(C,D), succ(N,M).


%% stats is det.
%% stats( -Codes:list(code)) is det.
%
%  Print or return memory usage statistics.
stats :- !, 
	stats(Codes),
	format('~s\r',[Codes]),
	flush_output.

stats(Codes) :- !, 
	statistics(heapused,Heap),
	statistics(localused,Local),
	statistics(globalused,Global),
	statistics(trailused,Trail),
	format(codes(Codes), 'heap: ~t~D ~18| local: ~t~D ~36| global: ~t~D ~57| trail: ~t~D ~77|',
		[Heap,Local,Global,Trail]).


%% select_def_option(+Option,+Default,+OptsIn,-OptsOut) is det.
%
%  Exactly the same as select_option/4 but with a different argument order:
%  =|option(Opt,Def,Opts1,Opts2)|= is equivalent to =|select_option(Opt,Opt1,Opts2,Def)|=.
%  Changed argument order allows multiple option selection to be written in 
%  DCG notation with the options list as the state.

select_def_option(Opt,Def,Opts1,Opts2) :- select_option(Opt,Opts1,Opts2,Def).
