% lib.pl

% LIBRARY of basic predicates


%:-open_eb(Db),cwrite('Prolog initialized_with'(Db)),cnl.

%prolog_init(yes).

% C execution starts here - Java also gets here if
% Start calling run/3 requests it
% should provide a first choice point as C expect it for cut (bug?)
% should NOT contain a CUT at this point which would break C emulator
prolog_run(_):-G=main,is_compiled(G),G,fail.
prolog_run(R):-R=nothing_to_do.

% prolog_run(G):-run(G,_ignore).

% acts on goals in first position in the body  

metacall(X):-is_compiled(X),!,X.
metacall(X):-extensions_present,!,on_undefined(X).
metacall(X):-traceln(undefined_call(X)),fail.

extensions_present:-this_class(C),nonvar(C),C=0. %,traceln('EXTENSIONS_PRESENT').

% acts on goal in other than first position in the body - only in C
% note the ::- for compiling it as binary clause directly

'::-'(metatrue(TC),strip_cont(TC,T,C,metacall(T,C))).
strip_cont(TC,T,C):- strip_cont0(TC,C-T).

% JAVA ONLY

% JAVA EXECUTION STARTS HERE triggered from java file Start
% real entry point: G_prolog_main run/3; search for: G_prolog_main main()

run((Answer:-Query),Answer):-!,metacall(Query).
run(Query,Answer):-metacall(Query),Answer=Query.

chop_last_arg(FXsC,FXs):-
   FXsC=..[F|XsC],
   XsC=[_|_],
   append(Xs,[_],XsC),
   !,
   FXs=..[F|Xs].
   
% reserved name, possibly calls from compiled code

% needs Cont arg chopped
'$undefined'(0,FXsC):-
   !,
   chop_last_arg(FXsC,FXs),
   '$handle_undefined'(FXs).
% call ok already   
'$undefined'(1,FXs):-
   '$handle_undefined'(FXs).
   
'$handle_undefined'(G):-
   metacall(G).

on_undefined(Undef):-
  swi_call(on_lean_undef(Undef)). %!!!


% END OF JAVA ONLY

% SMALL LIBRARY OF ESSENTIAL BUILTINS

'='(X,X).

(A->B) :- if(A,B,fail).

once(G):-if(G,true,fail).

((A->B) ; C) :- !,if(A,B,C).
('*->'(A,B) ; C) :- !,if_any(A,B,C).
';'(X,_):-X.
';'(_,X):-X.

% or with if - faster if explicit
if(A,B,_):-A,!,B.
if(_,_,C):-C.

% or without if - faster, simpler
or(A,_):-A.
or(_,B):-B.

(X,Y):-X,Y.

\+(X):-X,!,fail.
\+(_).

not(X):-X,!,fail.
not(_).

\=(X,X):-!,fail.
\=(_,_).

unifiable(X,Y):- \=(X,Y),!,fail.
unifiable(_,_).

case1(X,Cs):-
  member(X=>G,Cs),
  !,
  G.

case(X,Cs):-
  member(X=>G,Cs),
  G.
    
repeat.
repeat:-repeat.

var(X):-type_of(X,0). %# theese can be inlined

nonvar(X):-type_of(X,0),!,fail.
nonvar(_).

arbitrary_num_type(_,T,R):-int_type(T),!,R=T.
arbitrary_num_type(_,T,R):-float_type(T),!,R=T.
arbitrary_num_type(Num,T,NewType):-bignum_type(Num,T),ensure_number(Num,R),type_of(R,NewType).

bignum_type(X,T):-compound_type(T),arg(0,X,'$number').

integer(X):-type_of(X,T),arbitrary_num_type(X,T,NewT),int_type(NewT). %#

float(X):-type_of(X,T),arbitrary_num_type(X,T,NewT),float_type(NewT). %#

number(X):-type_of(X,T),arbitrary_num_type(X,T,NewT),number_type(NewT). %#

atom(X):-type_of(X,T),atom_type(T). %#

compound(X):-type_of(X,T),compound_type(T),\+(bignum_type(X,T)). %#

callable(X):-type_of(X,T),symbolic_type(T).

symbolic(X):-type_of(X,T),symbolic_type(T).

simple(X):-type_of(X,T),simple_type(T). %#

atomic(X):-type_of(X,A),atomic_type(A). %#

% todo: extend, refine

char_type(X,T):-integer(X),!,char_type0(T,X).
char_type(to_lower(X),Y):-to_lower(X,Y).
char_type(to_upper(X),Y):-to_upper(X,Y).

char_type0(upper,X):-"AZ"=[A,Z],X>=A,X=<Z,!.
char_type0(lower,X):-"az"=[A,Z],X>=A,X=<Z,!.
char_type0(digit,X):-"09"=[A,Z],X>=A,X=<Z,!.
char_type0(space,X):-member(X,[9,10,13,32]),!.
char_type0(period,X):-member(X,".?!"),!.

to_lower(X,Y):-[C,A,Z]="aAZ",X>=A,X=<Z,!,Y is (X-A)+C.
to_lower(X,X).

to_upper(X,Y):-[C,A,Z]="Aaz",X>=A,X=<Z,!,Y is (X-A)+C.
to_upper(X,X).


%% interactor(T): recognizes type of an interactor
interactor(T):-type_of(T,10).

%% other_object(T): recognizes type of some Java objects other than engines, interactors etc.
other_object(T):-type_of(T,11).

simple_type(1).
simple_type(2).

compound_type(3).

atom_type(2).
atom_type(9). % String object

int_type(1). % is small int
int_type(6). % only if Integer - should never happen
int_type(7). % only relevant if it is a BigInteger

float_type(8). % only relevant if it has BigDecimal

number_type(T):-int_type(T),!.
number_type(T):-float_type(T).

atomic_type(T):-atom_type(T),!.
atomic_type(T):-number_type(T),!.
atomic_type(12). % engine

symbolic_type(3). % compound
symbolic_type(9). % String object

%% traceln(Term): writes out term to console directly
traceln(X):-cwrite(X),cnl.

% true.  

% computes N1 is N+1, unless N > Max
succ_to(Max,Max,_):-!,fail.
succ_to(_,N,NewN):-succ(N,NewN).

between1(From,_,From).
between1(From,To,R):-
  succ_to(To,From,Next),
  between1(Next,To,R).

%% between(From,To, I) : generates integers I in [From..To]
between(From,To,X):-From=<To,between1(From,To,X).

%% length(Xs,L): true if the length of list Xs is L - generates Xs if needed  
length(Xs,L):-nonvar(L),!,len2list(L,Xs).
length(Xs,L):-list2len(Xs,L).

list2len(Xs,L):-list2len(Xs,0,L).

list2len([],N,N).
list2len([_|Xs],N,L):-succ(N,N1),list2len(Xs,N1,L).

len2list(L,Xs):-len2list(0,L,Xs).

len2list(To,To,Xs):-!,Xs=[].
len2list(From,To,[_|Xs]):-succ(From,Next),len2list(Next,To,Xs).

%% functor(T,F,N): extracts F/N from F(X1...XN) and/or creates aterm with arity N and function symbol F
functor(T,F,N):-atomic(T),!,F=T,N=0.  % mimics typical Prologs
functor(T,F,N):-nonvar(T),!,arg(0,T,F),arity(T,N).
functor(T,F,N):-integer(N),atom(F),N>=0,fun(F,N,T).

%% =..(Term,List) : called 'univ' - converts between F(X1..Xs) and [F,X1,..Xn]
'=..'(T,[F|_Xs]):-var(T),var(F),!,fail.
'=..'(T,FXs):-number(T),!,FXs=[T].
'=..'(T,FXs):-nonvar(T),!,arity(T,N),succ(N,M),term_list(0,M,T,FXs).
'=..'(T,[F|Xs]):- % nonvar(F),is_list(Xs), % Paulo's request ~
   list2len(Xs,N),fun(F,N,T),succ(N,M),term_list(1,M,T,Xs).

term_list(To,To,_,[]):-!.
term_list(From,To,F,[X|Xs]):-
  succ(From,Next),
  arg(From,F,X),
  term_list(Next,To,F,Xs).

%% callN(FXs,Ys): adds supplementary args from list Ys to closure FXs and call resulting goal
callN(FXs,Ys):-
  FXs=..[F|Xs],
  append(Xs,Ys,Zs),
  G=..[F|Zs],
  G.

%% call(FXs,A1,...AN) : calls a closure of the form FXs=F(X1..Xk) with A1..An appended afetr X1..Kk    
call(FXs,Y):-
  arity(FXs,N),
  succ(N,N1),
  fun(FXs,N1,G),
  arg(N1,G,Y),
  unify_args(0,N1,FXs,G),
  G.
    
call(FXs,Y1,Y2):-
  arity(FXs,N),
  succ(N,N1),succ(N1,N2),
  fun(FXs,N2,G),
  arg(N1,G,Y1),arg(N2,G,Y2),
  unify_args(0,N1,FXs,G),
  G.

call(FXs,Y1,Y2,Y3):-
  arity(FXs,N),
  succ(N,N1),succ(N1,N2),succ(N2,N3),
  fun(FXs,N3,G),
  arg(N1,G,Y1),arg(N2,G,Y2),arg(N3,G,Y3),
  unify_args(0,N1,FXs,G),
  G.

call(FXs,Y1,Y2,Y3,Y4):-callN(FXs,[Y1,Y2,Y3,Y4]).

call(FXs,Y1,Y2,Y3,Y4,Y5):-callN(FXs,[Y1,Y2,Y3,Y4,Y5]).

call(FXs,Y1,Y2,Y3,Y4,Y5,Y6):-callN(FXs,[Y1,Y2,Y3,Y4,Y5,Y6]).

call(FXs,Y1,Y2,Y3,Y4,Y5,Y6,Y7):-callN(FXs,[Y1,Y2,Y3,Y4,Y5,Y6,Y7]).

call(FXs,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8):-callN(FXs,[Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8]).

call(FXs,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9):-callN(FXs,[Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9]).

unify_args(To,To,_,_):-!.
unify_args(From,To,F,G):-
  succ(From,Next),
  arg(From,F,X),arg(From,G,X),
  unify_args(Next,To,F,G).

%% numbervars(T,N0,N): Replaces free vars with '$VAR'(N) starting from N0 ending with N that is computed
numbervars('$VAR'(N0), N0, N) :- !, succ(N0,N).
numbervars(X, N0, N) :- atomic(X), !, N0=N.
numbervars([X|Xs], N0, N) :- !,
  numbervars(X, N0, N1),
  numbervars(Xs, N1, N).
numbervars(X, N0, N) :-
  arity(X,A),
  numbervar_args(0, A, X, N0, N).

numbervar_args(A, A, _, N0, N) :- !, N0=N.
numbervar_args(A0, A, X, N0, N) :-
  succ(A0,A1),
  arg(A1, X, X1),
  numbervars(X1, N0, N1),
  numbervar_args(A1, A, X, N1, N).

namevars(T):-namevars(T,T).

namevars(T,NT):-
  copy_term(T,T0),
  numbervars(T0,0,_),
  namevars1(T0,NT).

namevars1('$VAR'(N),S):-!,atom_concat('_',N,S).
namevars1(A,R):-atomic(A),!,R=A.
namevars1(A,R):-number(A),!,R=A.
namevars1(T,R):-T=..[F|Xs],R=..[F|Ys],
  maplist(namevars1,Xs,Ys).

%ground(X):-term_hash(X,_),!,fail.
%ground(_).



%% call_ifdef(G,AltG) : if G is compiled calls G, otherwise calls AltG
call_ifdef(G,_):-is_compiled(G),!,G.
call_ifdef(_,Alt):-Alt.




call_if_defined(G,_):-is_compiled(G),!,G.
call_if_defined(G,_):-is_dynamic(G),!,G.
call_if_defined(_,Alt):-Alt.


to_clause(C,R):-arg(0,C,(':-')),arity(C,2),!,R=C.
to_clause(C,(C:-true)).


%% unify_with_occurs_check(X,Y): true if X Y unifies subject to occurs check - this algorithm always terminates
 
unify_with_occurs_check(X,T):-var(X),!,var_does_not_occur_in(T,X),X=T.
unify_with_occurs_check(T,X):-var(X),!,var_does_not_occur_in(T,X),X=T.
unify_with_occurs_check(X,T):-atomic(X),!,X=T.
unify_with_occurs_check(T,X):-atomic(X),!,X=T.
unify_with_occurs_check(T1,T2):-
  arity(T1,N),arity(T2,N),
  uoc_list(0,N,T1,T2).

uoc_list(I,N,_,_):-I>N,!.
uoc_list(I,N,T1,T2):-succ(I,J),arg(I,T1,X),arg(I,T2,Y),
  unify_with_occurs_check(X,Y),
  uoc_list(J,N,T1,T2).

var_does_not_occur_in(T,X):-var(X),var(T),!.
var_does_not_occur_in(T,X):-var_occurs_in(T,X),!,fail.
var_does_not_occur_in(_,_).

var_occurs_in(T,X):-var(T),!,same_vars1(X,T).
var_occurs_in(T,_):-atomic(T),!,fail.
var_occurs_in(T,X):-argn(_,T,A),var_occurs_in(A,X),!.

%% argn(I,T,X): backtracks applying arg(I,Y,X) over all args of T (arg 0 included)
argn(I,T,X):-arity(T,N),between(0,N,I),arg(I,T,X).



%% count_answers(Goal,Count): counts number of answers of a goal, efficiently
count_answers(Goal,Count):-
   Ctr=s(0),
   count_goal(Goal,Ctr,Count).
   
count_goal(Goal,Ctr,_):-   
    Goal,
    arg(1,Ctr,I),
    J is I+1,
    change_arg(1,Ctr,J),
  fail.
count_goal(_,Ctr,Count):-
  arg(1,Ctr,Count).
  
% end  
