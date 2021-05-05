% engine.pl

/*
A Simple Hierartchical Memory Management Algorithm for Logic Engines:

a) failure or stop stops an engine (and clears all its memory), 

b) stopping a parent engine automatically stops all its children. 
*/

current_engine(X):-current_engine0(E),inteng2funeng(E,X).

% create_engine uses oveloaded 1-st arg: prolog or engine
create_engine(X0,G,X):-
  funeng2inteng(X0,E0),
  create_engine0(E0,G,E),
  inteng2funeng(E,X).

%  gc collects on backtracking - but it does not work in all cases
%  (true;engine_stop0(E),fail;true). 
  
engine_get(X,A):-funeng2inteng(X,E),engine_get0(E,A).
engine_stop(X):-funeng2inteng(X,E),engine_stop0(E).

to_engine(X,Term):-funeng2inteng(X,E),to_engine0(E,Term).

from_engine(Term):-from_engine0(Term).

funeng2inteng('$engine'(E),R):-!,R=E.
funeng2inteng(Bad,_):-errmes(bad_engine,funeng2inteng(Bad)).

inteng2funeng(E,'$engine'(E)).

%% is_engine(E) : true if E is an engine
is_engine('$engine'(E)):-integer(E),engine_type(E).
%is_engine(E):-interactor(E).

init_engine(E,X,G,R):-create_engine(E,(X:-topcall(G)),R). %,traceln('*** create_engine ***'(G)).

%% new_engine(X,G,Engine): creates engine with goal G returning instance of 'the'(X) or 'no' as answer
new_engine(X,G,E):-inteng2funeng(0,E0),init_engine(E0,X,G,E).

new_engine(WamFile,X,G,E):-
  atom(WamFile),
  external_engine(WamFile,X,topcall(G),E). % in xlib

%% new_engine(Engine,X,G): loades an engine with goal G returning instance of 'the'(X) or 'no' as answer
load_engine(E,X,G):-is_engine(E),!,init_engine(E,X,G,_).
load_engine(E,X,G):-is_interactor(E),!,
  %traceln(load_interactor_might_fail_on_large_terms(E)),
  tell_interactor(E,(X:-topcall(G))).
load_engine(E,X,G):-errmes(load_engine(X,G),should_be_engine(E)).

%% get(Interactor,Result) : collects a result computed by an Interactor (an engine in particular) 
get(E,X):-is_engine(E),!,engine_get(E,X). %,traceln(e_got(X)).
get(E,X):- is_interactor(E),!,ask_interactor(E,X). %,traceln(i_got(X)).
get(E,_):-errmes(get,should_be_engine(E)).

%% stop(E): stops an Interactor (an engine in particular)
% stop(E):-integer(E),!,(is_engine(E)->engine_stop(E);true). % tolerant of multiple stops
stop(E):-var(E),!,errmes(stop,unexpected_var(E)).
stop(E):-is_engine(E),!,engine_stop(E).
stop(E):-is_interactor(E),!,stop_interactor(E).
stop(E):-errmes(stop,not_an_engine_or_interactor(E)).

%% stop: stops current engine
stop:-inteng2funeng(0,FunEng),stop(FunEng).


    

%% is_interactor(E) : true if E is an interactor (LogicInteractor - a kind of engine , in particular)
is_interactor(E):-interactor(E).

ecopy_term(T,CT):-atomic(T),!,CT=T.
ecopy_term(V,_):-var(V),!.
ecopy_term(T,CT):- % traceln(ct(T)),
  create_engine(0,(T:-true),E),engine_get(E,the(R)),engine_stop(E),CT=R.

%% copy_term(T,CT): creates copy of term T with fresh variables
copy_term(T,CT):-icall(1,T,CT).

% deprecated
dismantle_engine(E):-engine_stop(E).

%% throw(E): throws an exception E
throw(E):-var(E),!,errmes(instantiation_error, nonvar_expected_in(throw(E))).
throw(E):-
  % log(throwing(E)),
  return(exception(E)),fail,return(unexpected_after_throw(E)).


%% catch(Goal,Exception,OnException): if calling Goal throws E and E=Exception call OnException, otherwise throw E    
catch(Goal,Exception,OnException):-
  new_engine('$answer'(Goal),Goal,Source),
  pick_catch_element_of(Source,Exception,OnException,Goal).
  
pick_catch_element_of(I,Ex,Alt,G):-
  get(I,the(A)),
  catch_select_from(I,A,Ex,Alt,G).

catch_select_from(I,Ret,_Ex,_Alt,_G):-var(Ret),!,
  engine_stop(I),return(Ret).
catch_select_from(I,'$answer'(A),_Ex,_Alt,once(G)):-!,
  engine_stop(I),
  A=once(G).
catch_select_from(I,'$answer'(A),Ex,Alt,G):-!,
  catch_select_answer_from(I,A,Ex,Alt,G).  
catch_select_from(I,exception(Ex),Ex,Alt,_G):-!,
  engine_stop(I),topcall(Alt).
catch_select_from(I,Ret,_Ex,_Alt,_G):-
  engine_stop(I),return(Ret).

catch_select_answer_from(_I,A,_Ex,_Alt,A). 
catch_select_answer_from(I,_A,Ex,Alt,G):-
  pick_catch_element_of(I,Ex,Alt,G).
  

% deterministic variant of catch 
catch_once(Goal,Exception,OnException):-
  catch(once(Goal),Exception,OnException).

  
  
%% element_of(Interactor,X): backtracks over all answers returned by Interactor (possibly an engine)
element_of(I,X):-pick_element_of(I,X).
% element_of(I,_):-engine_stop(I),fail. % the engine does that when no mor sols are found !!!

pick_element_of(I,X):-get(I,the(A)),select_from(I,A,X).

select_from(_,A,A).
select_from(I,_,X):-pick_element_of(I,X).

good_element_of(E,X):-element_of(E,X),check_element(E,X).

check_element(E,X):-nonvar(X),X=exception(Exception),
  stop(E),
  !,
  throw(Exception).
check_element(_,_).

%% alt_findall(X,G,Xs): collects to a list Xs copies of all answers X of G using engines
alt_findall(X,G,Xs):-
  open_list(Ls),
  run_findall(Ls,X,G),
  close_list(Ls,Rs),
  %length(Rs,Len),
  copy_all_terms(Rs,Xs).

  
run_findall(L,X,G):-G,list_add(L,X),fail.  
run_findall(_,_,_).
 
copy_all_terms([],[]).
copy_all_terms([X|Xs],[Y|Ys]):-
  copy_term(X,Y),
  copy_all_terms(Xs,Ys).

%% findall(X,G,Xs): collects to a list Xs copies of all answers X of G using engines
findall(X,G,Xs):-findall(X,G,Xs,[]).

%% findall(X,G,Xs,End): collects to a list Xs copies of all answers X of G concatenated with list End
findall(X,G,Xs,End):-
   % traceln('***findall'(X,G)),
   new_engine('$found'(X),topcall(G),E),
   %type_of(E,Ty),traceln(type_of(E,Ty)),
 
   engine_get(E,Answer),
   collect_all_answers(Answer,E,As,End),
   Xs=As.   
   % stop(E). % only needed on exceptions
  

% collects all answers of a Solver
collect_all_answers(no,_,Xs,Xs).
collect_all_answers(the(R),E,[X|Xs],End):-
   check_return(R,E,X),
   %traceln(getting),
   engine_get(E,Answer),
   %traceln(got(E,Answer)),
   collect_all_answers(Answer,E,Xs,End).

check_return(Ret,_E,X):-nonvar(Ret),Ret='$found'(A),!,X=A.
check_return(Ret,E,_):-engine_stop(E),return(Ret),fail.


%% call_det(G, Det): succeeds if call(G) succeeds and unifies Det with true if G has no more solutions, with false otherwise


%% if_any(Cond,Then,Else) for each success of Cond evaluates Then, but if Cond just fails Else is called
if_any(Cond,Then,Else):-
  new_engine(Cond,Cond,Engine),
  engine_get(Engine,Answer),
  select_then_or_else(Answer,Engine,Cond,Then,Else).

select_then_or_else(no,_,_,_,Else):-Else.
select_then_or_else(the(BoundCond),Engine,Cond,Then,_):-
  backtrack_over_then(BoundCond,Engine,Cond,Then).

backtrack_over_then(Cond,_,Cond,Then):-Then.
backtrack_over_then(_,Engine,Cond,Then):-
  engine_get(Engine,the(NewBoundCond)),
  backtrack_over_then(NewBoundCond,Engine,Cond,Then).

% generic tools

% combines 2 by 2 answers I of Generator, by applying Closure F,
% and accumulating in Final the overall result 
% no 0 element is needed as in Haskell because we initialize
% with the first solution
% if the generator is 'empty' i.e if it always fails
% then fold will simply fail - this make its behvior compositional 

%% foldall(F,X^Generator,R): combines 2 by 2 answers X of Generator, by applying closure F
foldall(F,X^G,R):-
  new_engine('$found'(X),G,E),
  engine_get(E,the(A)),
  check_return(A,E,R1),
  combine_with(E,F,R1,R2),
  !,
  R=R2,
  engine_stop(E).

combine_with(E,F,R1,R3):-
  engine_get(E,the(A)),
  check_return(A,E,X),
  call(F,R1,X,R), % not yet defined
  !,
  R2=R,
  combine_with(E,F,R2,R3).
combine_with(_,_,R,R).

% fold operations on engines
efoldl(Engine,F,R1,R2):-
  engine_get(Engine,X),
  efoldl_cont(X,Engine,F,R1,R2).

efoldl_cont(no,_Engine,_F,R,R).
efoldl_cont(the(X),Engine,F,R1,R2):-
  check_element(Engine,X),
  call(F,R1,X,R),
  efoldl(Engine,F,R,R2).
  
efoldr(Engine,F,R1,R2):-
  engine_get(Engine,X),
  efoldr_cont(X,Engine,F,R1,R2).

efoldr_cont(no,_Engine,_F,R,R).
efoldr_cont(the(X),Engine,F,R1,R2):-
  check_element(Engine,X),
  efoldr(Engine,F,R1,R),
  call(F,X,R,R2).

best_of(Answer,Comparator,Generator):-
  new_engine(Answer,Generator,E),
  efoldl(E,compare_answers(Comparator),no,Best),
  Answer=Best.

compare_answers(Comparator,A1,A2,Best):-
  ( A1\==no,call(Comparator,A1,A2)->Best=A1
  ; Best=A2
  ).

/*
etest:-new_engine(X,member(X,[1,2,3]),E),
  is_engine(E),get(E,X),get(E,Y),engine_stop(E),
  println(E+X+Y),numbervars(E,0,_),fail;nl.
*/

%% ask_engine(Engine,Query, Result): injects Query to into Engine, which, will react to it with engine_yield(Answer)
ask_engine(Engine,Query, Result):-
  to_engine(Engine,Query),
  get(Engine,Result).

%% engine_yield(Answer): reacts to ask_engine(Engine,Query, Result) by calling from_engine and then returning Answer
engine_yield(Answer):-
  from_engine((Answer:-Goal)),
  call(Goal),
  return(Answer).

%% errmes(Message,Culprit): generates error message
errmes(Message,Culprit):-
  Error=error(Message,Culprit),
  % traceln(Error),
  log(Error),
  % traceln(after_log),
  throw(Error),
  fail.

user_error(M,E):-errmes(M,E).

warnmes(Message,Culprit):-
  traceln('failing_with_warning'(Message,Culprit)),
  fail.
% warnmes(_Message,_Culprit). 
  
% lazy engine operations

%% lazy_set_state(X,G): used with DCGs - sets stream to L such that X:G~>L.
lazy_set_state(X,G,_,L):-lazy_findall(X,G,L).

%% lazy_get_state(Xs) : to be used with DCGs - gets current lazy stream
lazy_get_state(Xs,Xs,Xs).


%% X:G~>LazyList: list comprehension syntax for lazy_findall
X:G~>LazyList:-lazy_findall(X,G,LazyList).

%% lazy_findall(X,G,LazyList): creates LazyList to be explored using DCG syntax with lazy_take,lazy_drop etc.
lazy_findall(X,G,'$l'(E,Xs-Xs)):-new_engine(X,G,E).

%% lazy_head(X): to be used with DCGs - gets head X of lazy stream, advances if needed
lazy_head(X,L,NewL):-lazy_take(1,[X],L,NewL).

%% lazy_tail: to be used with DCGs - consumes head of lazy stream, advances if needed
lazy_tail(L,NewL):-lazy_drop(1,L,NewL).

%% lazy_take(N,Xs): to be used with DCGs - gets first N of lazy stream as Xs, advances if needed
lazy_take(N,Xs, L,NewL):-
  lazy_split(N,Xs,L,Temp),
  lazy_mix(L,Temp,NewL).

lazy_mix('$l'(E,Xs-_),'$l'(E,_-Bs),R):-!,R='$l'(E,Xs-Bs).
lazy_mix(Xs,Xs).

%% lazy_drop(N): to be used with DCGs - drops first N of lazy stream, advances if needed
lazy_drop(N, L,NewL):-lazy_split(N,_,L,NewL).

lazy_split(N,[X|Xs],L,NewL):-
  N>0,
  N1 is N-1,
  lazy_split2(X,L,Temp),
  !,
  lazy_split(N1,Xs,Temp,NewL).
lazy_split(_,[],L,L).

lazy_split2(X,L0,L2):-lazy_force(L0,L1),lazy_split1(L1,L2,X).

lazy_split1([X|Xs],Xs,X).
lazy_split1('$l'(E,Q),'$l'(E,NewQ),X):-dq_remove(X,Q,NewQ).

lazy_force('$l'(E,Q),NewLazy):-dq_is_empty(Q),!,
  get(E,A),
  ( A=the(X)->dq_add(X,Q,NewQ),NewLazy='$l'(E,NewQ)
  ; stop(E),dq_close(Q,NewLazy)
  ).
lazy_force(Lazy,Lazy).

%% dq_add/3, dq_remove/3, dq_is_empty/1: difference list queue operations
dq_add(X,Xs-[X|Ys],Xs-Ys).
dq_remove(X,[X|Xs]-Ys,Xs-Ys).
dq_peek(X,[X|_]-_).
dq_is_empty(Xs-_):-var(Xs).
dq_close(Xs-[],Xs).

%tie_engines(X,G1,_,R):-new_engine(X,G1,E1),element_of(E1,R).
%tie_engines(X,_,G2,R):-new_engine(X,G2,E2),element_of(E2,R).
 

tie_goals(G1,_):-G1.
tie_goals(_,G2):-G2.

%% zip_goals(G1,G2): merges answer streams coming from G1 and G2 such that elements alternate
zip_goals(G1,G2):-
  term_variables(G1+G2,Vs),
  zip_goals(Vs,G1,G2,R),
  Vs=R.

%% zip_goals(G1,G2): merges answer streams of pattern X coming from G1 or G2 such that elements R alternate
zip_goals(X,G1,G2,R):-
  new_engine(X,G1,E1),
  new_engine(X,G2,E2),
  bi_element_of(E1,E2,R).
 
bi_element_of(E1,E2,X):-pick_element_of_either(E1,E2,X).

pick_element_of_either(I,J,X):-get(I,A),skip_failed(I,J,A,X).

skip_failed(I,J,the(A),X):-bi_select_from(J,I,A,X).
skip_failed(_,J,no,X):-pick_element_of(J,X).

bi_select_from(_,_,A,A).
bi_select_from(I,J,_,X):-pick_element_of_either(I,J,X).

%tie_engines(X,Y
ziptest:-
  zip_goals(for(I,1,3),for(I,101,106)), 
  writeln(I),
  fail.
  
  
