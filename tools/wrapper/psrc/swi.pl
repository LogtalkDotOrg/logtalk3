% SWI 7++ to Lean Prolog interface

:- use_module(library(jpl)).
:- use_module(library(lists)).
:- use_module(library(gensym)).
:- use_module(library(apply)).
:- use_module(library(readutil)).
:- use_module(library(porter_stem)).
:- use_module(library(listing)).
:- use_module(library(ctypes)).
:- use_module(library(error)).

:- initialization(add_import_module(user,('$'),end)).

:- set_prolog_flag(double_quotes, codes).

% :- add_import_module(('$'),system,begin).
%:- add_import_module(('$'),user,end).

:-[swi_fcompiler]. % now embeds the next two
% :-[swi_db]. included is swi_compiler
:-[swi_compat].

:-[swi_io].

:-[swi_natint].

% :-[smeta].

%:-[builtins].


% c:-[swi].

%:-consult(stests). % optional

slave:-
  abolish(expand_query/4),
  assertz(expand_query(Query,Query,Bindings,Bindings)).

master:-
  abolish(expand_query/4),
  assertz(expand_query(Query, lean_call(Query),Bindings,Bindings)).

% SWI calling Lean

@G:-lean_call(G).
 
%% lean_call(Goal): calls Lean and explores the (assumed finite) list of answers

lean_call(Goal):-
  term_variables(Goal,Vs),
  lean_once(Vss,findall(Vs,Goal,Vss)),
  member(Vs,Vss).

%% calls Lean and collects one answer at most

/*
lean_once(G):-
  term_variables(G,Vs),
  lean_once(Vs,G).
*/

lean_once(Goal):-lean_call(Goal),!.

lean_once(X,G):-
   flag('$lean',Ctr,Ctr+1),
   atom_concat('$goal',Ctr,SymG),
   atom_concat('$answer',Ctr,SymA),
   nb_setval(SymG,(X:-G)),
 %writeq(SymG:SymA=(X:-G)),nl,
   nb_setval(SymA,no),
  
   %writeln('ENTERING jpl'),
   
   atom_concat('$',Ctr,CtrSym),
   jpl_call('swi.Swi',lean_call_once,[CtrSym],_R), 
   %writeln('EXITING jpl'),
   nb_getval(SymA,A),
 %writeq('$answer________'=A),nl,
   nb_delete(SymG),
   nb_delete(SymA),
 %writeq(gcall______=A),nl,
   the(X)=A.

nb_setval_sym(Sym-No,V):-atom_concat(Sym,No,K),nb_setval(K,V).

nb_getval_sym(Sym-No,V):-atom_concat(Sym,No,K),nb_getval(K,V).

% Lean calling SWI  
bundle_call(B,M):-
  from_sbundle(B,T),
  handle_call(T,A),
  to_sbundle(A,M).

lean_light_call(Goal,Answer):-
  jpl_call('swi.Swi',lean_light_call,[Goal],Answer).


% exception(undefined_predicate,Context,fail). %:-warnmes(Context).

/*
prolog_exception_hook(ExceptionIn, ExceptionOut, Frame, CatchFrame):-
 writeln('HERE'+ExceptionIn),
 %ExceptionOut=existence_error(procedure,boo/1),context(bee),
ExceptionOut=ExceptionIn,
 prolog_frame_attribute(Frame, goal, G),
 prolog_frame_attribute(CatchFrame, goal, GG),
 writeln('CONTEXT'=GG),
 writeln('CULPRIT'=G).
*/ 

/* 
analyse_error(existence_error(procedure,('$'): (F/N)),G):-!,
  functor(P,F,N),
  lean_call(is_compiled(P)),
  !,
  G=findall(Xs,('$'):G0,Xss),
  writeln(calling(G0)),
  % lean_call(G0),
  warnmes(Ex:in(G0)).

analyse_error(Ex,G):-warnmes(Ex:in(G)).
*/

errmes(A,B):-write('***** '),writeln(swi_error(A,B)),fail.

warnmes(Ex):-write('*** '),writeln(Ex),fail.
 
to_sbundle(T,B):-atomic(T),!,B=bundle(v(0),v(2),T).  
to_sbundle(T0,B):-
  copy_term(T0,T),
  to_bundle(T,_,2,_,Bs,[]),
  B=..[bundle,v(0),v(2)|Bs].
 
%to_bundle(X,V,K1,K2,S1,S2):-writeln(entering_to_bundle(X,V,k1=K1,K2,S1,S2)),fail.
to_bundle(X,V,K1,K2)-->{var(X)},!,[X],{K2 is K1+1,V=X,X=v(K1)}. 
to_bundle(X,V,K,K)-->{integer(X),abs(X)<2^28},!,{V=X}.   
to_bundle(X,V,K1,K2)-->{number(X)},!,{atom_number(A,X)},
   to_bundle('$number'(A),V,K1,K2).
to_bundle([],V,K,K)-->!,{V ='[]'}.   
to_bundle(X,V,K,K)-->{atomic(X)},!,{V =X}.
to_bundle(v(I),V,K,K)--> !,{V=v(I)}.
to_bundle(T,V,K1,K4)-->{compound(T)},!,
   {    
     functor(T,F0,N), fix_cons(F0,N,F),
     T=..[_|Xs],
     functor(TT,F,N),TT=..[F|Ys]
   }, 
   [F/N],{V=v(K1),K2 is K1+1}, % ref
   add_args(Ys,K2,K3),
   to_bundles(Xs,Ys,K3,K4).
   
fix_cons('[|]',2,R):-!,R=('.').
fix_cons(X,_,X).

add_args([],K,K)-->[].
add_args([Y|Ys],K1,K3)-->{K2 is K1+1},[Y],add_args(Ys,K2,K3).
   
to_bundles([],[],K,K)-->[].
to_bundles([X|Xs],[Y|Ys],K1,K3)-->to_bundle(X,Y,K1,K2),to_bundles(Xs,Ys,K2,K3).    


from_sbundle(B,T):-
   %B=..[bundle,_|Xs],writeln(from_sbundle=Xs),
   functor(B,bundle,N),
   functor(D,dict,N),
   parse_bundle(N,B,D),
   %writeln(dict=D),
   arg(2,D,T).

parse_bundle(1,_,_):-!.  
parse_bundle(I,B,D):-I>1,J is I-1,
  arg(I,B,X),
  arg(I,D,T),
  parse_one(X,T,I,D),
  parse_bundle(J,B,D).


% parse_one(A,B,C,D):-writeln(entering_parse_one(A,B,C,D)),fail.
parse_one(v(X),T,_I,D):- !,X1 is X+1,arg(X1,D,T).
parse_one('$number'/1,T,I,D):- % already converted in bundle2swi
  I1 is I+1,
  arg(I1,D,N),
  !,
  T=N.
parse_one(F/K,T,I,D):- functor(T,F,K),
  TI is K,
  DI is I+K,
  parse_args(TI,DI,T,D).
parse_one(A,T,_I,_D):-atomic(A),!,T=A.

%parse_args(A,B,C,D):-writeln(entering_parse_args(A,B,C,D)),fail.
parse_args(0,_TI,_T,_D):-!.  
parse_args(TI,DI,T,D):-
  TI1 is TI-1,
  DI1 is DI-1,
  arg(DI,D,X),
  arg(TI,T,X),
  parse_args(TI1,DI1,T,D).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this receives the calls from Lean
% the goal can be assumed to be wrapped in safe_call/1
% but it could otherwise be inside a findall
% the special case of undefined Lean predicates
% can be assumed to have on extra wraper inside safe_call
% i.e. on_lean_undef/1

handle_call((X:-G),R):-!,call(G),R=X.
handle_call(G,R):-call(G),R=G.


% safe_call(G):-writeln('SAFE_CALL'=G),fail.
safe_call(on_lean_undef(G)):-!,on_lean_undef(G). % forward this special case
safe_call(G):- !,%is_visible(G),!, writeln('SAFE_CALL_CATCHING_IN'=G),
  catch(G,
      Ex,
      handle_swi_undef(Ex,G)
  ).
safe_call(G):-
  throw(undefined_in_swi(G)).
  
  
try_lean_pred_not_in_swi(P):-   
   ( is_visible(P)->true 
   ; writeln('*** deprecated: redirecting from SWI'=try_lean_pred_not_in_swi(P)),
     assertz((P:-lean_call(try_ifdef(P)))),
     lean_log(memoed(P))
   ).
     
%handle_swi_undef(Ex,U):-writeln('ENTER_____handle_swi_undef'=Ex+U),fail.   
handle_swi_undef(
      error(existence_error(procedure, F/N), context(_,_)),
      Undef
   ):- !, 
  % writeln('GOT_TO______handle_swi_undef'=F/N+in(Undef)),
  functor(P,F,N),
  (
    try_lean_pred_not_in_swi(P) -> safe_call(Undef)
  ; throw(undefined_swi_prolog_call(F/N,in(Undef)))
  ).
handle_swi_undef(Ex,_Undef):-
  throw(Ex).

lean_pred(F/N):-
   functor(P,F,N),
   try_lean_pred(P).
   
try_lean_pred(P):-   
   ( is_defined(P)->true 
   ; is_visible(P)->
     assertz((($):P:-user:P)),
     lean_log(memoed(user:P))
   ; 
   writeln('*** deprecated: redirecting from SWI'=($):P),
     assertz((($):P:-lean_call(try_ifdef(P)))),
     lean_log(memoed(($):P))
   ).

% special case
on_lean_undef(Undef):-is_defined(Undef),!,
  catch(('$'):Undef,
      Ex,
      handle_lean_undef(Ex,Undef)
  ).
on_lean_undef(Undef):-is_visible(Undef),!,
  % writeln('VISIBLE_UNDEF'=Undef),
  call(Undef).  
on_lean_undef(Undef):-throw(undefined(Undef)).

handle_lean_undef(error(existence_error(procedure,($):(F/N)),context(_Context,_)),Undef):- !, 
  % writeln('GOT______lean_undef'=F/N+in(Undef)),
  functor(P,F,N),
  (
    % writeln('TTRY_LEAN'), 
    try_lean_pred(P) -> on_lean_undef(Undef)
  ; throw(undefined_swi_and_lean_prolog_call(F/N,in(Undef)))
  ).
handle_lean_undef(Ex,Undef):-
  throw(swi_prolog_error(Ex,in(Undef))).
  
is_visible(P):-predicate_property(P,visible),!.

is_defined(P):-predicate_property(($):P,interpreted),!.

is_defined(P):-predicate_property(($):P,defined),!.


lean_log(Mes):-prolog_flag(lean_log,true),
  !,
  open('logfile.txt',append,S),
  write(S,Mes),writeln(S,'.'),
  close(S).
lean_log(_).  
  
 