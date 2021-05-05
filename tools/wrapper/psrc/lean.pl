% Lean to SWI Prolog 7++ interface

%:-op(800,fx,('#')).

%c:-[lean].
%:-[ltests].


% go:-println('starting').
% go:-digest_prolog_files([lean],'lwam.bp','lwam.bp').

% :- initialization(swi_call(use_module(library(logtalk)))).
% :- op(600, xfy, ::).

%% #Goal: API for calling SWI from Lean :
%  note: if cutting over non deterministic goal, 
% SWI engine is not freed!!!
% if you need to do that, use the optional API 
% and call stop_swi_engine directly

%%% LEan calls SWI

'#'(Goal):-swi_call(Goal).

%% swi_call(G):  run deterministic findall on SWI, and then explore answers
swi_call(G):-
   nonvar(G),term_variables(G,Vs),
   swi_det_call(Vss,findall(Vs,safe_call(G),Vss),Vss),
   member(Vs,Vss).   


swi_call_alt(G0):-
   nonvar(G0),term_variables(G0,Vs),G=(Vs:-safe_call(G0)),
   to_bundle(G,B),
   new_swi_engine(B,E),
   element_of(E,A),
   from_bundle(A,Vs). 
        
swi_once(T):-
  term_variables(T,Vs),
  swi_det_call(Vs,safe_call(T),R),
  % println(R),
  Vs=R.

swi_do(G):-swi_det_call(ignore,safe_call(G),_).
 
swi_det_call_old(X,T,A):-
  nonvar(T),
  to_bundle((X:-T),B),  
  % synchronized call to SWI
  swi_det_call0(B,R),
  R=the(BB),
  from_bundle(BB,A).
   
swi_det_call(X,T,A):-
  nonvar(T),
  bundle_op(0,(X:-T),A).
  
% lower level 
   
new_swi_engine(G,E):-
  current_engine_object(EO),
  call_java_class_method('swi.Swi',new_swi_engine(EO,G),E).  
     
get_swi_answer(E,A):-
     call_java_class_method('swi.Swi',get_swi_answer(E),A). 
 
stop_swi_engine(E):-
   call_java_class_method('swi.Swi',stop_swi_engine(E),_).   
  
%%% helper to SWI when it calls Lean 
 
% predicate called from SWI when calling Lean
handle_swi_signal(Ctr):-
   % pp(syms(SymG,SymA)),
   % get the goal by calling back SWI
   swi_once(nb_getval_sym('$goal'-Ctr,(A:-G))),
   % pp(lean_running_swi_goal=G),
   (topcall(G)->R=the(A);R=no),
   % give the result back to SWI
   % pp(lean_answering_swi=R),
   swi_once(nb_setval_sym('$answer'-Ctr,R)).
 
 % helper, for debugging

show_bundle(B):-call_java_class_method('swi.SwiEngine',show_bundle(B),_).

% semantics might be different to plain SWI - due to bugs there !!! 
qcompile(F):-swi_do(qcompile(F)).

% consults in SWI to module user
swi_consult(F):-swi_do(consult(F)).

