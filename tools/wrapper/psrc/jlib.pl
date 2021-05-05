% jlib.pl

/*
% cc:-set_verbosity(10,_),ccall(X=10),println(X).
*/

open_csocket(Host,Port,Client):-
  new_java_object('vm.extensions.Transport'(Host,Port,0),Client),
  invoke_java_method(Client,connect,TF),
  is_true(TF).
  
csock_read(S,Cs):-
 invoke_java_method(S,read_from,T),
 atom_codes(T,Cs).

csock_write(S,Cs):-
  atom_codes(T,Cs),
  invoke_java_method(S,write_to(T),_).
 
open_csocket(S):-
  open_csocket(localhost,10001,S).


close_csocket(S):-invoke_java_method(S,disconnect,_).

ask_csocket(S,Q,_):-
  send_canonical(S,Q),
  fail.
ask_csocket(S,_,A):-
  recv_canonical(S,A).

new_cserver(Port,Server):-
  new_java_object('vm.extensions.Transport'(Port),Server).
 
new_cservice(Server,Socket):-
   call_java_class_method('vm.extensions.Transport',newService(Server),Socket).

   
% cserver:-cserver(9001).
% moved to cserver.pl

%% ccall(H,P,X,G,R): calls cserver at H:P with goal G and gets back R=the(X) or 'no' if it fails
ccall(H,P,X,G,R):-
  open_csocket(H,P,S),
  ask_csocket(S,(X:-G),R),
  close_csocket(S).
  
ccall(H,P,G):-term_variables(G,Vs),ccall(H,P,Vs,G,the(Vs)).  
  
ccall(G):-ccall(localhost,9001,G).

update_cserver(S,G):-term_variables(G,Vs),update_cserver(S,Vs,G,the(Vs)).
    
update_cserver(S,X,G,A):-ask_csocket(S,(X-->G),A).
  
load_cserver(S,X,G):-
  ask_csocket(S,(X:-G),A),
  (A=ready->true;errmes(server_not_ready,server_returned(A))).
  
ask_cserver(S, A):-
  ask_csocket(S,more,A).

thank_cserver(S):-
  ask_csocket(S,thanks,A),
  (A=no->true;errmes(expected_no_from_cserver,got(A))).
  
celement_of(S,G,X):-
  load_cserver(S,X,G),
  pick_celement_of(S,X).
 
pick_celement_of(S,X):-ask_cserver(S,the(A)),cselect_from(S,A,X).

cselect_from(_,A,A).
cselect_from(I,_,X):-pick_celement_of(I,X).


ctop:-ctop(localhost,10001).
  
ctop(Host,Port):-
  stdio(I),
  open_csocket(Host,Port,S),
  writeln('Type quit to exit ctop.'),
  repeat,
    ctopstep1(S,ctop,I,G), % exit only with exceptions
  G=quit,
  !,
  close_csocket(S).
  
ctopstep1(S,Prompt,I,G):-
  interact_with(Prompt, I, G,Vs),
  !,
  ( G=quit->true
  ;
    reverse(Vs,RVs),
    ( celement_of(S,G,RVs),
      ( RVs=[]->!,thank_cserver(S),writeln('true.'),nl
      ; true
      ),
      numbervars(RVs,0,_),
      RVs=[X0=V0|RVs0],
      show_var(X0,V0),
      ( member(X=V,RVs0),write(', '),show_var(X,V),fail
      ;  write(' '),kbd_wait(C),
         ( [C]=";"->put_code(C)
         ; !,thank_cserver(S),[E]=".",put_code(E),nl
         )
      )
      ; writeln('false.')
    ),
    nl
  ).
ctopstep1(_S,_Prompt,_I,true):-    
  % writeln('toplevel_syntax_error.'),
  true.

 
 
ccall1(G):-ccall1(localhost,10001,G).

ccall1(H,P,G):-
  term_variables(G,Vs),
  ccall1(H,P,Vs,G,the(Vs)).

ccall1(H,P,X,G,R):-
 open_csocket(H,P,S),
 det_ccall(S,X,G,A),
 close_csocket(S),
 R=A.
  
det_ccall(S,X,G,A):-
 load_cserver(S,X,once(G)),
 ask_cserver(S,A).

jtsize(T,R):-var(T),!,R=12.  
jtsize(T,R):-atomic(T),!,R=8.
jtsize(T,R):-T=..[_F|Xs],
  length(Xs,L),
  maplist(jtsize,Xs,Rs),
  sumlist(Rs,S),
  R is S+ % args, recursively
  8+ % for the String _F
  16+ % for the Fun object "f" and [...]
  (1+L)*4. % array of args
  
htsize(T,R):-compound(T),!,htsize0(T,R).
htsize(_,4).
  
htsize0(T,R):-var(T),!,R=0.  % stay in a fun
htsize0(T,R):-atomic(T),!,R=0. % stay in a fun
htsize0(T,R):-T=..Xs,
  length(Xs,L),
  maplist(htsize0,Xs,Rs),
  sumlist(Rs,S),
  R is S+ % args, recursively
  L*4. % fun+args
 
 % moved from lib.pl
   
 ground(X):-term_variables(X,[]). 

 
stresstest:-
   D is 10,
   T is 1<<26,
   stresstest(D,T).
 
 stresstest(Dim,Times):-
   % new_array('java.lang.Object',Size,A),
   open_list(Ls),
   numlist(1,Dim,Dims),
   between(0,Times,I),
   writeln(I),
   new_ndim_array('java.lang.Object',Dims,A),
   list_add(Ls,keep(I,A)),
   fail.
 stresstest(S,T):-
   writeln(done(size(S),times(T))).
   
   
   
 