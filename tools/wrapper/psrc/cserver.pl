% cserver.pl


% PAIR: 
% ccall/1 with cserver/1 on port 9001
% shell_server with ctop on port 10001
% tested Oct 23, 2015

%% to_linear(T,CT): converts a term to parser independent representation - ## libs

to_linear(T0,NGs):-
  copy_term(T0,T),
  to_linear0(T,NGs).
  
to_linear0(T,[N|Gs]):-
  numbervars(T,0,N),
  to_canonical(T,Xs,[]),
  Gs=Xs.

to_canonical('$VAR'(T))-->!,{number_codes(T,Cs),length(Cs,L)},
  [1,L],inject_terminals(Cs).
to_canonical(T)-->{number(T),!,number_codes(T,Cs),length(Cs,L)},
  [2,L],inject_terminals(Cs).
to_canonical(T)-->{atom(T),!,atom_codes(T,Cs),length(Cs,L)},
  [3,L],inject_terminals(Cs).
to_canonical(T)-->
  {compound(T),'=..'(T,Ts),length(Ts,L)},
  [4,L],
  to_canonicals(Ts).

inject_terminals([T|Ts])-->!,[T],inject_terminals(Ts).
inject_terminals([])-->[].

to_canonicals([T|Ts])-->!,to_canonical(T),to_canonicals(Ts).
to_canonicals([])-->[].

%% from_linear(CT,T): converts a term back to normal Prolog representation
from_linear([N|Cs],T):-new_int_map(N,D),from_canonical(CT,D,Cs,[]),!,T=CT.

from_canonical(V,D)-->[1,L],!,extract_terminals(0,L,Cs), % var
  {number_codes(T,Cs),succ(T,I),int_map_get(D,I,V)}.
from_canonical(T,_)-->[2,L],!,extract_terminals(0,L,Cs),{number_codes(T,Cs)}.
from_canonical(T,_)-->[3,L],!,extract_terminals(0,L,Cs),{atom_codes(T,Cs)}.
from_canonical(T,D)-->[4,N],
  from_canonicals(0,N,Xs,D),
  {T=..Xs}.

from_canonicals(N,N,[],_)-->!,[].
from_canonicals(K,N,[X|Xs],D)-->{succ(K,K1)},
  from_canonical(X,D),
  from_canonicals(K1,N,Xs,D).

extract_terminals(N,N,[])-->!,[].
extract_terminals(K,N,[X|Xs])-->{succ(K,K1)},
  [X],
  extract_terminals(K1,N,Xs).


map_arity(64).

new_int_map(Max,D):-
  map_arity(Arity),
  Div is 1+Max // Arity,
  length(List,Div),
  maplist(new_int_slot(Arity),List),
  D=..['$$'|List].
  
int_map_get(D,I,X):-
  map_arity(Arity),
  Div is 1+(I // Arity),
  Mod is 1+(I mod Arity),
  arg(Div,D,Slot),
  arg(Mod,Slot,X).
  
  
new_int_slot(N,S):-functor(S,'$',N).  
  

/*
gg:-
  T1=fun("abc",goo(X,X,314,hi(Y,Y)),999),
  to_linear(T1,Xs),
  write(Xs),nl,
  write(T1),nl,
  from_canonical(Xs,T2),
  write(T2),nl,
  fail.
*/
  
% client in jlib.pl

% use shell_sever instead

cserver:-shell_server.

oldcserver:-cserver(9001).

cserver(Port):-
   % traceln(cserver_running_on_port(Port)),
   new_cserver(Port,Server),
   repeat,
     cserver_step(Server),
   fail.


 cserver_step(Server):-
   new_cservice(Server,S),
   recv_canonical(S,(X:-G)),
   %traceln((X:-G)),
   (G->R=the(X);R=no),
   send_canonical(S,R).
     
send_canonical(S,T):-to_linear0(T,Cs),csock_write(S,Cs).
recv_canonical(S,T):-csock_read(S,Cs),from_linear(Cs,T).

shell_server:-shell_server(10001).

shell_server(Port):-
   traceln(shell_server_running_on_port(Port)),
   new_cserver(Port,Server),
   shell_server_outer_loop(Server,_,[]),
   traceln(shell_server_finished_on_port(Port)).

shell_server_outer_loop(Server)-->
   % {traceln(at_beginning_of_outer_loop)},
   {new_cservice(Server,Socket)},
   shell_server_inner_loop(Socket),
   {close_csocket(Socket)},
   %{traceln(at_end_of_outer_loop_socket_closed)},
   shell_server_outer_loop(Server).
   
shell_server_inner_loop(S)-->
  %{traceln(begin_step_inner_loop)},
  shell_server_query_answer(S),
  !,
  %{traceln(end_step_inner_loop)},
  shell_server_inner_loop(S).
shell_server_inner_loop(_)-->
  %{traceln(inner_loop_failed)},
  [].

shell_server_query_answer(S,Db1,Db2):-
 %traceln(entering(shell_server_query_answer)),
 recv_canonical(S,Q),
 %traceln(shell_server_query_answer_got(Q)),
 shell_server_eval_query(Q,S,Db1,Db2),
 %traceln(shell_server_query_evaluated(Q)),
 true.

shell_server_eval_query((X:-G),S,D,D):-
   send_canonical(S,ready),
   %traceln(ready(X,G)),
   (G,R=the(X);R=no),
   recv_canonical(S,Cmd),
   %traceln(cmd(Cmd)),
   (Cmd=more->A=R;A=no),
   send_canonical(S,A),
   %traceln(cmd_answer(Cmd,A)),
   A=no,
   !.

hi-->{println(hi)}.
hi:-println(hi).
