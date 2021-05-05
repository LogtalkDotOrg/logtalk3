% xnet.pl

/*
bigbug:-bigbug(50000).

bigbug(Size):-
  numlist(0,Size,Big),
  i_server(S),
  i_call(S,compound(Big)),
  i_halt(S),
  println('OK').
*/  

%% default_port (ServerPort): returns default server port
default_s_port(4444).

%% find_free_port(P): finds a free port starting from 500 and trying out the next 100 ports
find_free_port(P):-find_free_port(5000,100,P).

%% find_free_port(BasePort,MaxTry,FreePort): tries to find free port starting from BasePort up to BasePort+MaxTry
find_free_port(BasePort,MaxTry,P):-
  call_java_class_method('vm.extensions.Transport',find_free_port(BasePort,MaxTry),P).
  

s_server:-default_s_port(P),s_server(P).

s_server(Port):-integer(Port),!,s_server(Port,true).
s_server(Goal):-default_s_port(Port),s_server(Port,Goal).

%% s_server(Port,Goal): starts socket server on Port and runs Goal on it
s_server(Port,Goal):-Clone=1,s_server(Port,Clone,ignore,Goal). % bug with 1 fixed 

%% s_server(Port,BPFile,Goal): starts socket server on Port and runs Goal on it using compiled BPFile
s_server(Port,BPFile,Goal):-Op=3,s_server(Port,Op,BPFile,Goal).

s_connection(Connection):-default_s_port(P),s_connection(localhost,P,Connection).


s_ping:-default_s_port(P),s_ping(P).

s_ping(P):-s_ping(localhost,P).

s_wait:-default_s_port(P),s_wait(P).

s_wait(P):-s_wait(localhost,P).

%% s_wait(Host,Port): waits until server on Host:Port is on
s_wait(Host,Port):-s_ping(Host,Port),!.
s_wait(Host,Port):-sleep_ms(200),s_wait(Host,Port).


%% timed_s_wait(MaxWait,Host,Port): waits up to MaxWait ms until server on Host:Port is on
timed_s_wait(Ms,Host,Port):-Ms>0,s_ping(Host,Port),!.
timed_s_wait(Ms,Host,Port):-Ms>0,min(200,Ms,Time),sleep_ms(Time),Ms1 is Ms-200,timed_s_wait(Ms1,Host,Port).

s_run_at(C,G):-generic_s_run_at(C,the(G),the(G)).

%% s_run_at(Connection,X,G,R): remote call on Connection to run G returning the(X), no or exception
s_run_at(C,X,G,R):-generic_s_run_at(C,the(X,G),R).

s_call(G):-default_s_port(P),s_call(P,G).

s_call(P,G):-generic_s_call(localhost,P,the(G),the(G)).

% bad: s_call(X,G,R):-default_s_port(P),s_call(P,X,G,R).
s_call(H,P,G):-s_call(H,P,G,G,the(G)).

s_call(P,X,G,R):-s_call(localhost,P,X,G,R).

%% s_call(Host,Port,X,G,R): remote call to Host:Port to run G returning R= the(X), no or exception
s_call(H,P,X,G,R):-generic_s_call(H,P,the(X,G),R).

s_wait_call(H,P,X,G,R):-s_wait(H,P),s_call(H,P,X,G,R).

s_stop:-default_s_port(P),s_stop(P).

s_stop(P):-s_stop(localhost,P).

%% s_stop(Host,Port): stops server at Host:Port without halting the process in which it runs - broken
s_stop(H,P):-s_connection(H,P,C),s_stop_at(C).

s_halt:-default_s_port(P),s_halt(P).

s_halt(P):-s_halt(localhost,P).

%% s_halt(Host,Port): halts server 
s_halt(H,P):-s_connection(H,P,C),s_halt_at(C).

%% s_stop_at(Connection): stops the server on Connection
s_stop_at(Connection):-s_run_at(Connection,stop),!.
s_stop_at(Connection):-s_disconnect(Connection).

%% s_halt_at(Connection): halts the process hosting the server on Connection
s_halt_at(Connection):-s_run_at(Connection,halt(0)),!.
s_halt_at(Connection):-s_disconnect(Connection).

generic_s_run_at(Connection,T,R):-
  export_term(T),
  ask_s_server(Connection),
  import_term(R0),
  check_if_exception(R0,R).
  
  
check_if_exception(X,R):-arg(0,X,the),arg(1,X,A),nonvar(A),A=exception,!,R=exception.
check_if_exception(X,X).

generic_s_call(Host,Port,G,R):-
  s_connection(Host,Port,C),
  %traceln(entering_at(C,G)),
  generic_s_run_at(C,G,R0),
  s_disconnect(C),
  %traceln(exiting_at(C,G)),
  R=R0.


% inner thread client/server API

%% i_server(S): creates internal emulation of socket server - returns handle S instead of running on a port
i_server(Handle):-i_server(true,Handle).

%% i_server(Goal,S): runs inner server S after initial Goal run in the context of the inner server
% Clone=1 means a deep copy of current code and symbol table is made
% a possibly safer alternative is 2 using default lwam.bp read from file as code space
i_server(Goal,Handle):-Clone=1,BPFile=ignore,i_server(Clone,BPFile,Goal,Handle).


%% i_server(BPFile,Goal,Handle): runs inner server initialize with Goal, based on alternate *.bp file as starting point
i_server(BPFile,Goal,Handle):-Op=3,i_server(Op,BPFile,Goal,Handle). % Prolog.BPFILE

%% i_run_at(C,G): runs goal G at connection C on inner server
i_run_at(C,G):-generic_i_run_at(C,the(G),the(G)).

%%i_run_at(C,X,G,R): runs goal G at connection C on inner server returns R=the(X) or no if it fails
i_run_at(C,X,G,R):-generic_i_run_at(C,the(X,G),R).

%% i_call(S,G): runs goal G on inner server S
i_call(S,G):-generic_i_call(S,the(G),the(G)).

%% i_call(S,X,G,R): runs G on inner server S returning R=the(X) or no if it fails
i_call(S,X,G,R):-generic_i_call(S,the(X,G),R).

%% i_stop(C): stops innter server at connection C
i_stop(C):-i_run_at(C,stop),!.
i_stop(_).

%% i_halt(S): stops inner server with handle S
i_halt(S):-i_connection(S,C),i_run_at(C,stop),i_stop(C).

%% i_connection(S,C): opens a connection C to an i_server S

%% stops an i_server
i_exit(S):-i_connection(S,C),i_stop(C).

generic_i_run_at(Connection,T,R):-
  export_term(T),
  
  ask_i_server(Connection),
 
  import_term(R0),
  check_if_exception(R0,R).

generic_i_call(S,G,R):-
  i_connection(S,C),
  generic_i_run_at(C,G,R0),
  i_disconnect(C),
  R=R0.

% CONFIGURABLE SERVER LOOPS: used by both s_seerver and i_server

'$prolog_immediately'(Goal,ok):-
  %println(started_server___________________(Goal)),
  topcall(Goal),
  !.
  %traceln(succeeded(Goal)).



'$prolog_loop':-
  
  %traceln(started_server),
  repeat,
    % traceln('REPEATING_AT'(E)),
    % prolog_step(E,NewT),
    prolog_step(NewT),
    %traceln('AFTER_PROLOG_STEP'(E)),
    ( nonvar(NewT),stop=NewT-> !, % no more repeats !!! BIGBUG - solved == fails on big terms
      %stop(E),
      %traceln(stopping_server),
      stop,
      fail
    ; true
    ).
    %traceln('END OF REPEAT AT'(E)).

 prolog_step(NewT):-  
    %traceln(prolog_before_step_import), 
    import_term(T),
    %traceln(prolog_step_after_import________(xT)),
    normalize_import(T,X,G),
    %traceln(normalize_import(xT,xX,xG)),
    %traceln('ENGINE'(E)),
    %traceln(engine_called_with(T=>G)),
    %load_engine(E,X,G),get(E,R), % G might kill external engine if too big !!!
    %new_engine(X,G,E),get(E,R),stop(E),
    (G->R=the(X);R=no),
    %traceln(engine_returned(G=>R)),
    %log(G=>R),
    normalize_export(R,NewT),
    export_term(NewT).
    %traceln(normalize_export(R,NewT)).


lbug:-
  for(_,1,10),
    s_call(stats),
    for(_,1,100),
       s_call(symgc),
    fail
  ; true.


normalize_import(no,fail,fail):-!.
normalize_import(the(X),X,X):-var(X),!.
normalize_import(the(stop),stop,stop):-!.
normalize_import(the(G),G,G):-!.
normalize_import(the(X,G),X,G):-!.
normalize_import(G,R,R):-nonvar(G),G\=no,!,R=G.
normalize_import(_G,fail,fail):-traceln(bad_goal_received_on_server).

normalize_export(X,no):-var(X),!,traceln(unexpect_var_on_server).
normalize_export(the(X),the(X)):-var(X),!.
normalize_export(the(X),stop):-nonvar(X),X=stop,!.
normalize_export(the(exception(_)),the(exception)):-!,traceln('***'(exception_on_server)).
normalize_export(the(X),the(X)):-!.
normalize_export(no,no):-!.
normalize_export(_,exception):-traceln(bad_answer_computed_on_server).
  



% started when the first request to s_server or i_server arrives
'$prolog_loop1':-
  %writeln('LeanPrologServerStarted'),
  repeat,
    \+import_loop_step,
  !,
  traceln('$prolog_loop'(stopped)),
 
  fail.
  
import_loop_step:-
   bundle_call(Ok),
   return(Ok).
   
 
 % bundle_call(Ok): used when old prolog calls Lean Prolog
   
 bundle_call(Ok):-
  import_term(T),
  %traceln(import_loop_in(T)),
  call_import(T,R,Code),
  %traceln(import_loop_out(T=>[R,Ok])),
  export_term(R),  % returns answer as a bundle of type Object[]
                     % the array Object[2]={refs,cells}
  Ok=Code. 
    
call_import(the(stop),no,0):-!.  
call_import(the(G),R,Ok):-!,catch_call(G,G,R,Ok).
call_import(the(X,G),R,Ok):-!,catch_call(X,G,R,Ok).
call_import(G,R,Ok):-nonvar(G),G\=no,!,catch_call(G,G,R,Ok).
call_import(_,no,1).

catch_call(X,G,R,Ok):-new_engine(X,G,E),engine_get(E,A),
  (A==no->true;engine_stop(E)),
  %traceln(catch_call(X,G,A)),
  check_answer(A,R,Ok).
  
check_answer(no,no,1).
check_answer(the(X),R,Ok):-check_exception(X,R,Ok).

check_exception(X,R,Ok):-nonvar(X),X=exception(E),!,
  writeln('*** exception'(E)),
  R=the(exception),
  Ok=1.
check_exception(X,the(X),1).

%% alternative inner server API

ileaks:-
  for(_,1,10),
    ileak,
    stats,
  fail.

ileak:-
  for(I,1,100000),
    writeln(level(I)),
    symbols(N),
    traceln(syms(N)),
    % symgc,
    % statistics,
    % itest,
  fail.
ileak.

itestx:-
  for(_I,1,100000),
    % symgc,
    symbols(N),
    traceln(syms(N)),
    itest,
  fail.
    
itest:-
  new_inner_server(S),
  query_inner_server(S,X=1),
  query_inner_server(S,member(Y,[2,3])),
  stop_inner_server(S),
  traceln(got(X,Y)).
 
query_inner_server(IServer,G):-
  term_variables(G,Vs),
  query_inner_server(IServer,Vs,G,R),
  R=the(Vs).

query_inner_server(Server,X,G,Answer):-
  tell_inner_server(Server,X,G),
  ask_inner_server(Server,Answer).
  
%% new_inner_server(IServer): creates an inner server consisting of thread and 2 hubs    
new_inner_server(IServer):-
  IServer=hubs(In,Out),
  hub(In),hub(Out),
  new_logic_thread(In,_,inner_server_loop(In,Out)).

%% inner_server_loop(In,Out): loops consuming data from hub In and returning answers to hub Out
inner_server_loop(In,Out):-
  ask_interactor(In,(X:-G)),
  ( G==stop,!,tell_interactor(Out,done),fail
  ; G->R=the(X)
  ; R=no
  ),
  tell_interactor(Out,R),
  inner_server_loop(In,Out).

%% tell_inner_server(IServer,X,G): gives a task that the server starts executing
tell_inner_server(hubs(In,_Out),X,G):-
  tell_interactor(In,(X:-G)).

%% ask_inner_server(IServer,Answer): collects the answer from an inner server (and possibly waits until is done)
ask_inner_server(hubs(_In,Out),Answer):-
  ask_interactor(Out,Answer).

%% stop_inner_server(IServer): stops an inner server
stop_inner_server(IServer):-
  IServer=hubs(In,Out),
  query_inner_server(IServer,_,stop,done),
  stop_interactor(In),
  stop_interactor(Out).

  
