% xtop.pl

%% is_prolog(Prolog): tells which Prolog is this: returns lprolog
is_prolog(lprolog).

version_data([5,4,4,
  'with fast swi_det_call and new swi_io and db predicates']). 

% topcall(T):-metacall(T).

topcall(G):-mcall_goal(G).

prolog_init(yes):-
  version_data([A,B,C,Mes]),
  cwrite('kp '),
  cwrite(A),cwrite('.'),cwrite(B),cwrite('.'),cwrite(C),cwrite(' '),
  cwrite(Mes),cnl,
  % traceln('License: free for research and academic use.'),
  call_ifdef(guardian_archangel,call_ifdef(guardian_angel,true)).

 
%% entry point in Prolog interactive loop     
toploop:-
  %traceln(starting(toploop)),
  toploop(main).

%% entry point in a module, in particular 'main'
toploop(WamFile):-
  trim_prolog_file(WamFile,Module,_Suf),
  traceln(entering(Module)),
  db_clear,
  apply_do_after_load(Module),
  run_cmd_args,
  toplevel(Module),
  traceln(exiting(Module)).

dyn_suffix('.dyn').


%% run_dynamics: unpacks dynamic commands from static predicate '$run'/1
run_dynamics:-
   is_compiled('$run'(_)),
   call('$run'(G)),
   topcall(G),
   fail.
run_dynamics.
   
 
%% run_dynamics(File): executes commands collected as '$run'/1 facts in <File>.dyn
run_dynamics(File):-apply_do_after_load(File).

%% apply_do_after_load(TopModule): consults .dyn complement of .bp file (if present)
apply_do_after_load(main):-!.
apply_do_after_load(Module):-
  dyn_suffix(Dy),
  atom_concat(Module,Dy,File),
  %find_file0(File0,[],File),
  exists_file(File),
  traceln(found_file(File)),
  db_reconsult(File,Module),
  traceln( consulted_file(File)),
  db_clear, % duplicates otherwise if done again
  ( db_clause(Module,'$run'(G),_),
    topcall(G),
    fail
  ; true
  ),   
  run_dynamics,  % same, if already embedded as $run/1
  db_clear(Module),
  traceln(cleared(Module)),
  !.
apply_do_after_load(_Module):-
  traceln(nothing_to_apply_do_after_loading). % (Module)).
  
 %% run_cmd_args: calls predicates on command line (backtracks over all their solutions!)   
run_cmd_args:-
  % traceln(run_cmd_args),
  pop_cmd_arg(T),
  % traceln(popping_cmd_argument(T)),
  nonvar(T),
  write_codes("<CMD LINE ARG> ?- "),writeln(T),
  run_cmd_arg(T),
  !,
  run_cmd_args.
run_cmd_args.

run_cmd_arg(Var):-var(Var),!,traceln(bad_cmd_arg_should_be_nonvar).
run_cmd_arg([File]):-!,scompile(File).  
run_cmd_arg(Goal):-
  topcall(Goal),
  fail.
run_cmd_arg(_).
 
 

 
%% toplevel(Module): interactive REPL for a module    
toplevel(Module):-
  stdio(I),
  trim_module_name(Module,Prompt),
  X=ignore,
  G=toplevel1(Prompt,I),
  new_engine(X,true,E),
  
  repeat,
    load_engine(E,X,G),
    get(E,A),
  handle_top_returns(A).
 

handle_top_returns(the(done)):-!. % exit
handle_top_returns(the(restart(F0))):-!,traceln(restarting_from(F0)).
handle_top_returns(the(exception(E))):-!,write('*** '),println(E),fail.

trim_module_name(main,main):-!.
trim_module_name(Module,Prompt):-atom_codes(Module,Cs),
  reverse(Cs,Rs),
  member(S,"/\\"),
  append(Xs,[S|_],Rs),
  !,
  reverse(Xs,Ps),
  atom_codes(Prompt,Ps).
trim_module_name(M,M).
  
toplevel1(Prompt,I):-
  repeat,
    engine_gc,
    topstep1(Prompt,I), % exit only with exceptions
  fail.
 
topstep1(Prompt,I):-
  interact_with(Prompt,I, T,Vs),
  !,
  topstep2(topcall,T,Vs,_CharRead).
topstep1(_Prompt,_I).



interact_with(Prompt0,I, T,Vs):-
  atomic_list_concat([Prompt0,' ?- '],Prompt),
  % current_input(J), swi_read_alt(Prompt,J, T,Vs), % losing cmd edit
  swi_read(Prompt,I, T,Vs).


swi_read(Prompt,I,T,NVs):-
  prompt_and_readln_codes(I,Prompt,TCs),
  ( TCs=the(Cs)->
    Cs\=[],
    atom_codes(S,Cs),
    S\=end_of_file,
    swi_call(
     read_term_from_atom(S,T,[variable_names(NVs)])
    )
 ;  TCs=[]->halt
 ; fail
 ).


topstep2(F,T,[],10):-!,topstep_novars(F,T).
topstep2(F,T,[X=V|XVs],C):-
  (
    call(F,T),
    %numbervars(T,0,_), 
    namevars(T),
    show_var(X,V),
    ( member(Xk=Vk,XVs),
      write(', '),
      show_var(Xk,Vk),
      fail
    ; kbd_wait(C)-> % fails if not stdio
      ([C]=";"->
       writeln(' ;')
       ; !, nl,topstep_end
      )
    ; writeln(' ;')
    ),
    fail
  ; topstep_end
  ).
  
topstep_end:-nl,writeln('No (more) answers.'),nl.

kbd_wait(_C):-fail. %get_single_char(C).
kbd_wait(C):-lean_get_code(C).

topstep_novars(F,T):-call(F,T),!,writeln('true.').
topstep_novars(_,_):-writeln('fail.').
  
show_var(X,A):-[SP,EQ]=" =",write(X),
 put_code(SP),put_code(EQ),
 put_code(SP),
 write(A),
 put_code(SP),
 flush_output.


% end
