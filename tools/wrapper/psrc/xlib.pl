% xlib.pl

% impure operations


%% numlist(Min,Max,Ns): generates list of ints between Min and Max
numlist(I,I,Is):-!,Is=[I].
numlist(I0,I,[I0|Is]):-I0<I,I1 is I0+1,numlist(I1,I,Is).

'..'(A,B,Is):-numlist(A,B,Is).

ints(A,B,Is):-numlist(A,B,Is).

% moved to xbuiltin.pl: atomic_list_concat(As,A):-maplist(name,As,Css)...

%% concat_atom(As,A): concatenates atoms on list As to A 
concat_atom(As,A):-atomic_list_concat(As,A).


%% digest_prolog_files(PrologFiles,OldRootWamFile,NewRootWamFile): extends lwam.bp with a list of pl files
digest_prolog_files(PrologFiles,OldRootWamFile,NewRootWamFile):-
   TempWamFile='temp__.bp',
   wcompile_mt(PrologFiles,TempWamFile),
   concatenate_files([OldRootWamFile,TempWamFile],NewRootWamFile).
   
%% external_engine(WamSource,X,G,E) : creates external engine E with separate code space and goal X,G
external_engine(WamSource,X,G,E):-
  external_engine(WamSource,E),
  load_engine(E,X,G).

%% external_engine(Source,E): creates new code space + engine from a *.bp file or reader
external_engine(Source,E):-
  new_interactor('vm.logic.LogicInteractor',Source,E).
 
external_engine(E):-external_engine('lwam.bp',E).

external_top(InFile,Source):-
  %traceln(external_TOP_calling(toploop(InFile))),
  external_engine(Source,ok,toploop(InFile),E),
  get(E,_A),
  stop(E),
  %traceln(external_TOP_finished(toploop(InFile))),
  % exit,
  true.

init_toploop(_Fname,InitGoal):-
  %println(ee=there),
  InitGoal,
  fail.
init_toploop(Fname,_InitGoal):-
  toploop(Fname).

% some flags

%% set_quickfail(V,OldV): sets flag forcing quick failure (if higher) 
set_quickfail(V,OldV):-jcall('vm.logic.Interact',set_quickfail(V),OldV).

get_quickfail(OldV):-jcall('vm.logic.Interact',get_quickfail,OldV).

%% set_verbosity(V,OldV): sets flag controlling verbosity (higher) of error messages
set_verbosity(V,OldV):-jcall('vm.logic.Interact',set_verbosity(V),OldV).

get_verbosity(OldV):-jcall('vm.logic.Interact',get_verbosity,OldV).

% compatibility with other Prologs

%% for(I,A,B): generates ints I in [A..B]
for(I,A,B):-between(A,B,I).

ctime(T):-cputime(T0),T is T0*1000.

%% statistics: prints out stats about various data areas
statistics:-(statistics(A,B),writeln(A=>B),fail;nl).

stats:-statistics.

%% set_prolog_flag(F,X): sets Prolog flag
set_prolog_flag(F,X):-F<==X.

%% current_prolog_flag(F,X): sets Prolog flag

current_prolog_flag(dialect,X):-is_prolog(X).
current_prolog_flag(version_data,X):-is_prolog(F),version_data(Xs),X=..[F|Xs].
current_prolog_flag(F,X):-gvar(F),F==>X.

meta_predicate(FN):-swi_call(meta_predicate(FN)).

%multifile(FN):-dynamic(FN).
%discontiguous(FN):-dynamic(FN).

%multifile(_).
%discontiguous(_).

multifile(FN):-dynamic(FN),swi_call(multifile(FN)).
discontiguous(FN):-dynamic(FN),swi_call(discontiguous(FN)).

match_before(Stop,Cs)-->match_before([Stop],Cs,_).

match_before(Stops,[],Stop)-->[Stop],{member(Stop,Stops)},!.
match_before(Stops,[C|Cs],Stop)-->[C],match_before(Stops,Cs,Stop).

%% set_encoding(Encoding): sets char encoding
set_encoding(Encoding):-
  call_java_class_method('vm.logic.Interact',setEncoding(Encoding),_).

%% get_encoding(Encoding): gets char encoding
get_encoding(Encoding):-
  call_java_class_method('vm.logic.Interact',getEncoding,Encoding).

force_encoding_of(S,Bad,Good, NewS):-
  call_java_class_method('vm.logic.Interact',forceEncodingOf(S,Bad,Good),NewS).

mac2utf(S,NewS):-force_encoding_of(S,'MacRoman','UTF-8', NewS).

win2utf(S,NewS):-force_encoding_of(S,'ISO-8859_1','UTF-8', NewS).

not_null(A):-var(A),!.
not_null('$null'):-!,fail.
not_null(_).
       
% backward compatibility box

% list50test(Xs):-call_java_class_method('vm.logic.TermConverter',bigList,Xs).

%% log(Mes): sends Mes + timestamp (usually something wrong) to the logger
log(Mes):-call_java_class_method('vm.logic.Interact',log(Mes),_).


log_to(Fname,Mes):-log_to(Fname,Mes,1).

%%  log_to(Fname,Mes,WithStamp): if WithTimeStamp>0 appends <<< tamp >>> + Mes to Fname
log_to(Fname,Mes,WithStamp):-
  call_java_class_method('vm.logic.Interact',log_to(Fname,Mes,WithStamp),_).
  
%% get_date_time(DateString): gets current date and time as formatted string

get_date_time(DateString):-call_java_class_method('vm.logic.Interact',getDateTime,DateString).

%% get_time(TimeFrom1970ms): get time from 1970 Jan 1 in miliseconds
get_time(TimeFrom1970ms):-call_java_class_method('vm.logic.Interact',getTime,TimeFrom1970ms).

symtest:-
  M is 1<<22,
  for(I,0,M),
  atom_concat(a,I,_),
  fail.
symtest.


mmaptest:-
  MI is 1000,
  MJ is 100,
  time(mmaptest(MI,MJ),T),
  R is MI*MJ*3,
  println(perf(ops(R),time(T))).

mmaptest(MI,MJ):-
  new_mmap(D),
  mmaptest1(D,MI,MJ),
  mmaptest2(D,MI,MJ),
  mmaptest3(D,MI,MJ).
  
mmaptest1(D,MI,MJ):-
  for(I,0,MI),
    for(J,0,MJ),
      mmap_put(D,I,J),  
  fail.
mmaptest1(_,_,_).

mmaptest2(D,MI,_):-
  for(I,0,MI),
      mmap_get(D,I,_),  
  fail.
mmaptest2(_,_,_).

mmaptest3(D,MI,MJ):-
  for(I,0,MI),
    for(J,0,MJ),
      mmap_remove(D,J,I),  
  fail.
mmaptest3(_,_,_).


mmaptest4:-
  default_mmap(D),
  mmap_put(D,a,b(1)),
  mmap_put(D,a,b(2)),
  mmap_put(D,11,3.14),
  mmap_put(D,11,2.73),
  writeln(D),
  (mmap_get(D,a,X),writeln(X),fail;true).
  

%% new_mmap(D): creates a new multi-map dictionary with one or more values associated to each key
new_mmap(D):-
  call_java_class_method('vm.extensions.MMap',create,D).

%% mmap_put(D,X,A): adds A to key X in multi-map D
mmap_put(D,X,A):-nonvar(X),nonvar(A),!,invoke_java_method(D,put(X,A),_).
mmap_put(D,X,A):-errmes(instantiation_error,mmap_put(D,X,A)).

%% mmap_get(D,X,A): returns an A associated to key X in multi-map D - backtracks if X unbound
mmap_get(D,X,A):-nonvar(D),nonvar(X),
   !,
   invoke_java_method(D,valueIterator(X),I),ielement_of(the(I),A).
mmap_get(D,X,A):-nonvar(D),!,
   invoke_java_method(D,keyIterator,KI),
   ielement_of(the(KI),X),
   invoke_java_method(D,valueIterator(X),I),
   ielement_of(the(I),A).
mmap_get(D,X,A):-errmes(instantiation_error,mmap_get(D,X,A)).

%% mmap_remove(D,X,A): removes specific A associated to key X in multi-map D
mmap_remove(D,X,A):-nonvar(D),nonvar(X),!,invoke_java_method(D,remove(X,A),_).
mmap_remove(D,X,A):-errmes(instantiation_error,mmap_remove(D,X,A)).

%% mmap_remove(D,X): removes all values associated to key X in multi-map D
mmap_remove(D,X):-nonvar(D),nonvar(X),!,invoke_java_method(D,remove(X),_).
mmap_remove(D,X):-errmes(instantiation_error,mmap_remove(D,X)).

default_mmap(M):-call_java_class_method('vm.extensions.MMap',default_mmap,M).
% end

jassert(Fact):-
  functor(Fact,F,_),
  default_mmap(M),
  mmap_put(M,F,Fact).
  
jasserted(Fact):-
  functor(Fact,F,_),
  default_mmap(M),
  mmap_get(M,F,Fact).

jretractall(Fact):-
  functor(Fact,F,_),
   default_mmap(M),
   mmap_remove(M,F).
   
   
jastests:-
  list_to_array([a(1),a(2),b(1),b(2)],A),jassert(arr(A)),
  list_to_array([1,3.14],AA),jassert(arr(AA)),
  (jasserted(arr(B)),array_to_list(B,Xs),writeln(Xs),fail;true).
  
  
% engine concurrent message queue API - works mostly like an e-mail

%% engine_send(Name,Mes): sends Mes to named engine     
engine_send(Name,Mes):-
  engine_get_name(MyName),
  ensure_mbox(Name,Q),
  invoke_java_method(Q,add('$mes'(MyName,Mes)),_).

%% engine_receive(From,Mes): receives message Mes and Name of the sender, From
engine_receive(From,Mes):-
  engine_get_name(Name),
  ensure_mbox(Name,Q),
  invoke_java_method(Q,poll,'$mes'(From,Mes)).

%% engine_messages(From,Mes) iterates over all messages binding From and Mes 
engine_messages(From,M):-
  engine_get_name(Me),
  ensure_mbox(Me,Q),
  invoke_java_method(Q,iterator,I),
  ielement_of(the(I),R),
  R='$mes'(From,M).

%% engine_has_messages: checks if this engine has messages
engine_has_messages:-engine_get_name(Name),engine_has_messages(Name).

%% engine_has_messages(Name): checks if named engine has messages
engine_has_messages(Name):-
   ensure_mbox(Name,Q),
   invoke_java_method(Q,isEmpty,R),
   is_false(R).


ensure_mbox(Name,Q):-gvar_get(Name,R),!,Q=R.
ensure_mbox(Name,Q):-
    new_java_object('java.util.concurrent.ConcurrentLinkedQueue',Q),
    gvar_set(Name,Q).
      
     
mes_test:-
  engine_set_name(alice),
  engine_send(alice,hi(42)),
  engine_send(alice,bye(42)),
  (engine_messages(From,M),writeln(mes(from(From,M))),fail;true),
  engine_has_messages(alice),
  engine_receive(From,Mes),
  writeln(got(From,Mes)),
  engine_send(bob,hello(100)),
  new_engine(got(Me,Hi),(
     engine_set_name(bob),
     engine_receive(Me,Hi)
  ),Other),
  engine_get(Other,the(MyMes)),
  writeln(MyMes),
  engine_stop(Other).
  
init_ag(Name,Goal):-
  engine_set_name(Name),
  new_mmap(M),
  % create_db(Name),
  mmap<=M,
  call(Goal).
  
new_ag(Name,X,Goal,Ag):-
   new_engine(X,init_ag(Name,Goal),Ag).
   
      
