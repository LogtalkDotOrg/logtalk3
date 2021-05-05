% xtask.pl

% multithreading task API 

%% call_with_time_limit(Secs,Goal): calls goal on a thread while watching that no more than Secs time is taken in which case it throws time_limit_exceeded
call_with_time_limit(Secs,Goal):-
  timed_call(Secs,Goal),
  !.
call_with_time_limit(_,_):-
  throw(time_limit_exceeded).

%% timed_call(Secs,Goal): calls goal on a thread while watching that no more than Secs time is taken in which case it fails
timed_call(Secs,Goal):-
  Ms is Secs * 1000,
  term_variables(Goal,Vs),
  timed_call(Ms,(Vs:-Goal),R),
  R=the(Vs).

%% timed_call(MSecs,(Answer:-Goal),Result): calls goal on a thread running for no more than MSecs and returns the(Answer),time_limit_exceeded or no
timed_call(MSecs,(Answer:-Goal),Result):-
  hub(H),
  new_logic_thread(H,good(Answer),once(Goal)),
  new_logic_thread(H,time_limit_exceeded,sleep_ms(MSecs)),
  ask_interactor(H,R),
  (R=the(good(X))->Result=the(X);R=no->Result=no;the(Result)=R),
  stop_interactor(H).

bg(G):-
  ( get_verbosity(V),V>2->
    traceln('bg/1 deprecated, use bg(Goal,Hub) and stop(Hub) to control thread')
  ; true
  ),
  bg(G,_H).

bg_clone(G):-
  traceln('bg_clone/1 deprecated, use bg_clone(Goal,Hub) and stop(Hub) to control thread'),
  bg_clone(G,_Hub).

%% bg_clone(Goal,Hub): launches a background thread discarding all solutions of Goal and cloning current symtable - use stop(Hub) to stop it
bg_clone(G,Hub):-bg(1,G,Hub).

%% bg(Goal,Hub): launches a background thread discarding all solutions of Goal. stop(hub) can be used to stop the thread 
bg(G,Hub):-bg(0,G,Hub).

bg(Clone,G,Hub):-
  var(Hub),
  !,
  hub(Hub),
  X=ignore,
  Source=no,
  add_fail(G,NewG),
  new_logic_thread(Hub,X,NewG,Clone,Source).
bg(Clone,G,Hub):-
  errmes(bg_expects_var_as_hub,bg(Clone,G,Hub)).

bg_from_file(G,H):-bg_from_file('lwam.bp',G,H).

bg_from_file(Fname,G,H):-hub(H),add_fail(G,NewG),new_file_thread(H,done,NewG,Fname).

%% bg_with(WamFileName,Goal,Hub): starts goal with code loaded from WamFileName
% see CodeLoader.load(...) for *.bp vs. *.wam loading - wam is merged with default *.bp in jar
% this actually happens in CodeLoader.fload(...)
bg_with(WamFileName,Goal,H):-wam_reader(WamFileName,Reader),bg_from_stream(Reader,Goal,H).

bg_from_stream(Stream,G,H):-hub(H),add_fail(G,NewG),new_stream_thread(H,done,NewG,Stream).

%% mbg(Goals):: launches Goals in parallel and waits until all of them terminate - note this ignores the answers they produce
mbg(Gs):-maplist(add_fail,Gs,NewGs),mbg(NewGs,_),fail.
mbg(_).

add_fail(G,(topcall(G),fail)).

%% mbg(Tasks,Result): launches a list of tasks [...(X:-G)...] on independent threads and collects their results X one at a time  
%% mbg/2 example: ?- mbg([(I:-for(I,1,3)),(J:-member(J,[a,b]))],R). % note - similar to multi_all/2, except that in iterates
mbg(XGs,R):-
  new_task_group(TG),
  mbg1(XGs,TG),
  run_mbg(TG,R).

mbg1([],_).
mbg1([XG|XGs],TG):-!,
  add_one_task(XG,TG),
  mbg1(XGs,TG). 
  
run_mbg(TG,R):-run_tasks(TG,R).
run_mbg(TG,_):-stop_task_group(TG),fail.
  

%% run_tasks(TG,Result): runs all tasks in group TG and collects all results one a time ; it fails when no results are left
run_tasks('$task_group'(Hub,Ctr),R):-
  repeat,
    get(Hub,A),
    ( A=no,ctr_dec(Ctr),ctr_get(Ctr,0),!,fail % this means a task has finished
    ; A=the(R)
    ).
    

% fixed task group task API

add_one_task((X:-G),TG):-!,new_task(TG,X,G).
add_one_task(G,TG):-new_task(TG,G,G).

%% add_task(G): add to default task group a task producing instances of G 
add_task(G):-add_task(G,G).

%% add_task(X,G): add to default task group a task producing answers X while running G 
add_task(X,G):-
 ( '$task_group'==>TG -> add_task(X,G,TG)
 ; add_task(X,G,TG),'$task_group' <==TG
 ).

%% run concurrently all tasks in current task group
run_tasks(X):-
  ('$task_group' ==>TG),
  run_tasks(TG,X).

%% stop_tasks tries to release tasks in default task group by stopping its Hub
stop_tasks:-
  '$task_group' ==>TG,
  remove_val('$task_group'),
  stop(TG).
    
%% add_task(X,G,TG) : adds a task (X:-G) to a task group TG - it also creates TG, if needed
add_task(X,G,TG):-var(TG),!,new_task_group(TG),new_task(TG,X,G).
add_task(X,G,TG):-new_task(TG,X,G).

new_task('$task_group'(Hub,Ctr),X,G):-new_logic_thread(Hub,X,G),ctr_inc(Ctr).

new_task_group(TG):-hub(H),new_ctr(Ctr),TG='$task_group'(H,Ctr).
  
stop_task_group('$task_group'(H,_)):-stop(H).
  
% end  

mt_test0(Rs):-multi_all([(I:-for(I,1,10)),(J:-member(J,[a,b,c]))],Rs).

mt_test1(Rss):-multi_findall([(I:-for(I,1,10)),(J:-member(J,[a,b,c]))],Rss).

mt_test2:-set_verbosity(10,_),for(I,1,100),println(mt_test2(I)),mt_test2(1000,_),fail.

mt_test2(N,Rss):-multi_findall([
  (a(I):-for(I,1,N)),
  (b(I):-for(I,1,N)),
  (c(I):-for(I,1,N)),
  (d(I):-for(I,1,N)),
  (e(I):-for(I,1,N))
],Rss).


mt_test3:-set_verbosity(10,_),for(_,1,100),mt_test3(1000),fail.


mt_test3(N):-
  multi_fold(min,[
  (I:-for(I,500,N)),
  (I:-for(I,200,N)),
  (I:-for(I,300,N)),
  (I:-for(I,100,N)),
  (I:-for(I,400,N))
],R),
println(min=R).

mt_test4:-alt_multi_all([(1:-true),(3:-true),(2:-true),(X:-member(X,[a,b]))],R),println(R).

mt_test5:-multi_first(1,[
   (never:-repeat,sleep(2),println(still_running),fail),
   (10:-true)
  ],R)
  ,println(R),symgc,stats.

xm1:-for(I,1,100),mt_test6,println(started(I)),fail;stats.
  
mt_test6:-multi_first(2,[
   (never:-repeat,sleep(3),println(still_running(a)),fail),
   (10:-true),(20:-true),(30:-true),
   (never:-repeat,sleep(3),println(still_running(b)),fail)
  ],R)
  ,println(R).
 
mt_test7:-time(multi_first([
   (4:-sleep(4)),
   (2:-sleep(2)),(3:-sleep(3)),(1:-sleep(1)),
   (never:-repeat,sleep(3),println(still_running(b)),fail)
  ],R))
  ,println(R).
  
mt_test8:-
   for(_,1,100), 
     hub(Hub),new_logic_thread(Hub,ignore,(for(J,1,1000),sleep_ms(50),println(J),fail)),
     sleep_ms(300),
     stop(Hub),
     println(stopped(Hub)),
   fail.

mt_test9(N):-
  timed_call(2000,(I:-for(I,1,N),println(still_working(I)),sleep_ms(200),I=N),R),
  println(R).
  
xm2:-
  mt_test9(6),
  mt_test9(50),
  G=(repeat,fail,R=never_happens),
  catch(call_with_time_limit(3,G),time_limit_exceeded,R=timeout),
  println(caught(R)),
  stats.
  


   
%% multi_all(XGs,Xs): runs list of goals XGs of the form Xs:-G ant collects all answers to a list

multi_all(XGs,Xs):-
  hub(Hub),
  length(XGs,ThreadCount),
  launch_logic_threads(XGs,Hub),
  collect_thread_results(ThreadCount,Hub,Xs),
  stop_interactor(Hub).

collect_thread_results(0,_Hub,[]).
collect_thread_results(ThreadCount,Hub,MoreXs):-ThreadCount>0,
  ask_interactor(Hub,Answer),
  count_thread_answer(Answer,ThreadCount,ThreadsLeft,Xs,MoreXs),
  collect_thread_results(ThreadsLeft,Hub,Xs).  
  
count_thread_answer(no,ThreadCount,ThreadsLeft,Xs,Xs):-
  ThreadsLeft is ThreadCount-1.
count_thread_answer(the(X),ThreadCount,ThreadCount,Xs,[X|Xs]).
 
%% multi_first(XGs,X): runs list of goals XGs of the form Xs:-G until a first answer X is found

multi_first(XGs,X):-multi_first(1,XGs,[X]). 

%% multi_first(K,XGs,Xs): runs list of goals XGs of the form Xs:-G until the first K answers Xs are found - or fewer if less then K
 
multi_first(K,XGs,Xs):-
  hub(Hub),
  length(XGs,ThreadCount),
  launch_logic_threads(XGs,Hub),
  collect_first_results(K,ThreadCount,Hub,Xs),
  stop_interactor(Hub).

collect_first_results(_,0,_Hub,[]).
collect_first_results(0,_,Hub,[]):-stop_interactor(Hub).
collect_first_results(K,ThreadCount,Hub,MoreXs):-K>0,ThreadCount>0,
  ask_interactor(Hub,Answer),
  count_thread_answer(Answer,ThreadCount,ThreadsLeft,Xs,MoreXs),
  (ThreadCount=:=ThreadsLeft->K1 is K-1;K1 is K),
  collect_first_results(K1,ThreadsLeft,Hub,Xs).  
  

%% multi_findall(XGs,Xss): for each (X:-G) it runs in parallel a findall(X,G,Xs) goal and collects a list of list of answers Xss  
multi_findall(XGs,Xss):-
  mark_answer_patterns(XGs,MarkedXGs,0),
  multi_all(MarkedXGs,MXs),
  collect_marked_answers(MXs,Xss).
 
mark_answer_patterns([],[],_). 
mark_answer_patterns([(X:-G)|XGs],[(N-X:-G)|MarkedXGs],N):-
  N1 is N+1,
  mark_answer_patterns(XGs,MarkedXGs,N1).

collect_marked_answers(MXs,Xss):-   
  findall(Xs,keygroup(MXs,_,Xs),Xss).


%% multi_fold(F,XGs,Xs): runs list of goals XGs of the form Xs:-G and collects all answers to a list
multi_fold(F,XGs,Final):-
  hub(Hub),
  length(XGs,ThreadCount),
  launch_logic_threads(XGs,Hub),
  ask_interactor(Hub,Answer),
  (Answer=the(Init)->fold_thread_results(ThreadCount,Hub,F,Init,Final);true),
  stop_interactor(Hub),
  Answer=the(_).

launch_logic_threads([],_Hub).
launch_logic_threads([(X:-G)|Gs],Hub):-
  Clone=0,Source=no,
  new_logic_thread(Hub,X,G,Clone,Source),
  %new_logic_thread(Hub,X,G),
  launch_logic_threads(Gs,Hub).

fold_thread_results(0,_Hub,_F,Best,Best).
fold_thread_results(ThreadCount,Hub,F,SoFar,Best):-ThreadCount>0,
  ask_interactor(Hub,Answer),
  count_thread_answer(Answer,ThreadCount,ThreadsLeft,F,SoFar,Better),
  fold_thread_results(ThreadsLeft,Hub,F,Better,Best).  
  
count_thread_answer(no,ThreadCount,ThreadsLeft,_F,SoFar,SoFar):-
  ThreadsLeft is ThreadCount-1.
count_thread_answer(the(X),ThreadCount,ThreadCount,F,SoFar,Better):-
  call(F,X,SoFar,Better).

%% multi_all(XGs,Xs): runs list of goals XGs of the form Xs:-G ant collects all answers to a list
alt_multi_all(XGs,Rs):-multi_fold(list_cons,[([]:-true)|XGs],Xs),reverse(Xs,Rs).

%% multi_best(F,XGs,M): runs list of goals XGs of the form N:-G where N is a number and extracts the max M
multi_best(XGs,R):-multi_fold(max,XGs,R).

list_cons(X,Xs,[X|Xs]).
list_head([X|_],X).
list_tail([_|Xs],Xs).  


% misc
  
call_at(H,XGs,X):-
  length(XGs,L),
  new_ctr(Ctr),
  foreach(
    member((X:-G),XGs),
    new_logic_thread(H,X,G)
  ),
  repeat,
    get(H,A),
    ( A=no,ctr_inc(Ctr),ctr_get(Ctr,L),!,fail
    ; A=the(X)
    ).
    
% end
