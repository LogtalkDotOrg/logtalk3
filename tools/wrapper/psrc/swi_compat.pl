
      
 % for Lean Prolog compatibility:
   
for(I,1,N):-between(1,N,I).
   
%%%%%%%%%%%%%%%%%%%%% Emulation of Lean's reflection API %%%%%%%%%%%%%%%%%%%

new_java_class(Name,Cls):-jpl_classname_to_type(Name,Cls).
 
new_java_object(Cls,Args,Obj):-
  Args=..[_|Params],
  jpl_new(Cls, Params, Obj).

new_java_object(ClassAndArgs,Object):-
  functor(ClassAndArgs,ClassName,_),
  new_java_class(ClassName,Class),
  new_java_object(Class,ClassAndArgs,Object).
  
invoke_java_method(ObjectOrClass,MethodName,Args,Result):-
  Args=..[_|Params],
  jpl_call(ObjectOrClass, MethodName, Params, Result).

call_java_class_method(ClassName,MethodAndArgs,Result):-
  new_java_class(ClassName,Class),
  invoke_java_method(Class,MethodAndArgs,Result).
 
 
invoke_java_method(ObjectOrClass,MethodName,Result):-
  atom(MethodName),!,
  invoke_java_method(ObjectOrClass,MethodName,[],Result).
invoke_java_method(ObjectOrClass,MethodAndArgs,Result):-
  functor(MethodAndArgs,MethodName,_),
  invoke_java_method(ObjectOrClass,MethodName,MethodAndArgs,Result).

call_java_instance_method(ClassNameAndArgs,MethodAndArgs,Result):-
  new_java_object(ClassNameAndArgs,Object),
  invoke_java_method(Object,MethodAndArgs,Result).
 
get_java_field(Object,FieldName,Value) :-
  jpl_get(Object, FieldName, Value).
  
set_java_field(Object,FieldName,Value) :-
  jpl_set(Object, FieldName, Value). 
  

%% set_java_class_field(ClassName,FieldName,NewVal): sets a static field of a named class to NewVal
set_java_class_field(ClassName,FieldName,NewVal):-
  new_java_class(ClassName,Class),
  set_java_field(Class,FieldName,NewVal).
    
/* makes the use of booleans returned by Java easier */

%% is_true(B): checks if a Boolean returned by Java represents true
is_true(B):-invoke_java_method(B,toString,T),T='true'.

%% is_false(B): checks if a Boolean returned by Java represents false
is_false(B):-invoke_java_method(B,toString,F),F='false'.

bool2int(B,I):-invoke_java_method(B,toString,V),bval2int(V,I).

bval2int(false,0).
bval2int(true,1).

to_boolean(S,B):-new_java_object('java.lang.Boolean'(S),B).
  
get_java_class(O,Class):-jpl_object_to_class(O,Class).

delete_java_object(_).
  
delete_java_class(_).

delete_java_objects(_). 

box_int(Int,Integer):-new_java_object('java.lang.Integer',arg(Int),Integer).
 
unbox_int(Integer,Int):-invoke_java_method(Integer,intValue,Int).
  
box_float(Plain,Boxed):-new_java_object('java.lang.Double',arg(Plain),Boxed).
 
unbox_float(Boxed,Plain):-invoke_java_method(Boxed,doubleValue,Plain).

  
new_array(Size,R):-
  call_java_class_method('swi.Swi',new_array(Size),R).
  
new_array(Type,Dim,A):-
  jpl_primitive_type(Type),
  !,
  jpl_new(array(Type),Dim,A).
new_array(ClassName,Dim,A):-integer(Dim),!,
  jpl_classname_to_class(ClassName,C),
  jpl_call('java.lang.reflect.Array',newInstance,[C,Dim],A).
list_to_array(Xs,A):-jpl_list_to_array(Xs,A).
 
array_set(A,I,X):-
  call_java_class_method('swi.Swi',array_set(A,I,X),_).
 
array_get(A,I,X):-
  call_java_class_method('swi.Swi',array_get(A,I),X).
   
array_to_list(A,Xs):-jpl_array_to_list(A,Xs). 

list_to_array(ClassName,Xs,A):-
  length(Xs,Dim),
  new_array(ClassName,Dim,A),
   (nth0(I, Xs, X),
       jpl_set(A, I, X),
    fail
    ; true
   ).
  
%%%%%%%%%%%%% reflection tests %%%%%%%%%%%
 
arrtest1:-
  %new_array(3,A),
  new_array('java.lang.String',2,A),
  array_set(A,0,hello),
  array_set(A,1,bye),
  array_get(A,0,X),writeln(X),
  array_get(A,1,Y),writeln(Y),
  writeln(done).  
    
arrtest:-
  new_array(3,A),
  array_set(A,0,hello),
  box_float(3.14,BF),
  array_set(A,1,BF),
  array_set(A,2,bye),
  array_get(A,0,X),writeln(X),
  array_get(A,1,Y),write(Y),unbox_float(Y,Y0),write('=>'),writeln(Y0),
  array_get(A,2,Z),writeln(Z),
  writeln(done).  
   
 jfine:-
   new_java_object('java.util.HashMap',M),
   invoke_java_method(M,put(milk,good),_),
   invoke_java_method(M,get(milk),R),
   writeln(got=R),
   fail
 ; true.  

 jbug:-
   new_java_object('java.util.HashMap',M),
   invoke_java_method(M,put(milk,very(good)),_),
   invoke_java_method(M,get(milk),R),
   writeln(got=R),
   fail
 ; true.  
 
 
 %%%% SWI Prolog Lean compatibility layer
 
 %% new_ctr(Ctr): creates a new couter initialized at 0
new_ctr(s(0)).

%% ctr_get(Ctr,X): gets current value X of Ctr
ctr_get(s(X),X).

%% ctr_add(Ctr,X):  adds value X to Ctr
ctr_add(Ctr,N):-arg(1,Ctr,V1),V2 is V1+N,change_arg(1,Ctr,V2).

%% ctr_inc(Ctr,X): increments Ctr
ctr_inc(Ctr):-ctr_add(Ctr,1).
%% ctr_dec(Ctr,X): decrements Ctr
ctr_dec(Ctr):-X is -1,ctr_add(Ctr,X).

change_arg(I,T,X):-nb_setarg(I,T,X).

   
 println(X):-writeln(X).
   
   
 %% gvar(V): backtracks over names of all global vars V
gvar(V):-nb_current(V,_),!.

gvars(Vs):-findall(V,gvar(V),Vs).


gvars:-nb_current(V,X),writeln('=>'(V,X)),fail;true.

%% Var <== Val: sets a global variable
'<=='(Var,Val):-gvar_set(Var,Val).

%% Var ==> Val: gets the value of a global variable 
'==>'(Var,Val):-gvar_get(Var,Val).

gvar_set(G,T) :- nb_setval(G,T).
 
gvar_get(G,T) :- nb_getval(G,T).
 
gvar_remove(G):-nb_delete(G).
 


%% Sym <= Val: sets a thread-local variable Sym to Val
'<='(Var,Val):-set_val(Var,Val).

%% Sym => Val: gets value Val of a thread-local variable Sym
'=>'(Var,Val):-get_val(Var,Val).
    
get_val(S,V):-nb_getval(S,V).

set_val(S,V):-nb_setval(S,V).

%% remove_val(Sym): removes attribute of symbol Sym
remove_val(S):-nb_delete(S).



 match_before(Stop,Cs)-->match_before([Stop],Cs,_).

match_before(Stops,[],Stop)-->[Stop],{member(Stop,Stops)},!.
match_before(Stops,[C|Cs],Stop)-->[C],match_before(Stops,Cs,Stop).
 
 
%% words2nat(Ws,Ns): interperses words with spaces
words2nat(Ws,Ns):-
  words2nat(Wss,Ws,[]),append(Wss,Vs),once(append(Us,[S],Vs)),(' '=S->Ns=Us;Ns=Vs).

words2nat([])-->[].
words2nat([[W,C,' ']|Wss])-->[W,C],{left_collider(C)},!,words2nat(Wss).
words2nat([[L,C]|Wss])-->[L,C],{collider(C)},!,words2nat(Wss).
%words2nat([[Q,W,Q,' ']|Wss])-->[Q],{Q=('"')},[W],[Q],!,words2nat(Wss).
words2nat([[Q|Vs],[Q]|Wss])-->[Q],{Q=('"')},match_before(Q,Ws),!,
  {words2nat(Ws,Vs)},words2nat(Wss).
words2nat([[W,' ']|Wss])-->[W],words2nat(Wss).
 
  
left_collider(W):-member(W,[(','),(':'),(';'),('.'),('!'),('?')]).

collider(W):-member(W,[(''''),('-')]).



%% words_code(Ws,C): backtracks over all char codes in a list of words
words_code(Ws,C):-words2nat(Ws,Ns),member(N,Ns),name(N,Xs),member(C,Xs).

%% words2sentence(Ws,Sentence): combines words with spaces as needed for a better looking natural language sentence
words2sentence(Ws,Sentence):-
 words2nat(Ws,Ns),
 atomic_list_concat(Ns,Sentence).

%% take(N,Xs,Ys): takes the first N elements Ys of list Xs
take(N,Xs,Ys):-split_at(N,Xs,Ys,_).

%% split_at(N,Xs,Ys,Zs): splits into first N elements Ys of list Xs and other elements Zs (if any) 
split_at(N,[X|Xs],[X|Ys],Zs):-N>0,!,N1 is N-1,split_at(N1,Xs,Ys,Zs).
split_at(_,Xs,[],Xs).

%% drop(N,Xs,Ys): drops the first N elements of list Xs and returns the left over elements Ys
drop(N,[_|Xs],Ys):-N>0,!,N1 is N-1,drop(N1,Xs,Ys).
drop(_,Ys,Ys).
 
call_ifdef(P,_):-predicate_property(P,visible),!,call(P).
call_ifdef(_,AltP)-call(AltP).

appendN(Xss,Xs):-append(Xss,Xs).

det_append(Xs,Ys,Rs):-append(Xs,Ys,Zs),!,Rs=Zs.

map(F,Xs,Ys):-maplist(F,Xs,Ys).
  
to_lower_char(C,LC):- [A,Z,LA]="AZa",C>=A,C=<Z,!, LC is (LA-A)+C.
to_lower_char(C,C).

to_upper_char(LC,C):- [LA,LZ,A]="azA",LC>=LA,LC=<LZ,!, C is (A-LA)+LC.
to_upper_char(LC,LC).

to_upper_chars([],[]).
to_upper_chars([X|Xs],[Y|Ys]):-
  to_upper_char(X,Y),
  to_upper_chars(Xs,Ys).

to_lower_chars([],[]).
to_lower_chars([X|Xs],[Y|Ys]):-
  to_lower_char(X,Y),
  to_lower_chars(Xs,Ys).
    
namecat(A,B,C,D):-atomic_list_concat([A,B,C],D).


if(X,Y,Z):-X->Y;Z.

if_any(X,Y,Z):- X *-> Y ; Z.
    
ctime(T):-T is cputime*1000.
 
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
  
 
keygroup(Ps,K,Xs):-
   keysort(Ps,Ss),
   group_pairs_by_key(Ss, KXs),
   member(K-Xs,KXs).
 
file2string(F,Cs):-
  read_file_to_codes(F,Cs,[]).
  
/*
% defined in swi_compiler.pl
find_prolog_file(FileDescr,NewFile):-
  SuffixesCss=["",".pl",".pro",".wam"],
  find_file(FileDescr,SuffixesCss,NewFile).
*/
  
find_file(FileDescr,NewFile):-
  SuffixesCss=["",".pl",".pro"],
  find_file(FileDescr,SuffixesCss,NewFile).


get_path([]). % Pss):-path_elements(Ps),maplist(atom_codes,Ps,Pss).

path_element(P):-fail,path_elements(Ps),member(P,Ps).

find_file(FileDescr,_Sufs,NewFile):-FileDescr='$str'(_),!,NewFile=FileDescr.
find_file(FileDescr,Sufs,NewFile):-
  find_file0(FileDescr,Sufs,NewFile),
  !.
find_file(FileDescr,_Sufs,_NewFile):-	
  errmes(no_such_file_not_found,FileDescr).
   
find_file0(FileDescr,Sufs,NewFile):-
   get_path(Ps),
   append(["","progs"],Ps,Prefs),
   find_file0(Prefs,FileDescr,Sufs,NewFile).

%% find_file(Prefs,File,Sufs,NewFile): finds File with name prefixed and suffixed as NewFile
    
find_file0(_,File,_,NewFile):-
  absolute_file_name(File,AFile),
  exists_file(AFile),
  !,
  NewFile=AFile.
find_file0(Prefs,File,Sufs,NewFile):-
  atom_codes(File,Fs),
  member(Suf,Sufs),
  member(Pref0,Prefs),
  expand_pref(Pref0,Pref),
  append([Pref,Fs,Suf],Cs),
  atom_codes(NewFile0,Cs),
  % traceln(fileCandidate(NewFile)),
  absolute_file_name(NewFile0,NewFile1),
  exists_file(NewFile1),
  !,
  NewFile=NewFile1.

find_file(Prefs,File,Sufs,NewFile):-
  find_file0(Prefs,File,Sufs,NewFile),
  !. 
find_file(_Prefs,File,_Sufs,_NewFile):-
  errmes(file_not_found,File).
  
expand_pref([],Cs):-!,Cs=[].
expand_pref(Cs,NewCs):-nonvar(Cs),append(Cs,"/",NewCs).







    