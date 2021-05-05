% xrefl.pl

% REFLECTION API - see xbuiltins.pl

/*
new_java_class(Name,Cls): 
  given a Name (Prolog constant), returns a handle to a Java class Cls 
  note that primitive types like int, double etc. also map to classes as given by their corresponding
  <ObjectType>.TYPE variables (of type Class).

new_java_object(Cls,Args,Obj): 
  returns Obj, an instance of class Cls by calling a constructor with Args. 
  If Args=none, a constructor with no arguments is called, if Args=args(A1,A2,...An) 
  then a matching constructor with arguments A1,A2...An is called, after appropriate conversions
   
invoke_java_method(Cls,Obj,MethName,Args,Answer): 
  Invokes on Obj a method named MethName of class or interface Cls with Args and unifies
  Answer with the value returned by the method (which can be one the special constant '$null'). 
  If Args=args(A1,A2,...An) then a matching method with arguments A1,A2...An is called, after
  appropriate conversions. 

get_java_field_handle(Obj,FieldName,FieldHandle): Given an object Obj and a FieldName
  it returns a FieldHandle (a handle to a java.lang.reflect.Field object) on which various
  get and set methods can then be invoked 
     
*/


     
% DERIVED OPERATIONS

/*
  add new object using convenient <className>(A1,...An) syntax
*/

%% new_java_object(ClassAndArgs,Object): makes new Java Object from Class
new_java_object(Class,Object):-
  other_object(Class), % actual type is Class in this case
  !,
  new_java_object(Class,void,Object).
new_java_object(ClassName,Object):-
  atom(ClassName),
  !,
  new_java_class(ClassName,Class),
  new_java_object(Class,void,Object).
new_java_object(ClassAndArgs,Object):-
  functor(ClassAndArgs,ClassName,_),
  new_java_class(ClassName,Class),
  new_java_object(Class,ClassAndArgs,Object).

invoke_java_method(ObjectOrClass,MethodName,Args,Result):-
  invoke_java_method(guess_the_class,ObjectOrClass,MethodName,Args,Result).

%% invoke_java_method(ObjectOrClass,MethodName,Result) : calls Java method using <methodName>(A1,...An) syntax 
invoke_java_method(ObjectOrClass,MethodName,Result):-
  atom(MethodName),!,
  invoke_java_method(ObjectOrClass,MethodName,void,Result).
invoke_java_method(ObjectOrClass,MethodAndArgs,Result):-
  functor(MethodAndArgs,MethodName,_),
  invoke_java_method(ObjectOrClass,MethodName,MethodAndArgs,Result).

%% call_java_class_method(ClassName,MethodAndArgs,Result): calls static Java method using <methodName>(A1,...An) syntax
call_java_class_method(ClassName,MethodAndArgs,Result):-
  new_java_class(ClassName,Class),
  invoke_java_method(Class,MethodAndArgs,Result).

/*
  creates a new object using a class_name+args template
  to which it applies a method_args template
  note that the object as such is lost
*/
call_java_instance_method(ClassNameAndArgs,MethodAndArgs,Result):-
  new_java_object(ClassNameAndArgs,Object),
  invoke_java_method(Object,MethodAndArgs,Result).
    
%% get_java_field(Object,FieldName,Val): get the content Val of a (public) Java field
get_java_field(Object,FieldName,Val):-
  get_java_field_handle(Object,FieldName,Handle),
  invoke_java_method(Handle,get(Object),Val).

%% get_java_class_field(ClassName,FieldName,Val): gets the content Val of a (public) static Java field
get_java_class_field(ClassName,FieldName,Val):-
  new_java_class(ClassName,Class),
  get_java_field(Class,FieldName,Val).

%% set_java_field(Object,FieldName,NewVal) : sets the content of a (public) Java field to NewVal
set_java_field(Object,FieldName,NewVal):-
  get_java_field_handle(Object,FieldName,Handle),
  invoke_java_method(Handle,set(Object,NewVal),_).

%% set_java_class_field(ClassName,FieldName,NewVal): sets a static field of a named class to NewVal
set_java_class_field(ClassName,FieldName,NewVal):-
  new_java_class(ClassName,Class),
  set_java_field(Class,FieldName,NewVal).


%% invoke_array_method(MethodAndArgs,R): calls static Array class methods
invoke_array_method(MethodAndArgs,R):-
   call_java_class_method('java.lang.reflect.Array',
     MethodAndArgs,R).

%% new_array(ClassName,Dim,Array): makes a new array to hold Dim elements from class ClassName
new_array(ClassName,Dim,Array):-
  integer(Dim),
  !,
  new_ndim_array(ClassName,[Dim],Array).
new_array(ClassName,[Dim|Dims],Array):-  
  new_ndim_array(ClassName,[Dim|Dims],Array).

new_ndim_array(ClassOrName,Dims,Array):-
  atomic(ClassOrName),
  !,
  new_java_class(ClassOrName,Class),
  new_ndim_array_of_class(Class,Dims,Array).
new_ndim_array(Class,Dims,Array):-
  new_ndim_array_of_class(Class,Dims,Array).
  
new_ndim_array_of_class(Class,Dims,Array):-
  new_java_object('vm.logic.IntStack',Stack),
  foreach(
    member(Dim,Dims),
    invoke_java_method(Stack,push(Dim),_)
  ),
  invoke_java_method(Stack,toArray,Ints),
  invoke_array_method(newInstance(Class,Ints),R),
  delete_java_class(Class),
  delete_java_object(Stack),
  delete_java_object(Ints),
  R=Array.



arrtest:-
  new_array('java.lang.Object',3,A),
  array_set(A,0,hello),
  array_set(A,1,3.14),
  array_set(A,2,bye),
  for(I,0,2),
  array_get(A,I,X),
  println(X),
  fail.
arrtest:-
  println(done).  
  




/* moved to Java now part of new_java_class
new_java_primitive_class(Name,Class):-
  primitive_type(Name,PName),
  get_java_class_field(PName,'TYPE',Class).

primitive_type(boolean,'Boolean.TYPE').
primitive_type(char,'Character.TYPE').
primitive_type(byte,'Byte.TYPE').
primitive_type(short,'Short.TYPE').
primitive_type(int,'Integer.TYPE').
primitive_type(long,'Long.TYPE').
primitive_type(float,'Float.TYPE').
primitive_type(double,'Double.TYPE').
primitive_type(void,'Void.TYPE').
*/


%% array_set(A,I,E): sets element I of array A
array_set(A,I,E):-invoke_array_method(set(A,I,E),_).

%% array_get(A,I,R): gets element I of array A
array_get(A,I,R):-invoke_array_method(get(A,I),R).

/* gets element of an N-dim array - index given as a list */
array_nget(A,[I|Is],R):-array_nget(Is,I,A,R).

array_nget([],I,A,R):-array_get(A,I,R).
array_nget([J|Is],I,A,R):-array_get(A,I,B),array_nget(Is,J,B,R).

/* sets element of an N-dim array - index given as a list */
array_nset(A,[I|Is],R):-array_nset(Is,I,A,R).

array_nset([],I,A,V):-array_set(A,I,V).
array_nset([J|Is],I,A,R):-array_get(A,I,B),array_nset(Is,J,B,R).

%% array_size(A,L): returns the number of elements of an array
array_size(A,L):-invoke_array_method(getLength(A),L).

%% list_to_array(Xs,A): converts a list to an array

list_to_array(Xs,A):-list_to_array('java.lang.Object',Xs,A).

list_to_array(ClassOrName,Xs,A):-
  length(Xs,Dim),
  new_array(ClassOrName,Dim,A),
  foreach(nth0(I,Xs,X),array_set(A,I,X)).

/*
  converts a list of lists to an array
*/
nlist_to_array(Xsx,A):-nlist_to_array('java.lang.Object',Xsx,A).

% limited to 2-dim arrays, for now
nlist_to_array(ClassOrName,Xsx,A):-
  length(Xsx,D0),
  get_dim(Xsx,D1),
  new_array(ClassOrName,[D0,D1],A),
  ( nth0(I,Xs,Xsx),
      nth0(J,X,Xs),
        array_nset(A,[I,J],X),
    fail
  ; true
  ).
  
get_dim(Xss,Dim):-
 maplist(length,Xss,Ls),sort(Ls,Ns),
 (Ns=[SameD]->Dim=SameD;errmes(get_dim_error,have_different_length(Ls))).

%% array_to_list(A,Is): converts an array to a list
array_to_list(A,Is):-array_size(A,Max),array_to_list(0,Max,A,Is).

array_to_list(Max,Max,_,[]).
array_to_list(I,Max,A,[X|Xs]):-I<Max,I1 is I+1,array_get(A,I,X),array_to_list(I1,Max,A,Xs).

array_to_nlist(A,Xsx):-array_to_nlist(A,2,Xsx).

/* converts an array to a list of lists of ... */
array_to_nlist(A,Dim,Xsx):-array_to_nlists(Dim,A,Xsx).

array_to_nlists(0,X,X).
array_to_nlists(N,A,Xss):-N>0,N1 is N-1,array_to_list(A,Xs),maplist(array_to_nlists(N1),Xs,Xss).

/* makes the use of booleans returned by Java easier */

%% is_true(B): checks if a Boolean returned by Java represents true
is_true(B):-invoke_java_method(B,toString,T),T='true'.

%% is_false(B): checks if a Boolean returned by Java represents false
is_false(B):-invoke_java_method(B,toString,F),F='false'.

bool2int(B,I):-invoke_java_method(B,toString,V),bval2int(V,I).

bval2int(false,0).
bval2int(true,1).

%% to_boolean(S,B): converts their string representation to booleans true, false
to_boolean(S,B):-new_java_object('java.lang.Boolean'(S),B).
  
%% get_java_class(O,Class): get the a handle to the class to which object O is an instance
get_java_class(O,Class):-invoke_java_method(O,getClass,Class).

delete_java_object(_).
  
delete_java_class(_).

delete_java_objects(_).  
  

%% next_of(NextIterator,E): generic iterator interface - assuming it implements next() returning null at end
next_of(NextIterator,E):-next_of(NextIterator,next,E).

% more general - Advancer is any "next()" method
next_of(NextIterator,Advancer,E):-
  repeat,              
     ( invoke_java_method(NextIterator,Advancer,E),
       E\==no,not_null(E)->true
       ; !,delete_java_object(NextIterator),fail
     ).

% reflection of specific Prolog runtime objects


%% predicate_property(Pred,Prop): true if Pred=F(X1...Xn) has property Prop
predicate_property(Pred,Prop):-nonvar(Pred),!,get_predicate_property(Pred,Prop).
predicate_property(Pred,Prop):-
  current_predicate(F/N),
  fun(F,N,Pred),
  get_predicate_property(Pred,Prop).
  
get_predicate_property(Pred,Prop):-is_compiled(Pred),
  P=(static),(nonvar(Prop),Prop=P->!;Prop=P).
get_predicate_property(Pred,Prop):-
  is_compiled(Pred),functor(Pred,F,N),
  P=primitive,(nonvar(Prop),Prop=P->!;Prop=P),
  once(current_builtin(F/N)).
get_predicate_property(Pred,Prop):-
  is_compiled(Pred),arg(0,Pred,F),arity(Pred,N),
  P=xbuiltin(I),(nonvar(Prop),Prop=P->!;Prop=P),
  once(is_xbuiltin(I,F/N)).
get_predicate_property(Pred,Prop):-
  is_compiled(Pred),arg(0,Pred,F),atom(F),
  P=built_in,(nonvar(Prop),Prop=P->!;Prop=P),
  once(current_system_symbol(F)).
get_predicate_property(Pred,Prop):-
  is_compiled(Pred),arg(0,Pred,F),atom(F),
  P=user_predicate,(nonvar(Prop),Prop=P->!;Prop=P),
  once(current_user_symbol(F)).
get_predicate_property(Pred,Prop):-
  is_dynamic(Pred),
  member(Prop,[interpreted,user_predicate]).
 
syspredcount:-
  findall(F/N,(predicate_property(X,built_in),
  functor(X,F,N)),Xs),sort(Xs,Ys),length(Xs,L),length(Ys,SL),println(L+SL),fail
; nl.

userpredcount:-
  findall(F/N,(predicate_property(X,user_predicate),
  functor(X,F,N)),Xs),sort(Xs,Ys),length(Xs,L),length(Ys,SL),println(L+SL),fail
; nl.
  
current_builtin(F/N1):-
  bbuiltin(H,_,_),
  arity(H,N),N1 is N-1,
  arg(0,H,F).

current_symbols(Xs):-findall(X,current_symbol(X),Xs).

%% current_atom(A): backtracks over all atoms (warning: it also keeps them alive!)
current_atom(A):-current_symbol(A),atom(A).

%% current_non_atom(A): backtracks over all unusual symbols - engines, bignums, other Java objects etc.
current_non_atom(S):-current_symbol(S),\+atom(S).

%% current_compiled(F/N): backtracks over all compiled predicates
current_compiled(F/N):-current_atom(F),for(N,1,32),fun(F,N,H),is_compiled(H).
    

% not really needed anymore
save_xbs:-
  tell('../psrc/xbs.pl'),
  findall(B,find_xbuiltin(B),Bs),
  sort(Bs,Us),
  foreach(member(U,Us),
    (write_canonical(U),write('.'),nl)
  ),
  told.
    
find_xbuiltin(is_xbuiltin(I,F/N)):-
  term_of('../psrc/xbuiltins.pl',(H:-Bs)),
  ( Bs=(B,_)->true
  ; B=Bs
  ),
  arg(0,B,xcall),
  arg(1,B,I),
  arg(0,H,F),
  arity(H,N).
  
 

hashCode(O,H):-invoke_java_method(O,hashCode,H).
  
  
class_of(O,C):-invoke_java_method(O,getClass,C).
 
   