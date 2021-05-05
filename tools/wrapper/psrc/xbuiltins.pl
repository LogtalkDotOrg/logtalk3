% xbuiltins.pl

% ibuiltins

hkey(T,N):-hkey(4096,T,N).

%% hkey(Mod,T,N): computes for nonvar term T a shallow hash key N in 0..Mod-1
hkey(Mod,T,N):-icall_op(0,Mod,T, N).

det_append(Xs,Ys,Zs):-icall_op(6,Xs,Ys, Zs).



%% engine_set_name(Name): gives a name to this engine
engine_set_name(Name):-icall(5,Name,_).

%% engine_get_name(Name): returns the name of this engine
engine_get_name(Name):-icall(6,_,Name).


term2bundle(Term,Bundle):-icall(7,Term,Bundle).
 
bundle2term(Bundle,Term):-icall(8,Bundle,Term).


bundle_op(Op,I,O):- icall_op(7,Op,I,O).

/*
bundle_op0(Op,I,R):-
  export_term(I),
  icall_op(8,Op,_,_),
  import_term(R).
*/
  
%term_hash(T,N):-hkey(T,N).

term_hash(T,N):-icall(2,T,N).

%% term_variables(Term,Vars) :: extracts set of free variables occurring in Term
term_variables(T,Vs):-icall(3,T,Vs).

%% dtime(T): returns elapsed time since last call to it
dtime(T):-icall(4,_,T).

swap_arg(I,T,New,Old):-icall_impure(0,I,T,New, Old).

%% nb_setarg(I,T,New): replaces distructively _constant_ arg I of T with New.
nb_setarg(I,T,New):-change_arg(I,T,New).

%% change_arg(I,T,New): replaces distructively _constant_ arg I of T with New.
change_arg(I,T,New):-icall_impure(0,I,T,New, _Old).

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

% prototype of new simple builtin: Op=0
new_builtin0(In,Out):-icall(0,In,Out).

% also: copy_term

xlen(Xs,L):-nonvar(Xs),new_ctr(Ctr),xlen_loop(Xs,Ctr),ctr_get(Ctr,L).

xlen_loop(Xs,Ctr):-member(_,Xs),ctr_inc(Ctr),fail.
xlen_loop(_,_).

xcall(Op,Input,Output):-xcall0(Op,Input,Output0),xcall_postprocess(Input,Output0,Output).

xcall_postprocess(Input,exception(I,E),Output):-is_xbuiltin(I,F/_),!,
  Input=..[_|Args0],
  % append(Args0,[Output],Args),
  !,
  T=..[F|Args0],
  throw_if_error(E,T,Output).
xcall_postprocess(_,Output,Output).

%throw_if_error(null_output,T,'$null'):-!,traceln(null_output_in(T)).
throw_if_error(E,T,_):-throw(error_in_xcall(T,E)),fail.

% xcall assumes first arg constant !!!

%% -(A,B): computes the opposite value B of A, B is -A
-(A,B):-xcall(0,i(A),B).

% abs(-(A),B):-!,xcall(1,i(A),B).

%% abs(A,B): computes absolute value B of A
abs(A,B):-xcall(1,i(A),B).

%% sign(N,S): computes signum of N (-1,0,1) as in S is sign(N)
sign(0,R):-!,R=0.
sign(X,R):-abs(X,X),!,R=1.
sign(_,R):- R is -1.

ncompare(A,B,C):-icall_op(2,A,B,R),!,C=R.
ncompare(X,Y,C):-xcall(2,i(X,Y),C).

%% ensure_number(Num,MaybeBignum): converts, if needed string or $number(N) to BigInteger or BigDecimal
ensure_number(Num,MaybeBignum):-xcall(3,i(Num,0),MaybeBignum). % calls N+0 - forces conversions

+(A,B,C):-icall_op(3,A,B,R),!,C=R.
+(A,B,C):-xcall(3,i(A,B),C).

-(A,B,C):-icall_op(4,A,B,R),!,C=R.
-(A,B,C):-xcall(4,i(A,B),C).

*(A,B,C):-icall_op(5,A,B,R),!,C=R.
*(A,B,C):-xcall(5,i(A,B),C).

pow(A,B,C):-xcall(6,i(A,B),C).
min(A,B,C):-xcall(7,i(A,B),C).
max(A,B,C):-xcall(8,i(A,B),C).
'/'(A,B,C):-xcall(9,i(A,B),C).

get_lowest_set_bit(A,C):-xcall(10,i(0,A),C).
bitcount(A,C):-xcall(10,i(1,A),C).
'<<'(A,B,C):-xcall(11,i(A,B),C).
'>>'(A,B,C):-xcall(12,i(A,B),C).
getbit(A,B,C):-xcall(13,i(A,B),C).
setbit(A,B,C,D):-xcall(14,i(A,B,C),D).
flipbit(A,B,C):-xcall(15,i(A,B),C).

% SWI compatible

%% msb(N,B): B is the index of highest set bit of N>0
msb(A,B):-A>0,bitcount(A,C),B is C-1.

%% lsb(N,B): B is the index of the lowest set bit of N>0
lsb(A,B):-A>0,get_lowest_set_bit(A,B).

ite(X,Y,Z,R):-xcall(16,i(X,Y,Z),R).
\(X,R):-xcall(17,i(X),R).

/\(A,B,R):-xcall(18,i(A,B),R).
\/(A,B,R):-xcall(19,i(A,B),R).
'xor'(A,B,R):-xcall(20,i(A,B),R).
'eq'(A,B,R):-xcall(21,i(A,B),R).
'impl'(A,B,R):-xcall(22,i(A,B),R).
'less'(A,B,R):-xcall(23,i(A,B),R).


div(A,B,C):-xcall(24,i(A,B),C).
mod(A,B,C):-xcall(25,i(A,B),C).
gcd(A,B,C):-xcall(26,i(A,B),C).
modinv(A,B,C):-xcall(27,i(A,B),C).

even(X):- mod(X,2,0).

odd(X) :- mod(X,2,1).

^(X,Y,Z):-pow(X,Y,Z).
'**'(X,Y,Z):-pow(X,Y,Z).

sqrt(X,Y):-E is 1/2,pow(X,E,Y).

'//'(A,B,C):-div(A,B,C).


% TODO: add arithmetic ops in XBuiltins

gc:-xcall(28,i(0),_).

%% symgc: force symbol garbage collection now
symgc:-xcall(28,i(1),_).

%% symgc_off: turn off automatic symbol grabage collection
symgc_off:-xcall(28,i(2),R),traceln(symgc(was(R),now(0))).

%% symgc_on: turn on automatic symbol grabage collection
symgc_on:-xcall(28,i(3),R),traceln(symgc(was(R),now(1))).

%% set_symgc_force(Bits): forces symbol grabage collection if more than 2^Bits syms are present
set_symgc_force(Bits):-Bits>22,Bits<30,X is 1<<Bits, %Paul was smaller for 32 bits
  call_java_class_method('vm.logic.AtomTable',set_symgc_force(X),_).

to_codes(A,B):- xcall(29,i(A),B).
from_codes(A,B):-new_fun1('$string',A,S),xcall(30,i(S),B).

new_fun1(F,X,FX):-fun(F,1,FX),arg(1,FX,X).
new_fun2(F,X,Y,FXY):-fun(F,2,FXY),arg(1,FXY,X),arg(2,FXY,Y).

to_ncodes(A,B):-xcall(31,i(A),B).
from_ncodes(A,B):-new_fun1('$string',A,S),xcall(32,i(S),B). % ok

halt(Code):-xcall(33,i(Code),_).
halt:-halt(0).

exit:-return(done).

%% new_interactor(Class,Initializer,Handle): creates a new Interactor from and Intractor Class 
new_interactor(Class,Initializer,Handle):-
  % the method is assumed to be called new_interactor
  xcall(34,i(Class,Initializer),Handle).

%% ask_interactor(Interactor,Query): asks an Interactor a Query 
ask_interactor(A,C):-xcall(35,i(A),C).


tell_interactor(A,B,C):-xcall(36,i(A,B),C).

%% tell_interactor(Interactor,Task): tells Interactor about a Task to be done next time
%tell_interactor(A,B):-tell_interactor(A,B,R),R=yes,!. % old - sometimes is this
%tell_interactor(A,B):-tell_interactor(A,B,R),!,R=B. % new - sometimes is that
tell_interactor(A,B):-tell_interactor(A,B,_R),!. %,traceln(tell_interactor(A,B,R)).
tell_interactor(A,B):-errmes(tell_interactor_failed(A),B).

stop_interactor(A,C):-xcall(37,i(A),C).

%% stop_interactor(Interactor): stops an interactor
stop_interactor(A):-stop_interactor(A,_).

% statistics

%% cputime(T): computes time since beginning of seesion in ms
cputime(T):-xcall(38,i(0),T).
symbols(T):-xcall(38,i(1),T).
engines(T):-xcall(38,i(2),T).
heapsizes(H):-xcall(38,i(3),T),arg(1,T,H).
stacksizes(H):-xcall(38,i(3),T),arg(2,T,H).
trailsizes(H):-xcall(38,i(3),T),arg(3,T,H).
flags(T):-xcall(38,i(4),T).
%% max_memory(M): returns max memory in Mbytes available to this Java VM
max_memory(M):-xcall(38,i(5),M).
%% used_memory(M): returns amount of memory in Mbytes used so far by this Java VM
used_memory(M):-xcall(38,i(6),M).
%% available_memory(M): computes memory still available - when reaching it Java crashes with OutOfMemory exception
available_memory(F):-max_memory(M),used_memory(U),F is M-U.
thread_count(M):-xcall(38,i(7),M).
% db_size(Fun):-xcall(38,i(8),Fun).
xprofile(Zs):-xprofile(3,Zs).
xprofile(K,Zs):-xcall(38,i(9),XIs),keysort(XIs,Sorted),
  reverse(Sorted,Ys),
  take(K,Ys,Zs).
%% max_cores(M): returns the maximum numbers of CPUs available to this JVM as seen by the OS  
max_cores(M):-xcall(38,i(10),M).  

jcall(Class,StaticMethodAndArgs,Result):-
  xcall(39,i(Class,StaticMethodAndArgs),Result).


%% gvar_set(Var,Val): sets global Var to Val as in Var <== Val  
gvar_set(Var,Val):-xcall(40,i(Var,Val),_).
%% gvar_get(Var,Val): gets Val of global Var as in Var ==> Val
gvar_get(Var,Val):-xcall(41,i(Var),the(Val)).

%% gvar_remove(Var): removes a global variable
gvar_remove(Var):-xcall(42,i(Var),_).

%% gvars(Vs): gets list of all gvars
gvars(Vs):-findall(V,gvar(V),Vs).

%% gvars: lists all gvars and their values

gvars:-
  current_output(S),
  list_gvars(S).
  
list_gvars(S):-  
  gvars(Gs),
  println(S,'% global variables'),
  member(X,Gs),
  gvar_get(X,V),
  portray_clause(S,X==>V),
  fail.
list_gvars(S):-nl(S).

%% gvar(V): backtracks over names of all global vars V
gvar(V):-V==>_,!.
gvar(V):-xcall(109,ignore,Iterator),ielement_of(the(Iterator),V).

%% Var <== Val: sets a global variable
'<=='(Var,Val):-gvar_set(Var,Val).

%% Var ==> Val: gets the value of a global variable 
'==>'(Var,Val):-gvar_get(Var,Val).

get_val(S,V):-atomic(S),xcall(68,i(S),the(V)).
set_val(S,V):-atomic(S),nonvar(V),xcall(69,i(S,V),_OldVal).

%% remove_val(Sym): removes attribute of symbol Sym
remove_val(S):-atomic(S),xcall(70,i(S),_OldVal).

%% Sym <= Val: sets a thread-local variable Sym to Val
'<='(Var,Val):-set_val(Var,Val).

%% Sym => Val: gets value Val of a thread-local variable Sym
'=>'(Var,Val):-get_val(Var,Val).


%% gensym_no(Base,N): gets next N associated to symbol Base - local to symbol table
gensym_no(Base,N):-Base=>N,!,N1 is N+1,Base<=N1.
gensym_no(Base,0):-Base<=1.

gensym_last_no(Base,N):-Base=>N.

%% reset_gensym(Base): resets gensym asocciated to base
reset_gensym(Base):-remove_val(Base).

%% gensym(S,X): generates news symbols on ecah call X=S1,X=S2... etc
gensym(B,X):-gensym_no(B,N),atom_concat(B,N,X).


%% global_gensym_no(Base,N): gets next N associated to symbol Base - global to process
global_gensym_no(Base,N):-Base==>N,!,N1 is N+1,Base<==N1.
global_gensym_no(Base,0):-Base<==1.

global_gensym_last_no(Base,N):-Base==>N.

%% reset_global_gensym(Base): resets gensym associated to base - global to process
reset_global_gensym(Base):-gvar_remove(Base).

%% global_gensym(S,X): generates news symbols on ecah call X=S1,X=S2... etc - global to process
global_gensym(B,X):-global_gensym_no(B,N),atom_concat(B,N,X).



%% random(Lim,Var) return random integer in 0..Lim-1
random(Lim,Var):-xcall(43,i(Lim),Var).

%% returns random integer in [0..1]
random(X):-Lim is 2^28,random(Lim,I),X is I/(Lim-1).

%% set_random_seed(IntSeed): sets random seed
set_random_seed(IntSeed):-xcall(44,i(IntSeed),_).

%% set_precision(DecimalDigits): sets precision of decimal computations
set_precision(DecimalDigits):-xcall(45,i(0,DecimalDigits),_).

%% set_format_precision(DecimalDigits): sets precision of decimal printouts
set_format_precision(DecimalDigits):-xcall(45,i(1,DecimalDigits),_).


%% exists_file(F): true if file or dir F exists
exists_file(F):-atom(F),xcall(46,i(F),1).

%% exists_dir(D): true if directory D exists
exists_dir(F):-atom(F),xcall(46,i(F),2).

exists_file_or_dir(X):-atom(X),xcall(46,i(X),Ok),Ok>0.

%% exists_absolute(FileOrDir): true if FileOrDir is or expands to an existing file or dir
exists_absolute(F):-absolute_file_name(F,AF),exists_file_or_dir(AF).

truncate(X,Y):-xcall(47,i(X),Y).

%% float_integer_part(X,Y): gets the integer part of a float number
float_integer_part(X,Y):-floor(X,Y).

%% float_fractional_part(X,Y): gets the fractional part of a float number
float_fractional_part(X,Y):-floor(X,F),-(X,F,Y).

ceiling(X,Y):-floor(X,F),Y is F+1.

ceil(X,Y):-ceiling(X,Y).

round(X,Y):-X1 is X+(1/2),floor(X1,Y).

floor(X,Y):-X>=0,!,truncate(X,Y).
floor(X,Y):-X1 is (X-1),truncate(X1,Y).
    
integer(X,Y):-round(X,Y).
float(X,Y):-Y is 1/2+(X-1/2).
    
log(X,Y):-xcall(48,i(X),Y).

log(B,X,Y):-log(X,LX),log(B,LB),'/'(LX,LB,Y).

sin(X,Y):-xcall(49,i(X),Y).
cos(X,Y):-xcall(50,i(X),Y).
tan(X,Y):-xcall(51,i(X),Y).
asin(X,Y):-xcall(52,i(X),Y).
acos(X,Y):-xcall(53,i(X),Y).
atan(X,Y):-xcall(54,i(X),Y).

arith_const(X,Y):-xcall(55,i(X),Y).

pi(X):-arith_const(0,X).
e(X):-arith_const(1,X).
epsilon(X):-arith_const(2,X).
    
exp(X,Y):-xcall(56,i(X),Y).

%% int_sqrt(N,R): computes integer square root R of N, using Newton's method
int_sqrt(0,0).
int_sqrt(N,R):-N>0,
  sqrt_iterate(N,N,K),
  K2 is K*K,
  (K2>N->R is K-1;R=K).

% iterates until close enough   
sqrt_iterate(N,X,NewR):-
  R is (X+(N//X))//2,
  A is abs(R-X),
  sqrt_iterate1(A,N,R,NewR).

sqrt_iterate1(A,_,R,NewR):-A<2,!,NewR=R.
sqrt_iterate1(_,N,R,NewR):-sqrt_iterate(N,R,NewR).
  
new_ops(Init,Ops):-xcall(58,i(Init),Ops).
get_pri(Ops,Op,Assoc, Pri):-xcall(59,i(Ops,Op,Assoc),Pri),Pri>0.
get_pris(Ops,Op,Pris):-xcall(60,i(Ops,Op),Pris).
op_succ(Ops,Pri,NewPri):-Pri>0,xcall(61,i(Ops,Pri), NewPri). %,NewPri>0.
op_pred(Ops,Pri,NewPri):-Pri>0,xcall(62,i(Ops,Pri), NewPri). %,NewPri>0.
all_ops(Ops,OpList):-xcall(63,i(Ops),OpList).
op(Ops,Pri,Assoc,Op):-xcall(64,i(Ops,Pri,Assoc,Op),_).

%to_number(S,X):-functor(X,'$number',1),arg(1,X,S). % should keep small ints
to_number(S,N):-xcall(65,i(S),N).

%% protect_engine(S): makes sure engine S is never garbage collected
protect_engine(FunEng):-funeng2inteng(FunEng,IntEng),xcall(66,i(IntEng),_).

%% unprotect_engine(S): makes sure engine S can be garbage collected
unprotect_engine(FunEng):-funeng2inteng(FunEng,IntEng),xcall(67,i(IntEng),_).

% connection related

export_term(T):-
  Op is -71,
  xcall(Op,T,_).
  
import_term(T):-
  Op is -72,
  xcall(Op,ignore,T).
  %println(imported=T).
  %T\=ignore.


to_bundle(T,B):-term2bundle(T,B).

from_bundle(B,T):-bundle2term(B,T).

/*
to_bundle(T,B):-
  export_term(T),
  xcall(85,i(0),B).
  %println(xcall(85)=B).

from_bundle(B,T):-
  xcall(86,B,yes),
  %println(here),
  import_term(T).
*/

% uses $prolog_loop in xnet
s_server(Port,Op,BPFile,Goal):-
  xcall(73,i(Port,Op,BPFile,Goal),yes).

%% s_connection(Host,Port,Connection): opens socket Connection to Host:Port
s_connection(Host,Port,Connection):-xcall(74,i(Host,Port,1),Connection).

%% s_ping(Host,Port): tests if server on Host:Port is on
s_ping(Host,Port):-xcall(74,i(Host,Port,0),C),other_object(C),s_disconnect(C).

ask_s_server(Connection):-xcall(75,i(Connection),_). % ASK_SERVER

%% s_disconnect(Connection): closes socket connection
s_disconnect(Connection):-xcall(76,i(Connection),_).
% s_stop(Connection): see xnet.pl
  
i_server(Op,BPFile,Goal,Handle):-xcall(81,i(Op,BPFile,Goal),Handle).
i_connection(LocalAgent,Connection):-xcall(82,i(LocalAgent),Connection).
ask_i_server(Connection):-xcall(83,i(Connection),yes). % ASK_ISERVER
i_disconnect(Connection):-xcall(84,i(Connection,0),_).
%i_stop(Connection):-xcall(84,i(Connection,1),_).    
    
shm_put(S,V):-shm_put(shm,S,V).
shm_get(S,V):-shm_get(shm,S,V).
shm_remove(S):-shm_remove(shm,S).
    
%% shm_put(F,S,V), shm_get(F,S,V) shm_remove(F,S): puts, gets, removes value V in shared memory dir F of shared var S
shm_put(F,S,V):-
  atom(F),atom(S),nonvar(V),export_term(V),
  (xcall(77,i(F,S),yes)->true;import_term(_)).
shm_get(F,S,V):-atom(F),atom(S),xcall(78,i(F,S),yes),import_term(V).
shm_remove(F,S):-atom(F),atom(S),xcall(79,i(F,S),yes).

%% sleep_ms(Ms): sleeps a number of miliseconds, approximately
sleep_ms(Ms):-integer(Ms),!,xcall(80,i(Ms),_).
sleep_ms(Ms):-type_of(Ms,T),errmes(bad_argument_of_type(T),in(sleep_ms(Ms))).

%% sleep_ns(Ms): sleeps a number of miliseconds, approximately
sleep_ns(Ns):-xcall(110,i(Ns),_).

sleep(Sec):-integer(Sec),!,Ms is Sec*1000,sleep_ms(Ms).
sleep(S):-type_of(S,T),errmes(bad_argument_of_type(T),in(sleep(S))).

%% dirs(Dir,Ds): collects list Ds of direcories in Dir  
dirs(Dir,Ds):-dirs_or_files(0,Dir,Ds).

%% dirs(Ds): collects list Ds of direcories in current directory
dirs(Ds):-current_dir(D),dirs(D,Ds).

%% files(Dir,Fs): collects list Fs of files that are not directories in Dir
files(Dir,Fs):-dirs_or_files(1,Dir,Fs).

dirs_or_files(DorF,Dir,Xs):-findall(X,dir_or_file(DorF,Dir,X),Xs).

%% dir_or_file(DorF,Dir,X): iterates over directories (when DorF=0) or files (when DorF=1) in Dir
dir_or_file(DorF,Dir,X):-
  member(DorF,[0,1]),
  xcall(87,i(Dir,DorF),Iterator),
  ielement_of(the(Iterator),X),
  \+(X='.DS_Store').

%% files(Fs): collects list Fs of files that are not directories in current directory
files(Fs):-current_dir(D),files(D,Fs).

%% is_dir(D): true if D is a subdirectory in current directory
is_dir(D):-dirs(Ds),member(D,Ds).

%% is_file(F): true if F is a file in current directory
is_file(F):-files(Fs),member(F,Fs).

% maybe_absolute_member(F,Fs):-member(F,Fs).
% maybe_absolute_member(F,Fs):-maplist(absolute_file_name,Fs,AFs),member(F,AFs).

dir:-ls.

%% ls: (also dir) lists current directory
ls:-is_dir(D),write(D),write('/'),nl,fail.
ls:-is_file(F),println(F),fail.
ls:-nl.


file_op(Op,Fname):-xcall(94,i(Op,Fname),1).
file_op(Op,F1,F2):-xcall(94,i(Op,F1,F2),1).

%% make_directory(D): makes a directory in current directory
make_directory(D):-file_op(0,D).

%% delete_directory(D): deletes a directory in current directory
delete_directory(D):-file_op(1,D).

%% delete_file(F): deletes a file in current directory
delete_file(F):-file_op(2,F).

%% rename_file(Old,New): renames a file
rename_file(F1,F2):-file_op(3,F1,F2).

%% newer_file_of(F1,F2,Newer): pick newer file among F1 and F2
newer_file_of(F1,F2,Newer):-xcall(88,i(F1,F2),the(Newer)).

absolute_file_name(F0,F):-xcall(89,i(F0),the(F)).

%% current_dir(D): returns current directory
current_dir(D):-absolute_file_name('.',D).

%% pwd: prints out current directory
pwd:-current_dir(D),println(D).


save_prolog(Fname):-xcall(90,i(Fname),1).

load_prolog(Fname,Prolog):-xcall(91,i(Fname),Prolog).
   
%% to_file(F,T): saves (large term T, possibly containing serilizable Java objects) to file F
to_file(S,V):-
  atom(S),nonvar(V),export_term(V),
  (xcall(92,i(S),yes)->true
  ;import_term(_)
  ).

%% from_file(F,T): loads (large term T, possibly containing serilizable Java objects) from file F 
from_file(S,V):-
  atom(S),
  xcall(93,i(S),yes),
  import_term(V).
   
%% system(Cmd): calls an operating system command Cmd !!! BUGGY
system(Cmd):-xcall(95,i(Cmd),1). 

% full reflection API

%% new_java_class(Name,Cls): returns handle to (public) Java class Cls from Name (package included!)
new_java_class(Name,Cls):-
  xcall(96,i(Name),Cls).

%% new_java_object(Cls,Args,Obj): calls a Java constructor with Args on Cls and returns a handle Obj
new_java_object(Cls,Args,Obj):-
  xcall(97,i(Cls,Args),Obj).

%% invoke_java_method(Cls,Obj,MethName,Args,Answer): invokes a java method in class Cls on Object Obj
invoke_java_method(Cls,Obj,MethName,Args,Answer):-
  xcall(98,i(Cls,Obj,MethName,Args),Answer).

%% get_java_field_handle(Obj,FieldName,FieldHandle): returns handle to FieldName in object Obj 
get_java_field_handle(Obj,FieldName,FieldHandle):-
  xcall(99,i(Obj,FieldName),FieldHandle).

  
%% hub(H): creates a hub - an interactor used to synchronize threads
hub(H):-xcall(100,ignore,H).
  
%% new_logic_thread(Hub,X,G): a new logic thread, sharing this symbol table is created - disables SYMGC
new_logic_thread(Hub,X,G):-Clone=0,new_logic_thread(Hub,X,G,Clone). % $$$$ !!! 0 now - watch for symgc

% new_pure_logic_thread(Hub,X,G): assumes no new symbol creation - not reads, atom_concat etc.
new_pure_logic_thread(Hub,X,G):-Clone=0,new_logic_thread(Hub,X,G,Clone).

new_logic_thread(Hub,X,G,Clone):-new_logic_thread(Hub,X,G,Clone,no). % reuse this Prolog

%% new_logic_thread(Hub,X,G,Clone,Source): creates a new thread by Cloning current Prolog or loading new Prolog from Source
new_logic_thread(Hub,X,G,Clone,Source):-xcall(101,i(Hub,(X:-G),Clone,Source),R),R=yes.

new_file_thread(Hub,X,G,Source):-Clone=3,new_logic_thread(Hub,X,G,Clone,Source).

new_stream_thread(Hub,X,G,Source):-Clone=4,new_logic_thread(Hub,X,G,Clone,Source).

compare0(A,B):-atomic(A),!,atomic(B),A=B.
compare0(A,B):-atomic(B),!,atomic(A),A=B.
compare0(A,B,R):-xcall(102,i(A,B),R).

get_cmd_line_strings(Discard,Args):-xcall(103,i(Discard),Args).

get_cmd_line_args(Discard,Terms):-
  get_cmd_line_strings(Discard,Args),
  maplist(maybe_atom_to_term,Args,Terms).

%% get_cmd_line_args(Terms): returns the list of arguments received on the command line
get_cmd_line_args(Terms):-get_cmd_line_args(0,Terms).

%% discards the arguments received on the command line
discard_cmd_line:-get_cmd_line_strings(1,_).

%% pops a command line argument as the(StringArgument) or 'no' if none is left
pop_cmd_arg(Term):-
  xcall(104,i(0),Arg),
  %traceln(got_cmd_arg(Arg)),
  Arg=the(X),
  maybe_atom_to_term(X,Term).
 
maybe_atom_to_term(X,Term):-atomic(X),!,atom_to_term(X,Term).
maybe_atom_to_term(Term,Term).

push_cmd_arg(Term):-xcall(104,i(1,Term),_).

add_cmd_arg(Term):-xcall(104,i(2,Term),_).   

%% current_symbol(Sym): gets the list of all atoms from the symbol table - note that it also means it keeps them alive!
current_symbol(Sym):-atomic(Sym),!.
current_symbol(Sym):-var(Sym),current_system_symbol(Sym).
current_symbol(Sym):-var(Sym),current_user_symbol(Sym).

%% current_system_symbol(Sym): iterates over all atoms from the symbol table that are part of the system
current_system_symbol(Sym):-xcall(105,i(0),SymIter),ielement_of(the(SymIter),Sym).

%% current_user_symbol(Sym): iterates over all atoms from the symbol table created by the user
current_user_symbol(Sym):-xcall(105,i(1),SymIter),ielement_of(the(SymIter),Sym).

%% pop_from_path(Dir): removes Dir from beginning of search path for find_file
pop_from_path(F):-xcall(106,i(0,_),the(F)).  

%% first_on_path(Dir): returns first directory on search path for find_file
get_first_on_path(F):-xcall(106,i(1,_),the(F)).  

% set_on_path(Dir): updates first directory on search path for find_file
set_first_on_path(F):-pop_from_path(_),push_to_path(F).  

%% push_to_path(Dir): ads Dir to beginning of search path for find_file
push_to_path(F):-xcall(106,i(2,F),_).  

%% add_to_path(Dir): ads Dir to end of search path for find_file
add_to_path(F):-xcall(106,i(3,F),_).


%% path_elements(Dirs): returns the list of directories in search path
path_elements(Fs):-xcall(107,ignore,Fs). 

%% clear_path: clears search path for find_file
clear_path:-xcall(108,ignore,_). 
    
% 109 - see gvars
    
%% atomic_list_concat(As,A): A is the concatentation of atomic elements on list As

atomic_list_concat(As,A):-xcall(111,i(As),A).

%% open_list(Bag): creates a persistent list 
open_list(Bag):-xcall(112,ignore,Bag).

%% adds a new term to the list
list_add(Bag,X):-xcall(113,i(Bag,X),_).

%% close_list(Bag,Xs): moves all elements of Bag to Prolog list Xs. Bag is still usable for new lis_adds.
close_list(Bag,Xs):-xcall(114,i(Bag),Xs).

%% java_profiling(on) turns on Java method profiling
java_profiling(on):-xcall(115,i(1),_).

%% java_profiling(off) turns off Java method profiling
java_profiling(off):-xcall(115,i(0),_).

symtest(Mes):-xcall(116,i(Mes),_).

thread_id(Id):-xcall(117,ignore,Id).

%% to_upper_case(S,T), to_lower_case(S,T): convert atoms to upper/lower case
to_upper_case(S,T):-xcall(118,i(S),T).
to_lower_case(S,T):-xcall(119,i(S),T).

%% concatenate_files(Fs,F): concatenates list of files Fs to file F 
concatenate_files(Fs,ToF):-xcall(120,i(Fs,ToF),_).

%% ingest_file(WamFile,BpFile): creates the .bp BpFile from the lwam.bp in lprolog.jar and the .wam file WamFile
ingest_file(I,O):-xcall(135,i(I,O),R),R=1.



%% ingest_file(WamFile,BpFile): creates the .bp BpFile from the lwam.bp in lprolog.jar and the .wam file WamFile
%named_call(I,O):-xcall(???,I,O).


swi_det_call0(I,O):-xcall(136,i(I),O).

%% subst_atom(Old,New,In,Out): replaces Old with New in atom In
subst_atom(Old,New,In,Out):-xcall(121,i(Old,New,In),Out).

%% to_rstring(T,S): converts a term T to a symbol such that it can be read back
to_rstring(T,S):-xcall(122,i(T),S).

%% object_to_quoted_string(T,S): converts a term T to a symbol such that it can be read back
object_to_quoted_string(T,S):-xcall(123,i(T),S).

%% getenv(Key,Value): gets an environment variable
getenv(Key,Val):-nonvar(Key),!,xcall(124,i(Key),the(Val)).
getenv(Key,Val):-xcall(125,ignore,TheI),ielement_of(TheI,Key),xcall(124,i(Key),the(Val)).

expand_env(S,X):-atom(S),xcall(126,i(S),X).

get_working_directory(Dir):-xcall(127,ignore,Dir).

set_working_directory(Dir,AbsDir):-xcall(128,i(Dir),the(AbsDir)).

set_working_directory(Dir):-set_working_directory(Dir,_),!.
set_working_directory(Dir):-errmes(no_such_directory,Dir).

get_engine_object(FunEng,ObjE):-
  funeng2inteng(FunEng,IntE),
  xcall(129,i(IntE),the(ObjE)).

new_engine_object(X,G,ObjE):-
  new_engine(X,G,FunEng),
  get_engine_object(FunEng,ObjE).
  
current_engine_object(ObjE):-
  inteng2funeng(0,FunEng),
  get_engine_object(FunEng,ObjE).
  
% engine_type(IntEng): recognizes type of an engine - to be used through is_engine
engine_type(IntEng):-xcall(130,i(IntEng),1).
   
engine_gc:-xcall(131,ignore,_).

%% to_set(Xs,Set): converts List of ground elements to set with no duplications
to_set(Xs,Set):-xcall(132,i(Xs),Set).

%% from_set(Set,Xs): converts set to List of ground elements
from_set(Set,Xs):-xcall(133,i(Set),Xs).

%% varnumbers(Term,NewTerm): inverse of numbervars - turns VAR terms into vars while keeping other vars unchanged
varnumbers(T,S):-xcall(134,i(T),S).

set_op(Name,Set,With):-invoke_java_method(Set,Name,args(With),_).

set_intersect_to(Set,Other):-set_op(retainAll,Set,Other).
set_difference_to(Set,Other):-set_op(removeAll,Set,Other).
set_union_to(Set,Other):-set_op(addAll,Set,Other).

%% ground_set_difference(As,Bs,Cs): computes the set difference without duplicates of atomic As,Bs
ground_set_difference(As,Bs,Cs):-
  to_set(As,S1),
  to_set(Bs,S2),
  set_difference_to(S1,S2),
  from_set(S1,Cs).

%% ground_set_union(As,Bs,Cs): computes the set union without duplicates of atomic As,Bs
ground_set_union(As,Bs,Cs):-
  to_set(As,S1),
  to_set(Bs,S2),
  set_union_to(S1,S2),
  from_set(S1,Cs).

%% ground_set_union(As,Bs,Cs): computes the set intersection without duplicates of atomic As,Bs    
ground_set_intersect(As,Bs,Cs):-
  to_set(As,S1),
  to_set(Bs,S2),
  set_intersect_to(S1,S2),
  from_set(S1,Cs).

/*      
settest:-
  [a,b,a,a,d,d,c]=S1,
  [f,c,d,d,d,e,e]=S2,
  ground_set_intersect(S1,S2,S3),
  println(S3).
*/  

% end
