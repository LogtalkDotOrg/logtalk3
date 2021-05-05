% builtins.pl

/*
% ---------- ADD BUILTINS: <here> and in LogicEngine -----------------------
*/

% APPLY= 63; // was 50

n_bbuiltin(64).   % first in_head builtin->BUILTIN
n_binline(66).  % first inline
n_barith(X1):-n_binline(X),X1 is X+5. % first arith in_body->ARITH

%% builtin/3: contains specs for builtins shared with C emulator
bbuiltin(Pred,Spec,Location):-bbuiltin0(Pred,Spec,Location).
bbuiltin(Pred,Spec,in_body):-bbuiltin1(Pred,Spec).

bbuiltin0('true'(_),0,in_head). % BUILTIN+0 =64
bbuiltin0('call'(_,_),1,in_head). % BUILTIN+1 = 65
% n_binline = 66
bbuiltin0(fail(_),0,in_body). % 66
bbuiltin0(cwrite(_,_),1,in_body). % inline = 67
bbuiltin0(cnl(_),2,in_body). % inline = 68

%% is_compiled(F(X1..XN)) : true if F/N it is a compiled predicate
bbuiltin0(is_compiled(_,_),3,in_body). % inline = 69
bbuiltin0(return(_,_),4,in_body). % 70

% add here and update ARITH and n_barith 5->...

bbuiltin1(strip_cont0(_,_,_),arith(0,1)). % 0-th: INLINE+0

%% current_engine(Handle) : returns Handle (a small int) representing current engine as seen by its parent
bbuiltin1(current_engine0(_Handle,_),arith(1,1)).
bbuiltin1(create_engine0(_,_,_,_),arith(2,1)).
bbuiltin1(engine_get0(_,_,_),arith(3,1)).
bbuiltin1(engine_stop0(_,_),arith(4,0)).

%% to_engine0(Engine,Term) : sends Term to Engine
bbuiltin1(to_engine0(_Engine,_Term,_),arith(5,0)).

%% from_engine(Term) : retrieves Term sent to this Engine
bbuiltin1(from_engine0(_Term,_),arith(6,1)).

bbuiltin1(this_class(_Handle,_),arith(7,1)).

%% arg(N,T,X): selects N-th argument of term T, function symbol if N=0
bbuiltin1(arg(_,_,_,_),arith(8,1)).
%% arity(T,N): returns arity of term T
bbuiltin1(arity(_,_,_),arith(9,1)).
%% fun(F,N,T): creates term T of arity N with function symbol F 
bbuiltin1(fun(_,_,_,_),arith(10,1)).

%% succ(N,N1) : computes N1 successor of N
bbuiltin1(succ(_,_,_),arith(11,1)).
bbuiltin1(type_of(_,_,_),arith(12,1)).

%% icall, icall_op, icall_impure: bridges to fast internal builtins called directly from emulator
bbuiltin1(icall(_Op,_Ins,_Outs,_Cont),arith(13,1)).
bbuiltin1(icall_op(_Op,_In1,_In2,_Out,_Cont),arith(14,1)).
bbuiltin1(icall_impure(_Op,_In1,_In2,_In3,_Out,_Cont),arith(15,1)).

bbuiltin1(xcall0(_Op,_Ins,_Outs,_Cont),arith(16,1)).

% add here - nothing to update 

cutp(X):-atom_codes(Cut,"$cut"),X=Cut.

bin_bbuiltin(('!'(X,Cont):-'true'(Cont))):-cutp(X).
bin_bbuiltin(C):-bbuiltin(B,_,Where),bu_body(Where,B,C).

bu_body(in_head,B,(B:-'true'(Cont))):-functor(B,_,N),arg(N,B,Cont).
bu_body(in_body,B,(B:-B)).
