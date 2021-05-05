natint_go:-  nconsult('../progs/prog.nl'),#(goal,X),writeln(X),fail.
   
nat_test1:-from_nat_file('../progs/prog.nl',R),writeln(R),fail.
   
natint_test:-nparse(
    "P a X _0 and X lists 1 2 3 if _0 holds P g a and P b P X and c _ _ 42 .",
  Ts),
  pp(Ts),fail;nl.
       
ncall(Cs0,R):-
  append(Cs0, " .",Cs),
  nparse(Cs,Cls),
  ( Cls=(H:-B)->call(B),R=H
  ; Cls=B,call(B),R=B
  ).
         
nconsult(File):-
  (forall(between(0,100,I),abolish((#)/I))),
  from_nat_file(File,Css),
  (member(Xs,Css),
   % writeln(xs=Xs),
    nparse(Xs,Cls),
    assertz(Cls),
    fail
  ; 
   true
  ),
  listing((#)).
 
from_nat_file(F,Wss):-
  read_file_to_string(F,Cs,[]),
  split_string(Cs,"."," \n\t",Css),
  findall(Ws,(member(Xs,Css),fix_nat_string(Xs,Ws)),Wss).
  
fix_nat_string(S,Cs):-string_codes(S,Cs0),Cs0=[_|_],append(Cs0," .",Cs),writeln(s=Cs0).
    

nparse(Cs,Cls):-
   wparse(Cs,Wss),
   maplist(maplist(to_tok(_)),Wss,Tss),
   maplist(arrify,Tss,Rss),
   !,
   append(Rss,Ts),
   to_cls(Ts,Cls).

to_words(Cs,Ws):-split_string(Cs," \n\t"," \n\t",Ws0),
  findall(GoodW,(member(W,Ws0),fix_word(W,GoodW)),Ws).

fix_word(W0,GoodW):-W0\=="",
  atom_codes(W,W0),
  ( atom_number(W,GoodW)->true
  ; GoodW=W
  ).

wparse(Cs,Wss):-to_words(Cs,Ws),cparse(Ws,Wss).

cparse(Ws,Wss):-bparse(Wss,Ws,[]).

bparse([Ws|Wss])-->sparse(Ws,[if,and]),!,bparse(Wss).
bparse([Ws])-->sparse(Ws,[(.)]).


sparse([],Ends)-->[End],{member(End,Ends)},!.
sparse([W|Ws],Ends)-->[W],sparse(Ws,Ends).

arrify([X, KWord | Xs], []):- KWord==holds,!, X=..[(#)|Xs].   
arrify([X, KWord | Xs], []):- KWord==lists,!, listify(Xs,X).  
arrify(Ts, [A]):- A=..[(#)|Ts].
  
listify(Xs,T):-listify(Xs,(#),nil,T).
  
listify(X,_,_,R):-var(X),!,R=X.  
listify([],_,Nil,R):-!,R=Nil.
listify([X|Xs],F,Nil,T):-!,functor(T,F,3),
  arg(1,T,list),arg(2,T,X),arg(3,T,Fs),listify(Xs,F,Nil,Fs).   
listify(X,_,_,X).   
   
to_tok(_D,W,W):-number(W),!.
to_tok(_D,'_',_):-!.
to_tok(D,W,V):-atomic(W),atom_codes(W,[C|_]),is_var_char(C),!,member(W-V,D),!.
to_tok(_D,W,W):-atomic(W).
 
is_var_char(95):-!. % 95 is '_'
is_var_char(C):-is_upper(C).

to_cls([H],H):-!.
to_cls([H,B|Bs],(H:-Cs)):-to_conj([B|Bs],Cs).
     
to_conj([X],X):-!.
to_conj([X|Xs],(X,Cs)):-to_conj(Xs,Cs).

pp(X):-portray_clause(X).  

 