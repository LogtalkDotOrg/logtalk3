% lists.pl

% higher order


mapfun(P,FXs,FYs):-arity(FXs,N),arg(0,FXs,F),fun(F,N,FYs),
  mapfun0(0,N,P,FXs,FYs).

mapfun0(N,N,_,_,_):-!.
mapfun0(K,N,P,FXs,FYs):-K<N,succ(K,K1),arg(K1,FXs,X),arg(K1,FYs,Y),
  call(P,X,Y),
  mapfun0(K1,N,P,FXs,FYs).


%% maplist(F,Xs1,Xs2,...): applies F to elements in the same position in Xs1,Xs2..

maplist(F,Xs):-maplist0(Xs,F).

maplist0([],_F):-!.
maplist0([X|Xs],F):-call(F,X),maplist0(Xs,F).

maplist(F,Xs,Ys):-maplist0(Xs,F,Ys).

maplist0([],_F,[]).
maplist0([X|Xs],F,[Y|Ys]):-call(F,X,Y),maplist0(Xs,F,Ys).

maplist(F,Xs,Ys,Zs):-maplist0(Xs,F,Ys,Zs).

maplist0([],_F,[],[]).
maplist0([X|Xs],F,[Y|Ys],[Z|Zs]):-call(F,X,Y,Z),maplist0(Xs,F,Ys,Zs).

%% sumlist(Xs,S): returns S, the sum of numbers on list Xs
sumlist([],0).
sumlist([X|Xs],R):-sumlist(Xs,R1),'+'(R1,X,R).

%% prodlist(Xs,P): returns P, the product of numbers on list Xs
prodlist([],1).
prodlist([X|Xs],R):-prodlist(Xs,R1),'*'(R1,X,R).

%% foldl(F,Z,Xs,R): combines with F all members of Xs, from left to right, starting with Z    
foldl(F,Z,Xs,R):-foldl0(Xs,F,Z,R).
  
foldl0([],_,R,R).
foldl0([X|Xs],F,R1,R3):-call(F,R1,X,R2),foldl0(Xs,F,R2,R3).

%% foldl(F,Z,Xs,R): combines with F all members of Xs, from right to left, starting with Z
foldr(F,Z,Xs,R):-foldr0(Xs,F,Z,R).
  
foldr0([],_,Z,Z).
foldr0([X|Xs],F,Z,R2):-foldr0(Xs,F,Z,R1),call(F,X,R1,R2).

%% combines Xs and Ys into pairs X-Y with X in Xs and Y in Ys
zip([],[],[]).
zip([X|Xs],[Y|Ys],[X-Y|Zs]):-zip(Xs,Ys,Zs).

%% zipWith(F,Xs,Ys,Zs): for X in Xs and Y in Zs computes and collects Z in Zs such that F(X,Y,Z)
zipWith(F,Xs,Ys,Zs):-zipWith2(Xs,Ys,F, Zs).

zipWith2([],[],_,[]).
zipWith2([X|Xs],[Y|Ys],F,[Z|Zs]):-call(F,X,Y,Z),zipWith2(Xs,Ys,F,Zs).
  
% LIST PROCESSING

%% is_list(X): true if X is instantiated to a list
is_list(X) :- var(X),!,fail.
is_list([]).
is_list([_|Xs]) :- is_list(Xs).

%% append(Xs,Ys,Zs): true if Zs is the concatenation of lists Xs and Ys

append(Xs,Ys,Zs):-det_append(Xs,Ys,Rs),!,Rs=Zs.
append(Xs,Ys,Zs):-general_append(Xs,Ys,Zs).
    
general_append([],Ys,Ys).
general_append([X|Xs],Ys,[X|Zs]):-general_append(Xs,Ys,Zs).

%% append(Xss,Xs): Xs is the concatentation of Xss obtained by folding with append/3
append(Xss,Xs):-foldl(det_append,[],Xss,Xs).

%% select(X,Xs,Ys) : pulls out an element X from list Xs returning the others as Ys
select(X,[X|S],S).
select(X,[Y|S1],[Y|S2]):- %nonvar(S1),
  select(X,S1,S2).

%% true if X occurs on list Xs - also works as a generator
member(X,[X|_]).
member(X,[_|Xs]):-member(X,Xs).

%% true if X occurs on list Xs - only checks
memberchk(X,[X|_]):-!.
memberchk(X,[_|Xs]):-memberchk(X,Xs).

rmember(X,[_|Xs]):-rmember(X,Xs).
rmember(X,[X|_]).

%% rselect/3: like select/3 but starts with the last one
rselect(X,[X|S],S).
rselect(X,[Y|S1],[Y|S2]):- %nonvar(S1),
  rselect(X,S1,S2).

%% select_nonvar/3: like select/3 but checks that list ends in a nonvar
select_nonvar(X,XXs,Xs):-nonvar(XXs),XXs=[X|Xs].
select_nonvar(X,YXs,[Y|Ys]):-nonvar(YXs),YXs=[Y|Xs],select_nonvar(X,Xs,Ys).

%% picks N-th element X starting from 0 occuring in Xs
nth0(N,Xs,X):-member_i(X,Xs,0,N).

%% nth1(N,Xs,X): picks N-th element X starting from 1 occuring in Xs  
nth1(N,Xs,X):-member_i(X,Xs,1,N).

member_i(X,[X|_],N,N).
member_i(X,[_|Xs],N1,N3):-
  succ(N1,N2),
  member_i(X,Xs,N2,N3).

%% last(Xs,X): picks X, the last element of list Xs
last([X],Last):-!,Last=X.
last([_|Xs],Last):-last(Xs,Last).

%% reverse(Xs,Ys) : Ys is Xs reversed
reverse(Xs,Ys):-rev(Xs,[],Ys).

rev([],Ys,Ys).
rev([X|Xs],Ys,Zs):-rev(Xs,[X|Ys],Zs).

%% init(Xs,AllButLast): returns all but the last element of a list
init([_],[]):-!.
init([X|Xs],[X|Ys]):-init(Xs,Ys).

%% take(N,Xs,Ys): takes the first N elements Ys of list Xs
take(N,Xs,Ys):-split_at(N,Xs,Ys,_).

%% split_at(N,Xs,Ys,Zs): splits into first N elements Ys of list Xs and other elements Zs (if any) 
split_at(N,[X|Xs],[X|Ys],Zs):-N>0,!,N1 is N-1,split_at(N1,Xs,Ys,Zs).
split_at(_,Xs,[],Xs).

%% drop(N,Xs,Ys): drops the first N elements of list Xs and returns the left over elements Ys
drop(N,[_|Xs],Ys):-N>0,!,N1 is N-1,drop(N1,Xs,Ys).
drop(_,Ys,Ys).

%% split_to_groups(K,Xs,Xss): splits Xs into a list of K lists
split_to_groups(K,Xs,Yss):-K>0,
  length(Xs,L),
  Mod is 1*(L mod K),
  Size is Mod+(L // K),
  ksplit(K,Size,Xs,Xss),
  !,
  Yss=Xss.

%% split_to_size(K,Xs,Xss): splits Xs into a list of lists of size K or less each
split_to_size(K,Xs,Yss):-
  K>0,
  length(Xs,L),
  Groups is L // K,
  ksplit(Groups,K,Xs,Xss),
  !,
  Yss=Xss.

ksplit(_,_,[],[]):-!.     
ksplit(0,_,Xs,[Xs]):-!.
ksplit(I,K,Xs,[Hs|Yss]):-
  I>0,
  I1 is I-1,
  split_at(K,Xs,Hs,Ts),
  ksplit(I1,K,Ts,Yss).
  
%% partition(Pred, Xs, Included, Excluded): separates Xs in two lists using Pred
partition(Pred, Xs, Included, Excluded) :-
  partition1(Xs, Pred, Included, Excluded).

partition1([],_,[],[]).
partition1([X|Xs],Pred,Is,Es) :-
   call(Pred, X),
   !,
   Is=[X|Is0],
   partition1(Xs,Pred,Is0,Es).
partition1([X|Xs],Pred,Is,[X|Es]):-
   partition1(Xs,Pred,Is,Es).
   
nth0(V, In, Element, Rest) :-
    var(V), !,
    generate_nth(0, V, In, Element, Rest).
 nth0(V, In, Element, Rest) :-
    V>=0,
    find_nth0(V, In, Element, Rest).
    
 generate_nth(I, I, [Head|Rest], Head, Rest).
 generate_nth(I, IN, [H|List], El, [H|Rest]) :-
    I1 is I+1,
    generate_nth(I1, IN, List, El, Rest).

 find_nth0(0, [Head|Rest], Head, Rest) :- !.
 find_nth0(N, [Head|Rest0], Elem, [Head|Rest]) :-
   M is N-1,
   find_nth0(M, Rest0, Elem, Rest). 
 
 subtract([], _, []) :- !.
   subtract([E|T], D, R) :-
   member(E, D), !,
   subtract(T, D, R).
   subtract([H|T], D, [H|R]) :-
   subtract(T, D, R).
    
 union([], L, L) :- !.
 union([H|T], L, R) :-
    member(H, L), !,
    union(T, L, R).
 union([H|T], L, [H|R]) :-
   union(T, L, R). 
   
 intersection([], _, []) :- !.
 intersection([X|T], L, Intersect) :-
    member(X, L), !,
    Intersect = [X|R],
    intersection(T, L, R).
 intersection([_|T], L, R) :-
   intersection(T, L, R).
 