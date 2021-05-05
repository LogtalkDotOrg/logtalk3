% sort.pl

/*
% sort, adapted from public domain code written by R.A. O'Keefe
% use merge_sort(<,_,_) if you do not want duplications eliminated
% use merge_sort(>,_,_) for descending order
*/

%% sort(Xs,SortedSetXs): sorts a list then removes duplicates
sort(L1,L2):-merge_sort(<,L1,DupL),remdup(DupL,L2).

%% msort(Xs,SortedMultisetXs): sorts a list without removing duplicates
msort(L1,L2):-merge_sort(<,L1,L2).

%% keysort(KVs,NewKVs): sorts by keys, a list of Key-Value pairs
keysort(L,S):-ksort(L,S).
      
merge_sort(Rel, L,S ):-
	length(L,N),
	merge_sort1(N, Rel, L,S,[] ).

merge_sort1( 0,_,L,[],L ):-!.
merge_sort1( 1,_,[X|L],[X],L ):-!.
merge_sort1( N,Rel,L,S,R ):-				% N >= 2
	N1 is N >> 1,	N2 is N-N1,
	merge_sort1( N1,Rel,L,S1,R1),	
	merge_sort1( N2,Rel,R1,S2,R),
	merge_2( S2,Rel,S1,S ).

merge_2([],_,S,S ):-!.
merge_2([X|L1],Rel,[Y|L2],[X|L] ):-compare(Rel,X,Y),!,
	merge_2(L1,Rel,[Y|L2],L ).
merge_2(L1,Rel,[Y|L2],[Y|L] ):-
	merge_2(L2,Rel,L1,L ).

merge(Xs,Ys,Zs):-merge_2(Xs,(<),Ys,Zs).

merge_set(Xs,Ys,Zs):-merge(Xs,Ys,Us),remdup(Us,Zs).

ksort(List, Sorted) :-
    -(0,1,X),
	keysort(List, X , S, []), !,
	Sorted = S.
ksort(X, Y):-
    user_error('illegal_arguments',keysort(X,Y)).

%% keygroup(KsVs,Keys,Vals): keysorts KsVs, then backtrack over groups of the form Key,Vals
keygroup(KsVs,K,Vs):-
  keysort(KsVs,Sorted),
  concordant_subset(Sorted,K,Vs).
  
keysort([Head|Tail], Lim, Sorted, Rest) :- !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run, Rest0),
	keysort(Rest0, 1, Lim, Run, Sorted, Rest).
keysort(Rest, _, [], Rest).

keysort([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
	J =\= Lim, !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkeyrun(Tail, Qh, Qh, Run1, Rest0),
	keysort(Rest0, 1, J, Run1, Run2, Rest1),
	keymerge(Run0, Run2, Run),
	K is J+J,
	keysort(Rest1, K, Lim, Run, Sorted, Rest).
keysort(Rest, _, _, Sorted, Sorted, Rest).

samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QT = [Q-_|QT2], 
	Q @=< H, !,
	QT2 = [Hd|_],
	samkeyrun(Tail, QH, QT2, Run, Rest).
samkeyrun([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QH = [Q-_|_],
	H @< Q, !,
	samkeyrun(Tail, [Hd|QH], QT, Run, Rest).
samkeyrun(Rest, Run, [_], Run, Rest).

% keymerge(+List, +List, -List).
keymerge([], L2, Out) :- !,
	Out = L2.
keymerge([H1|T1], L2, Out) :-	
	L2 = [K2-_|_],
	H1 = K1-_,
	K1 @=< K2, !,
	Out = [H1|Out1],
	keymerge(T1, L2, Out1).
keymerge(L1, [H2|L2], Out) :- !,
	Out = [H2|Out1],
	keymerge(L1, L2, Out1).
keymerge(List, _, List).

% removes duplicates

%% remdup(Multiset,Set) : removes duplicates from a sorted list Multiset
remdup([],[]).
remdup([X,Y|Xs],Ys):-compare(=,X,Y),!,remdup([X|Xs],Ys).
remdup([X|Xs],[X|Ys]):-remdup(Xs,Ys).


% source level term comparison - infelicity: X @<Y,Y@<X succeedes.

%% compare(R,X,Y) : true if R (<,=,>) expresses the standard order between X,Y
compare(R,X,Y):-compare0(X,Y,R).

%% A==B : is true if A and B are structurally the same (correesponding vars being also identical)
A==B :- compare0(A,B,(=)).

%% A\==B : is true if A and B are not structurally the same, negation of '=='
A\==B :- compare0(A,B,R),'$noteq'(R).

%% A @< B : true if A is smaller than B in standard order
%% A @> B : true if A is larger than B in standard order
%% A @=< B : true if A is smaller or equal than B in standard order
%% A @>= B : true if A is larger or equal than B in standard order

A @< B :- compare0(A,B,<).
A @> B :- compare0(A,B,>).
A @=< B :- compare0(A,B,R),'$lesseq'(R).
A @>= B :- compare0(A,B,R),'$gteq'(R).

'$lesseq'(<).
'$lesseq'(=).

'$gteq'(>).
'$gteq'(=).

'$noteq'(<).
'$noteq'(>).


% set operations - assumes standard ordering of elements

%% ord_union/3: computes the union of two orderd sets
ord_union([],Xs,Xs).
ord_union(Xs,[],Xs):-Xs=[_|_].
ord_union([X|Xs],[Y|Ys],[X|Zs]):-X==Y,
   ord_union(Xs,Ys,Zs).
ord_union([X|Xs],[Y|Ys],[X|Zs]):-
   before_in_set(X,Y),
   ord_union(Xs,[Y|Ys],Zs).
ord_union([X|Xs],[Y|Ys],[Y|Zs]):-
   before_in_set(Y,X),
   ord_union([X|Xs],Ys,Zs).

%% ord_intersection/3: computes the intersection of two orderd sets   
ord_intersection([],_,[]).
ord_intersection(Xs,[],[]):-Xs=[_|_].
ord_intersection([X|Xs],[Y|Ys],[X|Zs]):-X==Y,
   ord_intersection(Xs,Ys,Zs).
ord_intersection([X|Xs],[Y|Ys],Zs):-
   before_in_set(X,Y),
   ord_intersection(Xs,[Y|Ys],Zs).
ord_intersection([X|Xs],[Y|Ys],Zs):-
   before_in_set(Y,X),
   ord_intersection([X|Xs],Ys,Zs).
 
 
%% ord_subtract/3, ord_difference/3: computes the difference of two ordered sets 
ord_difference([],_,[]).
ord_difference(Xs,[],Xs):-Xs=[_|_].
ord_difference([X|Xs],[Y|Ys],Zs):-X==Y,
   ord_difference(Xs,Ys,Zs).
ord_difference([X|Xs],[Y|Ys],[X|Zs]):-
   before_in_set(X,Y),
   ord_difference(Xs,[Y|Ys],Zs).
ord_difference([X|Xs],[Y|Ys],Zs):-
   before_in_set(Y,X),
   ord_difference([X|Xs],Ys,Zs).

ord_subtract(Xs,Ys,Zs):-ord_difference(Xs,Ys,Zs).

%% ord_sym_difference/3 computes the symmetric difference of two ordered sets 
ord_sym_difference(Xs,Ys,Zs):-
  ord_difference(Xs,Ys,Xs0),
  ord_difference(Ys,Xs,Ys0),
  append(Xs0,Ys0,Zs).
    
%% ord_subset/2: checks if the first ordered set is included in the second
ord_subset(SmallXs,LargeXs):-
  ord_intersection(SmallXs,LargeXs,Xs),
  !,
  Xs=SmallXs.
  
   
before_in_set(X,Y):-compare('<',X,Y).  

%% sublist_of_length(K,Xs,Ys): generates sublists of Xs of length K
sublist_of_length(K,Xs,Ys):-
  length(Xs,L),
  K=<L,
  length(Ys,K),
  append(Ys,_,Suf),
  append(_,Suf,Xs).
  

% set_* ... : operations similar to ord_* but on sets (calls sort/2 first)
  
set_union(Xs0,Ys0,Zs):-sort(Xs0,Xs),sort(Ys0,Ys),ord_union(Xs,Ys,Zs).
set_intersection(Xs0,Ys0,Zs):-sort(Xs0,Xs),sort(Ys0,Ys),ord_intersection(Xs,Ys,Zs).
set_difference(Xs0,Ys0,Zs):-sort(Xs0,Xs),sort(Ys0,Ys),ord_difference(Xs,Ys,Zs).
set_sym_difference(Xs0,Ys0,Zs):-sort(Xs0,Xs),sort(Ys0,Ys),ord_sym_difference(Xs,Ys,Zs).
is_subset(Xs0,Ys):-sort(Xs0,Xs),ord_subset(Xs,Ys).

%% mset_* ... : operations similar to ord_* .. but on multisets (calls msort/2 first)

mset_union(Xs0,Ys0,Zs):-msort(Xs0,Xs),msort(Ys0,Ys),ord_union(Xs,Ys,Zs).
mset_intersection(Xs0,Ys0,Zs):-msort(Xs0,Xs),msort(Ys0,Ys),ord_intersection(Xs,Ys,Zs).
mset_difference(Xs0,Ys0,Zs):-msort(Xs0,Xs),msort(Ys0,Ys),ord_difference(Xs,Ys,Zs).
mset_sym_difference(Xs0,Ys0,Zs):-msort(Xs0,Xs),msort(Ys0,Ys),ord_sym_difference(Xs,Ys,Zs).
is_submset(Xs0,Ys):-msort(Xs0,Xs),ord_subset(Xs,Ys).

%% mset_sum(Xs,Ys,Zs):  multiset sum (disjoint union)
mset_sum(Xs,Ys,Zs):-append(Xs,Ys,Zs0),msort(Zs0,Zs).

xset_distance(_,[],[],R):-!,R=0.
xset_distance(DF,Xs,Ys,R):-
  call(DF,Xs,Ys,Ds),
  length(Xs,LX),
  length(Ys,LY),
  length(Ds,D),
  S is LX+LY,
  R is D/S.

%% set_distance(Xs,Ys,R): computes a distance between sets in N*log(N) time
set_distance(Xs,Ys,R):-xset_distance(set_sym_difference,Xs,Ys,R).

%% mset_distance(Xs,Ys,R): computes a distance between multisets in N*log(N) time
mset_distance(Xs,Ys,R):-xset_distance(mset_sym_difference,Xs,Ys,R).



  
  
