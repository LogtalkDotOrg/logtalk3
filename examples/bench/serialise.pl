% generated: 17 November 1989
% option(s): 
%
%   serialise
%
%   David H. D. Warren
%
%   itemize (pick a "serial number" for each 
%   unique integer in) a list of 25 integers

top:-serialise.

serialise :- serialise("ABLE WAS I ERE I SAW ELBA",_).

serialise(L,R) :-
    pairlists(L,R,A),
    arrange(A,T),
    numbered(T,1,_).

pairlists([X|L],[Y|R],[pair(X,Y)|A]) :- pairlists(L,R,A).
pairlists([],[],[]).

arrange([X|L],tree(T1,X,T2)) :-
    split(L,X,L1,L2),
    arrange(L1,T1),
    arrange(L2,T2).
arrange([],void).

split([X|L],X,L1,L2) :- !, split(L,X,L1,L2).
split([X|L],Y,[X|L1],L2) :- before(X,Y), !, split(L,Y,L1,L2).
split([X|L],Y,L1,[X|L2]) :- before(Y,X), !, split(L,Y,L1,L2).
split([],_,[],[]).

before(pair(X1,_),pair(X2,_)) :- X1 < X2.

numbered(tree(T1,pair(_,N1),T2),N0,N) :-
    numbered(T1,N0,N1),
    N2 is N1+1,
    numbered(T2,N2,N).
numbered(void,N,N).
