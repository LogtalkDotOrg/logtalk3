% generated: 19 June 1990
% option(s): 
%
%   browse
%
%   Tep Dobry (from Lisp version by R. P. Gabriel)
%
%   (modified January 1987 by Herve' Touati)

top :- 
    init(100,10,4,
         [[a,a,a,b,b,b,b,a,a,a,a,a,b,b,a,a,a],
          [a,a,b,b,b,b,a,a,[a,a],[b,b]],
          [a,a,a,b,[b,a],b,a,b,a]
         ],
         Symbols),
    randomize(Symbols,RSymbols,21),!,
    investigate(RSymbols,
                [[star(SA),B,star(SB),B,a,star(SA),a,star(SB),star(SA)],
                 [star(SA),star(SB),star(SB),star(SA),[star(SA)],[star(SB)]],
                 [_,_,star(_),[b,a],star(_),_,_]
                ]).


init(N,M,Npats,Ipats,Result) :- init(N,M,M,Npats,Ipats,Result).

init(0,_,_,_,_,_) :- !.
init(N,I,M,Npats,Ipats,[Symb|Rest]) :- 
    fill(I,[],L),
    get_pats(Npats,Ipats,Ppats),
    J is M - I,
    fill(J,[pattern(Ppats)|L],Symb),
    N1 is N - 1,
    (I =:= 0 -> I1 is M; I1 is I - 1),
    init(N1,I1,M,Npats,Ipats,Rest).

fill(0,L,L) :- !.
fill(N,L,[dummy([])|Rest]) :- 
    N1 is N - 1,
    fill(N1,L,Rest).

randomize([],[],_) :- !.
randomize(In,[X|Out],Rand) :-
    length(In,Lin),
    Rand1 is (Rand * 17) mod 251,
    N is Rand1 mod Lin,
    split(N,In,X,In1),
    randomize(In1,Out,Rand1).

split(0,[X|Xs],X,Xs) :- !.
split(N,[X|Xs],RemovedElt,[X|Ys]) :-
    N1 is N - 1,
    split(N1,Xs,RemovedElt,Ys).

investigate([],_) :- !.
investigate([U|Units],Patterns) :-
    property(U,pattern,Data),
    p_investigate(Data,Patterns),
    investigate(Units,Patterns).

get_pats(Npats,Ipats,Result) :- get_pats(Npats,Ipats,Result,Ipats).

get_pats(0,_,[],_) :- !.
get_pats(N,[X|Xs],[X|Ys],Ipats) :-
    N1 is N - 1,
    get_pats(N1,Xs,Ys,Ipats).
get_pats(N,[],Ys,Ipats) :-
    get_pats(N,Ipats,Ys,Ipats).

property([],_,_) :- fail.	/* don't really need this */
property([Prop|_],P,Val) :-
    functor(Prop,P,_),!,
    arg(1,Prop,Val).
property([_|RProps],P,Val) :-
    property(RProps,P,Val).

p_investigate([],_).
p_investigate([D|Data],Patterns) :-
    p_match(Patterns,D),
    p_investigate(Data,Patterns).

p_match([],_).
p_match([P|Patterns],D) :-
    (match(D,P),fail; true),
    p_match(Patterns,D).

match([],[]) :- !.
match([X|PRest],[Y|SRest]) :-
    var(Y),!,X = Y,
    match(PRest,SRest).
match(List,[Y|Rest]) :- 
    nonvar(Y),Y = star(X),!,
    '$concat'(X,SRest,List),
    match(SRest,Rest).
match([X|PRest],[Y|SRest]) :-
    (atom(X) -> X = Y; match(X,Y)),
    match(PRest,SRest).

'$concat'([],L,L).
'$concat'([X|L1],L2,[X|L3]) :- '$concat'(L1,L2,L3).
