%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples
% distributed with B-Prolog 7.1 (August 2008)


/* 
   Find the largest clique in a graph. A clique is a sub-graph that is 
   complete, i.e., any two vertices are connected.
   Problem Source: http://www.cs.sunysb.edu/~algorith/
   Algorithm: native generate-test algorithm. The fast one is in clique_fast.pl
   Program written by Neng-Fa Zhou, 3. 2002
*/


:- object(clique).

	:- public(go/0).

    go:-
        cputime(Start),
        top,
        cputime(End),        	
        T is End-Start,
        write('cputime='),write(T),nl.

    top:-
        vertices(Vs),
        clique(Vs,17).

    clique(Vs,N):-
        write(clique(N)),nl,
        {list_to_set(Vs,VSet)},     % list_to_set/2 is not declared built-in...
        Clique in {}..VSet,
        #Clique #= N,
        indomain(Clique),
        {set_to_list(Clique,List)}, % set_to_list/2 is not declared built-in...
        all_connected(List),
        write(Clique),nl.
    clique(Vs,N):-
        N1 is N-1,
        clique(Vs,N1).

    all_connected([]).
    all_connected([V|Vs]):-
        all_connected(V,Vs),
        all_connected(Vs).

    all_connected(_,[]).
    all_connected(V,[V1|Vs]):-
        connected(V,V1),
        all_connected(V,Vs).

    vertices([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]).

    connected(X,Y):-edge(X,Y),!.
    connected(X,Y):-edge(Y,X).

    %% The database can be accessed fast if written in matching clause
    % by taking advantage of the information that both arguments are input
    % edge(1,2).   into    edge(1,2) => true.
    edge(1,2).
    edge(1,12).
    edge(1,14).
    edge(1,15).
    edge(2,3).
    edge(2,15).
    edge(3,4).
    edge(3,15).
    edge(4,5).
    edge(4,15).
    edge(5,6).
    edge(5,15).
    edge(5,16).
    edge(6,7).
    edge(6,16).
    edge(7,8).
    edge(7,16).
    edge(7,17).
    edge(8,9).
    edge(8,17).
    edge(9,10).
    edge(9,13).
    edge(9,17).
    edge(10,11).
    edge(10,13).
    edge(11,12).
    edge(11,13).
    edge(12,13).
    edge(12,14).
    edge(13,14).
    edge(13,15).
    edge(13,16).
    edge(13,17).
    edge(14,15).
    edge(14,16).
    edge(14,17).
    edge(15,16).
    edge(15,17).
    edge(16,17).

:- end_object.
