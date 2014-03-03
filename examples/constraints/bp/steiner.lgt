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
By Neng-Fa Zhou

The ternary Steiner problem of order n is to find n(n-1)/6 sets of elements in {1,2,...,n} such that 
each set contains three elements and any two sets have at most one element in common. For example, 
the following shows a solution for size n=7:

      {1,2,3}, {1,4,5}, {1,6,7}, {2,4,6}, {2,5,7}, {3,4,7}, {3,5,6}

Problem taken from:
  C. Gervet: Interval Propagation to Reason about Sets: Definition and Implementation of a Practical 
  Language,  Constraints, An International Journal, vol.1, pp.191-246, 1997.
*/


:- object(steiner).

	:- public(go/0).

    go:-
        cputime(Start),
        top,
        cputime(End),
        T is End-Start,
        write('cputime='),write(T),nl.

    top:-
        steiner(9).
    top.

    steiner(N):-
        NoSets is N*(N-1)//6,
        length(Lsets,NoSets),
        Lsets in {}..{1..N},
        constrain(Lsets),
        labeling(Lsets),
        write(Lsets),nl.

    constrain([]).
    constrain([X|Xs]):-
        #X #= 3,
        constrain_pairs(X,Xs),
        constrain(Xs).

    constrain_pairs(_,[]).
    constrain_pairs(X,[Y|Ys]):-
        #(X /\ Y) #=< 1,
        constrain_pairs(X,Ys).

:- end_object.
