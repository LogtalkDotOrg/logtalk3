
:- module(a, [b/1]).

b(Z) :-
	zlist::zip([1,2,3,4,5],Z).
