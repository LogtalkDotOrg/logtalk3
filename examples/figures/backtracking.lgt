
:- object(bk(_X_, _Y_, _Z_)).

	:- uses(coroutining, [when/2]).

	:- public(n/1).
	n(_X_).
	n(_Y_).
	n(_Z_).

	:- public(node/2).
	node(_X_, _Y_) :-
		when(ground(_X_-_Y_), _X_ > _Y_).
	node(_Y_, _Z_) :-
		when(ground(_Y_-_Z_), _Y_ > _Z_).

:- end_object.


end_of_file.


?- bk(A,B,C)::(n(2), n(1), n(3), node(A,B), node(B,C)), bagof(X, bk(A,B,C)::n(X), L).
A = 3,
B = 2,
C = 1,
L = [3, 2, 1] ;
false.
