m(H, [H| _]).
m(H, [_| T]) :-
	m(H, T).

:- initialization((m(X,[1,2,3]), integer(X))).
