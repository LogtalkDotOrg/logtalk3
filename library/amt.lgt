
:- object(amt).

	:- uses(meta, [map/2, map/3]).

	:- public(p/1).
	p(L) :-
		map(length_rev(2), L).

	length_rev(Length, List) :-
		list::length(List, Length).

	:- public(p2/1).
	p2(L) :-
		map(integer::succ, [1,2,3], L).

	:- public([t1/1, t2/1, t3/1]).
	%Zs == [a-1, b-2, c-3].

	t1(Zs) :-
		map([X,Y]>>(X=A-B,Y=B-A), [1-a,2-b,3-c], Zs).

	t2(Zs) :-
		map([X,B-A]>>(X=A-B), [1-a,2-b,3-c], Zs).

	t3(Zs) :-
		map([A-B,B-A]>>true, [1-a,2-b,3-c], Zs).

	:- public([a1/2, a2/2]).

	a1(Xs, Ys) :-
		map({Z}/[X,Y]>>(clpfd:(Z#=X+Y)), Xs, Ys).

	a2(Xs, Ys) :-
		map(axx(_), Xs, Ys).

	axx(Z, X, Y) :-
		clpfd:(Z#=X+Y).

	:- public(common_prefix/3).  

	common_prefix(Front, Xs, Ys) :-		% adapted from a Richard O'Keefe example
		meta::map({Front}/append(Front), Xs, Ys).

	append([], List, List).
	append([Head| Tail], List, [Head| Tail2]) :-
		append(Tail, List, Tail2).

	:- public(common_prefix2/3).  

	common_prefix2(Front, Xs, Ys) :-		% adapted from a Richard O'Keefe example
		meta::map({Front}/{append(Front)}, Xs, Ys).

:- end_object.
