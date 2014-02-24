%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(sorting::sort(compare, [3,1,2,6,4,8,0], _)).


:- object(sorting).

	:- public(sort/3).
	:- meta_predicate(sort(3, *, *)).

	:- meta_predicate(merge(3, *, *, *)).

	sort(_, [], []) :- !.
	sort(_, [X], [X]) :- !.
	sort(Closure, [X, Y| Xs], Ys) :-
		split([X, Y| Xs], X1s, X2s),
		sort(Closure, X1s, Y1s),
		sort(Closure, X2s, Y2s),
		merge(Closure, Y1s, Y2s, Ys).

	split([], [], []).
	split([X| Xs], [X| Ys], Zs) :-
		split(Xs, Zs, Ys).

	merge(Closure, [X| Xs], [Y| Ys], Zs) :- !,
		call(Closure, Order, X, Y),
		merge(Order, Closure, [X| Xs], [Y| Ys], Zs).
	merge(_, [], Xs, Xs) :- !.
	merge(_, Xs, [], Xs).

	merge(<, Closure, [X| Xs], [Y| Ys], [X| Zs]) :-
		merge(Closure, Xs, [Y| Ys], Zs).
	merge(=, Closure, [X| Xs], [Y| Ys], [X| Zs]) :-
		merge(Closure, Xs, [Y| Ys], Zs).
	merge(>, Closure, [X| Xs], [Y| Ys], [Y| Zs]) :-
		merge(Closure, [X | Xs], Ys, Zs).

:- end_object.
