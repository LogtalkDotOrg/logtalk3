%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization((client_a::double([1,2,3], Doubles), write(Doubles))).


:- object(library_a).

	:- meta_predicate(map_(*, 2, *)).
	map_([], _, []).
	map_([X| Xs], Closure, [Y| Ys]) :-
		call(Closure, X, Y),
		map_(Xs, Closure, Ys).

	:- public(map/3).
	:- meta_predicate(map(*, 2, *)).
	map(In, scale(_), Out) :-			% the instantiated second argument
		!, map_(In, scale(3), Out).		% results in a compile-time error
	map(In, Closure, Out) :-
		map_(In, Closure, Out).

:- end_object.


:- object(client_a).

	:- public(double/2).
	double(Ints, Doubles) :-
		library_a::map(Ints, scale(2), Doubles).

	scale(Scale, X, Xscaled) :-
		Xscaled is X*Scale.

:- end_object.
