%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(catch(client_b_3::test(_), Error, (writeq(Error), nl))).


:- object(library_b_3).

	:- public(m/2).
	:- meta_predicate(m(1, *)).
	m(Closure, Arg) :-
		Closure =.. List,
		my_append(List, [Arg, _], NewList),
		Call =.. NewList,
		call(Call).

	my_append([], List, List).
	my_append([Head| Tail], List, [Head| Tail2]) :-
		my_append(Tail, List, Tail2).

:- end_object.


:- object(client_b_3).

	:- public(test/1).
	test(X) :-
		library_b_3::m(a, X).

	a(1). a(2).

	a(3, one). a(4, two).

:- end_object.
