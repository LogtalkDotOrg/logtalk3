%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(catch(client_b_3_variant::test(_), Error, writeq(Error))).

:- object(library_b_3_variant).

	:- public(m/2).
	:- meta_predicate(m(2, *)).
	m(Closure, Arg) :-
		call(Closure, Arg, _).

:- end_object.


:- object(client_b_3_variant).

	:- public(test/1).
	test(X) :-
		library_b_3_variant::m(a, X).

	a(1). a(2).

	a(3, one). a(4, two).

:- end_object.
