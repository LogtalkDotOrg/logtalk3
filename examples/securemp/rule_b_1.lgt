%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(client_b_1::test).


:- object(library_b_1).

	:- public(meta/2).
	:- meta_predicate(meta(0, 0)).
	meta(Goal1, Goal2) :-
		call(Goal1), call(Goal2).

	:- public(meta/1).
	:- meta_predicate(meta(0)).
	meta(Goal1) :-
		meta(Goal1, local).

	local :-
		write('local predicate in object library'), nl.

:- end_object.


:- object(client_b_1).

	:- public(test/0).
	test :-
		library_b_1::meta(goal).

	goal :-
	    write('goal meta-argument in object client'), nl.

	local :-
	    write('local predicate in object client'), nl.

:- end_object.
