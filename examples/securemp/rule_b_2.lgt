%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- initialization(catch(client_b_2::test, Error, (writeq(Error), nl))).


:- object(library_b_2).

	:- meta_predicate(meta(0)).
	meta(Goal) :-
		call(Goal).

	:- public(pred/1).
	pred(Arg) :-
		meta(Arg).

:- end_object.


:- object(client_b_2).

	:- public(test/0).
	test :-
		library_b_2::pred(term).

	term :-
		write('Some local, private predicate.').

:- end_object.
