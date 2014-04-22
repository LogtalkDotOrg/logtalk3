%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/04/22,
		comment is 'Unit tests for the "complements/restrict" example.'
	]).

	cover(vault).
	cover(my_vault).
	cover(hacker).

	test(complements_restrict_1) :-
		complements_object(Category, Object),
		Category == hacker, Object == my_vault.

	test(complements_restrict_2) :-
		my_vault::open('!"#$%&/()=').

	test(complements_restrict_3) :-
		\+ my_vault::open('1234567890').

:- end_object.
