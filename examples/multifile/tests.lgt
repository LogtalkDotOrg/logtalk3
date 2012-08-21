%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "multifile" example.']).

	unit(main).
	unit(other).
	unit(more).

	test(multifile_1) :-
		findall(X, main::a(X), Solutions),
		Solutions == [1, 2, 3, 4, 5].

	test(multifile_2) :-
		main::current_predicate(a/1),
		main::predicate_property(a(_), public),
		main::predicate_property(a(_), multifile).

	test(multifile_3) :-
		findall(X, main::b(X), Solutions),
		Solutions == [one, two, three].

	test(multifile_4) :-
		main::current_predicate(b/1),
		main::predicate_property(b(_), public),
		main::predicate_property(b(_), multifile).

:- end_object.
