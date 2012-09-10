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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/06,
		comment is 'Unit tests for the "instmethods" example.']).

	unit(root).
	unit(instance1).
	unit(instance2).
	unit(instance3).

	test(instmethods_1) :-
		instance1::predicate_property(method, defined_in(Object)),
		Object == root.

	test(instmethods_2) :-
		instance2::predicate_property(method, defined_in(Object)),
		Object == instance2.

	test(instmethods_3) :-
		instance3::predicate_property(method, defined_in(Object)),
		Object == instance3.

:- end_object.
