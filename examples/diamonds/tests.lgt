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
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "diamonds" example.']).

	test(d1) :-
		d1::predicate_property(m, defined_in(Object)),
		Object == b1.

	test(d2_1) :-
		d2::predicate_property(m, defined_in(Object)),
		Object == d2.

	test(d2_2) :-
		d2::predicate_property(c2_m, defined_in(Object)),
		Object == c2.

	test(d3_1) :-
		d3::predicate_property(b3_m, defined_in(Object)),
		Object == b3.

	test(d3_2) :-
		d3::predicate_property(c3_m, defined_in(Object)),
		Object == c3.

	test(d3_3) :-
		d3::predicate_property(m, defined_in(Object)),
		Object == b3.

:- end_object.
