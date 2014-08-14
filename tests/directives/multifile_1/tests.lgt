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
		date is 2014/08/14,
		comment is 'Unit tests for the multifile/1 built-in directive.'
	]).

	:- multifile(multifile_test_object::m1/1).
	multifile_test_object::m1(3).

	:- multifile(multifile_test_object::m2/1).
	multifile_test_object::m2(3).

	:- multifile(multifile_test_category::n1/1).
	multifile_test_category::n1(3).

	test(multifile_1_1) :-
		setof(X, multifile_test_object::m1(X), L),
		L == [1, 2, 3].

	test(multifile_1_2) :-
		multifile_test_object::predicate_property(m1(_), (multifile)),
		multifile_test_object::predicate_property(m1(_), static).

	test(multifile_1_3) :-
		setof(X, multifile_test_object::m2(X), L),
		L == [1, 2, 3].

	test(multifile_1_4) :-
		multifile_test_object::predicate_property(m2(_), (multifile)),
		multifile_test_object::predicate_property(m2(_), (dynamic)).

	test(multifile_1_5) :-
		setof(X, multifile_test_other::n1(X), L),
		L == [1, 2, 3, 4].

	test(multifile_1_6) :-
		multifile_test_other::predicate_property(n1(_), (multifile)).

	test(multifile_1_7) :-
		\+ multifile_test_other::n2(_).

	test(multifile_1_8) :-
		multifile_test_other::predicate_property(n2(_), (multifile)).

	test(multifile_1_9) :-
		multifile_test_object(2)::a(X, Y),
		X == 1, Y == 2.

:- end_object.
