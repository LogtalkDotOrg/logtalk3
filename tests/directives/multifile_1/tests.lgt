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
		date is 2014/08/19,
		comment is 'Unit tests for the multifile/1 built-in directive.'
	]).

	:- multifile(multifile_test_object::m1/1).
	multifile_test_object::m1(3).

	:- multifile(multifile_test_object::m2/1).
	multifile_test_object::m2(3).

	test(multifile_1_01) :-
		setof(X, multifile_test_object::m1(X), L),
		L == [1, 2, 3, 4, 5].

	test(multifile_1_02) :-
		multifile_test_object::predicate_property(m1(_), (multifile)),
		multifile_test_object::predicate_property(m1(_), static).

	test(multifile_1_03) :-
		setof(X, multifile_test_object::m2(X), L),
		L == [1, 2, 3, 4, 5].

	test(multifile_1_04) :-
		multifile_test_object::predicate_property(m2(_), (multifile)),
		multifile_test_object::predicate_property(m2(_), (dynamic)).

	test(multifile_1_05) :-
		multifile_test_object(1)::a(1, Y),
		Y == 1.

	test(multifile_1_06) :-
		multifile_test_object(Y)::a(1, 1),
		Y == 1.

	test(multifile_1_07) :-
		multifile_test_object(X)::a(1, Y),
		X == Y.

	test(multifile_1_08) :-
		multifile_test_object(2)::a(2, Y),
		Y == 2.

	test(multifile_1_09) :-
		multifile_test_object(Y)::a(2, 2),
		Y == 2.

	test(multifile_1_10) :-
		multifile_test_object(X)::a(2, Y),
		X == Y.

	test(multifile_1_11) :-
		multifile_test_object(_)::predicate_property(a(_, _), (multifile)).

:- end_object.
