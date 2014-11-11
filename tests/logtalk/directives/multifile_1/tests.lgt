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
		date is 2014/09/04,
		comment is 'Unit tests for the multifile/1 built-in directive.'
	]).

	:- multifile(multifile_primary_object::m1/1).
	multifile_primary_object::m1(3).

	:- multifile(multifile_primary_object::m2/1).
	multifile_primary_object::m2(3).

	:- multifile(multifile_primary_category::n1/1).
	multifile_primary_category::n1(3).

	test(multifile_1_01) :-
		setof(X, multifile_primary_object::m1(X), L),
		L == [1, 2, 3, 4, 5].

	test(multifile_1_02) :-
		multifile_primary_object::predicate_property(m1(_), (multifile)),
		multifile_primary_object::predicate_property(m1(_), static).

	test(multifile_1_03) :-
		setof(X, multifile_primary_object::m2(X), L),
		L == [1, 2, 3, 4, 5].

	test(multifile_1_04) :-
		multifile_primary_object::predicate_property(m2(_), (multifile)),
		multifile_primary_object::predicate_property(m2(_), (dynamic)).

	test(multifile_1_05) :-
		multifile_other_object::predicate_property(n1(_), (multifile)),
		multifile_other_object::predicate_property(n1(_), static).

	test(multifile_1_06) :-
		setof(X, multifile_other_object::n1(X), L),
		L == [1, 2, 3, 4].

	test(multifile_1_07) :-
		multifile_primary_object(1)::a(1, Y),
		Y == 1.

	test(multifile_1_08) :-
		multifile_primary_object(Y)::a(1, 1),
		Y == 1.

	test(multifile_1_09) :-
		multifile_primary_object(X)::a(1, Y),
		X == Y.

	test(multifile_1_10) :-
		multifile_primary_object(2)::a(2, Y),
		Y == 2.

	test(multifile_1_11) :-
		multifile_primary_object(Y)::a(2, 2),
		Y == 2.

	test(multifile_1_12) :-
		multifile_primary_object(X)::a(2, Y),
		X == Y.

	test(multifile_1_13) :-
		multifile_primary_object(_)::predicate_property(a(_, _), (multifile)),
		multifile_primary_object(_)::predicate_property(a(_, _), static).

	test(multifile_1_14) :-
		multifile_other_object(1)::b(1, Y),
		Y == 1.

	test(multifile_1_15) :-
		multifile_other_object(Y)::b(1, 1),
		Y == 1.

	test(multifile_1_16) :-
		multifile_other_object(X)::b(1, Y),
		X == Y.

	test(multifile_1_17) :-
		multifile_other_object(_)::predicate_property(b(_, _), (multifile)),
		multifile_other_object(_)::predicate_property(b(_, _), static).

:- end_object.
