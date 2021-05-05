:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:0:0,
		author is 'Paulo Moura',
		date is 2016-03-02,
		comment is 'Unit tests for the db predicates.'
	]).

	:- uses(user, [
		db_clear/1, db_reconsult/2, xsave/2,
		db_call/2,
		db_dynamic/2, db_abolish/2, db_is_dynamic/2,
		db_asserta/2, db_assertz/2, db_assert_unique/2,
		db_clause/3,
		db_retract/2, db_retractall/2,
		db_current_predicate/2
	]).

	:- discontiguous([
		succeeds/1, fails/1, throws/2, deterministic/1
	]).

	% db_clear/1 tests

	succeeds(db_clear_1_1) :-
		db_clear(foo).

	throws(db_clear_1_2, _) :-
		db_clear(_).

	succeeds(db_clear_1_3) :-
		db_assertz(foo, a(1)),
		db_clear(foo),
		\+ db_is_dynamic(foo, a(_)).

	deterministic(db_clear_1_4) :-
		db_clear(foo).

	% db_dynamic/2 tests

	succeeds(db_dynamic_2_1) :-
		db_clear(foo),
		db_dynamic(foo, dyn1/0),
		db_is_dynamic(foo, dyn1).

	succeeds(db_dynamic_2_2) :-
		db_clear(foo),
		\+ db_is_dynamic(foo, dyn2).

	succeeds(db_dynamic_2_3) :-
		db_clear(foo),
		db_dynamic(foo, dyn3/1),
		db_is_dynamic(foo, dyn3(_)).

	succeeds(db_dynamic_2_4) :-
		db_clear(foo),
		db_dynamic(foo, dyn4/2),
		db_dynamic(foo, dyn4/2),
		db_is_dynamic(foo, dyn4(_, _)).

	deterministic(db_dynamic_2_5) :-
		db_clear(foo),
		db_dynamic(foo, dyn5/3).		

	throws(db_dynamic_2_6, _) :-
		db_dynamic(_, dyn6/3).

	throws(db_dynamic_2_7, _) :-
		db_dynamic(foo, _).

	throws(db_dynamic_2_8, _) :-
		db_dynamic(foo, dyn8).

	throws(db_dynamic_2_9, _) :-
		db_dynamic(foo, dyn9/_).

	throws(db_dynamic_2_10, _) :-
		db_dynamic(foo, _/1).

	throws(db_dynamic_2_11, _) :-
		db_dynamic(foo, dyn10/a).

	% db_asserta/2 tests

	succeeds(db_asserta_2_1) :-
		db_clear(foo),
		db_asserta(foo, ba1(1)),
		db_is_dynamic(foo, ba1(_)).

	succeeds(db_asserta_2_2) :-
		db_clear(foo),
		db_asserta(foo, ba2(1)),
		\+ db_call(foo, ba2(2)).

	succeeds(db_asserta_2_3) :-
		db_clear(foo),
		db_asserta(foo, ba3(1)),
		db_asserta(foo, ba3(2)),
		db_asserta(foo, ba3(3)),
		bagof(X, db_call(foo, ba3(X)), Xs),
		Xs == [3, 2, 1].

	deterministic(db_asserta_2_4) :-
		db_clear(foo),
		db_asserta(foo, ba4(1)).

	throws(db_asserta_2_5, _) :-
		db_asserta(_, ba5(_)).

	throws(db_asserta_2_6, _) :-
		db_asserta(foo, _).

	% db_assertz/2 tests

	succeeds(db_assertz_2_1) :-
		db_clear(foo),
		db_assertz(foo, bz1(1)),
		db_is_dynamic(foo, bz1(_)).

	succeeds(db_assertz_2_2) :-
		db_clear(foo),
		db_assertz(foo, bz2(1)),
		\+ db_call(foo, bz2(2)).

	succeeds(db_assertz_2_3) :-
		db_clear(foo),
		db_assertz(foo, bz3(1)),
		db_assertz(foo, bz3(2)),
		db_assertz(foo, bz3(3)),
		bagof(X, db_call(foo, bz3(X)), Xs),
		Xs == [1, 2, 3].

	deterministic(db_assertz_2_4) :-
		db_clear(foo),
		db_assertz(foo, bz4(1)).

	throws(db_assertz_2_5, _) :-
		db_assertz(_, bz5(_)).

	throws(db_assertz_2_6, _) :-
		db_assertz(foo, _).

	% db_assert_unique/2 tests

	succeeds(db_assert_unique_2_1) :-
		db_clear(foo),
		db_assert_unique(foo, au1(1)),
		db_assert_unique(foo, au1(1)),
		bagof(X, db_call(foo, au1(X)), Xs),
		Xs == [1].

	deterministic(db_assert_unique_2_2) :-
		db_clear(foo),
		db_assert_unique(foo, au2(1)).

	throws(db_assert_unique_2_3, _) :-
		db_assert_unique(_, au3(_)).

	throws(db_assert_unique_2_4, _) :-
		db_assert_unique(foo, _).

	% db_current_predicate/2 tests

	succeeds(db_current_predicate_2_1) :-
		db_clear(foo),
		db_assertz(foo, cu1(1)),
		db_current_predicate(foo, cu1/1).

	succeeds(db_current_predicate_2_2) :-
		db_clear(foo),
		db_assertz(foo, cu2(1)),
		db_retractall(foo, cu2(_)),
		db_current_predicate(foo, cu2/1).

	deterministic(db_current_predicate_2_3) :-
		db_clear(foo),
		db_assertz(foo, cu3(1)),
		db_current_predicate(foo, cu3/1).

	throws(db_current_predicate_2_4, _) :-
		db_current_predicate(_, cu4/3).

	throws(db_current_predicate_2_5, _) :-
		db_current_predicate(foo, _).

	throws(db_current_predicate_2_6, _) :-
		db_current_predicate(foo, cu6).

	throws(db_current_predicate_2_7, _) :-
		db_current_predicate(foo, cu7/_).

	throws(db_current_predicate_2_8, _) :-
		db_current_predicate(foo, _/1).

	throws(db_current_predicate_2_9, _) :-
		db_current_predicate(foo, cu9/a).

	% db_clause/3 tests

	succeeds(db_clause_2_1) :-
		db_clear(foo),
		db_assertz(foo, cl1(1)),
		db_clause(foo, cl1(X), Body),
		X == 1, Body == true.

	succeeds(db_clause_2_2) :-
		db_clear(foo),
		db_assertz(foo, cl2(1)),
		\+ db_clause(foo, cl2(2), _).

	succeeds(db_clause_2_3) :-
		db_clear(foo),
		db_assertz(foo, (cl3(1) :- clb3(2))),
		db_clause(foo, cl3(X), Body),
		X == 1, Body == clb3(2).

	throws(db_clause_2_4, _) :-
		db_clause(_, cl4(_), _).

	throws(db_clause_2_5, _) :-
		db_clause(foo, _, _).

	throws(db_clause_2_6, _) :-
		db_clause(foo, 1, _).

	% db_retract/2 tests

	succeeds(db_retract_2_1) :-
		db_clear(foo),
		db_assertz(foo, r1(1)),
		db_retract(foo, r1(_)),
		db_is_dynamic(foo, r1(_)).

	succeeds(db_retract_2_2) :-
		db_clear(foo),
		db_assertz(foo, r2(1)),
		db_assertz(foo, r2(2)),
		db_assertz(foo, r2(3)),
		bagof(X, db_retract(foo, r2(X)), Xs),
		Xs == [1, 2, 3].

	succeeds(db_retract_2_3) :-
		db_clear(foo),
		db_assertz(foo, r3(1)),
		\+ db_retract(foo, r3(2)).

	succeeds(db_retract_2_4) :-
		db_clear(foo),
		db_assertz(foo, r4(1)),
		db_retract(foo, r4(_)),
		\+ db_retract(foo, r4(_)).

	throws(db_retract_2_5, _) :-
		db_retract(_, r5(_)).

	throws(db_retract_2_6, _) :-
		db_retract(foo, _).

	% db_retractall/2 tests

	succeeds(db_retractall_2_1) :-
		db_clear(foo),
		db_assertz(foo, ra1(1)),
		db_retractall(foo, ra1(_)),
		db_is_dynamic(foo, ra1(_)).

	succeeds(db_retractall_2_2) :-
		db_clear(foo),
		db_assertz(foo, ra2(1)),
		db_assertz(foo, ra2(2)),
		db_assertz(foo, ra2(3)),
		db_retractall(foo, ra2(_)),
		\+ db_call(foo, ra2(_)).

	succeeds(db_retractall_2_3) :-
		db_clear(foo),
		db_assertz(foo, ra3(1)),
		db_assertz(foo, ra3(2)),
		db_assertz(foo, ra3(3)),
		db_retractall(foo, ra3(2)),
		bagof(X, db_call(foo, ra3(X)), Xs),
		Xs == [1, 3].

	deterministic(db_retractall_2_4) :-
		db_clear(foo),
		db_dynamic(foo, ra4/1),
		db_retractall(foo, ra4(_)).

	throws(db_retractall_2_5, _) :-
		db_retractall(_, ra5(_)).

	throws(db_retractall_2_6, _) :-
		db_retractall(foo, _).

	% db_is_dynamic/2 tests

	succeeds(db_is_dynamic_2_1) :-
		db_clear(foo),
		db_asserta(foo, isd1(1)),
		db_is_dynamic(foo, isd1(_)).

	succeeds(db_is_dynamic_2_2) :-
		db_clear(foo),
		db_assertz(foo, isd2(1)),
		db_is_dynamic(foo, isd2(_)).

	deterministic(db_is_dynamic_2_3) :-
		db_clear(foo),
		db_assertz(foo, isd3(1)),
		db_is_dynamic(foo, isd3(_)).

	throws(db_is_dynamic_2_4, _) :-
		db_is_dynamic(_, isd4(_)).

	throws(db_is_dynamic_2_5, _) :-
		db_is_dynamic(foo, _).

	% db_abolish/2 tests

	succeeds(db_abolish_2_1) :-
		db_clear(foo),
		db_abolish(foo, ab1/1),
		\+ db_is_dynamic(foo, ab1(_)).

	succeeds(db_abolish_2_2) :-
		db_clear(foo),
		db_assertz(foo, ab2(1)),
		db_abolish(foo, ab2/1),
		\+ db_is_dynamic(foo, ab2(_)).

	succeeds(db_abolish_2_3) :-
		db_clear(foo),
		db_dynamic(foo, ab3/0),
		db_abolish(foo, ab3/0),
		\+ db_is_dynamic(foo, ab3).

	succeeds(db_abolish_2_4) :-
		db_clear(foo),
		db_dynamic(named_database, ab4/1),
		db_abolish(named_database, ab4/1),
		\+ db_is_dynamic(foo, ab4(_)).

	succeeds(db_abolish_2_5) :-
		db_clear(foo),
		db_dynamic(named_database, ab5/1),
		db_abolish(named_database, ab5/1),
		\+ db_current_predicate(foo, ab5/1).

	deterministic(db_abolish_2_6) :-
		db_clear(foo),
		db_abolish(foo, ab6/2).

	throws(db_abolish_2_7, _) :-
		db_abolish(_, ab7/3).

	throws(db_abolish_2_8, _) :-
		db_abolish(foo, _).

	throws(db_abolish_2_9, _) :-
		db_abolish(foo, ab9).

	throws(db_abolish_2_10, _) :-
		db_abolish(foo, ab10/_).

	throws(db_abolish_2_11, _) :-
		db_abolish(foo, _/1).

	throws(db_abolish_2_12, _) :-
		db_abolish(foo, ab12/a).

	% db_reconsult/2 tests

	succeeds(db_reconsult_2_1) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'data_in.pl', In),
		atom_concat(Directory, 'data_out.pl', Out),
		db_reconsult(In, foo),
		xsave(Out, foo).

	succeeds(db_reconsult_2_2) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'data_in.pl', In),
		db_reconsult(In, foo),
		setof(X, db_call(foo, s(X)), Xs),
		Xs == [1, 2, 3],
		setof(Y, db_call(foo, d(Y)), Ys),
		Ys == [1, 2, 3].

	succeeds(db_reconsult_2_3) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'dump.pl', File),
		db_dynamic(foo, rec3/1),
		db_assertz(foo, rec3(1)),
		db_assertz(foo, rec3(2)),
		db_assertz(foo, rec3(3)),
		xsave(File, foo),
		db_clear(foo),
		db_reconsult(File, foo),
		bagof(X, db_call(foo, rec3(X)), Xs),
		Xs == [1, 2, 3].

	succeeds(db_reconsult_2_4) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'empty.pl', In),
		db_reconsult(In, foo).

	deterministic(db_reconsult_2_5) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'data_in.pl', In),
		db_reconsult(In, foo).

	throws(db_reconsult_2_6, _) :-
		db_reconsult(_, foo).

	throws(db_reconsult_2_7, _) :-
		db_reconsult(file, _).

	% xsave/2 tests

	succeeds(xsave_2_1) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'dump1.pl', File),
		xsave(File, foo).

	succeeds(xsave_2_2) :-
		db_clear(foo),
		db_dynamic(foo, xs1/1),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'dump2.pl', File),
		xsave(File, foo).

	deterministic(xsave_2_3) :-
		db_clear(foo),
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'dump1.pl', File),
		xsave(File, foo).

	throws(xsave_2_4, _) :-
		xsave(_, foo).

	throws(xsave_2_5, _) :-
		xsave(file, _).

:- end_object.
