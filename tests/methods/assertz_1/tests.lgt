%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(assertz_1_test_object).

	:- set_logtalk_flag(dynamic_declarations, deny).

	:- public(p/1).
	:- dynamic(p/1).

	:- protected(q/2).
	:- dynamic(q/2).

	:- private(r/3).
	:- dynamic(r/3).

	:- public(s/4).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/06/07,
		comment is 'Unit tests for the assertz/1 built-in method.'
	]).

	throws(assertz_1_1, error(instantiation_error, logtalk(assertz_1_test_object::assertz(_),user))) :-
		{assertz_1_test_object::assertz(_)}.

	throws(assertz_1_2, error(instantiation_error, logtalk(assertz_1_test_object::assertz((_:-_)),user))) :-
		{assertz_1_test_object::assertz((_ :- _))}.

	throws(assertz_1_3, error(type_error(callable, 1), logtalk(assertz_1_test_object::assertz(1),user))) :-
		{assertz_1_test_object::assertz(1)}.

	throws(assertz_1_4, error(type_error(callable, 1), logtalk(assertz_1_test_object::assertz((1:-_)),user))) :-
		{assertz_1_test_object::assertz((1 :- _))}.

	throws(assertz_1_5, error(type_error(callable, 1), logtalk(assertz_1_test_object::assertz((p:-1)),user))) :-
		{assertz_1_test_object::assertz((p :- 1))}.

	throws(assertz_1_6, error(permission_error(modify, protected_predicate, q/2), logtalk(assertz_1_test_object::assertz(q(_,_)),user))) :-
		{assertz_1_test_object::assertz(q(_,_))}.

	throws(assertz_1_7, error(permission_error(modify, protected_predicate, q/2), logtalk(assertz_1_test_object::assertz((q(_,_) :-nl)),user))) :-
		{assertz_1_test_object::assertz((q(_,_) :- nl))}.

	throws(assertz_1_8, error(permission_error(modify, private_predicate, r/3), logtalk(assertz_1_test_object::assertz(r(_,_,_)),user))) :-
		{assertz_1_test_object::assertz(r(_,_,_))}.

	throws(assertz_1_9, error(permission_error(modify, private_predicate, r/3), logtalk(assertz_1_test_object::assertz((r(_,_,_) :-nl)),user))) :-
		{assertz_1_test_object::assertz((r(_,_,_) :- nl))}.

	throws(assertz_1_10, error(permission_error(modify, static_predicate, s/4), logtalk(assertz_1_test_object::assertz(s(_,_,_,_)),user))) :-
		{assertz_1_test_object::assertz(s(_,_,_,_))}.

	throws(assertz_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(assertz_1_test_object::assertz((s(_,_,_,_) :-nl)),user))) :-
		{assertz_1_test_object::assertz((s(_,_,_,_) :- nl))}.

	throws(assertz_1_12, error(permission_error(create, predicate_declaration, new/0), logtalk(assertz_1_test_object::assertz(new),user))) :-
		{assertz_1_test_object::assertz(new)}.

	succeeds(assertz_1_13) :-
		create_object(Object, [], [public(a/1), public(p/1)], []),
		Object::assertz(a(1)),
		Object::assertz(a(2)),
		Object::assertz(a(3)),
		Object::assertz((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [1,2,3],
		findall(Y, Object::p(Y), Ys),
		Ys == [1,2,3],
		abolish_object(Object).

	succeeds(assertz_1_14) :-
		create_object(Object, [], [set_logtalk_flag(dynamic_declarations, allow)], []),
		Object::assertz(a(1)),
		Object::assertz(a(2)),
		Object::assertz(a(3)),
		Object::assertz((p(X) :- a(X))),
		findall(X, Object::a(X), Xs),
		Xs == [1,2,3],
		findall(Y, Object::p(Y), Ys),
		Ys == [1,2,3],
		abolish_object(Object).

:- end_object.
