
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
		date is 2012/12/03,
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

	throws(assertz_1_7, error(permission_error(modify, protected_predicate, q/2), logtalk(assertz_1_test_object::assertz((q(_,_):-nl)),user))) :-
		{assertz_1_test_object::assertz((q(_,_) :- nl))}.

	throws(assertz_1_8, error(permission_error(modify, private_predicate, r/3), logtalk(assertz_1_test_object::assertz(r(_,_,_)),user))) :-
		{assertz_1_test_object::assertz(r(_,_,_))}.

	throws(assertz_1_9, error(permission_error(modify, private_predicate, r/3), logtalk(assertz_1_test_object::assertz((r(_,_,_):-nl)),user))) :-
		{assertz_1_test_object::assertz((r(_,_,_) :- nl))}.

	throws(assertz_1_10, error(permission_error(modify, static_predicate, s/4), logtalk(assertz_1_test_object::assertz(s(_,_,_,_)),user))) :-
		{assertz_1_test_object::assertz(s(_,_,_,_))}.

	throws(assertz_1_11, error(permission_error(modify, static_predicate, s/4), logtalk(assertz_1_test_object::assertz((s(_,_,_,_):-nl)),user))) :-
		{assertz_1_test_object::assertz((s(_,_,_,_) :- nl))}.

	throws(assertz_1_12, error(permission_error(create, predicate_declaration, new/0), logtalk(assertz_1_test_object::assertz(new),user))) :-
		{assertz_1_test_object::assertz(new)}.

:- end_object.
