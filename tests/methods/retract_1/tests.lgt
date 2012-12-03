
:- object(retract_1_test_object).

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
		comment is 'Unit tests for the retract/1 built-in method.'
	]).

	throws(retract_1_1, error(instantiation_error, logtalk(retract_1_test_object::retract(_),user))) :-
		{retract_1_test_object::retract(_)}.

	throws(retract_1_2, error(instantiation_error, logtalk(retract_1_test_object::retract((_:-_)),user))) :-
		{retract_1_test_object::retract((_ :- _))}.

	throws(retract_1_3, error(type_error(callable, 1), logtalk(retract_1_test_object::retract(1),user))) :-
		{retract_1_test_object::retract(1)}.

	throws(retract_1_4, error(type_error(callable, 1), logtalk(retract_1_test_object::retract((1:-_)),user))) :-
		{retract_1_test_object::retract((1 :- _))}.

	throws(retract_1_5, error(permission_error(modify, protected_predicate, q/2), logtalk(retract_1_test_object::retract(q(_,_)),user))) :-
		{retract_1_test_object::retract(q(_,_))}.

	throws(retract_1_6, error(permission_error(modify, private_predicate, r/3), logtalk(retract_1_test_object::retract(r(_,_,_)),user))) :-
		{retract_1_test_object::retract(r(_,_,_))}.

	throws(retract_1_7, error(permission_error(modify, static_predicate, s/4), logtalk(retract_1_test_object::retract(s(_,_,_,_)),user))) :-
		{retract_1_test_object::retract(s(_,_,_,_))}.

	throws(retract_1_8, error(existence_error(predicate_declaration, unknown/1), logtalk(retract_1_test_object::retract(unknown(_)),user))) :-
		{retract_1_test_object::retract(unknown(_))}.

:- end_object.
