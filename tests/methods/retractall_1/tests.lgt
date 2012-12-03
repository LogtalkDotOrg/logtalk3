
:- object(retractall_1_test_object).

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
		comment is 'Unit tests for the retractall/1 built-in method.'
	]).

	throws(retractall_1_1, error(instantiation_error, logtalk(retractall_1_test_object::retractall(_),user))) :-
		{retractall_1_test_object::retractall(_)}.

	throws(retractall_1_2, error(type_error(callable, 1), logtalk(retractall_1_test_object::retractall(1),user))) :-
		{retractall_1_test_object::retractall(1)}.

	throws(retractall_1_3, error(permission_error(modify, protected_predicate, q/2), logtalk(retractall_1_test_object::retractall(q(_,_)),user))) :-
		{retractall_1_test_object::retractall(q(_,_))}.

	throws(retractall_1_4, error(permission_error(modify, private_predicate, r/3), logtalk(retractall_1_test_object::retractall(r(_,_,_)),user))) :-
		{retractall_1_test_object::retractall(r(_,_,_))}.

	throws(retractall_1_5, error(permission_error(modify, static_predicate, s/4), logtalk(retractall_1_test_object::retractall(s(_,_,_,_)),user))) :-
		{retractall_1_test_object::retractall(s(_,_,_,_))}.

	throws(retractall_1_6, error(existence_error(predicate_declaration, unknown/1), logtalk(retractall_1_test_object::retractall(unknown(_)),user))) :-
		{retractall_1_test_object::retractall(unknown(_))}.

:- end_object.
