
:- category(category_call_test_category).

	:- public(p/1).

	:- public(q/1).
	q(1).

	:- protected(r/1).
	r(2).

	:- private(s/1).
	s(3).

	:- private(t/1).

:- end_category.


:- object(category_call_test_object,
	imports(category_call_test_category)).

	p(Goal) :-
		:Goal.

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/05,
		comment is 'Unit tests for the (:)/1 built-in control construct.'
	]).

	throws(category_call_1_1, error(instantiation_error,logtalk(:_,category_call_test_object))) :-
		category_call_test_object::p(_).

	throws(category_call_1_2, error(type_error(callable,1),logtalk(:1,category_call_test_object))) :-
		category_call_test_object::p(1).

	throws(category_call_1_3, error(existence_error(predicate_declaration,u/1),logtalk(:u(_),category_call_test_object))) :-
		category_call_test_object::p(u(_)).

	succeeds(category_call_1_4) :-
		category_call_test_object::p(q(X)),
		X == 1.

	succeeds(category_call_1_5) :-
		category_call_test_object::p(r(X)),
		X == 2.

	succeeds(category_call_1_6) :-
		category_call_test_object::p(s(X)),
		X == 3.

	fails(category_call_1_7) :-
		category_call_test_object::p(t(_)).

:- end_object.
