
:- object(call_super_test_object_1).

	:- public(p/1).

	:- protected(q/1).
	q(1).

	:- protected(r/1).

	:- private(s/1).
	s(2).

:- end_object.


:- object(call_super_test_object_2,
	extends(call_super_test_object_1)).

	p(Goal) :-
		^^Goal.

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/05,
		comment is 'Unit tests for the (^^)/1 built-in control construct.'
	]).

	throws(call_super_1_1, error(instantiation_error,logtalk(^^_,call_super_test_object_2))) :-
		call_super_test_object_2::p(_).

	throws(call_super_1_2, error(type_error(callable,1),logtalk(^^1,call_super_test_object_2))) :-
		call_super_test_object_2::p(1).

	throws(call_super_1_3, error(permission_error(access,private_predicate,s/1),logtalk(^^s(_),call_super_test_object_2))) :-
		call_super_test_object_2::p(s(_)).

	throws(call_super_1_4, error(existence_error(predicate_declaration,t/1),logtalk(^^t(_),call_super_test_object_2))) :-
		call_super_test_object_2::p(t(_)).

	succeeds(call_super_1_5) :-
		call_super_test_object_2::p(q(X)),
		X == 1.

	fails(call_super_1_6) :-
		call_super_test_object_2::p(r(_)).

:- end_object.
