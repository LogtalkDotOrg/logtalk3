
:- object(external_call_test_object).

	:- public(p/1).
	p(Goal) :-
		{Goal}.

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the {}/1 built-in control construct.'
	]).

	throws(external_call_1_1, error(instantiation_error,_)) :-
		external_call_test_object::p(_).

	throws(external_call_1_2, error(type_error(callable,1),_)) :-
		external_call_test_object::p(1).

	succeeds(external_call_1_3) :-
		external_call_test_object::p(true).

	fails(external_call_1_4) :-
		external_call_test_object::p(fail).

:- end_object.
