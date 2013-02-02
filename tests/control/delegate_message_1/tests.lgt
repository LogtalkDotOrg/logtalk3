
:- object(delegate_message_test_object_1).

	:- public(s/1).
	s(X) :-
		sender(X).

:- end_object.



:- object(delegate_message_test_object_2).

	:- public(p/1).
	p(X) :-
		delegate_message_test_object_1::s(X).

	:- public(q/1).
	q(X) :-
		[delegate_message_test_object_1::s(X)].

	:- public(r/0).
	r :-
		[user::foo].

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/02/01,
		comment is 'Unit tests for the []/1 built-in control construct.'
	]).

	succeeds(delegate_message_1_1) :-
		{delegate_message_test_object_2::p(X)},
		X == delegate_message_test_object_2.

	succeeds(delegate_message_1_2) :-
		{delegate_message_test_object_2::q(X)},
		X == user.

	throws(delegate_message_1_3, error(permission_error(access, object, user), logtalk([user::foo], delegate_message_test_object_2))) :-
		{delegate_message_test_object_2::r}.

:- end_object.
