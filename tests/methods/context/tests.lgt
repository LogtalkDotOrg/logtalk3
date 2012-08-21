
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/06/05,
		comment is 'Unit tests for the execution contex built-in methods.']).

	:- public(test_sender/1).
	test_sender(Sender) :-
		sender(Sender).

	test(this_1) :-
		this(This),
		This == tests.

	test(sender_1) :-
		{tests::test_sender(Sender)},
		Sender == user.

:- end_object.
