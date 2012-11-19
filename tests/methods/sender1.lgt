
:- object(sender1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the sender/1 built-in method.'
	]).

	:- public(test_sender/1).
	test_sender(Sender) :-
		sender(Sender).

	test(sender_1) :-
		this(This),
		{This::test_sender(Sender)},
		Sender == user.

:- end_object.
