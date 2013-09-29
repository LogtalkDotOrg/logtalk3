
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "user" built-in object.'
	]).

	test(user_1) :-
		current_object(user),
		object_property(user, built_in).

:- end_object.
