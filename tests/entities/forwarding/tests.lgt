
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "forwarding" built-in protocol.'
	]).

	test(forwarding_1) :-
		current_protocol(forwarding).

:- end_object.
