
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "expanding" built-in protocol.'
	]).

	test(expanding_1) :-
		current_protocol(expanding),
		protocol_property(expanding, built_in).

:- end_object.
