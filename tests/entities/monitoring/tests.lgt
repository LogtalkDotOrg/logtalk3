
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "monitoring" built-in protocol.'
	]).

	test(monitoring_1) :-
		current_protocol(monitoring),
		protocol_property(monitoring, built_in).

:- end_object.
