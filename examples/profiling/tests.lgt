
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/13,
		comment is 'Unit tests for the "profiling" example.']).

	unit(message_counter).
	unit(stop_watch).
	unit(timer).

:- end_object.
