
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "logtalk" built-in object.'
	]).

	test(logtalk_1) :-
		current_object(logtalk).

:- end_object.
