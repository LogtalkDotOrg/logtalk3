
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/08/13,
		comment is 'Unit tests for the "debug_hooks" example.'
	]).

	cover(hook_debug).
	cover(hook_production).
	cover(hook(_)).

:- end_object.
