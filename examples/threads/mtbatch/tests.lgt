
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2012-08-16,
		comment is 'Unit tests for the "threads/mtbatch" example.'
	]).

	cover(mtbatch).

:- end_object.
