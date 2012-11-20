
:- object(threaded_ignore1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the threaded_ignore/1 built-in predicate.'
	]).

	throws(threaded_ignore1_1, error(instantiation_error, logtalk(threaded_ignore(_), _))) :-
		{threaded_ignore(_)}.

	throws(threaded_ignore1_2, error(type_error(callable, 1), logtalk(threaded_ignore(_), _))) :-
		{threaded_ignore(1)}.

	succeeds(threaded_ignore1_3) :-
		{threaded_ignore(true)}.

	succeeds(threaded_ignore1_4) :-
		{threaded_ignore(fail)}.

:- end_object.
