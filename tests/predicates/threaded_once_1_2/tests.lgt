
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the threaded_once/1-2 built-in predicate.'
	]).

	throws(threaded_once_1_1, error(instantiation_error, logtalk(threaded_once(_), _))) :-
		{threaded_once(_)}.

	throws(threaded_once_1_2, error(type_error(callable, 1), logtalk(threaded_once(_), _))) :-
		{threaded_once(1)}.

	throws(threaded_once_2_1, error(instantiation_error, logtalk(threaded_once(_,_), _))) :-
		{threaded_once(_, _)}.

	throws(threaded_once_2_2, error(type_error(callable, 1), logtalk(threaded_once(_,_), _))) :-
		{threaded_once(1, _)}.

	throws(threaded_once_2_3, error(type_error(variable, tag), logtalk(threaded_once(_,_), _))) :-
		{threaded_once(true, tag)}.

:- end_object.
