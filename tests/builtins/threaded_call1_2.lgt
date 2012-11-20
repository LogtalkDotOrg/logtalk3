
:- object(threaded_call1_2,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the threaded_call/1-2 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_call1_1, error(instantiation_error, logtalk(threaded_call(_), _))) :-
		{threaded_call(_)}.

	throws(threaded_call1_2, error(type_error(callable, 1), logtalk(threaded_call(_), _))) :-
		{threaded_call(1)}.

	throws(threaded_call2_1, error(instantiation_error, logtalk(threaded_call(_,_), _))) :-
		{threaded_call(_, _)}.

	throws(threaded_call2_2, error(type_error(callable, 1), logtalk(threaded_call(_,_), _))) :-
		{threaded_call(1, _)}.

	throws(threaded_call2_3, error(type_error(variable, tag), logtalk(threaded_call(_,_), _))) :-
		{threaded_call(true, tag)}.

:- end_object.
