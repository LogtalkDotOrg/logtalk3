
:- object(threaded_peek1_2,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the threaded_peek/1-2 built-in predicate.'
	]).

	throws(threaded_peek1_1, error(instantiation_error, logtalk(threaded_peek(_), _))) :-
		{threaded_peek(_)}.

	throws(threaded_peek1_2, error(type_error(callable, 1), logtalk(threaded_peek(_), _))) :-
		{threaded_peek(1)}.

	throws(threaded_peek2_1, error(instantiation_error, logtalk(threaded_peek(_,_), _))) :-
		{threaded_peek(_, _)}.

	throws(threaded_peek2_2, error(type_error(callable, 1), logtalk(threaded_peek(_,_), _))) :-
		{threaded_peek(1, _)}.

	throws(threaded_peek2_3, error(instantiation_error, logtalk(threaded_peek(_,_), _))) :-
		{threaded_peek(true, _)}.

	succeeds(threaded_peek2_4) :-
		{threaded_call(true, Tag),
		 thread_sleep(1),
		 threaded_peek(true, Tag)}.

:- end_object.
