%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the threaded_peek/1-2 built-in predicate.'
	]).

	throws(threaded_peek_1_1, error(instantiation_error, logtalk(threaded_peek(_), _))) :-
		{threaded_peek(_)}.

	throws(threaded_peek_1_2, error(type_error(callable, 1), logtalk(threaded_peek(_), _))) :-
		{threaded_peek(1)}.

	throws(threaded_peek_2_1, error(instantiation_error, logtalk(threaded_peek(_,_), _))) :-
		{threaded_peek(_, _)}.

	throws(threaded_peek_2_2, error(type_error(callable, 1), logtalk(threaded_peek(_,_), _))) :-
		{threaded_peek(1, _)}.

	throws(threaded_peek_2_3, error(instantiation_error, logtalk(threaded_peek(_,_), _))) :-
		{threaded_peek(true, _)}.

	succeeds(threaded_peek_2_4) :-
		{threaded_call(true, Tag),
		 thread_sleep(1),
		 threaded_peek(true, Tag)}.

:- end_object.
