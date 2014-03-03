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
		comment is 'Unit tests for the threaded/1 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_1_1, error(instantiation_error, logtalk(threaded(_), _))) :-
		{threaded(_)}.

	throws(threaded_1_2, error(type_error(callable, 1), logtalk(threaded(_), _))) :-
		{threaded(1)}.

	succeeds(threaded_1_3) :-
		{threaded((true, true))}.

	succeeds(threaded_1_4) :-
		{threaded((fail; true))}.

	fails(threaded_1_5) :-
		{threaded((true, fail))}.

	fails(threaded_1_6) :-
		{threaded((fail; fail))}.

:- end_object.
