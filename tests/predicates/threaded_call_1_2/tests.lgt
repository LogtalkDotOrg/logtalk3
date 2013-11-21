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
		comment is 'Unit tests for the threaded_call/1-2 built-in predicate.'
	]).

	:- threaded.

	throws(threaded_call_1_1, error(instantiation_error, logtalk(threaded_call(_), _))) :-
		{threaded_call(_)}.

	throws(threaded_call_1_2, error(type_error(callable, 1), logtalk(threaded_call(_), _))) :-
		{threaded_call(1)}.

	throws(threaded_call_2_1, error(instantiation_error, logtalk(threaded_call(_,_), _))) :-
		{threaded_call(_, _)}.

	throws(threaded_call_2_2, error(type_error(callable, 1), logtalk(threaded_call(_,_), _))) :-
		{threaded_call(1, _)}.

	throws(threaded_call_2_3, error(type_error(variable, tag), logtalk(threaded_call(_,_), _))) :-
		{threaded_call(true, tag)}.

:- end_object.
