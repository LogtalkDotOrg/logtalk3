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
		comment is 'Unit tests for the threaded_exit/1-2 built-in predicate.'
	]).

	throws(threaded_exit_1_1, error(instantiation_error, logtalk(threaded_exit(_), _))) :-
		{threaded_exit(_)}.

	throws(threaded_exit_1_2, error(type_error(callable, 1), logtalk(threaded_exit(_), _))) :-
		{threaded_exit(1)}.

	throws(threaded_exit_2_1, error(instantiation_error, logtalk(threaded_exit(_,_), _))) :-
		{threaded_exit(_, _)}.

	throws(threaded_exit_2_2, error(type_error(callable, 1), logtalk(threaded_exit(_,_), _))) :-
		{threaded_exit(1, _)}.

	succeeds(threaded_exit_2_3) :-
		{threaded_call(true, Tag),
		 threaded_exit(true, Tag)}.

:- end_object.
