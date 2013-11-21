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
		comment is 'Unit tests for the current_event/5 built-in predicate.'
	]).

	throws(current_event_5_1, error(type_error(event, foo), logtalk(current_event(foo,_,_,_,_), _))) :-
		current_event(foo, _, _, _, _).

	throws(current_event_5_2, error(type_error(object_identifier, 1), logtalk(current_event(_,1,_,_,_), _))) :-
		current_event(_, 1, _, _, _).

	throws(current_event_5_3, error(type_error(callable, 1), logtalk(current_event(_,_,1,_,_), _))) :-
		current_event(_, _, 1, _, _).

	throws(current_event_5_4, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,1,_), _))) :-
		current_event(_, _, _, 1, _).

	throws(current_event_5_5, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,_,1), _))) :-
		current_event(_, _, _, _, 1).

:- end_object.
