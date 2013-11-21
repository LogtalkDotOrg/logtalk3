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
		comment is 'Unit tests for the define_events/5 built-in predicate.'
	]).

	throws(define_events_5_1, error(type_error(event, foo), logtalk(define_events(foo,_,_,_,_), _))) :-
		define_events(foo, _, _, _, _).

	throws(define_events_5_2, error(type_error(object_identifier, 1), logtalk(define_events(_,1,_,_,_), _))) :-
		define_events(_, 1, _, _, _).

	throws(define_events_5_3, error(type_error(callable, 1), logtalk(define_events(_,_,1,_,_), _))) :-
		define_events(_, _, 1, _, _).

	throws(define_events_5_4, error(type_error(object_identifier, 1), logtalk(define_events(_,_,_,1,_), _))) :-
		define_events(_, _, _, 1, _).

	throws(define_events_5_5, error(instantiation_error, logtalk(define_events(_,_,_,_,_), _))) :-
		define_events(_, _, _, _, _).

	throws(define_events_5_6, error(type_error(object_identifier, 1), logtalk(define_events(_,_,_,_,1), _))) :-
		define_events(_, _, _, _, 1).

	succeeds(define_events_5_7) :-
		create_object(Monitor, [implements(monitoring)], [], [before(_,_,_)]),
		define_events(before, _, _, _ , Monitor),
		current_event(before, _, _, _, Monitor),
		\+ current_event(after, _, _, _, Monitor),
		abolish_events(_, _, _, _, Monitor),
		\+ current_event(_, _, _, _, Monitor),
		abolish_object(Monitor).

	succeeds(define_events_5_8) :-
		create_object(Monitor, [implements(monitoring)], [], [after(_,_,_)]),
		define_events(after, _, _, _ , Monitor),
		current_event(after, _, _, _, Monitor),
		\+ current_event(before, _, _, _, Monitor),
		abolish_events(_, _, _, _, Monitor),
		\+ current_event(_, _, _, _, Monitor),
		abolish_object(Monitor).

:- end_object.
