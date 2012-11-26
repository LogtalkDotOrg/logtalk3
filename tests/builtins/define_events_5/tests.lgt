
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

:- end_object.
