
:- object(abolish_events5,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the abolish_events/5 built-in predicate.'
	]).

	throws(abolish_events5_1, error(type_error(event, foo), logtalk(abolish_events(foo,_,_,_,_), _))) :-
		abolish_events(foo, _, _, _, _).

	throws(abolish_events5_2, error(type_error(object_identifier, 1), logtalk(abolish_events(_,1,_,_,_), _))) :-
		abolish_events(_, 1, _, _, _).

	throws(abolish_events5_3, error(type_error(callable, 1), logtalk(abolish_events(_,_,1,_,_), _))) :-
		abolish_events(_, _, 1, _, _).

	throws(abolish_events5_4, error(type_error(object_identifier, 1), logtalk(abolish_events(_,_,_,1,_), _))) :-
		abolish_events(_, _, _, 1, _).

	throws(abolish_events5_5, error(type_error(object_identifier, 1), logtalk(abolish_events(_,_,_,_,1), _))) :-
		abolish_events(_, _, _, _, 1).

:- end_object.
