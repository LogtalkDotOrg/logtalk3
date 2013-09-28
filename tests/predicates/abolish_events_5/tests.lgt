
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/12,
		comment is 'Unit tests for the abolish_events/5 built-in predicate.'
	]).

	throws(abolish_events_5_1, error(type_error(event, foo), logtalk(abolish_events(foo,_,_,_,_), _))) :-
		abolish_events(foo, _, _, _, _).

	throws(abolish_events_5_2, error(type_error(object_identifier, 1), logtalk(abolish_events(_,1,_,_,_), _))) :-
		abolish_events(_, 1, _, _, _).

	throws(abolish_events_5_3, error(type_error(callable, 1), logtalk(abolish_events(_,_,1,_,_), _))) :-
		abolish_events(_, _, 1, _, _).

	throws(abolish_events_5_4, error(type_error(object_identifier, 1), logtalk(abolish_events(_,_,_,1,_), _))) :-
		abolish_events(_, _, _, 1, _).

	throws(abolish_events_5_5, error(type_error(object_identifier, 1), logtalk(abolish_events(_,_,_,_,1), _))) :-
		abolish_events(_, _, _, _, 1).

	succeeds(abolish_events_5_6) :-
		create_object(Monitor, [implements(monitoring)], [], [before(_,_,_), after(_,_,_)]),
		define_events(_, _, _, _ , Monitor),
		current_event(_, _, _, _, Monitor),
		abolish_events(_, _, _, _, Monitor),
		\+ current_event(_, _, _, _, Monitor),
		abolish_object(Monitor).

:- end_object.
