
:- object(current_event5,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the current_event/5 built-in predicate.'
	]).

	throws(current_event5_1, error(type_error(event, foo), logtalk(current_event(foo,_,_,_,_), _))) :-
		current_event(foo, _, _, _, _).

	throws(current_event5_2, error(type_error(object_identifier, 1), logtalk(current_event(_,1,_,_,_), _))) :-
		current_event(_, 1, _, _, _).

	throws(current_event5_3, error(type_error(callable, 1), logtalk(current_event(_,_,1,_,_), _))) :-
		current_event(_, _, 1, _, _).

	throws(current_event5_4, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,1,_), _))) :-
		current_event(_, _, _, 1, _).

	throws(current_event5_5, error(type_error(object_identifier, 1), logtalk(current_event(_,_,_,_,1), _))) :-
		current_event(_, _, _, _, 1).

:- end_object.
