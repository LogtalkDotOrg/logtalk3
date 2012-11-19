
:- object(current_predicate1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the current_predicate/1 built-in method.'
	]).

	throws(current_predicate_1_1, error(type_error(predicate_indicator, 1), logtalk(_, _))) :-
		current_predicate(1).

	throws(current_predicate_1_2, error(type_error(integer,b), logtalk(_, _))) :-
		current_predicate(a/b).

:- end_object.
