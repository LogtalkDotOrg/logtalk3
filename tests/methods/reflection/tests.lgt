
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/06/05,
		comment is 'Unit tests for the reflection built-in methods.']).

	% current_predicate/1 tests

	throws(current_predicate_1_1, error(type_error(predicate_indicator, 1), logtalk(_, _))) :-
		current_predicate(1).

	throws(current_predicate_1_2, error(type_error(predicate_indicator, a/b), logtalk(_, _))) :-
		current_predicate(a/b).

	% predicate_property/2 tests

:- end_object.
