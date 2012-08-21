
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2010/06/05,
		comment is 'Unit tests for the predicate directives.']).

	throws(phrase_2_1, error(permission_error(private_predicate, phrase/2), logtalk::phrase(_, _), _)) :-
		logtalk::phrase(_, _).

	throws(phrase_3_1, error(permission_error(private_predicate, phrase/3), logtalk::phrase(_, _, _), _)) :-
		logtalk::phrase(_, _, _).

:- end_object.
