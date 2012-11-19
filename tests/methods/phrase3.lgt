
:- object(phrase3,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the phrase/3 built-in method.'
	]).

	throws(phrase_3_1, error(permission_error(access, private_predicate, phrase/3), logtalk(logtalk::phrase(_, _, _), _))) :-
		logtalk::phrase(_, _, _).

:- end_object.
