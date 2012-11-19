
:- object(phrase2,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the phrase/2 built-in method.'
	]).

	throws(phrase_2_1, error(permission_error(access, private_predicate, phrase/2), logtalk(logtalk::phrase(_, _), _))) :-
		logtalk::phrase(_, _).

:- end_object.
