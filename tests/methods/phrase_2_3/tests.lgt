
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/10,
		comment is 'Unit tests for the phrase/2-3 built-in methods.'
	]).

	throws(phrase_2_1, error(permission_error(access, private_predicate, phrase/2), logtalk(logtalk::phrase(_, _), user))) :-
		{logtalk::phrase(_, _)}.

	throws(phrase_2_2, error(instantiation_error, logtalk(logtalk<<phrase(_, _), user))) :-
		{logtalk<<phrase(_, _)}.

	throws(phrase_2_3, error(type_error(callable,1), logtalk(logtalk<<phrase(1, _), user))) :-
		{logtalk<<phrase(1, _)}.

	throws(phrase_2_4, error(existence_error(procedure,foo/2), logtalk(logtalk<<phrase(foo, 1), user))) :-
		{logtalk<<phrase(foo, 1)}.

	throws(phrase_3_1, error(permission_error(access, private_predicate, phrase/3), logtalk(logtalk::phrase(_, _, _), user))) :-
		{logtalk::phrase(_, _, _)}.

	throws(phrase_3_2, error(instantiation_error, logtalk(logtalk<<phrase(_, _, _), user))) :-
		{logtalk<<phrase(_, _, _)}.

	throws(phrase_3_3, error(type_error(callable,1), logtalk(logtalk<<phrase(1, _, _), user))) :-
		{logtalk<<phrase(1, _, _)}.

	throws(phrase_3_4, error(existence_error(procedure,foo/2), logtalk(logtalk<<phrase(foo, 1, _), user))) :-
		{logtalk<<phrase(foo, 1, _)}.

	throws(phrase_3_5, error(existence_error(procedure,foo/2), logtalk(logtalk<<phrase(foo, _, 1), user))) :-
		{logtalk<<phrase(foo, _, 1)}.

:- end_object.
