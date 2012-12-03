
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/03,
		comment is 'Unit tests for the current_predicate/1 built-in method.'
	]).

	throws(current_predicate_1_1, error(type_error(predicate_indicator, 1), logtalk(This::current_predicate(1),user))) :-
		this(This),
		{This::current_predicate(1)}.

	throws(current_predicate_1_2, error(type_error(atom,1), logtalk(This::current_predicate(1/b),user))) :-
		this(This),
		{This::current_predicate(1/b)}.

	throws(current_predicate_1_3, error(type_error(integer,b), logtalk(This::current_predicate(a/b),user))) :-
		this(This),
		{This::current_predicate(a/b)}.

	throws(current_predicate_1_4, error(domain_error(not_less_than_zero, -1), logtalk(This::current_predicate(a/(-1)),user))) :-
		this(This),
		{This::current_predicate(a/(-1))}.

:- end_object.
