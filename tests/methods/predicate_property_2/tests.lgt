
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/12/03,
		comment is 'Unit tests for the predicate_property/2 built-in method.'
	]).

	throws(predicate_property_2_1, error(instantiation_error,logtalk(This::predicate_property(_,_),user))) :-
		this(This),
		{This::predicate_property(_, _)}.

	throws(predicate_property_2_2, error(type_error(callable, 1),logtalk(This::predicate_property(1, _),user))) :-
		this(This),
		{This::predicate_property(1, _)}.

	throws(predicate_property_2_3, error(domain_error(predicate_property, bar),logtalk(This::predicate_property(foo, bar),user))) :-
		this(This),
		{This::predicate_property(foo, bar)}.

:- end_object.
