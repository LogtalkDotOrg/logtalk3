
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/28,
		comment is 'Unit tests for the dynamic/0 built-in directive.'
	]).

	:- dynamic.

	test(dynamic_0_1) :-
		this(This),
		object_property(This, (dynamic)).

	test(dynamic_0_2) :-
		this(This),
		\+ object_property(This, static).

	% all predicates in a dynamic entity are implicitly dynamic

	:- private(p/0).

	test(dynamic_0_3) :-
		predicate_property(p, private),
		predicate_property(p, (dynamic)).

:- end_object.
