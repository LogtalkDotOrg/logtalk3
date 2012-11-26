
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the object_property/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(object_property_2_1, error(type_error(object_identifier, 1), logtalk(object_property(1, static), _))) :-
		object_property(1, static).

	throws(object_property_2_2, error(type_error(callable, 1), logtalk(object_property(logtalk, 1), _))) :-
		object_property(logtalk, 1).

	throws(object_property_2_3, error(domain_error(object_property, foo), logtalk(object_property(logtalk, foo), _))) :-
		object_property(logtalk, foo).

	fails(object_property_2_4) :-
		object_property(non_exisiting_object, _).

	fails(object_property_2_5) :-
		object_property(logtalk, (dynamic)).

	succeeds(object_property_2_6) :-
		findall(Prop, object_property(logtalk, Prop), _).

:- end_object.
