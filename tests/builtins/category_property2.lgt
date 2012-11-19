
:- object(category_property2,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the category_property/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(category_property_2_1, error(type_error(category_identifier, 1), logtalk(category_property(1, static), _))) :-
		category_property(1, static).

	throws(category_property_2_2, error(type_error(callable, 1), logtalk(category_property(monitoring, 1), _))) :-
		category_property(monitoring, 1).

	throws(category_property_2_3, error(domain_error(category_property, foo), logtalk(category_property(monitoring, foo), _))) :-
		category_property(monitoring, foo).

	fails(category_property_2_4) :-
		category_property(non_exisiting_category, _).

	fails(category_property_2_5) :-
		category_property(monitoring, (dynamic)).

	succeeds(category_property_2_6) :-
		findall(Prop, category_property(monitoring, Prop), _).

:- end_object.
