
:- object(protocol_property2,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the protocol_property/2 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(protocol_property_2_1, error(type_error(protocol_identifier, 1), logtalk(protocol_property(1, static), _))) :-
		protocol_property(1, static).

	throws(protocol_property_2_2, error(type_error(callable, 1), logtalk(protocol_property(monitoring, 1), _))) :-
		protocol_property(monitoring, 1).

	throws(protocol_property_2_3, error(domain_error(protocol_property, foo), logtalk(protocol_property(monitoring, foo), _))) :-
		protocol_property(monitoring, foo).

	fails(protocol_property_2_4) :-
		protocol_property(non_exisiting_protocol, _).

	fails(protocol_property_2_5) :-
		protocol_property(monitoring, (dynamic)).

	succeeds(protocol_property_2_6) :-
		findall(Prop, protocol_property(monitoring, Prop), _).

:- end_object.
