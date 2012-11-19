
:- object(current_protocol1,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the current_protocol/1 built-in predicate.'
	]).

	throws(current_protocol_1_1, error(type_error(protocol_identifier, 1), logtalk(current_protocol(1), _))) :-
		current_protocol(1).

	succeeds(current_protocol_1_2) :-
		current_protocol(monitoring).

	fails(current_protocol_1_3) :-
		current_object(non_exisiting_protocol).

	succeeds(expanding) :-
		current_protocol(expanding),
		protocol_property(expanding, final),
		protocol_property(expanding, static).

	succeeds(monitoring) :-
		current_protocol(monitoring),
		protocol_property(monitoring, final),
		protocol_property(monitoring, static).

:- end_object.
