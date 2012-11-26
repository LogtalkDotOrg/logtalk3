
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the abolish_protocol/1 built-in predicate.'
	]).

	throws(abolish_protocol_1_1, error(instantiation_error, logtalk(abolish_protocol(_), _))) :-
		abolish_protocol(_).

	throws(abolish_protocol_1_2, error(type_error(protocol_identifier, 1), logtalk(abolish_protocol(1), _))) :-
		abolish_protocol(1).

	throws(abolish_protocol_1_3, error(existence_error(protocol, non_exisiting_protocol), logtalk(abolish_protocol(non_exisiting_protocol), _))) :-
		abolish_protocol(non_exisiting_protocol).

	throws(abolish_protocol_1_4, error(permission_error(modify, static_protocol, monitoring), logtalk(abolish_protocol(monitoring), _))) :-
		abolish_protocol(monitoring).

:- end_object.
