
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the implements_protocol/2-3 built-in predicate.'
	]).

	throws(implements_protocol_2_1, error(type_error(object_identifier, 1), logtalk(implements_protocol(1, _), _))) :-
		implements_protocol(1, _).

	throws(implements_protocol_2_2, error(type_error(protocol_identifier, 1), logtalk(implements_protocol(_, 1), _))) :-
		implements_protocol(_, 1).

	% implements_protocol/3 tests

	throws(implements_protocol_3_1, error(type_error(object_identifier, 1), logtalk(implements_protocol(1, _, _), _))) :-
		implements_protocol(1, _, _).

	throws(implements_protocol_3_2, error(type_error(protocol_identifier, 1), logtalk(implements_protocol(_, 1, _), _))) :-
		implements_protocol(_, 1, _).

	throws(implements_protocol_3_3, error(type_error(scope, 1), logtalk(implements_protocol(_, _, 1), _))) :-
		implements_protocol(_, _, 1).

:- end_object.
