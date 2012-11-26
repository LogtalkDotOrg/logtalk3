
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the extends_protocol/2-3 built-in predicate.'
	]).

	% extends_protocol/2 tests

	throws(extends_protocol_2_1, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(1, _), _))) :-
		extends_protocol(1, _).

	throws(extends_protocol_2_2, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(_, 1), _))) :-
		extends_protocol(_, 1).

	% extends_protocol/3 tests

	throws(extends_protocol_3_1, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(1, _, _), _))) :-
		extends_protocol(1, _, _).

	throws(extends_protocol_3_2, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(_, 1, _), _))) :-
		extends_protocol(_, 1, _).

	throws(extends_protocol_3_3, error(type_error(scope, 1), logtalk(extends_protocol(_, _, 1), _))) :-
		extends_protocol(_, _, 1).

:- end_object.
