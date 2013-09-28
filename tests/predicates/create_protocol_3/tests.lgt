
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the create_protocol/3 built-in predicate.'
	]).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	throws(create_protocol_1_1, error(instantiation_error, logtalk(create_protocol(_, _, _), _))) :-
		create_protocol(_, _, _).

	throws(create_protocol_1_2, error(type_error(protocol_identifier, 1), logtalk(create_protocol(1, [], []), _))) :-
		create_protocol(1, [], []).

	throws(create_protocol_1_3, error(permission_error(modify, protocol, monitoring), logtalk(create_protocol(monitoring, [], []), _))) :-
		create_protocol(monitoring, [], []).

	throws(create_protocol_1_4, error(permission_error(modify, object, logtalk), logtalk(create_protocol(logtalk, [], []), _))) :-
		create_protocol(logtalk, [], []).

	throws(create_protocol_1_5, error(type_error(list, atom), logtalk(create_protocol(_, atom, []), _))) :-
		create_protocol(_, atom, []).

	throws(create_protocol_1_6, error(type_error(list, atom), logtalk(create_protocol(_, [], atom), _))) :-
		create_protocol(_, [], atom).

:- end_object.
