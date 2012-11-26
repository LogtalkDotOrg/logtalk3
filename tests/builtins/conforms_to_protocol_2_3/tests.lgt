
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/11/19,
		comment is 'Unit tests for the conforms_to_protocol/2-3 built-in predicate.'
	]).

	throws(conforms_to_protocol_2_1, error(type_error(object_identifier, 1), logtalk(conforms_to_protocol(1,_), _))) :-
		conforms_to_protocol(1, _).

	throws(conforms_to_protocol_2_2, error(type_error(protocol_identifier, 1), logtalk(conforms_to_protocol(logtalk,1), _))) :-
		conforms_to_protocol(logtalk, 1).

	throws(conforms_to_protocol_3_1, error(type_error(object_identifier, 1), logtalk(conforms_to_protocol(1,_,_), _))) :-
		conforms_to_protocol(1, _, _).

	throws(conforms_to_protocol_3_2, error(type_error(protocol_identifier, 1), logtalk(conforms_to_protocol(logtalk,1,_), _))) :-
		conforms_to_protocol(logtalk, 1, _).

	throws(conforms_to_protocol_3_3, error(type_error(scope, 1), logtalk(conforms_to_protocol(logtalk,_,1), _))) :-
		conforms_to_protocol(logtalk, _, 1).

	succeeds(conforms_to_protocol_2_) :-
		conforms_to_protocol(logtalk, expanding).

	succeeds(conforms_to_protocol3_) :-
		conforms_to_protocol(logtalk, expanding, Scope),
		Scope == (public).

:- end_object.
