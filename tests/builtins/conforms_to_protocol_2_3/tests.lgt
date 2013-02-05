
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/02/05,
		comment is 'Unit tests for the conforms_to_protocol/2-3 built-in predicates.'
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

:- end_object.
