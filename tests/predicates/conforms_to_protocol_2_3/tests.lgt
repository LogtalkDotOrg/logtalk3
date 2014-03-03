%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/03/11,
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

	throws(conforms_to_protocol_3_3, error(type_error(atom, 1), logtalk(conforms_to_protocol(logtalk,_,1), _))) :-
		conforms_to_protocol(logtalk, _, 1).

	throws(conforms_to_protocol_3_4, error(domain_error(scope, a), logtalk(conforms_to_protocol(logtalk,_,a), _))) :-
		conforms_to_protocol(logtalk, _, a).

:- end_object.
