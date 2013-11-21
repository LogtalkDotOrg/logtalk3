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
		date is 2012/12/12,
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

	succeeds(abolish_protocol_1_5) :-
		create_protocol(Protocol, [], []),
		current_protocol(Protocol),
		abolish_protocol(Protocol),
		\+ current_protocol(Protocol).

	succeeds(abolish_protocol_1_6) :-
		create_protocol(a_protocol, [], []),
		current_protocol(a_protocol),
		abolish_protocol(a_protocol),
		\+ current_protocol(a_protocol).

:- end_object.
