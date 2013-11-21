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
		date is 2013/05/04,
		comment is 'Unit tests for the current_protocol/1 built-in predicate.'
	]).

	throws(current_protocol_1_1, error(type_error(protocol_identifier, 1), logtalk(current_protocol(1), _))) :-
		current_protocol(1).

	succeeds(current_protocol_1_2) :-
		current_protocol(monitoring).

	fails(current_protocol_1_3) :-
		current_object(non_exisiting_protocol).

	succeeds(current_protocol_1_4) :-
		current_protocol(expanding),
		protocol_property(expanding, built_in),
		protocol_property(expanding, static).

	succeeds(current_protocol_1_5) :-
		current_protocol(monitoring),
		protocol_property(monitoring, built_in),
		protocol_property(monitoring, static).

	succeeds(current_protocol_1_6) :-
		current_protocol(forwarding),
		protocol_property(forwarding, built_in),
		protocol_property(forwarding, static).

:- end_object.
