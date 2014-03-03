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
		comment is 'Unit tests for the extends_protocol/2-3 built-in predicates.'
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

	throws(extends_protocol_3_3, error(type_error(atom, 1), logtalk(extends_protocol(_, _, 1), _))) :-
		extends_protocol(_, _, 1).

	throws(extends_protocol_3_4, error(domain_error(scope, a), logtalk(extends_protocol(_, _, a), _))) :-
		extends_protocol(_, _, a).

:- end_object.
