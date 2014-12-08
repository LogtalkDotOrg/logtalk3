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
		date is 2014/12/08,
		comment is 'Unit tests for the protocol/2 opening directive.'
	]).

	% test all possible syntaxes for protocol relations

	test(protocol_1) :-
		extends_protocol(protocol_1, parent1),
		extends_protocol(protocol_1, parent2).

	test(protocol_2) :-
		extends_protocol(protocol_2, parent1),
		extends_protocol(protocol_2, parent2).

	test(protocol_3) :-
		extends_protocol(protocol_3, parent1),
		extends_protocol(protocol_3, parent2).

:- end_object.
