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
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "lo/travellers" example.'
	]).

	test(lo_travellers_1) :-
		incremental::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route == oxford-london-portsmouth-brighton-exeter-aberystwyth.

	test(lo_travellers_2) :-
		presort::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route == brighton-london-oxford-portsmouth-exeter-aberystwyth.

	test(lo_travellers_3) :-
		circular::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route == london-brighton-portsmouth-exeter-aberystwyth-oxford-london.

	test(lo_travellers_4) :-
		permute::route([london, brighton, portsmouth, exeter, oxford, aberystwyth], Route),
		Route = (aberystwyth-exeter-portsmouth-brighton-london-oxford,_).

:- end_object.
