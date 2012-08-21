%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "hailstone" example.']).

	unit(hailstone).

	test(hailstone_1) :-
		hailstone::generate_sequence(10, Sequence),
		Sequence == [10, 5, 16, 8, 4, 2, 1].

	test(hailstone_2) :-
		hailstone::write_sequence(10).

	test(hailstone_3) :-
		hailstone::sequence_length(27, Length),
		Length == 112.

	test(hailstone_4) :-
		hailstone::longest_sequence(1, 1000, N, Length),
		N == 871,
		Length == 179.

:- end_object.
