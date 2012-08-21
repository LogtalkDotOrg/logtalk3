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
		author is 'Parker Jones and Paulo Moura',
		date is 2012/07/04,
		comment is 'Unit tests for the "tabling" example.']).

	:- if(current_logtalk_flag(tabling, supported)).

		unit(fibonacci).
		unit(paths).

		test(tabling_1) :-
			setof(Y, paths::path(1, Y), Ys),
			Ys = [2, 3, 4, 5].

		test(tabling_2) :-
			fibonacci::fib(30, F),
			F == 1346269.

	:- endif.

:- end_object.
