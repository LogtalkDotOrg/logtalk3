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
		version is 1.0,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/03/16,
		comment is 'Unit tests for the "symdiff" example.']).

	test(symdiff_1) :-
		(x**1 + x*0 - x*1)::simplify(S),
		S == 0.

	test(symdiff_2) :-
		(2*x**3 + x**2 - 4*x)::diff(D), D::simplify(S),
		D == 2 * (3*x**2*1) + 2*x**1*1-4*1, S == 2 * (3*x**2) + 2*x-4.

	test(symdiff_3) :-
		(log(x**2 + 2*x - 7) + 4*x)::diff(D), D::simplify(S),
		D == (2*x**1*1+2*1) * (x**2+2*x-7) ** -1 + 4*1, S == (2*x+2) * (x**2+2*x-7) ** -1 + 4.

:- end_object.
