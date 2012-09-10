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
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2010/04/26,
		comment is 'Unit tests for the "threads/integration2d" example.']).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	test(integration2d_1) :-
		quadsplit2d(1)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		Integral =~= -21.3333333333.

	test(integration2d_2) :-
		quadsplit2d(4)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral), 
		Integral =~= -21.3333333333.

	test(integration2d_3) :-
		quadsplit2d(16)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		Integral =~= -21.3333333333.

	test(integration2d_4) :-
		quadrec2d(1)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		Integral =~= -21.3333333333.

	test(integration2d_5) :-
		quadrec2d(4)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		Integral =~= -21.3333333333.

	test(integration2d_6) :-
		quadrec2d(16)::integrate(circle, -2, 2, -2, 2, 2, 2.5e-10, Integral),
		Integral =~= -21.3333333333.

	test(integration2d_7) :-
		quadrec2d(1)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral),
		Integral =~= 7.73592444761.

	test(integration2d_8) :-
		quadrec2d(4)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral),
		Integral =~= 7.73592444761.

	test(integration2d_9) :-
		quadrec2d(16)::integrate(i15, -2,2,-2,2, 2, 1.0e-4, Integral),
		Integral =~= 7.73592444761.

:- end_object.
