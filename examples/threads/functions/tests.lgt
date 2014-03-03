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
		comment is 'Unit tests for the "threads/functions" example.'
	]).

	:- uses(lgtunit, [op(700, xfx, '=~='), '=~='/2]).

	:- discontiguous(succeeds/1).

	succeeds(functions_01) :-
		bisection::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_02) :-
		newton::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_03) :-
		muller::find_root(f1, 1.0, 2.3, 1.0e-15, Zero),
		Zero =~= 2.0.

	succeeds(functions_04) :-
		bisection::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	succeeds(functions_05) :-
		newton::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	succeeds(functions_06) :-
		muller::find_root(f2, 1.0, 1.3, 1.0e-15, Zero),
		Zero =~= 1.25809265664599.

	fails(functions_07) :-
		bisection::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	succeeds(functions_08) :-
		muller::find_root(humps, -1.0, 2.0, 1.0e-15, Zero),
		Zero =~= 1.29954968258.

	throws(functions_09, error(evaluation_error(float_overflow), _)) :-
		newton::find_root(humps, -1.0, 2.0, 1.0e-15, _).

	succeeds(functions_10) :-
		function_root::find_root(f1, 1.0, 2.3, 1.0e-15, Zero, _),
		Zero =~= 2.0.

	succeeds(functions_11) :-
		function_root::find_root(f2, 1.0, 1.3, 1.0e-15, Zero, _),
		Zero =~= 1.25809265664599.

	succeeds(functions_12) :-
		function_root::find_root(f3, 0.0, 3.0, 1.0e-15, Zero, _),
		Zero =~= 1.4142135623731.

	succeeds(functions_13) :-
		function_root::find_root(f4, -1.0, 2.0, 1.0e-15, Zero, _),
		Zero =~= -8.88178419700125e-16.

	succeeds(functions_14) :-
		function_root::find_root(humps, -1.0, 2.0, 1.0e-15, Zero, _),
		Zero =~= 1.29954968258482.

:- end_object.
