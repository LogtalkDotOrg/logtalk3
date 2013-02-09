%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(areas).

	:- info([
		version is 1.1,
		author is 'Paul Crocker',
		date is 2008/07/19,
		comment is 'Interval and trapezium area predicates for quadrature methods.'
	]).

	:- private(interval_area/7).

	interval_area(_, Left, Right, 0, _, Acc, Area) :-
		Area is (Right-Left)*Acc,
		!.
	interval_area(Function, Left, Right, N, NP, Acc, Area) :-
		c(NP, N, C),
		w(NP, N, W),
		XK is Left + (Right-Left)*C,
		functions::eval(Function, XK, Y),
		N2 is N - 1,
		Acc2 is Acc + W*Y,
		interval_area(Function, Left, Right, N2, NP, Acc2, Area).

	:- private(trapezium_area/5).

	trapezium_area(Left, Right, Fleft, Fright, Area) :-
		Area is 0.5*(Right-Left)*(Fright+Fleft).

	w(1,1, 1.000000000000000).
	w(2,1, 0.500000000000000).
	w(2,2, 0.500000000000000).
	w(3,1, 0.277777777777778).
	w(3,2, 0.444444444444444).
	w(3,3, 0.277777777777778).
	w(4,1, 0.173927422568727).
	w(4,2, 0.326072577431273).
	w(4,3, 0.326072577431273).
	w(4,4, 0.173927422568727).

	c(1,1, 0.500000000000000).
	c(2,1, 0.211324865405187).
	c(2,2, 0.788675134594813).
	c(3,1, 0.112701665379258).
	c(3,2, 0.500000000000000).
	c(3,3, 0.887298334620742).
	c(4,1, 0.069431844209737).
	c(4,2, 0.330009478207572).
	c(4,3, 0.669990521792428).
	c(4,4, 0.930568155790263).

:- end_category.
