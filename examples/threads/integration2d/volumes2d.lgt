%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(volumes2d).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/07/18,
		comment is 'Interval and trapezium volume predicates for quadrature methods.']).

	:- private(interval_volume/9).

    interval_volume(_, _, _, C, D , 0, _, Acc, Volume) :-
		Volume is (D-C)*Acc,
		!.
    interval_volume(Function, A, B, Down, Up, N, NP, Acc, Volume) :-
		c(NP, N, C),
		w(NP, N, W),
		YK is Down + (Up-Down)*C,
		interval_area(Function, A, B, YK, NP, NP, 0.0, Area), !,	% cut needed due to a SWI-Prolog weird bug
		N2 is N - 1,
		Acc2 is Acc + W*Area,
		interval_volume(Function, A, B, Down, Up, N2, NP, Acc2, Volume).

	interval_area(_, Left, Right, _, 0, _, Acc, Area) :-
		Area is (Right-Left)*Acc,
		!.
	interval_area(Function, Left, Right, Y, N, NP, Acc, Area) :-
		c(NP, N, C),
		w(NP, N, W),
		XK is Left + (Right-Left)*C,
		functions2d::eval(Function, XK, Y, Z),
		N2 is N - 1,
		Acc2 is Acc + W*Z,
		interval_area(Function, Left, Right, Y, N2, NP, Acc2, Area).

	:- private(trapezium_volume/6).

	trapezium_volume(Function, A, B, C, D, Volume) :-
		functions2d::eval(Function, A, C, F1),
		functions2d::eval(Function, B, C, F2),	
		functions2d::eval(Function, A, D, F3),
		functions2d::eval(Function, B, D, F4),		    
		Volume is 0.25*(B-A)*(D-C)*(F1+F2+F3+F4).

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
