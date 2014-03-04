%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- protocol(integrate2d).

	:- info([
		version is 1.1,
		author is 'Paul Crocker',
		date is 2008/7/11,
		comment is 'Default protocol for Numerical Integration of functions of two variables.'
	]).

	:- public(integrate/8).
	:- mode(integrate(+object_identifier, +float, +float, +float, +float, +integer, +float, -float), one).
	:- info(integrate/8, [
		comment is 'Find the integral of a function of two variables in the interval [A, B] [C, D] given a maximum approximation error (epsilon).',
		argnames is ['Function', 'A', 'B', 'C', 'D','NP', 'Epsilon', 'Integral']
	]).

:- end_protocol.


:- object(quadrec2d(_Threads),
	implements(integrate2d),
	imports(volumes2d)).

	:- threaded.

	:- info([
		version is 1.1,
		author is 'Paul Crocker',
		date is 2013/04/19,
		comment is 'Multi-threading implementation of Recursive Gaussian Quadrature Methods for Numerical Integration for functions of two variables.',
		parameters is ['Threads' - 'Number of threads to use (1, 4, 16, 64, 256, ...).']
	]).

	integrate(Function, A, B, C, D, NP, Epsilon, Integral) :-
		parameter(1, Threads),
		Threads > 0,
		(	NP =:= 0 ->
			^^trapezium_volume(Function, A, B, C, D, InitialVolume),
			trapezium(Threads, Function, A, B, C, D, InitialVolume, Epsilon, Integral)
		;	NP > 0, NP < 4,
			^^interval_volume(Function, A, B, C, D, NP, NP, 0.0, InitialVolume),
			quadrature(Threads, Function, A, B, C, D, InitialVolume, NP, Epsilon, Integral)
		).

	quadrature(1, Function, A, B, C, D, Volume, NP, Epsilon, Integral) :-
		!,
		MiddleX is 0.5 * (A + B),
		MiddleY is 0.5 * (C + D),
		^^interval_volume(Function, A,       MiddleX, C,       MiddleY, NP, NP, 0.0, Volume1),
		^^interval_volume(Function, MiddleX, B,       C,       MiddleY, NP, NP, 0.0, Volume2),
		^^interval_volume(Function, A,       MiddleX, MiddleY, D,       NP, NP, 0.0, Volume3),
		^^interval_volume(Function, MiddleX, B,       MiddleY, D,       NP, NP, 0.0, Volume4),
		Error is abs(Volume-Volume1-Volume2-Volume3-Volume4),
		(	Error > Epsilon ->
			Epsilon4 is Epsilon/4.0,
			quadrature(Threads, Function, A,       MiddleX, C,       MiddleY, Volume1, NP, Epsilon4, I1),
			quadrature(Threads, Function, MiddleX, B,       C,       MiddleY, Volume2, NP, Epsilon4, I2),
			quadrature(Threads, Function, A,       MiddleX, MiddleY, D,       Volume3, NP, Epsilon4, I3),
			quadrature(Threads, Function, MiddleX, B,       MiddleY, D,       Volume4, NP, Epsilon4, I4),
			Integral is I1 + I2 + I3 + I4
		;	Integral is Volume1 + Volume2 + Volume3 + Volume4
		).

	quadrature(Threads, Function, A, B, C, D, Volume, NP, Epsilon, Integral) :-
		Threads > 1,
		MiddleX is 0.5 * (A + B),
		MiddleY is 0.5 * (C + D),
		^^interval_volume(Function, A,       MiddleX, C,       MiddleY, NP, NP, 0.0, Volume1),
		^^interval_volume(Function, MiddleX, B,       C,       MiddleY, NP, NP, 0.0, Volume2),
		^^interval_volume(Function, A,       MiddleX, MiddleY, D,       NP, NP, 0.0, Volume3),
		^^interval_volume(Function, MiddleX, B,       MiddleY, D,       NP, NP, 0.0, Volume4),
		Error is abs(Volume-Volume1-Volume2-Volume3-Volume4),
		(	Error > Epsilon ->
			Threads4 is Threads//4,
			Epsilon4 is Epsilon/4.0,
			threaded((
				quadrature(Threads4, Function, A,       MiddleX,  C,       MiddleY, Volume1, NP, Epsilon4, I1),
				quadrature(Threads4, Function, MiddleX, B,        C,       MiddleY, Volume2, NP, Epsilon4, I2),
				quadrature(Threads4, Function, A,       MiddleX,  MiddleY, D,       Volume3, NP, Epsilon4, I3),
				quadrature(Threads4, Function, MiddleX, B,        MiddleY, D,       Volume4, NP, Epsilon4, I4)
			)),
			Integral is I1 + I2 + I3 + I4
		;	Integral is Volume1 + Volume2 + Volume3 + Volume4
		).

	trapezium(Threads, Function, A, B, C, D, Volume, Epsilon, Integral) :-
		MiddleX is 0.5 * (A + B),
		MiddleY is 0.5 * (C + D),
		^^trapezium_volume(Function, A,       MiddleX, C,       MiddleY, Volume1),
		^^trapezium_volume(Function, MiddleX, B,       C,       MiddleY, Volume2),
		^^trapezium_volume(Function, A,       MiddleX, MiddleY, D,       Volume3),
		^^trapezium_volume(Function, MiddleX, B,       MiddleY, D,       Volume4),
		Error is abs(Volume-Volume1-Volume2-Volume3-Volume4),
		(	Error > Epsilon -> 
			(	Threads =:= 1 ->
				Epsilon4 is Epsilon/4.0,
				trapezium(Function, 1, A,       MiddleX, C,       MiddleY, Volume1, Epsilon4, I1),
				trapezium(Function, 1, MiddleX, B,       C,       MiddleY, Volume2, Epsilon4, I2),
				trapezium(Function, 1, A,       MiddleX, MiddleY, D,       Volume3, Epsilon4, I3),
				trapezium(Function, 1, MiddleX, B,       MiddleY, D,       Volume4, Epsilon4, I4)
			;	% Threads > 1,
				Threads4 is Threads//4,
				Epsilon4 is Epsilon/4.0,
				threaded((
					trapezium(Threads4, Function, A,       MiddleX, C,       MiddleY, Volume1, Epsilon4, I1), 
					trapezium(Threads4, Function, MiddleX, B,       C,       MiddleY, Volume2, Epsilon4, I2),
					trapezium(Threads4, Function, A,       MiddleX, MiddleY, D,       Volume3, Epsilon4, I3),
					trapezium(Threads4, Function, MiddleX, B,       MiddleY, D,       Volume4, Epsilon4, I4)
				))
			),
			Integral is I1 + I2 + I3 + I4
		;	Integral is Volume1 + Volume2 + Volume3 + Volume4
		).

:- end_object.


:- object(quadsplit2d(_Threads),
	implements(integrate2d),
	imports(volumes2d)).

	:- threaded.

	:- info([
		version is 1.2,
		author is 'Paul Crocker',
		date is 2013/04/19,
		comment is 'Multi-threading implementation of Recursive Gaussian Quadrature Methods for Numerical Integration for functions of two real variables.',
		parameters is ['Threads' - 'Number of threads to use (1, 4, 9, 16, 25, ...).']
	]).

	integrate(Function, A, B, C, D, NP, Epsilon, Integral) :-
		parameter(1, Threads),
		Threads > 0,
		B > A,
		D > C,
		NP >= 0, NP < 4,
		(	Threads =:= 1 ->
			start(Function, A, B, C, D, NP, Epsilon, Integral)
		;	% Threads > 1
			Threads2 is round(sqrt(Threads)),
			Epsilon2 is Epsilon/Threads,
			split(A, B, Threads2, ABIntervals),
			split(C, D, Threads2, CDIntervals),
			spawn(ABIntervals, CDIntervals, Function, NP, Epsilon2, [], Goals),
			collect(Goals, 0.0, Integral)
		).

	% split an interval into a list of intervals
	split(Inf, Sup, N, Intervals) :-
		Width is (Sup - Inf) / N,
		split(1, N, Width, Sup, Inf, Intervals).

	split(N, N, _, Sup, Current, [Current-Sup]) :-
		!.
	split(I, N, Width, Sup, Current, [Current-Next| Intervals]) :-
		I2 is I + 1,
		Next is Current + Width,
		split(I2, N, Width, Sup, Next, Intervals).

	% initiate the thread calls
	spawn([], _, _, _, _, Goals, Goals).
	spawn([Left-Right| ABIntervals], CDIntervals, Function, NP, Epsilon, Acc, Goals) :-
		spawn(CDIntervals, Left, Right, Function, NP, Epsilon, Acc, Acc2),
		spawn(ABIntervals, CDIntervals, Function, NP, Epsilon, Acc2, Goals).

	spawn([], _, _, _, _, _, Goals, Goals).
	spawn([Bottom-Top| CDIntervals], Left, Right, Function, NP, Epsilon, Acc, Goals) :-
		threaded_once(start(Function,Left,Right,Bottom,Top,NP,Epsilon,SubVolume)),
		spawn(CDIntervals, Left, Right, Function, NP, Epsilon, [start(Function,Left,Right,Bottom,Top,NP,Epsilon,SubVolume)| Acc], Goals).

	% wait for the threads to finish and then collect the results summing as we go
	collect([], Integral, Integral).
	collect([start(Function,Left,Right,Bottom,Top,NP,Epsilon,SubVolume)| Goals], Acc, Integral) :-
		threaded_exit(start(Function,Left,Right,Bottom,Top,NP,Epsilon,SubVolume)),
		Acc2 is Acc + SubVolume,
		collect(Goals, Acc2, Integral).

	% predicate that the threads will start
	start(Function, A, B, C, D, NP, Epsilon, Integral) :-
		(	NP =:= 0 -> 
			^^trapezium_volume(Function, A, B, C, D, InitialVolume),
			trapezium(Function, A, B, C, D, InitialVolume, Epsilon, Integral)
		;	% NP > 0,
			^^interval_volume(Function, A, B, C, D, NP, NP, 0.0, InitialVolume),
			quadrature(Function, A, B, C, D, InitialVolume, NP, Epsilon, Integral)
		).

	trapezium(Function, A, B, C, D, Volume, Epsilon, Integral) :-
		MiddleX is 0.5*(A+B),
		MiddleY is 0.5*(C+D),
		^^trapezium_volume(Function, A,       MiddleX, C,       MiddleY, Volume1),
		^^trapezium_volume(Function, MiddleX, B,       C,       MiddleY, Volume2),
		^^trapezium_volume(Function, A,       MiddleX, MiddleY, D,       Volume3),
		^^trapezium_volume(Function, MiddleX, B,       MiddleY, D,       Volume4),
		Error is abs(Volume - Volume1 - Volume2 - Volume3 - Volume4),
		(	Error > Epsilon -> 
			Epsilon4 is Epsilon/4.0,
			trapezium(Function, A,       MiddleX, C,       MiddleY, Volume1, Epsilon4, I1),
			trapezium(Function, MiddleX, B,       C,       MiddleY, Volume2, Epsilon4, I2),
			trapezium(Function, A,       MiddleX, MiddleY, D,       Volume3, Epsilon4, I3),
			trapezium(Function, MiddleX, B,       MiddleY, D,       Volume4, Epsilon4, I4),
			Integral is I1 + I2 + I3 + I4
		;	Integral is Volume1 + Volume2 + Volume3 + Volume4
		).

	quadrature(Function, A, B, C, D, Volume, NP, Epsilon, Integral) :-
		MiddleX is 0.5*(A+B),
		MiddleY is 0.5*(C+D),
		^^interval_volume(Function, A,       MiddleX, C,       MiddleY, NP, NP, 0.0, Volume1),
		^^interval_volume(Function, MiddleX, B,       C,       MiddleY, NP, NP, 0.0, Volume2),
		^^interval_volume(Function, A,       MiddleX, MiddleY, D,       NP, NP, 0.0, Volume3),
		^^interval_volume(Function, MiddleX, B,       MiddleY, D,       NP, NP, 0.0, Volume4),
		Error is abs(Volume - Volume1 - Volume2 - Volume3 - Volume4),
		(	Error > Epsilon ->
			Epsilon4 is Epsilon/4.0,
			quadrature(Function, A,       MiddleX, C,       MiddleY, Volume1, NP, Epsilon4, I1),
			quadrature(Function, MiddleX, B,       C,       MiddleY, Volume2, NP, Epsilon4, I2),
			quadrature(Function, A,       MiddleX, MiddleY, D,       Volume3, NP, Epsilon4, I3),
			quadrature(Function, MiddleX, B,       MiddleY, D,       Volume4, NP, Epsilon4, I4),
			Integral is I1 + I2 + I3 + I4
		;	Integral is Volume1 + Volume2 + Volume3 + Volume4
		).

:- end_object.
