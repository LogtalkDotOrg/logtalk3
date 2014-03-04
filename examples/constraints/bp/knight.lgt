%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% distributed with B-Prolog (November 2010)

/* 
	Find a knight tour that connects all the squares 
	by Neng-Fa Zhou, 2001, modified in 2005
*/

:- object(knight).

	:- public(go/0).

	go:-
		solve(Vars),
		output(Vars).

	solve(Vars) :-
		length(Vars,64), % 8*8=64
		computeDomain(Vars,1),
		circuit(Vars),  % built-in
		labeling_ff(Vars).

	computeDomain([],N).
	computeDomain([V|Vs],N) :-
		I is (N-1)//8+1,
		J is N-(I-1)*8,
		feasiblePositions(I,J,D),
		sort(D,SortedD),
		V in SortedD,
		N1 is N+1,
		computeDomain(Vs,N1).

	feasiblePositions(I,J,D) :-
		I1 is I+1,  J1 is J+2,
		I2 is I+1,  J2 is J-2,
		I3 is I-1,  J3 is J+2,
		I4 is I-1,  J4 is J-2,
		I5 is I+2,  J5 is J+1,
		I6 is I+2,  J6 is J-1,
		I7 is I-2,  J7 is J+1,
		I8 is I-2,  J8 is J-1,
		addFeasiblePositions([(I1,J1),(I2,J2),(I3,J3),(I4,J4),(I5,J5),(I6,J6),(I7,J7),(I8,J8)],D).

	addFeasiblePositions([],D) :-D=[].
	addFeasiblePositions([(I,J)|IJs],D) :-
		(I>=1,I=<8,J>=1,J=<8),!,
		N is (I-1)*8+J,
		D=[N|D1],
		addFeasiblePositions(IJs,D1).
	addFeasiblePositions([_|IJs],D) :-
		addFeasiblePositions(IJs,D).

	output(Vars) :-
		Array=..[a|Vars],
		output(Array,1,1,1).

	output(Array,I,J,N) :-N>64,!.
	output(Array,I,J,N) :-
		P is (I-1)*8+J,
		arg(P,Array,P1),
		I1 is (P1-1)//8+1,
		J1 is P1-(I1-1)*8,
		write((I,J)),write(' => '),write((I1,J1)),nl,
		N1 is N+1,
		output(Array,I1,J1,N1).

:- end_object.
