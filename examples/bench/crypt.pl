
% Cryptomultiplication:
% Find the unique answer to:
%	OEE
%	 EE
% 	---
%      EOEE
%      EOE
%      ----
%      OOEE
%
% where E=even, O=odd.
% This program generalizes easily to any such problem.
% Written by Peter Van Roy

top:-
	odd(A), even(B), even(C),
	even(E),
	mult([C,B,A], E, [I,H,G,F|X]),
	lefteven(F), odd(G), even(H), even(I), zero(X),
	lefteven(D),
	mult([C,B,A], D, [L,K,J|Y]),
	lefteven(J), odd(K), even(L), zero(Y),
	sum([I,H,G,F], [0,L,K,J], [P,O,N,M|Z]),
	odd(M), odd(N), even(O), even(P), zero(Z).
	% write(' '), write(A), write(B), write(C), nl,
	% write('  '), write(D), write(E), nl,
	% write(F), write(G), write(H), write(I), nl,
	% write(J), write(K), write(L), nl,
	% write(M), write(N), write(O), write(P), nl.

% Addition of two numbers
sum(AL, BL, CL) :- sum(AL, BL, 0, CL).

sum([A|AL], [B|BL], Carry, [C|CL]) :- !,
	X is (A+B+Carry),
	C is X mod 10,
	NewCarry is X // 10,
	sum(AL, BL, NewCarry, CL).
sum([], BL, 0, BL) :- !.
sum(AL, [], 0, AL) :- !.
sum([], [B|BL], Carry, [C|CL]) :- !,
	X is B+Carry,
	NewCarry is X // 10,
	C is X mod 10,
	sum([], BL, NewCarry, CL).
sum([A|AL], [], Carry, [C|CL]) :- !,
	X is A+Carry,
	NewCarry is X // 10,
	C is X mod 10,
	sum([], AL, NewCarry, CL).
sum([], [], Carry, [Carry]).

% Multiplication
mult(AL, D, BL) :- mult(AL, D, 0, BL).

mult([A|AL], D, Carry, [B|BL] ) :-
	X is A*D+Carry,
	B is X mod 10,
	NewCarry is X // 10,
	mult(AL, D, NewCarry, BL).
mult([], _, Carry, [C,Cend]) :-
	C is Carry mod 10,
	Cend is Carry // 10.

zero([]).
zero([0|L]) :- zero(L).

odd(1).
odd(3).
odd(5).
odd(7).
odd(9).

even(0).
even(2).
even(4).
even(6).
even(8).

lefteven(2).
lefteven(4).
lefteven(6).
lefteven(8).
