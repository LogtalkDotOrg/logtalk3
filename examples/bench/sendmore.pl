
% Cryptoaddition:
% Find the unique answer to:
%  SEND
% +MORE
% -----
% MONEY
% where each letter is a distinct digit.

top:-
	digit(D), digit(E), D=\=E,
	sumdigit(0, D, E, Y, C1),
	digit(N), N=\=Y, N=\=E, N=\=D,
	digit(R), R=\=N, R=\=Y, R=\=E, R=\=D,
	sumdigit(C1,N, R, E, C2),
	digit(O), O=\=R, O=\=N, O=\=Y, O=\=E, O=\=D,
	sumdigit(C2,E, O, N, C3),
	leftdigit(S), S=\=O, S=\=R, S=\=N, S=\=Y, S=\=E, S=\=D,
	leftdigit(M), M=\=S, M=\=O, M=\=R, M=\=N, M=\=Y, M=\=E, M=\=D,
	sumdigit(C3,S, M, O, M),
	% write(' '),write(S),write(E),write(N),write(D),nl,
	% write('+'),write(M),write(O),write(R),write(E),nl,
	% write('-----'),nl,
	% write(M),write(O),write(N),write(E),write(Y),nl,nl,
	fail.
top.

sumdigit(C, A, B, S, D) :-
	X is (C+A+B),
	(X<10
	-> S=X,       D=0
	;  S is X-10, D=1
	).

digit(0).
digit(1).
digit(2).
digit(3).
digit(4).
digit(5).
digit(6).
digit(7).
digit(8).
digit(9).

leftdigit(1).
leftdigit(2).
leftdigit(3).
leftdigit(4).
leftdigit(5).
leftdigit(6).
leftdigit(7).
leftdigit(8).
leftdigit(9).
