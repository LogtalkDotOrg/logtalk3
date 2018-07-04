% generated: 10 November 1989
% option(s): 
%
%   (queens) queens_8
%
%   from Sterling and Shapiro, "The Art of Prolog," page 211.
%
%   solve the 8 queens problem


%   This program solves the N queens problem:  place N pieces on an N
%   by N rectangular board so that no two pieces are on the same line
%   - horizontal, vertical, or diagonal.  (N queens so placed on an N
%   by N chessboard are unable to attack each other in a single move
%   under the rules of chess.)  The strategy is incremental generate-
%   and-test.
%
%   A solution is specified by a permutation of the list of numbers 1 to
%   N.  The first element of the list is the row number for the queen in
%   the first column, the second element is the row number for the queen
%   in the second column, et cetera.  This scheme implicitly incorporates
%   the observation that any solution of the problem has exactly one queen
%   in each column.
%
%   The program distinguishes symmetric solutions.  For example, 
%
%   ?- queens(4, Qs).
%
%   produces
%
%   Qs = [3,1,4,2] ;
%
%   Qs = [2,4,1,3]

top:-queens(8,Qs),fail.
top.

queens(N,Qs) :-
	range(1,N,Ns),
	queens(Ns,[],Qs).

queens([],Qs,Qs).
queens(UnplacedQs,SafeQs,Qs) :-
	select(UnplacedQs,UnplacedQs1,Q),
	not_attack(SafeQs,Q),
	queens(UnplacedQs1,[Q|SafeQs],Qs).

not_attack(Xs,X) :-
	not_attack(Xs,X,1).

not_attack([],_,_) :- !.
not_attack([Y|Ys],X,N) :-
	X =\= Y+N, X =\= Y-N,
	N1 is N+1,
	not_attack(Ys,X,N1).

select([X|Xs],Xs,X).
select([Y|Ys],[Y|Zs],X) :- select(Ys,Zs,X).

range(N,N,[N]) :- !.
range(M,N,[M|Ns]) :-
	M < N,
	M1 is M+1,
	range(M1,N,Ns).


