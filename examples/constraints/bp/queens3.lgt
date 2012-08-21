%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% distributed with B-Prolog (November 2010)

%   File   : queens.pl
%   Author : Neng-Fa ZHOU
%   Date   : 2000
%   Purpose: A linear-space program for solving the N-queens problem 

:- object(queens3).

	:- public(top/0).
	top:-
		N=96,
		top(N).

	:- public(go/1).
	go(N) :-
		queens(N).

	:- private(constrain_queen/3).	% avoid spurious compilation warnings

	queens(N):-
		statistics(runtime,[Start|_]),
		top(N),
		statistics(runtime,[End|_]),
		T is End-Start,
		write('%execution time ='), write(T), write(' milliseconds'),nl.

	top(N):-
	%	fd_vector_min_max(0,N), % set the size of bit vectors
		make_list(N,List),
		domain(List,1,N),
		constrain_queens(List,[]),
		labeling_ffc(List),
		write(List).

	make_list(0,[]):-!.
	make_list(N,[_|Rest]):-
		N1 is N-1,
		make_list(N1,Rest).

	constrain_queens([],_).
	constrain_queens([Q|Qs],Left):-
		constrain_queen(Q,Left,Qs),
		constrain_queens(Qs,[Q|Left]).
	
	% delay the constraint until Q is instantiated
	constrain_queen(Q,_,_),var(Q),{{ins(Q)}} => true.
	constrain_queen(Q,Left,Right) =>
		exclude_positions(Q,1,Left),
		exclude_positions(Q,1,Right).

	exclude_positions(_,_,[]).
	exclude_positions(Q0,N,[Q|Qs]):-
		R1 is Q0-N,
		R2 is Q0+N,
		domain_set_false(Q,Q0), % not in the same row
		domain_set_false(Q,R1), % not in the same diagonal
		domain_set_false(Q,R2), % not in the same diagonal
		N1 is N+1,
		exclude_positions(Q0,N1,Qs).

:- end_object.
