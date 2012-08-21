%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples distributed 
% with GNU Prolog 1.3.0 (August 2008)

/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : alpha.pl                                               */
/* Title          : alphacipher                                            */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : January 1993                                           */
/*                                                                         */
/* This problem comes from the news group rec.puzzle.                      */
/* The numbers 1 - 26 have been randomly assigned to the letters of the    */
/* alphabet. The numbers beside each word are the total of the values      */
/* assigned to the letters in the word. e.g for LYRE L,Y,R,E might equal   */
/* 5,9,20 and 13 respectively or any other combination that add up to 47.  */
/* Find the value of each letter under the equations:                      */
/*                                                                         */
/*    BALLET  45     GLEE  66     POLKA      59     SONG     61            */
/*    CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82            */
/*    CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72            */
/*    FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100            */
/*    FUGUE   50     OPERA 65     SOLO       37     WALTZ    34            */
/*                                                                         */
/* Solution:                                                               */
/*  [A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z] */
/*  [5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18] */
/*-------------------------------------------------------------------------*/


:- object(alpha).

	:- initialization(q).

	:- public(q/2).

	q(LD, Y) :-
		get_fd_labeling(Lab), statistics(runtime,_),
		alpha(LD, Lab), statistics(runtime,[_,Y]).

	q :-
		q(LD, Y),
		write(LD), nl,
		write('time : '), write(Y), nl.

	alpha(LD, Lab) :-
		fd_set_vector_max(26),
		LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],

		fd_all_different(LD),
		fd_domain(LD, 1, 26),

		B+A+L+L+E+T       #= 45,
		C+E+L+L+O         #= 43,
		C+O+N+C+E+R+T     #= 74,
		F+L+U+T+E         #= 30,
		F+U+G+U+E         #= 50,
		G+L+E+E           #= 66,
		J+A+Z+Z           #= 58,
		L+Y+R+E           #= 47,
		O+B+O+E           #= 53,
		O+P+E+R+A         #= 65,
		P+O+L+K+A         #= 59,
		Q+U+A+R+T+E+T     #= 50,
		S+A+X+O+P+H+O+N+E #= 134,
		S+C+A+L+E         #= 51,
		S+O+L+O           #= 37,
		S+O+N+G           #= 61,
		S+O+P+R+A+N+O     #= 82,
		T+H+E+M+E         #= 72,
		V+I+O+L+I+N       #= 100,
		W+A+L+T+Z         #= 34,

		lab(Lab, LD).

	lab(normal, L):-  
		fd_labeling(L).
	lab(ff, L):-
		fd_labelingff(L).

	get_fd_labeling(Lab):- 
		argument_counter(C),
		get_labeling1(C, Lab).

	get_labeling1(1, normal).
	get_labeling1(2, Lab):-
		argument_value(1, Lab).

:- end_object.
