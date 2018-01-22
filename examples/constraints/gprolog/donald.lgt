%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples distributed 
% with GNU Prolog 1.3.0 (August 2008)

/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : donald.pl                                              */
/* Title          : crypt-arithmetic                                       */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* Solve the operation:                                                    */
/*                                                                         */
/*    D O N A L D                                                          */
/*  + G E R A L D                                                          */
/*  --------------                                                         */
/*  = R O B E R T                                                          */
/*                                                                         */
/* (resolution by line)                                                    */
/*                                                                         */
/* Solution:                                                               */
/*  [D,O,N,A,L,G,E,R,B,T]                                                  */
/*  [5,2,6,4,8,1,9,7,3,0]                                                  */
/*-------------------------------------------------------------------------*/


:- object(donald).

	:- initialization(q).

	:- public(q/2).

	q(LD, Y) :-
		get_fd_labeling(Lab), statistics(runtime,_),
		donald(LD, Lab), statistics(runtime,[_,Y]).

	q :-
		q(LD, Y),
		write(LD), nl,
		write('time : '), write(Y), nl.

	donald(LD,Lab) :-
		fd_set_vector_max(9),
		LD = [D,O,N,A,L,G,E,R,B,T],
		fd_all_different(LD),
		fd_domain(LD, 0, 9),
		fd_domain([D, G], 1, 9),

			100000*D + 10000*O + 1000*N + 100*A + 10*L + D +
			100000*G + 10000*E + 1000*R + 100*A + 10*L + D
		#=  100000*R + 10000*O + 1000*B + 100*E + 10*R + T,

		lab(Lab, LD).

	lab(normal, L) :-
		fd_labeling(L).
	lab(ff, L) :-
		fd_labelingff(L).

	get_fd_labeling(Lab) :-
		argument_counter(C),
		get_labeling1(C, Lab).

	get_labeling1(1, normal).
	get_labeling1(2, Lab) :-
		argument_value(1, Lab).

:- end_object.
