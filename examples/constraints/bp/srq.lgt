%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


% code adapted to Logtalk by Paulo Moura from one of the CLP(FD) examples
% distributed with B-Prolog (November 2010)

/************************************************************************/
/* A solution to SRQ (Self-referential quizzes) implemented in B-Prolog.*/
/*                                                                      */
/* Antonio J. Fernandez                                                 */
/*                                                                      */
/*                   September 1997                                     */
/*                                                                      */
/************************************************************************/

:- object(srq).

	:- public(q/0).
	q:-
		statistics(runtime,_), srq(L), statistics(runtime,[_,Y]),
		write(L), nl,
		write('time : '), write(Y),nl.

	/* Use next predicate q to see the time in calculate all the solutions.*/
	:- public(q_all/0).
	q_all :-
		statistics(runtime,_),
		(	srq(L), 
			write(L), nl,
			fail
		;	nl
		),
		statistics(runtime,[_,Y]),
		write('no more solutions '),
		write('time all: '),write(Y), nl.

	:- private(iff/3).		% avoid spurious compilation warnings

	srq(L) :-
		L=[A,B,C,D,E],

	/* Defining domain variables and their domains */

		A=[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10],
		B=[B1,B2,B3,B4,B5,B6,B7,B8,B9,B10],
		C=[C1,C2,C3,C4,C5,C6,C7,C8,C9,C10],
		D=[D1,D2,D3,D4,D5,D6,D7,D8,D9,D10],
		E=[E1,E2,E3,E4,E5,E6,E7,E8,E9,E10],

		domain(A,0,1),domain(B,0,1),domain(C,0,1),domain(D,0,1),domain(E,0,1),

	/* We must define the negated variables because a direct negation     */
	/* function such as ~ in Oz  does not exist in clp(FD), and for that  */
	/* reason we must use not(X,Y) which is true if Y=~X                  */


		An=[A1n,A2n,A3n,A4n,A5n,A6n,A7n],
		Bn=[B2n,B4n,B6n,B8n],
		En=[E6n,E7n,E8n,E9n,E10n],
		domain(An,0,1),domain(Bn,0,1),domain(En,0,1),

		only_one([A1,B1,C1,D1,E1]),
		only_one([A2,B2,C2,D2,E2]),
		only_one([A3,B3,C3,D3,E3]),
		only_one([A4,B4,C4,D4,E4]),
		only_one([A5,B5,C5,D5,E5]),
		only_one([A6,B6,C6,D6,E6]),
		only_one([A7,B7,C7,D7,E7]),
		only_one([A8,B8,C8,D8,E8]),
		only_one([A9,B9,C9,D9,E9]),
		only_one([A10,B10,C10,D10,E10]),

	/* Defining the negated variables */

		not(A1,A1n),not(A2,A2n),not(A3,A3n),not(A4,A4n),not(A5,A5n),
		not(A6,A6n),not(A7,A7n),
		not(B2,B2n),not(B4,B4n),not(B6,B6n),not(B8,B8n),
		not(E6,E6n),not(E7,E7n),not(E8,E8n),not(E9,E9n),not(E10,E10n),
	/*Redundant constraints   */
	/*      10 #= SmAE + SmBCD, */
	/*Question 1  */
		and(A1n,A2n,A12n),and(A12n,A3n,A123n),and(A123n,A4,A1),
		and(A12n,A3,B1),
		and(A1n,A2,C1),
		D1=A1,
		and(A123n,A4n,E1),
	/*Question 2  */
		eq(A3,B3,C3,D3,E3,A4,B4,C4,D4,E4,A2),
		eq(A4,B4,C4,D4,E4,A5,B5,C5,D5,E5,B2),
		eq(A5,B5,C5,D5,E5,A6,B6,C6,D6,E6,C2),
		eq(A6,B6,C6,D6,E6,A7,B7,C7,D7,E7,D2),
		eq(A7,B7,C7,D7,E7,A8,B8,C8,D8,E8,E2),
	/*Question 3 */
		and(A4n,A5n,A45n),and(A45n,A6n,A456n),and(A456n,A7n,A4567n),
		A3=A4,
		and(A5,A4n,B3),
		and(A45n,A6,C3),
		and(A456n,A7,D3),
		and(A4567n,A8,E3),
	/*Question 4 */
		and(B2n,B4n,B24n),and(B24n,B6n,B246n),and(B246n,B8n,B2468n),
		A4=B2,
		and(B2n,B4,B4),
		and(B24n,B6,C4),
		and(B246n,B8,D4),
		and(B2468n,B10,E4),
	/*Question 5 */
		A5=C1,  B5=C3,  C5=C5,  D5=C7,  E5=C9,
	/*Question 6  */
		Aft in 0..1, Aftn in 0..1, Bef in 0..1, Befn in 0..1,  
		SmD in 0..10, DD12 in 0..1,
		DD34 in 0..1,DD1234 in 0..1, DD78 in 0..1, DD910 in 0..1,

		or(D1,D2,DD12),
		or(D3,D4,DD34),
		or(DD12,DD34,DD1234),
		or(DD1234,D5,Bef),
		or(D7,D8,DD78),
		or(D9,D10,DD910),
		or(DD78,DD910,Aft),
		not(Aft,Aftn),
		not(Bef,Befn),
		and(Bef,Aftn,A6),
		and(Aft,Befn,B6),
		and(Bef,Aft,C6),

		SmD #= D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10,

		iff(SmD,0,D6),
		E6=D6, 
	/*Question 7  */
		E678910n in 0..1,
		E78910n in 0..1,
		E8910n in 0..1,
		E910n in 0..1,
		E10n in 0..1,

		and(E9n,E10n,E910n),and(E8n,E910n,E8910n),and(E7n,E8910n,E78910n),
		and(E6n,E78910n,E678910n),
		and(E5,E678910n,A7),
		and(E6,E78910n,B7),
		and(E7,E8910n,C7),
		and(E8,E910n,D7),
		and(E9,E10n,E7),
	/*Question 8  */
		SmB in 0..10, SmC in 0..10, SmBC in 0..10, SmBCD in 0..10,
		SmB #= B1 + B2 + B3 + B4 + B5 + B6 + B7 + B8 + B9 + B10,
		SmC #= C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10,
		SmBC #= SmB + SmC,
		SmBCD #= SmBC + SmD,

		iff(SmBCD,7,A8),
		iff(SmBCD,6,B8),
		iff(SmBCD,5,C8),
		iff(SmBCD,4,D8),
		iff(SmBCD,3,E8),

	/*Question 9  */
		SmA in 0..10, SmE in 0..10, SmAE in 0..10,
		SmA #= A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + A9 + A10,
		SmE #= E1 + E2 + E3 + E4 + E5 + E6 + E7 + E8 + E9 + E10,
		SmAE #= SmA + SmE,  
		iff(SmAE,0,A9),
		iff(SmAE,1,B9),
		iff(SmAE,2,C9),
		iff(SmAE,3,D9),
		iff(SmAE,4,E9),

	/*Labeling. Use one . See in the end the meaning of each call*/
	/* labeling_list(L).*/
		labelingff_list(L).
	/*labelingdd_list(L).*/

	/* eq/11 calculates the equality between two question in the SRQ */

	eq(X1,X2,X3,X4,X5,Y1,Y2,Y3,Y4,Y5,Result) :-
		equiv(X1,Y1,Z1),equiv(X2,Y2,Z2),equiv(X3,Y3,Z3),equiv(X4,Y4,Z4),
		equiv(X5,Y5,Z5),
		and(Z1,Z2,Z12),and(Z3,Z4,Z34),and(Z12,Z34,Z1234),
		and(Z1234,Z5,Result).


	iff(X,Y,B),dvar(B),dvar(X),{{ins(B),ins(X),ins(Y)}} => true.
	iff(_,Y,B),dvar(B),dvar(Y),{{ins(B),ins(Y)}} => true.
	iff(X,Y,B), integer(B) => (B=:=1 -> X #= Y; X #\= Y).
	iff(X,Y,B), X=:=Y => (B=1).
	iff(_,_,B) => (B=0).

	/*DIFFERENT LABELING STRATEGY. Use the most appropiate in the solution */

	/* Normal */
	labeling_list([]) :-!.
	labeling_list([A|B]) :-labeling(A),labeling_list(B).

	/*first fail heuristic*/
	labelingff_list([]) :-!.
	labelingff_list([A|B]) :-labelingff(A),labelingff_list(B).

	/*Smallest domain. Using deleteff */
	labelingffc_list([]) :-!.
	labelingffc_list([A|B]) :- labelingffc(A),labelingffc_list(B).

:- end_object.
