%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% code adapted to Logtalk by Paulo Moura from one of the examples
% distributed with SICStus Prolog 4.0.2 (November 2010)

/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Car Sequencing Problem
 * Author    : Mats Carlsson
 */

:- object(cars_ix).

	:- public([
		cars_ix/2,
		cars_ix2/2,
		cars_ix3/2
	]).

	:- use_module(clpfd, [
		domain/3, element/3, global_cardinality/2, labeling/2,
		(#=<)/2, (#>=)/2,
		op(700, xfx, #=<), op(760, yfx, #>=)
	]).

	/* seems to admit six solutions:
	[1,2,6,3,5,4,4,5,3,6]
	[1,3,6,2,5,4,3,5,4,6]
	[1,3,6,2,6,4,5,3,4,5]
	[5,4,3,5,4,6,2,6,3,1]
	[6,3,5,4,4,5,3,6,2,1]
	[6,4,5,3,4,5,2,6,3,1]
	*/

	cars_ix(Lab, X) :-
		% domains
		X=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
		Y=[O11,O12,O13,O14,O15,
		O21,O22,O23,O24,O25,
		O31,O32,O33,O34,O35,
		O41,O42,O43,O44,O45,
		O51,O52,O53,O54,O55,
		O61,O62,O63,O64,O65,
		O71,O72,O73,O74,O75,
		O81,O82,O83,O84,O85,
		O91,O92,O93,O94,O95,
		O101,O102,O103,O104,O105],
		% L1=[1,0,0,0,1,1], % cap 1/2
		% L2=[0,0,1,1,0,1], % cap 2/3
		% L3=[1,0,0,0,1,0], % cap 1/3
		% L4=[1,1,0,1,0,0], % cap 2/5
		% L5=[0,0,1,0,0,0], % cap 1/5
		domain(Y, 0, 1),
		domain(X, 1, 6),
		% problem constraints
		capacities(X),
		element1(X1, O11),
		element2(X1, O12),
		element3(X1, O13),
		element4(X1, O14),
		element5(X1, O15),
		element1(X2, O21),
		element2(X2, O22),
		element3(X2, O23),
		element4(X2, O24),
		element5(X2, O25),
		element1(X3, O31),
		element2(X3, O32),
		element3(X3, O33),
		element4(X3, O34),
		element5(X3, O35),
		element1(X4, O41),
		element2(X4, O42),
		element3(X4, O43),
		element4(X4, O44),
		element5(X4, O45),
		element1(X5, O51),
		element2(X5, O52),
		element3(X5, O53),
		element4(X5, O54),
		element5(X5, O55),
		element1(X6, O61),
		element2(X6, O62),
		element3(X6, O63),
		element4(X6, O64),
		element5(X6, O65),
		element1(X7, O71),
		element2(X7, O72),
		element3(X7, O73),
		element4(X7, O74),
		element5(X7, O75),
		element1(X8, O81),
		element2(X8, O82),
		element3(X8, O83),
		element4(X8, O84),
		element5(X8, O85),
		element1(X9, O91),
		element2(X9, O92),
		element3(X9, O93),
		element4(X9, O94),
		element5(X9, O95),
		element1(X10, O101),
		element2(X10, O102),
		element3(X10, O103),
		element4(X10, O104),
		element5(X10, O105),
		1 #>= O11+O21,
		1 #>= O21+O31,
		1 #>= O31+O41,
		1 #>= O41+O51,
		1 #>= O51+O61,
		1 #>= O61+O71,
		1 #>= O71+O81,
		1 #>= O81+O91,
		1 #>= O91+O101,
		2 #>= O12+O22+O32,
		2 #>= O22+O32+O42,
		2 #>= O32+O42+O52,
		2 #>= O42+O52+O62,
		2 #>= O52+O62+O72,
		2 #>= O62+O72+O82,
		2 #>= O72+O82+O92,
		2 #>= O82+O92+O102,
		1 #>= O13+O23+O33,
		1 #>= O23+O33+O43,
		1 #>= O33+O43+O53,
		1 #>= O43+O53+O63,
		1 #>= O53+O63+O73,
		1 #>= O63+O73+O83,
		1 #>= O73+O83+O93,
		1 #>= O83+O93+O103,
		2 #>= O14+O24+O34+O44+O54,
		2 #>= O24+O34+O44+O54+O64,
		2 #>= O34+O44+O54+O64+O74,
		2 #>= O44+O54+O64+O74+O84,
		2 #>= O54+O64+O74+O84+O94,
		2 #>= O64+O74+O84+O94+O104,
		1 #>= O15+O25+O35+O45+O55,
		1 #>= O25+O35+O45+O55+O65,
		1 #>= O35+O45+O55+O65+O75,
		1 #>= O45+O55+O65+O75+O85,
		1 #>= O55+O65+O75+O85+O95,
		1 #>= O65+O75+O85+O95+O105,
		% redundant constraints
		O11+O21+O31+O41+O51+O61+O71+O81 #>= 4,
		O11+O21+O31+O41+O51+O61         #>= 3,
		O11+O21+O31+O41                 #>= 2,
		O11+O21                         #>= 1,
		O12+O22+O32+O42+O52+O62+O72     #>= 4,
		O12+O22+O32+O42                 #>= 2,
		O12                             #>= 0,
		O13+O23+O33+O43+O53+O63+O73     #>= 2,
		O13+O23+O33+O43                 #>= 1,
		O13                             #>= 0,
		O14+O24+O34+O44+O54             #>= 2,
		O15+O25+O35+O45+O55             #>= 1,
		% labeling
		labeling(Lab, X).


	cars_ix2(Lab, X) :-
		% domains
		X=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
		Y=[O11,O12,O13,O14,O15,
		O21,O22,O23,O24,O25,
		O31,O32,O33,O34,O35,
		O41,O42,O43,O44,O45,
		O51,O52,O53,O54,O55,
		O61,O62,O63,O64,O65,
		O71,O72,O73,O74,O75,
		O81,O82,O83,O84,O85,
		O91,O92,O93,O94,O95,
		O101,O102,O103,O104,O105],
		% L1=[1,0,0,0,1,1], % cap 1/2
		% L2=[0,0,1,1,0,1], % cap 2/3
		% L3=[1,0,0,0,1,0], % cap 1/3
		% L4=[1,1,0,1,0,0], % cap 2/5
		% L5=[0,0,1,0,0,0], % cap 1/5
		domain(Y, 0, 1),
		domain(X, 1, 6),
		% problem constraints
		capacities(X),
		element1(X1, O11),
		element2(X1, O12),
		element3(X1, O13),
		element4(X1, O14),
		element5(X1, O15),
		element1(X2, O21),
		element2(X2, O22),
		element3(X2, O23),
		element4(X2, O24),
		element5(X2, O25),
		element1(X3, O31),
		element2(X3, O32),
		element3(X3, O33),
		element4(X3, O34),
		element5(X3, O35),
		element1(X4, O41),
		element2(X4, O42),
		element3(X4, O43),
		element4(X4, O44),
		element5(X4, O45),
		element1(X5, O51),
		element2(X5, O52),
		element3(X5, O53),
		element4(X5, O54),
		element5(X5, O55),
		element1(X6, O61),
		element2(X6, O62),
		element3(X6, O63),
		element4(X6, O64),
		element5(X6, O65),
		element1(X7, O71),
		element2(X7, O72),
		element3(X7, O73),
		element4(X7, O74),
		element5(X7, O75),
		element1(X8, O81),
		element2(X8, O82),
		element3(X8, O83),
		element4(X8, O84),
		element5(X8, O85),
		element1(X9, O91),
		element2(X9, O92),
		element3(X9, O93),
		element4(X9, O94),
		element5(X9, O95),
		element1(X10, O101),
		element2(X10, O102),
		element3(X10, O103),
		element4(X10, O104),
		element5(X10, O105),
		ix_le(O11,O21, 1),
		ix_le(O21,O31, 1),
		ix_le(O31,O41, 1),
		ix_le(O41,O51, 1),
		ix_le(O51,O61, 1),
		ix_le(O61,O71, 1),
		ix_le(O71,O81, 1),
		ix_le(O81,O91, 1),
		ix_le(O91,O101, 1),
		ix_le(O12,O22,O32, 2),
		ix_le(O22,O32,O42, 2),
		ix_le(O32,O42,O52, 2),
		ix_le(O42,O52,O62, 2),
		ix_le(O52,O62,O72, 2),
		ix_le(O62,O72,O82, 2),
		ix_le(O72,O82,O92, 2),
		ix_le(O82,O92,O102, 2),
		ix_le(O13,O23,O33, 1),
		ix_le(O23,O33,O43, 1),
		ix_le(O33,O43,O53, 1),
		ix_le(O43,O53,O63, 1),
		ix_le(O53,O63,O73, 1),
		ix_le(O63,O73,O83, 1),
		ix_le(O73,O83,O93, 1),
		ix_le(O83,O93,O103, 1),
		ix_le(O14,O24,O34,O44,O54, 2),
		ix_le(O24,O34,O44,O54,O64, 2),
		ix_le(O34,O44,O54,O64,O74, 2),
		ix_le(O44,O54,O64,O74,O84, 2),
		ix_le(O54,O64,O74,O84,O94, 2),
		ix_le(O64,O74,O84,O94,O104, 2),
		ix_le(O15,O25,O35,O45,O55, 1),
		ix_le(O25,O35,O45,O55,O65, 1),
		ix_le(O35,O45,O55,O65,O75, 1),
		ix_le(O45,O55,O65,O75,O85, 1),
		ix_le(O55,O65,O75,O85,O95, 1),
		ix_le(O65,O75,O85,O95,O105, 1),
		% redundant constraints
		ix_ge(O11,O21,O31,O41,O51,O61,O71,O81 , 4),
		ix_ge(O11,O21,O31,O41,O51,O61         , 3),
		ix_ge(O11,O21,O31,O41                 , 2),
		ix_ge(O11,O21                         , 1),
		ix_ge(O12,O22,O32,O42,O52,O62,O72     , 4),
		ix_ge(O12,O22,O32,O42                 , 2),
		% O12                           , 0,
		ix_ge(O13,O23,O33,O43,O53,O63,O73     , 2),
		ix_ge(O13,O23,O33,O43                 , 1),
		% O13                           , 0,
		ix_ge(O14,O24,O34,O44,O54             , 2),
		ix_ge(O15,O25,O35,O45,O55             , 1),
		% labeling
		labeling(Lab, X).

	cars_ix3(Lab, X) :-
		% domains
		X=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
		Y=[O11,O12,O13,O14,O15,
		O21,O22,O23,O24,O25,
		O31,O32,O33,O34,O35,
		O41,O42,O43,O44,O45,
		O51,O52,O53,O54,O55,
		O61,O62,O63,O64,O65,
		O71,O72,O73,O74,O75,
		O81,O82,O83,O84,O85,
		O91,O92,O93,O94,O95,
		O101,O102,O103,O104,O105],
		L1=[1,0,0,0,1,1], % cap 1/2
		L2=[0,0,1,1,0,1], % cap 2/3
		L3=[1,0,0,0,1,0], % cap 1/3
		L4=[1,1,0,1,0,0], % cap 2/5
		L5=[0,0,1,0,0,0], % cap 1/5
		domain(Y, 0, 1),
		domain(X, 1, 6),
		% problem constraints
		capacities(X),
		element(X1, L1, O11),
		element(X1, L2, O12),
		element(X1, L3, O13),
		element(X1, L4, O14),
		element(X1, L5, O15),
		element(X2, L1, O21),
		element(X2, L2, O22),
		element(X2, L3, O23),
		element(X2, L4, O24),
		element(X2, L5, O25),
		element(X3, L1, O31),
		element(X3, L2, O32),
		element(X3, L3, O33),
		element(X3, L4, O34),
		element(X3, L5, O35),
		element(X4, L1, O41),
		element(X4, L2, O42),
		element(X4, L3, O43),
		element(X4, L4, O44),
		element(X4, L5, O45),
		element(X5, L1, O51),
		element(X5, L2, O52),
		element(X5, L3, O53),
		element(X5, L4, O54),
		element(X5, L5, O55),
		element(X6, L1, O61),
		element(X6, L2, O62),
		element(X6, L3, O63),
		element(X6, L4, O64),
		element(X6, L5, O65),
		element(X7, L1, O71),
		element(X7, L2, O72),
		element(X7, L3, O73),
		element(X7, L4, O74),
		element(X7, L5, O75),
		element(X8, L1, O81),
		element(X8, L2, O82),
		element(X8, L3, O83),
		element(X8, L4, O84),
		element(X8, L5, O85),
		element(X9, L1, O91),
		element(X9, L2, O92),
		element(X9, L3, O93),
		element(X9, L4, O94),
		element(X9, L5, O95),
		element(X10, L1, O101),
		element(X10, L2, O102),
		element(X10, L3, O103),
		element(X10, L4, O104),
		element(X10, L5, O105),
		ix_le(O11,O21, 1),
		ix_le(O21,O31, 1),
		ix_le(O31,O41, 1),
		ix_le(O41,O51, 1),
		ix_le(O51,O61, 1),
		ix_le(O61,O71, 1),
		ix_le(O71,O81, 1),
		ix_le(O81,O91, 1),
		ix_le(O91,O101, 1),
		ix_le(O12,O22,O32, 2),
		ix_le(O22,O32,O42, 2),
		ix_le(O32,O42,O52, 2),
		ix_le(O42,O52,O62, 2),
		ix_le(O52,O62,O72, 2),
		ix_le(O62,O72,O82, 2),
		ix_le(O72,O82,O92, 2),
		ix_le(O82,O92,O102, 2),
		ix_le(O13,O23,O33, 1),
		ix_le(O23,O33,O43, 1),
		ix_le(O33,O43,O53, 1),
		ix_le(O43,O53,O63, 1),
		ix_le(O53,O63,O73, 1),
		ix_le(O63,O73,O83, 1),
		ix_le(O73,O83,O93, 1),
		ix_le(O83,O93,O103, 1),
		ix_le(O14,O24,O34,O44,O54, 2),
		ix_le(O24,O34,O44,O54,O64, 2),
		ix_le(O34,O44,O54,O64,O74, 2),
		ix_le(O44,O54,O64,O74,O84, 2),
		ix_le(O54,O64,O74,O84,O94, 2),
		ix_le(O64,O74,O84,O94,O104, 2),
		ix_le(O15,O25,O35,O45,O55, 1),
		ix_le(O25,O35,O45,O55,O65, 1),
		ix_le(O35,O45,O55,O65,O75, 1),
		ix_le(O45,O55,O65,O75,O85, 1),
		ix_le(O55,O65,O75,O85,O95, 1),
		ix_le(O65,O75,O85,O95,O105, 1),
		% redundant constraints
		ix_ge(O11,O21,O31,O41,O51,O61,O71,O81 , 4),
		ix_ge(O11,O21,O31,O41,O51,O61         , 3),
		ix_ge(O11,O21,O31,O41                 , 2),
		ix_ge(O11,O21                         , 1),
		ix_ge(O12,O22,O32,O42,O52,O62,O72     , 4),
		ix_ge(O12,O22,O32,O42                 , 2),
		% O12                           , 0,
		ix_ge(O13,O23,O33,O43,O53,O63,O73     , 2),
		ix_ge(O13,O23,O33,O43                 , 1),
		% O13                           , 0,
		ix_ge(O14,O24,O34,O44,O54             , 2),
		ix_ge(O15,O25,O35,O45,O55             , 1),
		% labeling
		labeling(Lab, X).


	/*
	capacities(X) :-
		atmost(1, X, 1),
		atmost(1, X, 2),
		atmost(2, X, 3),
		atmost(2, X, 4),
		atmost(2, X, 5),
		atmost(2, X, 6).

	atmost(N, List, Val) :-
		count(Val, List, #=<, N).
	*/

	capacities(X) :-
		domain([C1,C2], 0, 1),
		domain([C3,C4,C5,C6], 0, 2),
		global_cardinality(X, [1-C1,2-C2,3-C3,4-C4,5-C5,6-C6]).

	element1(X, O) +:
		element(X, [1,0,0,0,1,1], O).
	element2(X, O) +:
		element(X, [0,0,1,1,0,1], O).
	element3(X, O) +:
		element(X, [1,0,0,0,1,0], O).
	element4(X, O) +:
		element(X, [1,1,0,1,0,0], O).
	element5(X, O) +:
		element(X, [0,0,1,0,0,0], O).


	ix_le(A,B,I) +: A+B #=< I.

	ix_le(A,B,C,I) +: A+B+C #=< I.

	ix_le(A,B,C,D,E,I) +: A+B+C+D+E #=< I.


	ix_ge(A,B,I) +: A+B #>= I.

	ix_ge(A,B,C,I) +: A+B+C #>= I.

	ix_ge(A,B,C,D,I) +: A+B+C+D #>= I.

	ix_ge(A,B,C,D,E,I) +: A+B+C+D+E #>= I.

	ix_ge(A,B,C,D,E,F,I) +: A+B+C+D+E+F #>= I.

	ix_ge(A,B,C,D,E,F,G,I) +: A+B+C+D+E+F+G #>= I.

	ix_ge(A,B,C,D,E,F,G,H,I) +: A+B+C+D+E+F+G+H #>= I.

:- end_object.
