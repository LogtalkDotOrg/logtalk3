%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(miss_cann,
	instantiates(heuristic_state_space)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2000/11/21,
		comment is 'Missionaries and cannibals heuristic state space search problem.'
	]).

	:- uses(loop, [forto/3]).

	initial_state(start, ((3,3), left, (0,0))).

	goal_state(end, ((0,0), right, (3,3))).

	print_state(((Ml,Cl), B, (Mr,Cr))) :-
		forto(1, Ml, write('M')),
		forto(1, Cl, write('C')),
		(	B = left ->
			write('.<__>..........')
		;	write('..........<__>.')
		),
		forto(1, Mr, write('M')),
		forto(1, Cr, write('C')),
		nl.

	next_state(((Ml,Cl),left,(Mr,Cr)), ((Ml2,Cl2),right,(Mr2,Cr2)), 1) :-  %mm
		Ml >= 2,
		once((Ml - 2 =:= 0; Ml - 2 >= Cl)),
		Cr =< 2,
		Ml2 is Ml - 2,
		Cl2 is Cl,
		Mr2 is Mr + 2,
		Cr2 is Cr.
	next_state(((Ml,Cl),left,(Mr,Cr)), ((Ml2,Cl2),right,(Mr2,Cr2)), 2) :-  %m
		Ml >= 1,
		once((Ml - 1 =:= 0; Ml - 1 >= Cl)),
		Cr =< 1,
		Ml2 is Ml - 1,
		Cl2 is Cl,
		Mr2 is Mr + 1,
		Cr2 is Cr.
	next_state(((Ml,Cl),left,(Mr,Cr)), ((Ml2,Cl2),right,(Mr2,Cr2)), 1) :-  %cc
		Cl >= 2,
		once((Mr >= Cr + 2;  Mr =:= 0)),
		Ml2 is Ml,
		Cl2 is Cl - 2,
		Mr2 is Mr,
		Cr2 is Cr + 2.
	next_state(((Ml,Cl),left,(Mr,Cr)), ((Ml2,Cl2),right,(Mr2,Cr2)), 2) :-  %c
		Cl >= 1,
		once((Mr >= Cr + 1; Mr =:= 0)),
		Ml2 is Ml,
		Cl2 is Cl - 1,
		Mr2 is Mr,
		Cr2 is Cr + 1.
	next_state(((Ml,Cl),left,(Mr,Cr)), ((Ml2,Cl2),right,(Mr2,Cr2)), 1) :-  %mc
		Ml >= 1,
		Cl >= 1,
		Mr >= Cr,
		Ml2 is Ml - 1,
		Cl2 is Cl - 1,
		Mr2 is Mr + 1,
		Cr2 is Cr + 1.
	next_state(((Ml,Cl),right,(Mr,Cr)), ((Ml2,Cl2),left,(Mr2,Cr2)), Cost) :-
		next_state(((Mr,Cr),left,(Ml,Cl)), ((Mr2,Cr2),right,(Ml2,Cl2)), Cost).

	heuristic(((_, _), _, (Mr, Cr)), Cost) :-
		Cost is 6 - (Mr + Cr).

:- end_object.
