%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  
%  Copyright 2016 Metagol authors
%  Copyright 2018-2019 Paulo Moura
%  All rights reserved.
%  
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions
%  are met:
%  
%  1. Redistributions of source code must retain the above copyright
%     notice, this list of conditions and the following disclaimer.
%  
%  2. Redistributions in binary form must reproduce the above copyright
%     notice, this list of conditions and the following disclaimer in
%     the documentation and/or other materials provided with the
%     distribution.
%  
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(hook, metagol).


:- object(trains,
	extends(metagol)).

%% tell metagol to use the BK
prim(car/1).
prim(shape/1).
prim(train/1).
prim(short/1).
prim(closed/1).
prim(long/1).
prim(open_car/1).
prim(load/3).
prim(wheels/2).
prim(has_car/2).
prim(double/1).
prim(jagged/1).

%% metarules
metarule([P,Q],([P,A]:-[[Q,A]])).
metarule([P,Q,R],([P,A]:-[[Q,A],[R,A]])).
metarule([P,Q,R],([P,A]:-[[Q,A,B],[R,B]])).
metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
metarule([P,Q,R],([P,A,B]:-[[Q,A,B],[R,B]])).
metarule([P,Q,X],([P,A]:-[[Q,A,X]])).
metarule([P,Q,X],([P,A,B]:-[[Q,A,B,X]])).

:- public(learn/1).
learn(Clauses) :-
	Pos = [
		e(east1),
		e(east2),
		e(east3),
		e(east4),
		e(east5)
	],
	Neg = [
		e(west6),
		e(west7),
		e(west8),
		e(west9),
		e(west10)
	],
	::learn(Pos, Neg, Prog),
	::pclauses(Prog, Clauses).

:- public(learn/0).
learn :-
	learn(Clauses),
	::pprint_clauses(Clauses).

% type definitions
car(car_11).
car(car_12).
car(car_13).
car(car_14).
car(car_21).
car(car_22).
car(car_23).
car(car_31).
car(car_32).
car(car_33).
car(car_41).
car(car_42).
car(car_43).
car(car_44).
car(car_51).
car(car_52).
car(car_53).
car(car_61).
car(car_62).
car(car_71).
car(car_72).
car(car_73).
car(car_81).
car(car_82).
car(car_91).
car(car_92).
car(car_93).
car(car_94).
car(car_101).
car(car_102).

shape(elipse).
shape(hexagon).
shape(rectangle).
shape(u_shaped).
shape(triangle).
shape(circle).
shape(nil).

train(east1).
train(east2).
train(east3).
train(east4).
train(east5).
train(west6).
train(west7).
train(west8).
train(west9).
train(west10).

closed(car_12).     % 1
closed(car_23).
closed(car_32).
closed(car_33).
closed(car_43).
closed(car_52).
closed(car_53).
closed(car_61).
closed(car_81).
double(car_42).
double(car_51).
double(car_71).
has_car(east1,car_11). % 11,12
has_car(east1,car_12).
has_car(east1,car_13).
has_car(east1,car_14).
has_car(east2,car_21).
has_car(east2,car_22).
has_car(east2,car_23).
has_car(east3,car_31).
has_car(east3,car_32).
has_car(east3,car_33).
has_car(east4,car_41).
has_car(east4,car_42).
has_car(east4,car_43).
has_car(east4,car_44).
has_car(east5,car_51).
has_car(east5,car_52).
has_car(east5,car_53).
has_car(west10,car_101).
has_car(west10,car_102).
has_car(west6,car_61).
has_car(west6,car_62).
has_car(west7,car_71).
has_car(west7,car_72).
has_car(west7,car_73).
has_car(west8,car_81).
has_car(west8,car_82).
has_car(west9,car_91).
has_car(west9,car_92).
has_car(west9,car_93).
has_car(west9,car_94).
jagged(car_73).
jagged(car_92).
load(car_101,rectangle,1).
load(car_102,rectangle,2).
load(car_11,rectangle,3). % 6,7,8
load(car_12,triangle,1).
load(car_13,hexagon,1).
load(car_14,circle,1).
load(car_21,triangle,1).
load(car_22,rectangle,1).
load(car_23,circle,2).
load(car_31,circle,1).
load(car_32,triangle,1).
load(car_33,triangle,1).
load(car_41,triangle,1).
load(car_42,triangle,1).
load(car_43,rectangle,1).
load(car_44,rectangle,1).
load(car_51,triangle,1).
load(car_52,rectangle,1).
load(car_53,circle,1).
load(car_61,circle,3).
load(car_62,triangle,1).
load(car_71,circle,1).
load(car_72,triangle,1).
load(car_73,nil,0).
load(car_81,rectangle,1).
load(car_82,circle,1).
load(car_91,circle,1).
load(car_92,rectangle,1).
load(car_93,circle,1).
load(car_93,rectangle,1).
long(car_102).
long(car_11).       % 2
long(car_13).
long(car_33).
long(car_61).
long(car_73).
long(car_81).
long(car_92).
open_car(car_101).
open_car(car_102).
open_car(car_11).       % 3
open_car(car_13).
open_car(car_14).
open_car(car_21).
open_car(car_22).
open_car(car_31).
open_car(car_41).
open_car(car_42).
open_car(car_44).
open_car(car_51).
open_car(car_62).
open_car(car_71).
open_car(car_72).
open_car(car_82).
open_car(car_91).
open_car(car_93).
open_car(car_94).
shape(car_101,u_shaped).
shape(car_102,rectangle).
shape(car_11,rectangle). % 4,5
shape(car_12,rectangle).
shape(car_13,rectangle).
shape(car_14,rectangle).
shape(car_21,u_shaped).
shape(car_22,u_shaped).
shape(car_23,rectangle).
shape(car_31,rectangle).
shape(car_32,hexagon).
shape(car_33,rectangle).
shape(car_41,u_shaped).
shape(car_42,rectangle).
shape(car_43,elipse).
shape(car_44,rectangle).
shape(car_51,rectangle).
shape(car_52,rectangle).
shape(car_53,rectangle).
shape(car_61,rectangle).
shape(car_62,rectangle).
shape(car_71,rectangle).
shape(car_72,u_shaped).
shape(car_73,rectangle).
shape(car_81,rectangle).
shape(car_82,u_shaped).
shape(car_91,u_shaped).
shape(car_92,rectangle).
shape(car_93,rectangle).
shape(car_94,u_shaped).
short(car_101).
short(car_12).      % 0
short(car_14).
short(car_21).
short(car_22).
short(car_23).
short(car_31).
short(car_32).
short(car_41).
short(car_42).
short(car_43).
short(car_44).
short(car_51).
short(car_52).
short(car_53).
short(car_62).
short(car_71).
short(car_72).
short(car_82).
short(car_91).
short(car_93).
short(car_94).
wheels(car_101,2).
wheels(car_102,2).
wheels(car_11,2).     % 9,10
wheels(car_12,2).
wheels(car_13,3).
wheels(car_14,2).
wheels(car_21,2).
wheels(car_22,2).
wheels(car_23,2).
wheels(car_31,2).
wheels(car_32,2).
wheels(car_33,3).
wheels(car_41,2).
wheels(car_42,2).
wheels(car_43,2).
wheels(car_44,2).
wheels(car_51,2).
wheels(car_52,3).
wheels(car_53,2).
wheels(car_61,2).
wheels(car_62,2).
wheels(car_71,2).
wheels(car_72,2).
wheels(car_73,2).
wheels(car_81,3).
wheels(car_82,2).
wheels(car_91,2).
wheels(car_92,2).
wheels(car_93,2).
wheels(car_94,2).

:- end_object.
