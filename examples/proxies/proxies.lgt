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


% we use a parametric object in order to give an interpretation to an 
% object proxy arguments and to encapsulate relevant predicates:

:- object(circle(_Id, _Radius, _Color)).

	:- public([
		id/1, radius/1, color/1,
		area/1, perimeter/1,
		print/0
	]).

	id(Id) :-
		parameter(1, Id).

	radius(Radius) :-
		parameter(2, Radius).

	color(Color) :-
		parameter(3, Color).

	area(Area) :-
		parameter(2, Radius),
		Area is 3.14159265358979*Radius*Radius.

	perimeter(Perimeter) :-
		parameter(2, Radius),
		Perimeter is 2*3.14159265358979*Radius.

	print :-
		id(Id), write('id: '), write(Id),
		area(Area), write(', area: '), write(Area),
		perimeter(Perimeter), write(', perimeter: '), write(Perimeter),
		color(Color), write(', color: '), write(Color), nl.

:- end_object.


% parametric object proxies:

circle('#1', 1.23, blue).
circle('#2', 3.71, yellow).
circle('#3', 0.39, green).
circle('#4', 5.74, black).
circle('#5', 8.32, cyan).
