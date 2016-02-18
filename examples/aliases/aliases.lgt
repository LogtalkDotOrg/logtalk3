%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


/*
This example illustrates the use of the predicate directive alias/2 for 
defining alternative names for inherited predicates.
*/


% first, we define a simple parametric object for representing rectangles:

:- object(rectangle(_Width, _Height)).

	:- public([
		width/1, height/1, area/1
	]).

	width(Width) :-
		parameter(1, Width).

	height(Height) :-
		parameter(2, Height).

	area(Area) :-
		::width(Width),
		::height(Height),
		Area is Width*Height.

:- end_object.


% next, we define a square object which adds an alias, side/1, for the 
% inherited predicate width/1:

:- object(square(Side),
	extends(rectangle(Side, Side))).

	:- alias(rectangle(_, _), [width/1 as side/1]).

:- end_object.


% we can also define a 1x1 square:

:- object(square1,
	extends(square(1))).

:- end_object.


% a similar example can be defined using ellipses and circles:

:- object(ellipse(_RX, _RY)).

	:- public([
		rx/1, ry/1, area/1
	]).

	rx(Rx) :-
		parameter(1, Rx).

	ry(Ry) :-
		parameter(2, Ry).

	area(Area) :-
		::rx(Rx),
		::ry(Ry),
		Area is Rx*Ry*3.1415927.

:- end_object.


% in this case, we define an alias named r/1 for the inherited 
% predicate rx/1:

:- object(circle(Radius),
	extends(ellipse(Radius, Radius))).

	:- alias(ellipse(_, _), [rx/1 as r/1]).

:- end_object.
