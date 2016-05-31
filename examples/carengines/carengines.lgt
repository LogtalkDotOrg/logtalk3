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
This is a simple example of category composition, i.e. extending of 
categories by other categories in order to provide modified components 
for building objects, using car engines.

The example defines a car engine protocol, "carcarenginep", a standard engine 
(classic), and an improved version of it, "sport". Both engines are then 
imported in two car models, "sedan" and "coupe".
*/


% first we define a protocol for describing the characteristics of an engine:

:- protocol(carenginep).

	:- public([
		reference/1,
		capacity/1,
		cylinders/1,
		horsepower_rpm/2,
		bore_stroke/2,
		fuel/1
	]).

:- end_protocol.


% second, we can define a typical engine as a category, which will be used 
% when "assembling" cars:

:- category(classic,
	implements(carenginep)).

	reference('M180.940').
	capacity(2195).
	cylinders(6).
	horsepower_rpm(94, 4800).
	bore_stroke(80, 72.8).
	fuel(gasoline).

:- end_category.


% next, we define a souped up version of the previous engine, which differs 
% from the standard one only in its reference and in its horsepower:

:- category(sport,
	extends(classic)).

	reference('M180.941').
	horsepower_rpm(HP, RPM) :-
		^^horsepower_rpm(ClassicHP, ClassicRPM),
		HP is truncate(ClassicHP*1.23),
		RPM is truncate(ClassicRPM*0.762).

:- end_category.


% with engines (and other components), we may start "assembling" some cars:

:- object(sedan,
	imports(classic)).

:- end_object.


:- object(coupe,
	imports(sport)).

:- end_object.
