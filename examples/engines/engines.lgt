%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
This is a simple example of category composition, i.e. extending of 
categories by other categories in order to provide modified components 
for building objects, using car engines.

The example defines a car engine protocol, "enginep", a standard engine 
(classic), and an improved version of it, "sport". Both engines are then 
imported in two car models, "sedan" and "coupe".
*/


% first we define a protocol for describing the characteristics of an engine:

:- protocol(enginep).

	:- public(reference/1).
	:- public(capacity/1).
	:- public(cylinders/1).
	:- public(horsepower_rpm/2).
	:- public(bore_stroke/2).
	:- public(fuel/1).

:- end_protocol.


% second, we can define a typical engine as a category, which will be used 
% when "assembling" cars:

:- category(classic,
	implements(enginep)).

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
