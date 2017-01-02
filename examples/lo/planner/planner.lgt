%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


% Planner

:- object(plan(_)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Air-line trip planner.',
		parnames is ['Mode'],
		source is 'Example adapted from the Francis G. McCabe L&O documentation.'
	]).

	:- public(from/3).
	:- mode(from(+atom, +atom, -list), zero_or_more).
	:- info(from/3, [
		comment is 'Plan a trip from Start to Destination.',
		argnames is ['Start', 'Destination', 'Plan']
	]).

	from(Start, Destination, Plan) :-
		from(Start, Destination, [], Plan).

	from(Start, Destination, _, [Step]) :-
		parameter(1, Mode),
		Mode::step(Start, Destination, Step),
		!.
	from(Start, Destination, Locations, [Step| Steps]) :-
		parameter(1, Mode),
		Mode::step(Start, City2, Step),
		not_member(City2, Locations),
		from(City2, Destination, [Start| Locations], Steps).

	not_member(_, []).
	not_member(City, [Location| Locations]) :-
		City \= Location,
		not_member(City, Locations).

:- end_object.



% Abstractions of City, Airport, and Flight

:- object(city).

	:- public(step/3).
	:- mode(step(+city, +city, -path), zero_or_more).

	:- public(airport/1).
	:- mode(airport(?atom), zero_or_more).

	step(X, Y, P1-P-P2) :-
		\+ same_city(X, Y), !,
		X::airport(XA),
		Y::airport(YA),
		plan(fly)::from(XA, YA, P),
		plan(city)::from(X, XA, P1),
		plan(city)::from(YA, Y, P2).

	step(X, Y, taxi(X, Y)) :-
		same_city(X, Y),
		X \= Y.

	same_city(X, Y) :-
		X::airport(A),
		Y::airport(A).

:- end_object.


:- object(airport).

	:- public(fly/1).
	:- mode(fly(?city), zero_or_more).

	:- public(airport/1).
	:- mode(airport(?city), zero_or_more).

	airport(Airport) :-
		self(Airport).

:- end_object.


:- object(fly).

	:- public(step/3).
	:- mode(step(+city, +city, -path), zero_or_more).

	step(From, To, fly(From, To)) :-
		From::fly(To).

:- end_object.



% Edinburgh locations

:- object(edinburgh,
	extends(city)).

	airport(edin).

:- end_object.


:- object(edin,
	extends(edinburgh)).

:- end_object.


:- object(castle,
	extends(edinburgh)).

:- end_object.


:- object(aiai,
	extends(edinburgh)).

:- end_object.



% Glasgow locations

:- object(glasgow,
	extends(city)).

	airport(renfrew).

:- end_object.



% London locations

:- object(london,
	extends(city)).

	airport(lhr).

:- end_object.


:- object(albert_hall,
	extends(london)).

:- end_object.


:- object(imperial,
	extends(london)).

:- end_object.



% Manchester locations

:- object(manchester,
	extends(city)).

	airport(ringway).

:- end_object.


:- object(victoria,
	extends(manchester)).

:- end_object.



% Airports

:- object(aberdeen_air,
	extends(airport)).

	fly(renfrew).

:- end_object.


:- object(lhr,
	extends(airport)).

	fly(edin).
	fly(ringway).

:- end_object.


:- object(renfrew,
	extends(airport)).

	fly(aberdeen_air).
	fly(ringway).

:- end_object.


:- object(ringway,
	extends((manchester, airport))).

	fly(lhr).
	fly(renfrew).

:- end_object.
