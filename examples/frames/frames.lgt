%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- category(frame,
	implements(forwarding)).

	:- public([
		slot/1, facet/2,
		intension/1, extension/1,
		part_of/1,
		subset/1
	]).

	slot(Slot) :-
		::current_predicate(Slot).
	slot(Slot) :-
		::intension(Intension),
		Intension::slot(Slot).
	slot(Slot) :-
		::part_of(Assembly),
		Assembly::slot(Slot).

	forward(Message) :-
		::intension(Intension),
		[Intension::Message].

	forward(Message) :-
		::part_of(Assembly),
		[Assembly::Message].

:- end_category.


% physical_object frame:

:- object(physical_object,
	imports(frame)).

	% slots
	:- public([
		weight/1, name/1, use/1
	]).

	% facets
	facet(weight, kilograms).

:- end_object.


% vehicle frame:

:- object(vehicle,
	extends(physical_object)).

	:- public([
		owner/1, dealers/1, year/1, age/1, propulsion_method/1
	]).

	use(transportation).
	has_a_part(propulsion_system).

	facet(age, years).
	facet(year, years).

:- end_object.


% car frame:

:- object(car,
	extends(vehicle)).

	:- public([
		make/1, model/1
	]).

	propulsion_method(internal_combustion_engine).
	has_a_part(electrical_system).
	extension(cars_on_road).

possible_values(car,make,
  [gm,ford,chrysler,amc,vw,toyota,nissan,bmw]).

:- end_object.


% electrical-system frame:

:- object(electrical_system,
	imports(frame)).

	part_of(car).

	has_a_part(battery).
	has_a_part(starter).

:- end_object.


% VW-Rabbit frame:

:- object(vw_rabbit,
	extends(car)).

	make(vw).
	model(rabbit).

:- end_object.


% cars-on-the-road-now frame:

:- object(cars_on_road,
	imports(frame)).

	intension(car).

	statistic(mean,age,6.4).

:- end_object.


% Joe's-VW-Rabbit frame:

:- object(joes_rabbit,
	extends(vw_rabbit)).

	extension(joes_rabbit_now).
	owner(joe).
	year(1976).

:- end_object.


% Joe's-VW-Rabbit-now frame:

:- object(joes_rabbit_now,
	imports(frame)).

	subset(cars_on_road).
	intension(joes_rabbit).

	statistic(size,none,1).

:- end_object.


% Joe's-VW-Rabbit-battery frame:

:- object(joes_rabbits_battery,
	extends(physical_object)).

	extension(joes_rabbits_battery_now).
	part_of(joes_rabbit).

:- end_object.


% Joe's-VW-Rabbit-battery-now frame:

:- object(joes_rabbits_battery_now,
	extends(physical_object)).

	intension(joes_rabbits_battery).
	contained_in(joes_rabbit_now).
	status(dead).

:- end_object.
