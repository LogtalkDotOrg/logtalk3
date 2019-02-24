%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


% a protocol contains a functionality cohesive set of predicate declarations
% and can be implemented by any number of objects and categories; in this
% example, we start by defining a protocol declaring predicates for common
% physical properties:

:- protocol(physical_properties).

	:- public([
		mass/1, volume/1
	]).

:- end_protocol.


% next, we define two objects, m1 and m2, implementing the protocol:

:- object(m1,
	implements(physical_properties)).

	mass(3).
	volume(2.17).

:- end_object.


:- object(m2,
	implements(physical_properties)).

	mass(4).
	volume(9.21).

:- end_object.


% a category is a fine grained unit of code reuse, used to encapsulate a
% cohesive set of predicate declarations and definitions, implementing a
% single functionality, that can be imported into any object

% categories can be interpreted as the dual concept of protocols, with
% both aiming for functional coheasion with the difference being that
% categories can both declare and define predicates

% in this example, we define a category for declaring planetary properties
% and declaring and defining planetary computations; we don't want to use
% an object as the gravitational_acceleration/1 predicate can only be defined
% for a concrete planet

:- category(planet).

	:- public([
		gravitational_acceleration/1,
		weight/2
	]).

	weight(Object, Weight) :-
		Object::mass(Mass),
		::gravitational_acceleration(Acceleration),
		Weight is Mass * Acceleration.

:- end_category.


% this category can be imported by any number of objects representing actual
% planets, for example, Earth and Mars:

:- object(earth,
	imports(planet)).

	gravitational_acceleration(9.80665).

:- end_object.


:- object(mars,
	imports(planet)).

	gravitational_acceleration(3.72076).

:- end_object.

