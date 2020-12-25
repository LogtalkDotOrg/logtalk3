%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Decorator design pattern:
%
% https://en.wikipedia.org/wiki/Decorator_pattern


:- object(shape).

	:- public(string/0).
	% default definition; do nothing
	string.

:- end_object.


:- object(circle,
	extends(shape)).

	:- public(radius/1).
	% default value
	radius(10.0).

	:- public(diameter/1).
	diameter(Diameter) :-
		::radius(Radius),
		Diameter is Radius * 2.

	string :-
		::radius(Radius),
		write('A circle of radius '), write(Radius), nl.

:- end_object.


% define the decorator as a parametric object using one parameter
% to pass the decorated object and a second parameter to pass the
% decoration

:- object(colored_shape(_Shape_, _Color_),
	implements(forwarding),
	extends(shape)).

	% define the string/0 predicate to print both the
	% decorated object data and the decoration
	string :-
		_Shape_::string,
		write('which is colored '), write(_Color_), nl.

	% forward unknown messages to the decorated object
	forward(Message) :-
		[_Shape_::Message].

:- end_object.


% we can define multiple decorators for the same type of objects;
% here we use an alternative solution with two dynamic predicates
% to store the decorated object and the decoration

:- object(named_shape,
	implements(forwarding),
	extends(shape)).

	:- public([shape/1, name/1]).
	:- dynamic([shape/1, name/1]).

	% define the string/0 predicate to print both the
	% decorated object data and the decoration
	string :-
		::shape(Shape),
		Shape::string,
		::name(Name),
		write('which is named '), write(Name), nl.

	% forward unknown messages to the decorated object
	forward(Message) :-
		::shape(Shape),
		[Shape::Message].

:- end_object.


% we can also fully define a decorator in a source file

:- object(my_named_shape,
	extends(named_shape)).

	shape(colored_shape(circle, green)).

	name('Mr. Round').

:- end_object.
