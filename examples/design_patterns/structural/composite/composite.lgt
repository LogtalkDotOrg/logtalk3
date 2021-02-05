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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Composite design pattern:
%
% https://en.wikipedia.org/wiki/Composite_pattern


% we illustrate this pattern using classes; first, we define a common
% meta-class with a simple predicate for creating new instances

:- object(meta_graphic,
	instantiates(meta_graphic)).

	:- public(new/1).

	new(Graphic) :-
		self(Self),
		create_object(Graphic, [instantiates(Self)], [], []).

:- end_object.


% the inheritance root for graphics defines a single predicate for
% printing the name of an instance (and also its class); this class
% defines the clients interface, abstracting the differences between
% whole and part objects

:- object(graphic,
	instantiates(meta_graphic)).

	:- public(print/0).

	print :-
		self(Self),
		instantiates_class(Self, Class),
		write(Class), write(': '), write(Self), nl.

:- end_object.


% for composite graphics, we need predicates to add and remove graphics;
% we also need to redefine the print predicate to print all graphics that
% are part of the composite graphic

:- object(composite_graphic,
	instantiates(meta_graphic),
	specializes(graphic)).

	:- public([add/1, remove/1]).

	:- protected(graphic_/1).
	:- dynamic(graphic_/1).

	print :-
		forall(
			::graphic_(Graphic),
			Graphic::print
		).

	add(Graphic) :-
		::assertz(graphic_(Graphic)).

	remove(Graphic) :-
		::retractall(graphic_(Graphic)).

:- end_object.


% an example of a simple graphic is an ellipse

:- object(ellipse,
	instantiates(meta_graphic),
	specializes(graphic)).

:- end_object.
