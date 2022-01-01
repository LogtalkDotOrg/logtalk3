%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
% page on the Bridge design pattern:
%
% https://en.wikipedia.org/wiki/Bridge_pattern


% decouple shape drawing from shape definition so that both can
% vary independently; for example, we may want to be able to draw
% a shape in different mediums (printer, screen, plotter, ...)


% we start by defining shape drawing using prototypes implementing
% a shape drawing protocol

:- protocol(drawing_api_protocol).

	:- public(draw_circle/3).

:- end_protocol.


:- object(drawing_api_1,
	implements(drawing_api_protocol)).

	draw_circle(X, Y, Radius) :-
		write('API1: circle at '), write(X:Y),
		write(' with radius '), write(Radius), nl.

:- end_object.


:- object(drawing_api_2,
	implements(drawing_api_protocol)).

	draw_circle(X, Y, Radius) :-
		write('API2: circle at '), write(X:Y),
		write(' with radius '), write(Radius), nl.

:- end_object.


% define shapes as classes using dynamic predicates to represent
% shape state

:- object(meta_shape,
	instantiates(meta_shape)).

	:- public(new/2).
	% a simple instance creation predicate with no attempt to
	% validate the list specifying the instances initial state
	new(Shape, State) :-
		self(Self),
		create_object(Shape, [instantiates(Self)], [], State).

:- end_object.


:- object(shape,
	instantiates(meta_shape)).

	% the draw/0 predicate abstracts how a shape is draw by
	% delegating the operation to a separate implementation
	:- public([
		draw/0, resize/1
	]).

	% allow deferring to runtime selecting the drawing API
	% to be used for a concrete shape
	:- protected(drawing_api/1).
	:- dynamic(drawing_api/1).

:- end_object.


:- object(circle,
	instantiates(meta_shape),
	specializes(shape)).

	:- protected([x/1, y/1, radius/1]).
	:- dynamic([x/1, y/1, radius/1]).

	% retrieve the instance state and delegate the drawing
	% operation to the configured implementer object
	draw :-
		::drawing_api(DrawingAPI),
		::x(X),
		::y(Y),
		::radius(Radius),
		DrawingAPI::draw_circle(X, Y, Radius).

	resize(Percent) :-
		::retract(radius(Radius)),
		NewRadius is Radius * (1 + Percent / 100),
		::assertz(radius(NewRadius)).

:- end_object.


% define a static instance of circle

:- object(a_circle,
	instantiates(circle)).

	drawing_api(drawing_api_2).
	x(1.7).
	y(11.3).
	radius(2.1).

:- end_object.
