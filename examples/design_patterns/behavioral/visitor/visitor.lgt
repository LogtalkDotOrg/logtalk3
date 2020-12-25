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
% page on the Visitor design pattern:
%
% https://en.wikipedia.org/wiki/Visitor_pattern


:- category(car).

	:- public(component/1).

	% simple visitor that applies a closure to
	% each car component
	:- public(visitor/1).
	:- meta_predicate(visitor(1)).
	visitor(Closure) :-
		forall(
			::component(Component),
			call(Closure, Component)
		).

:- end_category.


:- object(sedan,
	imports(car)).

	% ensure that this object can be hot-patched (but restricted
	% to complementing categories that only add new predicates)
	:- set_logtalk_flag(complements, restrict).

	component(engine(diesel)).

	component(wheel(front_left)).
	component(wheel(front_right)).
	component(wheel(rear_right)).
	component(wheel(rear_left)).

	component(wheel(left_door)).
	component(wheel(right_door)).

	component(body(station_wagon)).

:- end_object.


% pretend that the "sedan" object doesn't provide its own visitor
% meta-predicate

:- category(add_visitor,
	complements(sedan)).

	:- public(alt_visitor/1).
	:- meta_predicate(alt_visitor(1)).
	alt_visitor(Closure) :-
		forall(
			::component(Component),
			call(Closure, Component)
		).

:- end_category.
