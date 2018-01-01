%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


% categories are fine-grained units of code reuse
% that can be imported by any number of objects

:- category(ctg).

	:- public([
		get_default/1, set_default/1,
		get_value/1, set_value/1
	]).

	% categories can declare and handle dynamic
	% predicates but cannot contain clauses for them
	:- private(state/1).
	:- dynamic(state/1).

	get_default(State) :-
		% call state/1 in the context of "this"
		state(State).

	set_default(State) :-
		% retract clause in "this"
		retractall(state(_)),
		% assert clause in "this"
		assertz(state(State)).

	get_value(State) :-
		% call state/1 in the context of "self"
		::state(State).

	set_value(State) :-
		% retract all clauses in "self"
		::retractall(state(_)),
		% assert clause in "self"
		::assertz(state(State)).

:- end_category.


% category predicates are inherited by the descendants
% of the objects importing the category

:- object(top,
	imports(ctg)).

:- end_object.


:- object(middle,
	extends(top)).

:- end_object.


:- object(bottom,
	extends(middle)).

:- end_object.
