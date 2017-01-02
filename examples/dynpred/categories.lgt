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


:- category(ctg).					% categories are fine-grained units of code reuse
									% that can be imported by any number of objects
	:- public([
		get_default/1, set_default/1,
		get_value/1, set_value/1
	]).

	:- private(state/1).			% categories can declare and handle dynamic
	:- dynamic(state/1).			% predicates but cannot contain clauses for them

	get_default(State) :-
		state(State).				% called in the context of "this"

	set_default(State) :-
		retractall(state(_)),		% retracts clauses in "this"
		assertz(state(State)).		% asserts clause in "this"

	get_value(State) :-
		::state(State).				% called in the context of "self"

	set_value(State) :-
		::retractall(state(_)),		% retracts clauses in "self"
		::assertz(state(State)).	% asserts clause in "self"

:- end_category.


:- object(top,						% category predicates are inherited
	imports(ctg)).					% by the descendants of the object
									% importing the category
:- end_object.


:- object(middle,
	extends(top)).

:- end_object.


:- object(bottom,
	extends(middle)).

:- end_object.
