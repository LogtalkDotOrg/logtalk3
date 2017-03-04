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


% define a category that adds new functionality to the "employee" object:

:- category(add_on,
	% built-in protocol for the event handler methods
	implements(monitoring),
	% add the category predicates to the employee object
	complements(employee)).

	% define a "before" event handler for the complemented object:
	before(This, Message, Sender) :-
		this(This),
		write('Received message '), writeq(Message), write(' from '), writeq(Sender), nl.

	% add a new method to the complemented object:
	:- public(predicates/1).

	predicates(Predicates) :-
		setof(Predicate, ::current_predicate(Predicate), Predicates). 

	% define an alias for a predicate of the complemented object:
	:- alias(employee, [salary/1 as income/1]).

:- end_category.


% setup the object "employee" as a monitor for any message sent to itself:

:- initialization((
	define_events(before, employee, _, _, employee),
	set_logtalk_flag(events, allow)
)).
