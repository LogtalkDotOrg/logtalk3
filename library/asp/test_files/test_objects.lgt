%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(asp_reachability,
	implements(asp_problem_protocol)).

	edge(a, b).
	edge(b, c).
	reach(Start, End) :- edge(Start, End).
	reach(Start, End) :- edge(Start, Middle), reach(Middle, End).
	:- show(reach/2).

:- end_object.


:- object(asp_variable_fixture,
	implements(asp_problem_protocol)).

	p(Value, Value, _, _, _Named, _Named).
	rule_as_data :- not(halt).
	:- const(n, 2 + 3).
	:- constraint((p(Value, Value, _, _, _Named, _Named), Value > 4)).

:- end_object.


:- object(asp_raw_fixture,
	implements(asp_problem_protocol)).

	:- raw('#program base.').
	:- raw([35,112,114,111,103,114,97,109,32,98,97,115,101,46]).
	:- raw(['#',p,r,o,g,r,a,m,' ',b,a,s,e,'.']).
	:- raw("#program base.").

:- end_object.


:- object(asp_unrelated_fixture).

	:- public(value/1).
	value(ordinary).

:- end_object.


:- object(asp_team_selection,
	implements(asp_problem_protocol)).

	:- raw("#program base.").
	:- const(size, 2).
	:- const(hours, 7).
	:- const(budget, 2**3).

	person(alice).
	person(bob).
	person(clara).
	skill(alice, design).
	skill(bob, code).
	skill(clara, design).
	skill(clara, code).
	cost(alice, 2).
	cost(bob, 2).
	cost(clara, 5).
	merit(alice, 4).
	merit(bob, 3).
	merit(clara, 8).
	slot(interval(1, size)).

	choice(size, [conditional(selected(Person), person(Person))], size).
	selected(Person) :-
		person(Person),
		not(not(selected(Person))).
	choice(none, [conditional(reserve(Person), person(Person))], none).
	:- constraint((selected(Person), reserve(Person))).
	:- constraint((person(Person), not(selected(Person)), not(reserve(Person)))).
	neg(selected(Person)) :-
		reserve(Person).
	disjunction([onsite, remote]).
	:- constraint((onsite, remote)).

	qualified :-
		conditional(skill(Person, _), selected(Person)),
		aggregate(count, [aggregate_element([Person], selected(Person))]) >= size.
	:- constraint(not(qualified)).
	:- constraint(not(compare('=', aggregate(count, [aggregate_element([Person], selected(Person))]), size))).
	:- constraint(compare('!=', aggregate(count, [aggregate_element([Person], reserve(Person))]), 1)).
	:- constraint((aggregate(count, [aggregate_element([Person], selected(Person))]) =< 1)).
	:- constraint((selected(Person), cost(Person, Cost), Cost < 0)).
	:- constraint((selected(Person), cost(Person, Cost), Cost >= budget)).
	total(Sum) :-
		Sum = aggregate(sum, [aggregate_element([Cost,Person], (selected(Person), cost(Person, Cost)))]).
	:- constraint((total(Sum), Sum > budget)).

	summary(Sum, PositiveSum, Low, High, hours/size, arithmetic('\\', [hours,size]), abs(hours-size*(hours/size)), arithmetic('-', [1]), budget, size+1, size-1, size*hours) :-
		total(Sum),
		PositiveSum = aggregate(sum_plus, [aggregate_element([Cost,Person], (selected(Person), cost(Person, Cost)))]),
		Low = aggregate(min, [aggregate_element([Cost,Person], (selected(Person), cost(Person, Cost)))]),
		High = aggregate(max, [aggregate_element([Cost,Person], (selected(Person), cost(Person, Cost)))]).
	label(string([84,101,97,109,32,34,65,34,92,66,10,82,101,97,100,121])).
	limits(infimum, supremum).

	:- minimize([objective(Cost, 3, [Person], (selected(Person), cost(Person, Cost)))]).
	:- maximize([objective(Merit, 2, [Person], (selected(Person), merit(Person, Merit)))]).
	:- weak_constraint(remote, 1, 1, []).

	:- show(none).
	:- show(selected/1).
	:- show(signature(selected, 1, negative)).
	:- show(onsite/0).
	:- show(summary/12).
	:- show(label/1).
	:- show(limits/2).
	:- show(tuple([]), qualified).
	:- show(tuple([Person]), selected(Person)).
	:- show(tuple([Person,Slot]), (selected(Person), slot(Slot))).
	:- show(report(team(string([65,108,112,104,97])), tuple([])), qualified).

:- end_object.
