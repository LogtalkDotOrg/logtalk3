%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(process).

    :- public(domain/2).
	:- mode(domain(-list(object_identifier), -callable), one).
	:- info(domain/2, [
		comment is 'Returns the process dependencies and its domain.',
		argnames is ['Dependencies', 'Domain']]).

:- end_protocol.


:- object(a(_A_),
    implements(process)).

	:- uses(user, [(#>=)/2, (#=<)/2]).

	% process "a" have no dependencies and can
	% be executed two, three, or four times
	domain([], (_A_ #>= 2, _A_ #=< 4)).

:- end_object.


:- object(b(_B_),
    implements(process)).

	:- uses(user, [(#>=)/2, (#=<)/2]).

	% process "a" depends on process "a" and must
	% be executed at least the same number of times
	% as process "a" but at most three times
	domain([a(A)], (_B_ #>= A, _B_ #=< 3)).

:- end_object.


:- object(c(_C_),
    implements(process)).

	:- uses(user, [(#>=)/2, (#=<)/2]).

	% process "c" depends on processes "a" and "b"
	% and must executed one more time than process
	% "a" and  one more time than process "b"
    domain([a(A), b(B)], (_C_ #= B + 1, _C_ #= A + 1)).

:- end_object.


:- object(process_model).

    :- public(solve/2).
	:- mode(solve(+list(object_identifier), -list(object_identifier)), zero_or_more).
	:- info(solve/2, [
		comment is '.',
		argnames is ['Processes', 'Dependencies']]).

	solve(Processes, Dependencies) :-
		dependencies(Processes, Dependencies),
		constraints(Dependencies, Dependencies),
		term_variables(Dependencies, Variables),
		% experiment with commenting out the next goal
		{fd_labeling(Variables)}.

    constraints([], _).
    constraints([Process| Processes], Dependencies) :-
        Process::domain(Deps, Constraints),
		% unify the variables shared between the two domain/2 arguments
        unify(Deps, Dependencies),
		% establish the constraints
        {call(Constraints)},
        constraints(Processes, Dependencies).

	unify([], _).
	unify([Process| Processes], Dependencies) :-
		list::memberchk(Process, Dependencies),
		unify(Processes, Dependencies).

    :- public(dependencies/2).
	:- mode(dependencies(+list, -list), one).
	:- info(dependencies/2, [
		comment is 'Returns the processes dependencies.',
		argnames is ['Processes', 'Dependencies']]).

    dependencies(Processes, Dependencies) :-
	    dependencies(Processes, Processes, Dependencies).

	% this is just a quick hack for testing a simple example; it
	% needs a graph algorithm that returns all processes reachable
	% from a list of processes when following the dependency links
    dependencies([], Dependencies, Dependencies).
    dependencies([Process| Processes], Acc, Dependencies) :-
        Process::domain(Others, _),
        add(Others, Acc, Acc2),
        dependencies(Processes, Acc2, Dependencies).

	add([], All, All).
	add([Process| Processes], Acc, All) :-
		(	list::member(Process, Acc) ->
			Acc2 = Acc
		;	Acc2 = [Process| Acc]
		),
		add(Processes, Acc2, All).

:- end_object.
