%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright (c) 2010, Victor Lagerkvist
%  SPDX-License-Identifier: BSD-3-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(bfs_interpreter,
	implements(interpreterp)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'Breadth-first interpreter for general logic programs.'
	]).

	prove(Goal, DB) :-
		prove(Goal, -1, DB).

	prove(Goal, Limit, DB) :-
		State = state([Goal], 0, []),
		queue::jump(State, Q0 - Q0, Q),
		prove_branch(Q, Limit, DB).

	prove_branch(Q, _, _) :-
		queue::head(Q, State),
		State = state([], _, Bindings), %Goal state.
		execute_bindings(Bindings).
	prove_branch(Q, Limit, DB) :-
		queue::serve(Q, State, Q1),
		State = state(Goals, Depth, Bindings),
		0 =\= Depth - Limit,
		(	Goals = [not(G)|Gs] ->
			(	prove(G, DB) ->
				prove_branch(Q1, Limit, DB) %The whole branch failed. Move on!
			;	Depth1 is Depth + 1,
				State1 = state(Gs, Depth1, Bindings),
				queue::join(State1, Q1, Q2),
				counter::increment, %Inference counting.
				prove_branch(Q2, Limit, DB) %and continue with the rest of the branches.
			)
		;	expand_state(State, NewGoals, DB),
			queue::join_all(NewGoals, Q1, Q2),
			prove_branch(Q2, Limit, DB)
		).

	expand_state(state([], _, _), [], _) :- !.
	expand_state(state([Goal|Goals], Depth0, Bindings), NewGoals, DB) :-
		Depth is Depth0 + 1,
		%%Find all bodies which unifies with Goal. Since rules are
		%%represented as difference lists it is easy to append the
		%%new body with Goals. Goal in the template is a placeholder,
		%%and is later used in add_bindings/5 to create a unifier
		%%between the old goal and the resolvent.
		bagof(state(Body, Depth, Goal),
			  (
			   rule(Goal, Body, Goals, DB),
			   counter::increment %Inference counting.
			  ),
			NewGoals0),
		!,
		add_bindings(NewGoals0, Goal, Bindings, NewGoals, []).
	expand_state(_, [], _).

	add_bindings([], _, _, Tail, Tail).
	add_bindings([State0|States0], Goal, Bindings, [State|States], Tail) :-
		State0 = state(Goals, Depth, Goal0),
		State = state(Goals, Depth, [Goal = Goal0|Bindings]),
		add_bindings(States0, Goal, Bindings, States, Tail).

	execute_bindings([]).
	execute_bindings([X = Y|Bs]) :-
		X = Y,
		execute_bindings(Bs).

	rule(Head, Body, Tail, DB) :-
		(	Head = {Goal} ->
			call(Goal),
			Body = Tail
		;	DB::rule(Head, Body, Tail)
		).

:- end_object.
