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


:- category(best_first,
	implements(interpreterp)).

	:- info([
		version is 1:1:0,
		author is 'Victor Lagerkvist',
		date is 2019-03-08,
		comment is 'Best-first framework for general logic programs.'
	]).

	:- protected(f/4).
	:- mode(f(+float, +float, +float, -float), zero_or_more).
	:- info(f/4, [
		comment is '.',
		argnames is ['Length1', 'Length2', 'Depth', 'Cost']
	]).

	prove(Goal, DB) :-
		prove(Goal, -1, DB).

	prove(Goal, Limit, DB) :-
		minheap::as_heap([1 - state([Goal], 1, 0, [])], Heap),
		prove_branch(Heap, Limit, DB).

	prove_branch(Heap, _, _) :-
		minheap::top(Heap, _, state([], _, _, Bindings)),
		execute_bindings(Bindings).
	prove_branch(Heap, Limit, DB) :-
		minheap::delete(Heap, Cost, State, Heap1),
		State = state(Goals, Length, Depth, Bindings),
		0 =\= Depth - Limit,
		(   Goals = [not(G)|Gs] ->
			(   prove(G, DB) ->
				prove_branch(Heap1, Limit, DB)
			;	Length1 is Length - 1,
				Depth1 is Depth + 1,
				::f(Length, 0, Depth1, Cost1),
				counter::increment, %Inference counting.
				minheap::insert(Cost1, state(Gs, Length1, Depth1, Bindings), Heap1, Heap2),
				prove_branch(Heap2, Limit, DB)
			)
		;	expand_state(Cost, State, StateCostPairs, DB),
			minheap::insert_all(StateCostPairs, Heap1, Heap2),
			prove_branch(Heap2, Limit, DB)
		).

	expand_state(_, state([], 0, _, _), [], _) :- !.
	expand_state(_Cost0, state([Goal|Goals], Length1, Depth0, Bindings), Pairs, DB) :-
		Depth is Depth0 + 1,
		bagof(Cost - state(Body, Length, Depth, Goal),
			  Length1^Length2^(
				rule(Goal, Body, Length2, Goals, DB),
				Length is Length1 + Length2 - 1,
				counter::increment, %Inference counting.
				::f(Length1, Length2, Depth, Cost)
			  ),
			NewPairs0),
		!,
		add_bindings(NewPairs0, Goal, Bindings, Pairs).
	expand_state(_, _, [], _).

	rule(Head, Body, Length, Tail, DB) :-
		(	Head = {Goal} ->
			call(Goal),
			Body = Tail,
			Length = 0
		;	DB::rule(Head, Body, Length, Tail)
		).

	add_bindings([], _, _, []).
	add_bindings([Cost - State0|States0], Goal, Bindings, [Cost - State|States]) :-
		State0 = state(Goals, Length, Depth, Goal0),
		State = state(Goals, Length, Depth, [Goal = Goal0|Bindings]),
		add_bindings(States0, Goal, Bindings, States).

	execute_bindings([]).
	execute_bindings([X = Y|Bs]) :-
		X = Y,
		execute_bindings(Bs).

:- end_category.
