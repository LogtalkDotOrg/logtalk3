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


:- object(tabu_search(_Problem_, _RandomAlgorithm_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Tabu search optimization algorithm. Parameterized by a problem object implementing the ``tabu_search_protocol`` protocol and by a random number generator algorithm for the ``fast_random`` library. The algorithm minimizes the energy (cost) function defined by the problem. Custom stop conditions, delta-energy neighbor generation, full neighborhood enumeration, progress reporting, and restarts can be defined by the problem object or configured via options; suitable defaults are used otherwise.',
		parameters is [
			'Problem' - 'Problem object implementing ``tabu_search_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library (e.g. ``xoshiro128pp``, ``xoshiro256ss``, ``well512a``, ...).'
		],
		remarks is [
			'Algorithm' - 'Tabu search is a metaheuristic that guides a local search procedure using a short-term memory structure (the tabu list) to avoid cycling and to escape local minima. At each iteration a set of candidate neighbors is examined and the best admissible (non-tabu or aspiration-allowed) neighbor is selected.',
			'Tabu list' - 'A list of recently visited states paired with expiration steps. With fixed tenure the list behaves as a FIFO of maximum length ``tabu_tenure``. With ``tabu_tenure_range(Min, Max)`` each accepted move is assigned a random tenure drawn uniformly from the inclusive range.',
			'Aspiration criterion' - 'A tabu candidate is accepted when its energy is strictly better than the best energy found so far. This is the classic "best-so-far" aspiration criterion.',
			'Candidate generation' - 'By default the algorithm samples ``candidates(N)`` neighbors using ``neighbor_state/2`` (or ``neighbor_state/3`` when defined). If the problem defines ``neighbors/2``, that complete list is used instead (or a random sample of it when larger than the candidate limit).',
			'Delta-energy optimization' - 'If the problem object defines ``neighbor_state/3``, the algorithm uses the returned delta energy directly instead of calling ``state_energy/2`` on the neighbor. This is useful when computing the energy change is cheaper than recomputing the full energy.',
			'Progress reporting' - 'If the problem object defines ``progress/5``, it is called periodically with the current step, best energy, current energy, acceptance rate, and improvement rate. The reporting interval is controlled by the ``updates(N)`` option. A final report is always produced when the loop terminates.',
			'Best state tracking' - 'The algorithm tracks the best state found across all iterations and across all restart cycles, not just the final state.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.',
			'Restarts' - 'The ``restarts(N)`` option runs N additional tabu search cycles after the first. Each restart begins from the best state found so far with a cleared tabu list, allowing the search to escape deep local minima. Statistics accumulate across all cycles.'
		],
		see_also is [tabu_search(_), tabu_search_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-nonvar, -number), one).
	:- info(run/2, [
		comment is 'Runs the tabu search algorithm using default options and returns the best state found and its energy.',
		argnames is ['BestState', 'BestEnergy']
	]).

	:- public(run/3).
	:- mode(run(-nonvar, -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the tabu search algorithm using the given options and returns the best state found and its energy.',
		argnames is ['BestState', 'BestEnergy', 'Options'],
		remarks is [
			'``max_steps(N)`` option' - 'Maximum number of iterations per cycle (default: ``10000``).',
			'``tabu_tenure(T)`` option' - 'Fixed tabu tenure: maximum lifetime (in steps) of each tabu entry (default: ``7``). Ignored when ``tabu_tenure_range/2`` is also present.',
			'``tabu_tenure_range(Min, Max)`` option' - 'Random tabu tenure: on each accepted move a tenure is drawn uniformly from the inclusive integer range ``Min..Max``. Overrides ``tabu_tenure/1`` when present.',
			'``candidates(N)`` option' - 'Number of candidate neighbors examined per iteration (default: ``20``).',
			'``updates(N)`` option' - 'Number of progress reports during the run. Set to ``0`` to disable. Progress is reported by calling ``progress/5`` on the problem object (default: ``0``).',
			'``seed(S)`` option' - 'Positive integer seed for the random number generator, enabling reproducible runs (default: none).',
			'``restarts(N)`` option' - 'Number of additional tabu search cycles after the first. Each restart begins from the best state found so far with a cleared tabu list (default: ``0``).'
		]
	]).

	:- public(run/4).
	:- mode(run(-nonvar, -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the tabu search algorithm using the given options, returns the best state found and its energy, and returns run statistics.',
		argnames is ['BestState', 'BestEnergy', 'Statistics', 'Options'],
		remarks is [
			'Statistics list' - 'A list of ``Key(Value)`` pairs: ``steps(N)`` is the number of steps executed, ``acceptances(A)`` is the number of accepted moves, ``improvements(I)`` is the number of moves that improved the best energy, and ``final_tabu_size(S)`` is the number of non-expired tabu entries at termination.'
		]
	]).

	:- uses(_Problem_, [
		initial_state/1, state_energy/2, stop_condition/3, progress/5, neighbor_state/2, neighbor_state/3,
		neighbors/2
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		between/3, randomize/1
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(list, [
		length/2, member/2, nth1/3, selectchk/3, append/3
	]).

	run(BestState, BestEnergy) :-
		run(BestState, BestEnergy, _Statistics, []).

	run(BestState, BestEnergy, UserOptions) :-
		run(BestState, BestEnergy, _Statistics, UserOptions).

	run(BestState, BestEnergy, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		% handle seed option
		(	^^option(seed(Seed), Options) ->
			randomize(Seed)
		;	true
		),
		initial_state(State0),
		state_energy(State0, Energy0),
		^^option(max_steps(MaxSteps), Options),
		^^option(candidates(Candidates), Options),
		^^option(updates(Updates), Options),
		^^option(restarts(Restarts), Options),
		% tenure: range overrides fixed
		(	^^option(tabu_tenure_range(Min, Max), Options) ->
			TenureSpec = range(Min, Max)
		;	^^option(tabu_tenure(Tenure), Options),
			TenureSpec = fixed(Tenure)
		),
		% compute the update interval based on total expected steps (0 means disabled)
		TotalMaxSteps is MaxSteps * (Restarts + 1),
		(	Updates > 0 ->
			UpdateInterval is max(1, (TotalMaxSteps - 1) // Updates)
		;	UpdateInterval is 0
		),
		restart_loop(
			Restarts, MaxSteps, TenureSpec, Candidates, UpdateInterval,
			State0, Energy0,
			State0, Energy0,
			[],
			0, 0, 0,
			BestState, BestEnergy,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		),
		active_tabu_size(FinalTabu, FinalStep, FinalTabuSize),
		Statistics = [
			steps(FinalStep),
			acceptances(FinalAccepts),
			improvements(FinalImproves),
			final_tabu_size(FinalTabuSize)
		].

	% restart loop
	%
	% when Restarts is 0, this is the last (or only) cycle;
	% when Restarts > 0, run a cycle, then restart from the best state found with a cleared tabu list

	restart_loop(
		0, MaxSteps, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
		Tabu, StepOffset, AccIn, ImpIn, FinalBest, FinalBestE,
		FinalStep, FinalAccepts, FinalImproves, FinalTabu
	) :-
		!,
		EndStep is StepOffset + MaxSteps,
		loop(
			StepOffset, EndStep, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
			Tabu, StepOffset, AccIn, ImpIn, FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		).
	restart_loop(
		Restarts, MaxSteps, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
		Tabu, StepOffset, AccIn, ImpIn, FinalBest, FinalBestE,
		FinalStep, FinalAccepts, FinalImproves, FinalTabu
	) :-
		EndStep is StepOffset + MaxSteps,
		loop(
			StepOffset, EndStep, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
			Tabu, StepOffset, AccIn, ImpIn, CycleBest, CycleBestE,
			CycleStep, CycleAccepts, CycleImproves, _CycleTabu
		),
		% restart from best state with cleared tabu list
		Restarts1 is Restarts - 1,
		restart_loop(
			Restarts1, MaxSteps, TenureSpec, Cands, UpdInt, CycleBest, CycleBestE, CycleBest, CycleBestE,
			[], CycleStep, CycleAccepts, CycleImproves, FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		).

	% main loop
	%
	% Arguments:
	%     Step, MaxSteps, TenureSpec, Candidates, UpdateInterval, State, Energy, BestState, BestEnergy,
	%     TabuList, Trials, Accepts, Improves, OutBest, OutBestE,
	%     OutStep, OutAccepts, OutImproves, OutTabu
	%
	% TabuList is a list of State-Expire pairs.

	loop(
		Step, MaxSteps, _TenureSpec, _Cands, UpdInt, _State, _Energy, Best, BestE,
		Tabu, Trials, Accepts, Improves, Best, BestE,
		Step, Accepts, Improves, Tabu
	) :-
		% stop: maximum steps reached
		Step >= MaxSteps,
		!,
		report_final(Step, UpdInt, Trials, Accepts, Improves, BestE, BestE).
	loop(
		Step, _MaxSteps, _TenureSpec, _Cands, UpdInt, _State, Energy, Best, BestE,
		Tabu, Trials, Accepts, Improves, Best, BestE,
		Step, Accepts, Improves, Tabu
	) :-
		% stop: problem-defined stop condition
		stop_condition(Step, BestE, Energy),
		!,
		report_final(Step, UpdInt, Trials, Accepts, Improves, BestE, Energy).
	loop(
		Step, MaxSteps, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
		Tabu, Trials, Accepts, Improves, FinalBest, FinalBestE,
		FinalStep, FinalAccepts, FinalImproves, FinalTabu
	) :-
		% generate and select best admissible candidate
		select_candidate(State, Energy, BestEnergy, Tabu, Step, Cands, Neighbor, NeighborEnergy, Accepted),
		Trials1 is Trials + 1,
		(	Accepted == true ->
			NextState = Neighbor, NextEnergy = NeighborEnergy,
			Accepts1 is Accepts + 1,
			% update tabu list (record the state we leave with an expiration step)
			update_tabu(State, Step, TenureSpec, Tabu, NewTabu),
			% track best
			(	NeighborEnergy < BestEnergy ->
				NewBest = Neighbor, NewBestE = NeighborEnergy,
				Improves1 is Improves + 1
			;	NewBest = BestState, NewBestE = BestEnergy,
				Improves1 is Improves
			)
		;	% no admissible candidate found; stay put (rare)
			NextState = State, NextEnergy = Energy,
			Accepts1 is Accepts,
			NewTabu = Tabu,
			NewBest = BestState, NewBestE = BestEnergy,
			Improves1 is Improves
		),
		% progress reporting
		report_progress(Step, UpdInt, Trials1, Accepts1, Improves1, NewBestE, NextEnergy),
		% next step
		Step1 is Step + 1,
		loop(
			Step1, MaxSteps, TenureSpec, Cands, UpdInt, NextState, NextEnergy, NewBest, NewBestE,
			NewTabu, Trials1, Accepts1, Improves1, FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		).

	% candidate selection; prefer a full neighbors/2 list when available; otherwise sample

	select_candidate(State, Energy, BestEnergy, Tabu, Step, Cands, BestNeighbor, BestNeighborEnergy, Accepted) :-
		(	neighbors(State, AllNeighbors) ->
			length(AllNeighbors, Lenght),
			(	Lenght =< Cands ->
				Candidates = AllNeighbors
			;	% random sample of size Candidates
				sample_list(AllNeighbors, Cands, Candidates)
			),
			evaluate_candidates(Candidates, BestEnergy, Tabu, Step, BestNeighbor, BestNeighborEnergy, Accepted)
		;	% sample via repeated neighbor_state calls
			sample_neighbors(Cands, State, Energy, BestEnergy, Tabu, Step, none, 1.0e300, false, BestNeighbor, BestNeighborEnergy, Accepted)
		).

	% evaluate a concrete list of candidates, keeping the best admissible one
	evaluate_candidates(Candidates, BestEnergy, Tabu, Step, BestNeighbor, BestNeighborEnergy, Accepted) :-
		evaluate_candidates_(Candidates, BestEnergy, Tabu, Step, none, 1.0e300, false, BestNeighbor, BestNeighborEnergy, Accepted).

	evaluate_candidates_([], _BestEnergy, _Tabu, _Step, BestN, BestNE, Acc, BestN, BestNE, Acc).
	evaluate_candidates_([Candidate| Rest], BestEnergy, Tabu, Step, BestN0, BestNE0, Acc0, BestN, BestNE, Acc) :-
		state_energy(Candidate, CandEnergy),
		is_admissible(Candidate, CandEnergy, BestEnergy, Tabu, Step, Admissible),
		(	Admissible == true,
			CandEnergy < BestNE0 ->
			BestN1 = Candidate, BestNE1 = CandEnergy, Acc1 = true
		;	BestN1 = BestN0, BestNE1 = BestNE0, Acc1 = Acc0
		),
		evaluate_candidates_(Rest, BestEnergy, Tabu, Step, BestN1, BestNE1, Acc1, BestN, BestNE, Acc).

	% sample Candidates neighbors via neighbor generation, keep best admissible
	sample_neighbors(0, _State, _Energy, _BestEnergy, _Tabu, _Step, BestN, BestNE, Acc, BestN, BestNE, Acc) :-
		!.
	sample_neighbors(N, State, Energy, BestEnergy, Tabu, Step, BestN0, BestNE0, Acc0, BestN, BestNE, Acc) :-
		N > 0,
		generate_neighbor(State, Energy, Neighbor, NeighborEnergy, _DeltaE),
		is_admissible(Neighbor, NeighborEnergy, BestEnergy, Tabu, Step, Admissible),
		(	Admissible == true,
			NeighborEnergy < BestNE0 ->
			BestN1 = Neighbor, BestNE1 = NeighborEnergy, Acc1 = true
		;	BestN1 = BestN0, BestNE1 = BestNE0, Acc1 = Acc0
		),
		N1 is N - 1,
		sample_neighbors(N1, State, Energy, BestEnergy, Tabu, Step, BestN1, BestNE1, Acc1, BestN, BestNE, Acc).

	% admissibility (non-tabu or aspiration)

	is_admissible(Candidate, CandEnergy, BestEnergy, Tabu, Step, true) :-
		(	\+ is_tabu(Candidate, Tabu, Step) ->
			true
		;	% aspiration: better than global best
			CandEnergy < BestEnergy
		),
		!.
	is_admissible(_, _, _, _, _, false).

	% a state is tabu if it has a non-expired entry
	is_tabu(State, Tabu, Step) :-
		member(State-Expire, Tabu),
		Expire > Step,
		!.

	% neighbor generation (same pattern as SA)

	generate_neighbor(State, Energy, Neighbor, NeighborEnergy, DeltaE) :-
		(	neighbor_state(State, Neighbor, DeltaE) ->
			NeighborEnergy is Energy + DeltaE
		;	neighbor_state(State, Neighbor) ->
			state_energy(Neighbor, NeighborEnergy),
			DeltaE is NeighborEnergy - Energy
		;	fail
		).

	% tabu list update
	%
	% store State-Expire pairs; prune expired entries; for fixed tenure also
	% keep the active list from growing beyond the tenure bound

	update_tabu(State, Step, fixed(Tenure), Tabu0, Tabu) :-
		Tenure > 0,
		!,
		Expire is Step + Tenure,
		prune_tabu(Tabu0, Step, Active),
		length(Active, Lenght),
		(	Lenght < Tenure ->
			Tabu = [State-Expire| Active]
		;	% drop the oldest active entry (last in most-recent-first list)
			append(Prefix, [_], Active),
			!,
			Tabu = [State-Expire| Prefix]
		).
	update_tabu(State, Step, range(Min, Max), Tabu0, Tabu) :-
		!,
		between(Min, Max, Tenure),
		Expire is Step + Tenure,
		prune_tabu(Tabu0, Step, Active),
		Tabu = [State-Expire| Active].
	update_tabu(_, _, _, _, []).

	prune_tabu([], _, []).
	prune_tabu([State-Expire| Rest], Step, Active) :-
		(	Expire > Step ->
			Active = [State-Expire| Active1],
			prune_tabu(Rest, Step, Active1)
		;	prune_tabu(Rest, Step, Active)
		).

	active_tabu_size(Tabu, Step, Size) :-
		prune_tabu(Tabu, Step, Active),
		length(Active, Size).

	% random sample of a list (without replacement)

	sample_list(List, N, Sample) :-
		length(List, Length),
		(	N >= Length ->
			Sample = List
		;	sample_list_(N, List, Sample)
		).

	sample_list_(0, _, []) :-
		!.
	sample_list_(N, List, [X| Xs]) :-
		N > 0,
		length(List, Length),
		Length > 0,
		between(1, Length, Index),
		nth1(Index, List, X),
		selectchk(X, List, Rest),
		N1 is N - 1,
		sample_list_(N1, Rest, Xs).

	% progress reporting

	report_progress(Step, UpdInt, Trials, Accepts, Improves, BestE, CurrE) :-
		UpdInt > 0,
		Step > 0,
		Step mod UpdInt =:= 0,
		!,
		call_progress(Step, Trials, Accepts, Improves, BestE, CurrE).
	report_progress(_Step, _UpdInt, _Trials, _Accepts, _Improves, _BestE, _CurrE).

	report_final(Step, UpdInt, Trials, Accepts, Improves, BestE, CurrE) :-
		UpdInt > 0,
		!,
		call_progress(Step, Trials, Accepts, Improves, BestE, CurrE).
	report_final(_Step, _UpdInt, _Trials, _Accepts, _Improves, _BestE, _CurrE).

	call_progress(Step, Trials, Accepts, Improves, BestE, CurrE) :-
		(	Trials > 0 ->
			AccRate is Accepts / Trials,
			ImpRate is Improves / Trials
		;	AccRate is 0.0,
			ImpRate is 0.0
		),
		ignore(progress(Step, BestE, CurrE, AccRate, ImpRate)).

	% default options

	default_option(max_steps(10000)).
	default_option(tabu_tenure(7)).
	default_option(candidates(20)).
	default_option(updates(0)).
	default_option(restarts(0)).

	% option validation

	valid_option(max_steps(N)) :-
		valid(positive_integer, N).
	valid_option(tabu_tenure(T)) :-
		valid(non_negative_integer, T).
	valid_option(tabu_tenure_range(Min, Max)) :-
		valid(positive_integer, Min),
		valid(positive_integer, Max),
		Min =< Max.
	valid_option(candidates(N)) :-
		valid(positive_integer, N).
	valid_option(updates(N)) :-
		valid(non_negative_integer, N).
	valid_option(restarts(N)) :-
		valid(non_negative_integer, N).
	valid_option(seed(S)) :-
		valid(positive_integer, S).

:- end_object.


:- object(tabu_search(_Problem_),
	extends(tabu_search(_Problem_, xoshiro128pp))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Tabu search optimization algorithm using the Xoshiro128++ random number generator. Convenience object that extends ``tabu_search/2`` with the random algorithm bound to ``xoshiro128pp``.',
		parameters is [
			'Problem' - 'Problem object implementing ``tabu_search_protocol``.'
		],
		see_also is [tabu_search(_, _), tabu_search_problem_protocol]
	]).

:- end_object.
