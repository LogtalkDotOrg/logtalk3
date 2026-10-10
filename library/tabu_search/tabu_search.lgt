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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-10-10,
		comment is 'Tabu search optimization algorithm. Parameterized by a problem object implementing the ``tabu_search_problem_protocol`` protocol and by a random number generator algorithm for the ``fast_random`` library. Minimizes energy with optional problem-defined tabu keys, tenure, aspiration, restart states, neighbor generation, stopping, and progress reporting.',
		parameters is [
			'Problem' - 'Problem object implementing ``tabu_search_problem_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library (e.g. ``xoshiro128pp``, ``xoshiro256ss``, ``well512a``, ...).'
		],
		see_also is [tabu_search(_), tabu_search_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-nonvar, -number), one).
	:- info(run/2, [
		comment is 'Runs the tabu search algorithm using default options and returns the best state found and its energy.',
		argnames is ['BestState', 'BestEnergy'],
		exceptions is [
			'An implemented output hook fails or returns an invalid value' - domain_error(tabu_hook_result, 'Result')
		]
	]).

	:- public(run/3).
	:- mode(run(-nonvar, -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the tabu search algorithm using the given options and returns the best state found and its energy.',
		argnames is ['BestState', 'BestEnergy', 'Options'],
		exceptions is [
			'An option is invalid' - domain_error(option, 'Option'),
			'Exhaustive mode requires implemented enumeration' - existence_error(procedure, 'Problem'::neighbors/2),
			'An implemented output hook fails or returns an invalid value' - domain_error(tabu_hook_result, 'Result')
		],
		remarks is [
			'``max_steps(N)`` option' - 'Maximum number of iterations per cycle (default: ``10000``).',
			'``tabu_tenure(T)`` option' - 'Fixed lifetime in selection steps (default: ``7``). Ignored when a range or a problem ``tabu_tenure/4`` implementation is present.',
			'``tabu_tenure_range(Min, Max)`` option' - 'Uniform random tenure in the inclusive integer range. Overrides fixed tenure; ignored when the problem implements ``tabu_tenure/4``.',
			'``candidates(N)`` option' - 'Number of candidate neighbors examined per iteration (default: ``20``).',
			'``exhaustive(Boolean)`` option' - 'Evaluates the full ``neighbors/2`` list in source order when true, ignoring the candidate limit (default: ``false``). Requires implemented enumeration.',
			'``updates(N)`` option' - 'Number of progress reports during the run. Set to ``0`` to disable. Progress is reported by calling ``progress/5`` on the problem object (default: ``0``).',
			'``seed(S)`` option' - 'Positive integer seed for the random number generator, enabling reproducible runs (default: none).',
			'``restarts(N)`` option' - 'Number of additional cycles (default: ``0``). Clears memory and uses ``restart_state/2`` when implemented, otherwise the global best state.'
		]
	]).

	:- public(run/4).
	:- mode(run(-nonvar, -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the tabu search algorithm using the given options, returns the best state found and its energy, and returns run statistics.',
		argnames is ['BestState', 'BestEnergy', 'Statistics', 'Options'],
		exceptions is [
			'An option is invalid' - domain_error(option, 'Option'),
			'Exhaustive mode requires implemented enumeration' - existence_error(procedure, 'Problem'::neighbors/2),
			'An implemented output hook fails or returns an invalid value' - domain_error(tabu_hook_result, 'Result')
		],
		remarks is [
			'Statistics list' - 'A list of ``Key(Value)`` pairs: ``steps(N)`` is the number of steps executed, ``acceptances(A)`` is the number of accepted moves, ``improvements(I)`` is the number of moves that improved the best energy, and ``final_tabu_size(S)`` is the number of non-expired tabu entries at termination.'
		]
	]).

	:- uses(_Problem_, [
		initial_state/1, state_energy/2, stop_condition/3, progress/5, neighbor_state/2, neighbor_state/3,
		neighbors/2
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		between/3, randomize/1, permutation/2, set/4
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(list, [
		length/2
	]).

	:- private(select_candidate/10).
	:- mode(select_candidate(+compound, +nonvar, +number, +number, +list, +non_negative_integer, +positive_integer, -nonvar, -number, -boolean), one).
	:- info(select_candidate/10, [
		comment is 'Selects the best admissible candidate using the cached run policy.',
		argnames is ['Policy', 'State', 'Energy', 'BestEnergy', 'Tabu', 'Step', 'Candidates', 'Neighbor', 'NeighborEnergy', 'Accepted'],
		exceptions is [
			'Enumeration fails in exhaustive mode' - domain_error(tabu_hook_result, neighbors/2),
			'A key hook returns an invalid result' - domain_error(tabu_hook_result, 'Result')
		]
	]).

	:- private(require_neighborhood/2).
	:- mode(require_neighborhood(+boolean, +object_identifier), one).
	:- info(require_neighborhood/2, [
		comment is 'Checks the implementation required for exhaustive evaluation.',
		argnames is ['Exhaustive', 'Problem'],
		exceptions is [
			'Enumeration is not implemented' - existence_error(procedure, 'Problem'::neighbors/2)
		]
	]).

	:- private(restart_candidate/7).
	:- mode(restart_candidate(+compound, +nonvar, +number, -nonvar, -number, -nonvar, -number), one).
	:- info(restart_candidate/7, [
		comment is 'Resolves a restart state and updates the global best independently.',
		argnames is ['Policy', 'Best', 'BestEnergy', 'State', 'Energy', 'NewBest', 'NewBestEnergy'],
		exceptions is [
			'The restart hook fails or returns an invalid state' - domain_error(tabu_hook_result, 'Result')
		]
	]).

	:- private(state_key/3).
	:- mode(state_key(+boolean, +nonvar, -nonvar), one).
	:- info(state_key/3, [
		comment is 'Resolves an optional tabu key without copying the state.',
		argnames is ['KeyHook', 'State', 'Key'],
		exceptions is [
			'The key hook fails or returns an unbound key' - domain_error(tabu_hook_result, 'Result')
		]
	]).

	:- private(resolve_tenure/6).
	:- mode(resolve_tenure(+boolean, +compound, +non_negative_integer, +number, +number, -non_negative_integer), one).
	:- info(resolve_tenure/6, [
		comment is 'Resolves tenure with hook, range, then fixed-option precedence.',
		argnames is ['TenureHook', 'Specification', 'Step', 'BestEnergy', 'CurrentEnergy', 'Tenure'],
		exceptions is [
			'The tenure hook fails or returns an invalid value' - domain_error(tabu_hook_result, 'Result')
		]
	]).

	run(BestState, BestEnergy) :-
		run(BestState, BestEnergy, _Statistics, []).

	run(BestState, BestEnergy, UserOptions) :-
		run(BestState, BestEnergy, _Statistics, UserOptions).

	run(BestState, BestEnergy, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		parameter(1, Problem),
		hook_available(Problem, tabu_key(_, _), KeyHook),
		hook_available(Problem, restart_state(_, _), RestartHook),
		hook_available(Problem, tabu_tenure(_, _, _, _), TenureHook),
		hook_available(Problem, aspiration(_, _, _), AspirationHook),
		^^option(exhaustive(Exhaustive), Options),
		require_neighborhood(Exhaustive, Problem),
		Policy = policy(KeyHook, RestartHook, TenureHook, AspirationHook, Exhaustive),
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
			Policy, Restarts, MaxSteps, TenureSpec, Candidates, UpdateInterval,
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
	% when Restarts > 0, run a cycle, then resolve the next state with a cleared tabu list

	restart_loop(
		Policy, 0, MaxSteps, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
		Tabu, StepOffset, AccIn, ImpIn, FinalBest, FinalBestE,
		FinalStep, FinalAccepts, FinalImproves, FinalTabu
	) :-
		!,
		EndStep is StepOffset + MaxSteps,
		loop(
			Policy, StepOffset, EndStep, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
			Tabu, report(StepOffset, AccIn, ImpIn), AccIn, ImpIn, FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		).
	restart_loop(
		Policy, Restarts, MaxSteps, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
		Tabu, StepOffset, AccIn, ImpIn, FinalBest, FinalBestE,
		FinalStep, FinalAccepts, FinalImproves, FinalTabu
	) :-
		!,
		EndStep is StepOffset + MaxSteps,
		loop(
			Policy, StepOffset, EndStep, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
			Tabu, report(StepOffset, AccIn, ImpIn), AccIn, ImpIn, CycleBest, CycleBestE,
			CycleStep, CycleAccepts, CycleImproves, _CycleTabu
		),
		% resolve restart state with cleared tabu list
		Restarts1 is Restarts - 1,
		restart_candidate(Policy, CycleBest, CycleBestE, RestartState, RestartEnergy, RestartBest, RestartBestE),
		restart_loop(
			Policy, Restarts1, MaxSteps, TenureSpec, Cands, UpdInt, RestartState, RestartEnergy, RestartBest, RestartBestE,
			[], CycleStep, CycleAccepts, CycleImproves, FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		).

	% main loop
	%
	% Arguments:
	%     Policy, Step, MaxSteps, TenureSpec, Candidates, UpdateInterval, State, Energy, BestState, BestEnergy,
	%     TabuList, LastReport, Accepts, Improves, OutBest, OutBestE,
	%     OutStep, OutAccepts, OutImproves, OutTabu
	%
	% TabuList is a list of Key-Expire pairs.

	loop(
		_Policy, Step, MaxSteps, _TenureSpec, _Cands, UpdInt, _State, Energy, Best, BestE,
		Tabu, Report, Accepts, Improves, Best, BestE,
		Step, Accepts, Improves, Tabu
	) :-
		% stop: maximum steps reached
		Step >= MaxSteps,
		!,
		report_final(Step, UpdInt, Report, Accepts, Improves, BestE, Energy).
	loop(
		_Policy, Step, _MaxSteps, _TenureSpec, _Cands, UpdInt, _State, Energy, Best, BestE,
		Tabu, Report, Accepts, Improves, Best, BestE,
		Step, Accepts, Improves, Tabu
	) :-
		% stop: problem-defined stop condition
		stop_condition(Step, BestE, Energy),
		!,
		report_final(Step, UpdInt, Report, Accepts, Improves, BestE, Energy).
	loop(
		Policy, Step, MaxSteps, TenureSpec, Cands, UpdInt, State, Energy, BestState, BestEnergy,
		Tabu, Report, Accepts, Improves, FinalBest, FinalBestE,
		FinalStep, FinalAccepts, FinalImproves, FinalTabu
	) :-
		!,
		report_progress(Step, UpdInt, Report, Accepts, Improves, BestEnergy, Energy, NextReport),
		% generate and select best admissible candidate
		select_candidate(Policy, State, Energy, BestEnergy, Tabu, Step, Cands, Neighbor, NeighborEnergy, Accepted),
		(	Accepted == true ->
			NextState = Neighbor, NextEnergy = NeighborEnergy,
			Accepts1 is Accepts + 1,
			% update tabu list (record the state we leave with an expiration step)
			update_tabu(Policy, State, Step, BestEnergy, Energy, TenureSpec, Tabu, NewTabu),
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
		% next step
		Step1 is Step + 1,
		loop(
			Policy, Step1, MaxSteps, TenureSpec, Cands, UpdInt, NextState, NextEnergy, NewBest, NewBestE,
			NewTabu, NextReport, Accepts1, Improves1, FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTabu
		).

	% candidate selection; prefer a full neighbors/2 list when available; otherwise sample

	select_candidate(Policy, State, _Energy, BestEnergy, Tabu, Step, _Cands, BestNeighbor, BestNeighborEnergy, Accepted) :-
		Policy = policy(_, _, _, _, true),
		!,
		(	neighbors(State, AllNeighbors) ->
			evaluate_candidates(Policy, AllNeighbors, BestEnergy, Tabu, Step, BestNeighbor, BestNeighborEnergy, Accepted)
		;	domain_error(tabu_hook_result, neighbors/2)
		).
	select_candidate(Policy, State, Energy, BestEnergy, Tabu, Step, Cands, BestNeighbor, BestNeighborEnergy, Accepted) :-
		(	neighbors(State, AllNeighbors) ->
			length(AllNeighbors, Length),
			( 	Length =< Cands ->
				Candidates = AllNeighbors
			;	% random sample of size Candidates
				sample_list(AllNeighbors, Length, Cands, Candidates)
			),
			evaluate_candidates(Policy, Candidates, BestEnergy, Tabu, Step, BestNeighbor, BestNeighborEnergy, Accepted)
		;	% sample via repeated neighbor_state calls
			sample_neighbors(Policy, Cands, State, Energy, BestEnergy, Tabu, Step, none, 0, false, BestNeighbor, BestNeighborEnergy, Accepted)
		).

	% evaluate a concrete list of candidates, keeping the best admissible one
	evaluate_candidates(Policy, Candidates, BestEnergy, Tabu, Step, BestNeighbor, BestNeighborEnergy, Accepted) :-
		evaluate_candidates_(Policy, Candidates, BestEnergy, Tabu, Step, none, 0, false, BestNeighbor, BestNeighborEnergy, Accepted).

	evaluate_candidates_(_Policy, [], _BestEnergy, _Tabu, _Step, BestN, BestNE, Acc, BestN, BestNE, Acc) :-
		!.
	evaluate_candidates_(Policy, [Candidate| Rest], BestEnergy, Tabu, Step, BestN0, BestNE0, Acc0, BestN, BestNE, Acc) :-
		state_energy(Candidate, CandEnergy),
		(	(Acc0 == false; CandEnergy < BestNE0),
			is_admissible(Policy, Candidate, CandEnergy, BestEnergy, Tabu, Step, true) ->
			BestN1 = Candidate, BestNE1 = CandEnergy, Acc1 = true
		;	BestN1 = BestN0, BestNE1 = BestNE0, Acc1 = Acc0
		),
		evaluate_candidates_(Policy, Rest, BestEnergy, Tabu, Step, BestN1, BestNE1, Acc1, BestN, BestNE, Acc).

	sample_neighbors(_Policy, 0, _State, _Energy, _BestEnergy, _Tabu, _Step, BestN, BestNE, Acc, BestN, BestNE, Acc) :-
		!.
	sample_neighbors(Policy, N, State, Energy, BestEnergy, Tabu, Step, BestN0, BestNE0, Acc0, BestN, BestNE, Acc) :-
		N > 0,
		generate_neighbor(State, Energy, Neighbor, NeighborEnergy, _DeltaE),
		(	(Acc0 == false; NeighborEnergy < BestNE0),
			is_admissible(Policy, Neighbor, NeighborEnergy, BestEnergy, Tabu, Step, true) ->
			BestN1 = Neighbor, BestNE1 = NeighborEnergy, Acc1 = true
		;	BestN1 = BestN0, BestNE1 = BestNE0, Acc1 = Acc0
		),
		N1 is N - 1,
		sample_neighbors(Policy, N1, State, Energy, BestEnergy, Tabu, Step, BestN1, BestNE1, Acc1, BestN, BestNE, Acc).

	% admissibility (non-tabu or aspiration)

	is_admissible(Policy, Candidate, CandEnergy, BestEnergy, Tabu, Step, true) :-
		admissible_candidate(Policy, Candidate, CandEnergy, BestEnergy, Tabu, Step),
		!.
	is_admissible(_, _, _, _, _, _, false).

	admissible_candidate(Policy, Candidate, CandEnergy, BestEnergy, Tabu, Step) :-
		Policy = policy(_, _, _, false, _),
		!,
		(	CandEnergy < BestEnergy ->
			true
		;	\+ candidate_tabu(Policy, Candidate, Tabu, Step)
		).
	admissible_candidate(Policy, Candidate, CandEnergy, BestEnergy, Tabu, Step) :-
		Policy = policy(_, _, _, true, _),
		!,
		(	candidate_tabu(Policy, Candidate, Tabu, Step) ->
			parameter(1, Problem),
			Problem::aspiration(Candidate, CandEnergy, BestEnergy)
		;	true
		).

	candidate_tabu(Policy, State, Tabu, Step) :-
		Tabu \== [],
		Policy = policy(KeyHook, _, _, _, _),
		state_key(KeyHook, State, Key),
		is_tabu(Key, Tabu, Step).

	hook_available(Problem, Head, Available) :-
		(	Problem::predicate_property(Head, defined_in(_)) ->
			Available = true
		;	Available = false
		).

	require_neighborhood(false, _) :-
		!.
	require_neighborhood(true, Problem) :-
		!,
		(	Problem::predicate_property(neighbors(_, _), defined_in(_)) ->
			true
		;	existence_error(procedure, Problem::neighbors/2)
		).

	restart_candidate(policy(_, false, _, _, _), Best, BestE, Best, BestE, Best, BestE) :-
		!.
	restart_candidate(policy(_, true, _, _, _), Best, BestE, State, Energy, NewBest, NewBestE) :-
		!,
		parameter(1, Problem),
		(	Problem::restart_state(Best, State) ->
			(	nonvar(State) ->
				true
			;	domain_error(tabu_hook_result, restart_state/2-State)
			)
		;	domain_error(tabu_hook_result, restart_state/2)
		),
		state_energy(State, Energy),
		(	Energy < BestE ->
			NewBest = State,
			NewBestE = Energy
		;	NewBest = Best,
			NewBestE = BestE
		).

	state_key(false, State, State) :-
		!.
	state_key(true, State, Key) :-
		!,
		parameter(1, Problem),
		(	Problem::tabu_key(State, Key) ->
			(	nonvar(Key) ->
				true
			;	domain_error(tabu_hook_result, tabu_key/2-Key)
			)
		;	domain_error(tabu_hook_result, tabu_key/2)
		).

	% a state is tabu if it has a non-expired entry
	is_tabu(State, [Candidate-Expire| Rest], Step) :-
		(	Expire > Step,
			State == Candidate ->
			true
		;	is_tabu(State, Rest, Step)
		).

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
	% store Key-Expire pairs; prune entries inactive at the next selection step

	update_tabu(Policy, State, Step, BestEnergy, CurrentEnergy, Spec, Tabu0, Tabu) :-
		Policy = policy(KeyHook, _, TenureHook, _, _),
		resolve_tenure(TenureHook, Spec, Step, BestEnergy, CurrentEnergy, Tenure),
		NextStep is Step + 1,
		prune_tabu(Tabu0, NextStep, Active),
		(	Tenure > 0 ->
			Expire is Step + Tenure + 1,
			state_key(KeyHook, State, Key),
			Tabu = [Key-Expire| Active]
		;	Tabu = Active
		).

	resolve_tenure(false, fixed(Tenure), _, _, _, Tenure) :-
		!.
	resolve_tenure(false, range(Min, Max), _, _, _, Tenure) :-
		!,
		between(Min, Max, Tenure).
	resolve_tenure(true, _Spec, Step, BestEnergy, CurrentEnergy, Tenure) :-
		!,
		parameter(1, Problem),
		(	Problem::tabu_tenure(Step, BestEnergy, CurrentEnergy, Tenure) ->
			(	valid(non_negative_integer, Tenure) ->
				true
			;	domain_error(tabu_hook_result, tabu_tenure/4-Tenure)
			)
		;	domain_error(tabu_hook_result, tabu_tenure/4)
		).

	prune_tabu([], _, []).
	prune_tabu([State-Expire| Rest], Step, Active) :-
		(	Expire > Step ->
			Active = [State-Expire| Active1],
			prune_tabu(Rest, Step, Active1)
		;	prune_tabu(Rest, Step, Active)
		).

	active_tabu_size(Tabu, Step, Size) :-
		active_tabu_size(Tabu, Step, 0, Size).

	active_tabu_size([], _, Size, Size).
	active_tabu_size([_-Expire| Rest], Step, Count, Size) :-
		(	Expire > Step ->
			Next is Count + 1
		;	Next = Count
		),
		active_tabu_size(Rest, Step, Next, Size).

	% random sample of a list (without replacement)

	sample_list(List, Length, N, Sample) :-
		(	N >= Length ->
			Sample = List
		;	Sparse is min(N, Length - N),
			(	Sparse =< 20 ->
				set(Sparse, 1, Length, Indices),
				(	N =< Length - N ->
					Include = true
				;	Include = false
				),
				indexed_subset(List, 1, Indices, Include, Subset)
			;	sample_list_(N, Length, List, Subset)
			),
			permutation(Subset, Sample)
		).

	indexed_subset(List, _, [], false, List) :-
		!.
	indexed_subset(_, _, [], true, []) :-
		!.
	indexed_subset([Head| Tail], Position, [Index| Indices], Include, Sample) :-
		(	Position =:= Index ->
			Match = true,
			NextIndices = Indices
		;	Match = false,
			NextIndices = [Index| Indices]
		),
		(	Match == Include ->
			Sample = [Head| Rest]
		;	Sample = Rest
		),
		Next is Position + 1,
		indexed_subset(Tail, Next, NextIndices, Include, Rest).

	sample_list_(0, _, _, []) :-
		!.
	sample_list_(N, N, List, List) :-
		!.
	sample_list_(N, Length, [Head| Tail], Sample) :-
		between(1, Length, Draw),
		Length1 is Length - 1,
		(	Draw =< N ->
			Sample = [Head| Rest],
			N1 is N - 1
		;	Sample = Rest,
			N1 = N
		),
		sample_list_(N1, Length1, Tail, Rest).

	% progress reporting

	report_progress(Step, UpdInt, Report, Accepts, Improves, BestE, CurrE, report(Step, Accepts, Improves)) :-
		UpdInt > 0,
		Report = report(LastStep, _, _),
		Step > LastStep,
		Step mod UpdInt =:= 0,
		!,
		call_progress(Step, Report, Accepts, Improves, BestE, CurrE).
	report_progress(_Step, _UpdInt, Report, _Accepts, _Improves, _BestE, _CurrE, Report).

	report_final(Step, UpdInt, Report, Accepts, Improves, BestE, CurrE) :-
		UpdInt > 0,
		!,
		call_progress(Step, Report, Accepts, Improves, BestE, CurrE).
	report_final(_Step, _UpdInt, _Trials, _Accepts, _Improves, _BestE, _CurrE).

	call_progress(Step, report(LastStep, LastAccepts, LastImproves), Accepts, Improves, BestE, CurrE) :-
		Trials is Step - LastStep,
		(	Trials > 0 ->
			AccRate is (Accepts - LastAccepts) / Trials,
			ImpRate is (Improves - LastImproves) / Trials
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
	default_option(exhaustive(false)).

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
	valid_option(exhaustive(Boolean)) :-
		once((Boolean == true; Boolean == false)).

:- end_object.


:- object(tabu_search(_Problem_),
	extends(tabu_search(_Problem_, xoshiro128pp))).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-08-15,
		comment is 'Tabu search optimization algorithm using the Xoshiro128++ random number generator. Convenience object that extends ``tabu_search/2`` with the random algorithm bound to ``xoshiro128pp``.',
		parameters is [
			'Problem' - 'Problem object implementing ``tabu_search_problem_protocol``.'
		],
		see_also is [tabu_search(_, _), tabu_search_problem_protocol]
	]).

:- end_object.
