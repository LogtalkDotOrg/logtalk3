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


:- object(simulated_annealing(_Problem_, _RandomAlgorithm_),
	imports(options)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-24,
		comment is 'Simulated annealing optimization algorithm. Parameterized by a problem object implementing the ``simulated_annealing_protocol`` protocol and by a random number generator algorithm for the ``fast_random`` library. The algorithm minimizes the energy (cost) function defined by the problem. Custom cooling schedules, stop conditions, delta-energy neighbor generation, progress reporting, and reheating restarts can be defined by the problem object or configured via options; suitable defaults are used otherwise.',
		parameters is [
			'Problem' - 'Problem object implementing ``simulated_annealing_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library (e.g. ``xoshiro128pp``, ``xoshiro256ss``, ``well512a``, ...).'
		],
		remarks is [
			'Algorithm' - 'Simulated annealing is a probabilistic metaheuristic for global optimization. It explores the solution space by iteratively generating neighbor states and accepting them based on an energy-dependent probability that decreases over time as the temperature cools.',
			'Acceptance criterion' - 'Uses the standard Boltzmann acceptance criterion: a worse neighbor is accepted with probability ``exp(-DeltaE / Temperature)``.',
			'Default cooling schedule' - 'Geometric cooling: ``NewTemp is Temp * 0.995``. Override by defining ``cooling_schedule/3`` in the problem object.',
			'Default stop condition' - 'The search stops when the maximum number of steps is reached or the temperature drops below the minimum temperature. Override by defining ``stop_condition/3`` in the problem object.',
			'Delta-energy optimization' - 'If the problem object defines ``neighbor_state/3``, the algorithm uses the returned delta energy directly instead of calling ``state_energy/2`` on the neighbor. This is useful when computing the energy change is cheaper than recomputing the full energy.',
			'Progress reporting' - 'If the problem object defines ``progress/5``, it is called periodically with the current step, temperature, best energy, acceptance rate, and improvement rate. The reporting interval is controlled by the ``updates(N)`` option. A final report is always produced when the loop terminates.',
			'Best state tracking' - 'The algorithm tracks the best state found across all iterations and across all restart cycles, not just the final state.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.',
			'Reheating restart' - 'The ``restarts(N)`` option runs N additional SA cycles after the first. Each restart reheats the temperature to the initial value and begins from the best state found so far, allowing the search to escape local minima. Statistics accumulate across all cycles.',
			'Auto-temperature estimation' - 'The ``estimate_temperature/1-2`` predicates sample random neighbor transitions and compute an initial temperature that would produce a target acceptance rate. This avoids manual tuning of the initial temperature.'
		],
		see_also is [simulated_annealing(_), simulated_annealing_protocol]
	]).

	:- public(estimate_temperature/1).
	:- mode(estimate_temperature(-float), one).
	:- info(estimate_temperature/1, [
		comment is 'Estimates an initial temperature for the problem using default options (200 samples, 80% target acceptance rate).',
		argnames is ['Temperature']
	]).

	:- public(estimate_temperature/2).
	:- mode(estimate_temperature(-float, +list(compound)), one).
	:- info(estimate_temperature/2, [
		comment is 'Estimates an initial temperature by sampling random neighbor transitions. The temperature is computed so that the Boltzmann acceptance probability for the average uphill move equals the target acceptance rate. Sampling starts from the problem initial state and follows a random walk.',
		argnames is ['Temperature', 'Options'],
		remarks is [
			'``samples(N)`` option' - 'Number of random neighbor transitions to sample (default: ``200``).',
			'``acceptance_rate(P)`` option' - 'Target initial acceptance rate as an integer percentage between 1 and 99 (default: ``80``).'
		]
	]).

	:- public(run/2).
	:- mode(run(-term, -number), one).
	:- info(run/2, [
		comment is 'Runs the simulated annealing algorithm using default options and returns the best state found and its energy.',
		argnames is ['BestState', 'BestEnergy']
	]).

	:- public(run/3).
	:- mode(run(-term, -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the simulated annealing algorithm using the given options and returns the best state found and its energy.',
		argnames is ['BestState', 'BestEnergy', 'Options'],
		remarks is [
			'``max_steps(N)`` option' - 'Maximum number of iterations per cycle (default: ``10000``).',
			'``min_temperature(T)`` option' - 'Minimum temperature floor; search stops when temperature drops below this value (default: ``0.001``).',
			'``updates(N)`` option' - 'Number of progress reports during the run. Set to ``0`` to disable. Progress is reported by calling ``progress/5`` on the problem object (default: ``0``).',
			'``seed(S)`` option' - 'Positive integer seed for the random number generator, enabling reproducible runs (default: none).',
			'``restarts(N)`` option' - 'Number of additional SA cycles after the first. Each restart reheats the temperature and begins from the best state found so far (default: ``0``).'
		]
	]).

	:- public(run/4).
	:- mode(run(-term, -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the simulated annealing algorithm using the given options, returns the best state found and its energy, and returns run statistics.',
		argnames is ['BestState', 'BestEnergy', 'Statistics', 'Options'],
		remarks is [
			'Statistics list' - 'A list of ``Key(Value)`` pairs: ``steps(N)`` is the number of steps executed, ``acceptances(A)`` is the number of accepted moves, ``improvements(I)`` is the number of moves that improved the best energy, and ``final_temperature(T)`` is the temperature at termination.'
		]
	]).

	:- uses(_Problem_, [
		initial_state/1, state_energy/2, initial_temperature/1, stop_condition/3, progress/5,
		cooling_schedule/3, neighbor_state/2, neighbor_state/3
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		random/1, randomize/1
	]).

	:- uses(type, [
		valid/2
	]).

	estimate_temperature(Temperature) :-
		estimate_temperature(Temperature, []).

	estimate_temperature(Temperature, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(samples(Samples), Options),
		^^option(acceptance_rate(Percentage), Options),
		AcceptanceRate is Percentage / 100.0,
		initial_state(State0),
		state_energy(State0, Energy0),
		hook_enabled(neighbor_state(_, _, _), DeltaHook),
		sample_deltas(Samples, DeltaHook, State0, Energy0, 0, 0.0, Count, SumDelta),
		% Compute T = -mean_delta / ln(acceptance_rate)
		(	Count > 0 ->
			MeanDelta is SumDelta / Count,
			Temperature is -MeanDelta / log(AcceptanceRate)
		;	% No uphill moves found; use a heuristic fallback
			Temperature is 1.0
		).

	% Sample random neighbor transitions and collect absolute deltas
	% of uphill (worsening) moves. Walk follows the random neighbors
	% to explore the landscape.
	sample_deltas(0, _DeltaHook, _State, _Energy, Count, Sum, Count, Sum) :-
		!.
	sample_deltas(N, DeltaHook, State, Energy, Count0, Sum0, Count, Sum) :-
		generate_neighbor(DeltaHook, State, Energy, Neighbor, NeighborEnergy, DeltaE),
		(	DeltaE > 0 ->
			% Uphill move: accumulate the delta
			Count1 is Count0 + 1,
			Sum1 is Sum0 + DeltaE
		;	Count1 is Count0,
			Sum1 is Sum0
		),
		N1 is N - 1,
		sample_deltas(N1, DeltaHook, Neighbor, NeighborEnergy, Count1, Sum1, Count, Sum).

	run(BestState, BestEnergy) :-
		run(BestState, BestEnergy, _Statistics, []).

	run(BestState, BestEnergy, UserOptions) :-
		run(BestState, BestEnergy, _Statistics, UserOptions).

	run(BestState, BestEnergy, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		% Handle seed option
		(	^^option(seed(Seed), Options) ->
			randomize(Seed)
		;	true
		),
		initial_state(State0),
		state_energy(State0, Energy0),
		initial_temperature(Temp0),
		^^option(max_steps(MaxSteps), Options),
		^^option(min_temperature(MinTemp), Options),
		^^option(updates(Updates), Options),
		^^option(restarts(Restarts), Options),
		% Compute the update interval based on total expected steps (0 means disabled)
		TotalMaxSteps is MaxSteps * (Restarts + 1),
		(	Updates > 0 ->
			UpdateInterval is max(1, (TotalMaxSteps - 1) // Updates)
		;	UpdateInterval is 0
		),
		hook_enabled(neighbor_state(_, _, _), DeltaHook),
		hook_enabled(cooling_schedule(_, _, _), CoolingHook),
		hook_enabled(stop_condition(_, _, _), StopHook),
		Settings = settings(UpdateInterval, DeltaHook, CoolingHook, StopHook),
		restart_loop(
			Restarts, MaxSteps, MinTemp, Settings,
			Temp0, State0, Energy0,
			State0, Energy0,
			0, 0, 0,
			BestState, BestEnergy,
			FinalStep, FinalAccepts, FinalImproves, FinalTemp
		),
		Statistics = [
			steps(FinalStep),
			acceptances(FinalAccepts),
			improvements(FinalImproves),
			final_temperature(FinalTemp)
		].

	% --- Restart loop ---
	% When Restarts is 0, this is the last (or only) cycle.
	% When Restarts > 0, run a cycle, then restart from the best
	% state found with the initial temperature.

	restart_loop(0, MaxSteps, MinTemp, Settings,
				Temp, State, Energy,
				BestState, BestEnergy,
				StepOffset, AccIn, ImpIn,
				FinalBest, FinalBestE,
				FinalStep, FinalAccepts, FinalImproves, FinalTemp) :-
		!,
		EndStep is StepOffset + MaxSteps,
		loop(StepOffset, EndStep, MinTemp, Settings,
				Temp, State, Energy,
				BestState, BestEnergy,
				AccIn, ImpIn,
				FinalBest, FinalBestE,
				FinalStep, FinalAccepts, FinalImproves, FinalTemp).
	restart_loop(Restarts, MaxSteps, MinTemp, Settings,
				Temp, State, Energy,
				BestState, BestEnergy,
				StepOffset, AccIn, ImpIn,
				FinalBest, FinalBestE,
				FinalStep, FinalAccepts, FinalImproves, FinalTemp) :-
		EndStep is StepOffset + MaxSteps,
		loop(StepOffset, EndStep, MinTemp, Settings,
				Temp, State, Energy,
				BestState, BestEnergy,
				AccIn, ImpIn,
				CycleBest, CycleBestE,
				CycleStep, CycleAccepts, CycleImproves, _CycleTemp),
		% Restart from best state with reset temperature
		Restarts1 is Restarts - 1,
		initial_temperature(NewTemp),
		restart_loop(Restarts1, MaxSteps, MinTemp, Settings,
					NewTemp, CycleBest, CycleBestE,
					CycleBest, CycleBestE,
					CycleStep, CycleAccepts, CycleImproves,
					FinalBest, FinalBestE,
					FinalStep, FinalAccepts, FinalImproves, FinalTemp).

	% --- Main loop ---
	% Arguments: Step, MaxSteps, MinTemp, Settings,
	%            Temp, State, Energy,
	%            BestState, BestEnergy,
	%            Accepts, Improves,
	%            OutBest, OutBestE,
	%            OutStep, OutAccepts, OutImproves, OutTemp

	loop(Step, MaxSteps, _MinTemp, settings(UpdInt, _, _, _),
			Temp, _State, _Energy,
			Best, BestE,
			Accepts, Improves,
			Best, BestE,
			Step, Accepts, Improves, Temp) :-
		% Stop: maximum steps reached
		Step >= MaxSteps,
		!,
		report_final(Step, UpdInt, Accepts, Improves, Temp, BestE).
	loop(Step, _MaxSteps, _MinTemp, settings(UpdInt, _, _, true),
			Temp, _State, _Energy,
			Best, BestE,
			Accepts, Improves,
			Best, BestE,
			Step, Accepts, Improves, Temp) :-
		% Stop: problem-defined stop condition
		stop_condition(Step, Temp, BestE),
		!,
		report_final(Step, UpdInt, Accepts, Improves, Temp, BestE).
	loop(Step, _MaxSteps, MinTemp, settings(UpdInt, _, _, _),
			Temp, _State, _Energy,
			Best, BestE,
			Accepts, Improves,
			Best, BestE,
			Step, Accepts, Improves, Temp) :-
		% Stop: temperature below minimum
		Temp =< MinTemp,
		!,
		report_final(Step, UpdInt, Accepts, Improves, Temp, BestE).
	loop(Step, MaxSteps, MinTemp, Settings,
			Temp, State, Energy,
			BestState, BestEnergy,
			Accepts, Improves,
			FinalBest, FinalBestE,
			FinalStep, FinalAccepts, FinalImproves, FinalTemp) :-
		Settings = settings(UpdInt, DeltaHook, CoolingHook, _),
		% Generate neighbor (prefer delta-energy variant)
		generate_neighbor(DeltaHook, State, Energy, Neighbor, NeighborEnergy, DeltaE),
		% Accept or reject
		(	accept(DeltaE, Temp) ->
			NextState = Neighbor,
			NextEnergy = NeighborEnergy,
			Accepts1 is Accepts + 1,
			(	NeighborEnergy < BestEnergy ->
				NewBest = Neighbor,
				NewBestE = NeighborEnergy,
				Improves1 is Improves + 1
			;	NewBest = BestState,
				NewBestE = BestEnergy,
				Improves1 is Improves
			)
		;	NextState = State,
			NextEnergy = Energy,
			Accepts1 is Accepts,
			NewBest = BestState,
			NewBestE = BestEnergy,
			Improves1 is Improves
		),
		% Cool
		cool(CoolingHook, Temp, Step, NextTemp),
		% Progress reporting
		Step1 is Step + 1,
		report_progress(Step, UpdInt, Step1, Accepts1, Improves1, NextTemp, NewBestE),
		% Next step
		loop(Step1, MaxSteps, MinTemp, Settings,
				NextTemp, NextState, NextEnergy,
				NewBest, NewBestE,
				Accepts1, Improves1,
				FinalBest, FinalBestE,
				FinalStep, FinalAccepts, FinalImproves, FinalTemp).

	% --- Neighbor generation ---
	% Try neighbor_state/3 (with delta-E) first, fall back to neighbor_state/2

	generate_neighbor(true, State, Energy, Neighbor, NeighborEnergy, DeltaE) :-
		!,
		(	neighbor_state(State, Neighbor, DeltaE) ->
			NeighborEnergy is Energy + DeltaE
		;	generate_neighbor(false, State, Energy, Neighbor, NeighborEnergy, DeltaE)
		).
	generate_neighbor(false, State, Energy, Neighbor, NeighborEnergy, DeltaE) :-
		(	neighbor_state(State, Neighbor) ->
			state_energy(Neighbor, NeighborEnergy),
			DeltaE is NeighborEnergy - Energy
		;	fail
		).

	hook_enabled(Head, Enabled) :-
		(	(	_Problem_::predicate_property(Head, dynamic)
			;	_Problem_::predicate_property(Head, defined_in(_))
			) ->
			Enabled = true
		;	Enabled = false
		).

	% --- Boltzmann acceptance criterion ---

	accept(DeltaE, _Temp) :-
		DeltaE =< 0,
		!.
	accept(DeltaE, Temp) :-
		Temp > 0,
		P is exp(-DeltaE / Temp),
		random(R),
		R < P.

	% --- Custom or default cooling schedule ---

	cool(false, Temp, _Step, NewTemp) :-
		NewTemp is Temp * 0.995.
	cool(true, Temp, Step, NewTemp) :-
		(	cooling_schedule(Temp, Step, NewTemp) ->
			true
		;	% Default geometric cooling
			NewTemp is Temp * 0.995
		).

	% --- Progress reporting ---

	report_progress(Step, UpdInt, Trials, Accepts, Improves, Temp, BestE) :-
		(	UpdInt > 0,
			Step > 0,
			Step mod UpdInt =:= 0 ->
			call_progress(Step, Trials, Accepts, Improves, Temp, BestE)
		;	true
		).

	% Final progress report on termination
	report_final(Step, UpdInt, Accepts, Improves, Temp, BestE) :-
		UpdInt > 0,
		!,
		call_progress(Step, Step, Accepts, Improves, Temp, BestE).
	report_final(_Step, _UpdInt, _Accepts, _Improves, _Temp, _BestE).

	call_progress(Step, Trials, Accepts, Improves, Temp, BestE) :-
		(	Trials > 0 ->
			AccRate is Accepts / Trials,
			ImpRate is Improves / Trials
		;	AccRate is 0.0,
			ImpRate is 0.0
		),
		ignore(progress(Step, Temp, BestE, AccRate, ImpRate)).

	% --- Default options ---

	default_option(max_steps(10000)).
	default_option(min_temperature(0.001)).
	default_option(updates(0)).
	default_option(restarts(0)).
	default_option(samples(200)).
	default_option(acceptance_rate(80)).

	% --- Option validation ---

	valid_option(max_steps(N)) :-
		valid(positive_integer, N).
	valid_option(min_temperature(T)) :-
		valid(positive_float, T).
	valid_option(updates(N)) :-
		valid(non_negative_integer, N).
	valid_option(restarts(N)) :-
		valid(non_negative_integer, N).
	valid_option(seed(S)) :-
		valid(positive_integer, S).
	valid_option(samples(N)) :-
		valid(positive_integer, N).
	valid_option(acceptance_rate(P)) :-
		valid(positive_integer, P),
		P < 100.

:- end_object.


:- object(simulated_annealing(_Problem_),
	extends(simulated_annealing(_Problem_, xoshiro128pp))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-22,
		comment is 'Simulated annealing optimization algorithm using the Xoshiro128++ random number generator. Convenience object that extends ``simulated_annealing/2`` with the random algorithm bound to ``xoshiro128pp``.',
		parameters is [
			'Problem' - 'Problem object implementing ``simulated_annealing_protocol``.'
		],
		see_also is [simulated_annealing(_, _), simulated_annealing_protocol]
	]).

:- end_object.
