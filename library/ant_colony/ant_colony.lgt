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


:- object(ant_colony(_Problem_, _RandomAlgorithm_),
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Ant Colony Optimization (Ant System) metaheuristic. Parameterized by a problem object implementing the ``ant_colony_problem_protocol`` protocol and by a random number generator algorithm for the ``fast_random`` library. The algorithm minimizes the solution cost defined by the problem. Classic Ant System pheromone update, optional MAX-MIN pheromone bounds, candidate construction by probabilistic state transition, optional elitist reinforcement, progress reporting, and seed control are supported; suitable defaults are used otherwise.',
		parameters is [
			'Problem' - 'Problem object implementing ``ant_colony_problem_protocol``.',
			'RandomAlgorithm' - 'Random number generator algorithm for the ``fast_random`` library (e.g. ``xoshiro128pp``, ``xoshiro256ss``, ``well512a``, ...).'
		],
		remarks is [
			'Algorithm' - 'Ant Colony Optimization is a constructive metaheuristic inspired by the foraging behaviour of real ants. Artificial ants build solutions component by component on a construction graph, guided by pheromone trails and heuristic information. After each iteration the pheromone is evaporated and reinforced according to the quality of the constructed solutions.',
			'Pheromone model' - 'Pheromone is stored on directed edges (From-To). Undirected problems should return symmetric heuristic values; the library treats edges as directed for generality.',
			'State transition' - 'The probability of choosing next node ``j`` from ``i`` is proportional to ``Tau_ij^Alpha * Eta_ij^Beta``. Roulette-wheel selection is used among the yet-unvisited nodes.',
			'Pheromone update' - 'Classic Ant System: every edge evaporates by factor ``(1-Rho)``, then each ant deposits ``Q / Cost`` on the edges of its tour. When ``elite(E)`` is greater than zero the global-best tour receives an extra ``E * Q / BestCost`` deposit. After each update every trail is clamped to the interval ``[tau_min, tau_max]`` (MAX-MIN style bounds).',
			'Candidate generation' - 'Each ant constructs a complete tour by starting at a random node and repeatedly selecting the next unvisited node until the tour is closed.',
			'Progress reporting' - 'If the problem object defines ``progress/5``, it is called periodically with the current iteration, best cost, iteration-best cost, a placeholder acceptance rate, and improvement rate. The reporting interval is controlled by the ``updates(N)`` option. A final report is always produced when the loop terminates.',
			'Best solution tracking' - 'The algorithm tracks the best solution found across all iterations.',
			'Seed control' - 'The ``seed(S)`` option initializes the random number generator for reproducible runs.'
		],
		see_also is [ant_colony(_), ant_colony_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-list, -number), one).
	:- info(run/2, [
		comment is 'Runs the ant colony algorithm using default options and returns the best solution found and its cost.',
		argnames is ['BestSolution', 'BestCost']
	]).

	:- public(run/3).
	:- mode(run(-list, -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the ant colony algorithm using the given options and returns the best solution found and its cost.',
		argnames is ['BestSolution', 'BestCost', 'Options'],
		remarks is [
			'``max_iterations(N)`` option' - 'Maximum number of iterations (default: ``100``).',
			'``ants(N)`` option' - 'Number of ants (solutions constructed) per iteration (default: ``10``).',
			'``alpha(A)`` option' - 'Pheromone importance exponent (default: ``1.0``).',
			'``beta(B)`` option' - 'Heuristic importance exponent (default: ``2.0``).',
			'``rho(R)`` option' - 'Evaporation rate in ``(0,1]`` (default: ``0.5``).',
			'``q(Q)`` option' - 'Pheromone deposit constant (default: ``100.0``).',
			'``elite(E)`` option' - 'Elitist weight: extra deposit factor for the global-best tour (default: ``0``).',
			'``tau0(T)`` option' - 'Initial pheromone level on every edge (default: ``1.0``). Clamped into ``[tau_min, tau_max]`` at initialization.',
			'``tau_min(T)`` option' - 'Lower bound on pheromone trails; must be strictly positive (default: ``1.0e-12``).',
			'``tau_max(T)`` option' - 'Upper bound on pheromone trails; must be strictly positive and at least ``tau_min`` (default: ``1.0e300``).',
			'``updates(N)`` option' - 'Number of progress reports during the run. Set to ``0`` to disable (default: ``0``).',
			'``seed(S)`` option' - 'Positive integer seed for the random number generator, enabling reproducible runs (default: none).'
		]
	]).

	:- public(run/4).
	:- mode(run(-list, -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the ant colony algorithm using the given options, returns the best solution found and its cost, and returns run statistics.',
		argnames is ['BestSolution', 'BestCost', 'Statistics', 'Options'],
		remarks is [
			'Statistics list' - 'A list of ``Key(Value)`` pairs: ``iterations(N)`` is the number of iterations executed, ``solutions(S)`` is the total number of solutions constructed, ``improvements(I)`` is the number of times the global best was improved, and ``final_best_cost(C)`` is the best cost found.'
		]
	]).

	:- uses(_Problem_, [
		nodes/1, heuristic/3, solution_cost/2, stop_condition/3, progress/5
	]).

	:- uses(fast_random(_RandomAlgorithm_), [
		between/3, random/1, randomize/1
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(list, [
		length/2, member/2, select/3, selectchk/3, append/3, nth1/3
	]).

	run(BestSolution, BestCost) :-
		run(BestSolution, BestCost, _Statistics, []).

	run(BestSolution, BestCost, UserOptions) :-
		run(BestSolution, BestCost, _Statistics, UserOptions).

	run(BestSolution, BestCost, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	^^option(seed(Seed), Options) ->
			randomize(Seed)
		;	true
		),
		nodes(Nodes),
		length(Nodes, NumberOfNodes),
		NumberOfNodes > 1,
		^^option(max_iterations(MaxIterations), Options),
		^^option(ants(Ants), Options),
		^^option(alpha(Alpha), Options),
		^^option(beta(Beta), Options),
		^^option(rho(Rho), Options),
		^^option(q(Q), Options),
		^^option(elite(Elite), Options),
		^^option(tau0(Tau0), Options),
		^^option(tau_min(TauMin), Options),
		^^option(tau_max(TauMax), Options),
		^^option(updates(Updates), Options),
		(	TauMin =< TauMax ->
			true
		;	consistency_error(tau_bounds, TauMin, TauMax)
		),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxIterations - 1) // Updates)
		;	UpdateInterval is 0
		),
		% clamp initial pheromone into the allowed interval
		(	Tau0 < TauMin ->
			Tau0Clamped = TauMin
		;	Tau0 > TauMax ->
			Tau0Clamped = TauMax
		;	Tau0Clamped = Tau0
		),
		init_pheromone(Nodes, Tau0Clamped, Pheromone0),
		% construct an initial solution to seed the best
		construct_tour(Nodes, Pheromone0, Alpha, Beta, Tour0),
		solution_cost(Tour0, Cost0),
		Bounds = bounds(TauMin, TauMax),
		loop(
			0, MaxIterations, Ants, Alpha, Beta, Rho, Q, Elite, Bounds, UpdateInterval,
			Nodes, Pheromone0,
			Tour0, Cost0,
			0, 0,
			BestSolution, BestCost,
			FinalIter, FinalSolutions, FinalImproves
		),
		Statistics = [
			iterations(FinalIter),
			solutions(FinalSolutions),
			improvements(FinalImproves),
			final_best_cost(BestCost)
		].

	% main iteration loop

	loop(
		Iteration, MaxIterations, _Ants, _Alpha, _Beta, _Rho, _Q, _Elite, _Bounds, UpdateInterval,
		_Nodes, _Pheromone, BestSolution, BestCost,
		Solutions, Improves, BestSolution, BestCost,
		Iteration, Solutions, Improves
	) :-
		Iteration >= MaxIterations,
		!,
		report_final(Iteration, UpdateInterval, Solutions, Improves, BestCost, BestCost).
	loop(
		Iteration, _MaxIterations, _Ants, _Alpha, _Beta, _Rho, _Q, _Elite, _Bounds, UpdateInterval,
		_Nodes, _Pheromone, BestSolution, BestCost,
		Solutions, Improves, BestSolution, BestCost,
		Iteration, Solutions, Improves
	) :-
		% optional problem-defined stop condition (fails if not defined)
		catch(stop_condition(Iteration, BestCost, BestCost), error(existence_error(_, _), _), fail),
		!,
		report_final(Iteration, UpdateInterval, Solutions, Improves, BestCost, BestCost).
	loop(
		Iteration, MaxIterations, Ants, Alpha, Beta, Rho, Q, Elite, Bounds, UpdateInterval,
		Nodes, Pheromone,
		BestSolution, BestCost,
		Solutions, Improves,
		FinalBest, FinalBestCost,
		FinalIter, FinalSolutions, FinalImproves
	) :-
		% construct Ants solutions
		construct_colony(Ants, Nodes, Pheromone, Alpha, Beta, Tours),
		Solutions1 is Solutions + Ants,
		% evaluate and find iteration best
		evaluate_tours(Tours, EvalTours, IterBestTour, IterationBestCost),
		(	IterationBestCost < BestCost ->
			NewBest = IterBestTour, NewBestCost = IterationBestCost,
			Improves1 is Improves + 1
		;	NewBest = BestSolution, NewBestCost = BestCost,
			Improves1 is Improves
		),
		% pheromone update (evaporation + deposit + optional elite + bounds clamp)
		update_pheromone(Pheromone, Rho, Q, Elite, Bounds, EvalTours, NewBest, NewBestCost, NewPheromone),
		report_progress(Iteration, UpdateInterval, Solutions1, Improves1, NewBestCost, IterationBestCost),
		Iteration1 is Iteration + 1,
		loop(
			Iteration1, MaxIterations, Ants, Alpha, Beta, Rho, Q, Elite, Bounds, UpdateInterval,
			Nodes, NewPheromone,
			NewBest, NewBestCost,
			Solutions1, Improves1,
			FinalBest, FinalBestCost,
			FinalIter, FinalSolutions, FinalImproves
		).

	% construct a colony of Ants tours

	construct_colony(0, _Nodes, _Pheromone, _Alpha, _Beta, []) :-
		!.
	construct_colony(N, Nodes, Pheromone, Alpha, Beta, [Tour| Tours]) :-
		N > 0,
		construct_tour(Nodes, Pheromone, Alpha, Beta, Tour),
		N1 is N - 1,
		construct_colony(N1, Nodes, Pheromone, Alpha, Beta, Tours).

	% build one complete tour by successive probabilistic choices

	construct_tour(Nodes, Pheromone, Alpha, Beta, [Start| Tour]) :-
		length(Nodes, Lenght),
		between(1, Lenght, Index),
		nth1(Index, Nodes, Start),
		selectchk(Start, Nodes, Remaining),
		construct_tour(Remaining, Start, Start, Pheromone, Alpha, Beta, Tour).

	construct_tour([], _Start, _Current, _Pheromone, _Alpha, _Beta, []) :-
		% solution is the sequence of nodes; the cost predicate is responsible
		% for adding the closing edge back to the first node
		!.
	construct_tour(Remaining, Start, Current, Pheromone, Alpha, Beta, [Next| Tour]) :-
		Remaining \== [],
		select_next(Current, Remaining, Pheromone, Alpha, Beta, Next),
		selectchk(Next, Remaining, Remaining1),
		construct_tour(Remaining1, Start, Next, Pheromone, Alpha, Beta, Tour).

	% roulette-wheel selection among remaining nodes

	select_next(Current, Remaining, Pheromone, Alpha, Beta, Next) :-
		scores(Remaining, Current, Pheromone, Alpha, Beta, Scores, Total),
		(	Total =< 0.0 ->
			% fallback: uniform random among remaining
			length(Remaining, Lenght),
			between(1, Lenght, Index),
			nth1(Index, Remaining, Next)
		;	random(R0),
			Threshold is R0 * Total,
			pick_by_score(Scores, Threshold, 0.0, Next)
		).

	scores([], _Current, _Pheromone, _Alpha, _Beta, [], 0.0).
	scores([Node| Nodes], Current, Pheromone, Alpha, Beta, [Node-Score| Scores], Total) :-
		pheromone_lookup(Current, Node, Pheromone, Tau),
		heuristic(Current, Node, Eta),
		Score is (Tau ** Alpha) * (Eta ** Beta),
		scores(Nodes, Current, Pheromone, Alpha, Beta, Scores, Total0),
		Total is Total0 + Score.

	pick_by_score([Node-_Score], _Threshold, _Acc, Node) :-
		!.
	pick_by_score([Node-Score| Rest], Threshold, Acc, Chosen) :-
		Acc1 is Acc + Score,
		(	Acc1 >= Threshold ->
			Chosen = Node
		;	pick_by_score(Rest, Threshold, Acc1, Chosen)
		).

	% evaluate list of tours -> list of Tour-Cost, plus iteration best

	evaluate_tours([], [], none, 1.0e300).
	evaluate_tours([Tour| Tours], [Tour-Cost| Eval], BestTour, BestCost) :-
		solution_cost(Tour, Cost),
		evaluate_tours(Tours, Eval, BestTour0, BestCost0),
		(	Cost < BestCost0 ->
			BestTour = Tour, BestCost = Cost
		;	BestTour = BestTour0, BestCost = BestCost0
		).

	% pheromone: association list of From-To -> Tau (directed)

	init_pheromone(Nodes, Tau0, Pheromone) :-
		findall(From-To-Tau0,
			(	member(From, Nodes),
				member(To, Nodes),
				From \== To
			),
			Pheromone
		).

	pheromone_lookup(From, To, Pheromone, Tau) :-
		(	member(From-To-Tau, Pheromone) ->
			true
		;	Tau = 1.0e-10		% safety floor
		).

	update_pheromone(Pheromone0, Rho, Q, Elite, Bounds, EvalTours, GlobalBestSolution, GlobalBestCost, Pheromone) :-
		Bounds = bounds(TauMin, TauMax),
		% evaporate (floor at tau_min)
		evaporate(Pheromone0, Rho, TauMin, Pheromone1),
		% deposit from all ants
		deposit_all(EvalTours, Q, Pheromone1, Pheromone2),
		% optional elitist deposit
		(	Elite > 0, GlobalBestSolution \== none ->
			Deposit is Elite * Q / GlobalBestCost,
			deposit_tour(GlobalBestSolution, Deposit, Pheromone2, Pheromone3)
		;	Pheromone3 = Pheromone2
		),
		% MAX-MIN clamp into [tau_min, tau_max]
		clamp_pheromone(Pheromone3, TauMin, TauMax, Pheromone).

	evaporate([], _Rho, _TauMin, []).
	evaporate([From-To-Tau| Rest], Rho, TauMin, [From-To-NewTau| Rest1]) :-
		Temp is Tau * (1.0 - Rho),
		(	Temp < TauMin ->
			NewTau = TauMin
		;	NewTau = Temp
		),
		evaporate(Rest, Rho, TauMin, Rest1).

	clamp_pheromone([], _TauMin, _TauMax, []).
	clamp_pheromone([From-To-Tau| Rest], TauMin, TauMax, [From-To-NewTau| Rest1]) :-
		(	Tau < TauMin ->
			NewTau = TauMin
		;	Tau > TauMax ->
			NewTau = TauMax
		;	NewTau = Tau
		),
		clamp_pheromone(Rest, TauMin, TauMax, Rest1).

	deposit_all([], _Q, Pheromone, Pheromone).
	deposit_all([Tour-Cost| Rest], Q, Pheromone0, Pheromone) :-
		Deposit is Q / Cost,
		deposit_tour(Tour, Deposit, Pheromone0, Pheromone1),
		deposit_all(Rest, Q, Pheromone1, Pheromone).

	deposit_tour(Tour, Deposit, Pheromone0, Pheromone) :-
		Tour = [First| _],
		deposit_edges(Tour, First, Deposit, Pheromone0, Pheromone).

	deposit_edges([Last], First, Deposit, Pheromone0, Pheromone) :-
		!,
		add_pheromone(Last, First, Deposit, Pheromone0, Pheromone).
	deposit_edges([A, B| Rest], First, Deposit, Pheromone0, Pheromone) :-
		add_pheromone(A, B, Deposit, Pheromone0, Pheromone1),
		deposit_edges([B| Rest], First, Deposit, Pheromone1, Pheromone).

	add_pheromone(From, To, Deposit, Pheromone0, Pheromone) :-
		(	select(From-To-Tau, Pheromone0, Rest) ->
			Tau1 is Tau + Deposit,
			Pheromone = [From-To-Tau1| Rest]
		;	Pheromone = [From-To-Deposit| Pheromone0]
		).

	% progress reporting (rates are approximate placeholders for API compatibility)

	report_progress(Iteration, UpdateInterval, Solutions, Improves, BestCost, IterationBestCost) :-
		UpdateInterval > 0,
		Iteration > 0,
		Iteration mod UpdateInterval =:= 0,
		!,
		call_progress(Iteration, Solutions, Improves, BestCost, IterationBestCost).
	report_progress(_, _, _, _, _, _).

	report_final(Iteration, UpdateInterval, Solutions, Improves, BestCost, IterationBestCost) :-
		UpdateInterval > 0,
		!,
		call_progress(Iteration, Solutions, Improves, BestCost, IterationBestCost).
	report_final(_, _, _, _, _, _).

	call_progress(Iteration, Solutions, Improves, BestCost, IterationBestCost) :-
		(	Solutions > 0 ->
			% all constructed solutions are "accepted"
			AcceptanceRate is 1.0,
			ImprovementRate is Improves / Solutions
		;	AcceptanceRate is 0.0,
			ImprovementRate is 0.0
		),
		% optional; ignore failure when the problem does not define progress/5
		ignore(progress(Iteration, BestCost, IterationBestCost, AcceptanceRate, ImprovementRate)).

	% default options

	default_option(max_iterations(100)).
	default_option(ants(10)).
	default_option(alpha(1.0)).
	default_option(beta(2.0)).
	default_option(rho(0.5)).
	default_option(q(100.0)).
	default_option(elite(0)).
	default_option(tau0(1.0)).
	default_option(tau_min(1.0e-12)).
	default_option(tau_max(1.0e300)).
	default_option(updates(0)).

	% option validation

	valid_option(max_iterations(N)) :-
		valid(positive_integer, N).
	valid_option(ants(N)) :-
		valid(positive_integer, N).
	valid_option(alpha(A)) :-
		number(A), A >= 0.0.
	valid_option(beta(B)) :-
		number(B), B >= 0.0.
	valid_option(rho(R)) :-
		number(R), R > 0.0, R =< 1.0.
	valid_option(q(Q)) :-
		number(Q), Q > 0.0.
	valid_option(elite(E)) :-
		valid(non_negative_integer, E).
	valid_option(tau0(T)) :-
		number(T), T > 0.0.
	valid_option(tau_min(T)) :-
		number(T), T > 0.0.
	valid_option(tau_max(T)) :-
		number(T), T > 0.0.
	valid_option(updates(N)) :-
		valid(non_negative_integer, N).
	valid_option(seed(S)) :-
		valid(positive_integer, S).

:- end_object.


:- object(ant_colony(_Problem_),
	extends(ant_colony(_Problem_, xoshiro128pp))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-16,
		comment is 'Ant colony optimization algorithm using the Xoshiro128++ random number generator. Convenience object that extends ``ant_colony/2`` with the random algorithm bound to ``xoshiro128pp``.',
		parameters is [
			'Problem' - 'Problem object implementing ``ant_colony_problem_protocol``.'
		],
		see_also is [ant_colony(_, _), ant_colony_problem_protocol]
	]).

:- end_object.
