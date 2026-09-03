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


:- object(nelder_mead(_Problem_),
	imports(local_optimization_solver(_Problem_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Nelder-Mead (downhill simplex) derivative-free local optimizer for continuous problems. Supports optional box constraints via projection, minimization and maximization, and the standard reflection / expansion / contraction / shrink operators.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol``.'
		],
		remarks is [
			'Algorithm' - 'Classic Nelder-Mead simplex method. An optional adaptive-parameter variant (Gao-Han) can be selected with the ``adaptive(true)`` option.',
			'Bounds' - 'When the problem defines ``position_bounds/1``, trial points are projected onto the box after every operator.',
			'Initial simplex' - 'Built from the problem ``initial_point/1`` by stepping along each coordinate axis. The step size is controlled by the ``initial_step(S)`` option (default 0.05 of each finite range, or an absolute 0.05 when unbounded).'
		],
		see_also is [local_optimization_problem_protocol, local_optimization_solver(_), gradient_descent(_)]
	]).

	:- uses(_Problem_, [
		objective/2, position_bounds/1, stop_condition/3, progress/5
	]).

	:- uses(linear_algebra, [
		add_vectors/3, euclidean_norm/2, new_vector/3, scale_vector/3, subtract_vectors/3
	]).

	:- uses(list, [
		append/3, last/2, length/2, nth1/3, reverse/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	% public entry point

	run(BestPoint, BestValue, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(objective(ObjDir), Options),
		^^option(target_value(Target), Options),
		^^option(max_iterations(MaxIterations), Options),
		^^option(tol_x(TolX), Options),
		^^option(tol_f(TolF), Options),
		^^option(updates(Updates), Options),
		^^option(reflection(Alpha), Options),
		^^option(expansion(Gamma), Options),
		^^option(contraction(Rho), Options),
		^^option(shrink(Sigma), Options),
		^^option(adaptive(Adaptive), Options),
		^^option(initial_step(InitialStep), Options),
		(	Updates > 0 ->
			UpdateInterval is max(1, (MaxIterations - 1) // Updates)
		;	UpdateInterval = 0
		),
		^^initial_point(Options, Point0),
		(	position_bounds(Bounds) ->
			^^check_bounds(Bounds),
			^^check_point(Point0, Bounds)
		;	Bounds = [],
			^^check_point(Point0, [])
		),
		length(Point0, Dimension),
		Dimension >= 1,
		nelder_mead_coefficients(
			Adaptive, Dimension, Alpha, Gamma, Rho, Sigma,
			EffectiveAlpha, EffectiveGamma, EffectiveRho, EffectiveSigma
		),
		build_simplex(Point0, Dimension, Bounds, InitialStep, Simplex0),
		evaluate_simplex(Simplex0, Evaluated0, 0, Evals0),
		order_simplex(ObjDir, Evaluated0, Ordered0),
		Ordered0 = [Best0-Val0| _],
		loop(
			0, MaxIterations, UpdateInterval, Dimension, Bounds, ObjDir, Target,
			EffectiveAlpha, EffectiveGamma, EffectiveRho, EffectiveSigma, Adaptive, TolX, TolF,
			Ordered0, Best0, Val0, Evals0,
			FinalSimplex, BestPoint, BestValue, Iterations, Evaluations
		),
		simplex_size(FinalSimplex, SimplexSize),
		Statistics = [
			iterations(Iterations),
			evaluations(Evaluations),
			final_simplex_size(SimplexSize),
			final_value(BestValue)
		].

	% main loop

	loop(
		Iter, MaxIterations, UpdInt, _Dim, _Bounds, _ObjDir, _Target,
		_Alpha, _Gamma, _Rho, _Sigma, _Adaptive, _TolX, _TolF,
		Simplex, Best, Val, Evals,
		Simplex, Best, Val, Iter, Evals
	) :-
		Iter >= MaxIterations,
		!,
		^^report_final(Iter, UpdInt, Best, Val, 0.0).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, ObjDir, Target,
		_Alpha, _Gamma, _Rho, _Sigma, _Adaptive, _TolX, _TolF,
		Simplex, Best, Val, Evals,
		Simplex, Best, Val, Iter, Evals
	) :-
		^^target_reached(ObjDir, Val, Target),
		!,
		^^report_final(Iter, UpdInt, Best, Val, 0.0).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _ObjDir, _Target,
		_Alpha, _Gamma, _Rho, _Sigma, _Adaptive, _TolX, _TolF,
		Simplex, Best, Val, Evals,
		Simplex, Best, Val, Iter, Evals
	) :-
		stop_condition(Iter, Best, Val),
		!,
		^^report_final(Iter, UpdInt, Best, Val, 0.0).

	loop(
		Iter, _MaxIterations, UpdInt, _Dim, _Bounds, _ObjDir, _Target,
		_Alpha, _Gamma, _Rho, _Sigma, _Adaptive, TolX, TolF,
		Simplex, Best, Val, Evals,
		Simplex, Best, Val, Iter, Evals
	) :-
		converged(Simplex, TolX, TolF),
		!,
		^^report_final(Iter, UpdInt, Best, Val, 0.0).

	loop(
		Iter, MaxIterations, UpdInt, Dim, Bounds, ObjDir, Target,
		Alpha, Gamma, Rho, Sigma, Adaptive, TolX, TolF,
		Simplex0, _Best0, _Val0, Evals0,
		FinalSimplex, Best, Val, Iterations, Evaluations
	) :-
		nm_step(
			Simplex0, Dim, Bounds, ObjDir,
			Alpha, Gamma, Rho, Sigma, Adaptive,
			Simplex1, Evals0, Evals1
		),
		order_simplex(ObjDir, Simplex1, Ordered),
		Ordered = [Best1-Val1| _],
		Iter1 is Iter + 1,
		simplex_size(Ordered, Size),
		^^report_progress(Iter1, UpdInt, Best1, Val1, Size, Evals1),
		loop(
			Iter1, MaxIterations, UpdInt, Dim, Bounds, ObjDir, Target,
			Alpha, Gamma, Rho, Sigma, Adaptive, TolX, TolF,
			Ordered, Best1, Val1, Evals1,
			FinalSimplex, Best, Val, Iterations, Evaluations
		).

	% one Nelder-Mead iteration

	nm_step(Simplex, Dim, Bounds, ObjDir, Alpha, Gamma, Rho, Sigma, _Adaptive, NewSimplex, Evals0, Evals) :-
		Simplex = [Best-BestVal| Rest],
		once(append(MiddlePairs, [Worst-_WorstVal], Rest)),
		keys(MiddlePairs, MiddlePoints),
		centroid([Best| MiddlePoints], Dim, Centroid0),
		^^project_to_bounds(Centroid0, Bounds, Centroid),
		reflect(Worst, Centroid, Alpha, Reflected0),
		^^project_to_bounds(Reflected0, Bounds, Reflected),
		evaluate_objective(Reflected, RefVal),
		Evals1 is Evals0 + 1,
		(	^^better_value(ObjDir, RefVal, BestVal) ->
			expand(Reflected, Centroid, Gamma, Expanded0),
			^^project_to_bounds(Expanded0, Bounds, Expanded),
			evaluate_objective(Expanded, ExpVal),
			Evals2 is Evals1 + 1,
			(	^^better_value(ObjDir, ExpVal, RefVal) ->
				replace_worst(Simplex, Expanded-ExpVal, NewSimplex)
			;	replace_worst(Simplex, Reflected-RefVal, NewSimplex)
			),
			Evals = Evals2
		;	( 	MiddlePairs == [] ->
				SecondWorstVal = BestVal
			;	last(MiddlePairs, _-SecondWorstVal)
			),
			^^better_value(ObjDir, RefVal, SecondWorstVal) ->
			replace_worst(Simplex, Reflected-RefVal, NewSimplex),
			Evals = Evals1
		;	(	^^better_value(ObjDir, RefVal, _WorstVal) ->
				contract_outside(Reflected, Centroid, Rho, Contracted0),
				ContractReference = RefVal
			;	contract_inside(Worst, Centroid, Rho, Contracted0),
				ContractReference = _WorstVal
			),
			^^project_to_bounds(Contracted0, Bounds, Contracted),
			evaluate_objective(Contracted, ConVal),
			Evals2 is Evals1 + 1,
			( 	^^better_value(ObjDir, ConVal, ContractReference) ->
				replace_worst(Simplex, Contracted-ConVal, NewSimplex),
				Evals = Evals2
			;	shrink_simplex(Simplex, Best, Sigma, Bounds, Shrunk0),
				evaluate_pairs(Shrunk0, Shrunk, Evals2, Evals),
				NewSimplex = [Best-BestVal| Shrunk]
			)
		).

	% simplex operators

	nelder_mead_coefficients(false, _Dimension, Alpha, Gamma, Rho, Sigma, Alpha, Gamma, Rho, Sigma).
	nelder_mead_coefficients(true, Dimension, _Alpha, _Gamma, _Rho, _Sigma, 1.0, Gamma, Rho, Sigma) :-
		Gamma is 1.0 + 2.0 / Dimension,
		Rho is 0.75 - 1.0 / (2.0 * Dimension),
		Sigma is 1.0 - 1.0 / Dimension.

	reflect(Worst, Centroid, Alpha, Reflected) :-
		subtract_vectors(Centroid, Worst, Diff),
		scale_vector(Diff, Alpha, Scaled),
		add_vectors(Centroid, Scaled, Reflected).

	expand(Reflected, Centroid, Gamma, Expanded) :-
		subtract_vectors(Reflected, Centroid, Diff),
		scale_vector(Diff, Gamma, Scaled),
		add_vectors(Centroid, Scaled, Expanded).

	contract_outside(Reflected, Centroid, Rho, Contracted) :-
		subtract_vectors(Reflected, Centroid, Diff),
		scale_vector(Diff, Rho, Scaled),
		add_vectors(Centroid, Scaled, Contracted).

	contract_inside(Worst, Centroid, Rho, Contracted) :-
		subtract_vectors(Worst, Centroid, Diff),
		scale_vector(Diff, Rho, Scaled),
		add_vectors(Centroid, Scaled, Contracted).

	shrink_simplex([Best-_| Rest], Best, Sigma, Bounds, Shrunk) :-
		shrink_vertices(Rest, Best, Sigma, Bounds, Shrunk).

	shrink_vertices([], _, _, _, []).
	shrink_vertices([V-_| Vs], Best, Sigma, Bounds, [NewV| NewVs]) :-
		subtract_vectors(V, Best, Diff),
		scale_vector(Diff, Sigma, Scaled),
		add_vectors(Best, Scaled, NewV0),
		^^project_to_bounds(NewV0, Bounds, NewV),
		shrink_vertices(Vs, Best, Sigma, Bounds, NewVs).

	% simplex construction & helpers

	build_simplex(Point, Dimension, Bounds, InitialStep, Simplex) :-
		build_vertices(1, Dimension, Point, Bounds, InitialStep, [Point], Vertices0),
		reverse(Vertices0, Simplex).

	build_vertices(I, Dim, _, _, _, Acc, Acc) :-
		I > Dim,
		!.
	build_vertices(I, Dim, Point, Bounds, Step, Acc, Vertices) :-
		perturb(Point, I, Bounds, Step, Perturbed),
		I1 is I + 1,
		build_vertices(I1, Dim, Point, Bounds, Step, [Perturbed| Acc], Vertices).

	perturb(Point, I, Bounds, Step, Perturbed) :-
		(	Bounds == [] ->
			Delta = Step
		;	nth1(I, Bounds, Low-High),
			Range is High - Low,
			(	Range > 0.0 ->
				Delta is Step * Range
			;	Delta = Step
			)
		),
		nth1(I, Point, Xi),
		( 	Bounds == [] ->
			NewXi is Xi + Delta
		;	Candidate is Xi + Delta,
			( 	Candidate =< High ->
				NewXi = Candidate
			;	NewXi is Xi - Delta
			)
		),
		replace_nth1(I, Point, NewXi, Perturbed0),
		^^project_to_bounds(Perturbed0, Bounds, Perturbed).

	evaluate_simplex([], [], Evals, Evals).
	evaluate_simplex([P| Ps], [P-V| Pairs], Evals0, Evals) :-
		evaluate_objective(P, V),
		Evals1 is Evals0 + 1,
		evaluate_simplex(Ps, Pairs, Evals1, Evals).

	evaluate_pairs([], [], Evals, Evals).
	evaluate_pairs([P| Ps], [P-V| Pairs], Evals0, Evals) :-
		evaluate_objective(P, V),
		Evals1 is Evals0 + 1,
		evaluate_pairs(Ps, Pairs, Evals1, Evals).

	evaluate_objective(Point, Value) :-
		objective(Point, Value),
		( 	number(Value) ->
			true
		;	domain_error(objective, Value)
		).

	order_simplex(Objective, Pairs, Ordered) :-
		objective_keys(Objective, Pairs, Keyed),
		keysort(Keyed, Sorted),
		strip_objective_keys(Sorted, Ordered).

	objective_keys(_, [], []) :-
		!.
	objective_keys(minimize, [Point-Value| Pairs], [Value-(Point-Value)| Keyed]) :-
		objective_keys(minimize, Pairs, Keyed).
	objective_keys(maximize, [Point-Value| Pairs], [Key-(Point-Value)| Keyed]) :-
		Key is -Value,
		objective_keys(maximize, Pairs, Keyed).

	strip_objective_keys([], []) :-
		!.
	strip_objective_keys([_-Pair| Keyed], [Pair| Pairs]) :-
		strip_objective_keys(Keyed, Pairs).

	replace_worst(Simplex, NewPair, NewSimplex) :-
		append(Front, [_], Simplex),
		append(Front, [NewPair], NewSimplex),
		!.

	centroid(Points, Dim, Centroid) :-
		length(Points, N),
		new_vector(Dim, 0.0, Zeros),
		sum_points(Points, Zeros, Sum),
		Scale is 1.0 / N,
		scale_vector(Sum, Scale, Centroid).

	sum_points([], Sum, Sum).
	sum_points([Point| Points], Sum0, Sum) :-
		add_vectors(Point, Sum0, Sum1),
		sum_points(Points, Sum1, Sum).

	simplex_size([Best-_| Rest], Size) :-
		max_distance(Rest, Best, 0.0, Size).

	max_distance([], _, Acc, Acc).
	max_distance([P-_| Rest], Best, Acc0, Acc) :-
		subtract_vectors(P, Best, Diff),
		euclidean_norm(Diff, Norm),
		Acc1 is max(Acc0, Norm),
		max_distance(Rest, Best, Acc1, Acc).

	converged(Simplex, TolX, TolF) :-
		simplex_size(Simplex, Size),
		Size =< TolX,
		values_span(Simplex, Span),
		Span =< TolF.

	values_span([_-V0| Rest], Span) :-
		values_minmax(Rest, V0, V0, Min, Max),
		Span is Max - Min.

	values_minmax([], Min, Max, Min, Max).
	values_minmax([_-V| Rest], Min0, Max0, Min, Max) :-
		Min1 is min(Min0, V),
		Max1 is max(Max0, V),
		values_minmax(Rest, Min1, Max1, Min, Max).

	replace_nth1(1, [_| T], V, [V| T]) :-
		!.
	replace_nth1(N, [H| T], V, [H| T2]) :-
		N > 1,
		N1 is N - 1,
		replace_nth1(N1, T, V, T2).

	% progress hook

	progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations) :-
		ignore(progress(Iteration, BestPoint, BestValue, Measure, Evaluations)).

	% options specific to Nelder-Mead

	default_option(reflection(1.0)).
	default_option(expansion(2.0)).
	default_option(contraction(0.5)).
	default_option(shrink(0.5)).
	default_option(adaptive(false)).
	default_option(initial_step(0.05)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(reflection(Alpha)) :-
		number(Alpha), Alpha > 0.0.
	valid_option(expansion(Gamma)) :-
		number(Gamma), Gamma > 1.0.
	valid_option(contraction(Rho)) :-
		number(Rho), Rho > 0.0, Rho < 1.0.
	valid_option(shrink(Sigma)) :-
		number(Sigma), Sigma > 0.0, Sigma < 1.0.
	valid_option(adaptive(Adaptive)) :-
		once((Adaptive == true ; Adaptive == false)).
	valid_option(initial_step(InitialStep)) :-
		number(InitialStep), InitialStep > 0.0.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
