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


:- category(local_optimization_solver,
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-17,
		comment is 'Common code for local-optimization solvers: option handling, objective-direction helpers, bound projection, vector utilities, progress reporting, and basic validation. Concrete solvers import this category and implement the search loop.',
		see_also is [local_optimization_problem_protocol]
	]).

	:- public(run/2).
	:- mode(run(-list(number), -number), one).
	:- info(run/2, [
		comment is 'Runs the solver with default options and returns the best point and objective value found.',
		argnames is ['BestPoint', 'BestValue']
	]).

	:- public(run/3).
	:- mode(run(-list(number), -number, +list(compound)), one).
	:- info(run/3, [
		comment is 'Runs the solver with the given options and returns the best point and objective value found.',
		argnames is ['BestPoint', 'BestValue', 'Options']
	]).

	:- public(run/4).
	:- mode(run(-list(number), -number, -list(compound), +list(compound)), one).
	:- info(run/4, [
		comment is 'Runs the solver with the given options and returns the best point, objective value, and run statistics.',
		argnames is ['BestPoint', 'BestValue', 'Statistics', 'Options']
	]).

	:- protected(better_value/3).
	:- mode(better_value(+atom, +number, +number), zero_or_one).
	:- info(better_value/3, [
		comment is 'True when ``Value`` is strictly better than ``Reference`` according to the optimization direction.',
		argnames is ['Objective', 'Value', 'Reference']
	]).

	:- protected(target_reached/3).
	:- mode(target_reached(+atom, +number, +term), zero_or_one).
	:- info(target_reached/3, [
		comment is 'True when the current value has reached or passed the optional target in the selected direction.',
		argnames is ['Objective', 'Value', 'Target']
	]).

	:- protected(project_to_bounds/3).
	:- mode(project_to_bounds(+list(number), +list(pair), -list(number)), one).
	:- info(project_to_bounds/3, [
		comment is 'Clamps each component of a point to the corresponding ``Low-High`` interval. When ``Bounds`` is empty the point is returned unchanged.',
		argnames is ['Point', 'Bounds', 'Projected']
	]).

	:- protected(validate_bounds/1).
	:- mode(validate_bounds(+list(pair)), one_or_error).

	:- protected(validate_point/2).
	:- mode(validate_point(+list(number), +list(pair)), one_or_error).

	:- protected(report_progress/6).
	:- mode(report_progress(+non_negative_integer, +non_negative_integer, +list(number), +number, +number, +non_negative_integer), one).

	:- protected(progress_hook/5).
	progress_hook(_, _, _, _, _).

	:- protected(report_final/5).
	:- mode(report_final(+non_negative_integer, +non_negative_integer, +list(number), +number, +number), one).

	:- uses(list, [
		member/2
	]).

	% default public entry points (solvers may override)

	run(BestPoint, BestValue) :-
		::run(BestPoint, BestValue, _Statistics, []).

	run(BestPoint, BestValue, UserOptions) :-
		::run(BestPoint, BestValue, _Statistics, UserOptions).

	% run/4 is left undefined here – each solver must implement it.

	% objective-direction predicates

	better_value(minimize, Value, Reference) :-
		Value < Reference.
	better_value(maximize, Value, Reference) :-
		Value > Reference.

	target_reached(minimize, Value, Target) :-
		number(Target),
		Value =< Target.
	target_reached(maximize, Value, Target) :-
		number(Target),
		Value >= Target.

	% bound handling

	project_to_bounds(Point, [], Point) :-
		!.
	project_to_bounds([], [], []) :-
		!.
	project_to_bounds([X| Xs], [Low-High| Bounds], [Y| Ys]) :-
		(	X < Low  -> Y = Low
		;	X > High -> Y = High
		;	Y = X
		),
		project_to_bounds(Xs, Bounds, Ys).

	validate_bounds([]) :-
		!.
	validate_bounds([Low-High| Bounds]) :-
		number(Low), number(High), Low =< High,
		!,
		validate_bounds(Bounds).
	validate_bounds(Bounds) :-
		domain_error(position_bounds, Bounds).

	validate_point(Point, []) :-
		!,
		(	Point = [_|_],
			ground(Point),
			forall(member(X, Point), number(X)) ->
			true
		;	domain_error(initial_point, Point)
		).
	validate_point(Point, Bounds) :-
		length(Point, D),
		length(Bounds, D),
		!,
		validate_point_components(Point, Bounds).
	validate_point(Point, _Bounds) :-
		domain_error(initial_point, Point).

	validate_point_components([], []) :-
		!.
	validate_point_components([X| Xs], [Low-High| Bounds]) :-
		number(X), X >= Low, X =< High,
		!,
		validate_point_components(Xs, Bounds).
	validate_point_components(Point, _) :-
		domain_error(initial_point, Point).

	% progress reporting predicates

	report_progress(Iteration, UpdateInterval, BestPoint, BestValue, Measure, Evaluations) :-
		UpdateInterval > 0,
		Iteration > 0,
		Iteration mod UpdateInterval =:= 0,
		!,
		ignore(::progress_hook(Iteration, BestPoint, BestValue, Measure, Evaluations)).
	report_progress(_, _, _, _, _, _).

	report_final(Iteration, UpdateInterval, BestPoint, BestValue, Measure) :-
		UpdateInterval > 0,
		!,
		ignore(::progress_hook(Iteration, BestPoint, BestValue, Measure, _)).
	report_final(_, _, _, _, _).

	% shared option defaults and validation

	default_option(objective(minimize)).
	default_option(target_value(none)).
	default_option(max_iterations(1000)).
	default_option(tol_x(1.0e-8)).
	default_option(tol_f(1.0e-8)).
	default_option(tol_g(1.0e-6)).
	default_option(updates(0)).

	valid_option(objective(Objective)) :-
		once((Objective == minimize ; Objective == maximize)).
	valid_option(target_value(Target)) :-
		once((Target == none ; number(Target))).
	valid_option(max_iterations(N)) :-
		type::valid(positive_integer, N).
	valid_option(tol_x(T)) :-
		number(T), T >= 0.0.
	valid_option(tol_f(T)) :-
		number(T), T >= 0.0.
	valid_option(tol_g(T)) :-
		number(T), T >= 0.0.
	valid_option(updates(N)) :-
		type::valid(non_negative_integer, N).
	valid_option(seed(Seed)) :-
		type::valid(positive_integer, Seed).

:- end_category.
