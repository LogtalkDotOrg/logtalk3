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


:- category(local_optimization_solver(_Problem_),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-24,
		comment is 'Common code for local-optimization solvers: option handling, objective-direction helpers, bound projection, vector utilities, progress reporting, and basic validation. Concrete solvers import this category and implement the search loop.',
		parameters is [
			'Problem' - 'Problem object implementing ``local_optimization_problem_protocol``.'
		],
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

	:- protected(initial_point/2).
	:- mode(initial_point(+list(compound), -list(number)), one).
	:- info(initial_point/2, [
		comment is 'Returns the initial point defined by the ``initial_point/1`` option if present. Otherwise returns the problem defined initial point.',
		argnames is ['Options', 'Point']
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

	:- protected(check_bounds/1).
	:- mode(check_bounds(+list(pair)), one_or_error).
	:- info(check_bounds/1, [
		comment is 'Checks that the bounds are valid.',
		argnames is ['Bounds'],
		exceptions is [
			'``Bounds`` is a variable' - instantiation_error,
			'``Bounds`` is neither a variable nor a valid list of bounds' - domain_error(position_bounds, 'Bounds')
		]
	]).

	:- protected(check_point/2).
	:- mode(check_point(+list(number), +list(pair)), one_or_error).
	:- info(check_point/2, [
		comment is 'Checks that the bounds are valid. Assumes ``Point`` is already checked to be a list of numbers.',
		argnames is ['Point', 'Bounds'],
		exceptions is [
			'``Point`` is not a valid point' - domain_error(initial_point, 'Point')
		]
	]).

	:- protected(report_progress/6).
	:- mode(report_progress(+non_negative_integer, +non_negative_integer, +list(number), +number, +number, +non_negative_integer), one).
	:- info(report_progress/6, [
		comment is 'Reports solver progress by calling ``progress_hook/5`` if defined.',
		argnames is ['Iteration', 'UpdateInterval', 'BestPoint', 'BestValue', 'Measure', 'Evaluations']
	]).

	:- protected(report_final/5).
	:- mode(report_final(+non_negative_integer, +non_negative_integer, +list(number), +number, +number), one).
	:- info(report_final/5, [
		comment is 'Reports the final solver solution by calling ``progress_hook/5`` if defined.',
		argnames is ['Iteration', 'UpdateInterval', 'BestPoint', 'BestValue', 'Measure']
	]).

	:- protected(progress_hook/5).
	:- mode(progress_hook(+non_negative_integer, +list(number), +number, +number, +non_negative_integer), zero_or_one).
	:- info(progress_hook/5, [
		comment is 'User-defined hook predicate to report a solver step.',
		argnames is ['Iteration', 'BestPoint', 'BestValue', 'Measure', 'Evaluations']
	]).

	:- uses(list, [
		length/2, member/2
	]).

	% default public entry points (solvers may override)

	run(BestPoint, BestValue) :-
		::run(BestPoint, BestValue, _Statistics, []).

	run(BestPoint, BestValue, UserOptions) :-
		::run(BestPoint, BestValue, _Statistics, UserOptions).

	% run/4 is left undefined here – each solver must implement it.

	initial_point(Options, Point) :-
		(	^^option(initial_point(Point), Options) ->
			true
		;	_Problem_::initial_point(Point)
		).

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

	check_bounds(Bounds) :-
		var(Bounds),
		instantiation_error.
	check_bounds([]) :-
		!.
	check_bounds([Low-High| Bounds]) :-
		number(Low), number(High), Low =< High,
		!,
		check_bounds(Bounds).
	check_bounds(Bounds) :-
		domain_error(position_bounds, Bounds).

	check_point(Point, []) :-
		!,
		(	Point = [_|_],
			ground(Point),
			forall(member(X, Point), number(X)) ->
			true
		;	domain_error(initial_point, Point)
		).
	check_point(Point, Bounds) :-
		length(Point, Length),
		length(Bounds, Length),
		!,
		validate_point_components(Point, Bounds).
	check_point(Point, _Bounds) :-
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

	valid_option(initial_point(Point)) :-
		ground(Point),
		Point = [_|_],
		forall(member(X, Point), number(X)).
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
