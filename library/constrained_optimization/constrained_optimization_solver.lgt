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


:- category(constrained_optimization_solver).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Shared numeric, objective-direction, and validation predicates for constrained optimization solvers and their internal problem wrappers.'
	]).

	:- uses(list, [
		length/2
	]).

	% infinity norm: max_i |Vec_i|, 0.0 for an empty (or absent, i.e.
	% not-defined-by-the-problem) vector

	:- protected(abs_max/2).
	:- mode(abs_max(+list(number), -number), one).
	:- info(abs_max/2, [
		comment is 'Infinity norm: max_i abs(Vec_i), 0.0 for an empty vector.',
		argnames is ['Vec', 'Max']
	]).

	abs_max(Vec, Max) :-
		abs_max_acc(Vec, 0.0, Max).

	abs_max_acc([], Max, Max).
	abs_max_acc([V| Vs], Max0, Max) :-
		Max1 is max(Max0, abs(V)),
		abs_max_acc(Vs, Max1, Max).

	% "positive part" infinity norm: max_i max(0, Vec_i), 0.0 for an
	% empty vector - the natural violation measure for an inequality
	% h_i(x) =< 0, where only positive values represent a violation

	:- protected(positive_max/2).
	:- mode(positive_max(+list(number), -number), one).
	:- info(positive_max/2, [
		comment is 'Infinity norm of the positive part: max_i max(0, Vec_i), 0.0 for an empty vector.',
		argnames is ['Vec', 'Max']
	]).

	positive_max(Vec, Max) :-
		positive_max_acc(Vec, 0.0, Max).

	positive_max_acc([], Max, Max).
	positive_max_acc([V| Vs], Max0, Max) :-
		Max1 is max(Max0, V),
		positive_max_acc(Vs, Max1, Max).

	:- protected(direction_sign/2).
	:- mode(direction_sign(+atom, -number), one).
	:- info(direction_sign/2, [
		comment is 'Converts an objective direction into its numeric sign.',
		argnames is ['Direction', 'Sign']
	]).

	direction_sign(minimize, 1.0).
	direction_sign(maximize, -1.0).

	:- protected(objective_direction/2).
	:- mode(objective_direction(+number, -atom), one).
	:- info(objective_direction/2, [
		comment is 'Converts an objective numeric sign into its direction.',
		argnames is ['Sign', 'Direction']
	]).

	objective_direction(1.0, minimize).
	objective_direction(-1.0, maximize).

	:- protected(build_inner/3).
	:- mode(build_inner(+atom, +nonvar, -nonvar), one).
	:- info(build_inner/3, [
		comment is 'Constructs a parametric inner solver object from its class atom and problem object.',
		argnames is ['InnerSolver', 'Problem', 'Solver']
	]).

	build_inner(InnerSolver, Problem, Solver) :-
		Solver =.. [InnerSolver, Problem].

	:- protected(update_penalty/6).
	:- mode(update_penalty(+number, +number, +number, +number, +number, -number), one).
	:- info(update_penalty/6, [
		comment is 'Scales a penalty when the constraint violation does not decrease by the required factor.',
		argnames is ['OldViolation', 'NewViolation', 'RequiredDecrease', 'OldPenalty', 'Scale', 'NewPenalty']
	]).

	update_penalty(OldViolation, NewViolation, RequiredDecrease, OldPenalty, Scale, NewPenalty) :-
		(	NewViolation > RequiredDecrease * OldViolation ->
			NewPenalty is OldPenalty * Scale
		;	NewPenalty = OldPenalty
		).

	:- protected(split_at/4).
	:- mode(split_at(+integer, +list, -list, -list), one).
	:- info(split_at/4, [
		comment is 'Splits a list after its first N elements.',
		argnames is ['N', 'List', 'Prefix', 'Suffix']
	]).

	split_at(N, List, Prefix, Suffix) :-
		(	N =< 0 ->
			Prefix = [],
			Suffix = List
		;	List = [Element| Elements],
			Prefix = [Element| PrefixElements],
			N1 is N - 1,
			split_at(N1, Elements, PrefixElements, Suffix)
		).

	:- protected(abs_sum/2).
	:- mode(abs_sum(+list(number), -number), one).
	:- info(abs_sum/2, [
		comment is 'Computes the sum of the absolute values of a numeric list.',
		argnames is ['Values', 'Sum']
	]).

	abs_sum(Values, Sum) :-
		abs_sum_acc(Values, 0.0, Sum).

	abs_sum_acc([], Sum, Sum).
	abs_sum_acc([Value| Values], Sum0, Sum) :-
		Sum1 is Sum0 + abs(Value),
		abs_sum_acc(Values, Sum1, Sum).

	:- protected(validate_numeric_vector/2).
	:- mode(validate_numeric_vector(+list, +atom), one_or_error).
	:- info(validate_numeric_vector/2, [
		comment is 'Checks that a vector is a list of numbers.',
		argnames is ['Vector', 'Domain'],
		exceptions is [
			'``Vector`` is not a numeric vector' - domain_error('Domain', 'Vector')
		]
	]).

	validate_numeric_vector(Vector, Domain) :-
		(	numeric_vector(Vector) ->
			true
		;	domain_error(Domain, Vector)
		).

	numeric_vector([]).
	numeric_vector([Value| Values]) :-
		number(Value),
		numeric_vector(Values).

	:- protected(validate_constraint_data/4).
	:- mode(validate_constraint_data(+list(number), +list, +list, +atom), one_or_error).
	:- info(validate_constraint_data/4, [
		comment is 'Checks that constraint values are numeric and that the Jacobian has one numeric row per constraint and one column per point component.',
		argnames is ['Point', 'Values', 'Jacobian', 'Domain'],
		exceptions is [
			'``Values`` is not a numeric vector' - domain_error('Domain', 'Values'),
			'``Jacobian`` does not have one numeric row per constraint and one column per point component' - domain_error('Domain', 'Jacobian')
		]
	]).

	validate_constraint_data(Point, Values, Jacobian, Domain) :-
		validate_numeric_vector(Values, Domain),
		length(Point, Columns),
		length(Values, Rows),
		(	length(Jacobian, Rows) ->
			true
		;	domain_error(Domain, Jacobian)
		),
		validate_jacobian_rows(Jacobian, Columns, Domain).

	validate_jacobian_rows([], _Columns, _Domain).
	validate_jacobian_rows([Row| Rows], Columns, Domain) :-
		(	length(Row, Columns) ->
			true
		;	domain_error(Domain, [Row| Rows])
		),
		validate_numeric_vector(Row, Domain),
		validate_jacobian_rows(Rows, Columns, Domain).

:- end_category.
