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


:- category(ode_solver(_System_),
	implements(ode_solver_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Common validation and option handling for non-stiff initial-value ODE solvers.',
		parameters is [
			'System' - 'Object implementing ``ode_system_protocol``.'
		]
	]).

	:- protected(check_initial_value/3).
	:- mode(check_initial_value(+number, +list(number), +number), one_or_error).
	:- info(check_initial_value/3, [
		comment is 'Checks initial and final times and a non-empty numeric initial state.',
		argnames is ['InitialTime', 'InitialState', 'FinalTime'],
		exceptions is [
			'An argument is a variable' - instantiation_error,
			'A time is not a number' - type_error(number, 'Time'),
			'``InitialState`` is not a non-empty numeric list' - domain_error(ode_state, 'InitialState')
		]
	]).

	:- protected(evaluate_derivative/3).
	:- mode(evaluate_derivative(+number, +list(number), -list(number)), one_or_error).
	:- info(evaluate_derivative/3, [
		comment is 'Evaluates and validates a system derivative.',
		argnames is ['Time', 'State', 'Derivative'],
		exceptions is [
			'The system returns a non-numeric derivative or one with the wrong dimension' - domain_error(ode_derivative, 'Derivative')
		]
	]).

	:- uses(_System_, [derivative/3]).
	:- uses(list, [length/2, member/2]).

	solve(InitialTime, InitialState, FinalTime, Trajectory) :-
		::solve(InitialTime, InitialState, FinalTime, Trajectory, _Statistics, []).

	solve(InitialTime, InitialState, FinalTime, Trajectory, UserOptions) :-
		::solve(InitialTime, InitialState, FinalTime, Trajectory, _Statistics, UserOptions).

	check_initial_value(InitialTime, InitialState, FinalTime) :-
		( var(InitialTime) -> instantiation_error; true ),
		( var(InitialState) -> instantiation_error; true ),
		( var(FinalTime) -> instantiation_error; true ),
		( number(InitialTime) -> true; type_error(number, InitialTime) ),
		( number(FinalTime) -> true; type_error(number, FinalTime) ),
		( InitialState = [_|_], ground(InitialState), forall(member(Value, InitialState), number(Value)) ->
			true
		; domain_error(ode_state, InitialState)
		).

	evaluate_derivative(Time, State, Derivative) :-
		derivative(Time, State, Derivative),
		length(State, Dimension),
		( length(Derivative, Dimension), forall(member(Value, Derivative), number(Value)) ->
			true
		; domain_error(ode_derivative, Derivative)
		).

:- end_category.
