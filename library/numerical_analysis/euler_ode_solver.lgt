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


:- object(euler_ode_solver(_System_),
	imports(ode_solver(_System_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Fixed-step explicit Euler solver for non-stiff initial-value problems.',
		parameters is [
			'System' - 'Object implementing ``ode_system_protocol``.'
		],
		see_also is [ode_solver_protocol, rk4_ode_solver(_), rk45_ode_solver(_)]
	]).

	:- uses(linear_algebra, [
		add_scaled_vector/4
	]).

	:- uses(list, [
		reverse/2
	]).

	solve(InitialTime, InitialState, FinalTime, Trajectory, Statistics, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^option(step_size(StepSize), Options),
		^^option(max_steps(MaxSteps), Options),
		^^check_initial_value(InitialTime, InitialState, FinalTime),
		fixed_loop(InitialTime, InitialState, FinalTime, StepSize, MaxSteps, 0, 0, 0.0, [InitialTime-InitialState], Reversed, Steps, Evaluations, LastStep, Converged, Reason),
		reverse(Reversed, Trajectory),
		Statistics = [accepted_steps(Steps), rejected_steps(0), derivative_evaluations(Evaluations), final_step_size(LastStep), converged(Converged), termination_reason(Reason)].

	fixed_loop(Time, _State, FinalTime, _StepSize, _MaxSteps, Steps, Evaluations, LastStep, Trajectory, Trajectory, Steps, Evaluations, LastStep, true, final_time) :-
		Time =:= FinalTime,
		!.
	fixed_loop(_Time, _State, _FinalTime, _StepSize, MaxSteps, Steps, Evaluations, LastStep, Trajectory, Trajectory, Steps, Evaluations, LastStep, false, max_steps) :-
		Steps >= MaxSteps,
		!.
	fixed_loop(Time, State, FinalTime, StepSize, MaxSteps, Steps0, Evaluations0, _LastStep, Trajectory0, Trajectory, Steps, Evaluations, LastStep, Converged, Reason) :-
		signed_step(Time, FinalTime, StepSize, Step, NextTime),
		^^evaluate_derivative(Time, State, Derivative),
		add_scaled_vector(Derivative, Step, State, NextState),
		Steps1 is Steps0 + 1,
		Evaluations1 is Evaluations0 + 1,
		fixed_loop(NextTime, NextState, FinalTime, StepSize, MaxSteps, Steps1, Evaluations1, Step, [NextTime-NextState| Trajectory0], Trajectory, Steps, Evaluations, LastStep, Converged, Reason).

	signed_step(Time, FinalTime, StepSize, Step, NextTime) :-
		Remaining is abs(FinalTime - Time),
		Magnitude is min(StepSize, Remaining),
		(	FinalTime > Time ->
			Step = Magnitude
		;	Step is -Magnitude
		),
		(	Magnitude >= Remaining ->
			NextTime = FinalTime
		;	NextTime is Time + Step
		).

	default_option(step_size(0.01)).
	default_option(max_steps(100000)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(step_size(StepSize)) :-
		number(StepSize), StepSize > 0.0.
	valid_option(max_steps(MaxSteps)) :-
		integer(MaxSteps), MaxSteps > 0.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
