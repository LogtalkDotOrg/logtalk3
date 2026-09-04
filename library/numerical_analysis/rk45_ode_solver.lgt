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


:- object(rk45_ode_solver(_System_),
	imports(ode_solver(_System_))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Adaptive Dormand-Prince 5(4) solver with componentwise error scaling and FSAL derivative reuse.',
		parameters is [
			'System' - 'Object implementing ``ode_system_protocol``.'
		],
		see_also is [ode_solver_protocol, euler_ode_solver(_), rk4_ode_solver(_)]
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
		^^option(initial_step(InitialStep), Options),
		^^option(min_step(MinStep), Options),
		^^option(max_step(MaxStep), Options),
		^^option(tol_abs(TolAbs), Options),
		^^option(tol_rel(TolRel), Options),
		^^option(safety_factor(Safety), Options),
		^^option(max_steps(MaxSteps), Options),
		( MinStep =< MaxStep -> true; domain_error(step_interval, MinStep-MaxStep) ),
		^^check_initial_value(InitialTime, InitialState, FinalTime),
		(	InitialTime =:= FinalTime ->
			Trajectory = [InitialTime-InitialState],
			Accepted = 0, Rejected = 0, Evaluations = 0, LastStep = 0.0,
			Converged = true, Reason = final_time
		;	Direction is sign(FinalTime - InitialTime),
			StepMagnitude is min(MaxStep, max(MinStep, InitialStep)),
			^^evaluate_derivative(InitialTime, InitialState, InitialDerivative),
			adaptive_loop(InitialTime, InitialState, FinalTime, Direction, StepMagnitude, MinStep, MaxStep, TolAbs, TolRel, Safety, MaxSteps, 0, 0, 0, 1, InitialDerivative, 0.0, [InitialTime-InitialState], Reversed, Accepted, Rejected, Evaluations, LastStep, Converged, Reason),
			reverse(Reversed, Trajectory)
		),
		Statistics = [accepted_steps(Accepted), rejected_steps(Rejected), derivative_evaluations(Evaluations), final_step_size(LastStep), converged(Converged), termination_reason(Reason)].

	adaptive_loop(Time, _State, FinalTime, _Direction, _StepMagnitude, _MinStep, _MaxStep, _TolAbs, _TolRel, _Safety, _MaxSteps, _Attempts, Accepted, Rejected, Evaluations, _Derivative, LastStep, Trajectory, Trajectory, Accepted, Rejected, Evaluations, LastStep, true, final_time) :-
		Time =:= FinalTime,
		!.
	adaptive_loop(_Time, _State, _FinalTime, _Direction, _StepMagnitude, _MinStep, _MaxStep, _TolAbs, _TolRel, _Safety, MaxSteps, Attempts, Accepted, Rejected, Evaluations, _Derivative, LastStep, Trajectory, Trajectory, Accepted, Rejected, Evaluations, LastStep, false, max_steps) :-
		Attempts >= MaxSteps,
		!.
	adaptive_loop(Time, State, FinalTime, Direction, StepMagnitude, MinStep, MaxStep, TolAbs, TolRel, Safety, MaxSteps, Attempts0, Accepted0, Rejected0, Evaluations0, K1, _LastStep, Trajectory0, Trajectory, Accepted, Rejected, Evaluations, LastStep, Converged, Reason) :-
		Remaining is abs(FinalTime - Time),
		UsedMagnitude is min(StepMagnitude, Remaining),
		Step is Direction * UsedMagnitude,
		rk45_step(Time, State, Step, K1, NextState, NextDerivative, ErrorNorm, TolAbs, TolRel),
		Attempts1 is Attempts0 + 1,
		Evaluations1 is Evaluations0 + 6,
		step_factor(ErrorNorm, Safety, Factor),
		ProposedMagnitude is min(MaxStep, max(MinStep, UsedMagnitude * Factor)),
		(	ErrorNorm =< 1.0 ->
			(	UsedMagnitude >= Remaining ->
				NextTime = FinalTime
			;	NextTime is Time + Step
			),
			Accepted1 is Accepted0 + 1,
			adaptive_loop(NextTime, NextState, FinalTime, Direction, ProposedMagnitude, MinStep, MaxStep, TolAbs, TolRel, Safety, MaxSteps, Attempts1, Accepted1, Rejected0, Evaluations1, NextDerivative, Step, [NextTime-NextState| Trajectory0], Trajectory, Accepted, Rejected, Evaluations, LastStep, Converged, Reason)
		;	Rejected1 is Rejected0 + 1,
			(	UsedMagnitude =< MinStep ->
				Trajectory = Trajectory0, Accepted = Accepted0, Rejected = Rejected1,
				Evaluations = Evaluations1, LastStep = Step,
				Converged = false, Reason = min_step
			;	adaptive_loop(Time, State, FinalTime, Direction, ProposedMagnitude, MinStep, MaxStep, TolAbs, TolRel, Safety, MaxSteps, Attempts1, Accepted0, Rejected1, Evaluations1, K1, Step, Trajectory0, Trajectory, Accepted, Rejected, Evaluations, LastStep, Converged, Reason)
			)
		).

	rk45_step(Time, State, Step, K1, FifthOrder, K7, ErrorNorm, TolAbs, TolRel) :-
		stage(State, Step, [0.2-K1], State2),
		Time2 is Time + Step * 0.2,
		^^evaluate_derivative(Time2, State2, K2),
		stage(State, Step, [0.075-K1, 0.225-K2], State3),
		Time3 is Time + Step * 0.3,
		^^evaluate_derivative(Time3, State3, K3),
		stage(State, Step, [0.9777777777777777-K1, -3.7333333333333334-K2, 3.5555555555555554-K3], State4),
		Time4 is Time + Step * 0.8,
		^^evaluate_derivative(Time4, State4, K4),
		stage(State, Step, [2.9525986892242035-K1, -11.595793324188385-K2, 9.822892851699436-K3, -0.2908093278463649-K4], State5),
		Time5 is Time + Step * 0.8888888888888888,
		^^evaluate_derivative(Time5, State5, K5),
		stage(State, Step, [2.8462752525252526-K1, -10.757575757575758-K2, 8.906422717743473-K3, 0.2784090909090909-K4, -0.2735313036020583-K5], State6),
		Time6 is Time + Step,
		^^evaluate_derivative(Time6, State6, K6),
		stage(State, Step, [0.09114583333333333-K1, 0.44923629829290207-K3, 0.6510416666666666-K4, -0.322376179245283-K5, 0.13095238095238096-K6], FifthOrder),
		^^evaluate_derivative(Time6, FifthOrder, K7),
		stage(State, Step, [0.08991319444444444-K1, 0.4534890685834082-K3, 0.6140625-K4, -0.2715123820754717-K5, 0.08904761904761904-K6, 0.025-K7], FourthOrder),
		error_norm(State, FifthOrder, FourthOrder, TolAbs, TolRel, 0.0, ErrorNorm).

	stage(State, _Step, [], State) :-
		!.
	stage(State0, Step, [Coefficient-Derivative| Stages], State) :-
		Scale is Step * Coefficient,
		add_scaled_vector(Derivative, Scale, State0, State1),
		stage(State1, Step, Stages, State).

	error_norm([], [], [], _TolAbs, _TolRel, Error, Error).
	error_norm([Old| Olds], [Fifth| Fifths], [Fourth| Fourths], TolAbs, TolRel, Error0, Error) :-
		Scale is TolAbs + TolRel * max(abs(Old), abs(Fifth)),
		ComponentError is abs(Fifth - Fourth) / Scale,
		Error1 is max(Error0, ComponentError),
		error_norm(Olds, Fifths, Fourths, TolAbs, TolRel, Error1, Error).

	step_factor(Error, _Safety, 5.0) :-
		Error =< 0.0,
		!.
	step_factor(Error, Safety, Factor) :-
		Raw is Safety * (1.0 / Error) ** 0.2,
		Factor is min(5.0, max(0.2, Raw)).

	default_option(initial_step(0.01)).
	default_option(min_step(1.0e-12)).
	default_option(max_step(1.0)).
	default_option(tol_abs(1.0e-8)).
	default_option(tol_rel(1.0e-6)).
	default_option(safety_factor(0.9)).
	default_option(max_steps(100000)).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(initial_step(Step)) :-
		number(Step), Step > 0.0.
	valid_option(min_step(Step)) :-
		number(Step), Step > 0.0.
	valid_option(max_step(Step)) :-
		number(Step), Step > 0.0.
	valid_option(tol_abs(Tolerance)) :-
		number(Tolerance), Tolerance > 0.0.
	valid_option(tol_rel(Tolerance)) :-
		number(Tolerance), Tolerance >= 0.0.
	valid_option(safety_factor(Factor)) :-
		number(Factor), Factor > 0.0, Factor < 1.0.
	valid_option(max_steps(MaxSteps)) :-
		integer(MaxSteps), MaxSteps > 0.
	valid_option(Option) :-
		^^valid_option(Option).

:- end_object.
