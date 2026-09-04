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


:- object(ode_solver_tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'ODE solver tests.'
	]).

	cover(ode_solver(_)).
	cover(euler_ode_solver(_)).
	cover(rk4_ode_solver(_)).
	cover(rk45_ode_solver(_)).

	:- uses(list, [last/2, memberchk/2]).

	test(euler_exponential, deterministic(abs(Value - 2.5937424601) < 1.0e-12)) :-
		euler_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, Trajectory, [step_size(0.1)]),
		last(Trajectory, 1.0-[Value]).

	test(rk4_exponential, deterministic(abs(Value - 2.718281828459045) < 3.0e-6)) :-
		rk4_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, Trajectory, [step_size(0.1)]),
		last(Trajectory, 1.0-[Value]).

	test(rk4_backward, deterministic(abs(Value - 1.0) < 3.0e-6)) :-
		E is exp(1.0),
		rk4_ode_solver(exponential_ode)::solve(1.0, [E], 0.0, Trajectory, [step_size(0.1)]),
		last(Trajectory, 0.0-[Value]).

	test(rk4_endpoint_clipping, deterministic) :-
		rk4_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, Trajectory, Statistics, [step_size(0.3)]),
		last(Trajectory, FinalTime-_),
		^^assertion(abs(FinalTime - 1.0) < 1.0e-15),
		memberchk(accepted_steps(4), Statistics).

	test(ode_zero_span, deterministic(Trajectory == [2.0-[3.0]])) :-
		rk4_ode_solver(malformed_ode)::solve(2.0, [3.0], 2.0, Trajectory).

	test(ode_max_steps, deterministic) :-
		euler_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, _Trajectory, Statistics, [step_size(0.1), max_steps(2)]),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(max_steps), Statistics).

	test(ode_invalid_state, error(domain_error(ode_state, []))) :-
		euler_ode_solver(exponential_ode)::solve(0.0, [], 1.0, _).

	test(ode_invalid_derivative, error(domain_error(ode_derivative, [1.0, 2.0]))) :-
		euler_ode_solver(malformed_ode)::solve(0.0, [1.0], 1.0, _).

	test(ode_non_numeric_derivative, error(domain_error(ode_derivative, [not_a_number]))) :-
		euler_ode_solver(non_numeric_ode)::solve(0.0, [1.0], 1.0, _).

	test(ode_invalid_option, error(domain_error(option, step_size(0.0)))) :-
		euler_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, _, [step_size(0.0)]).

	test(rk45_exponential, deterministic(abs(Value - 2.718281828459045) < 1.0e-8)) :-
		rk45_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, Trajectory, [tol_abs(1.0e-10), tol_rel(1.0e-9)]),
		last(Trajectory, 1.0-[Value]).

	test(rk45_harmonic_oscillator, deterministic) :-
		HalfPi is acos(-1.0) / 2.0,
		rk45_ode_solver(harmonic_oscillator_ode)::solve(0.0, [1.0, 0.0], HalfPi, Trajectory),
		last(Trajectory, HalfPi-[Position, Velocity]),
		^^assertion(abs(Position) < 2.0e-7),
		^^assertion(abs(Velocity + 1.0) < 2.0e-7).

	test(rk45_backward_harmonic_oscillator, deterministic) :-
		HalfPi is acos(-1.0) / 2.0,
		rk45_ode_solver(harmonic_oscillator_ode)::solve(HalfPi, [0.0, -1.0], 0.0, Trajectory),
		last(Trajectory, 0.0-[Position, Velocity]),
		^^assertion(abs(Position - 1.0) < 2.0e-7),
		^^assertion(abs(Velocity) < 2.0e-7).

	test(rk45_rejects_large_step, deterministic(Rejected > 0)) :-
		rk45_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, _Trajectory, Statistics, [initial_step(1.0), tol_abs(1.0e-12), tol_rel(1.0e-12)]),
		memberchk(rejected_steps(Rejected), Statistics).

	test(rk45_min_step, deterministic) :-
		rk45_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, _Trajectory, Statistics, [initial_step(1.0), min_step(1.0), max_step(1.0), tol_abs(1.0e-15), tol_rel(0.0)]),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(min_step), Statistics).

	test(rk45_max_steps, deterministic) :-
		rk45_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, _Trajectory, Statistics, [initial_step(0.01), max_step(0.01), max_steps(1)]),
		memberchk(converged(false), Statistics),
		memberchk(termination_reason(max_steps), Statistics).

	test(rk45_invalid_step_interval, error(domain_error(step_interval, 2.0-1.0))) :-
		rk45_ode_solver(exponential_ode)::solve(0.0, [1.0], 1.0, _, [min_step(2.0), max_step(1.0)]).

:- end_object.
