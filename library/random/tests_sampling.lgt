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


:- object(fixed_sampling_source,
	implements(sampling_protocol)).

	random(0.5).

	random(Lower, Upper, Random) :-
		Random is (Lower + Upper) / 2.0.

	random_pair(0.5, 0.5).

	:- include(sampling).

:- end_object.


:- object(sampling_tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-10-10,
		comment is 'Unit tests for the sampling protocol implementation.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	:- set_logtalk_flag(arithmetic_expressions, silent).

	test(sampling_fast_random_standard_normal_seed, deterministic((Actual =~= Expected, ActualSeed == ExpectedSeed))) :-
		fast_random(xoshiro128pp)::get_seed(Seed),
		compare_standard_normal(fast_random(xoshiro128pp), Seed, Actual, Expected, ActualSeed, ExpectedSeed).

	test(sampling_fast_random_standard_normal_zero_draw, deterministic((Actual =~= Expected, ActualSeed == ExpectedSeed))) :-
		compare_standard_normal(fast_random(xoshiro128pp), seed(xoshiro128pp, s(0, 0, 1, 0)), Actual, Expected, ActualSeed, ExpectedSeed).

	test(sampling_random_standard_normal_seed, deterministic((Actual =~= Expected, ActualSeed == ExpectedSeed))) :-
		random(xoshiro128pp)::get_seed(Seed),
		compare_standard_normal(random(xoshiro128pp), Seed, Actual, Expected, ActualSeed, ExpectedSeed).

	test(sampling_random_standard_normal_zero_draw, deterministic((Actual =~= Expected, ActualSeed == ExpectedSeed))) :-
		compare_standard_normal(random(xoshiro128pp), seed(xoshiro128pp, s(0, 0, 1, 0)), Actual, Expected, ActualSeed, ExpectedSeed).

	test(sampling_lognormal_3, deterministic(Value =~= Expected)) :-
		Normal is 0.7 - 0.2 * sqrt(-2.0 * log(0.5)),
		Expected is exp(Normal),
		fixed_sampling_source::lognormal(0.7, 0.2, Value).

	test(sampling_geometric_2_certain_success, deterministic(Value == 1)) :-
		fixed_sampling_source::geometric(1.0, Value).

	test(sampling_geometric_2_zero_probability, fail) :-
		fixed_sampling_source::geometric(0.0, _).

	test(sampling_hypergeometric_4_invalid_successes, fail) :-
		fixed_sampling_source::hypergeometric(3, 4, 2, _).

	test(sampling_hypergeometric_4_invalid_draws, fail) :-
		fixed_sampling_source::hypergeometric(3, 2, 4, _).

	test(sampling_binomial_3_zero_trials, deterministic(Value == 0)) :-
		fixed_sampling_source::binomial(0, 0.5, Value).

	test(sampling_bernoulli_2_integer_result, deterministic(Value == 1)) :-
		fixed_sampling_source::bernoulli(0.75, Value).

	test(sampling_power_2, deterministic(Value =~= Expected)) :-
		Expected is sqrt(0.5),
		fixed_sampling_source::power(2.0, Value).

	test(sampling_triangular_4_degenerate, deterministic(Value == 2.0)) :-
		fixed_sampling_source::triangular(2.0, 2.0, 2.0, Value).

	test(sampling_von_mises_3_zero_concentration, deterministic(Value =:= pi)) :-
		fixed_sampling_source::von_mises(1.0, 0.0, Value).

	test(sampling_von_mises_3_normalized, true((0.0 =< Value, Value < 2.0*pi))) :-
		fixed_sampling_source::von_mises(-10.0, 1.0, Value).

	test(sampling_circular_uniform_polar_3, deterministic((Rho =~= ExpectedRho, Theta =:= pi))) :-
		ExpectedRho is sqrt(2.0),
		fixed_sampling_source::circular_uniform_polar(2.0, Rho, Theta).

	test(sampling_circular_uniform_polar_3_negative_radius, fail) :-
		fixed_sampling_source::circular_uniform_polar(-1.0, _, _).

	test(sampling_standard_cauchy_3_non_positive_scale, fail) :-
		fixed_sampling_source::standard_cauchy(0.0, 0.0, _).

	compare_standard_normal(Object, Seed, Actual, Expected, ActualSeed, ExpectedSeed) :-
		Object::get_seed(SavedSeed),
		Object::set_seed(Seed),
		repeat,
			Object::random(X1),
			Object::random(X2),
		X1 > 0.0,
		!,
		Expected is sqrt(-2.0 * log(X1)) * cos(2.0*pi*X2),
		Object::get_seed(ExpectedSeed),
		Object::set_seed(Seed),
		Object::standard_normal(Actual),
		Object::get_seed(ActualSeed),
		Object::set_seed(SavedSeed).

:- end_object.
