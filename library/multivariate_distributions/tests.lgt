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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-12,
		comment is 'Unit tests for the "multivariate_distributions" library.'
	]).

	cover(multivariate_distributions(_)).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2
	]).

	test(multivariate_normal_3_full_rank, deterministic(Sample =~= [3.0, 3.0])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal(
			[1.0, 2.0], [[4.0, 0.0], [0.0, 1.0]], Sample
		).

	test(multivariate_normal_4_singular, deterministic(Sample =~= [3.0, 2.0])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal(
			[1.0, 2.0], [[4.0, 0.0], [0.0, 0.0]], 1.0e-12, Sample
		).

	test(multivariate_normal_3_rank_zero, deterministic(Sample == [1.0, 2.0])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal(
			[1.0, 2.0], [[0.0, 0.0], [0.0, 0.0]], Sample
		).

	test(multivariate_normal_samples_4, deterministic(Samples =~= [[3.0, 3.0], [3.0, 3.0]])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_samples(
			2, [1.0, 2.0], [[4.0, 0.0], [0.0, 1.0]], Samples
		).

	test(multivariate_normal_samples_5_zero, deterministic(Samples == [])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_samples(
			0, [1.0], [[1.0]], 1.0e-12, Samples
		).

	test(multivariate_normal_density_4, deterministic(Density =~= 0.3989422804014327)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_density(
			[0.0], [0.0], [[1.0]], Density
		).

	test(multivariate_normal_log_density_5, deterministic(LogDensity =~= -0.9189385332046727)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_log_density(
			[0.0], [0.0], [[1.0]], 1.0e-12, LogDensity
		).

	test(multivariate_normal_log_density_4, deterministic(LogDensity =~= -0.9189385332046727)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_log_density(
			[0.0], [0.0], [[1.0]], LogDensity
		).

	test(multivariate_normal_density_5_singular_on_support, deterministic(Density =~= 0.24197072451914337)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_density(
			[1.0, 0.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], 1.0e-12, Density
		).

	test(multivariate_normal_density_5_singular_off_support, deterministic(Density == 0.0)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_density(
			[1.0, 1.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], 1.0e-12, Density
		).

	test(multivariate_normal_log_density_5_singular_off_support, deterministic(LogDensity == negative_infinity)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_log_density(
			[1.0, 1.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], 1.0e-12, LogDensity
		).

	test(multivariate_normal_density_4_rank_zero, deterministic(Density == 1.0)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal_density(
			[1.0, 2.0], [1.0, 2.0], [[0.0, 0.0], [0.0, 0.0]], Density
		).

	test(squared_mahalanobis_distance_4, deterministic(SquaredDistance =~= 4.0)) :-
		multivariate_distributions(fixed_multivariate_sampler)::squared_mahalanobis_distance(
			[4.0, 0.0], [0.0, 0.0], [[4.0, 0.0], [0.0, 1.0]], SquaredDistance
		).

	test(mahalanobis_distance_5, deterministic(Distance =~= 2.0)) :-
		multivariate_distributions(fixed_multivariate_sampler)::mahalanobis_distance(
			[4.0, 0.0], [0.0, 0.0], [[4.0, 0.0], [0.0, 1.0]], 1.0e-12, Distance
		).

	test(mahalanobis_distance_4, deterministic(Distance =~= 2.0)) :-
		multivariate_distributions(fixed_multivariate_sampler)::mahalanobis_distance(
			[4.0, 0.0], [0.0, 0.0], [[4.0, 0.0], [0.0, 1.0]], Distance
		).

	test(squared_mahalanobis_distance_5_off_support, error(domain_error(covariance_support, [1.0, 1.0]))) :-
		multivariate_distributions(fixed_multivariate_sampler)::squared_mahalanobis_distance(
			[1.0, 1.0], [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], 1.0e-12, _
		).

	test(multivariate_t_4, deterministic(Sample =~= [3.0, 3.0])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t(
			2.0, [1.0, 2.0], [[4.0, 0.0], [0.0, 1.0]], Sample
		).

	test(multivariate_t_5_singular, deterministic(Sample =~= [3.0, 2.0])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t(
			2.0, [1.0, 2.0], [[4.0, 0.0], [0.0, 0.0]], 1.0e-12, Sample
		).

	test(multivariate_t_samples_5, deterministic(Samples =~= [[3.0], [3.0]])) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t_samples(
			2, 2.0, [1.0], [[4.0]], Samples
		).

	test(multivariate_t_density_5_cauchy, deterministic(Density =~= 0.3183098861837907)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t_density(
			[0.0], 1.0, [0.0], [[1.0]], Density
		).

	test(multivariate_t_log_density_6_cauchy, deterministic(LogDensity =~= -1.1447298858494002)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t_log_density(
			[0.0], 1.0, [0.0], [[1.0]], 1.0e-12, LogDensity
		).

	test(multivariate_t_log_density_5_cauchy, deterministic(LogDensity =~= -1.1447298858494002)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t_log_density(
			[0.0], 1.0, [0.0], [[1.0]], LogDensity
		).

	test(multivariate_t_density_6_off_support, deterministic(Density == 0.0)) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t_density(
			[0.0, 1.0], 3.0, [0.0, 0.0], [[1.0, 0.0], [0.0, 0.0]], 1.0e-12, Density
		).

	test(multivariate_t_5_invalid_degrees_of_freedom, error(domain_error(positive_number, 0.0))) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_t(
			0.0, [0.0], [[1.0]], 1.0e-12, _
		).

	test(logistic_normal_3, deterministic((Sample =~= [Expected1, Expected2, Reference], Sum =~= 1.0))) :-
		Denominator is exp(2.0) + exp(1.0) + 1.0,
		Expected1 is exp(2.0) / Denominator,
		Expected2 is exp(1.0) / Denominator,
		Reference is 1.0 / Denominator,
		multivariate_distributions(fixed_multivariate_sampler)::logistic_normal(
			[0.0, 0.0], [[4.0, 0.0], [0.0, 1.0]], Sample
		),
		Sample = [First, Second, Third],
		Sum is First + Second + Third.

	test(logistic_normal_4_rank_zero, deterministic((Sample =~= [Expected, Reference], Sum =~= 1.0))) :-
		Expected is exp(2.0) / (exp(2.0) + 1.0),
		Reference is 1.0 / (exp(2.0) + 1.0),
		multivariate_distributions(fixed_multivariate_sampler)::logistic_normal(
			[2.0], [[0.0]], 1.0e-12, Sample
		),
		Sample = [First, Second],
		Sum is First + Second.

	test(logistic_normal_samples_4, deterministic((Samples = [First, Second], First =~= Second))) :-
		multivariate_distributions(fixed_multivariate_sampler)::logistic_normal_samples(
			2, [0.0], [[1.0]], Samples
		).

	test(multivariate_normal_4_empty_mean, error(domain_error(minimum_number_of_values(1), []))) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal([], [], 1.0e-12, _).

	test(multivariate_normal_4_dimension_mismatch, error(domain_error(covariance_dimensions(2), [[1.0]]))) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal([0.0, 0.0], [[1.0]], 1.0e-12, _).

	test(multivariate_normal_4_negative_tolerance, error(domain_error(non_negative_number, -1.0))) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal([0.0], [[1.0]], -1.0, _).

	test(multivariate_normal_4_indefinite, error(domain_error(positive_semidefinite_matrix, [[1.0, 0.0], [0.0, -1.0]]))) :-
		multivariate_distributions(fixed_multivariate_sampler)::multivariate_normal(
			[0.0, 0.0], [[1.0, 0.0], [0.0, -1.0]], 1.0e-12, _
		).

	test(dirichlet_2, deterministic) :-
		multivariate_distributions(fast_random)::dirichlet([1.0, 2.0], [First, Second]),
		^^assertion(First >= 0.0),
		^^assertion(Second >= 0.0),
		Sum is First + Second,
		^^assertion(Sum =~= 1.0).

	test(dirichlet_samples_3, deterministic) :-
		multivariate_distributions(fast_random)::dirichlet_samples(2, [1.0, 2.0], [First, Second]),
		length(First, FirstLength),
		length(Second, SecondLength),
		^^assertion(FirstLength =:= 2),
		^^assertion(SecondLength =:= 2).

	test(dirichlet_density_3, deterministic(Density =~= 1.0)) :-
		multivariate_distributions(fast_random)::dirichlet_density([0.25, 0.75], [1.0, 1.0], Density).

	test(dirichlet_log_density_3, deterministic(LogDensity =~= 0.0)) :-
		multivariate_distributions(fast_random)::dirichlet_log_density([0.25, 0.75], [1.0, 1.0], LogDensity).

	test(dirichlet_density_off_simplex, deterministic(Density =~= 0.0)) :-
		multivariate_distributions(fast_random)::dirichlet_density([0.25, 0.25], [1.0, 1.0], Density).

	test(dirichlet_density_uniform_boundary, deterministic(Density =~= 1.0)) :-
		multivariate_distributions(fast_random)::dirichlet_density([0.0, 1.0], [1.0, 1.0], Density).

	test(dirichlet_log_density_uniform_boundary, deterministic(LogDensity =~= 0.0)) :-
		multivariate_distributions(fast_random)::dirichlet_log_density([0.0, 1.0], [1.0, 1.0], LogDensity).

	test(dirichlet_density_zero_boundary, deterministic(Density == 0.0)) :-
		multivariate_distributions(fast_random)::dirichlet_density([0.0, 1.0], [2.0, 1.0], Density).

	test(dirichlet_log_density_zero_boundary, deterministic(LogDensity == negative_infinity)) :-
		multivariate_distributions(fast_random)::dirichlet_log_density([0.0, 1.0], [2.0, 1.0], LogDensity).

	test(dirichlet_density_infinite_boundary, deterministic(Density == positive_infinity)) :-
		multivariate_distributions(fast_random)::dirichlet_density([0.0, 1.0], [0.5, 1.0], Density).

	test(dirichlet_log_density_infinite_boundary, deterministic(LogDensity == positive_infinity)) :-
		multivariate_distributions(fast_random)::dirichlet_log_density([0.0, 1.0], [0.5, 1.0], LogDensity).

	test(dirichlet_density_mixed_boundary, deterministic(Density == undefined)) :-
		multivariate_distributions(fast_random)::dirichlet_density([0.0, 0.0, 1.0], [0.5, 2.0, 1.0], Density).

	test(dirichlet_log_density_mixed_boundary_order_independent, deterministic((LogDensity1 == undefined, LogDensity2 == undefined))) :-
		multivariate_distributions(fast_random)::dirichlet_log_density([0.0, 0.0, 1.0], [0.5, 2.0, 1.0], LogDensity1),
		multivariate_distributions(fast_random)::dirichlet_log_density([0.0, 0.0, 1.0], [2.0, 0.5, 1.0], LogDensity2).

	test(dirichlet_log_density_mixed_boundary_absorbing, deterministic(LogDensity == undefined)) :-
		multivariate_distributions(fast_random)::dirichlet_log_density([0.0, 0.0, 0.0, 1.0], [0.5, 2.0, 0.5, 1.0], LogDensity).

	test(multinomial_3, deterministic) :-
		multivariate_distributions(fast_random)::multinomial(10, [0.25, 0.75], [First, Second]),
		^^assertion(First >= 0),
		^^assertion(Second >= 0),
		Total is First + Second,
		^^assertion(Total =:= 10).

	test(multinomial_samples_4, deterministic) :-
		multivariate_distributions(fast_random)::multinomial_samples(2, 10, [0.25, 0.75], [First, Second]),
		length(First, FirstLength),
		length(Second, SecondLength),
		^^assertion(FirstLength =:= 2),
		^^assertion(SecondLength =:= 2).

	test(multinomial_density_4, deterministic(Density =~= 0.5)) :-
		multivariate_distributions(fast_random)::multinomial_density([1, 1], 2, [0.5, 0.5], Density).

	test(multinomial_log_density_4, deterministic(LogDensity =~= -0.6931471805599453)) :-
		multivariate_distributions(fast_random)::multinomial_log_density([1, 1], 2, [0.5, 0.5], LogDensity).

	test(multinomial_3_probability_distribution_error, error(domain_error(probability_distribution, [0.2, 0.2]))) :-
		multivariate_distributions(fast_random)::multinomial(2, [0.2, 0.2], _).

	test(multinomial_samples_4_probability_distribution_error, error(domain_error(probability_distribution, [0.2, 0.2]))) :-
		multivariate_distributions(fast_random)::multinomial_samples(1, 2, [0.2, 0.2], _).

	test(multinomial_density_4_probability_distribution_error, error(domain_error(probability_distribution, [0.2, 0.2]))) :-
		multivariate_distributions(fast_random)::multinomial_density([1, 1], 2, [0.2, 0.2], _).

	test(multinomial_log_density_4_probability_distribution_error, error(domain_error(probability_distribution, [0.2, 0.2]))) :-
		multivariate_distributions(fast_random)::multinomial_log_density([1, 1], 2, [0.2, 0.2], _).

	test(multinomial_density_4_empty_probabilities, error(domain_error(minimum_number_of_values(1), []))) :-
		multivariate_distributions(fast_random)::multinomial_density([], 0, [], _).

	test(multinomial_density_counts_mismatch, deterministic(Density =~= 0.0)) :-
		multivariate_distributions(fast_random)::multinomial_density([1, 0], 2, [0.5, 0.5], Density).

	test(multinomial_quantile_trivial_case, deterministic(Quantile == [0, 0])) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.5, 0, [0.3, 0.7], Quantile).

	test(multinomial_quantile_general_case, deterministic(Quantile == [1, 1])) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.4, 2, [0.5, 0.5], Quantile).

	test(multinomial_quantile_tie_lexicographic_order, deterministic(Quantile == [0, 2])) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.6, 2, [0.5, 0.5], Quantile).

	test(multinomial_quantile_approximate_tie_lexicographic_order, deterministic(Quantile == [0, 1, 4])) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.87, 5, [0.3333333333333333, 0.3333333333333333, 0.3333333333333333], Quantile).

	test(multinomial_quantile_zero_probability_category, deterministic(Quantile == [0, 2])) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.5, 2, [0.0, 1.0], Quantile).

	test(multinomial_quantile_probability_distribution_error, error(domain_error(probability_distribution, [0.2, 0.2]))) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.5, 2, [0.2, 0.2], _).

	test(multinomial_quantile_composition_resource_error, error(resource_error(multinomial_quantile_compositions))) :-
		multivariate_distributions(fast_random)::multinomial_quantile(0.5, 20, [0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1], _).

:- end_object.
