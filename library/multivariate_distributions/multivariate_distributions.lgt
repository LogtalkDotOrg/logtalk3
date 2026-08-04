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


:- object(multivariate_distributions(_Random_),
	implements(multivariate_distributions_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-04,
		comment is 'Multivariate probability distributions using an injected random source.',
		parameters is [
			'Random' - 'An object implementing the ``sampling_protocol`` protocol.'
		],
		see_also is [multivariate_distributions_protocol, linear_algebra, random(_), fast_random(_), backend_random]
	]).

	:- uses(_Random_, [
		standard_normal/1, standard_gamma/2
	]).

	:- uses(linear_algebra, [
		add_scaled_vector/4, add_vectors/3, dot_product/3, new_vector/3, scale_vector/3, subtract_vectors/3,
		symmetric_eigen/4
	]).

	:- uses(list, [
		append/3, length/2
	]).

	:- uses(type, [
		check/3
	]).

	multivariate_normal(Mean, Covariance, Sample) :-
		multivariate_normal(Mean, Covariance, 1.0e-12, Sample).

	multivariate_normal(Mean, Covariance, Tolerance, Sample) :-
		covariance_factorization(Mean, Covariance, Tolerance, Factorization),
		multivariate_normal_from_factorization(Mean, Factorization, Sample).

	multivariate_normal_samples(Count, Mean, Covariance, Samples) :-
		multivariate_normal_samples(Count, Mean, Covariance, 1.0e-12, Samples).

	multivariate_normal_samples(Count, Mean, Covariance, Tolerance, Samples) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		covariance_factorization(Mean, Covariance, Tolerance, Factorization),
		generate_multivariate_normal_samples(Count, Mean, Factorization, Samples).

	multivariate_normal_density(Point, Mean, Covariance, Density) :-
		multivariate_normal_density(Point, Mean, Covariance, 1.0e-12, Density).

	multivariate_normal_density(Point, Mean, Covariance, Tolerance, Density) :-
		normal_evaluation(Point, Mean, Covariance, Tolerance, Evaluation),
		(Evaluation = on_support(LogDensity, _SquaredDistance) ->
			Density is exp(LogDensity)
		;	Density = 0.0
		).

	multivariate_normal_log_density(Point, Mean, Covariance, LogDensity) :-
		multivariate_normal_log_density(Point, Mean, Covariance, 1.0e-12, LogDensity).

	multivariate_normal_log_density(Point, Mean, Covariance, Tolerance, LogDensity) :-
		normal_evaluation(Point, Mean, Covariance, Tolerance, Evaluation),
		(	Evaluation = on_support(LogDensity, _SquaredDistance) ->
			true
		;	LogDensity = negative_infinity
		).

	squared_mahalanobis_distance(Point, Mean, Covariance, SquaredDistance) :-
		squared_mahalanobis_distance(Point, Mean, Covariance, 1.0e-12, SquaredDistance).

	squared_mahalanobis_distance(Point, Mean, Covariance, Tolerance, SquaredDistance) :-
		point_factorization(Point, Mean, Covariance, Tolerance, Difference, Factorization),
		factorization_quadratic(Difference, Factorization, Evaluation),
		(	Evaluation = on_support(SquaredDistance) ->
			true
		;	domain_error(covariance_support, Point)
		).

	mahalanobis_distance(Point, Mean, Covariance, Distance) :-
		mahalanobis_distance(Point, Mean, Covariance, 1.0e-12, Distance).

	mahalanobis_distance(Point, Mean, Covariance, Tolerance, Distance) :-
		squared_mahalanobis_distance(Point, Mean, Covariance, Tolerance, SquaredDistance),
		Distance is sqrt(SquaredDistance).

	multivariate_t(DegreesOfFreedom, Location, Scale, Sample) :-
		multivariate_t(DegreesOfFreedom, Location, Scale, 1.0e-12, Sample).

	multivariate_t(DegreesOfFreedom, Location, Scale, Tolerance, Sample) :-
		context(Context),
		check(positive_number, DegreesOfFreedom, Context),
		covariance_factorization(Location, Scale, Tolerance, Factorization),
		multivariate_t_from_factorization(DegreesOfFreedom, Location, Factorization, Sample).

	multivariate_t_samples(Count, DegreesOfFreedom, Location, Scale, Samples) :-
		multivariate_t_samples(Count, DegreesOfFreedom, Location, Scale, 1.0e-12, Samples).

	multivariate_t_samples(Count, DegreesOfFreedom, Location, Scale, Tolerance, Samples) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		check(positive_number, DegreesOfFreedom, Context),
		covariance_factorization(Location, Scale, Tolerance, Factorization),
		generate_multivariate_t_samples(Count, DegreesOfFreedom, Location, Factorization, Samples).

	multivariate_t_density(Point, DegreesOfFreedom, Location, Scale, Density) :-
		multivariate_t_density(Point, DegreesOfFreedom, Location, Scale, 1.0e-12, Density).

	multivariate_t_density(Point, DegreesOfFreedom, Location, Scale, Tolerance, Density) :-
		multivariate_t_evaluation(Point, DegreesOfFreedom, Location, Scale, Tolerance, Evaluation),
		(	Evaluation = on_support(LogDensity) ->
			Density is exp(LogDensity)
		;	Density = 0.0
		).

	multivariate_t_log_density(Point, DegreesOfFreedom, Location, Scale, LogDensity) :-
		multivariate_t_log_density(Point, DegreesOfFreedom, Location, Scale, 1.0e-12, LogDensity).

	multivariate_t_log_density(Point, DegreesOfFreedom, Location, Scale, Tolerance, LogDensity) :-
		multivariate_t_evaluation(Point, DegreesOfFreedom, Location, Scale, Tolerance, Evaluation),
		(	Evaluation = on_support(LogDensity) ->
			true
		;	LogDensity = negative_infinity
		).

	logistic_normal(Mean, Covariance, Sample) :-
		logistic_normal(Mean, Covariance, 1.0e-12, Sample).

	logistic_normal(Mean, Covariance, Tolerance, Sample) :-
		multivariate_normal(Mean, Covariance, Tolerance, LatentSample),
		inverse_additive_log_ratio(LatentSample, Sample).

	logistic_normal_samples(Count, Mean, Covariance, Samples) :-
		logistic_normal_samples(Count, Mean, Covariance, 1.0e-12, Samples).

	logistic_normal_samples(Count, Mean, Covariance, Tolerance, Samples) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		covariance_factorization(Mean, Covariance, Tolerance, Factorization),
		generate_logistic_normal_samples(Count, Mean, Factorization, Samples).

	generate_multivariate_normal_samples(0, _Mean, _Factorization, []) :-
		!.
	generate_multivariate_normal_samples(Count, Mean, Factorization, [Sample| Samples]) :-
		multivariate_normal_from_factorization(Mean, Factorization, Sample),
		Remaining is Count - 1,
		generate_multivariate_normal_samples(Remaining, Mean, Factorization, Samples).

	generate_multivariate_t_samples(0, _DegreesOfFreedom, _Location, _Factorization, []) :-
		!.
	generate_multivariate_t_samples(Count, DegreesOfFreedom, Location, Factorization, [Sample| Samples]) :-
		multivariate_t_from_factorization(DegreesOfFreedom, Location, Factorization, Sample),
		Remaining is Count - 1,
		generate_multivariate_t_samples(Remaining, DegreesOfFreedom, Location, Factorization, Samples).

	generate_logistic_normal_samples(0, _Mean, _Factorization, []) :-
		!.
	generate_logistic_normal_samples(Count, Mean, Factorization, [Sample| Samples]) :-
		multivariate_normal_from_factorization(Mean, Factorization, LatentSample),
		inverse_additive_log_ratio(LatentSample, Sample),
		Remaining is Count - 1,
		generate_logistic_normal_samples(Remaining, Mean, Factorization, Samples).

	covariance_factorization(Mean, Covariance, Tolerance, factorization(Dimension, Tolerance, PositiveEigenpairs, NullEigenvectors, LogPseudoDeterminant)) :-
		context(Context),
		check(list(number), Mean, Context),
		check(list(list(number)), Covariance, Context),
		check(non_negative_number, Tolerance, Context),
		(	Mean == [] ->
			domain_error(minimum_number_of_values(1), Mean)
		;	length(Mean, Dimension),
			length(Covariance, CovarianceDimension),
			(	CovarianceDimension =:= Dimension ->
				symmetric_eigen(Covariance, Tolerance, Eigenvectors, Eigenvalues),
				classify_eigenpairs(Eigenvectors, Eigenvalues, Tolerance, Covariance, PositiveEigenpairs, NullEigenvectors, 0.0, LogPseudoDeterminant)
			;	domain_error(covariance_dimensions(Dimension), Covariance)
			)
		).

	classify_eigenpairs([], [], _Tolerance, _Covariance, [], [], LogPseudoDeterminant, LogPseudoDeterminant).
	classify_eigenpairs([Eigenvector| Eigenvectors], [Eigenvalue| Eigenvalues], Tolerance, Covariance, PositiveEigenpairs, NullEigenvectors, LogPseudoDeterminant0, LogPseudoDeterminant) :-
		(	Eigenvalue > Tolerance ->
			SquareRoot is sqrt(Eigenvalue),
			Inverse is 1.0 / Eigenvalue,
			PositiveEigenpairs = [eigenpair(SquareRoot, Inverse, Eigenvector)| RestPositiveEigenpairs],
			NullEigenvectors = RestNullEigenvectors,
			LogPseudoDeterminant1 is LogPseudoDeterminant0 + log(Eigenvalue)
		;	Eigenvalue >= -Tolerance ->
			PositiveEigenpairs = RestPositiveEigenpairs,
			NullEigenvectors = [Eigenvector| RestNullEigenvectors],
			LogPseudoDeterminant1 = LogPseudoDeterminant0
		;	domain_error(positive_semidefinite_matrix, Covariance)
		),
		classify_eigenpairs(Eigenvectors, Eigenvalues, Tolerance, Covariance, RestPositiveEigenpairs, RestNullEigenvectors, LogPseudoDeterminant1, LogPseudoDeterminant).

	multivariate_normal_from_factorization(Mean, factorization(Dimension, _Tolerance, PositiveEigenpairs, _NullEigenvectors, _LogPseudoDeterminant), Sample) :-
		new_vector(Dimension, 0.0, ZeroVector),
		random_displacement(PositiveEigenpairs, ZeroVector, Displacement),
		add_vectors(Mean, Displacement, Sample).

	random_displacement([], Displacement, Displacement).
	random_displacement([eigenpair(SquareRoot, _Inverse, Eigenvector)| Eigenpairs], Displacement0, Displacement) :-
		standard_normal(StandardNormal),
		Scale is SquareRoot * StandardNormal,
		add_scaled_vector(Eigenvector, Scale, Displacement0, Displacement1),
		random_displacement(Eigenpairs, Displacement1, Displacement).

	multivariate_t_from_factorization(DegreesOfFreedom, Location, Factorization, Sample) :-
		Factorization = factorization(Dimension, _Tolerance, PositiveEigenpairs, _NullEigenvectors, _LogPseudoDeterminant),
		new_vector(Dimension, 0.0, ZeroVector),
		random_displacement(PositiveEigenpairs, ZeroVector, Displacement0),
		Shape is DegreesOfFreedom / 2.0,
		standard_gamma(Shape, Gamma),
		ChiSquared is 2.0 * Gamma,
		Scale is sqrt(DegreesOfFreedom / ChiSquared),
		scale_vector(Displacement0, Scale, Displacement),
		add_vectors(Location, Displacement, Sample).

	point_factorization(Point, Mean, Covariance, Tolerance, Difference, Factorization) :-
		context(Context),
		check(list(number), Point, Context),
		covariance_factorization(Mean, Covariance, Tolerance, Factorization),
		Factorization = factorization(Dimension, _FactorizationTolerance, _PositiveEigenpairs, _NullEigenvectors, _LogPseudoDeterminant),
		length(Point, PointDimension),
		(	PointDimension =:= Dimension ->
			subtract_vectors(Point, Mean, Difference)
		;	domain_error(point_dimensions(Dimension), Point)
		).

	normal_evaluation(Point, Mean, Covariance, Tolerance, Evaluation) :-
		point_factorization(Point, Mean, Covariance, Tolerance, Difference, Factorization),
		factorization_quadratic(Difference, Factorization, QuadraticEvaluation),
		(	QuadraticEvaluation = on_support(SquaredDistance) ->
			Factorization = factorization(_Dimension, _FactorizationTolerance, PositiveEigenpairs, _NullEigenvectors, LogPseudoDeterminant),
			length(PositiveEigenpairs, Rank),
			LogDensity is -0.5 * (Rank * log(2.0 * pi) + LogPseudoDeterminant + SquaredDistance),
			Evaluation = on_support(LogDensity, SquaredDistance)
		;	Evaluation = off_support
		).

	multivariate_t_evaluation(Point, DegreesOfFreedom, Location, Scale, Tolerance, Evaluation) :-
		context(Context),
		check(positive_number, DegreesOfFreedom, Context),
		point_factorization(Point, Location, Scale, Tolerance, Difference, Factorization),
		factorization_quadratic(Difference, Factorization, QuadraticEvaluation),
		(	QuadraticEvaluation = on_support(SquaredDistance) ->
			Factorization = factorization(_Dimension, _FactorizationTolerance, PositiveEigenpairs, _NullEigenvectors, LogPseudoDeterminant),
			length(PositiveEigenpairs, Rank),
			HalfDegreesOfFreedom is DegreesOfFreedom / 2.0,
			HalfDegreesOfFreedomAndRank is (DegreesOfFreedom + Rank) / 2.0,
			log_gamma(HalfDegreesOfFreedomAndRank, LogGammaNumerator),
			log_gamma(HalfDegreesOfFreedom, LogGammaDenominator),
			LogDensity is LogGammaNumerator - LogGammaDenominator - 0.5 * (Rank * log(DegreesOfFreedom * pi) + LogPseudoDeterminant) - HalfDegreesOfFreedomAndRank * log(1.0 + SquaredDistance / DegreesOfFreedom),
			Evaluation = on_support(LogDensity)
		;	Evaluation = off_support
		).

	factorization_quadratic(Difference, factorization(_Dimension, Tolerance, PositiveEigenpairs, NullEigenvectors, _LogPseudoDeterminant), Evaluation) :-
		null_residual_squared(NullEigenvectors, Difference, 0.0, ResidualSquared),
		ToleranceSquared is Tolerance * Tolerance,
		(	ResidualSquared =< ToleranceSquared ->
			positive_quadratic(PositiveEigenpairs, Difference, 0.0, SquaredDistance),
			Evaluation = on_support(SquaredDistance)
		;	Evaluation = off_support
		).

	null_residual_squared([], _Difference, ResidualSquared, ResidualSquared).
	null_residual_squared([Eigenvector| Eigenvectors], Difference, ResidualSquared0, ResidualSquared) :-
		dot_product(Eigenvector, Difference, Projection),
		ResidualSquared1 is ResidualSquared0 + Projection * Projection,
		null_residual_squared(Eigenvectors, Difference, ResidualSquared1, ResidualSquared).

	positive_quadratic([], _Difference, SquaredDistance, SquaredDistance).
	positive_quadratic([eigenpair(_SquareRoot, Inverse, Eigenvector)| Eigenpairs], Difference, SquaredDistance0, SquaredDistance) :-
		dot_product(Eigenvector, Difference, Projection),
		SquaredDistance1 is SquaredDistance0 + Inverse * Projection * Projection,
		positive_quadratic(Eigenpairs, Difference, SquaredDistance1, SquaredDistance).

	inverse_additive_log_ratio(LatentSample, Sample) :-
		maximum_with_reference(LatentSample, 0.0, Maximum),
		exponentials_and_sum(LatentSample, Maximum, Exponentials, 0.0, Sum0),
		Reference is exp(-Maximum),
		Sum is Sum0 + Reference,
		normalize_exponentials(Exponentials, Sum, Sample0),
		ReferenceValue is Reference / Sum,
		append(Sample0, [ReferenceValue], Sample).

	maximum_with_reference([], Maximum, Maximum).
	maximum_with_reference([Value| Values], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, Value),
		maximum_with_reference(Values, Maximum1, Maximum).

	exponentials_and_sum([], _Maximum, [], Sum, Sum).
	exponentials_and_sum([Value| Values], Maximum, [Exponential| Exponentials], Sum0, Sum) :-
		Exponential is exp(Value - Maximum),
		Sum1 is Sum0 + Exponential,
		exponentials_and_sum(Values, Maximum, Exponentials, Sum1, Sum).

	normalize_exponentials([], _Sum, []).
	normalize_exponentials([Exponential| Exponentials], Sum, [Value| Values]) :-
		Value is Exponential / Sum,
		normalize_exponentials(Exponentials, Sum, Values).

	log_gamma(Value, LogGamma) :-
		(	Value < 0.5 ->
			log_gamma(1.0 - Value, ReflectedLogGamma),
			LogGamma is log(pi) - log(sin(pi * Value)) - ReflectedLogGamma
		;	ShiftedValue is Value - 1.0,
			lanczos_sum(ShiftedValue, Sum),
			T is ShiftedValue + 7.5,
			LogGamma is 0.5 * log(2.0 * pi) + (ShiftedValue + 0.5) * log(T) - T + log(Sum)
		).

	lanczos_sum(Value, Sum) :-
		lanczos_coefficients(Coefficients),
		lanczos_sum(Coefficients, Value, 1, 0.99999999999980993, Sum).

	lanczos_sum([], _Value, _Index, Sum, Sum).
	lanczos_sum([Coefficient| Coefficients], Value, Index, Sum0, Sum) :-
		Sum1 is Sum0 + Coefficient / (Value + Index),
		NextIndex is Index + 1,
		lanczos_sum(Coefficients, Value, NextIndex, Sum1, Sum).

	lanczos_coefficients([
		676.5203681218851,
		-1259.1392167224028,
		771.32342877765313,
		-176.61502916214059,
		12.507343278686905,
		-0.13857109526572012,
		9.9843695780195716e-6,
		1.5056327351493116e-7
	]).

:- end_object.
