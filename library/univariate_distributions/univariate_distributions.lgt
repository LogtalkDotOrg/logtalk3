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


:- object(univariate_distributions(_Random_),
	implements(univariate_distributions_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-11,
		comment is 'Univariate probability distributions using an injected random source.',
		parameters is [
			'Random' - 'An object implementing the ``sampling_protocol`` protocol.'
		],
		remarks is [
			'Normal cumulative distribution' - 'Uses the Abramowitz and Stegun formula 26.2.17 approximation.',
			'Normal quantile' - 'Uses the Peter J. Acklam rational approximation.'
		],
		see_also is [
			univariate_distributions_protocol, multivariate_distributions(_), random(_), fast_random(_),
			fast_random, backend_random
		]
	]).

	:- uses(_Random_, [
		standard_normal/1 as random_standard_normal/1, normal/3 as random_normal/3,
		standard_gamma/2 as random_standard_gamma/2, gamma/3 as random_gamma/3,
		beta/3 as random_beta/3, exponential/2 as random_exponential/2
	]).

	:- uses(type, [
		check/3
	]).

	standard_normal(Value) :-
		random_standard_normal(Value).

	standard_normal_samples(Count, Samples) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		generate_standard_normal_samples(Count, Samples).

	normal(Mean, Deviation, Value) :-
		context(Context),
		check(float, Mean, Context),
		check(non_negative_float, Deviation, Context),
		random_normal(Mean, Deviation, Value).

	normal_samples(Count, Mean, Deviation, Samples) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		check(float, Mean, Context),
		check(non_negative_float, Deviation, Context),
		generate_normal_samples(Count, Mean, Deviation, Samples).

	standard_normal_density(Value, Density) :-
		(	number(Value) ->
			Density is exp(-0.5 * Value * Value) / sqrt(2.0 * pi)
		;	type_error(number, Value)
		).

	standard_normal_log_density(Value, LogDensity) :-
		(	number(Value) ->
			LogDensity is -0.5 * Value * Value - 0.5 * log(2.0 * pi)
		;	type_error(number, Value)
		).

	standard_normal_distribution(Value, Probability) :-
		(	number(Value) ->
			standard_normal_distribution_unchecked(Value, Probability)
		;	type_error(number, Value)
		).

	standard_normal_quantile(Probability, Quantile) :-
		(	\+ number(Probability) ->
			type_error(number, Probability)
		;	Probability > 0.0,
			Probability < 1.0 ->
			standard_normal_quantile_unchecked(Probability, Quantile)
		;	domain_error(open_probability, Probability)
		).

	normal_density(Value, Mean, Deviation, Density) :-
		check_normal_evaluation_arguments(Value, Mean, Deviation),
		StandardValue is (Value - Mean) / Deviation,
		standard_normal_density(StandardValue, StandardDensity),
		Density is StandardDensity / Deviation.

	normal_log_density(Value, Mean, Deviation, LogDensity) :-
		check_normal_evaluation_arguments(Value, Mean, Deviation),
		StandardValue is (Value - Mean) / Deviation,
		standard_normal_log_density(StandardValue, StandardLogDensity),
		LogDensity is StandardLogDensity - log(Deviation).

	normal_distribution(Value, Mean, Deviation, Probability) :-
		check_normal_evaluation_arguments(Value, Mean, Deviation),
		StandardValue is (Value - Mean) / Deviation,
		standard_normal_distribution_unchecked(StandardValue, Probability).

	normal_quantile(Probability, Mean, Deviation, Quantile) :-
		standard_normal_quantile(Probability, StandardQuantile),
		context(Context),
		check(number, Mean, Context),
		check(positive_number, Deviation, Context),
		Quantile is Mean + Deviation * StandardQuantile.

	standard_t(DegreesOfFreedom, Value) :-
		check_positive_number(DegreesOfFreedom),
		random_standard_normal(Normal),
		Shape is DegreesOfFreedom / 2.0,
		random_standard_gamma(Shape, Gamma),
		Value is Normal / sqrt(2.0 * Gamma / DegreesOfFreedom).

	standard_t_samples(Count, DegreesOfFreedom, Samples) :-
		check_count_and_positive_number(Count, DegreesOfFreedom),
		generate_standard_t_samples(Count, DegreesOfFreedom, Samples).

	standard_t_density(Value, DegreesOfFreedom, Density) :-
		standard_t_log_density(Value, DegreesOfFreedom, LogDensity),
		Density is exp(LogDensity).

	standard_t_log_density(Value, DegreesOfFreedom, LogDensity) :-
		check_value_and_positive_number(Value, DegreesOfFreedom),
		HalfDegreesOfFreedom is DegreesOfFreedom / 2.0,
		log_gamma((DegreesOfFreedom + 1.0) / 2.0, LogGammaNumerator),
		log_gamma(HalfDegreesOfFreedom, LogGammaDenominator),
		LogDensity is LogGammaNumerator - LogGammaDenominator - 0.5 * log(DegreesOfFreedom * pi) -
			0.5 * (DegreesOfFreedom + 1.0) * log(1.0 + Value * Value / DegreesOfFreedom).

	standard_t_distribution(Value, DegreesOfFreedom, Probability) :-
		check_value_and_positive_number(Value, DegreesOfFreedom),
		X is DegreesOfFreedom / (DegreesOfFreedom + Value * Value),
		regularized_beta(X, DegreesOfFreedom / 2.0, 0.5, BetaProbability),
		( Value >= 0.0 -> Probability is 1.0 - 0.5 * BetaProbability; Probability is 0.5 * BetaProbability ).

	standard_t_quantile(Probability, DegreesOfFreedom, Quantile) :-
		check_probability_and_positive_number(Probability, DegreesOfFreedom),
		( Probability >= 0.5, Probability =< 0.5 ->
			Quantile = 0.0
		; bracket_symmetric_t(Probability, DegreesOfFreedom, -1.0, 1.0, Lower, Upper),
		  bisect_t(80, Probability, DegreesOfFreedom, Lower, Upper, Quantile)
		).

	t(Location, Scale, DegreesOfFreedom, Value) :-
		check_location_scale_degrees(Location, Scale, DegreesOfFreedom),
		standard_t(DegreesOfFreedom, StandardValue),
		Value is Location + Scale * StandardValue.

	t_samples(Count, Location, Scale, DegreesOfFreedom, Samples) :-
		context(Context),
		check(non_negative_integer, Count, Context),
		check_location_scale_degrees(Location, Scale, DegreesOfFreedom),
		generate_t_samples(Count, Location, Scale, DegreesOfFreedom, Samples).

	t_density(Value, Location, Scale, DegreesOfFreedom, Density) :-
		t_log_density(Value, Location, Scale, DegreesOfFreedom, LogDensity),
		Density is exp(LogDensity).

	t_log_density(Value, Location, Scale, DegreesOfFreedom, LogDensity) :-
		context(Context),
		check(number, Value, Context),
		check_location_scale_degrees(Location, Scale, DegreesOfFreedom),
		StandardValue is (Value - Location) / Scale,
		standard_t_log_density(StandardValue, DegreesOfFreedom, StandardLogDensity),
		LogDensity is StandardLogDensity - log(Scale).

	t_distribution(Value, Location, Scale, DegreesOfFreedom, Probability) :-
		context(Context),
		check(number, Value, Context),
		check_location_scale_degrees(Location, Scale, DegreesOfFreedom),
		StandardValue is (Value - Location) / Scale,
		standard_t_distribution(StandardValue, DegreesOfFreedom, Probability).

	t_quantile(Probability, Location, Scale, DegreesOfFreedom, Quantile) :-
		check_probability_and_positive_number(Probability, DegreesOfFreedom),
		context(Context),
		check(number, Location, Context),
		check(positive_number, Scale, Context),
		standard_t_quantile(Probability, DegreesOfFreedom, StandardQuantile),
		Quantile is Location + Scale * StandardQuantile.

	chi_squared(DegreesOfFreedom, Value) :-
		check_positive_number(DegreesOfFreedom),
		random_standard_gamma(DegreesOfFreedom / 2.0, Gamma),
		Value is 2.0 * Gamma.

	chi_squared_samples(Count, DegreesOfFreedom, Samples) :-
		check_count_and_positive_number(Count, DegreesOfFreedom),
		generate_chi_squared_samples(Count, DegreesOfFreedom, Samples).

	chi_squared_density(Value, DegreesOfFreedom, Density) :-
		chi_squared_log_density(Value, DegreesOfFreedom, LogDensity),
		( LogDensity == negative_infinity -> Density = 0.0; Density is exp(LogDensity) ).

	chi_squared_log_density(Value, DegreesOfFreedom, LogDensity) :-
		check_value_and_positive_number(Value, DegreesOfFreedom),
		( Value =< 0.0 ->
			LogDensity = negative_infinity
		; log_gamma(DegreesOfFreedom / 2.0, LogGamma),
			LogDensity is (DegreesOfFreedom / 2.0 - 1.0) * log(Value) - Value / 2.0 -
				(DegreesOfFreedom / 2.0) * log(2.0) - LogGamma
		).

	chi_squared_distribution(Value, DegreesOfFreedom, Probability) :-
		check_value_and_positive_number(Value, DegreesOfFreedom),
		( Value =< 0.0 -> Probability = 0.0; regularized_gamma_p(DegreesOfFreedom / 2.0, Value / 2.0, Probability) ).

	chi_squared_quantile(Probability, DegreesOfFreedom, Quantile) :-
		check_probability_and_positive_number(Probability, DegreesOfFreedom),
		bracket_positive(chi_squared, Probability, DegreesOfFreedom, unused, 1.0, Upper),
		bisect_positive(80, chi_squared, Probability, DegreesOfFreedom, unused, 0.0, Upper, Quantile).

	gamma(Shape, Scale, Value) :-
		check_two_positive_numbers(Shape, Scale),
		random_gamma(Shape, Scale, Value).

	gamma_samples(Count, Shape, Scale, Samples) :-
		check_count_and_two_positive_numbers(Count, Shape, Scale),
		generate_gamma_samples(Count, Shape, Scale, Samples).

	gamma_density(Value, Shape, Scale, Density) :-
		gamma_log_density(Value, Shape, Scale, LogDensity),
		( LogDensity == negative_infinity -> Density = 0.0; Density is exp(LogDensity) ).

	gamma_log_density(Value, Shape, Scale, LogDensity) :-
		check_value_and_two_positive_numbers(Value, Shape, Scale),
		( Value =< 0.0 ->
			LogDensity = negative_infinity
		; log_gamma(Shape, LogGamma),
			LogDensity is (Shape - 1.0) * log(Value) - Value / Scale - LogGamma - Shape * log(Scale)
		).

	gamma_distribution(Value, Shape, Scale, Probability) :-
		check_value_and_two_positive_numbers(Value, Shape, Scale),
		( Value =< 0.0 -> Probability = 0.0; regularized_gamma_p(Shape, Value / Scale, Probability) ).

	gamma_quantile(Probability, Shape, Scale, Quantile) :-
		check_probability_and_two_positive_numbers(Probability, Shape, Scale),
		bracket_positive(gamma, Probability, Shape, Scale, Scale, Upper),
		bisect_positive(80, gamma, Probability, Shape, Scale, 0.0, Upper, Quantile).

	beta(Alpha, Beta, Value) :-
		check_two_positive_numbers(Alpha, Beta),
		random_beta(Alpha, Beta, Value).

	beta_samples(Count, Alpha, Beta, Samples) :-
		check_count_and_two_positive_numbers(Count, Alpha, Beta),
		generate_beta_samples(Count, Alpha, Beta, Samples).

	beta_density(Value, Alpha, Beta, Density) :-
		beta_log_density(Value, Alpha, Beta, LogDensity),
		( LogDensity == negative_infinity -> Density = 0.0; Density is exp(LogDensity) ).

	beta_log_density(Value, Alpha, Beta, LogDensity) :-
		check_value_and_two_positive_numbers(Value, Alpha, Beta),
		( (Value =< 0.0; Value >= 1.0) ->
			LogDensity = negative_infinity
		; log_beta(Alpha, Beta, LogBeta),
			LogDensity is (Alpha - 1.0) * log(Value) + (Beta - 1.0) * log(1.0 - Value) - LogBeta
		).

	beta_distribution(Value, Alpha, Beta, Probability) :-
		check_value_and_two_positive_numbers(Value, Alpha, Beta),
		( Value =< 0.0 -> Probability = 0.0; Value >= 1.0 -> Probability = 1.0; regularized_beta(Value, Alpha, Beta, Probability) ).

	beta_quantile(Probability, Alpha, Beta, Quantile) :-
		check_probability_and_two_positive_numbers(Probability, Alpha, Beta),
		bisect_positive(80, beta, Probability, Alpha, Beta, 0.0, 1.0, Quantile).

	exponential(Scale, Value) :-
		check_positive_number(Scale),
		random_exponential(Scale, Value).

	exponential_samples(Count, Scale, Samples) :-
		check_count_and_positive_number(Count, Scale),
		generate_exponential_samples(Count, Scale, Samples).

	exponential_density(Value, Scale, Density) :-
		check_value_and_positive_number(Value, Scale),
		( Value < 0.0 -> Density = 0.0; Density is exp(-Value / Scale) / Scale ).

	exponential_log_density(Value, Scale, LogDensity) :-
		check_value_and_positive_number(Value, Scale),
		( Value < 0.0 -> LogDensity = negative_infinity; LogDensity is -Value / Scale - log(Scale) ).

	exponential_distribution(Value, Scale, Probability) :-
		check_value_and_positive_number(Value, Scale),
		( Value =< 0.0 -> Probability = 0.0; Probability is 1.0 - exp(-Value / Scale) ).

	exponential_quantile(Probability, Scale, Quantile) :-
		check_open_probability(Probability),
		check_positive_number(Scale),
		Quantile is -Scale * log(1.0 - Probability).

	fisher(DegreesOfFreedom1, DegreesOfFreedom2, Value) :-
		check_two_positive_numbers(DegreesOfFreedom1, DegreesOfFreedom2),
		random_standard_gamma(DegreesOfFreedom1 / 2.0, Gamma1),
		random_standard_gamma(DegreesOfFreedom2 / 2.0, Gamma2),
		Value is (Gamma1 / DegreesOfFreedom1) / (Gamma2 / DegreesOfFreedom2).

	fisher_samples(Count, DegreesOfFreedom1, DegreesOfFreedom2, Samples) :-
		check_count_and_two_positive_numbers(Count, DegreesOfFreedom1, DegreesOfFreedom2),
		generate_fisher_samples(Count, DegreesOfFreedom1, DegreesOfFreedom2, Samples).

	fisher_density(Value, DegreesOfFreedom1, DegreesOfFreedom2, Density) :-
		fisher_log_density(Value, DegreesOfFreedom1, DegreesOfFreedom2, LogDensity),
		( LogDensity == negative_infinity -> Density = 0.0; Density is exp(LogDensity) ).

	fisher_log_density(Value, DegreesOfFreedom1, DegreesOfFreedom2, LogDensity) :-
		check_value_and_two_positive_numbers(Value, DegreesOfFreedom1, DegreesOfFreedom2),
		( Value =< 0.0 ->
			LogDensity = negative_infinity
		; Half1 is DegreesOfFreedom1 / 2.0, Half2 is DegreesOfFreedom2 / 2.0,
			log_beta(Half1, Half2, LogBeta),
			LogDensity is Half1 * log(DegreesOfFreedom1 / DegreesOfFreedom2) + (Half1 - 1.0) * log(Value) -
				(Half1 + Half2) * log(1.0 + DegreesOfFreedom1 * Value / DegreesOfFreedom2) - LogBeta
		).

	fisher_distribution(Value, DegreesOfFreedom1, DegreesOfFreedom2, Probability) :-
		check_value_and_two_positive_numbers(Value, DegreesOfFreedom1, DegreesOfFreedom2),
		( Value =< 0.0 -> Probability = 0.0
		; X is DegreesOfFreedom1 * Value / (DegreesOfFreedom1 * Value + DegreesOfFreedom2),
		  regularized_beta(X, DegreesOfFreedom1 / 2.0, DegreesOfFreedom2 / 2.0, Probability)
		).

	fisher_quantile(Probability, DegreesOfFreedom1, DegreesOfFreedom2, Quantile) :-
		check_probability_and_two_positive_numbers(Probability, DegreesOfFreedom1, DegreesOfFreedom2),
		bracket_positive(fisher, Probability, DegreesOfFreedom1, DegreesOfFreedom2, 1.0, Upper),
		bisect_positive(80, fisher, Probability, DegreesOfFreedom1, DegreesOfFreedom2, 0.0, Upper, Quantile).

	generate_standard_normal_samples(0, []) :-
		!.
	generate_standard_normal_samples(Count, [Sample| Samples]) :-
		random_standard_normal(Sample),
		Remaining is Count - 1,
		generate_standard_normal_samples(Remaining, Samples).

	generate_normal_samples(0, _Mean, _Deviation, []) :-
		!.
	generate_normal_samples(Count, Mean, Deviation, [Sample| Samples]) :-
		random_normal(Mean, Deviation, Sample),
		Remaining is Count - 1,
		generate_normal_samples(Remaining, Mean, Deviation, Samples).

	generate_standard_t_samples(0, _DegreesOfFreedom, []) :- !.
	generate_standard_t_samples(Count, DegreesOfFreedom, [Sample| Samples]) :- standard_t(DegreesOfFreedom, Sample), Remaining is Count - 1, generate_standard_t_samples(Remaining, DegreesOfFreedom, Samples).
	generate_t_samples(0, _Location, _Scale, _DegreesOfFreedom, []) :- !.
	generate_t_samples(Count, Location, Scale, DegreesOfFreedom, [Sample| Samples]) :- t(Location, Scale, DegreesOfFreedom, Sample), Remaining is Count - 1, generate_t_samples(Remaining, Location, Scale, DegreesOfFreedom, Samples).
	generate_chi_squared_samples(0, _DegreesOfFreedom, []) :- !.
	generate_chi_squared_samples(Count, DegreesOfFreedom, [Sample| Samples]) :- chi_squared(DegreesOfFreedom, Sample), Remaining is Count - 1, generate_chi_squared_samples(Remaining, DegreesOfFreedom, Samples).
	generate_gamma_samples(0, _Shape, _Scale, []) :- !.
	generate_gamma_samples(Count, Shape, Scale, [Sample| Samples]) :- random_gamma(Shape, Scale, Sample), Remaining is Count - 1, generate_gamma_samples(Remaining, Shape, Scale, Samples).
	generate_beta_samples(0, _Alpha, _Beta, []) :- !.
	generate_beta_samples(Count, Alpha, Beta, [Sample| Samples]) :- random_beta(Alpha, Beta, Sample), Remaining is Count - 1, generate_beta_samples(Remaining, Alpha, Beta, Samples).
	generate_exponential_samples(0, _Scale, []) :- !.
	generate_exponential_samples(Count, Scale, [Sample| Samples]) :- random_exponential(Scale, Sample), Remaining is Count - 1, generate_exponential_samples(Remaining, Scale, Samples).
	generate_fisher_samples(0, _DegreesOfFreedom1, _DegreesOfFreedom2, []) :- !.
	generate_fisher_samples(Count, DegreesOfFreedom1, DegreesOfFreedom2, [Sample| Samples]) :- fisher(DegreesOfFreedom1, DegreesOfFreedom2, Sample), Remaining is Count - 1, generate_fisher_samples(Remaining, DegreesOfFreedom1, DegreesOfFreedom2, Samples).

	check_positive_number(Value) :- context(Context), check(positive_number, Value, Context).
	check_two_positive_numbers(Value1, Value2) :- context(Context), check(positive_number, Value1, Context), check(positive_number, Value2, Context).
	check_count_and_positive_number(Count, Value) :- context(Context), check(non_negative_integer, Count, Context), check(positive_number, Value, Context).
	check_count_and_two_positive_numbers(Count, Value1, Value2) :- context(Context), check(non_negative_integer, Count, Context), check(positive_number, Value1, Context), check(positive_number, Value2, Context).
	check_value_and_positive_number(Value, Parameter) :- context(Context), check(number, Value, Context), check(positive_number, Parameter, Context).
	check_value_and_two_positive_numbers(Value, Parameter1, Parameter2) :- context(Context), check(number, Value, Context), check(positive_number, Parameter1, Context), check(positive_number, Parameter2, Context).
	check_probability_and_positive_number(Probability, Parameter) :- check_open_probability(Probability), check_positive_number(Parameter).
	check_probability_and_two_positive_numbers(Probability, Parameter1, Parameter2) :- check_open_probability(Probability), check_two_positive_numbers(Parameter1, Parameter2).
	check_open_probability(Probability) :- ( \+ number(Probability) -> type_error(number, Probability); Probability > 0.0, Probability < 1.0 -> true; domain_error(open_probability, Probability) ).
	check_location_scale_degrees(Location, Scale, DegreesOfFreedom) :- context(Context), check(number, Location, Context), check(positive_number, Scale, Context), check(positive_number, DegreesOfFreedom, Context).

	distribution_value(chi_squared, Value, Parameter, _Unused, Probability) :- chi_squared_distribution(Value, Parameter, Probability).
	distribution_value(gamma, Value, Shape, Scale, Probability) :- gamma_distribution(Value, Shape, Scale, Probability).
	distribution_value(beta, Value, Alpha, Beta, Probability) :- beta_distribution(Value, Alpha, Beta, Probability).
	distribution_value(fisher, Value, DegreesOfFreedom1, DegreesOfFreedom2, Probability) :- fisher_distribution(Value, DegreesOfFreedom1, DegreesOfFreedom2, Probability).

	bracket_positive(Distribution, Probability, Parameter1, Parameter2, Upper0, Upper) :-
		distribution_value(Distribution, Upper0, Parameter1, Parameter2, CurrentProbability),
		( CurrentProbability >= Probability -> Upper = Upper0; Upper1 is Upper0 * 2.0, bracket_positive(Distribution, Probability, Parameter1, Parameter2, Upper1, Upper) ).

	bisect_positive(0, _Distribution, _Probability, _Parameter1, _Parameter2, Lower, Upper, Quantile) :- !, Quantile is (Lower + Upper) / 2.0.
	bisect_positive(Iterations, Distribution, Probability, Parameter1, Parameter2, Lower, Upper, Quantile) :-
		Middle is (Lower + Upper) / 2.0,
		distribution_value(Distribution, Middle, Parameter1, Parameter2, MiddleProbability),
		Remaining is Iterations - 1,
		( MiddleProbability < Probability -> bisect_positive(Remaining, Distribution, Probability, Parameter1, Parameter2, Middle, Upper, Quantile); bisect_positive(Remaining, Distribution, Probability, Parameter1, Parameter2, Lower, Middle, Quantile) ).

	bracket_symmetric_t(Probability, DegreesOfFreedom, Lower0, Upper0, Lower, Upper) :-
		standard_t_distribution(Lower0, DegreesOfFreedom, LowerProbability),
		standard_t_distribution(Upper0, DegreesOfFreedom, UpperProbability),
		( LowerProbability =< Probability, Probability =< UpperProbability -> Lower = Lower0, Upper = Upper0
		; Lower1 is Lower0 * 2.0, Upper1 is Upper0 * 2.0, bracket_symmetric_t(Probability, DegreesOfFreedom, Lower1, Upper1, Lower, Upper)
		).

	bisect_t(0, _Probability, _DegreesOfFreedom, Lower, Upper, Quantile) :- !, Quantile is (Lower + Upper) / 2.0.
	bisect_t(Iterations, Probability, DegreesOfFreedom, Lower, Upper, Quantile) :-
		Middle is (Lower + Upper) / 2.0,
		standard_t_distribution(Middle, DegreesOfFreedom, MiddleProbability),
		Remaining is Iterations - 1,
		( MiddleProbability < Probability -> bisect_t(Remaining, Probability, DegreesOfFreedom, Middle, Upper, Quantile); bisect_t(Remaining, Probability, DegreesOfFreedom, Lower, Middle, Quantile) ).

	regularized_gamma_p(_Shape, X, 0.0) :- X =< 0.0, !.
	regularized_gamma_p(Shape, X, Probability) :-
		log_gamma(Shape, LogGamma),
		( X < Shape + 1.0 -> gamma_series(Shape, X, 1, 1.0 / Shape, 1.0 / Shape, Series), Probability is Series * exp(-X + Shape * log(X) - LogGamma)
		; gamma_fraction(Shape, X, 1, X + 1.0 - Shape, 1.0 / 1.0e-300, 1.0 / (X + 1.0 - Shape), 1.0 / (X + 1.0 - Shape), Fraction), Probability is 1.0 - exp(-X + Shape * log(X) - LogGamma) * Fraction
		).

	gamma_series(_Shape, _X, 200, _Term, Sum, Sum) :- !.
	gamma_series(Shape, X, Index, Term0, Sum0, Sum) :-
		Term is Term0 * X / (Shape + Index), Sum1 is Sum0 + Term,
		( abs(Term) =< abs(Sum1) * 1.0e-14 -> Sum = Sum1; Next is Index + 1, gamma_series(Shape, X, Next, Term, Sum1, Sum) ).

	gamma_fraction(_Shape, _X, 200, _B, _C, _D, H, H) :- !.
	gamma_fraction(Shape, X, Index, B0, C0, D0, H0, H) :-
		A is -Index * (Index - Shape), B is B0 + 2.0,
		D1 is A * D0 + B, safe_nonzero(D1, D2), D is 1.0 / D2,
		C1 is B + A / C0, safe_nonzero(C1, C), Delta is D * C, H1 is H0 * Delta,
		( abs(Delta - 1.0) =< 1.0e-14 -> H = H1; Next is Index + 1, gamma_fraction(Shape, X, Next, B, C, D, H1, H) ).

	regularized_beta(X, _Alpha, _Beta, 0.0) :- X =< 0.0, !.
	regularized_beta(X, _Alpha, _Beta, 1.0) :- X >= 1.0, !.
	regularized_beta(X, Alpha, Beta, Probability) :-
		log_beta(Alpha, Beta, LogBeta), Front is exp(Alpha * log(X) + Beta * log(1.0 - X) - LogBeta),
		( X < (Alpha + 1.0) / (Alpha + Beta + 2.0) -> beta_fraction(Alpha, Beta, X, Fraction), Probability is Front * Fraction / Alpha
		; beta_fraction(Beta, Alpha, 1.0 - X, Fraction), Probability is 1.0 - Front * Fraction / Beta
		).

	beta_fraction(Alpha, Beta, X, Fraction) :-
		D0 is 1.0 - (Alpha + Beta) * X / (Alpha + 1.0), safe_nonzero(D0, D1), D is 1.0 / D1,
		beta_fraction(Alpha, Beta, X, 1, 1.0, D, D, Fraction).
	beta_fraction(_Alpha, _Beta, _X, 200, _C, _D, H, H) :- !.
	beta_fraction(Alpha, Beta, X, Index, C0, D0, H0, Fraction) :-
		M2 is 2 * Index,
		AA is Index * (Beta - Index) * X / ((Alpha + M2 - 1.0) * (Alpha + M2)),
		D1 is 1.0 + AA * D0, safe_nonzero(D1, D2), D3 is 1.0 / D2,
		C1 is 1.0 + AA / C0, safe_nonzero(C1, C2), H1 is H0 * D3 * C2,
		AA2 is -(Alpha + Index) * (Alpha + Beta + Index) * X / ((Alpha + M2) * (Alpha + M2 + 1.0)),
		D4 is 1.0 + AA2 * D3, safe_nonzero(D4, D5), D is 1.0 / D5,
		C3 is 1.0 + AA2 / C2, safe_nonzero(C3, C), Delta is D * C, H is H1 * Delta,
		( abs(Delta - 1.0) =< 1.0e-14 -> Fraction = H; Next is Index + 1, beta_fraction(Alpha, Beta, X, Next, C, D, H, Fraction) ).

	safe_nonzero(Value, SafeValue) :- ( abs(Value) < 1.0e-300 -> ( Value < 0.0 -> SafeValue = -1.0e-300; SafeValue = 1.0e-300 ); SafeValue = Value ).

	log_beta(Alpha, Beta, LogBeta) :- log_gamma(Alpha, LogAlpha), log_gamma(Beta, LogBeta0), log_gamma(Alpha + Beta, LogSum), LogBeta is LogAlpha + LogBeta0 - LogSum.
	log_gamma(Value, LogGamma) :-
		( Value < 0.5 -> log_gamma(1.0 - Value, Reflected), LogGamma is log(pi) - log(sin(pi * Value)) - Reflected
		; Shifted is Value - 1.0, lanczos_sum(Shifted, Sum), T is Shifted + 7.5, LogGamma is 0.5 * log(2.0 * pi) + (Shifted + 0.5) * log(T) - T + log(Sum)
		).
	lanczos_sum(Value, Sum) :- lanczos_coefficients(Coefficients), lanczos_sum(Coefficients, Value, 1, 0.99999999999980993, Sum).
	lanczos_sum([], _Value, _Index, Sum, Sum).
	lanczos_sum([Coefficient| Coefficients], Value, Index, Sum0, Sum) :- Sum1 is Sum0 + Coefficient / (Value + Index), Next is Index + 1, lanczos_sum(Coefficients, Value, Next, Sum1, Sum).
	lanczos_coefficients([676.5203681218851, -1259.1392167224028, 771.32342877765313, -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7]).

	check_normal_evaluation_arguments(Value, Mean, Deviation) :-
		context(Context),
		check(number, Value, Context),
		check(number, Mean, Context),
		check(positive_number, Deviation, Context).

	standard_normal_distribution_unchecked(Value, Probability) :-
		AbsoluteValue is abs(Value),
		T is 1.0 / (1.0 + 0.2316419 * AbsoluteValue),
		Polynomial is T * (0.319381530 + T * (-0.356563782 + T * (1.781477937 + T * (-1.821255978 + T * 1.330274429)))),
		standard_normal_density(AbsoluteValue, Density),
		UpperProbability is Density * Polynomial,
		(	Value >= 0.0 ->
			Probability is 1.0 - UpperProbability
		;	Probability = UpperProbability
		).

	standard_normal_quantile_unchecked(Probability, Quantile) :-
		Plow = 0.02425,
		Phigh is 1.0 - Plow,
		(	Probability < Plow ->
			Q is sqrt(-2.0 * log(Probability)),
			inverse_tail(Q, Quantile)
		;	Probability =< Phigh ->
			Q is Probability - 0.5,
			R is Q * Q,
			inverse_central(Q, R, Quantile)
		;	Q is sqrt(-2.0 * log(1.0 - Probability)),
			inverse_tail(Q, TailQuantile),
			Quantile is -TailQuantile
		).

	inverse_tail(Q, Quantile) :-
		Numerator is (((((-0.007784894002430293 * Q - 0.3223964580411365) * Q - 2.400758277161838) * Q - 2.549732539343734) * Q + 4.374664141464968) * Q + 2.938163982698783),
		Denominator is ((((0.007784695709041462 * Q + 0.3224671290700398) * Q + 2.445134137142996) * Q + 3.754408661907416) * Q + 1.0),
		Quantile is Numerator / Denominator.

	inverse_central(Q, R, Quantile) :-
		Numerator is (((((-39.69683028665376 * R + 220.9460984245205) * R - 275.9285104469687) * R + 138.3577518672690) * R - 30.66479806614716) * R + 2.506628277459239) * Q,
		Denominator is (((((-54.47609879822406 * R + 161.5858368580409) * R - 155.6989798598866) * R + 66.80131188771972) * R - 13.28068155288572) * R + 1.0),
		Quantile is Numerator / Denominator.

:- end_object.
