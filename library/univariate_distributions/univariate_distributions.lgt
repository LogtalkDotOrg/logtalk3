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
		standard_normal/1 as random_standard_normal/1, normal/3 as random_normal/3
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
