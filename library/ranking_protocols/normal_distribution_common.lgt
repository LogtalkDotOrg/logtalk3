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


:- category(normal_distribution_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'Shared portable standard normal distribution numeric helpers.'
	]).

	:- protected(standard_normal_density/2).
	:- mode(standard_normal_density(+number, -float), one_or_error).
	:- info(standard_normal_density/2, [
		comment is 'Computes the standard normal probability density at the given value.',
		argnames is ['Value', 'Density'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value')
		]
	]).

	:- protected(standard_normal_distribution/2).
	:- mode(standard_normal_distribution(+number, -float), one_or_error).
	:- info(standard_normal_distribution/2, [
		comment is 'Computes an approximation of the standard normal cumulative distribution at the given value.',
		argnames is ['Value', 'Probability'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value')
		]
	]).

	:- protected(standard_normal_quantile/2).
	:- mode(standard_normal_quantile(+number, -float), one_or_error).
	:- info(standard_normal_quantile/2, [
		comment is 'Computes an approximation of the standard normal quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'Quantile'],
		exceptions is [
			'``Probability`` is not a number' - type_error(number, 'Probability'),
			'``Probability`` is not strictly between zero and one' - domain_error(open_probability, 'Probability')
		]
	]).

	standard_normal_density(Value, Density) :-
		(	number(Value) ->
			Density is exp(-0.5 * Value * Value) / sqrt(2.0 * pi)
		;	type_error(number, Value)
		).

	standard_normal_distribution(Value, Probability) :-
		(	number(Value) ->
			standard_normal_distribution_unchecked(Value, Probability)
		;	type_error(number, Value)
		).

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

	standard_normal_quantile(Probability, Quantile) :-
		(\+	number(Probability) ->
			type_error(number, Probability)
		;	Probability > 0.0,
			Probability < 1.0 ->
			standard_normal_quantile_unchecked(Probability, Quantile)
		;	domain_error(open_probability, Probability)
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

:- end_category.
