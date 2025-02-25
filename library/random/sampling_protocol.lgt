%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(sampling_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-02-25,
		comment is 'Predicates for sampling probability distributions.',
		see_also is [random_protocol, pseudo_random_protocol]
	]).

	:- public(normal/3).
	:- mode(normal(+float, +non_negative_float, -float), one).
	:- info(normal/3, [
		comment is 'Returns a scaled normally (Gaussian) distributed random value with the given mean and standard deviation.',
		argnames is ['Mean', 'Deviation', 'Value']
	]).

	:- public(lognormal/3).
	:- mode(lognormal(+float, +non_negative_float, -float), one).
	:- info(lognormal/3, [
		comment is 'Returns a scaled log normally distributed random value with the given mean and standard deviation for the normal distribution.',
		argnames is ['Mean', 'Deviation', 'Value']
	]).

	:- public(wald/3).
	:- mode(wald(+positive_float, +positive_float, -float), one).
	:- info(wald/3, [
		comment is 'Returns a scaled Wald (inverse Gaussian) distributed random value with the given mean.',
		argnames is ['Mean', 'Scale', 'Value']
	]).

	:- public(chi_squared/2).
	:- mode(chi_squared(+positive_integer, -float), one).
	:- info(chi_squared/2, [
		comment is 'Returns a chi-squared distributed random value given the degrees of freedom.',
		argnames is ['DegreesOfFreedom', 'Value']
	]).

	:- public(standard_t/2).
	:- mode(standard_t(+positive_integer, -float), one).
	:- info(standard_t/2, [
		comment is 'Returns a standard Student\'s t distributed random value given the degrees of freedom.',
		argnames is ['DegreesOfFreedom', 'Value']
	]).

	:- public(standard_cauchy/3).
	:- mode(standard_cauchy(+float, +float, -float), one).
	:- info(standard_cauchy/3, [
		comment is 'Returns a standard Cauchy distributed random value.',
		argnames is ['Location', 'Scale', 'Value']
	]).

	:- public(standard_exponential/1).
	:- mode(standard_exponential(-float), one).
	:- info(standard_exponential/1, [
		comment is 'Returns a standard exponential distributed random value.',
		argnames is ['Value']
	]).

	:- public(standard_gamma/2).
	:- mode(standard_gamma(+positive_float, -float), one).
	:- info(standard_gamma/2, [
		comment is 'Returns a standard gamma distributed random value.',
		argnames is ['Shape', 'Value']
	]).

	:- public(standard_normal/1).
	:- mode(standard_normal(-float), one).
	:- info(standard_normal/1, [
		comment is 'Returns a standard normally (Gaussian) distributed random value (using a default mean of 0.0 and a default deviation of 1.0).',
		argnames is ['Value']
	]).

	:- public(fisher/3).
	:- mode(fisher(+positive_integer, +positive_integer, -float), one).
	:- info(fisher/3, [
		comment is 'Returns a Fisher distributed random value given the degrees of freedom in the numerator and in the denominator.',
		argnames is ['DegreesOfFreedomNumerator', 'DegreesOfFreedomDenominator', 'Value']
	]).

	:- public(logseries/2).
	:- mode(logseries(+non_negative_integer, -positive_integer), zero_or_one).
	:- info(logseries/2, [
		comment is 'Returns a logseries distributed random value. Requires ``0.0 < Shape < 1`` and fails otherwise.',
		argnames is ['Shape', 'Value']
	]).

	:- public(geometric/2).
	:- mode(geometric(+probability, -positive_integer), one).
	:- info(geometric/2, [
		comment is 'Returns a geometric distributed random value (trials until the first success).',
		argnames is ['Probability', 'Value']
	]).

	:- public(hypergeometric/4).
	:- mode(hypergeometric(+non_negative_integer, +non_negative_integer, +non_negative_integer, -non_negative_integer), one).
	:- info(hypergeometric/4, [
		comment is 'Returns a hypergeometric distributed random value.',
		argnames is ['Population', 'Successes', 'Draws', 'Value']
	]).

	:- public(exponential/2).
	:- mode(exponential(+positive_float, -float), one).
	:- info(exponential/2, [
		comment is 'Returns an exponentially distributed random value.',
		argnames is ['Lambda', 'Value']
	]).

	:- public(binomial/3).
	:- mode(binomial(+positive_integer, +positive_float, -float), one).
	:- info(binomial/3, [
		comment is 'Returns a binomial distributed random value.',
		argnames is ['Trials', 'Probability', 'Value']
	]).

	:- public(bernoulli/2).
	:- mode(bernoulli(+positive_integer, -float), one).
	:- info(bernoulli/2, [
		comment is 'Returns a Bernoulli distributed random value.',
		argnames is ['Probability', 'Value']
	]).

	:- public(beta/3).
	:- mode(beta(+positive_float, +positive_float, -float), one).
	:- info(beta/3, [
		comment is 'Returns a beta distributed random value.',
		argnames is ['Alpha', 'Beta', 'Value']
	]).

	:- public(gamma/3).
	:- mode(gamma(+positive_float, +positive_float, -float), one).
	:- info(gamma/3, [
		comment is 'Returns a scaled gamma distributed random value.',
		argnames is ['Shape', 'Scale', 'Value']
	]).

	:- public(logistic/3).
	:- mode(logistic(+float, +positive_float, -float), one).
	:- info(logistic/3, [
		comment is 'Returns a scaled logistic distributed random value.',
		argnames is ['Location', 'Scale', 'Value']
	]).

	:- public(logistic/2).
	:- mode(logistic(+float, -float), one).
	:- info(logistic/2, [
		comment is 'Returns a logistic distributed random value.',
		argnames is ['Location', 'Value']
	]).

	:- public(logistic/1).
	:- mode(logistic(-float), one).
	:- info(logistic/1, [
		comment is 'Returns a logistic distributed random value.',
		argnames is ['Value']
	]).

	:- public(poisson/2).
	:- mode(poisson(+non_negative_float, -non_negative_integer), one).
	:- info(poisson/2, [
		comment is 'Returns a Poisson distributed random value given the expected number of events.',
		argnames is ['Mean', 'Value']
	]).

	:- public(power/2).
	:- mode(power(+positive_float, -float), one).
	:- info(power/2, [
		comment is 'Returns a power distributed random value.',
		argnames is ['Exponent', 'Value']
	]).

	:- public(weibull/3).
	:- mode(weibull(+float, +positive_float, -float), one).
	:- info(weibull/3, [
		comment is 'Returns a scaled Weibull distributed random value.',
		argnames is ['Lambda', 'Shape', 'Value']
	]).

	:- public(weibull/2).
	:- mode(weibull(+positive_float, -float), one).
	:- info(weibull/2, [
		comment is 'Returns a Weibull distributed random value.',
		argnames is ['Shape', 'Value']
	]).

	:- public(uniform/3).
	:- mode(uniform(+float, +float, -float), zero_or_one).
	:- info(uniform/3, [
		comment is 'Returns a uniform distributed random value in the interval``[Lower, Upper[``. Fails if ``Lower`` or ``Upper`` are not integers or if ``Lower > Upper``. Same as ``random/3``.',
		argnames is ['Lower', 'Upper', 'Value']
	]).

	:- public(uniform/1).
	:- mode(uniform(-float), one).
	:- info(uniform/1, [
		comment is 'Returns a uniform distributed random value in the interval``[0.0, 1.0[``. Same as ``random/1``.',
		argnames is ['Value']
	]).

	:- public(triangular/4).
	:- mode(triangular(+float, +float, +float, -float), zero_or_one).
	:- info(triangular/4, [
		comment is 'Returns a triangular distributed random value. Fails if the ``Left =< Mode =< Right`` condition does not hold.',
		argnames is ['Left', 'Mode', 'Right', 'Value']
	]).

	:- public(von_mises/3).
	:- mode(von_mises(+float, +non_negative_float, -float), zero_or_one).
	:- info(von_mises/3, [
		comment is 'Returns a von Mises distributed random value.',
		argnames is ['Mode', 'Concentration', 'Value']
	]).

	:- public(gumbel/3).
	:- mode(gumbel(+float, +non_negative_float, -float), zero_or_one).
	:- info(gumbel/3, [
		comment is 'Returns a Gumbel distributed random value.',
		argnames is ['Location', 'Scale', 'Value']
	]).

	:- public(dirichlet/2).
	:- mode(dirichlet(+list(positive_float), -list(positive_float)), one).
	:- info(dirichlet/2, [
		comment is 'Returns a Dirichlet distributed list of random values.',
		argnames is ['Alphas', 'Thetas']
	]).

	:- public(circular_uniform_polar/3).
	:- mode(circular_uniform_polar(+float, +float, -float), one).
	:- info(circular_uniform_polar/3, [
		comment is 'Returns a circular uniform distributed random point in polar coordinates given the circle radius.',
		argnames is ['Radius', 'Rho', 'Theta']
	]).

	:- public(circular_uniform_cartesian/3).
	:- mode(circular_uniform_cartesian(+float, +float, -float), one).
	:- info(circular_uniform_cartesian/3, [
		comment is 'Returns a circular uniform distributed random point in cartesian coordinates given the circle radius.',
		argnames is ['Radius', 'X', 'Y']
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			random/1, random/3,
			sequence/4, set/4, permutation/2,
			randseq/4, randset/4
		]).
	:- endif.

:- end_protocol.
