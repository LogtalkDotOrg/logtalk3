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


:- protocol(univariate_distributions_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-11,
		comment is 'Univariate probability distribution predicates.',
		see_also is [univariate_distributions(_), multivariate_distributions_protocol, sampling_protocol]
	]).

	:- public(standard_normal/1).
	:- mode(standard_normal(-float), one).
	:- info(standard_normal/1, [
		comment is 'Returns a standard normally distributed random value.',
		argnames is ['Value']
	]).

	:- public(standard_normal_samples/2).
	:- mode(standard_normal_samples(+integer, -list(float)), one_or_error).
	:- info(standard_normal_samples/2, [
		comment is 'Returns the requested number of standard normally distributed random values.',
		argnames is ['Count', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count')
		]
	]).

	:- public(normal/3).
	:- mode(normal(+float, +non_negative_float, -float), one_or_error).
	:- info(normal/3, [
		comment is 'Returns a normally distributed random value with the given mean and standard deviation. A zero deviation returns the mean.',
		argnames is ['Mean', 'Deviation', 'Value'],
		exceptions is [
			'``Mean`` is a variable' - instantiation_error,
			'``Mean`` is neither a variable nor a float' - type_error(float, 'Mean'),
			'``Deviation`` is a variable' - instantiation_error,
			'``Deviation`` is neither a variable nor a float' - type_error(float, 'Deviation'),
			'``Deviation`` is a negative float' - domain_error(non_negative_float, 'Deviation')
		]
	]).

	:- public(normal_samples/4).
	:- mode(normal_samples(+integer, +float, +non_negative_float, -list(float)), one_or_error).
	:- info(normal_samples/4, [
		comment is 'Returns the requested number of normally distributed random values with the given mean and standard deviation.',
		argnames is ['Count', 'Mean', 'Deviation', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Mean`` is a variable' - instantiation_error,
			'``Mean`` is neither a variable nor a float' - type_error(float, 'Mean'),
			'``Deviation`` is a variable' - instantiation_error,
			'``Deviation`` is neither a variable nor a float' - type_error(float, 'Deviation'),
			'``Deviation`` is a negative float' - domain_error(non_negative_float, 'Deviation')
		]
	]).

	:- public(standard_normal_density/2).
	:- mode(standard_normal_density(+number, -float), one_or_error).
	:- info(standard_normal_density/2, [
		comment is 'Computes the standard normal probability density at the given value.',
		argnames is ['Value', 'Density'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value')
		]
	]).

	:- public(standard_normal_log_density/2).
	:- mode(standard_normal_log_density(+number, -float), one_or_error).
	:- info(standard_normal_log_density/2, [
		comment is 'Computes the standard normal log-density at the given value.',
		argnames is ['Value', 'LogDensity'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value')
		]
	]).

	:- public(standard_normal_distribution/2).
	:- mode(standard_normal_distribution(+number, -float), one_or_error).
	:- info(standard_normal_distribution/2, [
		comment is 'Computes an approximation of the standard normal cumulative distribution at the given value.',
		argnames is ['Value', 'Probability'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value')
		]
	]).

	:- public(standard_normal_quantile/2).
	:- mode(standard_normal_quantile(+number, -float), one_or_error).
	:- info(standard_normal_quantile/2, [
		comment is 'Computes an approximation of the standard normal quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'Quantile'],
		exceptions is [
			'``Probability`` is not a number' - type_error(number, 'Probability'),
			'``Probability`` is not strictly between zero and one' - domain_error(open_probability, 'Probability')
		]
	]).

	:- public(normal_density/4).
	:- mode(normal_density(+number, +number, +positive_number, -float), one_or_error).
	:- info(normal_density/4, [
		comment is 'Computes the normal probability density at a value for the given mean and positive standard deviation.',
		argnames is ['Value', 'Mean', 'Deviation', 'Density'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value'),
			'``Mean`` is not a number' - type_error(number, 'Mean'),
			'``Deviation`` is not a number' - type_error(number, 'Deviation'),
			'``Deviation`` is not positive' - domain_error(positive_number, 'Deviation')
		]
	]).

	:- public(normal_log_density/4).
	:- mode(normal_log_density(+number, +number, +positive_number, -float), one_or_error).
	:- info(normal_log_density/4, [
		comment is 'Computes the normal log-density at a value for the given mean and positive standard deviation.',
		argnames is ['Value', 'Mean', 'Deviation', 'LogDensity'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value'),
			'``Mean`` is not a number' - type_error(number, 'Mean'),
			'``Deviation`` is not a number' - type_error(number, 'Deviation'),
			'``Deviation`` is not positive' - domain_error(positive_number, 'Deviation')
		]
	]).

	:- public(normal_distribution/4).
	:- mode(normal_distribution(+number, +number, +positive_number, -float), one_or_error).
	:- info(normal_distribution/4, [
		comment is 'Computes an approximation of the normal cumulative distribution at a value for the given mean and positive standard deviation.',
		argnames is ['Value', 'Mean', 'Deviation', 'Probability'],
		exceptions is [
			'``Value`` is not a number' - type_error(number, 'Value'),
			'``Mean`` is not a number' - type_error(number, 'Mean'),
			'``Deviation`` is not a number' - type_error(number, 'Deviation'),
			'``Deviation`` is not positive' - domain_error(positive_number, 'Deviation')
		]
	]).

	:- public(normal_quantile/4).
	:- mode(normal_quantile(+number, +number, +positive_number, -float), one_or_error).
	:- info(normal_quantile/4, [
		comment is 'Computes an approximation of the normal quantile for a probability strictly between zero and one and the given mean and positive standard deviation.',
		argnames is ['Probability', 'Mean', 'Deviation', 'Quantile'],
		exceptions is [
			'``Probability`` is not a number' - type_error(number, 'Probability'),
			'``Probability`` is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'``Mean`` is not a number' - type_error(number, 'Mean'),
			'``Deviation`` is not a number' - type_error(number, 'Deviation'),
			'``Deviation`` is not positive' - domain_error(positive_number, 'Deviation')
		]
	]).

:- end_protocol.
