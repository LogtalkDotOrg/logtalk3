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

	%----------------------------------------------------------------------
	% Normal
	%----------------------------------------------------------------------

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

	%----------------------------------------------------------------------
	% Student's t
	%----------------------------------------------------------------------

	:- public(standard_t/2).
	:- mode(standard_t(+positive_number, -float), one_or_error).
	:- info(standard_t/2, [
		comment is 'Returns a standard Student\'s t distributed random value.',
		argnames is ['DegreesOfFreedom', 'Value'],
		exceptions is [
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(standard_t_samples/3).
	:- mode(standard_t_samples(+integer, +positive_number, -list(float)), one_or_error).
	:- info(standard_t_samples/3, [
		comment is 'Returns the requested number of standard Student\'s t distributed random values.',
		argnames is ['Count', 'DegreesOfFreedom', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(standard_t_density/3).
	:- mode(standard_t_density(+number, +positive_number, -float), one_or_error).
	:- info(standard_t_density/3, [
		comment is 'Computes the standard Student\'s t probability density at the given value.',
		argnames is ['Value', 'DegreesOfFreedom', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(standard_t_log_density/3).
	:- mode(standard_t_log_density(+number, +positive_number, -float), one_or_error).
	:- info(standard_t_log_density/3, [
		comment is 'Computes the standard Student\'s t log-density at the given value.',
		argnames is ['Value', 'DegreesOfFreedom', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(standard_t_distribution/3).
	:- mode(standard_t_distribution(+number, +positive_number, -float), one_or_error).
	:- info(standard_t_distribution/3, [
		comment is 'Computes an approximation of the standard Student\'s t cumulative distribution at the given value.',
		argnames is ['Value', 'DegreesOfFreedom', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(standard_t_quantile/3).
	:- mode(standard_t_quantile(+number, +positive_number, -float), one_or_error).
	:- info(standard_t_quantile/3, [
		comment is 'Computes an approximation of the standard Student\'s t quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'DegreesOfFreedom', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(t/4).
	:- mode(t(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(t/4, [
		comment is 'Returns a location-scale Student\'s t distributed random value.',
		argnames is ['Location', 'Scale', 'DegreesOfFreedom', 'Value'],
		exceptions is [
			'Location is not a number' - type_error(number, 'Location'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(t_samples/5).
	:- mode(t_samples(+integer, +number, +positive_number, +positive_number, -list(float)), one_or_error).
	:- info(t_samples/5, [
		comment is 'Returns the requested number of location-scale Student\'s t distributed random values.',
		argnames is ['Count', 'Location', 'Scale', 'DegreesOfFreedom', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'Location is not a number' - type_error(number, 'Location'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(t_density/5).
	:- mode(t_density(+number, +number, +positive_number, +positive_number, -float), one_or_error).
	:- info(t_density/5, [
		comment is 'Computes the location-scale Student\'s t probability density.',
		argnames is ['Value', 'Location', 'Scale', 'DegreesOfFreedom', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Location is not a number' - type_error(number, 'Location'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(t_log_density/5).
	:- mode(t_log_density(+number, +number, +positive_number, +positive_number, -float), one_or_error).
	:- info(t_log_density/5, [
		comment is 'Computes the location-scale Student\'s t log-density.',
		argnames is ['Value', 'Location', 'Scale', 'DegreesOfFreedom', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Location is not a number' - type_error(number, 'Location'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(t_distribution/5).
	:- mode(t_distribution(+number, +number, +positive_number, +positive_number, -float), one_or_error).
	:- info(t_distribution/5, [
		comment is 'Computes an approximation of the location-scale Student\'s t cumulative distribution.',
		argnames is ['Value', 'Location', 'Scale', 'DegreesOfFreedom', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Location is not a number' - type_error(number, 'Location'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(t_quantile/5).
	:- mode(t_quantile(+number, +number, +positive_number, +positive_number, -float), one_or_error).
	:- info(t_quantile/5, [
		comment is 'Computes an approximation of the location-scale Student\'s t quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'Location', 'Scale', 'DegreesOfFreedom', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'Location is not a number' - type_error(number, 'Location'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	%----------------------------------------------------------------------
	% Chi-squared
	%----------------------------------------------------------------------

	:- public(chi_squared/2).
	:- mode(chi_squared(+positive_number, -float), one_or_error).
	:- info(chi_squared/2, [
		comment is 'Returns a chi-squared distributed random value.',
		argnames is ['DegreesOfFreedom', 'Value'],
		exceptions is [
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(chi_squared_samples/3).
	:- mode(chi_squared_samples(+integer, +positive_number, -list(float)), one_or_error).
	:- info(chi_squared_samples/3, [
		comment is 'Returns the requested number of chi-squared distributed random values.',
		argnames is ['Count', 'DegreesOfFreedom', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(chi_squared_density/3).
	:- mode(chi_squared_density(+number, +positive_number, -float), one_or_error).
	:- info(chi_squared_density/3, [
		comment is 'Computes the chi-squared probability density at the given value.',
		argnames is ['Value', 'DegreesOfFreedom', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(chi_squared_log_density/3).
	:- mode(chi_squared_log_density(+number, +positive_number, -float), one_or_error).
	:- info(chi_squared_log_density/3, [
		comment is 'Computes the chi-squared log-density at the given value.',
		argnames is ['Value', 'DegreesOfFreedom', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(chi_squared_distribution/3).
	:- mode(chi_squared_distribution(+number, +positive_number, -float), one_or_error).
	:- info(chi_squared_distribution/3, [
		comment is 'Computes an approximation of the chi-squared cumulative distribution at the given value.',
		argnames is ['Value', 'DegreesOfFreedom', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	:- public(chi_squared_quantile/3).
	:- mode(chi_squared_quantile(+number, +positive_number, -float), one_or_error).
	:- info(chi_squared_quantile/3, [
		comment is 'Computes an approximation of the chi-squared quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'DegreesOfFreedom', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'DegreesOfFreedom is a variable' - instantiation_error,
			'DegreesOfFreedom is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'DegreesOfFreedom is not positive' - domain_error(positive_number, 'DegreesOfFreedom')
		]
	]).

	%----------------------------------------------------------------------
	% Gamma (shape-scale)
	%----------------------------------------------------------------------

	:- public(gamma/3).
	:- mode(gamma(+positive_number, +positive_number, -float), one_or_error).
	:- info(gamma/3, [
		comment is 'Returns a gamma distributed random value (shape-scale parameterization).',
		argnames is ['Shape', 'Scale', 'Value'],
		exceptions is [
			'Shape is a variable' - instantiation_error,
			'Shape is neither a variable nor a number' - type_error(number, 'Shape'),
			'Shape is not positive' - domain_error(positive_number, 'Shape'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(gamma_samples/4).
	:- mode(gamma_samples(+integer, +positive_number, +positive_number, -list(float)), one_or_error).
	:- info(gamma_samples/4, [
		comment is 'Returns the requested number of gamma distributed random values.',
		argnames is ['Count', 'Shape', 'Scale', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'Shape is a variable' - instantiation_error,
			'Shape is neither a variable nor a number' - type_error(number, 'Shape'),
			'Shape is not positive' - domain_error(positive_number, 'Shape'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(gamma_density/4).
	:- mode(gamma_density(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(gamma_density/4, [
		comment is 'Computes the gamma probability density at the given value.',
		argnames is ['Value', 'Shape', 'Scale', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Shape is a variable' - instantiation_error,
			'Shape is neither a variable nor a number' - type_error(number, 'Shape'),
			'Shape is not positive' - domain_error(positive_number, 'Shape'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(gamma_log_density/4).
	:- mode(gamma_log_density(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(gamma_log_density/4, [
		comment is 'Computes the gamma log-density at the given value.',
		argnames is ['Value', 'Shape', 'Scale', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Shape is a variable' - instantiation_error,
			'Shape is neither a variable nor a number' - type_error(number, 'Shape'),
			'Shape is not positive' - domain_error(positive_number, 'Shape'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(gamma_distribution/4).
	:- mode(gamma_distribution(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(gamma_distribution/4, [
		comment is 'Computes an approximation of the gamma cumulative distribution at the given value.',
		argnames is ['Value', 'Shape', 'Scale', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Shape is a variable' - instantiation_error,
			'Shape is neither a variable nor a number' - type_error(number, 'Shape'),
			'Shape is not positive' - domain_error(positive_number, 'Shape'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(gamma_quantile/4).
	:- mode(gamma_quantile(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(gamma_quantile/4, [
		comment is 'Computes an approximation of the gamma quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'Shape', 'Scale', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'Shape is a variable' - instantiation_error,
			'Shape is neither a variable nor a number' - type_error(number, 'Shape'),
			'Shape is not positive' - domain_error(positive_number, 'Shape'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	%----------------------------------------------------------------------
	% Beta
	%----------------------------------------------------------------------

	:- public(beta/3).
	:- mode(beta(+positive_number, +positive_number, -float), one_or_error).
	:- info(beta/3, [
		comment is 'Returns a beta distributed random value.',
		argnames is ['Alpha', 'Beta', 'Value'],
		exceptions is [
			'Alpha is a variable' - instantiation_error,
			'Alpha is neither a variable nor a number' - type_error(number, 'Alpha'),
			'Alpha is not positive' - domain_error(positive_number, 'Alpha'),
			'Beta is a variable' - instantiation_error,
			'Beta is neither a variable nor a number' - type_error(number, 'Beta'),
			'Beta is not positive' - domain_error(positive_number, 'Beta')
		]
	]).

	:- public(beta_samples/4).
	:- mode(beta_samples(+integer, +positive_number, +positive_number, -list(float)), one_or_error).
	:- info(beta_samples/4, [
		comment is 'Returns the requested number of beta distributed random values.',
		argnames is ['Count', 'Alpha', 'Beta', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'Alpha is a variable' - instantiation_error,
			'Alpha is neither a variable nor a number' - type_error(number, 'Alpha'),
			'Alpha is not positive' - domain_error(positive_number, 'Alpha'),
			'Beta is a variable' - instantiation_error,
			'Beta is neither a variable nor a number' - type_error(number, 'Beta'),
			'Beta is not positive' - domain_error(positive_number, 'Beta')
		]
	]).

	:- public(beta_density/4).
	:- mode(beta_density(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(beta_density/4, [
		comment is 'Computes the beta probability density at the given value.',
		argnames is ['Value', 'Alpha', 'Beta', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Alpha is a variable' - instantiation_error,
			'Alpha is neither a variable nor a number' - type_error(number, 'Alpha'),
			'Alpha is not positive' - domain_error(positive_number, 'Alpha'),
			'Beta is a variable' - instantiation_error,
			'Beta is neither a variable nor a number' - type_error(number, 'Beta'),
			'Beta is not positive' - domain_error(positive_number, 'Beta')
		]
	]).

	:- public(beta_log_density/4).
	:- mode(beta_log_density(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(beta_log_density/4, [
		comment is 'Computes the beta log-density at the given value.',
		argnames is ['Value', 'Alpha', 'Beta', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Alpha is a variable' - instantiation_error,
			'Alpha is neither a variable nor a number' - type_error(number, 'Alpha'),
			'Alpha is not positive' - domain_error(positive_number, 'Alpha'),
			'Beta is a variable' - instantiation_error,
			'Beta is neither a variable nor a number' - type_error(number, 'Beta'),
			'Beta is not positive' - domain_error(positive_number, 'Beta')
		]
	]).

	:- public(beta_distribution/4).
	:- mode(beta_distribution(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(beta_distribution/4, [
		comment is 'Computes an approximation of the beta cumulative distribution at the given value.',
		argnames is ['Value', 'Alpha', 'Beta', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Alpha is a variable' - instantiation_error,
			'Alpha is neither a variable nor a number' - type_error(number, 'Alpha'),
			'Alpha is not positive' - domain_error(positive_number, 'Alpha'),
			'Beta is a variable' - instantiation_error,
			'Beta is neither a variable nor a number' - type_error(number, 'Beta'),
			'Beta is not positive' - domain_error(positive_number, 'Beta')
		]
	]).

	:- public(beta_quantile/4).
	:- mode(beta_quantile(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(beta_quantile/4, [
		comment is 'Computes an approximation of the beta quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'Alpha', 'Beta', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'Alpha is a variable' - instantiation_error,
			'Alpha is neither a variable nor a number' - type_error(number, 'Alpha'),
			'Alpha is not positive' - domain_error(positive_number, 'Alpha'),
			'Beta is a variable' - instantiation_error,
			'Beta is neither a variable nor a number' - type_error(number, 'Beta'),
			'Beta is not positive' - domain_error(positive_number, 'Beta')
		]
	]).

	%----------------------------------------------------------------------
	% Exponential (scale parameterization)
	%----------------------------------------------------------------------

	:- public(exponential/2).
	:- mode(exponential(+positive_number, -float), one_or_error).
	:- info(exponential/2, [
		comment is 'Returns an exponentially distributed random value (scale parameterization).',
		argnames is ['Scale', 'Value'],
		exceptions is [
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(exponential_samples/3).
	:- mode(exponential_samples(+integer, +positive_number, -list(float)), one_or_error).
	:- info(exponential_samples/3, [
		comment is 'Returns the requested number of exponentially distributed random values.',
		argnames is ['Count', 'Scale', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(exponential_density/3).
	:- mode(exponential_density(+number, +positive_number, -float), one_or_error).
	:- info(exponential_density/3, [
		comment is 'Computes the exponential probability density at the given value.',
		argnames is ['Value', 'Scale', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(exponential_log_density/3).
	:- mode(exponential_log_density(+number, +positive_number, -float), one_or_error).
	:- info(exponential_log_density/3, [
		comment is 'Computes the exponential log-density at the given value.',
		argnames is ['Value', 'Scale', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(exponential_distribution/3).
	:- mode(exponential_distribution(+number, +positive_number, -float), one_or_error).
	:- info(exponential_distribution/3, [
		comment is 'Computes the exponential cumulative distribution at the given value.',
		argnames is ['Value', 'Scale', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	:- public(exponential_quantile/3).
	:- mode(exponential_quantile(+number, +positive_number, -float), one_or_error).
	:- info(exponential_quantile/3, [
		comment is 'Computes the exponential quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'Scale', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'Scale is a variable' - instantiation_error,
			'Scale is neither a variable nor a number' - type_error(number, 'Scale'),
			'Scale is not positive' - domain_error(positive_number, 'Scale')
		]
	]).

	%----------------------------------------------------------------------
	% Fisher-Snedecor (F)
	%----------------------------------------------------------------------

	:- public(fisher/3).
	:- mode(fisher(+positive_number, +positive_number, -float), one_or_error).
	:- info(fisher/3, [
		comment is 'Returns a Fisher-Snedecor (F) distributed random value.',
		argnames is ['DegreesOfFreedom1', 'DegreesOfFreedom2', 'Value'],
		exceptions is [
			'DegreesOfFreedom1 is a variable' - instantiation_error,
			'DegreesOfFreedom1 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom1 is not positive' - domain_error(positive_number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom2 is a variable' - instantiation_error,
			'DegreesOfFreedom2 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom2'),
			'DegreesOfFreedom2 is not positive' - domain_error(positive_number, 'DegreesOfFreedom2')
		]
	]).

	:- public(fisher_samples/4).
	:- mode(fisher_samples(+integer, +positive_number, +positive_number, -list(float)), one_or_error).
	:- info(fisher_samples/4, [
		comment is 'Returns the requested number of Fisher-Snedecor (F) distributed random values.',
		argnames is ['Count', 'DegreesOfFreedom1', 'DegreesOfFreedom2', 'Samples'],
		exceptions is [
			'Count is a variable' - instantiation_error,
			'Count is neither a variable nor an integer' - type_error(integer, 'Count'),
			'Count is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'DegreesOfFreedom1 is a variable' - instantiation_error,
			'DegreesOfFreedom1 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom1 is not positive' - domain_error(positive_number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom2 is a variable' - instantiation_error,
			'DegreesOfFreedom2 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom2'),
			'DegreesOfFreedom2 is not positive' - domain_error(positive_number, 'DegreesOfFreedom2')
		]
	]).

	:- public(fisher_density/4).
	:- mode(fisher_density(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(fisher_density/4, [
		comment is 'Computes the Fisher-Snedecor (F) probability density at the given value.',
		argnames is ['Value', 'DegreesOfFreedom1', 'DegreesOfFreedom2', 'Density'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom1 is a variable' - instantiation_error,
			'DegreesOfFreedom1 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom1 is not positive' - domain_error(positive_number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom2 is a variable' - instantiation_error,
			'DegreesOfFreedom2 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom2'),
			'DegreesOfFreedom2 is not positive' - domain_error(positive_number, 'DegreesOfFreedom2')
		]
	]).

	:- public(fisher_log_density/4).
	:- mode(fisher_log_density(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(fisher_log_density/4, [
		comment is 'Computes the Fisher-Snedecor (F) log-density at the given value.',
		argnames is ['Value', 'DegreesOfFreedom1', 'DegreesOfFreedom2', 'LogDensity'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom1 is a variable' - instantiation_error,
			'DegreesOfFreedom1 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom1 is not positive' - domain_error(positive_number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom2 is a variable' - instantiation_error,
			'DegreesOfFreedom2 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom2'),
			'DegreesOfFreedom2 is not positive' - domain_error(positive_number, 'DegreesOfFreedom2')
		]
	]).

	:- public(fisher_distribution/4).
	:- mode(fisher_distribution(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(fisher_distribution/4, [
		comment is 'Computes an approximation of the Fisher-Snedecor (F) cumulative distribution at the given value.',
		argnames is ['Value', 'DegreesOfFreedom1', 'DegreesOfFreedom2', 'Probability'],
		exceptions is [
			'Value is not a number' - type_error(number, 'Value'),
			'DegreesOfFreedom1 is a variable' - instantiation_error,
			'DegreesOfFreedom1 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom1 is not positive' - domain_error(positive_number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom2 is a variable' - instantiation_error,
			'DegreesOfFreedom2 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom2'),
			'DegreesOfFreedom2 is not positive' - domain_error(positive_number, 'DegreesOfFreedom2')
		]
	]).

	:- public(fisher_quantile/4).
	:- mode(fisher_quantile(+number, +positive_number, +positive_number, -float), one_or_error).
	:- info(fisher_quantile/4, [
		comment is 'Computes an approximation of the Fisher-Snedecor (F) quantile for a probability strictly between zero and one.',
		argnames is ['Probability', 'DegreesOfFreedom1', 'DegreesOfFreedom2', 'Quantile'],
		exceptions is [
			'Probability is not a number' - type_error(number, 'Probability'),
			'Probability is not strictly between zero and one' - domain_error(open_probability, 'Probability'),
			'DegreesOfFreedom1 is a variable' - instantiation_error,
			'DegreesOfFreedom1 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom1 is not positive' - domain_error(positive_number, 'DegreesOfFreedom1'),
			'DegreesOfFreedom2 is a variable' - instantiation_error,
			'DegreesOfFreedom2 is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom2'),
			'DegreesOfFreedom2 is not positive' - domain_error(positive_number, 'DegreesOfFreedom2')
		]
	]).

:- end_protocol.
