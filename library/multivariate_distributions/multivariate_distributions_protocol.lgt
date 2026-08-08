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


:- protocol(multivariate_distributions_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Multivariate probability distribution predicates.',
		see_also is [multivariate_distributions(_), linear_algebra, sampling_protocol]
	]).

	:- public(multivariate_normal/3).
	:- mode(multivariate_normal(+list(number), +list(list(number)), -list(float)), one_or_error).
	:- info(multivariate_normal/3, [
		comment is 'Returns a multivariate normally distributed random vector using the default numerical tolerance of 1.0e-12. Singular positive-semidefinite covariance matrices are supported.',
		argnames is ['Mean', 'Covariance', 'Sample'],
		exceptions is [
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance')
		]
	]).

	:- public(multivariate_normal/4).
	:- mode(multivariate_normal(+list(number), +list(list(number)), +number, -list(float)), one_or_error).
	:- info(multivariate_normal/4, [
		comment is 'Returns a multivariate normally distributed random vector using the given non-negative numerical tolerance. Singular positive-semidefinite covariance matrices are supported.',
		argnames is ['Mean', 'Covariance', 'Tolerance', 'Sample'],
		exceptions is [
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Tolerance`` is negative' - domain_error(non_negative_number, 'Tolerance'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_normal_samples/4).
	:- mode(multivariate_normal_samples(+integer, +list(number), +list(list(number)), -list(list(float))), one_or_error).
	:- info(multivariate_normal_samples/4, [
		comment is 'Returns the requested number of multivariate normally distributed random row vectors using the default numerical tolerance of 1.0e-12.',
		argnames is ['Count', 'Mean', 'Covariance', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance')
		]
	]).

	:- public(multivariate_normal_samples/5).
	:- mode(multivariate_normal_samples(+integer, +list(number), +list(list(number)), +number, -list(list(float))), one_or_error).
	:- info(multivariate_normal_samples/5, [
		comment is 'Returns the requested number of multivariate normally distributed random row vectors using the given non-negative numerical tolerance.',
		argnames is ['Count', 'Mean', 'Covariance', 'Tolerance', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_normal_density/4).
	:- mode(multivariate_normal_density(+list(number), +list(number), +list(list(number)), -float), one_or_error).
	:- info(multivariate_normal_density/4, [
		comment is 'Computes the multivariate normal density at a point using the default numerical tolerance of 1.0e-12. For a singular covariance matrix, computes the density on its affine support and returns zero outside it.',
		argnames is ['Point', 'Mean', 'Covariance', 'Density'],
		exceptions is [
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance'),
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the mean dimension' - domain_error(point_dimensions(_), 'Point')
		]
	]).

	:- public(multivariate_normal_density/5).
	:- mode(multivariate_normal_density(+list(number), +list(number), +list(list(number)), +number, -float), one_or_error).
	:- info(multivariate_normal_density/5, [
		comment is 'Computes the multivariate normal density at a point using the given non-negative numerical tolerance. For a singular covariance matrix, computes the density on its affine support and returns zero outside it.',
		argnames is ['Point', 'Mean', 'Covariance', 'Tolerance', 'Density'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the mean dimension' - domain_error(point_dimensions(_), 'Point'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_normal_log_density/4).
	:- mode(multivariate_normal_log_density(+list(number), +list(number), +list(list(number)), -term), one_or_error).
	:- info(multivariate_normal_log_density/4, [
		comment is 'Computes the multivariate normal log-density at a point using the default numerical tolerance of 1.0e-12. Returns the atom ``negative_infinity`` outside the affine support of a singular covariance matrix.',
		argnames is ['Point', 'Mean', 'Covariance', 'LogDensity'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the mean dimension' - domain_error(point_dimensions(_), 'Point'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance')
		]
	]).

	:- public(multivariate_normal_log_density/5).
	:- mode(multivariate_normal_log_density(+list(number), +list(number), +list(list(number)), +number, -term), one_or_error).
	:- info(multivariate_normal_log_density/5, [
		comment is 'Computes the multivariate normal log-density at a point using the given non-negative numerical tolerance. Returns the atom ``negative_infinity`` outside the affine support of a singular covariance matrix.',
		argnames is ['Point', 'Mean', 'Covariance', 'Tolerance', 'LogDensity'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the mean dimension' - domain_error(point_dimensions(_), 'Point'),
			'``Tolerance`` is negative' - domain_error(non_negative_number, 'Tolerance'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(squared_mahalanobis_distance/4).
	:- mode(squared_mahalanobis_distance(+list(number), +list(number), +list(list(number)), -float), one_or_error).
	:- info(squared_mahalanobis_distance/4, [
		comment is 'Computes the squared Mahalanobis distance using the default numerical tolerance of 1.0e-12.',
		argnames is ['Point', 'Mean', 'Covariance', 'SquaredDistance'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` is outside the affine support of ``Covariance``' - domain_error(covariance_support, 'Point'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element')
		]
	]).

	:- public(squared_mahalanobis_distance/5).
	:- mode(squared_mahalanobis_distance(+list(number), +list(number), +list(list(number)), +number, -float), one_or_error).
	:- info(squared_mahalanobis_distance/5, [
		comment is 'Computes the squared Mahalanobis distance using the given non-negative numerical tolerance.',
		argnames is ['Point', 'Mean', 'Covariance', 'Tolerance', 'SquaredDistance'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` is outside the affine support of ``Covariance``' - domain_error(covariance_support, 'Point'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(mahalanobis_distance/4).
	:- mode(mahalanobis_distance(+list(number), +list(number), +list(list(number)), -float), one_or_error).
	:- info(mahalanobis_distance/4, [
		comment is 'Computes the Mahalanobis distance using the default numerical tolerance of 1.0e-12.',
		argnames is ['Point', 'Mean', 'Covariance', 'Distance'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` is outside the affine support of ``Covariance``' - domain_error(covariance_support, 'Point'),
			'``Point`` dimensions do not match the mean dimension' - domain_error(point_dimensions(_), 'Point'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element')
		]
	]).

	:- public(mahalanobis_distance/5).
	:- mode(mahalanobis_distance(+list(number), +list(number), +list(list(number)), +number, -float), one_or_error).
	:- info(mahalanobis_distance/5, [
		comment is 'Computes the Mahalanobis distance using the given non-negative numerical tolerance.',
		argnames is ['Point', 'Mean', 'Covariance', 'Tolerance', 'Distance'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` is outside the affine support of ``Covariance``' - domain_error(covariance_support, 'Point'),
			'``Point`` dimensions do not match the mean dimension' - domain_error(point_dimensions(_), 'Point'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_t/4).
	:- mode(multivariate_t(+positive_number, +list(number), +list(list(number)), -list(float)), one_or_error).
	:- info(multivariate_t/4, [
		comment is 'Returns a multivariate Student\'s t distributed random vector using the default numerical tolerance of 1.0e-12.',
		argnames is ['DegreesOfFreedom', 'Location', 'Scale', 'Sample'],
		exceptions is [
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor an integer' - type_error(integer, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is not a positive integer' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale')
		]
	]).

	:- public(multivariate_t/5).
	:- mode(multivariate_t(+positive_number, +list(number), +list(list(number)), +number, -list(float)), one_or_error).
	:- info(multivariate_t/5, [
		comment is 'Returns a multivariate Student\'s t distributed random vector using the given non-negative numerical tolerance.',
		argnames is ['DegreesOfFreedom', 'Location', 'Scale', 'Tolerance', 'Sample'],
		exceptions is [
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Tolerance`` is negative' - domain_error(non_negative_number, 'Tolerance'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_t_samples/5).
	:- mode(multivariate_t_samples(+integer, +positive_number, +list(number), +list(list(number)), -list(list(float))), one_or_error).
	:- info(multivariate_t_samples/5, [
		comment is 'Returns the requested number of multivariate Student\'s t distributed random row vectors using the default numerical tolerance of 1.0e-12.',
		argnames is ['Count', 'DegreesOfFreedom', 'Location', 'Scale', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale')
		]
	]).

	:- public(multivariate_t_samples/6).
	:- mode(multivariate_t_samples(+integer, +positive_number, +list(number), +list(list(number)), +number, -list(list(float))), one_or_error).
	:- info(multivariate_t_samples/6, [
		comment is 'Returns the requested number of multivariate Student\'s t distributed random row vectors using the given non-negative numerical tolerance.',
		argnames is ['Count', 'DegreesOfFreedom', 'Location', 'Scale', 'Tolerance', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is a negative integer' - domain_error(non_negative_integer, 'Count'),
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Tolerance`` is negative' - domain_error(non_negative_number, 'Tolerance'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_t_density/5).
	:- mode(multivariate_t_density(+list(number), +positive_number, +list(number), +list(list(number)), -float), one_or_error).
	:- info(multivariate_t_density/5, [
		comment is 'Computes the multivariate Student\'s t density using the default numerical tolerance of 1.0e-12. Returns zero outside the affine support of a singular scale matrix.',
		argnames is ['Point', 'DegreesOfFreedom', 'Location', 'Scale', 'Density'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the location dimension' - domain_error(point_dimensions(_), 'Point'),
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale')
		]
	]).

	:- public(multivariate_t_density/6).
	:- mode(multivariate_t_density(+list(number), +positive_number, +list(number), +list(list(number)), +number, -float), one_or_error).
	:- info(multivariate_t_density/6, [
		comment is 'Computes the multivariate Student\'s t density using the given non-negative numerical tolerance. Returns zero outside the affine support of a singular scale matrix.',
		argnames is ['Point', 'DegreesOfFreedom', 'Location', 'Scale', 'Tolerance', 'Density'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the location dimension' - domain_error(point_dimensions(_), 'Point'),
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(multivariate_t_log_density/5).
	:- mode(multivariate_t_log_density(+list(number), +positive_number, +list(number), +list(list(number)), -term), one_or_error).
	:- info(multivariate_t_log_density/5, [
		comment is 'Computes the multivariate Student\'s t log-density using the default numerical tolerance of 1.0e-12. Returns ``negative_infinity`` outside singular affine support.',
		argnames is ['Point', 'DegreesOfFreedom', 'Location', 'Scale', 'LogDensity'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the location dimension' - domain_error(point_dimensions(_), 'Point'),
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale')
		]
	]).

	:- public(multivariate_t_log_density/6).
	:- mode(multivariate_t_log_density(+list(number), +positive_number, +list(number), +list(list(number)), +number, -term), one_or_error).
	:- info(multivariate_t_log_density/6, [
		comment is 'Computes the multivariate Student\'s t log-density using the given non-negative numerical tolerance. Returns ``negative_infinity`` outside singular affine support.',
		argnames is ['Point', 'DegreesOfFreedom', 'Location', 'Scale', 'Tolerance', 'LogDensity'],
		exceptions is [
			'``Point`` is a variable or a partial list' - instantiation_error,
			'``Point`` is neither a partial list nor a list' - type_error(list, 'Point'),
			'An element ``Element`` of the ``Point`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Point`` dimensions do not match the location dimension' - domain_error(point_dimensions(_), 'Point'),
			'``DegreesOfFreedom`` is a variable' - instantiation_error,
			'``DegreesOfFreedom`` is neither a variable nor a number' - type_error(number, 'DegreesOfFreedom'),
			'``DegreesOfFreedom`` is a number but not a positive number' - domain_error(positive_number, 'DegreesOfFreedom'),
			'``Location`` is empty' - domain_error(minimum_number_of_values(1), 'Location'),
			'``Scale`` dimensions do not match the location dimension' - domain_error(covariance_dimensions(_), 'Scale'),
			'``Scale`` is not symmetric' - domain_error(symmetric_matrix, 'Scale'),
			'``Scale`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Scale'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(logistic_normal/3).
	:- mode(logistic_normal(+list(number), +list(list(number)), -list(float)), one_or_error).
	:- info(logistic_normal/3, [
		comment is 'Returns an additive-log-ratio logistic-normal random vector using the default numerical tolerance of 1.0e-12. A latent vector of length d maps to a simplex vector of length d+1 using the final component as reference.',
		argnames is ['Mean', 'Covariance', 'Sample'],
		exceptions is [
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance')
		]
	]).

	:- public(logistic_normal/4).
	:- mode(logistic_normal(+list(number), +list(list(number)), +number, -list(float)), one_or_error).
	:- info(logistic_normal/4, [
		comment is 'Returns an additive-log-ratio logistic-normal random vector using the given non-negative numerical tolerance.',
		argnames is ['Mean', 'Covariance', 'Tolerance', 'Sample'],
		exceptions is [
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

	:- public(logistic_normal_samples/4).
	:- mode(logistic_normal_samples(+integer, +list(number), +list(list(number)), -list(list(float))), one_or_error).
	:- info(logistic_normal_samples/4, [
		comment is 'Returns the requested number of additive-log-ratio logistic-normal random row vectors using the default numerical tolerance of 1.0e-12.',
		argnames is ['Count', 'Mean', 'Covariance', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance')
		]
	]).

	:- public(logistic_normal_samples/5).
	:- mode(logistic_normal_samples(+integer, +list(number), +list(list(number)), +number, -list(list(float))), one_or_error).
	:- info(logistic_normal_samples/5, [
		comment is 'Returns the requested number of additive-log-ratio logistic-normal random row vectors using the given non-negative numerical tolerance.',
		argnames is ['Count', 'Mean', 'Covariance', 'Tolerance', 'Samples'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is neither a variable nor an integer' - type_error(integer, 'Count'),
			'``Count`` is an integer but not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Mean`` is a variable or a partial list' - instantiation_error,
			'``Mean`` is neither a partial list nor a list' - type_error(list, 'Mean'),
			'An element ``Element`` of the ``Mean`` list is neither a variable nor a number' - type_error(number, 'Element'),
			'``Mean`` is empty' - domain_error(minimum_number_of_values(1), 'Mean'),
			'``Covariance`` is a variable or a partial list' - instantiation_error,
			'``Covariance`` is neither a partial list nor a list' - type_error(list(list(number)), 'Covariance'),
			'An element ``Element`` of the ``Covariance`` list is neither a variable nor a list of numbers' - type_error(list(number), 'Element'),
			'``Covariance`` dimensions do not match the mean dimension' - domain_error(covariance_dimensions(_), 'Covariance'),
			'``Covariance`` is not symmetric' - domain_error(symmetric_matrix, 'Covariance'),
			'``Covariance`` is not positive semidefinite' - domain_error(positive_semidefinite_matrix, 'Covariance'),
			'``Tolerance`` is a variable' - instantiation_error,
			'``Tolerance`` is neither a variable nor a number' - type_error(number, 'Tolerance'),
			'``Tolerance`` is a nuber but not a non-negative number' - domain_error(non_negative_number, 'Tolerance')
		]
	]).

:- end_protocol.
