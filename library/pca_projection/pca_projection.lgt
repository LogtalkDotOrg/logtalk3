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


:- object(pca_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Principal Component Analysis reducer for continuous datasets using a portable power-iteration eigensolver.',
		see_also is [lda_projection, random_projection]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(linear_algebra, [
		covariance_matrix/2
	]).

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		length(AttributeNames, FeatureCount),
		^^option(n_components(RequestedComponents), Options),
		^^check_component_count(RequestedComponents, FeatureCount, ComponentCount),
		covariance_matrix(Rows, CovarianceMatrix),
		^^extract_components(CovarianceMatrix, ComponentCount, Options, Components, ExplainedVariances),
		build_diagnostics(AttributeNames, Components, ExplainedVariances, Options, Diagnostics),
		DimensionReducer = pca_reducer(Encoders, Components, ExplainedVariances, Diagnostics).

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		check_minimum_examples(Examples),
		^^check_example_values(Examples, AttributeNames).

	check_minimum_examples(Examples) :-
		length(Examples, Count),
		(	Count >= 2 ->
			true
		;	domain_error(minimum_number_of_examples, Count)
		).

	example_attribute_values(_-AttributeValues, AttributeValues).

	build_diagnostics(AttributeNames, Components, ExplainedVariances, Options, Diagnostics) :-
		^^base_dimension_reducer_diagnostics(pca_projection, AttributeNames, Components, Options, [explained_variances(ExplainedVariances)], Diagnostics).

	print_dimension_reducer_properties(pca_reducer(Encoders, Components, ExplainedVariances, Diagnostics)) :-
		format('PCA Dimension Reducer~n', []),
		format('=====================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Components),
		format('Explained variances: ~w~n', [ExplainedVariances]).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-8)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).

:- end_object.
