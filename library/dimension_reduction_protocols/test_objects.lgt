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


:- object(sample_dimension_reducer,
	imports(dimension_reducer_common)).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(list, [
		length/2
	]).

	learn(Dataset, sample_dimension_reducer(Encoders, Components, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		^^check_examples_non_empty(Dataset, Examples),
		^^check_example_values(Examples, AttributeNames),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		length(AttributeNames, FeatureCount),
		identity_prefix_components(FeatureCount, Components),
		^^base_dimension_reducer_diagnostics(sample_dimension_reducer, AttributeNames, Components, Options, [], Diagnostics).

	identity_prefix_components(FeatureCount, Components) :-
		component_count(FeatureCount, ComponentCount),
		identity_prefix_components(1, ComponentCount, FeatureCount, Components).

	component_count(FeatureCount, 1) :-
		FeatureCount =< 1,
		!.
	component_count(_FeatureCount, 2).

	identity_prefix_components(Index, ComponentCount, _FeatureCount, []) :-
		Index > ComponentCount,
		!.
	identity_prefix_components(Index, ComponentCount, FeatureCount, [Component| Components]) :-
		basis_vector(1, FeatureCount, Index, Component),
		NextIndex is Index + 1,
		identity_prefix_components(NextIndex, ComponentCount, FeatureCount, Components).

	basis_vector(Current, FeatureCount, _Index, []) :-
		Current > FeatureCount,
		!.
	basis_vector(Index, FeatureCount, Index, [1.0| Component]) :-
		!,
		Next is Index + 1,
		basis_vector(Next, FeatureCount, Index, Component).
	basis_vector(Current, FeatureCount, Index, [0.0| Component]) :-
		Next is Current + 1,
		basis_vector(Next, FeatureCount, Index, Component).

	print_dimension_reducer_properties(DimensionReducer) :-
		writeq(DimensionReducer), nl.

	example_attribute_values(_-AttributeValues, AttributeValues).

	default_option(feature_scaling(false)).

	valid_option(feature_scaling(FeatureScaling)) :-
		type::valid(boolean, FeatureScaling).

:- end_object.


:- object(target_measurements,
	implements(target_supervised_dimension_reduction_dataset_protocol)).

	attribute_values(length, continuous).
	attribute_values(width, continuous).
	attribute_values(weight, continuous).

	target(score).

	example(1, 0.5, [length-1.0, width-2.0, weight-3.0]).
	example(2, 1.5, [length-2.0, width-3.0, weight-5.0]).

:- end_object.
