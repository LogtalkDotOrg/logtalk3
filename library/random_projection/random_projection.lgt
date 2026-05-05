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


:- object(random_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Random projection reducer for continuous datasets using a portable seeded Rademacher projection matrix.',
		see_also is [lda_projection, pca]
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1 as get_random_seed/1, set_seed/1 as set_random_seed/1, random/1 as random_float/1,
		randomize/1 as randomize_seed/1
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

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		length(AttributeNames, FeatureCount),
		^^option(n_components(RequestedComponents), Options),
		^^check_component_count(RequestedComponents, FeatureCount, ComponentCount),
		generate_components(ComponentCount, FeatureCount, Options, Components),
		build_diagnostics(AttributeNames, Components, Options, Diagnostics),
		DimensionReducer = random_projection_reducer(Encoders, Components, Diagnostics),
		!.

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		^^check_example_values(Examples, AttributeNames).

	example_attribute_values(_-AttributeValues, AttributeValues).

	generate_components(ComponentCount, FeatureCount, Options, Components) :-
		get_random_seed(OriginalSeed),
		^^option(random_seed(RandomSeed), Options),
		randomize_seed(RandomSeed),
		generate_components(ComponentCount, FeatureCount, ComponentCount, Components),
		set_random_seed(OriginalSeed).

	generate_components(0, _FeatureCount, _ComponentCount, []) :-
		!.
	generate_components(Remaining, FeatureCount, ComponentCount, [Component| Components]) :-
		Remaining > 0,
		component_scale(ComponentCount, Scale),
		generate_component(FeatureCount, Scale, Component),
		NextRemaining is Remaining - 1,
		generate_components(NextRemaining, FeatureCount, ComponentCount, Components).

	component_scale(ComponentCount, Scale) :-
		Scale is 1.0 / sqrt(ComponentCount).

	generate_component(0, _Scale, []) :-
		!.
	generate_component(FeatureCount, Scale, [Weight| Component]) :-
		FeatureCount > 0,
		random_float(Random),
		(   Random < 0.5 ->
			Weight is -Scale
		;   Weight = Scale
		),
		NextFeatureCount is FeatureCount - 1,
		generate_component(NextFeatureCount, Scale, Component).

	build_diagnostics(AttributeNames, Components, Options, Diagnostics) :-
		^^base_dimension_reducer_diagnostics(random_projection, AttributeNames, Components, Options, [], Diagnostics).

	print_dimension_reducer_properties(random_projection_reducer(Encoders, Components, Diagnostics)) :-
		format('Random Projection Dimension Reducer~n', []),
		format('==================================~n~n', []),
		^^print_dimension_reducer_details(Diagnostics, Encoders, Components).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(random_seed(1357911)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(random_seed(RandomSeed)) :-
		valid(positive_integer, RandomSeed).

:- end_object.
