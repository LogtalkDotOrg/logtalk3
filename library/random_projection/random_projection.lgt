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
		date is 2026-04-28,
		comment is 'Random projection reducer for continuous datasets using a portable seeded Rademacher projection matrix.',
		remarks is [
			'Algorithm' - 'Centers the training data, optionally standardizes continuous attributes, and samples a dense Rademacher projection matrix with entries in {-$1/sqrt(k)$, +$1/sqrt(k)$} where $k$ is the requested reduced dimensionality.',
			'Feature handling' - 'Supports continuous attributes only. Missing or nonnumeric values are rejected.',
			'Reproducibility' - 'Projection matrices are generated using the portable ``fast_random(xoshiro128pp)`` pseudo-random generator and can be reproduced by setting the ``random_seed/1`` option.',
			'Dimension reducer representation' - 'The learned reducer is represented by default as ``random_projection_reducer(Encoders, Components, Diagnostics)`` where ``Encoders`` stores attribute centering/scaling metadata, ``Components`` stores the sampled projection vectors, and ``Diagnostics`` records the learned model metadata and effective options.'
		],
		see_also is [lda_projection, pca]
	]).

	:- public(learn/3).
	:- mode(learn(+object_identifier, -compound, +list(compound)), one).
	:- info(learn/3, [
		comment is 'Learns a random projection reducer from the given dataset object using the specified options.',
		argnames is ['Dataset', 'DimensionReducer', 'Options']
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
		ComponentCount is min(RequestedComponents, FeatureCount),
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
		length(AttributeNames, FeatureCount),
		length(Components, ComponentCount),
		Diagnostics = [
			model(random_projection),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			component_count(ComponentCount)
		].

	dimension_reducer_data(DimensionReducer, Encoders, Components) :-
		DimensionReducer =.. [_Functor, Encoders, Components| _].

	dimension_reducer_diagnostics_data(random_projection_reducer(_Encoders, _Components, Diagnostics), Diagnostics).

	print_dimension_reducer_properties(random_projection_reducer(Encoders, Components, Diagnostics)) :-
		format('Random Projection Dimension Reducer~n', []),
		format('==================================~n~n', []),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Encoders: ~w~n', [Encoders]),
		length(Components, ComponentCount),
		format('Components: ~w~n', [ComponentCount]).

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
