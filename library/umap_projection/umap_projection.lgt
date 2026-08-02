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


:- object(umap_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'Uniform Manifold Approximation and Projection dimension reducer for continuous, categorical, and mixed datasets.',
		see_also is [tsne_projection, spectral_clusterer]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1 as get_random_seed/1, set_seed/1 as set_random_seed/1,
		randomize/1 as randomize_seed/1, normal/3 as random_normal/3,
		random/1 as random_float/1
	]).

	:- uses(linear_algebra, [
		symmetric_eigen/5, transpose_matrix/2
	]).

	:- uses(assignvars, [
		assignable/2,
		op(100, xfx, '<='), ('<=')/2,
		op(100, xfx, '=>'), ('=>')/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2, nth0/3
	]).

	:- uses(numberlist, [
		euclidean_distance/3, manhattan_distance/3
	]).

	:- uses(pairs, [
		keys/2
	]).

	:- uses(population, [
		arithmetic_mean/2, variance/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	learn(Dataset, DimensionReducer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		check_option_relations(Options),
		^^dataset_attributes(Dataset, Attributes),
		check_mixed_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		check_examples(Dataset, Attributes, AttributeNames, Examples),
		build_encoders(Attributes, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, TrainingRows),
		keys(Examples, ExampleIds),
		encoded_feature_count(Encoders, EncodedFeatureCount),
		length(TrainingRows, SampleCount),
		^^option(n_components(RequestedComponentCount), Options),
		MaxComponentCount is min(EncodedFeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponentCount, MaxComponentCount, ComponentCount),
		resolve_neighbor_count(Options, SampleCount, NeighborCount),
		check_local_connectivity(Options, NeighborCount),
		^^option(distance_metric(Metric), Options),
		exact_nearest_neighbors(TrainingRows, Metric, NeighborCount, NeighborRows),
		fuzzy_graph(NeighborRows, Options, FuzzyGraph),
		fit_curve_parameters(Options, CurveA, CurveB),
		initialize_embedding(FuzzyGraph, SampleCount, ComponentCount, Options, InitializationUsed, InitialEmbeddingRows),
		embedding_cross_entropy(FuzzyGraph, InitialEmbeddingRows, CurveA, CurveB, InitialCrossEntropy),
		optimize_embedding(FuzzyGraph, InitialEmbeddingRows, CurveA, CurveB, Options, EmbeddingRows),
		embedding_cross_entropy(FuzzyGraph, EmbeddingRows, CurveA, CurveB, FinalCrossEntropy),
		build_diagnostics(AttributeNames, EncodedFeatureCount, SampleCount, ComponentCount, NeighborCount, Metric, FuzzyGraph, CurveA, CurveB, InitializationUsed, InitialCrossEntropy, FinalCrossEntropy, Options, Diagnostics),
		DimensionReducer = umap_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, FuzzyGraph, Diagnostics).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		DimensionReducer = umap_reducer(Encoders, _ExampleIds, TrainingRows, EmbeddingRows, _FuzzyGraph, Diagnostics),
		encode_instance(Encoders, Instance, Features),
		memberchk(distance_metric(Metric), Diagnostics),
		memberchk(effective_n_neighbors(NeighborCount), Diagnostics),
		nearest_neighbors(TrainingRows, Features, Metric, NeighborCount, Neighbors),
		transform_memberships(Neighbors, Memberships),
		membership_weighted_coordinates(Memberships, EmbeddingRows, InitialCoordinates),
		memberchk(curve_parameters(CurveA, CurveB), Diagnostics),
		memberchk(options(Options), Diagnostics),
		optimize_transform(Memberships, EmbeddingRows, InitialCoordinates, CurveA, CurveB, Options, Coordinates),
		coordinate_pairs(Coordinates, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		(	var(DimensionReducer) ->
			instantiation_error
		;	DimensionReducer = umap_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, FuzzyGraph, Diagnostics),
			valid_mixed_encoders(Encoders),
			valid_example_ids(ExampleIds),
			length(Encoders, FeatureCount),
			encoded_feature_count(Encoders, EncodedFeatureCount),
			length(ExampleIds, SampleCount),
			SampleCount >= 3,
			valid_numeric_rows(TrainingRows, EncodedFeatureCount, SampleCount),
			memberchk(component_count(ComponentCount), Diagnostics),
			valid(positive_integer, ComponentCount),
			valid_numeric_rows(EmbeddingRows, ComponentCount, SampleCount),
			memberchk(effective_n_neighbors(NeighborCount), Diagnostics),
			valid_fuzzy_graph(FuzzyGraph, SampleCount),
			length(FuzzyGraph, EdgeCount),
			memberchk(graph_edge_count(EdgeCount), Diagnostics),
			valid_umap_diagnostics(Diagnostics, FeatureCount, EncodedFeatureCount, SampleCount, ComponentCount, NeighborCount) ->
			true
		;	domain_error(dimension_reducer, DimensionReducer)
		).

	example_attribute_values(_-AttributeValues, AttributeValues).

	dimension_reducer_diagnostics_data(umap_reducer(_Encoders, _ExampleIds, _TrainingRows, _EmbeddingRows, _FuzzyGraph, Diagnostics), Diagnostics).

	print_dimension_reducer_properties(umap_reducer(Encoders, _ExampleIds, TrainingRows, EmbeddingRows, FuzzyGraph, Diagnostics)) :-
		format('UMAP Dimension Reducer~n', []),
		format('======================~n~n', []),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Encoders: ~w~n', [Encoders]),
		length(TrainingRows, SampleCount),
		format('Training rows: ~w~n', [SampleCount]),
		length(FuzzyGraph, EdgeCount),
		format('Fuzzy graph edges: ~w~n', [EdgeCount]),
		EmbeddingRows = [FirstEmbeddingRow| _],
		length(FirstEmbeddingRow, ComponentCount),
		format('Embedding dimensions: ~w~n', [ComponentCount]).

	check_mixed_attributes([]).
	check_mixed_attributes([Attribute-Values| Attributes]) :-
		(	Values == continuous ->
			true
		;	valid_categorical_values(Values) ->
			true
		;	domain_error(attribute_declarations, Attribute)
		),
		check_mixed_attributes(Attributes).

	valid_categorical_values(Values) :-
		Values = [_| _],
		proper_nonvar_values(Values),
		distinct_values(Values).

	proper_nonvar_values([]).
	proper_nonvar_values([Value| Values]) :-
		nonvar(Value),
		proper_nonvar_values(Values).

	distinct_values([]).
	distinct_values([Value| Values]) :-
		\+ member(Value, Values),
		distinct_values(Values).

	check_examples(Dataset, Attributes, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		length(Examples, SampleCount),
		(	SampleCount >= 3 ->
			true
		;	domain_error(minimum_number_of_examples, SampleCount)
		),
		check_distinct_example_ids(Examples, []),
		check_example_values(Examples, Attributes, AttributeNames).

	check_distinct_example_ids([], _SeenIds).
	check_distinct_example_ids([Id-_AttributeValues| Examples], SeenIds) :-
		(	member(Id, SeenIds) ->
			domain_error(example_identifier, Id)
		;	check_distinct_example_ids(Examples, [Id| SeenIds])
		).

	check_example_values([], _Attributes, _AttributeNames).
	check_example_values([_-AttributeValues| Examples], Attributes, AttributeNames) :-
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_attribute_values(Attributes, AttributeValues),
		check_example_values(Examples, Attributes, AttributeNames).

	check_attribute_bindings(AttributeNames, AttributeValues) :-
		check_declared_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_declared_attribute_bindings([], _AttributeValues).
	check_declared_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(	Count =:= 1 ->
			true
		;	Count =:= 0 ->
			existence_error(attribute, Attribute)
		;	domain_error(attribute_occurrences, Attribute)
		),
		check_declared_attribute_bindings(Attributes, AttributeValues).

	check_undeclared_attribute_bindings([], _AttributeNames).
	check_undeclared_attribute_bindings([Attribute-_Value| AttributeValues], AttributeNames) :-
		(	member(Attribute, AttributeNames) ->
			true
		;	domain_error(declared_attribute, Attribute)
		),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	attribute_occurrences([], _Attribute, Count, Count).
	attribute_occurrences([Attribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		attribute_occurrences(AttributeValues, Attribute, Count1, Count).
	attribute_occurrences([_OtherAttribute-_Value| AttributeValues], Attribute, Count0, Count) :-
		attribute_occurrences(AttributeValues, Attribute, Count0, Count).

	check_attribute_values([], _AttributeValues).
	check_attribute_values([Attribute-continuous| Attributes], AttributeValues) :-
		!,
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			true
		;	number(Value) ->
			true
		;	type_error(number, Value)
		),
		check_attribute_values(Attributes, AttributeValues).
	check_attribute_values([Attribute-Values| Attributes], AttributeValues) :-
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			true
		;	member(Value, Values) ->
			true
		;	domain_error(attribute_value(Attribute, Values), Value)
		),
		check_attribute_values(Attributes, AttributeValues).

	attribute_value(Attribute, AttributeValues, Value) :-
		(	member(Attribute-Value, AttributeValues) ->
			true
		;	existence_error(attribute, Attribute)
		).

	build_encoders([], _Examples, _Options, []).
	build_encoders([Attribute-continuous| Attributes], Examples, Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		!,
		observed_attribute_values(Examples, Attribute, Values),
		(	Values == [] ->
			domain_error(observed_attribute_values, Attribute)
		;	arithmetic_mean(Values, Mean),
			continuous_scale(Values, Options, Scale)
		),
		build_encoders(Attributes, Examples, Options, Encoders).
	build_encoders([Attribute-Values| Attributes], Examples, Options, [categorical(Attribute, Values)| Encoders]) :-
		observed_attribute_values(Examples, Attribute, ObservedValues),
		(	ObservedValues == [] ->
			domain_error(observed_attribute_values, Attribute)
		;	true
		),
		build_encoders(Attributes, Examples, Options, Encoders).

	observed_attribute_values([], _Attribute, []).
	observed_attribute_values([_-AttributeValues| Examples], Attribute, Values) :-
		attribute_value(Attribute, AttributeValues, Value),
		(	nonvar(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		observed_attribute_values(Examples, Attribute, Rest).

	continuous_scale(Values, Options, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(	FeatureScaling == true ->
			length(Values, Count),
			(	Count > 1 ->
				variance(Values, Variance)
			;	Variance = 0.0
			),
			(	Variance > 0.0 ->
				Scale is sqrt(Variance)
			;	Scale = 1.0
			)
		;	Scale = 1.0
		).

	examples_to_rows([], _Encoders, []).
	examples_to_rows([_-AttributeValues| Examples], Encoders, [Row| Rows]) :-
		encode_instance(Encoders, AttributeValues, Row),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance(Encoders, AttributeValues, Features) :-
		encoder_attribute_names(Encoders, AttributeNames),
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_encoded_attribute_values(Encoders, AttributeValues),
		encode_instance_values(Encoders, AttributeValues, Features).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| Attributes]) :-
		!,
		encoder_attribute_names(Encoders, Attributes).
	encoder_attribute_names([categorical(Attribute, _Values)| Encoders], [Attribute| Attributes]) :-
		encoder_attribute_names(Encoders, Attributes).

	check_encoded_attribute_values([], _AttributeValues).
	check_encoded_attribute_values([continuous(Attribute, _Mean, _Scale)| Encoders], AttributeValues) :-
		!,
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			true
		;	number(Value) ->
			true
		;	type_error(number, Value)
		),
		check_encoded_attribute_values(Encoders, AttributeValues).
	check_encoded_attribute_values([categorical(Attribute, Values)| Encoders], AttributeValues) :-
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			true
		;	member(Value, Values) ->
			true
		;	domain_error(attribute_value(Attribute, Values), Value)
		),
		check_encoded_attribute_values(Encoders, AttributeValues).

	encode_instance_values([], _AttributeValues, []).
	encode_instance_values([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		!,
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			Feature = 0.0
		;	Feature is (Value - Mean) / Scale
		),
		encode_instance_values(Encoders, AttributeValues, Features).
	encode_instance_values([categorical(Attribute, Values)| Encoders], AttributeValues, Features) :-
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			missing_one_hot_encode(Values, Features, RestFeatures)
		;	one_hot_encode(Values, Value, Features, [0.0| RestFeatures])
		),
		encode_instance_values(Encoders, AttributeValues, RestFeatures).

	one_hot_encode([], _Value, Features, Features).
	one_hot_encode([Category| Categories], Value, [Feature| Features], RestFeatures) :-
		(	Value == Category ->
			Feature = 1.0
		;	Feature = 0.0
		),
		one_hot_encode(Categories, Value, Features, RestFeatures).

	missing_one_hot_encode([], [1.0| Features], Features).
	missing_one_hot_encode([_| Values], [0.0| Features], RestFeatures) :-
		missing_one_hot_encode(Values, Features, RestFeatures).

	encoded_feature_count(Encoders, EncodedFeatureCount) :-
		encoded_feature_count(Encoders, 0, EncodedFeatureCount).

	encoded_feature_count([], EncodedFeatureCount, EncodedFeatureCount).
	encoded_feature_count([continuous(_Attribute, _Mean, _Scale)| Encoders], EncodedFeatureCount0, EncodedFeatureCount) :-
		!,
		EncodedFeatureCount1 is EncodedFeatureCount0 + 1,
		encoded_feature_count(Encoders, EncodedFeatureCount1, EncodedFeatureCount).
	encoded_feature_count([categorical(_Attribute, Values)| Encoders], EncodedFeatureCount0, EncodedFeatureCount) :-
		length(Values, ValueCount),
		EncodedFeatureCount1 is EncodedFeatureCount0 + ValueCount + 1,
		encoded_feature_count(Encoders, EncodedFeatureCount1, EncodedFeatureCount).

	resolve_neighbor_count(Options, SampleCount, NeighborCount) :-
		^^option(n_neighbors(RequestedNeighborCount), Options),
		NeighborCount is min(RequestedNeighborCount, SampleCount - 1).

	exact_nearest_neighbors(Rows, Metric, NeighborCount, NeighborRows) :-
		exact_nearest_neighbors(Rows, Rows, Metric, NeighborCount, 0, NeighborRows).

	exact_nearest_neighbors([], _AllRows, _Metric, _NeighborCount, _Index, []).
	exact_nearest_neighbors([Row| Rows], AllRows, Metric, NeighborCount, Index, [Neighbors| NeighborRows]) :-
		indexed_distances(AllRows, Row, Metric, 0, -1, Distances),
		keysort(Distances, SortedDistances),
		take_neighbors(NeighborCount, SortedDistances, Neighbors),
		NextIndex is Index + 1,
		exact_nearest_neighbors(Rows, AllRows, Metric, NeighborCount, NextIndex, NeighborRows).

	nearest_neighbors(Rows, Row, Metric, NeighborCount, Neighbors) :-
		indexed_distances(Rows, Row, Metric, 0, -1, Distances),
		keysort(Distances, SortedDistances),
		take_neighbors(NeighborCount, SortedDistances, Neighbors).

	indexed_distances([], _Row, _Metric, _Index, _ExcludedIndex, []).
	indexed_distances([OtherRow| Rows], Row, Metric, Index, ExcludedIndex, Distances) :-
		(	Index =:= ExcludedIndex ->
			Distances = Rest
		;	metric_distance(Metric, Row, OtherRow, Distance),
			Distances = [Distance-Index| Rest]
		),
		NextIndex is Index + 1,
		indexed_distances(Rows, Row, Metric, NextIndex, ExcludedIndex, Rest).

	take_neighbors(0, _Distances, []) :-
		!.
	take_neighbors(Count, [Distance-Index| Distances], [neighbor(Index, Distance)| Neighbors]) :-
		NextCount is Count - 1,
		take_neighbors(NextCount, Distances, Neighbors).

	metric_distance(euclidean, Left, Right, Distance) :-
		euclidean_distance(Left, Right, Distance).
	metric_distance(manhattan, Left, Right, Distance) :-
		manhattan_distance(Left, Right, Distance).
	metric_distance(cosine, Left, Right, Distance) :-
		cosine_products(Left, Right, 0.0, 0.0, 0.0, DotProduct, LeftNormSquared, RightNormSquared),
		(	LeftNormSquared =< 0.0 ->
			(	RightNormSquared =< 0.0 ->
				Distance = 0.0
			;	Distance = 1.0
			)
		;	RightNormSquared =< 0.0 ->
			Distance = 1.0
		;	Distance0 is 1.0 - DotProduct / sqrt(LeftNormSquared * RightNormSquared),
			Distance is max(0.0, min(2.0, Distance0))
		).

	cosine_products([], [], DotProduct, LeftNormSquared, RightNormSquared, DotProduct, LeftNormSquared, RightNormSquared).
	cosine_products([Left| Lefts], [Right| Rights], DotProduct0, LeftNormSquared0, RightNormSquared0, DotProduct, LeftNormSquared, RightNormSquared) :-
		DotProduct1 is DotProduct0 + Left * Right,
		LeftNormSquared1 is LeftNormSquared0 + Left * Left,
		RightNormSquared1 is RightNormSquared0 + Right * Right,
		cosine_products(Lefts, Rights, DotProduct1, LeftNormSquared1, RightNormSquared1, DotProduct, LeftNormSquared, RightNormSquared).

	fuzzy_graph(NeighborRows, Options, FuzzyGraph) :-
		neighbor_distance_mean(NeighborRows, 0.0, 0, GlobalMean),
		^^option(local_connectivity(LocalConnectivity), Options),
		length(NeighborRows, SampleCount),
		NeighborRows = [FirstNeighborRow| _],
		length(FirstNeighborRow, NeighborCount),
		smooth_neighbor_rows(NeighborRows, LocalConnectivity, GlobalMean, NeighborCount, 0, DirectedMemberships),
		^^option(set_op_mix_ratio(MixRatio), Options),
		merge_directed_memberships(0, SampleCount, DirectedMemberships, MixRatio, FuzzyGraph).

	neighbor_distance_mean([], Sum, Count, Mean) :-
		(	Count > 0 ->
			Mean is Sum / Count
		;	Mean = 0.0
		).
	neighbor_distance_mean([Neighbors| NeighborRows], Sum0, Count0, Mean) :-
		neighbor_row_sum(Neighbors, Sum0, Sum1, Count0, Count1),
		neighbor_distance_mean(NeighborRows, Sum1, Count1, Mean).

	neighbor_row_sum([], Sum, Sum, Count, Count).
	neighbor_row_sum([neighbor(_Index, Distance)| Neighbors], Sum0, Sum, Count0, Count) :-
		Sum1 is Sum0 + Distance,
		Count1 is Count0 + 1,
		neighbor_row_sum(Neighbors, Sum1, Sum, Count1, Count).

	smooth_neighbor_rows([], _LocalConnectivity, _GlobalMean, _NeighborCount, _Index, []).
	smooth_neighbor_rows([[_Self| Neighbors]| NeighborRows], LocalConnectivity, GlobalMean, NeighborCount, Index, DirectedMemberships) :-
		neighbor_distances(Neighbors, Distances),
		local_connectivity_distance(Distances, LocalConnectivity, Rho),
		Target is log(NeighborCount) / log(2.0),
		smooth_sigma(Distances, Rho, Target, Sigma0),
		arithmetic_mean_or_zero(Distances, RowMean),
		(	Rho > 0.0 ->
			MinimumSigma is 0.001 * RowMean
		;	MinimumSigma is 0.001 * GlobalMean
		),
		Sigma is max(Sigma0, MinimumSigma),
		directed_memberships(Neighbors, Index, Rho, Sigma, RowMemberships),
		NextIndex is Index + 1,
		smooth_neighbor_rows(NeighborRows, LocalConnectivity, GlobalMean, NeighborCount, NextIndex, RestMemberships),
		append(RowMemberships, RestMemberships, DirectedMemberships).

	neighbor_distances([], []).
	neighbor_distances([neighbor(_Index, Distance)| Neighbors], [Distance| Distances]) :-
		neighbor_distances(Neighbors, Distances).

	positive_distances([], []).
	positive_distances([Distance| Distances], PositiveDistances) :-
		(	Distance > 0.0 ->
			PositiveDistances = [Distance| Rest]
		;	PositiveDistances = Rest
		),
		positive_distances(Distances, Rest).

	local_connectivity_distance(Distances, LocalConnectivity, Rho) :-
		positive_distances(Distances, PositiveDistances),
		length(PositiveDistances, Count),
		IntegerPart is floor(LocalConnectivity),
		Interpolation is LocalConnectivity - IntegerPart,
		(	Count >= IntegerPart, IntegerPart > 0 ->
			Position is IntegerPart - 1,
			nth0(Position, PositiveDistances, Base),
			(	Interpolation > 1.0e-5, Count > IntegerPart ->
				nth0(IntegerPart, PositiveDistances, Next),
				Rho is Base + Interpolation * (Next - Base)
			;	Rho = Base
			)
		;	PositiveDistances = [Last| Rest] ->
			last_distance(Rest, Last, Rho)
		;	Rho = 0.0
		).

	last_distance([], Last, Last).
	last_distance([Distance| Distances], _Last0, Last) :-
		last_distance(Distances, Distance, Last).

	smooth_sigma(Distances, Rho, Target, Sigma) :-
		smooth_sigma(0, Distances, Rho, Target, 0.0, 1.0e300, 1.0, Sigma).

	smooth_sigma(64, _Distances, _Rho, _Target, _Lower, _Upper, Sigma, Sigma) :-
		!.
	smooth_sigma(Iteration, Distances, Rho, Target, Lower, Upper, Sigma0, Sigma) :-
		membership_sum(Distances, Rho, Sigma0, 0.0, Sum),
		Difference is abs(Sum - Target),
		(	Difference < 1.0e-5 ->
			Sigma = Sigma0
		;	Sum > Target ->
			Upper1 = Sigma0,
			Sigma1 is (Lower + Sigma0) / 2.0,
			NextIteration is Iteration + 1,
			smooth_sigma(NextIteration, Distances, Rho, Target, Lower, Upper1, Sigma1, Sigma)
		;	Lower1 = Sigma0,
			(	Upper > 1.0e299 ->
				Sigma1 is Sigma0 * 2.0
			;	Sigma1 is (Sigma0 + Upper) / 2.0
			),
			NextIteration is Iteration + 1,
			smooth_sigma(NextIteration, Distances, Rho, Target, Lower1, Upper, Sigma1, Sigma)
		).

	membership_sum([], _Rho, _Sigma, Sum, Sum).
	membership_sum([Distance| Distances], Rho, Sigma, Sum0, Sum) :-
		membership_strength(Distance, Rho, Sigma, Strength),
		Sum1 is Sum0 + Strength,
		membership_sum(Distances, Rho, Sigma, Sum1, Sum).

	membership_strength(Distance, Rho, Sigma, Strength) :-
		(	Distance =< Rho ->
			Strength = 1.0
		;	Sigma =< 0.0 ->
			Strength = 1.0
		;	Strength is exp(-((Distance - Rho) / Sigma))
		).

	directed_memberships([], _Source, _Rho, _Sigma, []).
	directed_memberships([neighbor(Target, Distance)| Neighbors], Source, Rho, Sigma, Memberships) :-
		(	Source =:= Target ->
			Memberships = Rest
		;	membership_strength(Distance, Rho, Sigma, Strength),
			Memberships = [directed(Source, Target, Strength)| Rest]
		),
		directed_memberships(Neighbors, Source, Rho, Sigma, Rest).

	merge_directed_memberships(Source, Count, _DirectedMemberships, _MixRatio, []) :-
		Source >= Count,
		!.
	merge_directed_memberships(Source, Count, DirectedMemberships, MixRatio, FuzzyGraph) :-
		Target is Source + 1,
		merge_source_memberships(Source, Target, Count, DirectedMemberships, MixRatio, SourceEdges),
		NextSource is Source + 1,
		merge_directed_memberships(NextSource, Count, DirectedMemberships, MixRatio, RestEdges),
		append(SourceEdges, RestEdges, FuzzyGraph).

	merge_source_memberships(_Source, Target, Count, _DirectedMemberships, _MixRatio, []) :-
		Target >= Count,
		!.
	merge_source_memberships(Source, Target, Count, DirectedMemberships, MixRatio, Edges) :-
		directed_weight(Source, Target, DirectedMemberships, Forward),
		directed_weight(Target, Source, DirectedMemberships, Reverse),
		Union is Forward + Reverse - Forward * Reverse,
		Intersection is Forward * Reverse,
		Weight is MixRatio * Union + (1.0 - MixRatio) * Intersection,
		(	Weight > 0.0 ->
			Edges = [edge(Source, Target, Weight)| Rest]
		;	Edges = Rest
		),
		NextTarget is Target + 1,
		merge_source_memberships(Source, NextTarget, Count, DirectedMemberships, MixRatio, Rest).

	directed_weight(Source, Target, DirectedMemberships, Weight) :-
		(	member(directed(Source, Target, Found), DirectedMemberships) ->
			Weight = Found
		;	Weight = 0.0
		).

	arithmetic_mean_or_zero([], 0.0).
	arithmetic_mean_or_zero([Value| Values], Mean) :-
		arithmetic_mean([Value| Values], Mean).

	fit_curve_parameters(Options, CurveA, CurveB) :-
		^^option(spread(Spread), Options),
		^^option(min_dist(MinDist), Options),
		fit_curve_iterations(0, Spread, MinDist, 0.0, 0.0, LogA, LogB),
		CurveA is exp(LogA),
		CurveB is exp(LogB).

	fit_curve_iterations(25, _Spread, _MinDist, LogA, LogB, LogA, LogB) :-
		!.
	fit_curve_iterations(Iteration, Spread, MinDist, LogA0, LogB0, LogA, LogB) :-
		curve_normal_equations(0, Spread, MinDist, LogA0, LogB0, 0.0, 0.0, 0.0, 0.0, 0.0, H11, H12, H22, G1, G2),
		DampedH11 is H11 + 1.0e-6,
		DampedH22 is H22 + 1.0e-6,
		Determinant is DampedH11 * DampedH22 - H12 * H12,
		(	abs(Determinant) =< 1.0e-18 ->
			LogA = LogA0,
			LogB = LogB0
		;	DeltaA is (-G1 * DampedH22 + H12 * G2) / Determinant,
			DeltaB is (H12 * G1 - DampedH11 * G2) / Determinant,
			Step is max(abs(DeltaA), abs(DeltaB)),
			LogA1 is LogA0 + DeltaA,
			LogB1 is LogB0 + DeltaB,
			(	Step =< 1.0e-9 ->
				LogA = LogA1,
				LogB = LogB1
			;	NextIteration is Iteration + 1,
				fit_curve_iterations(NextIteration, Spread, MinDist, LogA1, LogB1, LogA, LogB)
			)
		).

	curve_normal_equations(300, _Spread, _MinDist, _LogA, _LogB, H110, H120, H220, G10, G20, H11, H12, H22, G1, G2) :-
		!,
		H11 = H110,
		H12 = H120,
		H22 = H220,
		G1 = G10,
		G2 = G20.
	curve_normal_equations(Index, Spread, MinDist, LogA, LogB, H110, H120, H220, G10, G20, H11, H12, H22, G1, G2) :-
		Index < 300,
		Distance is Index * (3.0 * Spread / 299.0),
		curve_target(Distance, Spread, MinDist, Target),
		curve_value_and_jacobian(Distance, LogA, LogB, Value, JacobianA, JacobianB),
		Residual is Value - Target,
		H111 is H110 + JacobianA * JacobianA,
		H121 is H120 + JacobianA * JacobianB,
		H221 is H220 + JacobianB * JacobianB,
		G11 is G10 + JacobianA * Residual,
		G21 is G20 + JacobianB * Residual,
		NextIndex is Index + 1,
		curve_normal_equations(NextIndex, Spread, MinDist, LogA, LogB, H111, H121, H221, G11, G21, H11, H12, H22, G1, G2).

	curve_target(Distance, Spread, MinDist, Target) :-
		(	Distance < MinDist ->
			Target = 1.0
		;	Target is exp(-((Distance - MinDist) / Spread))
		).

	curve_value_and_jacobian(Distance, LogA, LogB, Value, JacobianA, JacobianB) :-
		CurveA is exp(LogA),
		CurveB is exp(LogB),
		(	Distance =< 0.0 ->
			Value = 1.0,
			JacobianA = 0.0,
			JacobianB = 0.0
		;	Power is Distance ** (2.0 * CurveB),
			Term is CurveA * Power,
			Value is 1.0 / (1.0 + Term),
			Common is -Value * (1.0 - Value),
			JacobianA = Common,
			JacobianB is Common * 2.0 * CurveB * log(Distance)
		).

	initialize_embedding(FuzzyGraph, SampleCount, ComponentCount, Options, InitializationUsed, Embedding) :-
		get_random_seed(OriginalSeed),
		^^option(random_seed(RandomSeed), Options),
		catch(
			( randomize_seed(RandomSeed),
				discard_random_values(12),
				initialize_seeded_embedding(FuzzyGraph, SampleCount, ComponentCount, Options, InitializationUsed, Embedding)
			),
			Error,
			( set_random_seed(OriginalSeed),
				throw(Error)
			)
		),
		set_random_seed(OriginalSeed).

	initialize_seeded_embedding(FuzzyGraph, SampleCount, ComponentCount, Options, InitializationUsed, Embedding) :-
		^^option(initialization(Initialization), Options),
		(	Initialization == random ->
			random_embedding(SampleCount, ComponentCount, Embedding),
			InitializationUsed = random
		;	graph_connected(FuzzyGraph, SampleCount),
			catch(
				spectral_embedding(FuzzyGraph, SampleCount, ComponentCount, Embedding),
				_Error,
				fail
			) ->
			InitializationUsed = spectral
		;	random_embedding(SampleCount, ComponentCount, Embedding),
			InitializationUsed = random_fallback
		).

	discard_random_values(0) :-
		!.
	discard_random_values(Count) :-
		random_float(_),
		NextCount is Count - 1,
		discard_random_values(NextCount).

	graph_connected(_FuzzyGraph, 1) :-
		!.
	graph_connected(FuzzyGraph, SampleCount) :-
		reachable_vertices([0], FuzzyGraph, [], Reachable),
		length(Reachable, SampleCount).

	reachable_vertices([], _FuzzyGraph, Reachable, Reachable).
	reachable_vertices([Vertex| Pending], FuzzyGraph, Visited, Reachable) :-
		(	member(Vertex, Visited) ->
			reachable_vertices(Pending, FuzzyGraph, Visited, Reachable)
		;	graph_neighbors(FuzzyGraph, Vertex, Neighbors),
			append(Pending, Neighbors, NextPending),
			reachable_vertices(NextPending, FuzzyGraph, [Vertex| Visited], Reachable)
		).

	graph_neighbors([], _Vertex, []).
	graph_neighbors([edge(Source, Target, _Weight)| Edges], Vertex, Neighbors) :-
		(	Vertex =:= Source ->
			Neighbors = [Target| Rest]
		;	Vertex =:= Target ->
			Neighbors = [Source| Rest]
		;	Neighbors = Rest
		),
		graph_neighbors(Edges, Vertex, Rest).

	spectral_embedding(FuzzyGraph, SampleCount, ComponentCount, Embedding) :-
		graph_degrees(0, SampleCount, FuzzyGraph, Degrees),
		positive_degrees(Degrees),
		normalized_adjacency(0, SampleCount, FuzzyGraph, Degrees, Matrix),
		symmetric_eigen(Matrix, 1.0e-8, 1000, [_Trivial| Eigenvectors], _Eigenvalues),
		take_vectors(ComponentCount, Eigenvectors, Components),
		length(Components, ComponentCount),
		transpose_matrix(Components, Rows0),
		scale_embedding(Rows0, Rows1),
		add_embedding_noise(Rows1, Embedding).

	graph_degrees(Index, SampleCount, _FuzzyGraph, []) :-
		Index >= SampleCount,
		!.
	graph_degrees(Index, SampleCount, FuzzyGraph, [Degree| Degrees]) :-
		vertex_degree(FuzzyGraph, Index, 0.0, Degree),
		NextIndex is Index + 1,
		graph_degrees(NextIndex, SampleCount, FuzzyGraph, Degrees).

	vertex_degree([], _Index, Degree, Degree).
	vertex_degree([edge(Source, Target, Weight)| Edges], Index, Degree0, Degree) :-
		(	Index =:= Source ->
			Degree1 is Degree0 + Weight
		;	Index =:= Target ->
			Degree1 is Degree0 + Weight
		;	Degree1 = Degree0
		),
		vertex_degree(Edges, Index, Degree1, Degree).

	positive_degrees([]).
	positive_degrees([Degree| Degrees]) :-
		Degree > 0.0,
		positive_degrees(Degrees).

	normalized_adjacency(Index, SampleCount, _FuzzyGraph, _Degrees, []) :-
		Index >= SampleCount,
		!.
	normalized_adjacency(Index, SampleCount, FuzzyGraph, Degrees, [Row| Rows]) :-
		normalized_adjacency_row(0, SampleCount, Index, FuzzyGraph, Degrees, Row),
		NextIndex is Index + 1,
		normalized_adjacency(NextIndex, SampleCount, FuzzyGraph, Degrees, Rows).

	normalized_adjacency_row(Column, SampleCount, _Row, _FuzzyGraph, _Degrees, []) :-
		Column >= SampleCount,
		!.
	normalized_adjacency_row(Column, SampleCount, Row, FuzzyGraph, Degrees, [Value| Values]) :-
		graph_weight(Row, Column, FuzzyGraph, Weight),
		nth0(Row, Degrees, RowDegree),
		nth0(Column, Degrees, ColumnDegree),
		Value is Weight / sqrt(RowDegree * ColumnDegree),
		NextColumn is Column + 1,
		normalized_adjacency_row(NextColumn, SampleCount, Row, FuzzyGraph, Degrees, Values).

	graph_weight(Row, Column, FuzzyGraph, Weight) :-
		(	Row < Column ->
			Source = Row, Target = Column
		;	Source = Column, Target = Row
		),
		(	member(edge(Source, Target, Found), FuzzyGraph) ->
			Weight = Found
		;	Weight = 0.0
		).

	take_vectors(0, _Vectors, []) :-
		!.
	take_vectors(Count, [Vector| Vectors], [Vector| Selected]) :-
		NextCount is Count - 1,
		take_vectors(NextCount, Vectors, Selected).

	scale_embedding(Rows, ScaledRows) :-
		maximum_absolute_rows(Rows, 0.0, Maximum),
		(	Maximum > 0.0 ->
			Scale is 10.0 / Maximum
		;	Scale = 1.0
		),
		scale_rows(Rows, Scale, ScaledRows).

	maximum_absolute_rows([], Maximum, Maximum).
	maximum_absolute_rows([Row| Rows], Maximum0, Maximum) :-
		maximum_absolute_row(Row, Maximum0, Maximum1),
		maximum_absolute_rows(Rows, Maximum1, Maximum).

	maximum_absolute_row([], Maximum, Maximum).
	maximum_absolute_row([Value| Values], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, abs(Value)),
		maximum_absolute_row(Values, Maximum1, Maximum).

	scale_rows([], _Scale, []).
	scale_rows([Row| Rows], Scale, [ScaledRow| ScaledRows]) :-
		scale_row(Row, Scale, ScaledRow),
		scale_rows(Rows, Scale, ScaledRows).

	scale_row([], _Scale, []).
	scale_row([Value| Values], Scale, [Scaled| ScaledValues]) :-
		Scaled is Value * Scale,
		scale_row(Values, Scale, ScaledValues).

	add_embedding_noise([], []).
	add_embedding_noise([Row| Rows], [NoisyRow| NoisyRows]) :-
		add_row_noise(Row, NoisyRow),
		add_embedding_noise(Rows, NoisyRows).

	add_row_noise([], []).
	add_row_noise([Value| Values], [Noisy| NoisyValues]) :-
		random_normal(0.0, 0.0001, Noise),
		Noisy is Value + Noise,
		add_row_noise(Values, NoisyValues).

	random_embedding(0, _ComponentCount, []) :-
		!.
	random_embedding(SampleCount, ComponentCount, [Row| Rows]) :-
		random_embedding_row(ComponentCount, Row),
		NextSampleCount is SampleCount - 1,
		random_embedding(NextSampleCount, ComponentCount, Rows).

	random_embedding_row(0, []) :-
		!.
	random_embedding_row(ComponentCount, [Coordinate| Coordinates]) :-
		random_float(Random),
		Coordinate is -10.0 + 20.0 * Random,
		NextComponentCount is ComponentCount - 1,
		random_embedding_row(NextComponentCount, Coordinates).

	optimize_embedding(FuzzyGraph, Embedding0, CurveA, CurveB, Options, Embedding) :-
		get_random_seed(OriginalSeed),
		^^option(random_seed(RandomSeed), Options),
		catch(
			(	randomize_seed(RandomSeed),
				discard_random_values(12),
				optimize_seeded_embedding(FuzzyGraph, Embedding0, CurveA, CurveB, Options, Embedding)
			),
			Error,
			(	set_random_seed(OriginalSeed),
				throw(Error)
			)
		),
		set_random_seed(OriginalSeed),
		!.

	optimize_seeded_embedding(FuzzyGraph, Embedding0, CurveA, CurveB, Options, Embedding) :-
		length(Embedding0, SampleCount),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(negative_sample_rate(NegativeSampleRate), Options),
		build_edge_schedules(FuzzyGraph, MaximumIterations, NegativeSampleRate, Schedules0),
		assignable_rows(Embedding0, AssignableRows),
		optimize_epochs(0, MaximumIterations, SampleCount, CurveA, CurveB, Options, AssignableRows, Schedules0, _Schedules),
		assigned_rows(AssignableRows, Embedding).

	assignable_rows([], []).
	assignable_rows([Row| Rows], [Assignable| Assignables]) :-
		assignable(Assignable, Row),
		assignable_rows(Rows, Assignables).

	assigned_rows([], []).
	assigned_rows([Assignable| Assignables], [Row| Rows]) :-
		Assignable => Row,
		assigned_rows(Assignables, Rows).

	build_edge_schedules(FuzzyGraph, MaximumIterations, NegativeSampleRate, Schedules) :-
		maximum_edge_weight(FuzzyGraph, 0.0, MaximumWeight),
		build_edge_schedules(FuzzyGraph, MaximumWeight, MaximumIterations, NegativeSampleRate, Schedules).

	maximum_edge_weight([], Maximum, Maximum).
	maximum_edge_weight([edge(_Source, _Target, Weight)| Edges], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, Weight),
		maximum_edge_weight(Edges, Maximum1, Maximum).

	build_edge_schedules([], _MaximumWeight, _MaximumIterations, _NegativeSampleRate, []).
	build_edge_schedules([edge(Source, Target, Weight)| Edges], MaximumWeight, MaximumIterations, NegativeSampleRate, Schedules) :-
		EpochsPerSample is MaximumWeight / Weight,
		(	EpochsPerSample =< MaximumIterations ->
			EpochsPerNegativeSample is EpochsPerSample / NegativeSampleRate,
			Schedules = [schedule(Source, Target, EpochsPerSample, EpochsPerSample, EpochsPerNegativeSample, EpochsPerNegativeSample)| Rest]
		;	Schedules = Rest
		),
		build_edge_schedules(Edges, MaximumWeight, MaximumIterations, NegativeSampleRate, Rest).

	optimize_epochs(Epoch, MaximumIterations, _SampleCount, _CurveA, _CurveB, _Options, _AssignableRows, Schedules, Schedules) :-
		Epoch >= MaximumIterations,
		!.
	optimize_epochs(Epoch, MaximumIterations, SampleCount, CurveA, CurveB, Options, AssignableRows, Schedules0, Schedules) :-
		^^option(learning_rate(LearningRate), Options),
		Alpha is LearningRate * (1.0 - Epoch / MaximumIterations),
		^^option(repulsion_strength(RepulsionStrength), Options),
		process_schedules(Schedules0, Epoch, SampleCount, CurveA, CurveB, Alpha, RepulsionStrength, AssignableRows, Schedules1),
		NextEpoch is Epoch + 1,
		optimize_epochs(NextEpoch, MaximumIterations, SampleCount, CurveA, CurveB, Options, AssignableRows, Schedules1, Schedules).

	process_schedules([], _Epoch, _SampleCount, _CurveA, _CurveB, _Alpha, _RepulsionStrength, _AssignableRows, []).
	process_schedules([schedule(Source, Target, EpochsPerSample, NextPositive0, EpochsPerNegativeSample, NextNegative0)| Schedules], Epoch, SampleCount, CurveA, CurveB, Alpha, RepulsionStrength, AssignableRows, [schedule(Source, Target, EpochsPerSample, NextPositive, EpochsPerNegativeSample, NextNegative)| UpdatedSchedules]) :-
		(	NextPositive0 =< Epoch ->
			attractive_update(Source, Target, AssignableRows, CurveA, CurveB, Alpha),
			negative_sample_count(Epoch, NextNegative0, EpochsPerNegativeSample, NegativeCount),
			negative_updates(NegativeCount, Source, SampleCount, AssignableRows, CurveA, CurveB, Alpha, RepulsionStrength),
			NextPositive is NextPositive0 + EpochsPerSample,
			NextNegative is NextNegative0 + NegativeCount * EpochsPerNegativeSample
		;	NextPositive = NextPositive0,
			NextNegative = NextNegative0
		),
		process_schedules(Schedules, Epoch, SampleCount, CurveA, CurveB, Alpha, RepulsionStrength, AssignableRows, UpdatedSchedules).

	negative_sample_count(Epoch, NextNegative, EpochsPerNegativeSample, Count) :-
		(	NextNegative =< Epoch ->
			Count is floor((Epoch - NextNegative) / EpochsPerNegativeSample) + 1
		;	Count = 0
		).

	attractive_update(Source, Target, AssignableRows, CurveA, CurveB, Alpha) :-
		nth0(Source, AssignableRows, SourceAssignable),
		nth0(Target, AssignableRows, TargetAssignable),
		SourceAssignable => SourceRow,
		TargetAssignable => TargetRow,
		squared_row_distance(SourceRow, TargetRow, 0.0, DistanceSquared),
		attractive_coefficient(DistanceSquared, CurveA, CurveB, Coefficient),
		update_pair_rows(SourceRow, TargetRow, Coefficient, Alpha, UpdatedSourceRow, UpdatedTargetRow),
		SourceAssignable <= UpdatedSourceRow,
		TargetAssignable <= UpdatedTargetRow.

	attractive_coefficient(DistanceSquared, CurveA, CurveB, Coefficient) :-
		(	DistanceSquared > 0.0 ->
			Power is DistanceSquared ** (CurveB - 1.0),
			Coefficient is -2.0 * CurveA * CurveB * Power / (CurveA * DistanceSquared ** CurveB + 1.0)
		;	Coefficient = 0.0
		).

	negative_updates(0, _Source, _SampleCount, _AssignableRows, _CurveA, _CurveB, _Alpha, _RepulsionStrength) :-
		!.
	negative_updates(Count, Source, SampleCount, AssignableRows, CurveA, CurveB, Alpha, RepulsionStrength) :-
		random_float(Random),
		Target is floor(Random * SampleCount),
		(	Target =:= Source ->
			true
		;	repulsive_update(Source, Target, AssignableRows, CurveA, CurveB, Alpha, RepulsionStrength)
		),
		NextCount is Count - 1,
		negative_updates(NextCount, Source, SampleCount, AssignableRows, CurveA, CurveB, Alpha, RepulsionStrength).

	repulsive_update(Source, Target, AssignableRows, CurveA, CurveB, Alpha, RepulsionStrength) :-
		nth0(Source, AssignableRows, SourceAssignable),
		nth0(Target, AssignableRows, TargetAssignable),
		SourceAssignable => SourceRow,
		TargetAssignable => TargetRow,
		squared_row_distance(SourceRow, TargetRow, 0.0, DistanceSquared),
		(	DistanceSquared > 0.0 ->
			Coefficient is 2.0 * RepulsionStrength * CurveB / ((0.001 + DistanceSquared) * (CurveA * DistanceSquared ** CurveB + 1.0)),
			update_source_row(SourceRow, TargetRow, Coefficient, Alpha, UpdatedSourceRow),
			SourceAssignable <= UpdatedSourceRow
		;	true
		).

	squared_row_distance([], [], DistanceSquared, DistanceSquared).
	squared_row_distance([Left| Lefts], [Right| Rights], DistanceSquared0, DistanceSquared) :-
		Difference is Left - Right,
		DistanceSquared1 is DistanceSquared0 + Difference * Difference,
		squared_row_distance(Lefts, Rights, DistanceSquared1, DistanceSquared).

	update_pair_rows([], [], _Coefficient, _Alpha, [], []).
	update_pair_rows([Source| Sources], [Target| Targets], Coefficient, Alpha, [UpdatedSource| UpdatedSources], [UpdatedTarget| UpdatedTargets]) :-
		Gradient0 is Coefficient * (Source - Target),
		clip_gradient(Gradient0, Gradient),
		Update is Gradient * Alpha,
		UpdatedSource is Source + Update,
		UpdatedTarget is Target - Update,
		update_pair_rows(Sources, Targets, Coefficient, Alpha, UpdatedSources, UpdatedTargets).

	update_source_row([], [], _Coefficient, _Alpha, []).
	update_source_row([Source| Sources], [Target| Targets], Coefficient, Alpha, [UpdatedSource| UpdatedSources]) :-
		Gradient0 is Coefficient * (Source - Target),
		clip_gradient(Gradient0, Gradient),
		UpdatedSource is Source + Gradient * Alpha,
		update_source_row(Sources, Targets, Coefficient, Alpha, UpdatedSources).

	clip_gradient(Gradient0, Gradient) :-
		(	Gradient0 > 4.0 ->
			Gradient = 4.0
		;	Gradient0 < -4.0 ->
			Gradient = -4.0
		;	Gradient = Gradient0
		).

	embedding_cross_entropy(FuzzyGraph, Embedding, CurveA, CurveB, CrossEntropy) :-
		length(Embedding, SampleCount),
		cross_entropy_rows(0, SampleCount, FuzzyGraph, Embedding, CurveA, CurveB, 0.0, CrossEntropy).

	cross_entropy_rows(Source, SampleCount, _FuzzyGraph, _Embedding, _CurveA, _CurveB, CrossEntropy, CrossEntropy) :-
		Source >= SampleCount,
		!.
	cross_entropy_rows(Source, SampleCount, FuzzyGraph, Embedding, CurveA, CurveB, CrossEntropy0, CrossEntropy) :-
		Target is Source + 1,
		cross_entropy_targets(Target, SampleCount, Source, FuzzyGraph, Embedding, CurveA, CurveB, CrossEntropy0, CrossEntropy1),
		NextSource is Source + 1,
		cross_entropy_rows(NextSource, SampleCount, FuzzyGraph, Embedding, CurveA, CurveB, CrossEntropy1, CrossEntropy).

	cross_entropy_targets(Target, SampleCount, _Source, _FuzzyGraph, _Embedding, _CurveA, _CurveB, CrossEntropy, CrossEntropy) :-
		Target >= SampleCount,
		!.
	cross_entropy_targets(Target, SampleCount, Source, FuzzyGraph, Embedding, CurveA, CurveB, CrossEntropy0, CrossEntropy) :-
		graph_weight(Source, Target, FuzzyGraph, Weight),
		nth0(Source, Embedding, SourceRow),
		nth0(Target, Embedding, TargetRow),
		squared_row_distance(SourceRow, TargetRow, 0.0, DistanceSquared),
		Probability0 is 1.0 / (1.0 + CurveA * DistanceSquared ** CurveB),
		Probability is min(1.0 - 1.0e-12, max(1.0e-12, Probability0)),
		Contribution is -(Weight * log(Probability) + (1.0 - Weight) * log(1.0 - Probability)),
		CrossEntropy1 is CrossEntropy0 + Contribution,
		NextTarget is Target + 1,
		cross_entropy_targets(NextTarget, SampleCount, Source, FuzzyGraph, Embedding, CurveA, CurveB, CrossEntropy1, CrossEntropy).

	transform_memberships(Neighbors, Memberships) :-
		neighbor_distances(Neighbors, Distances),
		length(Neighbors, NeighborCount),
		Target is log(NeighborCount) / log(2.0),
		local_connectivity_distance(Distances, 1.0, Rho),
		smooth_sigma(Distances, Rho, Target, Sigma0),
		arithmetic_mean_or_zero(Distances, MeanDistance),
		Sigma is max(Sigma0, 0.001 * MeanDistance),
		transform_memberships(Neighbors, Rho, Sigma, Memberships).

	transform_memberships([], _Rho, _Sigma, []).
	transform_memberships([neighbor(Index, Distance)| Neighbors], Rho, Sigma, [membership(Index, Strength)| Memberships]) :-
		membership_strength(Distance, Rho, Sigma, Strength),
		transform_memberships(Neighbors, Rho, Sigma, Memberships).

	membership_weighted_coordinates(Memberships, EmbeddingRows, Coordinates) :-
		membership_weighted_rows(Memberships, EmbeddingRows, WeightedRows, 0.0, WeightSum),
		sum_weighted_rows(WeightedRows, WeightSum, Coordinates).

	membership_weighted_rows([], _EmbeddingRows, [], WeightSum, WeightSum).
	membership_weighted_rows([membership(Index, Weight)| Memberships], EmbeddingRows, [Weight-Row| WeightedRows], WeightSum0, WeightSum) :-
		nth0(Index, EmbeddingRows, Row),
		WeightSum1 is WeightSum0 + Weight,
		membership_weighted_rows(Memberships, EmbeddingRows, WeightedRows, WeightSum1, WeightSum).

	sum_weighted_rows([Weight-Row| WeightedRows], WeightSum, Coordinates) :-
		weighted_row(Row, Weight, Initial),
		accumulate_weighted_rows(WeightedRows, Initial, Sum),
		normalize_row(Sum, WeightSum, Coordinates).

	accumulate_weighted_rows([], Sum, Sum).
	accumulate_weighted_rows([Weight-Row| WeightedRows], Sum0, Sum) :-
		add_weighted_row(Row, Weight, Sum0, Sum1),
		accumulate_weighted_rows(WeightedRows, Sum1, Sum).

	weighted_row([], _Weight, []).
	weighted_row([Value| Values], Weight, [Weighted| WeightedValues]) :-
		Weighted is Weight * Value,
		weighted_row(Values, Weight, WeightedValues).

	add_weighted_row([], _Weight, [], []).
	add_weighted_row([Value| Values], Weight, [Sum0| Sums0], [Sum| Sums]) :-
		Sum is Sum0 + Weight * Value,
		add_weighted_row(Values, Weight, Sums0, Sums).

	normalize_row([], _WeightSum, []).
	normalize_row([Sum| Sums], WeightSum, [Value| Values]) :-
		Value is Sum / WeightSum,
		normalize_row(Sums, WeightSum, Values).

	optimize_transform(Memberships, EmbeddingRows, Coordinates0, CurveA, CurveB, Options, Coordinates) :-
		get_random_seed(OriginalSeed),
		^^option(transform_seed(TransformSeed), Options),
		catch(
			(	randomize_seed(TransformSeed),
				discard_random_values(12),
				optimize_seeded_transform(Memberships, EmbeddingRows, Coordinates0, CurveA, CurveB, Options, Coordinates)
			),
			Error,
			(	set_random_seed(OriginalSeed),
				throw(Error)
			)
		),
		set_random_seed(OriginalSeed),
		!.

	optimize_seeded_transform(Memberships, EmbeddingRows, Coordinates0, CurveA, CurveB, Options, Coordinates) :-
		^^option(negative_sample_rate(NegativeSampleRate), Options),
		transform_schedules(Memberships, 100, NegativeSampleRate, Schedules0),
		assignable(CoordinatesAssignable, Coordinates0),
		length(EmbeddingRows, SampleCount),
		optimize_transform_epochs(0, 100, SampleCount, CurveA, CurveB, Options, EmbeddingRows, CoordinatesAssignable, Schedules0, _Schedules),
		CoordinatesAssignable => Coordinates.

	transform_schedules(Memberships, MaximumIterations, NegativeSampleRate, Schedules) :-
		maximum_membership_weight(Memberships, 0.0, MaximumWeight),
		transform_schedules(Memberships, MaximumWeight, MaximumIterations, NegativeSampleRate, Schedules).

	maximum_membership_weight([], Maximum, Maximum).
	maximum_membership_weight([membership(_Index, Weight)| Memberships], Maximum0, Maximum) :-
		Maximum1 is max(Maximum0, Weight),
		maximum_membership_weight(Memberships, Maximum1, Maximum).

	transform_schedules([], _MaximumWeight, _MaximumIterations, _NegativeSampleRate, []).
	transform_schedules([membership(Target, Weight)| Memberships], MaximumWeight, MaximumIterations, NegativeSampleRate, Schedules) :-
		EpochsPerSample is MaximumWeight / Weight,
		(	EpochsPerSample =< MaximumIterations ->
			EpochsPerNegativeSample is EpochsPerSample / NegativeSampleRate,
			Schedules = [transform_schedule(Target, EpochsPerSample, EpochsPerSample, EpochsPerNegativeSample, EpochsPerNegativeSample)| Rest]
		;	Schedules = Rest
		),
		transform_schedules(Memberships, MaximumWeight, MaximumIterations, NegativeSampleRate, Rest).

	optimize_transform_epochs(Epoch, MaximumIterations, _SampleCount, _CurveA, _CurveB, _Options, _EmbeddingRows, _CoordinatesAssignable, Schedules, Schedules) :-
		Epoch >= MaximumIterations,
		!.
	optimize_transform_epochs(Epoch, MaximumIterations, SampleCount, CurveA, CurveB, Options, EmbeddingRows, CoordinatesAssignable, Schedules0, Schedules) :-
		^^option(learning_rate(LearningRate), Options),
		Alpha is LearningRate * (1.0 - Epoch / MaximumIterations),
		^^option(repulsion_strength(RepulsionStrength), Options),
		process_transform_schedules(Schedules0, Epoch, SampleCount, CurveA, CurveB, Alpha, RepulsionStrength, EmbeddingRows, CoordinatesAssignable, Schedules1),
		NextEpoch is Epoch + 1,
		optimize_transform_epochs(NextEpoch, MaximumIterations, SampleCount, CurveA, CurveB, Options, EmbeddingRows, CoordinatesAssignable, Schedules1, Schedules).

	process_transform_schedules([], _Epoch, _SampleCount, _CurveA, _CurveB, _Alpha, _RepulsionStrength, _EmbeddingRows, _CoordinatesAssignable, []).
	process_transform_schedules([transform_schedule(Target, EpochsPerSample, NextPositive0, EpochsPerNegativeSample, NextNegative0)| Schedules], Epoch, SampleCount, CurveA, CurveB, Alpha, RepulsionStrength, EmbeddingRows, CoordinatesAssignable, [transform_schedule(Target, EpochsPerSample, NextPositive, EpochsPerNegativeSample, NextNegative)| UpdatedSchedules]) :-
		(	NextPositive0 =< Epoch ->
			transform_attractive_update(Target, EmbeddingRows, CoordinatesAssignable, CurveA, CurveB, Alpha),
			negative_sample_count(Epoch, NextNegative0, EpochsPerNegativeSample, NegativeCount),
			transform_negative_updates(NegativeCount, SampleCount, EmbeddingRows, CoordinatesAssignable, CurveA, CurveB, Alpha, RepulsionStrength),
			NextPositive is NextPositive0 + EpochsPerSample,
			NextNegative is NextNegative0 + NegativeCount * EpochsPerNegativeSample
		;	NextPositive = NextPositive0,
			NextNegative = NextNegative0
		),
		process_transform_schedules(Schedules, Epoch, SampleCount, CurveA, CurveB, Alpha, RepulsionStrength, EmbeddingRows, CoordinatesAssignable, UpdatedSchedules).

	transform_attractive_update(Target, EmbeddingRows, CoordinatesAssignable, CurveA, CurveB, Alpha) :-
		CoordinatesAssignable => Coordinates0,
		nth0(Target, EmbeddingRows, TargetRow),
		squared_row_distance(Coordinates0, TargetRow, 0.0, DistanceSquared),
		attractive_coefficient(DistanceSquared, CurveA, CurveB, Coefficient),
		update_source_row(Coordinates0, TargetRow, Coefficient, Alpha, Coordinates),
		CoordinatesAssignable <= Coordinates.

	transform_negative_updates(0, _SampleCount, _EmbeddingRows, _CoordinatesAssignable, _CurveA, _CurveB, _Alpha, _RepulsionStrength) :-
		!.
	transform_negative_updates(Count, SampleCount, EmbeddingRows, CoordinatesAssignable, CurveA, CurveB, Alpha, RepulsionStrength) :-
		random_float(Random),
		Target is floor(Random * SampleCount),
		CoordinatesAssignable => Coordinates0,
		nth0(Target, EmbeddingRows, TargetRow),
		squared_row_distance(Coordinates0, TargetRow, 0.0, DistanceSquared),
		(	DistanceSquared > 0.0 ->
			Coefficient is 2.0 * RepulsionStrength * CurveB / ((0.001 + DistanceSquared) * (CurveA * DistanceSquared ** CurveB + 1.0)),
			update_source_row(Coordinates0, TargetRow, Coefficient, Alpha, Coordinates),
			CoordinatesAssignable <= Coordinates
		;	true
		),
		NextCount is Count - 1,
		transform_negative_updates(NextCount, SampleCount, EmbeddingRows, CoordinatesAssignable, CurveA, CurveB, Alpha, RepulsionStrength).

	coordinate_pairs([], _Index, []).
	coordinate_pairs([Coordinate| Coordinates], Index, [Name-Coordinate| Pairs]) :-
		atomic_concat(component_, Index, Name),
		NextIndex is Index + 1,
		coordinate_pairs(Coordinates, NextIndex, Pairs).

	build_diagnostics(AttributeNames, EncodedFeatureCount, SampleCount, ComponentCount, NeighborCount, Metric, FuzzyGraph, CurveA, CurveB, InitializationUsed, InitialCrossEntropy, FinalCrossEntropy, Options, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		length(FuzzyGraph, EdgeCount),
		^^option(feature_scaling(FeatureScaling), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		Diagnostics = [
			model(umap_projection),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			encoded_feature_count(EncodedFeatureCount),
			sample_count(SampleCount),
			component_count(ComponentCount),
			effective_n_neighbors(NeighborCount),
			graph_edge_count(EdgeCount),
			distance_metric(Metric),
			curve_parameters(CurveA, CurveB),
			initialization_used(InitializationUsed),
			iterations(MaximumIterations),
			initial_cross_entropy(InitialCrossEntropy),
			final_cross_entropy(FinalCrossEntropy),
			preprocessing([center(true), feature_scaling(FeatureScaling), continuous_missing_values(mean_imputation), categorical_encoding(one_hot_with_missing_indicator)]),
			implementation_stage(optimized)
		].

	valid_example_ids(ExampleIds) :-
		valid(list, ExampleIds),
		valid_example_ids(ExampleIds, []).

	valid_example_ids([], _SeenIds).
	valid_example_ids([Id| Ids], SeenIds) :-
		ground(Id),
		\+ member(Id, SeenIds),
		valid_example_ids(Ids, [Id| SeenIds]).

	valid_mixed_encoders(Encoders) :-
		valid(list, Encoders),
		valid_mixed_encoders(Encoders, []).

	valid_mixed_encoders([], _SeenAttributes).
	valid_mixed_encoders([continuous(Attribute, Mean, Scale)| Encoders], SeenAttributes) :-
		!,
		atom(Attribute),
		valid(number, Mean),
		valid(positive_number, Scale),
		\+ member(Attribute, SeenAttributes),
		valid_mixed_encoders(Encoders, [Attribute| SeenAttributes]).
	valid_mixed_encoders([categorical(Attribute, Values)| Encoders], SeenAttributes) :-
		atom(Attribute),
		valid_categorical_values(Values),
		\+ member(Attribute, SeenAttributes),
		valid_mixed_encoders(Encoders, [Attribute| SeenAttributes]).

	valid_numeric_rows(Rows, Width, RowCount) :-
		valid(list, Rows),
		length(Rows, RowCount),
		valid_numeric_row_values(Rows, Width).

	valid_numeric_row_values([], _Width).
	valid_numeric_row_values([Row| Rows], Width) :-
		valid(list(number, Width), Row),
		valid_numeric_row_values(Rows, Width).

	valid_fuzzy_graph(FuzzyGraph, SampleCount) :-
		valid(list, FuzzyGraph),
		valid_fuzzy_edges(FuzzyGraph, SampleCount, -1, -1).

	valid_fuzzy_edges([], _SampleCount, _PreviousSource, _PreviousTarget).
	valid_fuzzy_edges([edge(Source, Target, Weight)| Edges], SampleCount, PreviousSource, PreviousTarget) :-
		valid(non_negative_integer, Source),
		valid(non_negative_integer, Target),
		Source < Target,
		Target < SampleCount,
		valid(positive_number, Weight),
		Weight =< 1.0,
		(	Source > PreviousSource ->
			true
		;	Source =:= PreviousSource,
			Target > PreviousTarget
		),
		valid_fuzzy_edges(Edges, SampleCount, Source, Target).

	valid_umap_diagnostics(Diagnostics, FeatureCount, EncodedFeatureCount, SampleCount, ComponentCount, NeighborCount) :-
		^^valid_dimension_reducer_metadata(Diagnostics),
		memberchk(model(umap_projection), Diagnostics),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(encoded_feature_count(EncodedFeatureCount), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(effective_n_neighbors(NeighborCount), Diagnostics),
		memberchk(graph_edge_count(EdgeCount), Diagnostics),
		valid(non_negative_integer, EdgeCount),
		memberchk(distance_metric(Metric), Diagnostics),
		valid_metric(Metric),
		memberchk(curve_parameters(CurveA, CurveB), Diagnostics),
		valid(positive_number, CurveA),
		valid(positive_number, CurveB),
		memberchk(initialization_used(InitializationUsed), Diagnostics),
		valid_initialization_used(InitializationUsed),
		memberchk(iterations(Iterations), Diagnostics),
		valid(positive_integer, Iterations),
		memberchk(initial_cross_entropy(InitialCrossEntropy), Diagnostics),
		valid(non_negative_number, InitialCrossEntropy),
		memberchk(final_cross_entropy(FinalCrossEntropy), Diagnostics),
		valid(non_negative_number, FinalCrossEntropy),
		memberchk(preprocessing([center(true), feature_scaling(FeatureScaling), continuous_missing_values(mean_imputation), categorical_encoding(one_hot_with_missing_indicator)]), Diagnostics),
		valid(boolean, FeatureScaling),
		memberchk(implementation_stage(optimized), Diagnostics).

	check_option_relations(Options) :-
		^^option(min_dist(MinDist), Options),
		^^option(spread(Spread), Options),
		(	MinDist =< Spread ->
			true
		;	domain_error(min_dist_spread, MinDist-Spread)
		).

	check_local_connectivity(Options, NeighborCount) :-
		^^option(local_connectivity(LocalConnectivity), Options),
		(	LocalConnectivity =< NeighborCount ->
			true
		;	domain_error(local_connectivity, LocalConnectivity)
		).

	valid_metric(euclidean).
	valid_metric(manhattan).
	valid_metric(cosine).

	valid_initialization_used(spectral).
	valid_initialization_used(random).
	valid_initialization_used(random_fallback).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(n_neighbors(15)).
	default_option(distance_metric(euclidean)).
	default_option(initialization(spectral)).
	default_option(min_dist(0.1)).
	default_option(spread(1.0)).
	default_option(local_connectivity(1.0)).
	default_option(set_op_mix_ratio(1.0)).
	default_option(learning_rate(1.0)).
	default_option(repulsion_strength(1.0)).
	default_option(negative_sample_rate(5)).
	default_option(maximum_iterations(500)).
	default_option(random_seed(1357911)).
	default_option(transform_seed(42)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(n_neighbors(NeighborCount)) :-
		valid(positive_integer, NeighborCount),
		NeighborCount > 1.
	valid_option(distance_metric(Metric)) :-
		valid_metric(Metric).
	valid_option(initialization(Initialization)) :-
		once((Initialization == spectral; Initialization == random)).
	valid_option(min_dist(MinDist)) :-
		valid(non_negative_number, MinDist).
	valid_option(spread(Spread)) :-
		valid(positive_number, Spread).
	valid_option(local_connectivity(LocalConnectivity)) :-
		valid(positive_number, LocalConnectivity).
	valid_option(set_op_mix_ratio(SetOpMixRatio)) :-
		valid(non_negative_number, SetOpMixRatio),
		SetOpMixRatio =< 1.0.
	valid_option(learning_rate(LearningRate)) :-
		valid(positive_number, LearningRate).
	valid_option(repulsion_strength(RepulsionStrength)) :-
		valid(non_negative_number, RepulsionStrength).
	valid_option(negative_sample_rate(NegativeSampleRate)) :-
		valid(positive_integer, NegativeSampleRate).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(random_seed(RandomSeed)) :-
		valid(positive_integer, RandomSeed).
	valid_option(transform_seed(TransformSeed)) :-
		valid(positive_integer, TransformSeed).

:- end_object.
