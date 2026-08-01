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


:- object(tsne_projection,
	imports(dimension_reducer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-01,
		comment is 'Exact t-distributed Stochastic Neighbor Embedding dimension reducer for continuous datasets.',
		see_also is [kernel_pca_projection, pca_projection, random_projection]
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
		transpose_matrix/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2, nth1/3
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
		check_iteration_options(Options),
		^^dataset_attributes(Dataset, Attributes),
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(Id-AttributeValues, Dataset::example(Id, AttributeValues), Examples),
		check_examples(Dataset, AttributeNames, Examples),
		build_encoders(AttributeNames, Examples, Options, Encoders),
		examples_to_rows(Examples, Encoders, TrainingRows),
		keys(Examples, ExampleIds),
		length(AttributeNames, FeatureCount),
		length(TrainingRows, SampleCount),
		^^option(n_components(RequestedComponentCount), Options),
		MaxComponentCount is min(FeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponentCount, MaxComponentCount, ComponentCount),
		check_perplexity(Options, SampleCount),
		squared_distance_matrix(TrainingRows, DistanceMatrix),
		^^option(perplexity(Perplexity), Options),
		conditional_probability_matrix(DistanceMatrix, Perplexity, ConditionalProbabilities),
		joint_probability_matrix(ConditionalProbabilities, JointProbabilities),
		initialize_embedding(SampleCount, ComponentCount, Options, InitialEmbedding),
		kl_divergence(JointProbabilities, InitialEmbedding, InitialKLDivergence),
		optimize_embedding(JointProbabilities, InitialEmbedding, Options, Convergence, Iterations, FinalDelta, EmbeddingRows),
		kl_divergence(JointProbabilities, EmbeddingRows, FinalKLDivergence),
		build_diagnostics(AttributeNames, SampleCount, ComponentCount, Options, Convergence, Iterations, FinalDelta, InitialKLDivergence, FinalKLDivergence, Diagnostics),
		DimensionReducer = tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		DimensionReducer = tsne_reducer(Encoders, _ExampleIds, TrainingRows, EmbeddingRows, _Diagnostics),
		encode_instance(Encoders, Instance, Features),
		instance_distances(TrainingRows, Features, Distances),
		::dimension_reducer_options(DimensionReducer, Options),
		^^option(perplexity(Perplexity), Options),
		conditional_probabilities(Distances, 0, Perplexity, Probabilities),
		weighted_embedding_mean(Probabilities, EmbeddingRows, InitialCoordinates),
		optimize_new_coordinates(Probabilities, EmbeddingRows, Options, InitialCoordinates, Coordinates),
		coordinate_pairs(Coordinates, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		(	var(DimensionReducer) ->
			instantiation_error
		;	DimensionReducer = tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics),
			^^valid_linear_encoders(Encoders),
			valid_example_ids(ExampleIds),
			length(Encoders, FeatureCount),
			length(ExampleIds, SampleCount),
			SampleCount > 1,
			valid_numeric_rows(TrainingRows, FeatureCount, SampleCount),
			memberchk(component_count(ComponentCount), Diagnostics),
			valid(positive_integer, ComponentCount),
			valid_numeric_rows(EmbeddingRows, ComponentCount, SampleCount),
			valid_tsne_diagnostics(Diagnostics, FeatureCount, SampleCount, ComponentCount) ->
			true
		;	domain_error(dimension_reducer, DimensionReducer)
		).

	example_attribute_values(_-AttributeValues, AttributeValues).

	dimension_reducer_diagnostics_data(tsne_reducer(_Encoders, _ExampleIds, _TrainingRows, _EmbeddingRows, Diagnostics), Diagnostics).

	print_dimension_reducer_properties(tsne_reducer(Encoders, _ExampleIds, TrainingRows, EmbeddingRows, Diagnostics)) :-
		format('t-SNE Dimension Reducer~n', []),
		format('========================~n~n', []),
		format('Diagnostics: ~w~n', [Diagnostics]),
		format('Encoders: ~w~n', [Encoders]),
		length(TrainingRows, SampleCount),
		format('Training rows: ~w~n', [SampleCount]),
		EmbeddingRows = [FirstEmbeddingRow| _],
		length(FirstEmbeddingRow, ComponentCount),
		format('Embedding dimensions: ~w~n', [ComponentCount]).

	check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		length(Examples, SampleCount),
		(	SampleCount >= 2 ->
			true
		;	domain_error(minimum_number_of_examples, SampleCount)
		),
		check_distinct_example_ids(Examples, []),
		check_example_values(Examples, AttributeNames).

	check_example_values([], _AttributeNames).
	check_example_values([_-AttributeValues| Examples], AttributeNames) :-
		check_attribute_bindings(AttributeNames, AttributeValues),
		check_attribute_values(AttributeNames, AttributeValues),
		check_example_values(Examples, AttributeNames).

	check_attribute_bindings(AttributeNames, AttributeValues) :-
		check_declared_attribute_bindings(AttributeNames, AttributeValues),
		check_undeclared_attribute_bindings(AttributeValues, AttributeNames).

	check_declared_attribute_bindings([], _AttributeValues).
	check_declared_attribute_bindings([Attribute| Attributes], AttributeValues) :-
		attribute_occurrences(AttributeValues, Attribute, 0, Count),
		(	Count == 1 ->
			true
		;	Count == 0 ->
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
	check_attribute_values([Attribute| Attributes], AttributeValues) :-
		attribute_value(Attribute, AttributeValues, Value),
		(	var(Value) ->
			true
		;	number(Value) ->
			true
		;	type_error(number, Value)
		),
		check_attribute_values(Attributes, AttributeValues).

	attribute_value(Attribute, AttributeValues, Value) :-
		(	member(Attribute-Value, AttributeValues) ->
			true
		;	existence_error(attribute, Attribute)
		).

	build_encoders([], _Examples, _Options, []).
	build_encoders([Attribute| Attributes], Examples, Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		observed_attribute_values(Examples, Attribute, Values),
		(	Values == [] ->
			domain_error(observed_attribute_values, Attribute)
		;	arithmetic_mean(Values, Mean),
			continuous_scale(Values, Options, Scale)
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
		check_attribute_values(AttributeNames, AttributeValues),
		encode_instance_values(Encoders, AttributeValues, Features).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| Attributes]) :-
		encoder_attribute_names(Encoders, Attributes).

	encode_instance_values([], _AttributeValues, []).
	encode_instance_values([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		attribute_value(Attribute, AttributeValues, Value),
		( var(Value) ->
			Feature = 0.0
		; Feature is (Value - Mean) / Scale
		),
		encode_instance_values(Encoders, AttributeValues, Features).

	check_distinct_example_ids([], _SeenIds).
	check_distinct_example_ids([Id-_AttributeValues| Examples], SeenIds) :-
		(	member(Id, SeenIds) ->
			domain_error(example_identifier, Id)
		;	check_distinct_example_ids(Examples, [Id| SeenIds])
		).

	check_iteration_options(Options) :-
		^^option(early_exaggeration_iterations(EarlyIterations), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		(	EarlyIterations =< MaximumIterations ->
			true
		;	domain_error(early_exaggeration_iterations, EarlyIterations-MaximumIterations)
		).

	check_perplexity(Options, SampleCount) :-
		^^option(perplexity(Perplexity), Options),
		(	Perplexity < SampleCount ->
			true
		;	domain_error(perplexity, Perplexity-SampleCount)
		).

	squared_distance_matrix([], []).
	squared_distance_matrix([Row| Rows], Matrix) :-
		squared_distance_rows([Row| Rows], [Row| Rows], Matrix).

	squared_distance_rows([], _AllRows, []).
	squared_distance_rows([Row| Rows], AllRows, [DistanceRow| DistanceRows]) :-
		instance_distances(AllRows, Row, DistanceRow),
		squared_distance_rows(Rows, AllRows, DistanceRows).

	instance_distances([], _Row, []).
	instance_distances([OtherRow| Rows], Row, [Distance| Distances]) :-
		squared_distance(Row, OtherRow, 0.0, Distance),
		instance_distances(Rows, Row, Distances).

	squared_distance([], [], Distance, Distance).
	squared_distance([Left| Lefts], [Right| Rights], Distance0, Distance) :-
		Difference is Left - Right,
		Distance1 is Distance0 + Difference * Difference,
		squared_distance(Lefts, Rights, Distance1, Distance).

	conditional_probability_matrix(DistanceMatrix, Perplexity, ProbabilityMatrix) :-
		conditional_probability_rows(DistanceMatrix, Perplexity, 1, ProbabilityMatrix).

	conditional_probability_rows([], _Perplexity, _Index, []).
	conditional_probability_rows([Distances| DistanceRows], Perplexity, Index, [Probabilities| ProbabilityRows]) :-
		conditional_probabilities(Distances, Index, Perplexity, Probabilities),
		NextIndex is Index + 1,
		conditional_probability_rows(DistanceRows, Perplexity, NextIndex, ProbabilityRows).

	conditional_probabilities(Distances, ExcludedIndex, Perplexity, Probabilities) :-
		TargetEntropy is log(Perplexity),
		minimum_distance(Distances, ExcludedIndex, MinimumDistance),
		search_precision(Distances, ExcludedIndex, MinimumDistance, TargetEntropy, 0.0, unbounded, 1.0, 0, Probabilities).

	minimum_distance(Distances, ExcludedIndex, MinimumDistance) :-
		minimum_distance(Distances, ExcludedIndex, 1, none, MinimumDistance).

	minimum_distance([], _ExcludedIndex, _Index, MinimumDistance, MinimumDistance) :-
		MinimumDistance \== none.
	minimum_distance([Distance| Distances], ExcludedIndex, Index, Minimum0, Minimum) :-
		(	Index =:= ExcludedIndex ->
			Minimum1 = Minimum0
		;	Minimum0 == none ->
			Minimum1 = Distance
		;	Minimum1 is min(Minimum0, Distance)
		),
		NextIndex is Index + 1,
		minimum_distance(Distances, ExcludedIndex, NextIndex, Minimum1, Minimum).

	search_precision(Distances, ExcludedIndex, MinimumDistance, TargetEntropy, Lower, Upper, Precision, Iteration, Probabilities) :-
		probability_row(Distances, ExcludedIndex, MinimumDistance, Precision, Probabilities0, Entropy),
		EntropyDifference is Entropy - TargetEntropy,
		(	abs(EntropyDifference) =< 1.0e-5 ->
			Probabilities = Probabilities0
		;	Iteration >= 60 ->
			Probabilities = Probabilities0
		;	next_precision(EntropyDifference, Lower, Upper, Precision, NextLower, NextUpper, NextPrecision),
			NextIteration is Iteration + 1,
			search_precision(Distances, ExcludedIndex, MinimumDistance, TargetEntropy, NextLower, NextUpper, NextPrecision, NextIteration, Probabilities)
		).

	next_precision(EntropyDifference, _Lower, Upper, Precision, Precision, Upper, NextPrecision) :-
		EntropyDifference > 0.0,
		!,
		(	Upper == unbounded ->
			NextPrecision is Precision * 2.0
		;	NextPrecision is (Precision + Upper) / 2.0
		).
	next_precision(_EntropyDifference, Lower, _Upper, Precision, Lower, Precision, NextPrecision) :-
		(	Lower =< 0.0 ->
			NextPrecision is Precision / 2.0
		;	NextPrecision is (Precision + Lower) / 2.0
		).

	probability_row(Distances, ExcludedIndex, MinimumDistance, Precision, Probabilities, Entropy) :-
		unnormalized_probabilities(Distances, ExcludedIndex, MinimumDistance, Precision, 1, Unnormalized, 0.0, Sum, 0.0, WeightedDistanceSum),
		normalize_values(Unnormalized, Sum, Probabilities),
		Entropy is log(Sum) + Precision * WeightedDistanceSum / Sum.

	unnormalized_probabilities([], _ExcludedIndex, _MinimumDistance, _Precision, _Index, [], Sum, Sum, WeightedSum, WeightedSum).
	unnormalized_probabilities([Distance| Distances], ExcludedIndex, MinimumDistance, Precision, Index, [Probability| Probabilities], Sum0, Sum, WeightedSum0, WeightedSum) :-
		(	Index =:= ExcludedIndex ->
			Probability = 0.0,
			Sum1 = Sum0,
			WeightedSum1 = WeightedSum0
		;	AdjustedDistance is Distance - MinimumDistance,
			Probability is exp(-Precision * AdjustedDistance),
			Sum1 is Sum0 + Probability,
			WeightedSum1 is WeightedSum0 + Probability * AdjustedDistance
		),
		NextIndex is Index + 1,
		unnormalized_probabilities(Distances, ExcludedIndex, MinimumDistance, Precision, NextIndex, Probabilities, Sum1, Sum, WeightedSum1, WeightedSum).

	normalize_values([], _Sum, []).
	normalize_values([Value| Values], Sum, [Normalized| NormalizedValues]) :-
		Normalized is Value / Sum,
		normalize_values(Values, Sum, NormalizedValues).

	joint_probability_matrix(ConditionalProbabilities, JointProbabilities) :-
		length(ConditionalProbabilities, SampleCount),
		joint_probability_rows(ConditionalProbabilities, ConditionalProbabilities, SampleCount, 1, JointProbabilities).

	joint_probability_rows([], _Matrix, _SampleCount, _Index, []).
	joint_probability_rows([Row| Rows], Matrix, SampleCount, Index, [JointRow| JointRows]) :-
		joint_probability_row(Row, Matrix, SampleCount, Index, 1, JointRow),
		NextIndex is Index + 1,
		joint_probability_rows(Rows, Matrix, SampleCount, NextIndex, JointRows).

	joint_probability_row([], _Matrix, _SampleCount, _RowIndex, _ColumnIndex, []).
	joint_probability_row([Probability| Probabilities], Matrix, SampleCount, RowIndex, ColumnIndex, [JointProbability| JointProbabilities]) :-
		(	RowIndex =:= ColumnIndex ->
			JointProbability = 0.0
		;	nth1(ColumnIndex, Matrix, OtherRow),
			nth1(RowIndex, OtherRow, ReverseProbability),
			JointProbability is (Probability + ReverseProbability) / (2.0 * SampleCount)
		),
		NextColumnIndex is ColumnIndex + 1,
		joint_probability_row(Probabilities, Matrix, SampleCount, RowIndex, NextColumnIndex, JointProbabilities).

	initialize_embedding(SampleCount, ComponentCount, Options, Embedding) :-
		get_random_seed(OriginalSeed),
		^^option(random_seed(RandomSeed), Options),
		catch(
			(	randomize_seed(RandomSeed),
				discard_random_values(12),
				random_embedding(SampleCount, ComponentCount, Embedding)
			),
			Error,
			(	set_random_seed(OriginalSeed),
				throw(Error)
			)
		),
		set_random_seed(OriginalSeed).

	discard_random_values(0) :-
		!.
	discard_random_values(Count) :-
		random_float(_),
		NextCount is Count - 1,
		discard_random_values(NextCount).

	random_embedding(0, _ComponentCount, []) :-
		!.
	random_embedding(SampleCount, ComponentCount, [Row| Rows]) :-
		random_embedding_row(ComponentCount, Row),
		NextSampleCount is SampleCount - 1,
		random_embedding(NextSampleCount, ComponentCount, Rows).

	random_embedding_row(0, []) :-
		!.
	random_embedding_row(ComponentCount, [Coordinate| Coordinates]) :-
		random_normal(0.0, 1.0e-4, Coordinate),
		NextComponentCount is ComponentCount - 1,
		random_embedding_row(NextComponentCount, Coordinates).

	optimize_embedding(Probabilities, Embedding0, Options, Convergence, Iterations, FinalDelta, Embedding) :-
		zero_matrix_like(Embedding0, PreviousUpdates),
		constant_matrix_like(Embedding0, 1.0, Gains),
		iterate_embedding(Probabilities, Options, 0, Embedding0, PreviousUpdates, Gains, Convergence, Iterations, FinalDelta, Embedding).

	iterate_embedding(Probabilities, Options, Iteration0, Embedding0, PreviousUpdates0, Gains0, Convergence, Iterations, FinalDelta, Embedding) :-
		low_dimensional_probabilities(Embedding0, QProbabilities, Weights),
		Iteration is Iteration0 + 1,
		optimization_phase(Iteration, Options, ProbabilityScale, Momentum),
		gradient_matrix(Probabilities, QProbabilities, Weights, Embedding0, ProbabilityScale, Gradients),
		^^option(learning_rate(LearningRate), Options),
		update_embedding(Embedding0, Gradients, PreviousUpdates0, Gains0, LearningRate, Momentum, Embedding1, PreviousUpdates, Gains, Delta),
		center_embedding(Embedding1, CenteredEmbedding),
		^^option(early_exaggeration_iterations(EarlyIterations), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		(	Iteration > EarlyIterations, Delta =< Tolerance ->
			Convergence = tolerance,
			Iterations = Iteration,
			FinalDelta = Delta,
			Embedding = CenteredEmbedding
		;	Iteration >= MaximumIterations ->
			Convergence = maximum_iterations_exhausted,
			Iterations = Iteration,
			FinalDelta = Delta,
			Embedding = CenteredEmbedding
		;	iterate_embedding(Probabilities, Options, Iteration, CenteredEmbedding, PreviousUpdates, Gains, Convergence, Iterations, FinalDelta, Embedding)
		).

	optimization_phase(Iteration, Options, ProbabilityScale, Momentum) :-
		^^option(early_exaggeration_iterations(EarlyIterations), Options),
		(	Iteration =< EarlyIterations ->
			^^option(early_exaggeration(ProbabilityScale), Options),
			Momentum = 0.5
		;	ProbabilityScale = 1.0,
			Momentum = 0.8
		).

	low_dimensional_probabilities(Embedding, Probabilities, Weights) :-
		low_dimensional_weight_rows(Embedding, Embedding, 1, Weights, 0.0, WeightSum),
		normalize_matrix(Weights, WeightSum, Probabilities).

	low_dimensional_weight_rows([], _Embedding, _Index, [], Sum, Sum).
	low_dimensional_weight_rows([Row| Rows], Embedding, Index, [WeightRow| WeightRows], Sum0, Sum) :-
		low_dimensional_weight_row(Embedding, Row, Index, 1, WeightRow, Sum0, Sum1),
		NextIndex is Index + 1,
		low_dimensional_weight_rows(Rows, Embedding, NextIndex, WeightRows, Sum1, Sum).

	low_dimensional_weight_row([], _Row, _RowIndex, _ColumnIndex, [], Sum, Sum).
	low_dimensional_weight_row([OtherRow| Rows], Row, RowIndex, ColumnIndex, [Weight| Weights], Sum0, Sum) :-
		(	RowIndex =:= ColumnIndex ->
			Weight = 0.0,
			Sum1 = Sum0
		;	squared_distance(Row, OtherRow, 0.0, Distance),
			Weight is 1.0 / (1.0 + Distance),
			Sum1 is Sum0 + Weight
		),
		NextColumnIndex is ColumnIndex + 1,
		low_dimensional_weight_row(Rows, Row, RowIndex, NextColumnIndex, Weights, Sum1, Sum).

	normalize_matrix([], _Sum, []).
	normalize_matrix([Row| Rows], Sum, [NormalizedRow| NormalizedRows]) :-
		normalize_values(Row, Sum, NormalizedRow),
		normalize_matrix(Rows, Sum, NormalizedRows).

	gradient_matrix(Probabilities, QProbabilities, Weights, Embedding, ProbabilityScale, Gradients) :-
		gradient_rows(Probabilities, QProbabilities, Weights, Embedding, Embedding, ProbabilityScale, Gradients).

	gradient_rows([], [], [], [], _Embedding, _ProbabilityScale, []).
	gradient_rows([ProbabilityRow| ProbabilityRows], [QRow| QRows], [WeightRow| WeightRows], [Coordinates| CoordinateRows], Embedding, ProbabilityScale, [Gradient| Gradients]) :-
		zero_vector(Coordinates, ZeroGradient),
		gradient_row(ProbabilityRow, QRow, WeightRow, Embedding, Coordinates, ProbabilityScale, ZeroGradient, Gradient),
		gradient_rows(ProbabilityRows, QRows, WeightRows, CoordinateRows, Embedding, ProbabilityScale, Gradients).

	gradient_row([], [], [], [], _Coordinates, _ProbabilityScale, Gradient, Gradient).
	gradient_row([Probability| Probabilities], [QProbability| QProbabilities], [Weight| Weights], [OtherCoordinates| OtherRows], Coordinates, ProbabilityScale, Gradient0, Gradient) :-
		Factor is 4.0 * (ProbabilityScale * Probability - QProbability) * Weight,
		add_scaled_difference(Coordinates, OtherCoordinates, Factor, Gradient0, Gradient1),
		gradient_row(Probabilities, QProbabilities, Weights, OtherRows, Coordinates, ProbabilityScale, Gradient1, Gradient).

	zero_matrix_like([], []).
	zero_matrix_like([Row| Rows], [ZeroRow| ZeroRows]) :-
		zero_vector(Row, ZeroRow),
		zero_matrix_like(Rows, ZeroRows).

	constant_matrix_like([], _Constant, []).
	constant_matrix_like([Row| Rows], Constant, [ConstantRow| ConstantRows]) :-
		constant_vector(Row, Constant, ConstantRow),
		constant_matrix_like(Rows, Constant, ConstantRows).

	zero_vector([], []).
	zero_vector([_Value| Values], [0.0| Zeroes]) :-
		zero_vector(Values, Zeroes).

	constant_vector([], _Constant, []).
	constant_vector([_Value| Values], Constant, [Constant| Constants]) :-
		constant_vector(Values, Constant, Constants).

	add_scaled_difference([], [], _Factor, [], []).
	add_scaled_difference([Left| Lefts], [Right| Rights], Factor, [Accumulator| Accumulators], [Result| Results]) :-
		Result is Accumulator + Factor * (Left - Right),
		add_scaled_difference(Lefts, Rights, Factor, Accumulators, Results).

	update_embedding([], [], [], [], _LearningRate, _Momentum, [], [], [], 0.0).
	update_embedding([Row| Rows], [Gradient| Gradients], [PreviousUpdate| PreviousUpdates0], [Gain| Gains0], LearningRate, Momentum, [UpdatedRow| UpdatedRows], [Update| Updates], [UpdatedGain| UpdatedGains], Delta) :-
		update_row(Row, Gradient, PreviousUpdate, Gain, LearningRate, Momentum, UpdatedRow, Update, UpdatedGain, RowDelta),
		update_embedding(Rows, Gradients, PreviousUpdates0, Gains0, LearningRate, Momentum, UpdatedRows, Updates, UpdatedGains, RowsDelta),
		Delta is max(RowDelta, RowsDelta).

	update_row([], [], [], [], _LearningRate, _Momentum, [], [], [], 0.0).
	update_row([Coordinate| Coordinates], [Gradient| Gradients], [PreviousUpdate| PreviousUpdates], [Gain| Gains], LearningRate, Momentum, [UpdatedCoordinate| UpdatedCoordinates], [Update| Updates], [UpdatedGain| UpdatedGains], Delta) :-
		(	Gradient * PreviousUpdate < 0.0 ->
			UpdatedGain is Gain + 0.2
		;	UpdatedGain is max(0.01, Gain * 0.8)
		),
		Update is Momentum * PreviousUpdate - LearningRate * UpdatedGain * Gradient,
		UpdatedCoordinate is Coordinate + Update,
		update_row(Coordinates, Gradients, PreviousUpdates, Gains, LearningRate, Momentum, UpdatedCoordinates, Updates, UpdatedGains, TailDelta),
		Delta is max(abs(Update), TailDelta).

	center_embedding(Embedding, CenteredEmbedding) :-
		transpose_matrix(Embedding, Columns),
		center_columns(Columns, CenteredColumns),
		transpose_matrix(CenteredColumns, CenteredEmbedding).

	center_columns([], []).
	center_columns([Column| Columns], [CenteredColumn| CenteredColumns]) :-
		arithmetic_mean(Column, Mean),
		subtract_scalar(Column, Mean, CenteredColumn),
		center_columns(Columns, CenteredColumns).

	subtract_scalar([], _Scalar, []).
	subtract_scalar([Value| Values], Scalar, [Result| Results]) :-
		Result is Value - Scalar,
		subtract_scalar(Values, Scalar, Results).

	kl_divergence(Probabilities, Embedding, KLDivergence) :-
		low_dimensional_probabilities(Embedding, QProbabilities, _Weights),
		kl_rows(Probabilities, QProbabilities, 0.0, KLDivergence).

	kl_rows([], [], KLDivergence, KLDivergence).
	kl_rows([ProbabilityRow| ProbabilityRows], [QRow| QRows], KLDivergence0, KLDivergence) :-
		kl_row(ProbabilityRow, QRow, KLDivergence0, KLDivergence1),
		kl_rows(ProbabilityRows, QRows, KLDivergence1, KLDivergence).

	kl_row([], [], KLDivergence, KLDivergence).
	kl_row([Probability| Probabilities], [QProbability| QProbabilities], KLDivergence0, KLDivergence) :-
		(	Probability > 0.0 ->
			SafeQ is max(QProbability, 1.0e-12),
			KLDivergence1 is KLDivergence0 + Probability * log(Probability / SafeQ)
		;	KLDivergence1 = KLDivergence0
		),
		kl_row(Probabilities, QProbabilities, KLDivergence1, KLDivergence).

	weighted_embedding_mean(Probabilities, EmbeddingRows, Mean) :-
		EmbeddingRows = [FirstRow| _],
		zero_vector(FirstRow, Zero),
		weighted_embedding_sum(Probabilities, EmbeddingRows, Zero, Mean).

	weighted_embedding_sum([], [], Mean, Mean).
	weighted_embedding_sum([Probability| Probabilities], [Row| Rows], Mean0, Mean) :-
		add_scaled_row(Row, Probability, Mean0, Mean1),
		weighted_embedding_sum(Probabilities, Rows, Mean1, Mean).

	add_scaled_row([], _Scale, [], []).
	add_scaled_row([Value| Values], Scale, [Accumulator| Accumulators], [Result| Results]) :-
		Result is Accumulator + Scale * Value,
		add_scaled_row(Values, Scale, Accumulators, Results).

	optimize_new_coordinates(Probabilities, EmbeddingRows, Options, Coordinates0, Coordinates) :-
		zero_vector(Coordinates0, PreviousUpdate),
		iterate_new_coordinates(Probabilities, EmbeddingRows, Options, 0, Coordinates0, PreviousUpdate, Coordinates).

	iterate_new_coordinates(Probabilities, EmbeddingRows, Options, Iteration0, Coordinates0, PreviousUpdate0, Coordinates) :-
		new_point_gradient(Probabilities, EmbeddingRows, Coordinates0, Gradient),
		^^option(learning_rate(LearningRate), Options),
		update_new_coordinates(Coordinates0, Gradient, PreviousUpdate0, LearningRate, 0.5, Coordinates1, PreviousUpdate, Delta),
		Iteration is Iteration0 + 1,
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		(	Delta =< Tolerance ->
			Coordinates = Coordinates1
		;	Iteration >= MaximumIterations ->
			Coordinates = Coordinates1
		;	iterate_new_coordinates(Probabilities, EmbeddingRows, Options, Iteration, Coordinates1, PreviousUpdate, Coordinates)
		).

	new_point_gradient(Probabilities, EmbeddingRows, Coordinates, Gradient) :-
		new_point_weights(EmbeddingRows, Coordinates, Weights, 0.0, WeightSum),
		normalize_values(Weights, WeightSum, QProbabilities),
		zero_vector(Coordinates, ZeroGradient),
		gradient_row(Probabilities, QProbabilities, Weights, EmbeddingRows, Coordinates, 1.0, ZeroGradient, Gradient).

	new_point_weights([], _Coordinates, [], Sum, Sum).
	new_point_weights([Row| Rows], Coordinates, [Weight| Weights], Sum0, Sum) :-
		squared_distance(Coordinates, Row, 0.0, Distance),
		Weight is 1.0 / (1.0 + Distance),
		Sum1 is Sum0 + Weight,
		new_point_weights(Rows, Coordinates, Weights, Sum1, Sum).

	update_new_coordinates([], [], [], _LearningRate, _Momentum, [], [], 0.0).
	update_new_coordinates([Coordinate| Coordinates], [Gradient| Gradients], [PreviousUpdate| PreviousUpdates], LearningRate, Momentum, [UpdatedCoordinate| UpdatedCoordinates], [Update| Updates], Delta) :-
		Update is Momentum * PreviousUpdate - LearningRate * Gradient,
		UpdatedCoordinate is Coordinate + Update,
		update_new_coordinates(Coordinates, Gradients, PreviousUpdates, LearningRate, Momentum, UpdatedCoordinates, Updates, TailDelta),
		Delta is max(abs(Update), TailDelta).

	coordinate_pairs([], _Index, []).
	coordinate_pairs([Coordinate| Coordinates], Index, [Name-Coordinate| Pairs]) :-
		atomic_concat(component_, Index, Name),
		NextIndex is Index + 1,
		coordinate_pairs(Coordinates, NextIndex, Pairs).

	build_diagnostics(AttributeNames, SampleCount, ComponentCount, Options, Convergence, Iterations, FinalDelta, InitialKLDivergence, FinalKLDivergence, Diagnostics) :-
		length(AttributeNames, FeatureCount),
		^^option(feature_scaling(FeatureScaling), Options),
		Preprocessing = [center(true), feature_scaling(FeatureScaling), missing_values(mean_imputation)],
		Diagnostics = [
			model(tsne_projection),
			options(Options),
			attribute_names(AttributeNames),
			feature_count(FeatureCount),
			sample_count(SampleCount),
			component_count(ComponentCount),
			convergence(Convergence),
			iterations(Iterations),
			final_delta(FinalDelta),
			initial_kl_divergence(InitialKLDivergence),
			final_kl_divergence(FinalKLDivergence),
			preprocessing(Preprocessing)
		].

	valid_example_ids(ExampleIds) :-
		valid(list, ExampleIds),
		valid_example_ids(ExampleIds, []).

	valid_example_ids([], _SeenIds).
	valid_example_ids([Id| Ids], SeenIds) :-
		ground(Id),
		\+ member(Id, SeenIds),
		valid_example_ids(Ids, [Id| SeenIds]).

	valid_numeric_rows(Rows, Width, RowCount) :-
		valid(list, Rows),
		length(Rows, RowCount),
		valid_numeric_row_values(Rows, Width).

	valid_numeric_row_values([], _Width).
	valid_numeric_row_values([Row| Rows], Width) :-
		valid(list(number, Width), Row),
		valid_numeric_row_values(Rows, Width).

	valid_tsne_diagnostics(Diagnostics, FeatureCount, SampleCount, ComponentCount) :-
		^^valid_dimension_reducer_metadata(Diagnostics),
		memberchk(model(tsne_projection), Diagnostics),
		memberchk(feature_count(FeatureCount), Diagnostics),
		memberchk(sample_count(SampleCount), Diagnostics),
		memberchk(component_count(ComponentCount), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		once((
			Convergence == tolerance
		;	Convergence == maximum_iterations_exhausted
		)),
		memberchk(iterations(Iterations), Diagnostics),
		valid(positive_integer, Iterations),
		memberchk(final_delta(FinalDelta), Diagnostics),
		valid(non_negative_number, FinalDelta),
		memberchk(initial_kl_divergence(InitialKLDivergence), Diagnostics),
		valid(non_negative_number, InitialKLDivergence),
		memberchk(final_kl_divergence(FinalKLDivergence), Diagnostics),
		valid(non_negative_number, FinalKLDivergence),
		memberchk(preprocessing([center(true), feature_scaling(FeatureScaling), missing_values(mean_imputation)]), Diagnostics),
		valid(boolean, FeatureScaling).

	default_option(n_components(2)).
	default_option(feature_scaling(true)).
	default_option(perplexity(5.0)).
	default_option(learning_rate(200.0)).
	default_option(early_exaggeration(12.0)).
	default_option(early_exaggeration_iterations(250)).
	default_option(maximum_iterations(1000)).
	default_option(tolerance(1.0e-7)).
	default_option(random_seed(1357911)).

	valid_option(n_components(Components)) :-
		valid(positive_integer, Components).
	valid_option(feature_scaling(FeatureScaling)) :-
		valid(boolean, FeatureScaling).
	valid_option(perplexity(Perplexity)) :-
		valid(positive_float, Perplexity).
	valid_option(learning_rate(LearningRate)) :-
		valid(positive_float, LearningRate).
	valid_option(early_exaggeration(EarlyExaggeration)) :-
		valid(positive_float, EarlyExaggeration).
	valid_option(early_exaggeration_iterations(Iterations)) :-
		valid(non_negative_integer, Iterations).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		valid(positive_float, Tolerance).
	valid_option(random_seed(RandomSeed)) :-
		valid(positive_integer, RandomSeed).

:- end_object.
