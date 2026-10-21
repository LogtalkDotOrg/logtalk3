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
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-10-21,
		comment is 'Exact t-distributed Stochastic Neighbor Embedding dimension reducer for continuous datasets.',
		see_also is [kernel_pca_projection, pca_projection, random_projection]
	]).

	:- private(gradient_matrix/6).
	:- mode(gradient_matrix(+list(list(number)), +list(list(number)), +number, +list(list(number)), +number, -list(list(number))), one).
	:- info(gradient_matrix/6, [
		comment is 'Computes the unfused training gradient matrix as a utility for tests and benchmarks.',
		argnames is ['Probabilities', 'Weights', 'WeightSum', 'Embedding', 'ProbabilityScale', 'Gradients']
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
		new_vector_like/2, transpose_matrix/2
	]).

	:- uses(list, [
		length/2, memberchk/2
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
		check_examples(Dataset, AttributeNames, Examples, AlignedExamples),
		pairs::values(AlignedExamples, ValueRows),
		transpose_matrix(ValueRows, ValueColumns),
		build_encoders(AttributeNames, ValueColumns, Options, Encoders),
		examples_to_rows(AlignedExamples, Encoders, TrainingRows),
		keys(Examples, ExampleIds),
		length(AttributeNames, FeatureCount),
		length(TrainingRows, SampleCount),
		^^option(n_components(RequestedComponentCount), Options),
		MaxComponentCount is min(FeatureCount, SampleCount - 1),
		^^check_component_count(RequestedComponentCount, MaxComponentCount, ComponentCount),
		check_perplexity(Options, SampleCount),
		squared_distance_matrix(TrainingRows, DistanceMatrix),
		^^option(perplexity(Perplexity), Options),
		conditional_probability_matrix(DistanceMatrix, Perplexity, ConditionalProbabilities, AffinitySearch),
		joint_probability_matrix(ConditionalProbabilities, JointProbabilities),
		initialize_embedding(SampleCount, ComponentCount, Options, InitialEmbedding),
		kl_divergence(JointProbabilities, InitialEmbedding, InitialKLDivergence),
		optimize_embedding(JointProbabilities, InitialEmbedding, Options, Convergence, Iterations, FinalDelta, EmbeddingRows),
		kl_divergence(JointProbabilities, EmbeddingRows, FinalKLDivergence),
		build_diagnostics(AttributeNames, SampleCount, ComponentCount, Options, Convergence, Iterations, FinalDelta, InitialKLDivergence, FinalKLDivergence, AffinitySearch, Diagnostics),
		DimensionReducer = tsne_reducer(Encoders, ExampleIds, TrainingRows, EmbeddingRows, Diagnostics).

	transform(DimensionReducer, Instance, ReducedInstance) :-
		::check_dimension_reducer(DimensionReducer),
		DimensionReducer = tsne_reducer(Encoders, _ExampleIds, TrainingRows, EmbeddingRows, _Diagnostics),
		encode_instance(Encoders, Instance, Features),
		instance_distances(TrainingRows, Features, Distances),
		::dimension_reducer_options(DimensionReducer, Options),
		^^option(perplexity(Perplexity), Options),
		conditional_probabilities(Distances, 0, Perplexity, Probabilities, Achieved, Status),
		(	Status == matched ->
			true
		;	logtalk::print_message(warning, tsne_projection, perplexity_mismatch(Perplexity, Achieved, Status))
		),
		weighted_embedding_mean(Probabilities, EmbeddingRows, InitialCoordinates),
		optimize_new_coordinates(Probabilities, EmbeddingRows, Options, InitialCoordinates, Coordinates),
		coordinate_pairs(Coordinates, 1, ReducedInstance).

	check_dimension_reducer(DimensionReducer) :-
		(	\+ ground(DimensionReducer) ->
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
			encoder_attribute_names(Encoders, AttributeNames),
			memberchk(attribute_names(AttributeNames), Diagnostics),
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

	check_examples(Dataset, AttributeNames, Examples, AlignedExamples) :-
		^^check_examples_non_empty(Dataset, Examples),
		length(Examples, SampleCount),
		(	SampleCount >= 2 ->
			true
		;	domain_error(minimum_number_of_examples, SampleCount)
		),
		avltree::new(SeenIds),
		check_distinct_example_ids(Examples, SeenIds),
		avltree::new(EmptySlots),
		attribute_slots(AttributeNames, EmptySlots, Slots),
		align_examples(Examples, AttributeNames, Slots, AlignedExamples).

	align_examples([], _AttributeNames, _Slots, []).
	align_examples([Id-AttributeValues| Examples], AttributeNames, Slots, [Id-Values| AlignedExamples]) :-
		prepare_instance(AttributeNames, Slots, AttributeValues, Values),
		align_examples(Examples, AttributeNames, Slots, AlignedExamples).

	attribute_slots([], Slots, Slots).
	attribute_slots([Attribute| Attributes], Slots0, Slots) :-
		avltree::insert(Slots0, Attribute, binding(0, _), Slots1),
		attribute_slots(Attributes, Slots1, Slots).

	prepare_instance(AttributeNames, Slots0, AttributeValues, Values) :-
		collect_bindings(AttributeValues, Slots0, Slots, Undeclared),
		declared_values(AttributeNames, Slots, Values),
		(	Undeclared = [Attribute| _] ->
			domain_error(declared_attribute, Attribute)
		;	true
		),
		check_observed_values(Values).

	collect_bindings([], Slots, Slots, []).
	collect_bindings([Attribute-Value| Bindings], Slots0, Slots, Undeclared) :-
		(	avltree::lookup(Attribute, binding(Count0, FirstValue), Slots0) ->
			Count is Count0 + 1,
			(	Count0 =:= 0 ->
				StoredValue = Value
			;	StoredValue = FirstValue
			),
			avltree::insert(Slots0, Attribute, binding(Count, StoredValue), Slots1),
			collect_bindings(Bindings, Slots1, Slots, Undeclared)
		;	Undeclared = [Attribute| Remaining],
			collect_bindings(Bindings, Slots0, Slots, Remaining)
		).

	declared_values([], _Slots, []).
	declared_values([Attribute| Attributes], Slots, [Value| Values]) :-
		avltree::lookup(Attribute, binding(Count, Value), Slots),
		(	Count == 1 ->
			true
		;	Count == 0 ->
			existence_error(attribute, Attribute)
		;	domain_error(attribute_occurrences, Attribute)
		),
		declared_values(Attributes, Slots, Values).

	check_observed_values([]).
	check_observed_values([Value| Values]) :-
		(	var(Value) ->
			true
		;	number(Value) ->
			true
		;	type_error(number, Value)
		),
		check_observed_values(Values).

	build_encoders([], [], _Options, []).
	build_encoders([Attribute| Attributes], [Column| Columns], Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		observed_values(Column, Values),
		(	Values == [] ->
			domain_error(observed_attribute_values, Attribute)
		;	arithmetic_mean(Values, Mean),
			continuous_scale(Values, Options, Scale)
		),
		build_encoders(Attributes, Columns, Options, Encoders).

	observed_values([], []).
	observed_values([Value| Observations], Values) :-
		(	nonvar(Value) ->
			Values = [Value| Rest]
		;	Values = Rest
		),
		observed_values(Observations, Rest).

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
	examples_to_rows([_-Values| Examples], Encoders, [Row| Rows]) :-
		encode_instance_values(Encoders, Values, Row),
		examples_to_rows(Examples, Encoders, Rows).

	encode_instance(Encoders, AttributeValues, Features) :-
		encoder_attribute_names(Encoders, AttributeNames),
		avltree::new(EmptySlots),
		attribute_slots(AttributeNames, EmptySlots, Slots),
		prepare_instance(AttributeNames, Slots, AttributeValues, Values),
		encode_instance_values(Encoders, Values, Features).

	encoder_attribute_names([], []).
	encoder_attribute_names([continuous(Attribute, _Mean, _Scale)| Encoders], [Attribute| Attributes]) :-
		encoder_attribute_names(Encoders, Attributes).

	encode_instance_values([], [], []).
	encode_instance_values([continuous(_Attribute, Mean, Scale)| Encoders], [Value| Values], [Feature| Features]) :-
		(	var(Value) ->
			Feature = 0.0
		;	Feature is (Value - Mean) / Scale
		),
		encode_instance_values(Encoders, Values, Features).

	check_distinct_example_ids([], _SeenIds).
	check_distinct_example_ids([Id-_AttributeValues| Examples], SeenIds) :-
		(	\+ ground(Id) ->
			instantiation_error
		;	avltree::lookup(Id, _, SeenIds) ->
			domain_error(example_identifier, Id)
		;	avltree::insert(SeenIds, Id, true, UpdatedIds),
			check_distinct_example_ids(Examples, UpdatedIds)
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
		(	Perplexity =< SampleCount - 1 ->
			true
		;	domain_error(perplexity, Perplexity-SampleCount)
		).

	squared_distance_matrix([], []).
	squared_distance_matrix([Row| Rows], Matrix) :-
		symmetric_matrix([Row| Rows], distance, Matrix).

	symmetric_matrix(Rows, Kind, Matrix) :-
		length(Rows, Count),
		length(Matrix, Count),
		symmetric_pair_rows(Rows, Matrix, Kind).

	symmetric_pair_rows([], [], _Kind).
	symmetric_pair_rows([Row| Rows], [[0.0| Cells]| Cursors], Kind) :-
		(	Kind == distance ->
			distance_pairs(Rows, Row, Cells, Cursors, NextCursors)
		;	weight_pairs(Rows, Row, Cells, Cursors, NextCursors)
		),
		symmetric_pair_rows(Rows, NextCursors, Kind).

	distance_pairs([], _Row, [], [], []).
	distance_pairs([Other| Rows], Row, [Distance| Cells], [[Distance| Tail]| Cursors], [Tail| NextCursors]) :-
		squared_distance(Row, Other, 0.0, Distance),
		distance_pairs(Rows, Row, Cells, Cursors, NextCursors).

	weight_pairs([], _Row, [], [], []).
	weight_pairs([Other| Rows], Row, [Weight| Cells], [[Weight| Tail]| Cursors], [Tail| NextCursors]) :-
		squared_distance(Row, Other, 0.0, Distance),
		Weight is 1.0 / (1.0 + Distance),
		weight_pairs(Rows, Row, Cells, Cursors, NextCursors).

	instance_distances([], _Row, []).
	instance_distances([OtherRow| Rows], Row, [Distance| Distances]) :-
		squared_distance(Row, OtherRow, 0.0, Distance),
		instance_distances(Rows, Row, Distances).

	squared_distance([], [], Distance, Distance).
	squared_distance([Left| Lefts], [Right| Rights], Distance0, Distance) :-
		Difference is Left - Right,
		Distance1 is Distance0 + Difference * Difference,
		squared_distance(Lefts, Rights, Distance1, Distance).

	conditional_probability_matrix(DistanceMatrix, Perplexity, ProbabilityMatrix, AffinitySearch) :-
		conditional_probability_rows(DistanceMatrix, Perplexity, 1, ProbabilityMatrix, Outcomes),
		Outcomes = [First-_| _],
		affinity_summary(Outcomes, summary(0, 0, 0), First, First, AffinitySearch).

	conditional_probability_rows([], _Perplexity, _Index, [], []).
	conditional_probability_rows([Distances| DistanceRows], Perplexity, Index, [Probabilities| ProbabilityRows], [Achieved-Status| Outcomes]) :-
		conditional_probabilities(Distances, Index, Perplexity, Probabilities, Achieved, Status),
		NextIndex is Index + 1,
		conditional_probability_rows(DistanceRows, Perplexity, NextIndex, ProbabilityRows, Outcomes).

	conditional_probabilities(Distances, ExcludedIndex, Perplexity, Probabilities, Achieved, Status) :-
		minimum_distance(Distances, ExcludedIndex, MinimumDistance),
		nearest_count(Distances, ExcludedIndex, MinimumDistance, 1, 0, NearestCount),
		length(Distances, Count),
		(	ExcludedIndex > 0 ->
			EligibleCount is Count - 1
		;	EligibleCount = Count
		),
		(	Perplexity =< NearestCount ->
			limiting_probabilities(Distances, ExcludedIndex, nearest(MinimumDistance), NearestCount, 1, Probabilities),
			Achieved is float(NearestCount),
			(	Perplexity =:= NearestCount ->
				Status = matched
			;	Status = tie_limited
			)
		;	Perplexity =:= EligibleCount ->
			limiting_probabilities(Distances, ExcludedIndex, all, EligibleCount, 1, Probabilities),
			Achieved is float(EligibleCount),
			Status = matched
		;	TargetEntropy is log(Perplexity),
			search_precision(Distances, ExcludedIndex, MinimumDistance, TargetEntropy, 0.0, unbounded, 1.0, 0, Probabilities, Achieved, Status)
		).

	nearest_count([], _Excluded, _Minimum, _Index, Count, Count).
	nearest_count([Distance| Distances], Excluded, Minimum, Index, Count0, Count) :-
		(	Index =\= Excluded, Distance =:= Minimum ->
			Count1 is Count0 + 1
		;	Count1 = Count0
		),
		Next is Index + 1,
		nearest_count(Distances, Excluded, Minimum, Next, Count1, Count).

	limiting_probabilities([], _Excluded, _Mode, _Count, _Index, []).
	limiting_probabilities([Distance| Distances], Excluded, Mode, Count, Index, [Probability| Probabilities]) :-
		(	Index =:= Excluded ->
			Probability = 0.0
		;	Mode == all ->
			Probability is 1.0 / Count
		;	Mode = nearest(Minimum), Distance =:= Minimum ->
			Probability is 1.0 / Count
		;	Probability = 0.0
		),
		Next is Index + 1,
		limiting_probabilities(Distances, Excluded, Mode, Count, Next, Probabilities).

	affinity_summary([], Summary, Minimum, Maximum, affinity_search(Summary, achieved_perplexity_range(Minimum, Maximum))).
	affinity_summary([Achieved-Status| Outcomes], summary(Matched0, Tied0, Limited0), Minimum0, Maximum0, Summary) :-
		(	Status == matched ->
			Matched is Matched0 + 1,
			Tied = Tied0,
			Limited = Limited0
		;	Status == tie_limited ->
			Matched = Matched0,
			Tied is Tied0 + 1,
			Limited = Limited0
		;	Matched = Matched0,
			Tied = Tied0,
			Limited is Limited0 + 1
		),
		Minimum is min(Minimum0, Achieved),
		Maximum is max(Maximum0, Achieved),
		affinity_summary(Outcomes, summary(Matched, Tied, Limited), Minimum, Maximum, Summary).

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

	search_precision(Distances, ExcludedIndex, MinimumDistance, TargetEntropy, Lower, Upper, Precision, Iteration, Probabilities, Achieved, Status) :-
		shift_distances(Distances, ExcludedIndex, MinimumDistance, 1, Shifted),
		search_adjusted_precision(Shifted, TargetEntropy, Lower, Upper, Precision, Iteration, Probabilities, Achieved, Status).

	search_adjusted_precision(Shifted, TargetEntropy, Lower, Upper, Precision, Iteration, Probabilities, Achieved, Status) :-
		precision_weights(Shifted, Precision, Weights, 0.0, Sum, 0.0, WeightedSum),
		Entropy is log(Sum) + Precision * WeightedSum / Sum,
		EntropyDifference is Entropy - TargetEntropy,
		(	abs(EntropyDifference) =< 1.0e-5 ->
			normalize_values(Weights, Sum, Probabilities),
			Achieved is exp(Entropy),
			Status = matched
		;	Iteration >= 60 ->
			normalize_values(Weights, Sum, Probabilities),
			Achieved is exp(Entropy),
			Status = iteration_limit
		;	next_precision(EntropyDifference, Lower, Upper, Precision, NextLower, NextUpper, NextPrecision),
			NextIteration is Iteration + 1,
			search_adjusted_precision(Shifted, TargetEntropy, NextLower, NextUpper, NextPrecision, NextIteration, Probabilities, Achieved, Status)
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

	shift_distances([], _Excluded, _Minimum, _Index, []).
	shift_distances([Distance| Distances], Excluded, Minimum, Index, [Adjusted| Shifted]) :-
		(	Index =:= Excluded ->
			Adjusted = excluded
		;	Adjusted is Distance - Minimum
		),
		Next is Index + 1,
		shift_distances(Distances, Excluded, Minimum, Next, Shifted).

	precision_weights([], _Precision, [], Sum, Sum, WeightedSum, WeightedSum).
	precision_weights([AdjustedDistance| Distances], Precision, [Probability| Probabilities], Sum0, Sum, WeightedSum0, WeightedSum) :-
		(	AdjustedDistance == excluded ->
			Probability = 0.0,
			Sum1 = Sum0,
			WeightedSum1 = WeightedSum0
		;	Probability is exp(-Precision * AdjustedDistance),
			Sum1 is Sum0 + Probability,
			WeightedSum1 is WeightedSum0 + Probability * AdjustedDistance
		),
		precision_weights(Distances, Precision, Probabilities, Sum1, Sum, WeightedSum1, WeightedSum).

	normalize_values([], _Sum, []).
	normalize_values([Value| Values], Sum, [Normalized| NormalizedValues]) :-
		Normalized is Value / Sum,
		normalize_values(Values, Sum, NormalizedValues).

	joint_probability_matrix(ConditionalProbabilities, JointProbabilities) :-
		length(ConditionalProbabilities, SampleCount),
		transpose_matrix(ConditionalProbabilities, Transposed),
		joint_probability_rows(ConditionalProbabilities, Transposed, SampleCount, 1, JointProbabilities).

	joint_probability_rows([], [], _SampleCount, _Index, []).
	joint_probability_rows([Row| Rows], [Column| Columns], SampleCount, Index, [JointRow| JointRows]) :-
		joint_probability_row(Row, Column, SampleCount, Index, 1, JointRow),
		NextIndex is Index + 1,
		joint_probability_rows(Rows, Columns, SampleCount, NextIndex, JointRows).

	joint_probability_row([], [], _SampleCount, _RowIndex, _ColumnIndex, []).
	joint_probability_row([Probability| Probabilities], [ReverseProbability| ReverseProbabilities], SampleCount, RowIndex, ColumnIndex, [JointProbability| JointProbabilities]) :-
		(	RowIndex =:= ColumnIndex ->
			JointProbability = 0.0
		;	JointProbability is (Probability + ReverseProbability) / (2.0 * SampleCount)
		),
		NextColumnIndex is ColumnIndex + 1,
		joint_probability_row(Probabilities, ReverseProbabilities, SampleCount, RowIndex, NextColumnIndex, JointProbabilities).

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
		^^option(learning_rate(LearningRate), Options),
		^^option(early_exaggeration(EarlyScale), Options),
		^^option(early_exaggeration_iterations(EarlyIterations), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		Configuration = training_config(LearningRate, EarlyScale, EarlyIterations, MaximumIterations, Tolerance),
		iterate_embedding(Probabilities, Configuration, 0, Embedding0, PreviousUpdates, Gains, Convergence, Iterations, FinalDelta, Embedding).

	iterate_embedding(Probabilities, Configuration, Iteration0, Embedding0, PreviousUpdates0, Gains0, Convergence, Iterations, FinalDelta, Embedding) :-
		Configuration = training_config(LearningRate, EarlyScale, EarlyIterations, MaximumIterations, Tolerance),
		low_dimensional_weights(Embedding0, Weights, WeightSum),
		Iteration is Iteration0 + 1,
		optimization_phase(Iteration, EarlyIterations, EarlyScale, ProbabilityScale, Momentum),
		training_step(Probabilities, Weights, WeightSum, Embedding0, ProbabilityScale, PreviousUpdates0, Gains0, LearningRate, Momentum, Embedding1, PreviousUpdates, Gains, Delta),
		center_embedding(Embedding1, CenteredEmbedding),
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
		;	iterate_embedding(Probabilities, Configuration, Iteration, CenteredEmbedding, PreviousUpdates, Gains, Convergence, Iterations, FinalDelta, Embedding)
		).

	optimization_phase(Iteration, EarlyIterations, EarlyScale, ProbabilityScale, Momentum) :-
		(	Iteration =< EarlyIterations ->
			ProbabilityScale = EarlyScale,
			Momentum = 0.5
		;	ProbabilityScale = 1.0,
			Momentum = 0.8
		).

	low_dimensional_weights(Embedding, Weights, WeightSum) :-
		length(Embedding, Count),
		length(Weights, Count),
		weight_sum_rows(Embedding, Weights, Weights, 0, 0.0, WeightSum).

	weight_sum_rows([], [], [], _PrefixCount, Sum, Sum).
	weight_sum_rows([Row| Rows], [FullRow| FullRows], [[0.0| Cells]| Cursors], PrefixCount, Sum0, Sum) :-
		weight_prefix_sum(PrefixCount, FullRow, Sum0, PrefixSum),
		DiagonalSum is PrefixSum + 0.0,
		weight_sum_pairs(Rows, Row, Cells, Cursors, NextCursors, DiagonalSum, Sum1),
		NextPrefixCount is PrefixCount + 1,
		weight_sum_rows(Rows, FullRows, NextCursors, NextPrefixCount, Sum1, Sum).

	weight_prefix_sum(0, _Row, Sum, Sum) :-
		!.
	weight_prefix_sum(Count, [Weight| Weights], Sum0, Sum) :-
		Sum1 is Sum0 + Weight,
		Next is Count - 1,
		weight_prefix_sum(Next, Weights, Sum1, Sum).

	weight_sum_pairs([], _Row, [], [], [], Sum, Sum).
	weight_sum_pairs([Other| Rows], Row, [Weight| Cells], [[Weight| Tail]| Cursors], [Tail| NextCursors], Sum0, Sum) :-
		squared_distance(Row, Other, 0.0, Distance),
		Weight is 1.0 / (1.0 + Distance),
		Sum1 is Sum0 + Weight,
		weight_sum_pairs(Rows, Row, Cells, Cursors, NextCursors, Sum1, Sum).

	gradient_matrix(Probabilities, Weights, WeightSum, Embedding, ProbabilityScale, Gradients) :-
		transpose_matrix(Embedding, Columns),
		gradient_rows(Probabilities, Weights, WeightSum, Embedding, Columns, ProbabilityScale, Gradients).

	gradient_rows([], [], _WeightSum, [], _Columns, _ProbabilityScale, []).
	gradient_rows([ProbabilityRow| ProbabilityRows], [WeightRow| WeightRows], WeightSum, [Coordinates| CoordinateRows], Columns, ProbabilityScale, [Gradient| Gradients]) :-
		gradient_coefficients(ProbabilityRow, WeightRow, WeightSum, ProbabilityScale, 4.0, Coefficients),
		gradient_coordinates(Coordinates, Columns, Coefficients, Gradient),
		gradient_rows(ProbabilityRows, WeightRows, WeightSum, CoordinateRows, Columns, ProbabilityScale, Gradients).

	gradient_coefficients([], [], _WeightSum, _ProbabilityScale, _DerivativeScale, []).
	gradient_coefficients([Probability| Probabilities], [Weight| Weights], WeightSum, ProbabilityScale, DerivativeScale, [Factor| Factors]) :-
		Factor is DerivativeScale * (ProbabilityScale * Probability - Weight / WeightSum) * Weight,
		gradient_coefficients(Probabilities, Weights, WeightSum, ProbabilityScale, DerivativeScale, Factors).

	gradient_coordinates([], [], _Coefficients, []).
	gradient_coordinates([Coordinate| Coordinates], [Column| Columns], Coefficients, [Gradient| Gradients]) :-
		coordinate_gradient(Column, Coefficients, Coordinate, 0.0, Gradient),
		gradient_coordinates(Coordinates, Columns, Coefficients, Gradients).

	coordinate_gradient([], [], _Coordinate, Gradient, Gradient).
	coordinate_gradient([Other| Others], [Factor| Factors], Coordinate, Gradient0, Gradient) :-
		Gradient1 is Gradient0 + Factor * (Coordinate - Other),
		coordinate_gradient(Others, Factors, Coordinate, Gradient1, Gradient).

	zero_matrix_like([], []).
	zero_matrix_like([Row| Rows], [ZeroRow| ZeroRows]) :-
		new_vector_like(Row, ZeroRow),
		zero_matrix_like(Rows, ZeroRows).

	constant_matrix_like([], _Constant, []).
	constant_matrix_like([Row| Rows], Constant, [ConstantRow| ConstantRows]) :-
		constant_vector(Row, Constant, ConstantRow),
		constant_matrix_like(Rows, Constant, ConstantRows).

	constant_vector([], _Constant, []).
	constant_vector([_Value| Values], Constant, [Constant| Constants]) :-
		constant_vector(Values, Constant, Constants).

	training_step(Probabilities, Weights, WeightSum, Embedding, ProbabilityScale, PreviousUpdates, Gains, LearningRate, Momentum, UpdatedEmbedding, Updates, UpdatedGains, Delta) :-
		transpose_matrix(Embedding, Columns),
		gradient_update_rows(Probabilities, Weights, WeightSum, Embedding, Columns, ProbabilityScale, PreviousUpdates, Gains, LearningRate, Momentum, UpdatedEmbedding, Updates, UpdatedGains, 0.0, Delta).

	gradient_update_rows([], [], _WeightSum, [], _Columns, _ProbabilityScale, [], [], _LearningRate, _Momentum, [], [], [], Delta, Delta).
	gradient_update_rows([ProbabilityRow| ProbabilityRows], [WeightRow| WeightRows], WeightSum, [Row| Rows], Columns, ProbabilityScale, [PreviousUpdate| PreviousUpdates0], [Gain| Gains0], LearningRate, Momentum, [UpdatedRow| UpdatedRows], [Update| Updates], [UpdatedGain| UpdatedGains], Delta0, Delta) :-
		gradient_coefficients(ProbabilityRow, WeightRow, WeightSum, ProbabilityScale, 4.0, Coefficients),
		gradient_coordinates(Row, Columns, Coefficients, Gradient),
		update_row(Row, Gradient, PreviousUpdate, Gain, LearningRate, Momentum, UpdatedRow, Update, UpdatedGain, 0.0, RowDelta),
		Delta1 is max(RowDelta, Delta0),
		gradient_update_rows(ProbabilityRows, WeightRows, WeightSum, Rows, Columns, ProbabilityScale, PreviousUpdates0, Gains0, LearningRate, Momentum, UpdatedRows, Updates, UpdatedGains, Delta1, Delta).

	update_row([], [], [], [], _LearningRate, _Momentum, [], [], [], Delta, Delta).
	update_row([Coordinate| Coordinates], [Gradient| Gradients], [PreviousUpdate| PreviousUpdates], [Gain| Gains], LearningRate, Momentum, [UpdatedCoordinate| UpdatedCoordinates], [Update| Updates], [UpdatedGain| UpdatedGains], Delta0, Delta) :-
		(	Gradient * PreviousUpdate < 0.0 ->
			UpdatedGain is Gain + 0.2
		;	UpdatedGain is max(0.01, Gain * 0.8)
		),
		Update is Momentum * PreviousUpdate - LearningRate * UpdatedGain * Gradient,
		UpdatedCoordinate is Coordinate + Update,
		Delta1 is max(abs(Update), Delta0),
		update_row(Coordinates, Gradients, PreviousUpdates, Gains, LearningRate, Momentum, UpdatedCoordinates, Updates, UpdatedGains, Delta1, Delta).

	center_embedding([], []).
	center_embedding([First| Rows], CenteredEmbedding) :-
		new_vector_like(First, Zero),
		coordinate_sums([First| Rows], Zero, Sums),
		length([First| Rows], Count),
		normalize_values(Sums, Count, Means),
		center_rows([First| Rows], Means, CenteredEmbedding).

	coordinate_sums([], Sums, Sums).
	coordinate_sums([Row| Rows], Sums0, Sums) :-
		add_coordinates(Row, Sums0, Sums1),
		coordinate_sums(Rows, Sums1, Sums).

	add_coordinates([], [], []).
	add_coordinates([Value| Values], [Sum| Sums], [Result| Results]) :-
		Result is Sum + Value,
		add_coordinates(Values, Sums, Results).

	center_rows([], _Means, []).
	center_rows([Row| Rows], Means, [Centered| CenteredRows]) :-
		subtract_means(Row, Means, Centered),
		center_rows(Rows, Means, CenteredRows).

	subtract_means([], [], []).
	subtract_means([Value| Values], [Mean| Means], [Centered| CenteredValues]) :-
		Centered is Value - Mean,
		subtract_means(Values, Means, CenteredValues).

	kl_divergence(Probabilities, Embedding, KLDivergence) :-
		low_dimensional_weights(Embedding, Weights, WeightSum),
		kl_rows(Probabilities, Weights, WeightSum, 0.0, RawDivergence),
		KLDivergence is max(0.0, RawDivergence).

	kl_rows([], [], _WeightSum, KLDivergence, KLDivergence).
	kl_rows([ProbabilityRow| ProbabilityRows], [WeightRow| WeightRows], WeightSum, KLDivergence0, KLDivergence) :-
		kl_row(ProbabilityRow, WeightRow, WeightSum, KLDivergence0, KLDivergence1),
		kl_rows(ProbabilityRows, WeightRows, WeightSum, KLDivergence1, KLDivergence).

	kl_row([], [], _WeightSum, KLDivergence, KLDivergence).
	kl_row([Probability| Probabilities], [Weight| Weights], WeightSum, KLDivergence0, KLDivergence) :-
		(	Probability > 0.0 ->
			SafeQ is max(Weight / WeightSum, 1.0e-12),
			KLDivergence1 is KLDivergence0 + Probability * log(Probability / SafeQ)
		;	KLDivergence1 = KLDivergence0
		),
		kl_row(Probabilities, Weights, WeightSum, KLDivergence1, KLDivergence).

	weighted_embedding_mean(Probabilities, EmbeddingRows, Mean) :-
		EmbeddingRows = [FirstRow| _],
		new_vector_like(FirstRow, Zero),
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
		new_vector_like(Coordinates0, PreviousUpdate),
		transpose_matrix(EmbeddingRows, Columns),
		^^option(learning_rate(LearningRate), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		Configuration = new_point_config(LearningRate, MaximumIterations, Tolerance),
		iterate_new_coordinates(Probabilities, EmbeddingRows, Columns, Configuration, 0, Coordinates0, PreviousUpdate, Coordinates).

	iterate_new_coordinates(Probabilities, EmbeddingRows, Columns, Configuration, Iteration0, Coordinates0, PreviousUpdate0, Coordinates) :-
		Configuration = new_point_config(LearningRate, MaximumIterations, Tolerance),
		new_point_gradient_columns(Probabilities, EmbeddingRows, Columns, Coordinates0, Gradient),
		update_new_coordinates(Coordinates0, Gradient, PreviousUpdate0, LearningRate, 0.5, Coordinates1, PreviousUpdate, 0.0, Delta),
		Iteration is Iteration0 + 1,
		(	Delta =< Tolerance ->
			Coordinates = Coordinates1
		;	Iteration >= MaximumIterations ->
			Coordinates = Coordinates1
		;	iterate_new_coordinates(Probabilities, EmbeddingRows, Columns, Configuration, Iteration, Coordinates1, PreviousUpdate, Coordinates)
		).

	new_point_gradient_columns(Probabilities, EmbeddingRows, Columns, Coordinates, Gradient) :-
		new_point_weights(EmbeddingRows, Coordinates, Weights, 0.0, WeightSum),
		gradient_coefficients(Probabilities, Weights, WeightSum, 1.0, 2.0, Coefficients),
		gradient_coordinates(Coordinates, Columns, Coefficients, Gradient).

	new_point_weights([], _Coordinates, [], Sum, Sum).
	new_point_weights([Row| Rows], Coordinates, [Weight| Weights], Sum0, Sum) :-
		squared_distance(Coordinates, Row, 0.0, Distance),
		Weight is 1.0 / (1.0 + Distance),
		Sum1 is Sum0 + Weight,
		new_point_weights(Rows, Coordinates, Weights, Sum1, Sum).

	update_new_coordinates([], [], [], _LearningRate, _Momentum, [], [], Delta, Delta).
	update_new_coordinates([Coordinate| Coordinates], [Gradient| Gradients], [PreviousUpdate| PreviousUpdates], LearningRate, Momentum, [UpdatedCoordinate| UpdatedCoordinates], [Update| Updates], Delta0, Delta) :-
		Update is Momentum * PreviousUpdate - LearningRate * Gradient,
		UpdatedCoordinate is Coordinate + Update,
		Delta1 is max(abs(Update), Delta0),
		update_new_coordinates(Coordinates, Gradients, PreviousUpdates, LearningRate, Momentum, UpdatedCoordinates, Updates, Delta1, Delta).

	coordinate_pairs([], _Index, []).
	coordinate_pairs([Coordinate| Coordinates], Index, [Name-Coordinate| Pairs]) :-
		atomic_concat(component_, Index, Name),
		NextIndex is Index + 1,
		coordinate_pairs(Coordinates, NextIndex, Pairs).

	build_diagnostics(AttributeNames, SampleCount, ComponentCount, Options, Convergence, Iterations, FinalDelta, InitialKLDivergence, FinalKLDivergence, AffinitySearch, Diagnostics) :-
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
			AffinitySearch,
			preprocessing(Preprocessing)
		].

	valid_example_ids(ExampleIds) :-
		valid(list, ExampleIds),
		avltree::new(SeenIds),
		valid_example_ids(ExampleIds, SeenIds).

	valid_example_ids([], _SeenIds).
	valid_example_ids([Id| Ids], SeenIds) :-
		ground(Id),
		\+ avltree::lookup(Id, _, SeenIds),
		avltree::insert(SeenIds, Id, true, UpdatedIds),
		valid_example_ids(Ids, UpdatedIds).

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
		memberchk(options(Options), Diagnostics),
		forall(
			default_option(Default),
			(	functor(Default, Name, Arity),
				functor(Required, Name, Arity),
				^^option(Required, Options)
			)
		),
		^^option(n_components(ComponentCount), Options),
		ComponentCount =< FeatureCount,
		ComponentCount < SampleCount,
		^^option(early_exaggeration_iterations(EarlyIterations), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		EarlyIterations =< MaximumIterations,
		^^option(perplexity(Perplexity), Options),
		Perplexity =< SampleCount - 1,
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
		Iterations =< MaximumIterations,
		memberchk(final_delta(FinalDelta), Diagnostics),
		valid(non_negative_number, FinalDelta),
		memberchk(initial_kl_divergence(InitialKLDivergence), Diagnostics),
		valid(non_negative_number, InitialKLDivergence),
		memberchk(final_kl_divergence(FinalKLDivergence), Diagnostics),
		valid(non_negative_number, FinalKLDivergence),
		memberchk(affinity_search(summary(Matched, Tied, Limited), achieved_perplexity_range(Minimum, Maximum)), Diagnostics),
		valid(non_negative_integer, Matched),
		valid(non_negative_integer, Tied),
		valid(non_negative_integer, Limited),
		SampleCount =:= Matched + Tied + Limited,
		valid(positive_number, Minimum),
		valid(positive_number, Maximum),
		Minimum >= 1.0,
		Minimum =< Maximum,
		Maximum =< SampleCount - 1,
		memberchk(preprocessing([center(true), feature_scaling(FeatureScaling), missing_values(mean_imputation)]), Diagnostics),
		^^option(feature_scaling(FeatureScaling), Options),
		memberchk(attribute_names(AttributeNames), Diagnostics),
		valid(list(atom, FeatureCount), AttributeNames).

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
		valid(positive_float, Perplexity),
		Perplexity >= 1.0.
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

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(perplexity_mismatch(Requested, Achieved, Status), tsne_projection) -->
		['Requested perplexity ~w; achieved ~w (~w).'-[Requested, Achieved, Status], nl].

:- end_object.
