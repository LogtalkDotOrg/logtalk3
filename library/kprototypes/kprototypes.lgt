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


:- object(kprototypes,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-24,
		comment is 'k-Prototypes clusterer for mixed datasets with continuous and discrete attributes. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		remarks is [
			'Algorithm' - 'Uses an iterative prototype-update algorithm with deterministic initialization and deterministic cluster assignments.',
			'Feature handling' - 'Supports continuous and discrete attributes in the same dataset. Continuous attributes can be standardized using z-score scaling before clustering.',
			'Categorical weighting' - 'Uses the ``gamma`` option to weight discrete mismatches in the mixed distance function.',
			'Initialization' - 'Supports ``first_k`` initialization and a deterministic ``spread`` initialization that repeatedly chooses the farthest example from the prototypes selected so far.',
			'Empty clusters' - 'If an iteration leaves a cluster empty, its prototype is kept unchanged from the previous iteration.',
			'Clusterer representation' - 'The learned clusterer is represented by default as ``kprototypes_clusterer(Encoders, Prototypes, Options, Diagnostics)`` where ``Encoders`` stores the feature encoding metadata, ``Prototypes`` stores the learned mixed prototypes in cluster-id order, ``Options`` stores the effective training options, and ``Diagnostics`` stores convergence metadata.'
		],
		see_also is [clusterer_protocol, clustering_dataset_protocol, kmeans, kmedoids]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2, member/2
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

	learn(Dataset, Clusterer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-AttributeValues,
			Dataset::example(Id, AttributeValues),
			Examples
		),
		check_examples(Dataset, Attributes, AttributeNames, Examples),
		build_encoders(Attributes, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		length(Rows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		^^option(initialization(Initialization), Options),
		initialize_prototypes(Initialization, K, Rows, Encoders, Options, InitialPrototypes),
		optimize_prototypes(Rows, Encoders, Options, 0, 0.0, InitialPrototypes, Prototypes, Convergence, Iterations, FinalShift),
		build_diagnostics(Count, Prototypes, Options, Convergence, Iterations, FinalShift, Diagnostics),
		Clusterer = kprototypes_clusterer(Encoders, Prototypes, Options, Diagnostics).

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Prototypes, Options, _Diagnostics),
		encode_instance(Encoders, Instance, Features),
		nearest_prototype(Prototypes, Encoders, Features, Options, Cluster, _Distance).

	build_diagnostics(TrainingExampleCount, Prototypes, Options, Convergence, Iterations, FinalShift, Diagnostics) :-
		length(Prototypes, PrototypeCount),
		Diagnostics = [
			model(kprototypes),
			prototype_count(PrototypeCount),
			training_example_count(TrainingExampleCount),
			convergence(Convergence),
			iterations(Iterations),
			final_shift(FinalShift),
			options(Options)
		].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Prototypes, _Options, Diagnostics).

	clusterer_data(Clusterer, Encoders, Prototypes, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Prototypes, Options, Diagnostics].

	check_examples(Dataset, Attributes, AttributeNames, Examples) :-
		^^check_examples_non_empty(Dataset, Examples),
		check_example_values(Examples, Attributes, AttributeNames).

	check_example_values([], _, _).
	check_example_values([_-AttributeValues| Examples], Attributes, AttributeNames) :-
		check_example_attributes(AttributeNames, Attributes, AttributeValues),
		check_example_values(Examples, Attributes, AttributeNames).

	check_example_attributes(AttributeNames, Attributes, AttributeValues) :-
		^^check_attribute_bindings(AttributeNames, AttributeValues),
		check_example_attributes_checked(AttributeNames, Attributes, AttributeValues).

	check_example_attributes_checked([], _, _).
	check_example_attributes_checked([Attribute| AttributeNames], Attributes, AttributeValues) :-
		attribute_spec(Attribute, Attributes, Spec),
		^^attribute_value(Attribute, AttributeValues, Value),
		check_attribute_value(Attribute, Spec, Value),
		check_example_attributes_checked(AttributeNames, Attributes, AttributeValues).

	attribute_spec(Attribute, Attributes, Spec) :-
		(   member(Attribute-Spec, Attributes) ->
			true
		;   existence_error(attribute, Attribute)
		).

	check_attribute_value(_Attribute, continuous, Value) :-
		!,
		(   nonvar(Value) ->
			true
		;   instantiation_error
		),
		(   number(Value) ->
			true
		;   type_error(number, Value)
		).
	check_attribute_value(Attribute, AllowedValues, Value) :-
		(   nonvar(Value) ->
			true
		;   instantiation_error
		),
		(   member(Value, AllowedValues) ->
			true
		;   domain_error(attribute_value(Attribute, AllowedValues), Value)
		).

	build_encoders([], _, _, []).
	build_encoders([Attribute-continuous| Attributes], Examples, Options, [continuous(Attribute, Mean, Scale)| Encoders]) :-
		!,
		continuous_stats(Attribute, Examples, Options, Mean, Scale),
		build_encoders(Attributes, Examples, Options, Encoders).
	build_encoders([Attribute-Values| Attributes], Examples, Options, [discrete(Attribute, Values)| Encoders]) :-
		build_encoders(Attributes, Examples, Options, Encoders).

	continuous_stats(Attribute, Examples, Options, Mean, Scale) :-
		^^option(feature_scaling(FeatureScaling), Options),
		(   FeatureScaling == on ->
			^^known_attribute_values(Examples, Attribute, Values),
			arithmetic_mean(Values, Mean),
			length(Values, Count),
			(   Count > 1 ->
				variance(Values, Variance)
			;   Variance = 0.0
			),
			(   Variance > 0.0 ->
				Scale is sqrt(Variance)
			;   Scale = 1.0
			)
		;   Mean = 0.0,
			Scale = 1.0
		).

	encode_instance(Encoders, AttributeValues, Features) :-
		^^check_encoded_attribute_bindings(Encoders, AttributeValues),
		encode_instance_checked(Encoders, AttributeValues, Features).

	encode_instance_checked([], _, []).
	encode_instance_checked([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		!,
		^^attribute_value(Attribute, AttributeValues, Value),
		^^normalize_continuous(Value, Mean, Scale, Feature),
		encode_instance_checked(Encoders, AttributeValues, Features).
	encode_instance_checked([discrete(Attribute, AllowedValues)| Encoders], AttributeValues, [Value| Features]) :-
		^^attribute_value(Attribute, AttributeValues, Value),
		check_attribute_value(Attribute, AllowedValues, Value),
		encode_instance_checked(Encoders, AttributeValues, Features).

	initialize_prototypes(first_k, K, Rows, _Encoders, _Options, Prototypes) :-
		^^take_first_k(K, Rows, Prototypes).
	initialize_prototypes(spread, K, [_-First| Rows], Encoders, Options, [First| Prototypes]) :-
		Remaining is K - 1,
		select_spread_prototypes(Remaining, Rows, [First], Encoders, Options, Prototypes).

	select_spread_prototypes(0, _, _, _, _, []) :-
		!.
	select_spread_prototypes(Count, Candidates, Selected, Encoders, Options, [Vector| Prototypes]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, Encoders, Options, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_prototypes(NextCount, RemainingCandidates, [Vector| Selected], Encoders, Options, Prototypes).

	farthest_candidate([Candidate| Candidates], Selected, Encoders, Options, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_prototype_distance(Vector, Selected, Encoders, Options, Distance),
		farthest_candidate(Candidates, Selected, Encoders, Options, Candidate, Distance, BestCandidate),
		^^remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _, _, _Options, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, Encoders, Options, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_prototype_distance(Vector, Selected, Encoders, Options, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, Encoders, Options, BestCandidate1, BestDistance1, BestCandidate).

	optimize_prototypes(Rows, Encoders, Options, Iteration, PreviousShift, Prototypes0, Prototypes, Convergence, Iterations, FinalShift) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Iteration >= MaximumIterations ->
			Prototypes = Prototypes0,
			Convergence = maximum_iterations,
			Iterations = Iteration,
			FinalShift = PreviousShift
		;   assign_rows(Rows, Prototypes0, Encoders, Options, Assignments),
			recompute_prototypes(Prototypes0, Assignments, Encoders, 1, Prototypes1),
			max_prototype_shift(Prototypes0, Prototypes1, Encoders, Options, Shift),
			^^option(tolerance(Tolerance), Options),
			NextIteration is Iteration + 1,
			(   Shift =< Tolerance ->
				Prototypes = Prototypes1,
				Convergence = tolerance,
				Iterations = NextIteration,
				FinalShift = Shift
			;   optimize_prototypes(Rows, Encoders, Options, NextIteration, Shift, Prototypes1, Prototypes, Convergence, Iterations, FinalShift)
			)
		).

	assign_rows([], _, _, _, []).
	assign_rows([_-Vector| Rows], Prototypes, Encoders, Options, [Cluster-Vector| Assignments]) :-
		nearest_prototype(Prototypes, Encoders, Vector, Options, Cluster, _Distance),
		assign_rows(Rows, Prototypes, Encoders, Options, Assignments).

	nearest_prototype([Prototype| Prototypes], Encoders, Vector, Options, Cluster, Distance) :-
		mixed_distance(Encoders, Vector, Prototype, Options, InitialDistance),
		nearest_prototype(Prototypes, Encoders, Vector, Options, 2, 1, InitialDistance, Cluster, Distance).

	nearest_prototype([], _Encoders, _Vector, _Options, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_prototype([Prototype| Prototypes], Encoders, Vector, Options, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		mixed_distance(Encoders, Vector, Prototype, Options, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_prototype(Prototypes, Encoders, Vector, Options, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	closest_prototype_distance(Vector, [Prototype| Prototypes], Encoders, Options, Distance) :-
		mixed_distance(Encoders, Vector, Prototype, Options, InitialDistance),
		closest_prototype_distance(Prototypes, Encoders, Vector, Options, InitialDistance, Distance).

	closest_prototype_distance([], _Encoders, _Vector, _Options, BestDistance, BestDistance).
	closest_prototype_distance([Prototype| Prototypes], Encoders, Vector, Options, BestDistance0, BestDistance) :-
		mixed_distance(Encoders, Vector, Prototype, Options, Distance0),
		(   Distance0 < BestDistance0 ->
			BestDistance1 = Distance0
		;   BestDistance1 = BestDistance0
		),
		closest_prototype_distance(Prototypes, Encoders, Vector, Options, BestDistance1, BestDistance).

	mixed_distance([], [], [], _Options, 0.0).
	mixed_distance([continuous(_, _, _)| Encoders], [Feature| Features], [PrototypeFeature| PrototypeFeatures], Options, Distance) :-
		!,
		mixed_distance(Encoders, Features, PrototypeFeatures, Options, RestDistance),
		Delta is Feature - PrototypeFeature,
		Distance is RestDistance + Delta * Delta.
	mixed_distance([discrete(_, _)| Encoders], [Feature| Features], [PrototypeFeature| PrototypeFeatures], Options, Distance) :-
		mixed_distance(Encoders, Features, PrototypeFeatures, Options, RestDistance),
		^^option(gamma(Gamma), Options),
		(   Feature == PrototypeFeature ->
			Distance = RestDistance
		;   Distance is RestDistance + Gamma
		).

	recompute_prototypes([], _, _, _, []).
	recompute_prototypes([Prototype0| Prototypes0], Assignments, Encoders, Cluster, [Prototype| Prototypes]) :-
		assigned_vectors(Assignments, Cluster, Vectors),
		(   Vectors == [] ->
			Prototype = Prototype0
		;   prototype_from_vectors(Encoders, Vectors, Prototype)
		),
		NextCluster is Cluster + 1,
		recompute_prototypes(Prototypes0, Assignments, Encoders, NextCluster, Prototypes).

	assigned_vectors([], _, []).
	assigned_vectors([Cluster-Vector| Assignments], Cluster, [Vector| Vectors]) :-
		!,
		assigned_vectors(Assignments, Cluster, Vectors).
	assigned_vectors([_OtherCluster-_| Assignments], Cluster, Vectors) :-
		assigned_vectors(Assignments, Cluster, Vectors).

	prototype_from_vectors(Encoders, Vectors, Prototype) :-
		transpose(Vectors, Columns),
		prototype_from_columns(Encoders, Columns, Prototype).

	transpose([[]| _], []) :-
		!.
	transpose(Vectors, [Column| Columns]) :-
		first_column(Vectors, Column, RemainingVectors),
		transpose(RemainingVectors, Columns).

	first_column([], [], []).
	first_column([[Value| Values]| Vectors], [Value| Column], [Values| RemainingVectors]) :-
		first_column(Vectors, Column, RemainingVectors).

	prototype_from_columns([], [], []).
	prototype_from_columns([continuous(_, _, _)| Encoders], [Column| Columns], [Feature| Features]) :-
		!,
		arithmetic_mean(Column, Feature),
		prototype_from_columns(Encoders, Columns, Features).
	prototype_from_columns([discrete(_, AllowedValues)| Encoders], [Column| Columns], [Feature| Features]) :-
		categorical_mode(AllowedValues, Column, Feature),
		prototype_from_columns(Encoders, Columns, Features).

	categorical_mode([Value| Values], Column, Mode) :-
		count_occurrences(Column, Value, 0, Count),
		categorical_mode(Values, Column, Value, Count, Mode).

	categorical_mode([], _Column, BestValue, _BestCount, BestValue).
	categorical_mode([Value| Values], Column, BestValue0, BestCount0, BestValue) :-
		count_occurrences(Column, Value, 0, Count),
		(   Count > BestCount0 ->
			BestValue1 = Value,
			BestCount1 = Count
		;   BestValue1 = BestValue0,
			BestCount1 = BestCount0
		),
		categorical_mode(Values, Column, BestValue1, BestCount1, BestValue).

	count_occurrences([], _Value, Count, Count).
	count_occurrences([Value| Values], Value, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		count_occurrences(Values, Value, Count1, Count).
	count_occurrences([_Other| Values], Value, Count0, Count) :-
		count_occurrences(Values, Value, Count0, Count).

	max_prototype_shift([], [], _Encoders, _Options, 0.0).
	max_prototype_shift([Prototype0| Prototypes0], [Prototype1| Prototypes1], Encoders, Options, MaxShift) :-
		mixed_distance(Encoders, Prototype0, Prototype1, Options, Shift),
		max_prototype_shift(Prototypes0, Prototypes1, Encoders, Options, RestMaxShift),
		MaxShift is max(Shift, RestMaxShift).

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, Prototypes, Options, Diagnostics),
		format('k-Prototypes Clusterer~n', []),
		format('======================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nPrototypes:~n', []),
		print_prototypes(Prototypes, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		!,
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).
	print_encoders([discrete(Attribute, Values)| Encoders]) :-
		format('  ~w (discrete, values=~w)~n', [Attribute, Values]),
		print_encoders(Encoders).

	print_prototypes([], _).
	print_prototypes([Prototype| Prototypes], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Prototype]),
		NextCluster is Cluster + 1,
		print_prototypes(Prototypes, NextCluster).

	default_option(k(2)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-6)).
	default_option(initialization(spread)).
	default_option(gamma(1.0)).
	default_option(feature_scaling(on)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(initialization(Initialization)) :-
		once((Initialization == first_k; Initialization == spread)).
	valid_option(gamma(Gamma)) :-
		number(Gamma),
		Gamma >= 0.0.
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
