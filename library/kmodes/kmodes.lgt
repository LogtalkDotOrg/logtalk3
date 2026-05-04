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


:- object(kmodes,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'k-Modes clusterer for discrete datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		see_also is [clusterer_protocol, clustering_dataset_protocol, kprototypes]
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

	:- uses(type, [
		valid/2
	]).

	learn(Dataset, Clusterer, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^dataset_attributes(Dataset, Attributes),
		check_discrete_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-AttributeValues,
			Dataset::example(Id, AttributeValues),
			Examples
		),
		check_examples(Dataset, Attributes, AttributeNames, Examples),
		build_encoders(Attributes, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		length(Rows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		^^option(initialization(Initialization), Options),
		initialize_modes(Initialization, K, Rows, Encoders, InitialModes),
		optimize_modes(Rows, Encoders, Options, 0, 0.0, InitialModes, Modes, Convergence, Iterations, FinalShift),
		build_diagnostics(Count, Modes, Options, Convergence, Iterations, FinalShift, Diagnostics),
		Clusterer = kmodes_clusterer(Encoders, Modes, Options, Diagnostics),
		!.

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Modes, Options, _Diagnostics),
		encode_instance(Encoders, Instance, Features),
		nearest_mode(Modes, Encoders, Features, Options, Cluster, _Distance).

	clusterer_data(Clusterer, Encoders, Modes, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Modes, Options, Diagnostics].

	build_diagnostics(TrainingExampleCount, Modes, Options, Convergence, Iterations, FinalShift, Diagnostics) :-
		length(Modes, ModeCount),
		Diagnostics = [
			model(kmodes),
			mode_count(ModeCount),
			training_example_count(TrainingExampleCount),
			convergence(Convergence),
			iterations(Iterations),
			final_shift(FinalShift),
			options(Options)
		].

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Modes, _Options, Diagnostics).

	check_clusterer(Clusterer) :-
		(   clusterer_data(Clusterer, Encoders, Modes, Options, Diagnostics),
			^^valid_discrete_encoders(Encoders),
			^^valid_mixed_vectors(Encoders, Modes),
			^^valid_clusterer_metadata(kmodes, Options, Diagnostics),
			length(Modes, ModeCount),
			^^valid_diagnostic_count(mode_count, Diagnostics, ModeCount) ->
			true
		;   domain_error(clusterer, Clusterer)
		).

	check_discrete_attributes([]).
	check_discrete_attributes([Attribute-Values| Attributes]) :-
		(   Values == continuous ->
			domain_error(discrete_attribute(Attribute), continuous)
		;   true
		),
		check_discrete_attributes(Attributes).

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
		attribute_spec(Attribute, Attributes, AllowedValues),
		^^attribute_value(Attribute, AttributeValues, Value),
		check_attribute_value(Attribute, AllowedValues, Value),
		check_example_attributes_checked(AttributeNames, Attributes, AttributeValues).

	attribute_spec(Attribute, Attributes, Spec) :-
		(   member(Attribute-Spec, Attributes) ->
			true
		;   existence_error(attribute, Attribute)
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

	build_encoders([], []).
	build_encoders([Attribute-Values| Attributes], [discrete(Attribute, Values)| Encoders]) :-
		build_encoders(Attributes, Encoders).

	encode_instance(Encoders, AttributeValues, Features) :-
		^^check_encoded_attribute_bindings(Encoders, AttributeValues),
		encode_instance_checked(Encoders, AttributeValues, Features).

	encode_instance_checked([], _, []).
	encode_instance_checked([discrete(Attribute, AllowedValues)| Encoders], AttributeValues, [Value| Features]) :-
		^^attribute_value(Attribute, AttributeValues, Value),
		check_attribute_value(Attribute, AllowedValues, Value),
		encode_instance_checked(Encoders, AttributeValues, Features).

	initialize_modes(first_k, K, Rows, _Encoders, Modes) :-
		^^take_first_k(K, Rows, Modes).
	initialize_modes(spread, K, [_-First| Rows], Encoders, [First| Modes]) :-
		Remaining is K - 1,
		select_spread_modes(Remaining, Rows, [First], Encoders, Modes).

	select_spread_modes(0, _, _, _, []) :-
		!.
	select_spread_modes(Count, Candidates, Selected, Encoders, [Vector| Modes]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, Encoders, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_modes(NextCount, RemainingCandidates, [Vector| Selected], Encoders, Modes).

	farthest_candidate([Candidate| Candidates], Selected, Encoders, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_mode_distance(Vector, Selected, Encoders, Distance),
		farthest_candidate(Candidates, Selected, Encoders, Candidate, Distance, BestCandidate),
		^^remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _, _, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, Encoders, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_mode_distance(Vector, Selected, Encoders, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, Encoders, BestCandidate1, BestDistance1, BestCandidate).

	optimize_modes(Rows, Encoders, Options, Iteration, PreviousShift, Modes0, Modes, Convergence, Iterations, FinalShift) :-
		^^option(maximum_iterations(MaximumIterations), Options),
		(   Iteration >= MaximumIterations ->
			Modes = Modes0,
			Convergence = maximum_iterations,
			Iterations = Iteration,
			FinalShift = PreviousShift
		;   assign_rows(Rows, Modes0, Encoders, Options, Assignments),
			recompute_modes(Modes0, Assignments, Encoders, 1, Modes1),
			max_mode_shift(Modes0, Modes1, Encoders, 0.0, Shift),
			^^option(tolerance(Tolerance), Options),
			NextIteration is Iteration + 1,
			(   Shift =< Tolerance ->
				Modes = Modes1,
				Convergence = tolerance,
				Iterations = NextIteration,
				FinalShift = Shift
			;   optimize_modes(Rows, Encoders, Options, NextIteration, Shift, Modes1, Modes, Convergence, Iterations, FinalShift)
			)
		).

	assign_rows([], _, _, _, []).
	assign_rows([_-Vector| Rows], Modes, Encoders, Options, [Cluster-Vector| Assignments]) :-
		nearest_mode(Modes, Encoders, Vector, Options, Cluster, _Distance),
		assign_rows(Rows, Modes, Encoders, Options, Assignments).

	nearest_mode([Mode| Modes], Encoders, Vector, _Options, Cluster, Distance) :-
		mismatch_distance(Encoders, Vector, Mode, 0.0, InitialDistance),
		nearest_mode(Modes, Encoders, Vector, 2, 1, InitialDistance, Cluster, Distance).

	nearest_mode([], _Encoders, _Vector, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_mode([Mode| Modes], Encoders, Vector, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		mismatch_distance(Encoders, Vector, Mode, 0.0, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_mode(Modes, Encoders, Vector, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	closest_mode_distance(Vector, [Mode| Modes], Encoders, Distance) :-
		mismatch_distance(Encoders, Vector, Mode, 0.0, InitialDistance),
		closest_mode_distance(Modes, Encoders, Vector, InitialDistance, Distance).

	closest_mode_distance([], _Encoders, _Vector, BestDistance, BestDistance).
	closest_mode_distance([Mode| Modes], Encoders, Vector, BestDistance0, BestDistance) :-
		mismatch_distance(Encoders, Vector, Mode, 0.0, Distance0),
		(   Distance0 < BestDistance0 ->
			BestDistance1 = Distance0
		;   BestDistance1 = BestDistance0
		),
		closest_mode_distance(Modes, Encoders, Vector, BestDistance1, BestDistance).

	mismatch_distance([], [], [], Distance, Distance).
	mismatch_distance([discrete(_, _)| Encoders], [Feature| Features], [ModeFeature| ModeFeatures], Distance0, Distance) :-
		(   Feature == ModeFeature ->
			Distance1 is Distance0
		;   Distance1 is Distance0 + 1.0
		),
		mismatch_distance(Encoders, Features, ModeFeatures, Distance1, Distance).

	recompute_modes([], _, _, _, []).
	recompute_modes([Mode0| Modes0], Assignments, Encoders, Cluster, [Mode| Modes]) :-
		assigned_vectors(Cluster, Assignments, Vectors),
		(   Vectors == [] ->
			Mode = Mode0
		;   mode_from_vectors(Encoders, Vectors, Mode)
		),
		NextCluster is Cluster + 1,
		recompute_modes(Modes0, Assignments, Encoders, NextCluster, Modes).

	assigned_vectors(_, [], []) :-
		!.
	assigned_vectors(Cluster, [Cluster-Vector| Assignments], [Vector| Vectors]) :-
		!,
		assigned_vectors(Cluster, Assignments, Vectors).
	assigned_vectors(Cluster, [_OtherCluster-_| Assignments], Vectors) :-
		assigned_vectors(Cluster, Assignments, Vectors).

	mode_from_vectors(Encoders, Vectors, Mode) :-
		transpose(Vectors, Columns),
		mode_from_columns(Encoders, Columns, Mode).

	transpose([[]| _], []) :-
		!.
	transpose(Vectors, [Column| Columns]) :-
		first_column(Vectors, Column, RemainingVectors),
		transpose(RemainingVectors, Columns).

	first_column([], [], []).
	first_column([[Value| Values]| Vectors], [Value| Column], [Values| RemainingVectors]) :-
		first_column(Vectors, Column, RemainingVectors).

	mode_from_columns([], [], []).
	mode_from_columns([discrete(_, AllowedValues)| Encoders], [Column| Columns], [Feature| Features]) :-
		categorical_mode(AllowedValues, Column, Feature),
		mode_from_columns(Encoders, Columns, Features).

	categorical_mode([Value| Values], Column, Mode) :-
		count_occurrences(Value, Column, Count),
		categorical_mode(Values, Column, Value, Count, Mode).

	categorical_mode([], _Column, BestValue, _BestCount, BestValue).
	categorical_mode([Value| Values], Column, BestValue0, BestCount0, BestValue) :-
		count_occurrences(Value, Column, Count),
		(   Count > BestCount0 ->
			BestValue1 = Value,
			BestCount1 = Count
		;   BestValue1 = BestValue0,
			BestCount1 = BestCount0
		),
		categorical_mode(Values, Column, BestValue1, BestCount1, BestValue).

	count_occurrences(_Value, [], 0) :-
		!.
	count_occurrences(Value, [Value| Values], Count) :-
		!,
		count_occurrences(Value, Values, RestCount),
		Count is RestCount + 1.
	count_occurrences(Value, [_Other| Values], Count) :-
		count_occurrences(Value, Values, Count).

	max_mode_shift([], [], _Encoders, MaxShift, MaxShift).
	max_mode_shift([Mode0| Modes0], [Mode1| Modes1], Encoders, MaxShift0, MaxShift) :-
		mismatch_distance(Encoders, Mode0, Mode1, 0.0, Shift),
		MaxShift1 is max(MaxShift0, Shift),
		max_mode_shift(Modes0, Modes1, Encoders, MaxShift1, MaxShift).

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, Modes, Options, Diagnostics),
		format('k-Modes Clusterer~n', []),
		format('=================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nModes:~n', []),
		print_modes(Modes, 1).

	print_encoders([]).
	print_encoders([discrete(Attribute, Values)| Encoders]) :-
		format('  ~w (discrete, values=~w)~n', [Attribute, Values]),
		print_encoders(Encoders).

	print_modes([], _).
	print_modes([Mode| Modes], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Mode]),
		NextCluster is Cluster + 1,
		print_modes(Modes, NextCluster).

	default_option(k(2)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(0.0)).
	default_option(initialization(spread)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(maximum_iterations(MaximumIterations)) :-
		valid(positive_integer, MaximumIterations).
	valid_option(tolerance(Tolerance)) :-
		number(Tolerance),
		Tolerance >= 0.0.
	valid_option(initialization(Initialization)) :-
		once((Initialization == first_k; Initialization == spread)).

:- end_object.
