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


:- object(kcenters,
	imports(clusterer_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'k-Centers clusterer for continuous datasets. Learns from a dataset object implementing the ``clustering_dataset_protocol`` protocol and returns a clusterer term that can be used for assigning new instances to clusters and exported as predicate clauses.',
		see_also is [clusterer_protocol, clustering_dataset_protocol, kmeans, kmedoids, kmedians]
	]).

	:- uses(format, [
		format/2
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(numberlist, [
		euclidean_distance/3, manhattan_distance/3
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
		^^check_continuous_attributes(Attributes),
		keys(Attributes, AttributeNames),
		findall(
			Id-AttributeValues,
			Dataset::example(Id, AttributeValues),
			Examples
		),
		^^check_examples(Dataset, AttributeNames, Examples),
		^^build_encoders(AttributeNames, Examples, Options, Encoders),
		^^examples_to_rows(Examples, Encoders, Rows),
		length(Rows, Count),
		^^option(k(K), Options),
		^^check_cluster_count(K, Count),
		^^option(initialization(Initialization), Options),
		initialize_centers(Initialization, K, Rows, Options, Centers),
		build_diagnostics(Count, Centers, Options, Diagnostics),
		Clusterer = kcenters_clusterer(Encoders, Centers, Options, Diagnostics),
		!.

	cluster(Clusterer, Instance, Cluster) :-
		clusterer_data(Clusterer, Encoders, Centers, Options, _Diagnostics),
		^^encode_instance(Encoders, Instance, Features),
		nearest_center(Centers, Features, Options, Cluster, _Distance).

	build_diagnostics(TrainingExampleCount, Centers, Options, Diagnostics) :-
		length(Centers, CenterCount),
		selection_strategy(Options, SelectionStrategy),
		Diagnostics = [
			model(kcenters),
			center_count(CenterCount),
			training_example_count(TrainingExampleCount),
			selection_strategy(SelectionStrategy),
			options(Options)
		].

	selection_strategy(Options, SelectionStrategy) :-
		^^option(initialization(Initialization), Options),
		(   Initialization == spread ->
			SelectionStrategy = deterministic_farthest_first
		;   SelectionStrategy = first_k
		).

	clusterer_diagnostics_data(Clusterer, Diagnostics) :-
		clusterer_data(Clusterer, _Encoders, _Centers, _Options, Diagnostics).

	clusterer_data(Clusterer, Encoders, Centers, Options, Diagnostics) :-
		Clusterer =.. [_Functor, Encoders, Centers, Options, Diagnostics].

	check_clusterer(Clusterer) :-
		(   clusterer_data(Clusterer, Encoders, Centers, Options, Diagnostics),
			length(Encoders, FeatureCount),
			^^valid_continuous_encoders(Encoders),
			valid(list(list(number, FeatureCount)), Centers),
			^^valid_clusterer_metadata(kcenters, Options, Diagnostics),
			length(Centers, CenterCount),
			^^valid_diagnostic_count(center_count, Diagnostics, CenterCount),
			^^valid_diagnostic_choice(selection_strategy, Diagnostics, [deterministic_farthest_first, first_k]) ->
			true
		;   domain_error(clusterer, Clusterer)
		).

	initialize_centers(first_k, K, Rows, _Options, Centers) :-
		^^take_first_k(K, Rows, Centers).
	initialize_centers(spread, K, [_-First| Rows], Options, [First| Centers]) :-
		Remaining is K - 1,
		select_spread_centers(Remaining, Rows, [First], Options, Centers).

	select_spread_centers(0, _, _, _, []) :-
		!.
	select_spread_centers(Count, Candidates, Selected, Options, [Vector| Centers]) :-
		Count > 0,
		farthest_candidate(Candidates, Selected, Options, BestCandidate, RemainingCandidates),
		BestCandidate = _-Vector,
		NextCount is Count - 1,
		select_spread_centers(NextCount, RemainingCandidates, [Vector| Selected], Options, Centers).

	farthest_candidate([Candidate| Candidates], Selected, Options, BestCandidate, RemainingCandidates) :-
		Candidate = _-Vector,
		closest_center_distance(Vector, Selected, Options, Distance),
		farthest_candidate(Candidates, Selected, Options, Candidate, Distance, BestCandidate),
		^^remove_candidate(BestCandidate, [Candidate| Candidates], RemainingCandidates).

	farthest_candidate([], _, _Options, BestCandidate, _BestDistance, BestCandidate).
	farthest_candidate([Candidate| Candidates], Selected, Options, BestCandidate0, BestDistance0, BestCandidate) :-
		Candidate = _-Vector,
		closest_center_distance(Vector, Selected, Options, Distance),
		(   Distance > BestDistance0 ->
			BestCandidate1 = Candidate,
			BestDistance1 = Distance
		;   BestCandidate1 = BestCandidate0,
			BestDistance1 = BestDistance0
		),
		farthest_candidate(Candidates, Selected, Options, BestCandidate1, BestDistance1, BestCandidate).

	nearest_center([Center| Centers], Features, Options, Cluster, Distance) :-
		distance(Options, Features, Center, InitialDistance),
		nearest_center(Centers, Features, Options, 2, 1, InitialDistance, Cluster, Distance).

	nearest_center([], _Features, _Options, _Index, BestCluster, BestDistance, BestCluster, BestDistance).
	nearest_center([Center| Centers], Features, Options, Index, BestCluster0, BestDistance0, BestCluster, BestDistance) :-
		distance(Options, Features, Center, Distance0),
		(   Distance0 < BestDistance0 ->
			BestCluster1 = Index,
			BestDistance1 = Distance0
		;   BestCluster1 = BestCluster0,
			BestDistance1 = BestDistance0
		),
		NextIndex is Index + 1,
		nearest_center(Centers, Features, Options, NextIndex, BestCluster1, BestDistance1, BestCluster, BestDistance).

	closest_center_distance(Vector, [Center| Centers], Options, Distance) :-
		distance(Options, Vector, Center, InitialDistance),
		closest_center_distance(Centers, Vector, Options, InitialDistance, Distance).

	closest_center_distance([], _Vector, _Options, BestDistance, BestDistance).
	closest_center_distance([Center| Centers], Vector, Options, BestDistance0, BestDistance) :-
		distance(Options, Vector, Center, Distance0),
		(   Distance0 < BestDistance0 ->
			BestDistance1 = Distance0
		;   BestDistance1 = BestDistance0
		),
		closest_center_distance(Centers, Vector, Options, BestDistance1, BestDistance).

	distance(Options, Vector1, Vector2, Distance) :-
		^^option(distance_metric(Metric), Options),
		distance_metric(Metric, Vector1, Vector2, Distance).

	distance_metric(euclidean, Vector1, Vector2, Distance) :-
		euclidean_distance(Vector1, Vector2, Distance).
	distance_metric(manhattan, Vector1, Vector2, Distance) :-
		manhattan_distance(Vector1, Vector2, Distance).

	print_clusterer(Clusterer) :-
		clusterer_data(Clusterer, Encoders, Centers, Options, Diagnostics),
		format('k-Centers Clusterer~n', []),
		format('===================~n~n', []),
		format('Options: ~w~n~n', [Options]),
		format('Diagnostics: ~w~n~n', [Diagnostics]),
		format('Encoders:~n', []),
		print_encoders(Encoders),
		format('~nCenters:~n', []),
		print_centers(Centers, 1).

	print_encoders([]).
	print_encoders([continuous(Attribute, Mean, Scale)| Encoders]) :-
		format('  ~w (continuous, mean=~4f, scale=~4f)~n', [Attribute, Mean, Scale]),
		print_encoders(Encoders).

	print_centers([], _).
	print_centers([Center| Centers], Cluster) :-
		format('  cluster ~d: ~w~n', [Cluster, Center]),
		NextCluster is Cluster + 1,
		print_centers(Centers, NextCluster).

	default_option(k(2)).
	default_option(initialization(spread)).
	default_option(distance_metric(euclidean)).
	default_option(feature_scaling(on)).

	valid_option(k(K)) :-
		valid(positive_integer, K).
	valid_option(initialization(Initialization)) :-
		once((Initialization == first_k; Initialization == spread)).
	valid_option(distance_metric(Metric)) :-
		once((Metric == euclidean; Metric == manhattan)).
	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).

:- end_object.
