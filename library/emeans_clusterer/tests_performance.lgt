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


:- object(tests_performance,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-19,
		comment is 'Performance and reference-fit benchmarks for the "emeans_clusterer" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2, benchmark_reified/3
	]).

	:- uses(list, [
		length/2, memberchk/2, nth1/3
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_centroid_distance-MinCentroidDistance))]) :-
		reference_fit(two_blobs, [k(2), initialization(spread), feature_scaling(off), epochs(5), seed(42)], 0.5, 5.0, TrainTime, Inertia, MinCentroidDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_centroid_distance-MinCentroidDistance))]) :-
		reference_fit(iris_unlabeled, [algorithm(ema_lloyd), k(3), epochs(5), seed(42)], 7.0, 2.0, TrainTime, Inertia, MinCentroidDistance).

	test(emeans_scaled_fixture, true, [note(metrics(small_seconds-SmallTime, large_seconds-LargeTime, large_one_update_seconds-OneUpdateTime))]) :-
		training_fixture(1025, 8, 5, SmallTime),
		training_fixture(4097, 32, 5, LargeTime),
		training_fixture(4097, 32, 1, OneUpdateTime).

	test(emeans_refinement_budget, true, [note(metrics(one_update_seconds-OneTime, default_seconds-DefaultTime))]) :-
		refinement_rows(1024, Rows),
		Initial = [[0.0],[2.0]],
		benchmark_reified((emeans_clusterer(fast_random(xoshiro128pp))<<refine(Rows, Initial, 1, _, refinement(iteration_limit,1,2,_))), OneTime, success),
		benchmark_reified((emeans_clusterer(fast_random(xoshiro128pp))<<refine(Rows, Initial, 5, _, refinement(stable,2,2,_))), DefaultTime, success).

	% auxiliary predicates

	refinement_rows(0, []) :-
		!.
	refinement_rows(I, [I-[0.0],I-[1.0],I-[1.0],I-[2.0],I-[10.0]| Rows]) :-
		Next is I - 1,
		refinement_rows(Next, Rows).

	training_fixture(N, K, Limit, Time) :-
		fixture_rows(N, K, Rows),
		Options = [epochs(2), tolerance(0.0)],
		benchmark_reified(fit_rows(Rows, Options, N, K, Limit), Time, success).

	fit_rows(Rows, Options, N, K, Limit) :-
		emeans_clusterer(fast_random(xoshiro128pp))<<initialize_seed(42),
		emeans_clusterer(fast_random(xoshiro128pp))<<initialize_centroids(spread, K, Rows, Initial),
		emeans_clusterer(fast_random(xoshiro128pp))<<init_state(Initial, State),
		emeans_clusterer(fast_random(xoshiro128pp))<<optimize_ema_lloyd(Rows, Options, N, K, Initial, State, Trained, _, _, _),
		emeans_clusterer(fast_random(xoshiro128pp))<<refine(Rows, Trained, Limit, _, refinement(_, _, K, _)).

	fixture_rows(0, _, []) :-
		!.
	fixture_rows(I, K, [I-[X,Y]| Rows]) :-
		X is 10.0 * (I mod K) + 0.01 * (I mod 19),
		Y is 0.01 * (I mod 23),
		Next is I - 1,
		fixture_rows(Next, K, Rows).

	reference_fit(Dataset, Options, MaximumInertia, MinimumCentroidDistance, TrainTime, Inertia, MinCentroidDistance) :-
		benchmark(emeans_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		emeans_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, Inertia, MinCentroidDistance),
		Inertia =< MaximumInertia,
		MinCentroidDistance >= MinimumCentroidDistance.

	clusterer_metrics(Dataset, emeans_clusterer(Encoders, Centroids, Options, _Diagnostics), Inertia, MinCentroidDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		training_inertia(Examples, emeans_clusterer(Encoders, Centroids, Options, _Diagnostics), Inertia),
		minimum_centroid_distance(Centroids, MinCentroidDistance).

	training_inertia([], _, 0.0).
	training_inertia([AttributeValues| Examples], Clusterer, Inertia) :-
		Clusterer = emeans_clusterer(Encoders, Centroids, _Options, _Diagnostics),
		emeans_clusterer::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Centroids, Centroid),
		squared_distance(Features, Centroid, ExampleInertia),
		training_inertia(Examples, Clusterer, RestInertia),
		Inertia is ExampleInertia + RestInertia.

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		Feature is (Value - Mean) / Scale,
		encode_instance(Encoders, AttributeValues, Features).

	squared_distance([], [], 0.0).
	squared_distance([Value| Values], [Centroid| Centroids], DistanceSquared) :-
		squared_distance(Values, Centroids, RestDistanceSquared),
		Delta is Value - Centroid,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

	minimum_centroid_distance([], 0.0).
	minimum_centroid_distance([_], 0.0).
	minimum_centroid_distance([Centroid| Centroids], MinimumDistance) :-
		minimum_distance_to_others(Centroid, Centroids, FirstMinimumDistance),
		minimum_centroid_distance(Centroids, RestMinimumDistance),
		(	RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;	MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Centroid, [Other| Others], MinimumDistance) :-
		squared_distance(Centroid, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_others(Centroid, Others, Distance, MinimumDistance).

	minimum_distance_to_others(_Centroid, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Centroid, [Other| Others], MinimumDistance0, MinimumDistance) :-
		squared_distance(Centroid, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Centroid, Others, MinimumDistance1, MinimumDistance).

:- end_object.
