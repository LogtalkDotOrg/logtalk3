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
		date is 2026-04-24,
		comment is 'Performance and reference-fit benchmarks for the "kmeans" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		length/2, memberchk/2, nth1/3
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_centroid_distance-MinCentroidDistance))]) :-
		reference_fit(two_blobs, [k(2), initialization(spread), feature_scaling(off)], 0.5, 5.0, TrainTime, Inertia, MinCentroidDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_centroid_distance-MinCentroidDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 7.0, 2.0, TrainTime, Inertia, MinCentroidDistance).

	reference_fit(Dataset, Options, MaximumInertia, MinimumCentroidDistance, TrainTime, Inertia, MinCentroidDistance) :-
		benchmark(kmeans::learn(Dataset, _Clusterer, Options), TrainTime),
		kmeans::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, Inertia, MinCentroidDistance),
		Inertia =< MaximumInertia,
		MinCentroidDistance >= MinimumCentroidDistance.

	clusterer_metrics(Dataset, kmeans_clusterer(Encoders, Centroids, Options, _Diagnostics), Inertia, MinCentroidDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		training_inertia(Examples, kmeans_clusterer(Encoders, Centroids, Options, _Diagnostics), Inertia),
		minimum_centroid_distance(Centroids, MinCentroidDistance).

	training_inertia([], _, 0.0).
	training_inertia([AttributeValues| Examples], Clusterer, Inertia) :-
		Clusterer = kmeans_clusterer(Encoders, Centroids, _Options, _Diagnostics),
		kmeans::cluster(Clusterer, AttributeValues, Cluster),
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
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
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
