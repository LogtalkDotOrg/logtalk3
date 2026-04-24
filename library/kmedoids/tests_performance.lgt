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
		comment is 'Performance and reference-fit benchmarks for the "kmedoids" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		memberchk/2, nth1/3
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_medoid_distance-MinMedoidDistance))]) :-
		reference_fit(two_blobs, [k(2), initialization(spread), feature_scaling(off)], 0.5, 5.0, TrainTime, Inertia, MinMedoidDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_medoid_distance-MinMedoidDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 8.5, 2.2, TrainTime, Inertia, MinMedoidDistance).

	reference_fit(Dataset, Options, MaximumInertia, MinimumMedoidDistance, TrainTime, Inertia, MinMedoidDistance) :-
		benchmark(kmedoids::learn(Dataset, _Clusterer, Options), TrainTime),
		kmedoids::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, Inertia, MinMedoidDistance),
		Inertia =< MaximumInertia,
		MinMedoidDistance >= MinimumMedoidDistance.

	clusterer_metrics(Dataset, kmedoids_clusterer(Encoders, Medoids, Options, _Diagnostics), Inertia, MinMedoidDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		training_inertia(Examples, kmedoids_clusterer(Encoders, Medoids, Options, _Diagnostics), Inertia),
		minimum_medoid_distance(Medoids, MinMedoidDistance).

	training_inertia([], _, 0.0).
	training_inertia([AttributeValues| Examples], Clusterer, Inertia) :-
		Clusterer = kmedoids_clusterer(Encoders, Medoids, _Options, _Diagnostics),
		kmedoids::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Medoids, Medoid),
		squared_distance(Features, Medoid, ExampleInertia),
		training_inertia(Examples, Clusterer, RestInertia),
		Inertia is ExampleInertia + RestInertia.

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		Feature is (Value - Mean) / Scale,
		encode_instance(Encoders, AttributeValues, Features).

	squared_distance([], [], 0.0).
	squared_distance([Value| Values], [Medoid| Medoids], DistanceSquared) :-
		squared_distance(Values, Medoids, RestDistanceSquared),
		Delta is Value - Medoid,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

	minimum_medoid_distance([], 0.0).
	minimum_medoid_distance([_], 0.0).
	minimum_medoid_distance([Medoid| Medoids], MinimumDistance) :-
		minimum_distance_to_others(Medoid, Medoids, FirstMinimumDistance),
		minimum_medoid_distance(Medoids, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Medoid, [Other| Others], MinimumDistance) :-
		squared_distance(Medoid, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_others(Medoid, Others, Distance, MinimumDistance).

	minimum_distance_to_others(_Medoid, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Medoid, [Other| Others], MinimumDistance0, MinimumDistance) :-
		squared_distance(Medoid, Other, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Medoid, Others, MinimumDistance1, MinimumDistance).

:- end_object.
