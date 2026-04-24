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
		date is 2026-04-23,
		comment is 'Performance and reference-fit benchmarks for the "gaussian_mixture" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	test(two_blobs_reference_fit, true, [note(metrics(train_seconds-TrainTime, min_component_weight-MinWeight, min_mean_distance-MinMeanDistance))]) :-
		reference_fit(two_blobs, [k(2), initialization(spread), feature_scaling(off)], 0.25, 5.5, TrainTime, MinWeight, MinMeanDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, min_component_weight-MinWeight, min_mean_distance-MinMeanDistance))]) :-
		reference_fit(iris_unlabeled, [k(3), initialization(spread)], 0.1, 2.0, TrainTime, MinWeight, MinMeanDistance).

	reference_fit(Dataset, Options, MinimumWeight, MinimumMeanDistance, TrainTime, MinWeight, MinMeanDistance) :-
		benchmark(gaussian_mixture::learn(Dataset, _Clusterer, Options), TrainTime),
		gaussian_mixture::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Clusterer, MinWeight, MinMeanDistance),
		MinWeight >= MinimumWeight,
		MinMeanDistance >= MinimumMeanDistance.

	clusterer_metrics(gaussian_mixture_clusterer(_Encoders, Components, Weights, _Options, _Diagnostics), MinWeight, MinMeanDistance) :-
		minimum_weight(Weights, MinWeight),
		minimum_mean_distance(Components, MinMeanDistance).

	minimum_weight([Weight| Weights], MinimumWeight) :-
		minimum_weight(Weights, Weight, MinimumWeight).

	minimum_weight([], MinimumWeight, MinimumWeight).
	minimum_weight([Weight| Weights], CurrentMinimum, MinimumWeight) :-
		NextMinimum is min(CurrentMinimum, Weight),
		minimum_weight(Weights, NextMinimum, MinimumWeight).

	minimum_mean_distance([], 0.0).
	minimum_mean_distance([_], 0.0).
	minimum_mean_distance([component(Mean, _Variances)| Components], MinimumDistance) :-
		minimum_distance_to_means(Mean, Components, FirstMinimumDistance),
		minimum_mean_distance(Components, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_means(Mean, [component(OtherMean, _OtherVariances)| Components], MinimumDistance) :-
		squared_distance(Mean, OtherMean, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		minimum_distance_to_means(Mean, Components, Distance, MinimumDistance).

	minimum_distance_to_means(_Mean, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_means(Mean, [component(OtherMean, _OtherVariances)| Components], MinimumDistance0, MinimumDistance) :-
		squared_distance(Mean, OtherMean, DistanceSquared),
		Distance is sqrt(DistanceSquared),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_means(Mean, Components, MinimumDistance1, MinimumDistance).

	squared_distance([], [], 0.0).
	squared_distance([Value1| Values1], [Value2| Values2], DistanceSquared) :-
		squared_distance(Values1, Values2, RestDistanceSquared),
		Delta is Value1 - Value2,
		DistanceSquared is RestDistanceSquared + Delta * Delta.

:- end_object.
