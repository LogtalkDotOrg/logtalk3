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
		date is 2026-05-06,
		comment is 'Performance and reference-fit benchmarks for the "kmodes_clusterer" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		memberchk/2, nth1/3
	]).

	test(shopping_profiles_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_mode_distance-MinModeDistance))]) :-
		reference_fit(shopping_profiles, [k(2), initialization(spread)], 2.0, 4.0, TrainTime, Inertia, MinModeDistance).

	test(shopping_profiles_first_k_training_baseline, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_mode_distance-MinModeDistance))]) :-
		benchmark(kmodes_clusterer::learn(shopping_profiles, _Clusterer, [k(2), initialization(first_k)]), TrainTime),
		kmodes_clusterer::learn(shopping_profiles, Clusterer, [k(2), initialization(first_k)]),
		clusterer_metrics(shopping_profiles, Clusterer, Inertia, MinModeDistance),
		Inertia >= 0.0,
		MinModeDistance >= 0.0.

	reference_fit(Dataset, Options, MaximumInertia, MinimumModeDistance, TrainTime, Inertia, MinModeDistance) :-
		benchmark(kmodes_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		kmodes_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, Inertia, MinModeDistance),
		Inertia =< MaximumInertia,
		MinModeDistance >= MinimumModeDistance.

	clusterer_metrics(Dataset, kmodes_clusterer(Encoders, Modes, Options, _Diagnostics), Inertia, MinModeDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		training_inertia(Examples, kmodes_clusterer(Encoders, Modes, Options, _Diagnostics), Inertia),
		minimum_mode_distance(Encoders, Modes, MinModeDistance).

	training_inertia([], _, 0.0).
	training_inertia([AttributeValues| Examples], Clusterer, Inertia) :-
		Clusterer = kmodes_clusterer(Encoders, Modes, _Options, _Diagnostics),
		kmodes_clusterer::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Modes, Mode),
		mismatch_distance(Encoders, Features, Mode, ExampleInertia),
		training_inertia(Examples, Clusterer, RestInertia),
		Inertia is ExampleInertia + RestInertia.

	encode_instance([], _, []).
	encode_instance([discrete(Attribute, _AllowedValues)| Encoders], AttributeValues, [Value| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		encode_instance(Encoders, AttributeValues, Features).

	mismatch_distance([], [], [], 0.0).
	mismatch_distance([discrete(_, _)| Encoders], [Feature| Features], [ModeFeature| ModeFeatures], Distance) :-
		mismatch_distance(Encoders, Features, ModeFeatures, RestDistance),
		(	Feature == ModeFeature ->
			Distance = RestDistance
		;	Distance is RestDistance + 1.0
		).

	minimum_mode_distance(_Encoders, [], 0.0).
	minimum_mode_distance(_Encoders, [_], 0.0).
	minimum_mode_distance(Encoders, [Mode| Modes], MinimumDistance) :-
		minimum_distance_to_others(Encoders, Mode, Modes, FirstMinimumDistance),
		minimum_mode_distance(Encoders, Modes, RestMinimumDistance),
		(	RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;	MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Encoders, Mode, [Other| Others], MinimumDistance) :-
		mismatch_distance(Encoders, Mode, Other, Distance),
		minimum_distance_to_others(Encoders, Mode, Others, Distance, MinimumDistance).

	minimum_distance_to_others(_Encoders, _Mode, [], MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Encoders, Mode, [Other| Others], MinimumDistance0, MinimumDistance) :-
		mismatch_distance(Encoders, Mode, Other, Distance),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Encoders, Mode, Others, MinimumDistance1, MinimumDistance).

:- end_object.
