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
		comment is 'Performance and reference-fit benchmarks for the "kprototypes_clusterer" library.'
	]).

	:- uses(lgtunit, [
		benchmark/2
	]).

	:- uses(list, [
		memberchk/2, nth1/3
	]).

	test(mixed_profiles_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_prototype_distance-MinPrototypeDistance))]) :-
		reference_fit(mixed_profiles, [k(2), initialization(spread), gamma(1.0)], 10.0, 3.0, TrainTime, Inertia, MinPrototypeDistance).

	test(iris_unlabeled_reference_fit, true, [note(metrics(train_seconds-TrainTime, inertia-Inertia, min_prototype_distance-MinPrototypeDistance))]) :-
		reference_fit(iris_unlabeled, [k(3)], 8.5, 2.2, TrainTime, Inertia, MinPrototypeDistance).

	reference_fit(Dataset, Options, MaximumInertia, MinimumPrototypeDistance, TrainTime, Inertia, MinPrototypeDistance) :-
		benchmark(kprototypes_clusterer::learn(Dataset, _Clusterer, Options), TrainTime),
		kprototypes_clusterer::learn(Dataset, Clusterer, Options),
		clusterer_metrics(Dataset, Clusterer, Inertia, MinPrototypeDistance),
		Inertia =< MaximumInertia,
		MinPrototypeDistance >= MinimumPrototypeDistance.

	clusterer_metrics(Dataset, kprototypes_clusterer(Encoders, Prototypes, Options, _Diagnostics), Inertia, MinPrototypeDistance) :-
		findall(
			AttributeValues,
			Dataset::example(_, AttributeValues),
			Examples
		),
		training_inertia(Examples, kprototypes_clusterer(Encoders, Prototypes, Options, _Diagnostics), Inertia),
		minimum_prototype_distance(Encoders, Prototypes, Options, MinPrototypeDistance).

	training_inertia([], _, 0.0).
	training_inertia([AttributeValues| Examples], Clusterer, Inertia) :-
		Clusterer = kprototypes_clusterer(Encoders, Prototypes, Options, _Diagnostics),
		kprototypes_clusterer::cluster(Clusterer, AttributeValues, Cluster),
		encode_instance(Encoders, AttributeValues, Features),
		nth1(Cluster, Prototypes, Prototype),
		mixed_distance(Encoders, Features, Prototype, Options, ExampleInertia),
		training_inertia(Examples, Clusterer, RestInertia),
		Inertia is ExampleInertia + RestInertia.

	encode_instance([], _, []).
	encode_instance([continuous(Attribute, Mean, Scale)| Encoders], AttributeValues, [Feature| Features]) :-
		!,
		memberchk(Attribute-Value, AttributeValues),
		Feature is (Value - Mean) / Scale,
		encode_instance(Encoders, AttributeValues, Features).
	encode_instance([discrete(Attribute, _AllowedValues)| Encoders], AttributeValues, [Value| Features]) :-
		memberchk(Attribute-Value, AttributeValues),
		encode_instance(Encoders, AttributeValues, Features).

	mixed_distance([], [], [], _Options, 0.0).
	mixed_distance([continuous(_, _, _)| Encoders], [Feature| Features], [PrototypeFeature| PrototypeFeatures], Options, Distance) :-
		!,
		mixed_distance(Encoders, Features, PrototypeFeatures, Options, RestDistance),
		Delta is Feature - PrototypeFeature,
		Distance is RestDistance + Delta * Delta.
	mixed_distance([discrete(_, _)| Encoders], [Feature| Features], [PrototypeFeature| PrototypeFeatures], Options, Distance) :-
		mixed_distance(Encoders, Features, PrototypeFeatures, Options, RestDistance),
		memberchk(gamma(Gamma), Options),
		(   Feature == PrototypeFeature ->
			Distance = RestDistance
		;   Distance is RestDistance + Gamma
		).

	minimum_prototype_distance(_Encoders, [], _Options, 0.0).
	minimum_prototype_distance(_Encoders, [_], _Options, 0.0).
	minimum_prototype_distance(Encoders, [Prototype| Prototypes], Options, MinimumDistance) :-
		minimum_distance_to_others(Encoders, Prototype, Prototypes, Options, FirstMinimumDistance),
		minimum_prototype_distance(Encoders, Prototypes, Options, RestMinimumDistance),
		(   RestMinimumDistance =< 0.0 ->
			MinimumDistance = FirstMinimumDistance
		;   MinimumDistance is min(FirstMinimumDistance, RestMinimumDistance)
		).

	minimum_distance_to_others(Encoders, Prototype, [Other| Others], Options, MinimumDistance) :-
		mixed_distance(Encoders, Prototype, Other, Options, Distance),
		minimum_distance_to_others(Encoders, Prototype, Others, Options, Distance, MinimumDistance).

	minimum_distance_to_others(_Encoders, _Prototype, [], _Options, MinimumDistance, MinimumDistance).
	minimum_distance_to_others(Encoders, Prototype, [Other| Others], Options, MinimumDistance0, MinimumDistance) :-
		mixed_distance(Encoders, Prototype, Other, Options, Distance),
		MinimumDistance1 is min(MinimumDistance0, Distance),
		minimum_distance_to_others(Encoders, Prototype, Others, Options, MinimumDistance1, MinimumDistance).

:- end_object.
