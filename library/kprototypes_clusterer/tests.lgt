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


:- object(invalid_mixed_profiles,
	implements(clustering_dataset_protocol)).

	attribute_values(age, continuous).
	attribute_values(income, continuous).
	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).

	example(1, [age-23, age-24, income-32000, channel-online, region-north]).
	example(2, [age-52, income-78000, channel-retail, region-south]).

:- end_object.


:- object(invalid_mixed_profile_declarations,
	implements(clustering_dataset_protocol)).

	attribute_values(age, continuous).
	attribute_values(age, continuous).
	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).

	example(1, [age-23, channel-online, region-north]).
	example(2, [age-52, channel-retail, region-south]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "kprototypes_clusterer" library.'
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(kprototypes_clusterer, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1, valid_clusterer/1
	]).

	cover(kprototypes_clusterer).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kprototypes_learn_2_mixed_profiles, deterministic(ground(Clusterer))) :-
		learn(mixed_profiles, Clusterer).

	test(kprototypes_valid_clusterer_1_valid, deterministic(valid_clusterer(Clusterer))) :-
		learn(mixed_profiles, Clusterer).

	test(kprototypes_valid_clusterer_1_invalid, fail) :-
		learn(mixed_profiles, kprototypes_clusterer(Encoders, Prototypes, Options, _Diagnostics)),
		valid_clusterer(kprototypes_clusterer(Encoders, Prototypes, Options, [model(kprototypes_clusterer), prototype_count(99), options(Options)])).

	test(kprototypes_cluster_3_left_segment, deterministic(Cluster == 1)) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		cluster(Clusterer, [age-24, income-34000, channel-online, region-north], Cluster).

	test(kprototypes_cluster_3_right_segment, deterministic(Cluster == 2)) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		cluster(Clusterer, [age-53, income-80000, channel-retail, region-south], Cluster).

	test(kprototypes_learn_3_custom_options, deterministic([K, MaximumIterations, Tolerance, Initialization, Gamma, FeatureScaling] == [2, 40, 1.0e-5, first_k, 1.5, off])) :-
		learn(mixed_profiles, kprototypes_clusterer(_Encoders, _Prototypes, Options, _Diagnostics), [k(2), maximum_iterations(40), tolerance(1.0e-5), initialization(first_k), gamma(1.5), feature_scaling(off)]),
		memberchk(k(K), Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(initialization(Initialization), Options),
		memberchk(gamma(Gamma), Options),
		memberchk(feature_scaling(FeatureScaling), Options).

	test(kprototypes_cluster_3_two_blobs_extremes, deterministic(Cluster1 \== Cluster2)) :-
		learn(two_blobs, Clusterer, [k(2), initialization(spread), feature_scaling(off)]),
		cluster(Clusterer, [x-0.9, y-1.1], Cluster1),
		cluster(Clusterer, [x-5.1, y-5.0], Cluster2).

	test(kprototypes_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		export_to_clauses(mixed_profiles, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [age-26, income-35500, channel-online, region-north], Cluster).

	test(kprototypes_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(mixed_profiles, Clusterer),
		export_to_file(mixed_profiles, Clusterer, clustered, File).

	test(kprototypes_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		export_to_file(mixed_profiles, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [age-54, income-81000, channel-retail, region-south], Cluster).

	test(kprototypes_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(mixed_profiles, Clusterer),
		print_clusterer(Clusterer).

	test(kprototypes_diagnostics_2_rich, deterministic((Iterations > 0, FinalShift >= 0.0, [Model, PrototypeCount, TrainingExampleCount, Gamma] == [kprototypes_clusterer, 2, 6, 1.0]))) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(prototype_count(PrototypeCount), Diagnostics),
		memberchk(training_example_count(TrainingExampleCount), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		assertion(nonvar(Convergence)),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(gamma(Gamma), Options).

	test(kprototypes_learn_3_maximum_iterations_termination, deterministic((FinalShift > 0.0, Convergence == maximum_iterations, Iterations == 1))) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(first_k), gamma(1.0), feature_scaling(off), maximum_iterations(1), tolerance(1.0e-9)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(kprototypes_learn_3_tolerance_termination, deterministic((FinalShift > 0.0, Convergence == tolerance, Iterations == 1))) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(first_k), gamma(1.0), feature_scaling(off), maximum_iterations(50), tolerance(1.0e12)]),
		diagnostics(Clusterer, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_shift(FinalShift), Diagnostics).

	test(kprototypes_cluster_3_invalid_discrete_value, error(domain_error(attribute_value(channel, [online, retail]), phone))) :-
		learn(mixed_profiles, Clusterer),
		cluster(Clusterer, [age-24, income-34000, channel-phone, region-north], _Cluster).

	test(kprototypes_cluster_3_missing_attribute, error(existence_error(attribute, region))) :-
		learn(mixed_profiles, Clusterer),
		cluster(Clusterer, [age-24, income-34000, channel-online], _Cluster).

	test(kprototypes_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(channel, 1), 2))) :-
		learn(mixed_profiles, Clusterer),
		cluster(Clusterer, [age-24, income-34000, channel-online, channel-retail, region-north], _Cluster).

	test(kprototypes_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), loyalty))) :-
		learn(mixed_profiles, Clusterer),
		cluster(Clusterer, [age-24, income-34000, channel-online, region-north, loyalty-basic], _Cluster).

	test(kprototypes_learn_3_duplicate_training_attribute, error(domain_error(attribute_occurrences(age, 1), 2))) :-
		learn(invalid_mixed_profiles, _Clusterer).

	test(kprototypes_learn_3_duplicate_attribute_declaration, error(permission_error(repeat, attribute_declaration, age))) :-
		learn(invalid_mixed_profile_declarations, _Clusterer, [feature_scaling(off)]).

	test(kprototypes_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 6), 7))) :-
		learn(mixed_profiles, _Clusterer, [k(7)]).

:- end_object.
