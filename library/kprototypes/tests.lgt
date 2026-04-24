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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Unit tests for the "kprototypes" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(kprototypes, [
		cluster/3, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1
	]).

	cover(kprototypes).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kprototypes_learn_2_mixed_profiles, deterministic(ground(Clusterer))) :-
		learn(mixed_profiles, Clusterer).

	test(kprototypes_cluster_3_left_segment, deterministic(Cluster == 1)) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		cluster(Clusterer, [age-24, income-34000, channel-online, region-north], Cluster).

	test(kprototypes_cluster_3_right_segment, deterministic(Cluster == 2)) :-
		learn(mixed_profiles, Clusterer, [k(2), initialization(spread), gamma(1.0)]),
		cluster(Clusterer, [age-53, income-80000, channel-retail, region-south], Cluster).

	test(kprototypes_learn_3_custom_options, deterministic((memberchk(k(2), Options), memberchk(maximum_iterations(40), Options), memberchk(tolerance(1.0e-5), Options), memberchk(initialization(first_k), Options), memberchk(gamma(1.5), Options), memberchk(feature_scaling(off), Options)))) :-
		learn(mixed_profiles, kprototypes_clusterer(_Encoders, _Prototypes, Options), [k(2), maximum_iterations(40), tolerance(1.0e-5), initialization(first_k), gamma(1.5), feature_scaling(off)]).

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

	test(kprototypes_cluster_3_invalid_discrete_value, error(domain_error(attribute_value(channel, [online, retail]), phone))) :-
		learn(mixed_profiles, Clusterer),
		cluster(Clusterer, [age-24, income-34000, channel-phone, region-north], _Cluster).

	test(kprototypes_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 6), 7))) :-
		learn(mixed_profiles, _Clusterer, [k(7)]).

:- end_object.
