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
		comment is 'Unit tests for the "kmodes" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(kmodes, [
		cluster/3, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1
	]).

	cover(kmodes).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kmodes_learn_2_shopping_profiles, deterministic(ground(Clusterer))) :-
		learn(shopping_profiles, Clusterer).

	test(kmodes_cluster_3_left_segment, deterministic(Cluster == 1)) :-
		learn(shopping_profiles, Clusterer, [k(2), initialization(spread)]),
		cluster(Clusterer, [channel-online, region-north, loyalty-basic, device-mobile], Cluster).

	test(kmodes_cluster_3_right_segment, deterministic(Cluster == 2)) :-
		learn(shopping_profiles, Clusterer, [k(2), initialization(spread)]),
		cluster(Clusterer, [channel-retail, region-south, loyalty-premium, device-desktop], Cluster).

	test(kmodes_learn_3_custom_options, deterministic((memberchk(k(2), Options), memberchk(maximum_iterations(40), Options), memberchk(tolerance(0.0), Options), memberchk(initialization(first_k), Options)))) :-
		learn(shopping_profiles, kmodes_clusterer(_Encoders, _Modes, Options), [k(2), maximum_iterations(40), tolerance(0.0), initialization(first_k)]).

	test(kmodes_export_to_clauses_4, deterministic(Cluster == 1)) :-
		learn(shopping_profiles, Clusterer, [k(2), initialization(spread)]),
		export_to_clauses(shopping_profiles, Clusterer, clustered, [ExportedClusterer]),
		cluster(ExportedClusterer, [channel-online, region-north, loyalty-premium, device-mobile], Cluster).

	test(kmodes_export_to_file_4_written, deterministic(os::file_exists(File))) :-
		^^file_path('test_output.pl', File),
		learn(shopping_profiles, Clusterer),
		export_to_file(shopping_profiles, Clusterer, clustered, File).

	test(kmodes_export_to_file_4_loaded, deterministic(Cluster == 2)) :-
		^^file_path('test_output.pl', File),
		learn(shopping_profiles, Clusterer, [k(2), initialization(spread)]),
		export_to_file(shopping_profiles, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		cluster(ExportedClusterer, [channel-retail, region-south, loyalty-basic, device-desktop], Cluster).

	test(kmodes_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		learn(shopping_profiles, Clusterer),
		print_clusterer(Clusterer).

	test(kmodes_cluster_3_invalid_discrete_value, error(domain_error(attribute_value(device, [mobile, desktop]), tablet))) :-
		learn(shopping_profiles, Clusterer),
		cluster(Clusterer, [channel-online, region-north, loyalty-basic, device-tablet], _Cluster).

	test(kmodes_learn_3_mixed_profiles, error(domain_error(discrete_attribute(age), continuous))) :-
		learn(mixed_profiles, _Clusterer).

	test(kmodes_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 6), 7))) :-
		learn(shopping_profiles, _Clusterer, [k(7)]).

:- end_object.
