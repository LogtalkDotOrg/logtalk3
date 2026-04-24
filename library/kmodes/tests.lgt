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


:- object(invalid_shopping_profiles,
	implements(clustering_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).
	attribute_values(loyalty, [basic, premium]).
	attribute_values(device, [mobile, desktop]).

	example(1, [channel-online, region-north, loyalty-basic, device-mobile, device-desktop]).
	example(2, [channel-retail, region-south, loyalty-premium, device-desktop]).

:- end_object.


:- object(unstable_profiles,
	implements(clustering_dataset_protocol)).

	attribute_values(segment, [a, b]).

	example(1, [segment-a]).
	example(2, [segment-b]).
	example(3, [segment-b]).

:- end_object.


:- object(invalid_shopping_profile_declarations,
	implements(clustering_dataset_protocol)).

	attribute_values(channel, [online, retail]).
	attribute_values(channel, [online, retail]).
	attribute_values(region, [north, south]).

	example(1, [channel-online, region-north]).
	example(2, [channel-retail, region-south]).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-24,
		comment is 'Unit tests for the "kmodes" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(kmodes, [
		cluster/3, diagnostics/2, export_to_clauses/4, export_to_file/4, learn/2, learn/3, print_clusterer/1
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
		learn(shopping_profiles, kmodes_clusterer(_Encoders, _Modes, Options, _Diagnostics), [k(2), maximum_iterations(40), tolerance(0.0), initialization(first_k)]).

	test(kmodes_diagnostics_2_rich_metadata, deterministic((memberchk(model(kmodes), Diagnostics), memberchk(mode_count(2), Diagnostics), memberchk(training_example_count(6), Diagnostics), memberchk(convergence(_), Diagnostics), memberchk(iterations(Iterations), Diagnostics), Iterations >= 1, memberchk(final_shift(FinalShift), Diagnostics), FinalShift >= 0.0, memberchk(options(Options), Diagnostics), memberchk(initialization(spread), Options)))) :-
		learn(shopping_profiles, Clusterer, [k(2), initialization(spread)]),
		diagnostics(Clusterer, Diagnostics).

	test(kmodes_learn_3_maximum_iterations_termination, deterministic((memberchk(convergence(maximum_iterations), Diagnostics), memberchk(iterations(1), Diagnostics), memberchk(final_shift(FinalShift), Diagnostics), FinalShift > 0.0))) :-
		learn(unstable_profiles, Clusterer, [k(1), maximum_iterations(1), tolerance(0.0), initialization(first_k)]),
		diagnostics(Clusterer, Diagnostics).

	test(kmodes_learn_3_tolerance_termination, deterministic((memberchk(convergence(tolerance), Diagnostics), memberchk(iterations(1), Diagnostics), memberchk(final_shift(FinalShift), Diagnostics), FinalShift >= 0.0))) :-
		learn(shopping_profiles, Clusterer, [k(2), maximum_iterations(40), tolerance(1000.0), initialization(first_k)]),
		diagnostics(Clusterer, Diagnostics).

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

	test(kmodes_cluster_3_duplicate_attribute, error(domain_error(attribute_occurrences(device, 1), 2))) :-
		learn(shopping_profiles, Clusterer),
		cluster(Clusterer, [channel-online, region-north, loyalty-basic, device-mobile, device-desktop], _Cluster).

	test(kmodes_cluster_3_undeclared_attribute, error(domain_error(declared_attribute(_), segment))) :-
		learn(shopping_profiles, Clusterer),
		cluster(Clusterer, [channel-online, region-north, loyalty-basic, device-mobile, segment-student], _Cluster).

	test(kmodes_learn_3_duplicate_training_attribute, error(domain_error(attribute_occurrences(device, 1), 2))) :-
		learn(invalid_shopping_profiles, _Clusterer).

	test(kmodes_learn_3_duplicate_attribute_declaration, error(permission_error(repeat, attribute_declaration, channel))) :-
		learn(invalid_shopping_profile_declarations, _Clusterer, [k(2)]).

	test(kmodes_learn_3_mixed_profiles, error(domain_error(discrete_attribute(age), continuous))) :-
		learn(mixed_profiles, _Clusterer).

	test(kmodes_learn_3_invalid_cluster_count, error(domain_error(cluster_count(1, 6), 7))) :-
		learn(shopping_profiles, _Clusterer, [k(7)]).

:- end_object.
