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


:- object(sample_clusterer,
	imports(clusterer_common)).

	:- uses(list, [
		member/2, memberchk/2
	]).

	learn(_Dataset, sample_clusterer([x, y]), _Options).

	check_clusterer(sample_clusterer(Attributes)) :-
		(	^^valid_attribute_names(Attributes) ->
			true
		;	domain_error(clusterer, sample_clusterer(Attributes))
		).

	clusterer_diagnostics_data(sample_clusterer(Attributes), [
		model(sample_clusterer),
		attributes(Attributes),
		options([])
	]).

	cluster(sample_clusterer(_Attributes), Instance, Cluster) :-
		(\+ member(x-_, Instance) ->
			Cluster = categorical
		; memberchk(x-X, Instance),
		  (X < 3 -> Cluster = left ; Cluster = right)
		).

	print_clusterer(Clusterer) :-
		writeq(Clusterer), nl.

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-30,
		comment is 'Smoke tests for the "clustering_protocols" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	% Dataset protocol smoke tests.

	test(two_blobs_attribute_values, deterministic(Attributes == [x-continuous, y-continuous])) :-
		findall(Attribute-Values, two_blobs::attribute_values(Attribute, Values), Attributes).

	test(two_blobs_examples_count, deterministic(Count == 8)) :-
		findall(Id, two_blobs::example(Id, _Values), Ids),
		length(Ids, Count).

	test(all_noise_examples_count, deterministic(Count == 4)) :-
		findall(Id, all_noise::example(Id, _Values), Ids),
		length(Ids, Count).

	test(bridge_noise_examples_count, deterministic(Count == 10)) :-
		findall(Id, bridge_noise::example(Id, _Values), Ids),
		length(Ids, Count).

	test(dead_component_blobs_examples_count, deterministic(Count == 6)) :-
		findall(Id, dead_component_blobs::example(Id, _Values), Ids),
		length(Ids, Count).

	test(duplicate_points_duplicate_examples, deterministic((Values1 == Values2, memberchk(x-0.0, Values1), memberchk(y-0.0, Values1)))) :-
		duplicate_points::example(1, Values1),
		duplicate_points::example(2, Values2).

	test(imbalanced_three_modes_examples_count, deterministic(Count == 9)) :-
		findall(Id, imbalanced_three_modes::example(Id, _Values), Ids),
		length(Ids, Count).

	test(iris_unlabeled_examples_shape, deterministic((memberchk(sepal_length-5.1, Values), memberchk(petal_width-0.2, Values)))) :-
		iris_unlabeled::example(1, Values).

	test(large_two_blobs_examples_count, deterministic(Count == 100)) :-
		findall(Id, large_two_blobs::example(Id, _Values), Ids),
		length(Ids, Count).

	test(mixed_profiles_attribute_values, deterministic((memberchk(channel-[online, retail], Attributes), memberchk(region-[north, south], Attributes), memberchk(age-continuous, Attributes), memberchk(income-continuous, Attributes)))) :-
		findall(Attribute-Values, mixed_profiles::attribute_values(Attribute, Values), Attributes).

	test(scaling_bands_examples_shape, deterministic((memberchk(x-20.0, Values), memberchk(y-(-0.1), Values)))) :-
		scaling_bands::example(3, Values).

	test(single_blob_examples_count, deterministic(Count == 6)) :-
		findall(Id, single_blob::example(Id, _Values), Ids),
		length(Ids, Count).

	% Sample clusterer protocol smoke tests.

	test(sample_clusterer_learn_2, deterministic(Clusterer == sample_clusterer([x, y]))) :-
		sample_clusterer::learn(two_blobs, Clusterer).

	test(sample_clusterer_cluster_3_continuous, deterministic(Cluster == left)) :-
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::cluster(Clusterer, [x-1.0, y-1.1], Cluster).

	test(sample_clusterer_cluster_3_categorical, deterministic(Cluster == categorical)) :-
		sample_clusterer::learn(mixed_profiles, Clusterer),
		sample_clusterer::cluster(Clusterer, [channel-online, region-north], Cluster).

	test(sample_clusterer_diagnostics_2, deterministic((memberchk(model(sample_clusterer), Diagnostics), memberchk(attributes([x, y]), Diagnostics), memberchk(options([]), Diagnostics)))) :-
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::diagnostics(Clusterer, Diagnostics).

	test(sample_clusterer_valid_clusterer_1, deterministic(sample_clusterer::valid_clusterer(Clusterer))) :-
		sample_clusterer::learn(two_blobs, Clusterer).

	test(sample_clusterer_invalid_clusterer_1, fail) :-
		sample_clusterer::valid_clusterer(sample_clusterer([x, 1])).

	test(sample_clusterer_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::diagnostics(Clusterer, Diagnostics),
		findall(Diagnostic, sample_clusterer::diagnostic(Clusterer, Diagnostic), Enumerated).

	test(sample_clusterer_clusterer_options_2, deterministic(Options == [])) :-
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::clusterer_options(Clusterer, Options).

	test(sample_clusterer_export_to_clauses_4, deterministic(Clause == clustered([x, y]))) :-
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::export_to_clauses(two_blobs, Clusterer, clustered, [Clause]).

	test(sample_clusterer_export_to_file_4_header, deterministic(HeaderLines == ['% exported clusterer predicate: clustered/1', '% training dataset: two_blobs', '% diagnostics: [model(sample_clusterer),attributes([x,y]),options([])]', '% clustered(Clusterer)'])) :-
		^^file_path('test_output.pl', File),
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::export_to_file(two_blobs, Clusterer, clustered, File),
		header_lines(File, HeaderLines).

	test(sample_clusterer_export_to_file_4, deterministic(Attributes == [x, y])) :-
		^^file_path('test_output.pl', File),
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::export_to_file(two_blobs, Clusterer, clustered, File),
		logtalk_load(File),
		{clustered(ExportedClusterer)},
		ExportedClusterer = sample_clusterer(Attributes).

	test(sample_clusterer_print_clusterer_1, deterministic) :-
		^^suppress_text_output,
		sample_clusterer::learn(two_blobs, Clusterer),
		sample_clusterer::print_clusterer(Clusterer).

	% auxiliary predicates

	header_lines(File, Lines) :-
		open(File, read, Stream),
		reader::line_to_codes(Stream, Codes),
		read_lines(Codes, Stream, Lines).

	read_lines(end_of_file, Stream, []) :-
		!,
		close(Stream).
	read_lines(Codes, Stream, [Line| Lines]) :-
		Codes = [0'%| _],
		!,
		atom_codes(Line, Codes),
		reader::line_to_codes(Stream, NextCodes),
		read_lines(NextCodes, Stream, Lines).
	read_lines(_, Stream, []) :-
		close(Stream).

:- end_object.
