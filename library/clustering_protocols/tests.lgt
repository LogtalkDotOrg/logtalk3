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


:- object(clustering_protocols_test_adapter,
	imports([clusterer_common, search_indexing])).

	:- public([
		common_dataset_attributes/2,
		common_check_continuous_attributes/1,
		common_check_examples/3,
		common_check_attribute_bindings/2,
		common_check_encoded_attribute_bindings/2,
		common_build_encoders/4,
		common_examples_to_rows/3,
		common_encode_instance/3,
		common_check_cluster_count/2,
		common_take_first_k/3,
		common_remove_candidate/3,
		common_valid_continuous_encoders/1,
		common_valid_discrete_encoders/1,
		common_valid_mixed_encoders/1,
		common_valid_mixed_vectors/2,
		common_valid_clusterer_metadata/3,
		common_valid_diagnostic_count/3,
		common_valid_diagnostic_choice/3,
		index_build/3,
		index_range_query/5,
		index_split_sorted_rows/5
	]).

	common_dataset_attributes(Dataset, Attributes) :-
		^^dataset_attributes(Dataset, Attributes).

	common_check_continuous_attributes(Attributes) :-
		^^check_continuous_attributes(Attributes).

	common_check_examples(Dataset, AttributeNames, Examples) :-
		^^check_examples(Dataset, AttributeNames, Examples).

	common_check_attribute_bindings(AttributeNames, AttributeValues) :-
		^^check_attribute_bindings(AttributeNames, AttributeValues).

	common_check_encoded_attribute_bindings(Encoders, AttributeValues) :-
		^^check_encoded_attribute_bindings(Encoders, AttributeValues).

	common_build_encoders(AttributeNames, Examples, Options, Encoders) :-
		^^build_encoders(AttributeNames, Examples, Options, Encoders).

	common_examples_to_rows(Examples, Encoders, Rows) :-
		^^examples_to_rows(Examples, Encoders, Rows).

	common_encode_instance(Encoders, AttributeValues, Features) :-
		^^encode_instance(Encoders, AttributeValues, Features).

	common_check_cluster_count(K, Count) :-
		^^check_cluster_count(K, Count).

	common_take_first_k(K, Rows, Vectors) :-
		^^take_first_k(K, Rows, Vectors).

	common_remove_candidate(Candidate, Candidates, RemainingCandidates) :-
		^^remove_candidate(Candidate, Candidates, RemainingCandidates).

	common_valid_continuous_encoders(Encoders) :-
		^^valid_continuous_encoders(Encoders).

	common_valid_discrete_encoders(Encoders) :-
		^^valid_discrete_encoders(Encoders).

	common_valid_mixed_encoders(Encoders) :-
		^^valid_mixed_encoders(Encoders).

	common_valid_mixed_vectors(Encoders, Vectors) :-
		^^valid_mixed_vectors(Encoders, Vectors).

	common_valid_clusterer_metadata(Model, Options, Diagnostics) :-
		^^valid_clusterer_metadata(Model, Options, Diagnostics).

	common_valid_diagnostic_count(Functor, Diagnostics, Count) :-
		^^valid_diagnostic_count(Functor, Diagnostics, Count).

	common_valid_diagnostic_choice(Functor, Diagnostics, Choices) :-
		^^valid_diagnostic_choice(Functor, Diagnostics, Choices).

	clusterer_diagnostics_data(test_clusterer, [
		model(test),
		options([feature_scaling(off), cell_size(1.0)])
	]).

	index_build(Rows, Options, Index) :-
		^^build_auto_search_index(Rows, Options, Index).

	index_range_query(Index, Vector, Options, Epsilon, Neighbors) :-
		^^range_query(Index, Vector, Options, Epsilon, Neighbors).

	index_split_sorted_rows(SortedRows, InnerUpperBound, OuterLowerBound, InnerRows, OuterRows) :-
		^^split_sorted_rows(SortedRows, InnerUpperBound, OuterLowerBound, InnerRows, OuterRows).

	search_index_cell_size(Options, CellSize) :-
		^^option(cell_size(CellSize), Options).

	select_metric_pivot([Pivot| Rows], Options, Pivot, SortedRows) :-
		Pivot = _-Vector,
		decorate_rows(Rows, Vector, Options, DecoratedRows),
		keysort(DecoratedRows, SortedRows).

	distance(_Options, Vector1, Vector2, Distance) :-
		squared_distance(Vector1, Vector2, 0.0, SquaredDistance),
		Distance is sqrt(SquaredDistance).

	decorate_rows([], _Pivot, _Options, []).
	decorate_rows([Id-Vector| Rows], Pivot, Options, [Distance-(Id-Vector)| DecoratedRows]) :-
		distance(Options, Pivot, Vector, Distance),
		decorate_rows(Rows, Pivot, Options, DecoratedRows).

	squared_distance([], [], SquaredDistance, SquaredDistance).
	squared_distance([Value1| Values1], [Value2| Values2], SquaredDistance0, SquaredDistance) :-
		Difference is Value1 - Value2,
		SquaredDistance1 is SquaredDistance0 + Difference * Difference,
		squared_distance(Values1, Values2, SquaredDistance1, SquaredDistance).

	default_option(feature_scaling(off)).
	default_option(cell_size(1.0)).

	valid_option(feature_scaling(FeatureScaling)) :-
		once((FeatureScaling == on; FeatureScaling == off)).
	valid_option(cell_size(CellSize)) :-
		number(CellSize),
		CellSize > 0.

:- end_object.


:- object(duplicate_attribute_declarations).

	:- public(attribute_values/2).

	attribute_values(x, continuous).
	attribute_values(x, continuous).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-09-12,
		comment is 'Smoke tests for the "clustering_protocols" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	cover(clusterer_common).
	cover(search_indexing).

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

	% Shared clustering helper tests.

	test(common_dataset_attributes, deterministic(Attributes == [x-continuous, y-continuous])) :-
		clustering_protocols_test_adapter::common_dataset_attributes(two_blobs, Attributes).

	test(common_dataset_attributes_duplicate, error(permission_error(repeat, attribute_declaration, x))) :-
		clustering_protocols_test_adapter::common_dataset_attributes(duplicate_attribute_declarations, _).

	test(common_check_continuous_attributes, deterministic) :-
		clustering_protocols_test_adapter::common_check_continuous_attributes([x-continuous, y-continuous]).

	test(common_check_continuous_attributes_error, error(domain_error(continuous_attribute(channel), [online, retail]))) :-
		clustering_protocols_test_adapter::common_check_continuous_attributes([channel-[online, retail]]).

	test(common_check_examples, deterministic) :-
		findall(Id-Values, two_blobs::example(Id, Values), Examples),
		clustering_protocols_test_adapter::common_check_examples(two_blobs, [x, y], Examples).

	test(common_check_examples_empty, error(domain_error(non_empty_dataset, two_blobs))) :-
		clustering_protocols_test_adapter::common_check_examples(two_blobs, [x, y], []).

	test(common_check_attribute_bindings_missing, error(existence_error(attribute, y))) :-
		clustering_protocols_test_adapter::common_check_attribute_bindings([x, y], [x-1.0]).

	test(common_check_attribute_bindings_duplicate, error(domain_error(attribute_occurrences(x, 1), 2))) :-
		clustering_protocols_test_adapter::common_check_attribute_bindings([x], [x-1.0, x-2.0]).

	test(common_check_attribute_bindings_undeclared, error(domain_error(declared_attribute([x]), y))) :-
		clustering_protocols_test_adapter::common_check_attribute_bindings([x], [x-1.0, y-2.0]).

	test(common_check_encoded_attribute_bindings_mixed, deterministic) :-
		Encoders = [continuous(x, 0.0, 1.0), discrete(channel, [online, retail])],
		clustering_protocols_test_adapter::common_check_encoded_attribute_bindings(Encoders, [x-1.0, channel-online]).

	test(common_encoding_pipeline, deterministic((Encoders == [continuous(x, 0.0, 1.0), continuous(y, 0.0, 1.0)], Rows == [a-[1.0, 2.0], b-[3.0, 4.0]], Features == [5.0, 6.0]))) :-
		Examples = [a-[x-1.0, y-2.0], b-[x-3.0, y-4.0]],
		clustering_protocols_test_adapter::common_build_encoders([x, y], Examples, [feature_scaling(off), cell_size(1.0)], Encoders),
		clustering_protocols_test_adapter::common_examples_to_rows(Examples, Encoders, Rows),
		clustering_protocols_test_adapter::common_encode_instance(Encoders, [x-5.0, y-6.0], Features).

	test(common_encoding_pipeline_scaled, deterministic((Mean > 1.99, Mean < 2.01, Scale > 0.0))) :-
		Examples = [a-[x-1.0], b-[x-3.0]],
		clustering_protocols_test_adapter::common_build_encoders([x], Examples, [feature_scaling(on), cell_size(1.0)], [continuous(x, Mean, Scale)]).

	test(common_check_cluster_count_error, error(domain_error(cluster_count(1, 2), 3))) :-
		clustering_protocols_test_adapter::common_check_cluster_count(3, 2).

	test(common_take_and_remove, deterministic((Vectors == [[0.0], [1.0]], Remaining == [a-[0.0], c-[2.0]]))) :-
		Rows = [a-[0.0], b-[1.0], c-[2.0]],
		clustering_protocols_test_adapter::common_take_first_k(2, Rows, Vectors),
		clustering_protocols_test_adapter::common_remove_candidate(b-[1.0], Rows, Remaining).

	test(common_encoder_validations, true) :-
		clustering_protocols_test_adapter::common_valid_continuous_encoders([continuous(x, 0.0, 1.0)]),
		clustering_protocols_test_adapter::common_valid_discrete_encoders([discrete(channel, [online, retail])]),
		clustering_protocols_test_adapter::common_valid_mixed_encoders([continuous(x, 0.0, 1.0), discrete(channel, [online, retail])]),
		clustering_protocols_test_adapter::common_valid_mixed_vectors([continuous(x, 0.0, 1.0), discrete(channel, [online, retail])], [[1.0, online], [2, retail]]).

	test(common_diagnostic_validations, deterministic) :-
		Diagnostics = [model(test), options([feature_scaling(off), cell_size(1.0)]), iterations(3), method(exact)],
		clustering_protocols_test_adapter::common_valid_clusterer_metadata(test, [feature_scaling(off), cell_size(1.0)], Diagnostics),
		clustering_protocols_test_adapter::common_valid_diagnostic_count(iterations, Diagnostics, 3),
		clustering_protocols_test_adapter::common_valid_diagnostic_choice(method, Diagnostics, [heuristic, exact]).

	test(common_valid_clusterer, deterministic(clustering_protocols_test_adapter::valid_clusterer(test_clusterer))).

	test(common_invalid_clusterer, fail) :-
		clustering_protocols_test_adapter::valid_clusterer(not_a_clusterer).

	% Shared search-index tests.

	test(search_index_empty, deterministic(Index == metric_tree(empty))) :-
		clustering_protocols_test_adapter::index_build([], [cell_size(1.0)], Index).

	test(search_index_empty_metric_tree_query, deterministic(Neighbors == [])) :-
		clustering_protocols_test_adapter::index_build([], [], Index),
		clustering_protocols_test_adapter::index_range_query(Index, [0.0], [], 1.0, Neighbors).

	test(search_index_split_empty, deterministic((InnerUpperBound == none, OuterLowerBound == none, InnerRows == [], OuterRows == []))) :-
		clustering_protocols_test_adapter::index_split_sorted_rows([], InnerUpperBound, OuterLowerBound, InnerRows, OuterRows).

	test(search_index_metric_tree_query, deterministic(Neighbors == [a-[0.0, 0.0], b-[1.0, 0.0]])) :-
		Rows = [a-[0.0, 0.0], b-[1.0, 0.0], c-[4.0, 0.0]],
		clustering_protocols_test_adapter::index_build(Rows, [cell_size(1.0)], Index),
		clustering_protocols_test_adapter::index_range_query(Index, [0.0, 0.0], [], 1.1, Neighbors).

	test(search_index_metric_tree_node_queries, deterministic((AllCount == 17, Near == [metric_1-[1.0, 0.0, 0.0, 0.0, 0.0]], Far == []))) :-
		indexed_metric_rows(1, 17, Rows),
		clustering_protocols_test_adapter::index_build(Rows, [cell_size(1.0)], Index),
		clustering_protocols_test_adapter::index_range_query(Index, [1.0, 0.0, 0.0, 0.0, 0.0], [], 100.0, All),
		length(All, AllCount),
		clustering_protocols_test_adapter::index_range_query(Index, [1.0, 0.0, 0.0, 0.0, 0.0], [], 0.1, Near),
		clustering_protocols_test_adapter::index_range_query(Index, [100.0, 0.0, 0.0, 0.0, 0.0], [], 0.1, Far).

	test(search_index_grid_query, deterministic((Functor == grid, Neighbors == [row_1-[1.0, 0.0], row_2-[2.0, 0.0]]))) :-
		indexed_rows(1, 64, Rows),
		clustering_protocols_test_adapter::index_build(Rows, [cell_size(2.0)], Index),
		functor(Index, Functor, 2),
		clustering_protocols_test_adapter::index_range_query(Index, [1.5, 0.0], [], 0.6, Neighbors).

	% auxiliary predicates

	indexed_rows(Index, Last, []) :-
		Index > Last,
		!.
	indexed_rows(Index, Last, [Id-[Value, 0.0]| Rows]) :-
		atomic_concat(row_, Index, Id),
		Value is float(Index),
		NextIndex is Index + 1,
		indexed_rows(NextIndex, Last, Rows).

	indexed_metric_rows(Index, Last, []) :-
		Index > Last,
		!.
	indexed_metric_rows(Index, Last, [Id-[Value, 0.0, 0.0, 0.0, 0.0]| Rows]) :-
		atomic_concat(metric_, Index, Id),
		Value is float(Index),
		NextIndex is Index + 1,
		indexed_metric_rows(NextIndex, Last, Rows).

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
