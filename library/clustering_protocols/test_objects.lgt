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
