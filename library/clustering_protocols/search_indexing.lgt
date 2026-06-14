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


:- category(search_indexing).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Shared helpers for adaptive search-index construction and range queries used by clustering libraries.'
	]).

	:- protected(build_auto_search_index/3).
	:- mode(build_auto_search_index(+list(pair), +list(compound), -compound), one).
	:- info(build_auto_search_index/3, [
		comment is 'Builds an adaptive search index by selecting either a grid index or a metric tree based on dataset shape.',
		argnames is ['Rows', 'Options', 'SearchIndex']
	]).

	:- protected(build_metric_tree/3).
	:- mode(build_metric_tree(+list(pair), +list(compound), -compound), one).
	:- info(build_metric_tree/3, [
		comment is 'Builds a metric tree search index for encoded dataset rows.',
		argnames is ['Rows', 'Options', 'MetricTree']
	]).

	:- protected(build_grid_index/3).
	:- mode(build_grid_index(+list(pair), +list(compound), -compound), one).
	:- info(build_grid_index/3, [
		comment is 'Builds a grid-based search index for encoded dataset rows using the object-defined cell size.',
		argnames is ['Rows', 'Options', 'GridIndex']
	]).

	:- protected(range_query/5).
	:- mode(range_query(+compound, +list(number), +list(compound), +number, -list(pair)), one).
	:- info(range_query/5, [
		comment is 'Queries a search index for rows within the given epsilon distance of an encoded feature vector.',
		argnames is ['SearchIndex', 'Vector', 'Options', 'Epsilon', 'Neighbors']
	]).

	:- protected(search_index_cell_size/2).
	:- mode(search_index_cell_size(+list(compound), -number), one).
	:- info(search_index_cell_size/2, [
		comment is 'Returns the grid cell size to use when constructing a grid-based search index.',
		argnames is ['Options', 'CellSize']
	]).

	:- protected(select_metric_pivot/4).
	:- mode(select_metric_pivot(+list(pair), +list(compound), -pair, -list(pair)), one).
	:- info(select_metric_pivot/4, [
		comment is 'Selects a pivot row and returns the remaining rows decorated and sorted by distance to that pivot.',
		argnames is ['Rows', 'Options', 'Pivot', 'SortedRows']
	]).

	:- protected(distance/4).
	:- mode(distance(+list(compound), +list(number), +list(number), -number), one).
	:- info(distance/4, [
		comment is 'Computes the distance between two encoded feature vectors using the effective object options.',
		argnames is ['Options', 'Vector1', 'Vector2', 'Distance']
	]).

	:- protected(split_sorted_rows/5).
	:- mode(split_sorted_rows(+list(pair), -number, -number, -list(pair), -list(pair)), one).
	:- info(split_sorted_rows/5, [
		comment is 'Splits rows decorated with distances into inner and outer partitions and returns their boundary distances.',
		argnames is ['SortedRows', 'InnerUpperBound', 'OuterLowerBound', 'InnerRows', 'OuterRows']
	]).

	:- uses(list, [
		length/2, reverse/2
	]).

	build_auto_search_index([], _Options, metric_tree(empty)).
	build_auto_search_index([Row| Rows], Options, SearchIndex) :-
		row_count_and_dimension([Row| Rows], RowCount, Dimension),
		(	grid_index_applicable(RowCount, Dimension) ->
			build_grid_index([Row| Rows], Options, SearchIndex)
		;	build_metric_tree([Row| Rows], Options, MetricTree),
			SearchIndex = metric_tree(MetricTree)
		).

	build_metric_tree(Rows, Options, MetricTree) :-
		metric_tree_leaf_size(Rows, LeafSize),
		build_metric_tree(Rows, Options, LeafSize, MetricTree).

	build_metric_tree([], _Options, _LeafSize, empty).
	build_metric_tree([Row| Rows], _Options, LeafSize, leaf([Row| Rows])) :-
		length([Row| Rows], Count),
		Count =< LeafSize,
		!.
	build_metric_tree(Rows, Options, LeafSize, node(Id-Vector, InnerUpperBound, OuterLowerBound, InnerTree, OuterTree)) :-
		::select_metric_pivot(Rows, Options, Id-Vector, SortedRows),
		split_sorted_rows(SortedRows, InnerUpperBound, OuterLowerBound, InnerRows, OuterRows),
		build_metric_tree(InnerRows, Options, LeafSize, InnerTree),
		build_metric_tree(OuterRows, Options, LeafSize, OuterTree).

	metric_tree_leaf_size(Rows, LeafSize) :-
		row_count_and_dimension(Rows, RowCount, Dimension),
		Candidate is round((sqrt(RowCount) * 2.0) / max(1, Dimension)),
		LeafSize is max(8, min(32, Candidate)).

	row_count_and_dimension([_-Vector| Rows], RowCount, Dimension) :-
		length([_-Vector| Rows], RowCount),
		length(Vector, Dimension).

	grid_index_applicable(RowCount, Dimension) :-
		RowCount >= 64,
		Dimension =< 4.

	build_grid_index(Rows, Options, grid(CellSize, Cells)) :-
		::search_index_cell_size(Options, CellSize),
		position_rows(Rows, 1, PositionedRows),
		reverse(PositionedRows, ReversedRows),
		avltree::new(Cells0),
		build_grid_cells(ReversedRows, CellSize, Cells0, Cells).

	position_rows([], _Position, []).
	position_rows([Row| Rows], Position, [Position-Row| PositionedRows]) :-
		NextPosition is Position + 1,
		position_rows(Rows, NextPosition, PositionedRows).

	build_grid_cells([], _CellSize, Cells, Cells).
	build_grid_cells([Position-(Id-Vector)| Rows], CellSize, Cells0, Cells) :-
		grid_cell_coordinates(Vector, CellSize, Cell),
		(	avltree::lookup(Cell, CellRows0, Cells0) ->
			avltree::insert(Cells0, Cell, [Position-(Id-Vector)| CellRows0], Cells1)
		;	avltree::insert(Cells0, Cell, [Position-(Id-Vector)], Cells1)
		),
		build_grid_cells(Rows, CellSize, Cells1, Cells).

	grid_cell_coordinates([], _CellSize, []).
	grid_cell_coordinates([Value| Values], CellSize, [Index| Indices]) :-
		Index is floor(Value / CellSize),
		grid_cell_coordinates(Values, CellSize, Indices).

	range_query(metric_tree(MetricTree), Vector, Options, Epsilon, Neighbors) :-
		metric_range_query(MetricTree, Vector, Options, Epsilon, Neighbors).
	range_query(grid(CellSize, Cells), Vector, Options, Epsilon, Neighbors) :-
		grid_range_query(CellSize, Cells, Vector, Options, Epsilon, Neighbors).

	grid_range_query(CellSize, Cells, Vector, Options, Epsilon, Neighbors) :-
		grid_cell_coordinates(Vector, CellSize, Cell),
		neighboring_cells(Cell, NeighborCells),
		grid_candidate_entries(NeighborCells, Cells, CandidateEntries0),
		keysort(CandidateEntries0, CandidateEntries),
		filter_grid_candidates(CandidateEntries, Vector, Options, Epsilon, Neighbors).

	neighboring_cells([], [[]]).
	neighboring_cells([Index| Indices], NeighborCells) :-
		neighboring_cells(Indices, TailCells),
		prepend_neighbor_offsets(TailCells, Index, NeighborCells).

	prepend_neighbor_offsets([], _Index, []).
	prepend_neighbor_offsets([TailCell| TailCells], Index, NeighborCells) :-
		PreviousIndex is Index - 1,
		NextIndex is Index + 1,
		NeighborCells = [[PreviousIndex| TailCell], [Index| TailCell], [NextIndex| TailCell]| RestNeighborCells],
		prepend_neighbor_offsets(TailCells, Index, RestNeighborCells).

	grid_candidate_entries(NeighborCells, Cells, CandidateEntries) :-
		grid_candidate_entries(NeighborCells, Cells, [], CandidateEntries).

	grid_candidate_entries([], _Cells, CandidateEntries, CandidateEntries).
	grid_candidate_entries([NeighborCell| NeighborCells], Cells, CandidateEntries0, CandidateEntries) :-
		(	avltree::lookup(NeighborCell, CellEntries, Cells) ->
			append_positioned_rows(CellEntries, CandidateEntries0, CandidateEntries1)
		;	CandidateEntries1 = CandidateEntries0
		),
		grid_candidate_entries(NeighborCells, Cells, CandidateEntries1, CandidateEntries).

	append_positioned_rows([], CandidateEntries, CandidateEntries).
	append_positioned_rows([CandidateEntry| CellEntries], CandidateEntries0, CandidateEntries) :-
		append_positioned_rows(CellEntries, [CandidateEntry| CandidateEntries0], CandidateEntries).

	filter_grid_candidates(CandidateEntries, Vector, Options, Epsilon, Neighbors) :-
		filter_grid_candidates(CandidateEntries, Vector, Options, Epsilon, Neighbors, []).

	filter_grid_candidates([], _Vector, _Options, _Epsilon, Neighbors, Neighbors).
	filter_grid_candidates([_Position-(Id-NeighborVector)| CandidateEntries], Vector, Options, Epsilon, Neighbors, Tail) :-
		::distance(Options, Vector, NeighborVector, Distance),
		(	Distance =< Epsilon ->
			Neighbors = [Id-NeighborVector| NeighborTail]
		;	Neighbors = NeighborTail
		),
		filter_grid_candidates(CandidateEntries, Vector, Options, Epsilon, NeighborTail, Tail).

	metric_range_query(Tree, Vector, Options, Epsilon, Neighbors) :-
		metric_range_query(Tree, Vector, Options, Epsilon, Neighbors, []).

	metric_range_query(empty, _Vector, _Options, _Epsilon, Neighbors, Neighbors).
	metric_range_query(leaf(Rows), Vector, Options, Epsilon, Neighbors, Tail) :-
		metric_range_query_leaf(Rows, Vector, Options, Epsilon, Neighbors, Tail).
	metric_range_query(node(Id-NodeVector, InnerUpperBound, OuterLowerBound, InnerTree, OuterTree), Vector, Options, Epsilon, Neighbors, Tail) :-
		::distance(Options, Vector, NodeVector, Distance),
		current_neighbor(Id, NodeVector, Distance, Epsilon, Neighbors, NeighborTail0),
		(	search_inner_branch(Distance, Epsilon, InnerUpperBound) ->
			metric_range_query(InnerTree, Vector, Options, Epsilon, NeighborTail0, NeighborTail1)
		;	NeighborTail1 = NeighborTail0
		),
		(	search_outer_branch(Distance, Epsilon, OuterLowerBound) ->
			metric_range_query(OuterTree, Vector, Options, Epsilon, NeighborTail1, Tail)
		;	NeighborTail1 = Tail
		).

	metric_range_query_leaf([], _Vector, _Options, _Epsilon, Neighbors, Neighbors).
	metric_range_query_leaf([Id-NeighborVector| Rows], Vector, Options, Epsilon, Neighbors, Tail) :-
		::distance(Options, Vector, NeighborVector, Distance),
		(	Distance =< Epsilon ->
			Neighbors = [Id-NeighborVector| NeighborTail]
		;	Neighbors = NeighborTail
		),
		metric_range_query_leaf(Rows, Vector, Options, Epsilon, NeighborTail, Tail).

	search_inner_branch(_Distance, _Epsilon, none) :-
		fail.
	search_inner_branch(Distance, Epsilon, InnerUpperBound) :-
		Distance - Epsilon =< InnerUpperBound.

	search_outer_branch(_Distance, _Epsilon, none) :-
		fail.
	search_outer_branch(Distance, Epsilon, OuterLowerBound) :-
		Distance + Epsilon >= OuterLowerBound.

	current_neighbor(Id, Vector, Distance, Epsilon, [Id-Vector| Tail], Tail) :-
		Distance =< Epsilon,
		!.
	current_neighbor(_Id, _Vector, _Distance, _Epsilon, Tail, Tail).

	split_sorted_rows(SortedRows, InnerUpperBound, OuterLowerBound, InnerRows, OuterRows) :-
		length(SortedRows, Count),
		InnerCount is Count // 2,
		take_first_n_decorated(InnerCount, SortedRows, InnerDecoratedRows, OuterDecoratedRows),
		undecorate_rows(InnerDecoratedRows, InnerRows),
		undecorate_rows(OuterDecoratedRows, OuterRows),
		upper_bound_distance(InnerDecoratedRows, InnerUpperBound),
		lower_bound_distance(OuterDecoratedRows, OuterLowerBound).

	take_first_n_decorated(0, Rows, [], Rows) :-
		!.
	take_first_n_decorated(N, [Row| Rows], [Row| SelectedRows], RemainingRows) :-
		N > 0,
		NextN is N - 1,
		take_first_n_decorated(NextN, Rows, SelectedRows, RemainingRows).

	undecorate_rows([], []).
	undecorate_rows([_-Row| DecoratedRows], [Row| Rows]) :-
		undecorate_rows(DecoratedRows, Rows).

	upper_bound_distance([], none).
	upper_bound_distance([Distance-_| DecoratedRows], UpperBound) :-
		upper_bound_distance(DecoratedRows, Distance, UpperBound).

	upper_bound_distance([], UpperBound, UpperBound).
	upper_bound_distance([Distance-_| DecoratedRows], _CurrentUpperBound, UpperBound) :-
		upper_bound_distance(DecoratedRows, Distance, UpperBound).

	lower_bound_distance([], none).
	lower_bound_distance([Distance-_| _], Distance).

:- end_category.
