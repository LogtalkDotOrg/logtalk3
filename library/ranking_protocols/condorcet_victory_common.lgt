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


:- category(condorcet_victory_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Shared direct-victory preprocessing helpers for Condorcet-family rankers.'
	]).

	:- protected(index_items/3).
	:- mode(index_items(+list, +integer, -list), one).
	:- info(index_items/3, [
		comment is 'Builds ``Item-Index`` pairs for the ordered training items.',
		argnames is ['Items', 'Index', 'Indices']
	]).

	:- protected(build_direct_strengths/5).
	:- mode(build_direct_strengths(+list, +compound, +integer, +atom, -list), one).
	:- info(build_direct_strengths/5, [
		comment is 'Builds the dense direct-victory strength matrix from aggregated pairwise matchups using the selected victory-strength semantics.',
		argnames is ['Matchups', 'IndexDictionary', 'Count', 'VictoryStrength', 'DirectStrengths']
	]).

	:- protected(zero_matrix/2).
	:- mode(zero_matrix(+integer, -list), one).
	:- info(zero_matrix/2, [
		comment is 'Builds a square zero matrix with the given row and column count.',
		argnames is ['Count', 'Matrix']
	]).

	:- protected(matrix_entry/4).
	:- mode(matrix_entry(+list, +integer, +integer, -integer), one).
	:- info(matrix_entry/4, [
		comment is 'Returns the value stored at the given matrix row and column.',
		argnames is ['Matrix', 'RowIndex', 'ColumnIndex', 'Value']
	]).

	:- protected(set_matrix_entry/5).
	:- mode(set_matrix_entry(+list, +integer, +integer, +integer, -list), one).
	:- info(set_matrix_entry/5, [
		comment is 'Returns an updated matrix where the given row and column entry was replaced by the supplied value.',
		argnames is ['Matrix', 'RowIndex', 'ColumnIndex', 'Value', 'UpdatedMatrix']
	]).

	:- uses(avltree, [
		lookup/3 as dictionary_lookup/3
	]).

	:- uses(list, [
		nth1/3
	]).

	index_items([], _Index, []).
	index_items([Item| Items], Index, [Item-Index| Indices]) :-
		NextIndex is Index + 1,
		index_items(Items, NextIndex, Indices).

	build_direct_strengths(Matchups, IndexDictionary, Count, VictoryStrength, DirectStrengths) :-
		zero_matrix(Count, DirectStrengths0),
		accumulate_direct_strengths(Matchups, IndexDictionary, VictoryStrength, DirectStrengths0, DirectStrengths).

	accumulate_direct_strengths([], _IndexDictionary, _VictoryStrength, DirectStrengths, DirectStrengths).
	accumulate_direct_strengths([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], IndexDictionary, VictoryStrength, DirectStrengths0, DirectStrengths) :-
		dictionary_lookup(Item1, Index1, IndexDictionary),
		dictionary_lookup(Item2, Index2, IndexDictionary),
		direct_strengths(Item1Wins, Item2Wins, VictoryStrength, Strength12, Strength21),
		set_matrix_entry(DirectStrengths0, Index1, Index2, Strength12, DirectStrengths1),
		set_matrix_entry(DirectStrengths1, Index2, Index1, Strength21, DirectStrengths2),
		accumulate_direct_strengths(Matchups, IndexDictionary, VictoryStrength, DirectStrengths2, DirectStrengths).

	direct_strengths(Item1Wins, Item2Wins, winning_votes, Strength12, Strength21) :-
		!,
		(	Item1Wins > Item2Wins ->
			Strength12 = Item1Wins,
			Strength21 = 0
		;	Item2Wins > Item1Wins ->
			Strength12 = 0,
			Strength21 = Item2Wins
		;	Strength12 = 0,
			Strength21 = 0
		).
	direct_strengths(Item1Wins, Item2Wins, margins, Strength12, Strength21) :-
		(	Item1Wins > Item2Wins ->
			Strength12 is Item1Wins - Item2Wins,
			Strength21 = 0
		;	Item2Wins > Item1Wins ->
			Strength12 = 0,
			Strength21 is Item2Wins - Item1Wins
		;	Strength12 = 0,
			Strength21 = 0
		).

	zero_matrix(Count, Matrix) :-
		zero_matrix(Count, Count, Matrix).

	zero_matrix(0, _Width, []) :-
		!.
	zero_matrix(RemainingRows, Width, [Row| Matrix]) :-
		RemainingRows > 0,
		zero_row(Width, Row),
		NextRemainingRows is RemainingRows - 1,
		zero_matrix(NextRemainingRows, Width, Matrix).

	zero_row(0, []) :-
		!.
	zero_row(Count, [0| Row]) :-
		Count > 0,
		NextCount is Count - 1,
		zero_row(NextCount, Row).

	matrix_entry(Matrix, RowIndex, ColumnIndex, Value) :-
		nth1(RowIndex, Matrix, Row),
		nth1(ColumnIndex, Row, Value).

	set_matrix_entry([Row| Matrix], 1, ColumnIndex, Value, [UpdatedRow| Matrix]) :-
		!,
		set_row_entry(Row, ColumnIndex, Value, UpdatedRow).
	set_matrix_entry([Row| Matrix], RowIndex, ColumnIndex, Value, [Row| UpdatedMatrix]) :-
		RowIndex > 1,
		NextRowIndex is RowIndex - 1,
		set_matrix_entry(Matrix, NextRowIndex, ColumnIndex, Value, UpdatedMatrix).

	set_row_entry([_Entry| Row], 1, Value, [Value| Row]) :-
		!.
	set_row_entry([Entry| Row], ColumnIndex, Value, [Entry| UpdatedRow]) :-
		ColumnIndex > 1,
		NextColumnIndex is ColumnIndex - 1,
		set_row_entry(Row, NextColumnIndex, Value, UpdatedRow).

:- end_category.
