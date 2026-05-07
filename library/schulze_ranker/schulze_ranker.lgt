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


:- object(schulze_ranker,
	imports([ranking_dataset_common, score_ranker_model_common, condorcet_victory_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Schulze pairwise preference ranker. Learns one deterministic score per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by computing strongest paths over aggregated head-to-head outcomes and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, copeland_ranker, borda_ranker]
	]).

	:- public(strongest_paths/2).
	:- mode(strongest_paths(+compound, -list(compound)), one).
	:- info(strongest_paths/2, [
		comment is 'Returns the labeled strongest-path relation as ``path(Item1,Item2,Strength)`` terms for all ordered pairs of distinct learned items.',
		argnames is ['Ranker', 'StrongestPaths']
	]).

	:- uses(avltree, [
		as_dictionary/2
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_matchups(Dataset, Matchups),
		length(Items, Count),
		^^index_items(Items, 1, IndexPairs),
		as_dictionary(IndexPairs, IndexDictionary),
		^^option(victory_strength(VictoryStrength), Options),
		^^build_direct_strengths(Matchups, IndexDictionary, Count, VictoryStrength, DirectStrengths),
		compute_strongest_paths(DirectStrengths, StrongestPathMatrix),
		schulze_scores(Count, StrongestPathMatrix, ScoreValues),
		score_pairs(Items, ScoreValues, Scores),
		pairwise_strongest_paths(Items, StrongestPathMatrix, StrongestPaths),
		Ranker = schulze_ranker(Items, Scores, [
			model(schulze_ranker),
			options(Options),
			strongest_paths(StrongestPaths),
			dataset_summary(DatasetSummary)
		]).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	strongest_paths(Ranker, StrongestPaths) :-
		^^score_ranker_data(Ranker, _Items, _Scores, Diagnostics),
		memberchk(strongest_paths(StrongestPaths), Diagnostics).

	valid_score_ranker_diagnostics(Items, _Scores, Diagnostics) :-
		^^valid_ranker_metadata(schulze_ranker, Diagnostics),
		memberchk(strongest_paths(StrongestPaths), Diagnostics),
		valid_strongest_paths(Items, StrongestPaths).

	valid_strongest_paths(_Items, []).
	valid_strongest_paths(Items, [path(Item1, Item2, Strength)| StrongestPaths]) :-
		memberchk(Item1, Items),
		memberchk(Item2, Items),
		Item1 \== Item2,
		number(Strength),
		Strength >= 0,
		valid_strongest_paths(Items, StrongestPaths).

	compute_strongest_paths(DirectStrengths, StrongestPaths) :-
		length(DirectStrengths, Count),
		compute_strongest_paths(1, Count, DirectStrengths, StrongestPaths).

	compute_strongest_paths(Index, Count, StrongestPaths, StrongestPaths) :-
		Index > Count,
		!.
	compute_strongest_paths(Index, Count, StrongestPaths0, StrongestPaths) :-
		update_paths_for_pivot(Index, StrongestPaths0, StrongestPaths1),
		NextIndex is Index + 1,
		compute_strongest_paths(NextIndex, Count, StrongestPaths1, StrongestPaths).

	update_paths_for_pivot(Pivot, OriginalPaths, UpdatedPaths) :-
		length(OriginalPaths, Count),
		update_paths_for_pivot(1, Count, Pivot, OriginalPaths, UpdatedPaths).

	update_paths_for_pivot(RowIndex, Count, _Pivot, _OriginalPaths, []) :-
		RowIndex > Count,
		!.
	update_paths_for_pivot(RowIndex, Count, Pivot, OriginalPaths, [UpdatedRow| UpdatedPaths]) :-
		update_row_for_pivot(RowIndex, 1, Count, Pivot, OriginalPaths, UpdatedRow),
		NextRowIndex is RowIndex + 1,
		update_paths_for_pivot(NextRowIndex, Count, Pivot, OriginalPaths, UpdatedPaths).

	update_row_for_pivot(_RowIndex, ColumnIndex, Count, _Pivot, _OriginalPaths, []) :-
		ColumnIndex > Count,
		!.
	update_row_for_pivot(RowIndex, ColumnIndex, Count, Pivot, OriginalPaths, [Value| Values]) :-
		updated_strength_value(RowIndex, ColumnIndex, Pivot, OriginalPaths, Value),
		NextColumnIndex is ColumnIndex + 1,
		update_row_for_pivot(RowIndex, NextColumnIndex, Count, Pivot, OriginalPaths, Values).

	updated_strength_value(RowIndex, ColumnIndex, _Pivot, OriginalPaths, Value) :-
		RowIndex =:= ColumnIndex,
		!,
		^^matrix_entry(OriginalPaths, RowIndex, ColumnIndex, Value).
	updated_strength_value(RowIndex, ColumnIndex, Pivot, OriginalPaths, Value) :-
		RowIndex =:= Pivot,
		!,
		^^matrix_entry(OriginalPaths, RowIndex, ColumnIndex, Value).
	updated_strength_value(RowIndex, ColumnIndex, Pivot, OriginalPaths, Value) :-
		ColumnIndex =:= Pivot,
		!,
		^^matrix_entry(OriginalPaths, RowIndex, ColumnIndex, Value).
	updated_strength_value(RowIndex, ColumnIndex, Pivot, OriginalPaths, Value) :-
		^^matrix_entry(OriginalPaths, RowIndex, ColumnIndex, CurrentStrength),
		^^matrix_entry(OriginalPaths, RowIndex, Pivot, LeftStrength),
		^^matrix_entry(OriginalPaths, Pivot, ColumnIndex, RightStrength),
		(   LeftStrength =< RightStrength ->
			CandidateStrength = LeftStrength
		;   CandidateStrength = RightStrength
		),
		(   CandidateStrength > CurrentStrength ->
			Value = CandidateStrength
		;   Value = CurrentStrength
		).

	schulze_scores(Count, StrongestPaths, Scores) :-
		schulze_scores(1, Count, StrongestPaths, Scores).

	schulze_scores(Index, Count, _StrongestPaths, []) :-
		Index > Count,
		!.
	schulze_scores(Index, Count, StrongestPaths, [Score| Scores]) :-
		schulze_score(Index, 1, Count, StrongestPaths, 0, Score),
		NextIndex is Index + 1,
		schulze_scores(NextIndex, Count, StrongestPaths, Scores).

	schulze_score(_Index, OpponentIndex, Count, _StrongestPaths, Score, Score) :-
		OpponentIndex > Count,
		!.
	schulze_score(Index, OpponentIndex, Count, StrongestPaths, Score0, Score) :-
		(   Index =:= OpponentIndex ->
			Score1 = Score0
		;   ^^matrix_entry(StrongestPaths, Index, OpponentIndex, ForwardStrength),
			^^matrix_entry(StrongestPaths, OpponentIndex, Index, ReverseStrength),
			(   ForwardStrength > ReverseStrength ->
				Score1 is Score0 + 1
			;   Score1 = Score0
			)
		),
		NextOpponentIndex is OpponentIndex + 1,
		schulze_score(Index, NextOpponentIndex, Count, StrongestPaths, Score1, Score).

	score_pairs([], [], []).
	score_pairs([Item| Items], [Score| ScoreValues], [Item-Score| Scores]) :-
		score_pairs(Items, ScoreValues, Scores).

	pairwise_strongest_paths(Items, Matrix, StrongestPaths) :-
		pairwise_strongest_paths(Items, Items, Matrix, StrongestPaths-[]).

	pairwise_strongest_paths([], _AllItems, [], StrongestPaths-StrongestPaths).
	pairwise_strongest_paths([Item| Items], AllItems, [Row| Matrix], StrongestPaths0-StrongestPaths) :-
		row_strongest_paths(AllItems, Item, Row, StrongestPaths0-StrongestPaths1),
		pairwise_strongest_paths(Items, AllItems, Matrix, StrongestPaths1-StrongestPaths).

	row_strongest_paths([], _Item, [], StrongestPaths-StrongestPaths).
	row_strongest_paths([OtherItem| OtherItems], Item, [Strength| Strengths], StrongestPaths0-StrongestPaths) :-
		(   Item == OtherItem ->
			StrongestPaths0 = StrongestPaths1
		;   StrongestPaths0 = [path(Item, OtherItem, Strength)| StrongestPaths1]
		),
		row_strongest_paths(OtherItems, Item, Strengths, StrongestPaths1-StrongestPaths).

	score_ranker_model(schulze_ranker).

	score_ranker_label('Schulze').

	score_ranker_term(Items, Scores, Diagnostics, schulze_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		integer(Score),
		Score >= 0.

	valid_option(victory_strength(Value)) :-
		once((Value == winning_votes; Value == margins)).

	default_option(victory_strength(winning_votes)).

:- end_object.
