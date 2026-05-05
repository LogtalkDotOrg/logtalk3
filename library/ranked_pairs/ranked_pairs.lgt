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


:- object(ranked_pairs,
	imports([ranking_dataset_common, score_ranker_model_common, condorcet_victory_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Ranked Pairs pairwise preference ranker. Learns one deterministic score per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by locking direct pairwise victories in descending strength order while avoiding cycles and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, schulze, copeland]
	]).

	:- public(locked_pairs/2).
	:- mode(locked_pairs(+compound, -list(compound)), one).
	:- info(locked_pairs/2, [
		comment is 'Returns the accepted lock graph in lock order as ``lock(Item1,Item2,Strength)`` terms for the learned items.',
		argnames is ['Ranker', 'LockedPairs']
	]).

	:- uses(avltree, [
		as_dictionary/2
	]).

	:- uses(list, [
		length/2, memberchk/2, nth1/3, reverse/2, sort/4
	]).

	:- uses(numberlist, [
		sum/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_matchups(Dataset, Matchups),
		^^index_items(Items, 1, IndexPairs),
		as_dictionary(IndexPairs, IndexDictionary),
		length(Items, Count),
		^^option(victory_strength(VictoryStrength), Options),
		^^option(tie_breaking(TieBreaking), Options),
		^^build_direct_strengths(Matchups, IndexDictionary, Count, VictoryStrength, DirectStrengths),
		direct_victories(IndexPairs, DirectStrengths, TieBreaking, Victories),
		lock_victories(Victories, Count, LockedPairs, LockedClosure),
		locked_scores(LockedClosure, ScoreValues),
		score_pairs(Items, ScoreValues, Scores),
		Ranker = ranked_pairs_ranker(Items, Scores, [
			model(ranked_pairs),
			options(Options),
			locked_pairs(LockedPairs),
			dataset_summary(DatasetSummary)
		]).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	locked_pairs(Ranker, LockedPairs) :-
		^^score_ranker_data(Ranker, _Items, _Scores, Diagnostics),
		memberchk(locked_pairs(LockedPairs), Diagnostics).

	valid_score_ranker_diagnostics(Items, _Scores, Diagnostics) :-
		^^valid_ranker_metadata(ranked_pairs, Diagnostics),
		memberchk(locked_pairs(LockedPairs), Diagnostics),
		valid_locked_pairs(Items, LockedPairs).

	valid_locked_pairs(_Items, []).
	valid_locked_pairs(Items, [lock(Item1, Item2, Strength)| LockedPairs]) :-
		memberchk(Item1, Items),
		memberchk(Item2, Items),
		Item1 \== Item2,
		number(Strength),
		Strength > 0,
		valid_locked_pairs(Items, LockedPairs).

	direct_victories(IndexPairs, DirectStrengths, TieBreaking, Victories) :-
		direct_victories(IndexPairs, IndexPairs, DirectStrengths, TieBreaking, 1, VictoryPairs-[], _),
		sort(1, @=<, VictoryPairs, SortedPairs),
		pair_values(SortedPairs, Victories).

	direct_victories([], _AllIndexPairs, [], _TieBreaking, Sequence, VictoryPairs-VictoryPairs, Sequence).
	direct_victories([Winner-WinnerIndex| IndexPairs], AllIndexPairs, [Row| Rows], TieBreaking, Sequence0, VictoryPairs0-VictoryPairs, Sequence) :-
		row_victories(AllIndexPairs, Winner, WinnerIndex, Row, TieBreaking, Sequence0, VictoryPairs0-VictoryPairs1, Sequence1),
		direct_victories(IndexPairs, AllIndexPairs, Rows, TieBreaking, Sequence1, VictoryPairs1-VictoryPairs, Sequence).

	row_victories([], _Winner, _WinnerIndex, [], _TieBreaking, Sequence, VictoryPairs-VictoryPairs, Sequence).
	row_victories([Loser-LoserIndex| IndexPairs], Winner, WinnerIndex, [Strength| Strengths], TieBreaking, Sequence0, VictoryPairs0-VictoryPairs, Sequence) :-
		(   WinnerIndex =:= LoserIndex ->
			VictoryPairs0 = VictoryPairs1,
			Sequence1 = Sequence0
		;   Strength =:= 0 ->
			VictoryPairs0 = VictoryPairs1,
			Sequence1 = Sequence0
		;   victory_key(TieBreaking, Strength, Winner, Loser, Sequence0, Key),
			VictoryPairs0 = [Key-victory(WinnerIndex, LoserIndex, Winner, Loser, Strength)| VictoryPairs1],
			Sequence1 is Sequence0 + 1
		),
		row_victories(IndexPairs, Winner, WinnerIndex, Strengths, TieBreaking, Sequence1, VictoryPairs1-VictoryPairs, Sequence).

	victory_key(term_order, Strength, Winner, Loser, _Sequence, pair(NegStrength, Winner, Loser)) :-
		NegStrength is -Strength.
	victory_key(declaration_order, Strength, _Winner, _Loser, Sequence, pair(NegStrength, Sequence)) :-
		NegStrength is -Strength.

	pair_values([], []).
	pair_values([_Key-Value| Pairs], [Value| Values]) :-
		pair_values(Pairs, Values).

	lock_victories(Victories, Count, LockedPairs, LockedClosure) :-
		^^zero_matrix(Count, LockedClosure0),
		lock_victories(Victories, LockedClosure0, LockedClosure, [], LockedPairs0),
		reverse(LockedPairs0, LockedPairs).

	lock_victories([], LockedClosure, LockedClosure, LockedPairs, LockedPairs).
	lock_victories([victory(WinnerIndex, LoserIndex, Winner, Loser, Strength)| Victories], LockedClosure0, LockedClosure, LockedPairs0, LockedPairs) :-
		(   introduces_cycle(WinnerIndex, LoserIndex, LockedClosure0) ->
			LockedClosure1 = LockedClosure0,
			LockedPairs1 = LockedPairs0
		;   add_locked_edge(WinnerIndex, LoserIndex, LockedClosure0, LockedClosure1),
			LockedPairs1 = [lock(Winner, Loser, Strength)| LockedPairs0]
		),
		lock_victories(Victories, LockedClosure1, LockedClosure, LockedPairs1, LockedPairs).

	introduces_cycle(WinnerIndex, LoserIndex, LockedClosure) :-
		^^matrix_entry(LockedClosure, LoserIndex, WinnerIndex, 1).

	add_locked_edge(WinnerIndex, LoserIndex, LockedClosure0, LockedClosure) :-
		predecessor_flags(LockedClosure0, WinnerIndex, PredecessorFlags),
		successor_flags(LockedClosure0, LoserIndex, SuccessorFlags),
		update_closure(LockedClosure0, PredecessorFlags, SuccessorFlags, LockedClosure).

	predecessor_flags(LockedClosure, WinnerIndex, PredecessorFlags) :-
		predecessor_flags(LockedClosure, 1, WinnerIndex, PredecessorFlags).

	predecessor_flags([], _CurrentIndex, _WinnerIndex, []).
	predecessor_flags([Row| Rows], CurrentIndex, WinnerIndex, [Flag| Flags]) :-
		(   CurrentIndex =:= WinnerIndex ->
			Flag = 1
		;   ^^matrix_entry([Row], 1, WinnerIndex, Flag)
		),
		NextIndex is CurrentIndex + 1,
		predecessor_flags(Rows, NextIndex, WinnerIndex, Flags).

	successor_flags(LockedClosure, LoserIndex, SuccessorFlags) :-
		nth1(LoserIndex, LockedClosure, LoserRow),
		successor_flags(LoserRow, 1, LoserIndex, SuccessorFlags).

	successor_flags([], _CurrentIndex, _LoserIndex, []).
	successor_flags([Value| Values], CurrentIndex, LoserIndex, [Flag| Flags]) :-
		(   CurrentIndex =:= LoserIndex ->
			Flag = 1
		;   Flag = Value
		),
		NextIndex is CurrentIndex + 1,
		successor_flags(Values, NextIndex, LoserIndex, Flags).

	update_closure([], [], _SuccessorFlags, []).
	update_closure([Row| Rows], [PredecessorFlag| PredecessorFlags], SuccessorFlags, [UpdatedRow| UpdatedRows]) :-
		(   PredecessorFlag =:= 1 ->
			update_closure_row(Row, SuccessorFlags, UpdatedRow)
		;   UpdatedRow = Row
		),
		update_closure(Rows, PredecessorFlags, SuccessorFlags, UpdatedRows).

	update_closure_row([], [], []).
	update_closure_row([Value| Values], [SuccessorFlag| SuccessorFlags], [UpdatedValue| UpdatedValues]) :-
		(   SuccessorFlag =:= 1 ->
			UpdatedValue = 1
		;   UpdatedValue = Value
		),
		update_closure_row(Values, SuccessorFlags, UpdatedValues).

	locked_scores([], []).
	locked_scores([Row| Rows], [Score| Scores]) :-
		sum(Row, Score),
		locked_scores(Rows, Scores).

	score_pairs([], [], []).
	score_pairs([Item| Items], [Score| ScoreValues], [Item-Score| Scores]) :-
		score_pairs(Items, ScoreValues, Scores).

	score_ranker_model(ranked_pairs).

	score_ranker_label('Ranked Pairs').

	score_ranker_term(Items, Scores, Diagnostics, ranked_pairs_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		integer(Score),
		Score >= 0.

	valid_option(victory_strength(Value)) :-
		once((Value == winning_votes; Value == margins)).
	valid_option(tie_breaking(Value)) :-
		once((Value == term_order; Value == declaration_order)).

	default_option(victory_strength(winning_votes)).
	default_option(tie_breaking(term_order)).

:- end_object.
