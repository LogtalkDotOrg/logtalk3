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


:- object(kemeny_young,
	imports([ranking_dataset_common, score_ranker_model_common, condorcet_victory_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Kemeny-Young pairwise preference ranker. Learns one deterministic consensus order from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by maximizing the total agreeing pairwise preference weight over all linear orders and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, ranked_pairs, schulze]
	]).

	:- public(consensus_ranking/2).
	:- mode(consensus_ranking(+compound, -list), one).
	:- info(consensus_ranking/2, [
		comment is 'Returns the learned full-item consensus ranking selected by the Kemeny-Young optimization.',
		argnames is ['Ranker', 'ConsensusRanking']
	]).

	:- public(consensus_score/2).
	:- mode(consensus_score(+compound, -number), one).
	:- info(consensus_score/2, [
		comment is 'Returns the maximum total pairwise agreement score of the learned consensus ranking.',
		argnames is ['Ranker', 'ConsensusScore']
	]).

	:- uses(avltree, [
		as_dictionary/2, insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3,
		new/1 as dictionary_new/1
	]).

	:- uses(list, [
		length/2, memberchk/2, reverse/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		(   Items = [Item] ->
			singleton_ranker(Item, Options, DatasetSummary, Ranker)
		;   ^^pairwise_dataset_matchups(Dataset, Matchups),
			length(Items, Count),
			^^index_items(Items, 1, IndexPairs),
			as_dictionary(IndexPairs, IndexDictionary),
			build_preference_matrix(Matchups, IndexDictionary, Count, PreferenceMatrix),
			^^option(tie_breaking(TieBreaking), Options),
			best_consensus_ranking(Items, IndexDictionary, PreferenceMatrix, TieBreaking, ConsensusRanking, ConsensusScore),
			scores_from_consensus(Items, ConsensusRanking, Scores),
			Ranker = kemeny_young_ranker(Items, Scores, [
				model(kemeny_young),
				options(Options),
				consensus_ranking(ConsensusRanking),
				consensus_score(ConsensusScore),
				dataset_summary(DatasetSummary)
			])
		).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	consensus_ranking(Ranker, ConsensusRanking) :-
		^^score_ranker_data(Ranker, _Items, _Scores, Diagnostics),
		memberchk(consensus_ranking(ConsensusRanking), Diagnostics).

	consensus_score(Ranker, ConsensusScore) :-
		^^score_ranker_data(Ranker, _Items, _Scores, Diagnostics),
		memberchk(consensus_score(ConsensusScore), Diagnostics).

	valid_score_ranker_diagnostics(Items, _Scores, Diagnostics) :-
		^^valid_ranker_metadata(kemeny_young, Diagnostics),
		memberchk(consensus_ranking(ConsensusRanking), Diagnostics),
		valid_item_permutation(Items, ConsensusRanking),
		memberchk(consensus_score(ConsensusScore), Diagnostics),
		number(ConsensusScore),
		ConsensusScore >= 0.

	valid_item_permutation([], []).
	valid_item_permutation([Item| Items], Ranking0) :-
		select_ordered(Item, Ranking0, Ranking),
		valid_item_permutation(Items, Ranking).

	singleton_ranker(Item, Options, DatasetSummary, kemeny_young_ranker([Item], [Item-0], [
		model(kemeny_young),
		options(Options),
		consensus_ranking([Item]),
		consensus_score(0),
		dataset_summary(DatasetSummary)
	])).

	build_preference_matrix(Matchups, IndexDictionary, Count, PreferenceMatrix) :-
		^^zero_matrix(Count, PreferenceMatrix0),
		accumulate_preference_matrix(Matchups, IndexDictionary, PreferenceMatrix0, PreferenceMatrix).

	accumulate_preference_matrix([], _IndexDictionary, PreferenceMatrix, PreferenceMatrix).
	accumulate_preference_matrix([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], IndexDictionary, PreferenceMatrix0, PreferenceMatrix) :-
		dictionary_lookup(Item1, Index1, IndexDictionary),
		dictionary_lookup(Item2, Index2, IndexDictionary),
		^^set_matrix_entry(PreferenceMatrix0, Index1, Index2, Item1Wins, PreferenceMatrix1),
		^^set_matrix_entry(PreferenceMatrix1, Index2, Index1, Item2Wins, PreferenceMatrix2),
		accumulate_preference_matrix(Matchups, IndexDictionary, PreferenceMatrix2, PreferenceMatrix).

	best_consensus_ranking(Items, IndexDictionary, PreferenceMatrix, TieBreaking, ConsensusRanking, ConsensusScore) :-
		ordered_items(TieBreaking, Items, SearchItems),
		search_best_consensus(SearchItems, [], 0, IndexDictionary, PreferenceMatrix, -1, [], ConsensusScore, ConsensusRanking).

	ordered_items(declaration_order, Items, Items).
	ordered_items(term_order, Items, OrderedItems) :-
		term_sorted_items(Items, OrderedItems).

	term_sorted_items([], []).
	term_sorted_items([Item| Items], SortedItems) :-
		term_sorted_items(Items, SortedItems0),
		insert_term_sorted(SortedItems0, Item, SortedItems).

	insert_term_sorted([], Item, [Item]).
	insert_term_sorted([Other| Items], Item, [Item, Other| Items]) :-
		Item @< Other,
		!.
	insert_term_sorted([Other| Items], Item, [Other| SortedItems]) :-
		insert_term_sorted(Items, Item, SortedItems).

	search_best_consensus(Remaining, PrefixRev, CurrentScore, IndexDictionary, PreferenceMatrix, BestScore0, BestRanking0, BestScore, BestRanking) :-
		upper_bound(PrefixRev, Remaining, CurrentScore, IndexDictionary, PreferenceMatrix, UpperBound),
		(   UpperBound =< BestScore0 ->
			BestScore = BestScore0,
			BestRanking = BestRanking0
		;   Remaining == [] ->
			reverse(PrefixRev, Ranking),
			(   CurrentScore > BestScore0 ->
				BestScore = CurrentScore,
				BestRanking = Ranking
			;   BestScore = BestScore0,
				BestRanking = BestRanking0
			)
		;   explore_consensus_candidates(Remaining, Remaining, PrefixRev, CurrentScore, IndexDictionary, PreferenceMatrix, BestScore0, BestRanking0, BestScore, BestRanking)
		).

	explore_consensus_candidates([], _Remaining, _PrefixRev, _CurrentScore, _IndexDictionary, _PreferenceMatrix, BestScore, BestRanking, BestScore, BestRanking).
	explore_consensus_candidates([Candidate| Candidates], Remaining, PrefixRev, CurrentScore, IndexDictionary, PreferenceMatrix, BestScore0, BestRanking0, BestScore, BestRanking) :-
		select_ordered(Candidate, Remaining, NextRemaining),
		prefix_contribution(PrefixRev, Candidate, IndexDictionary, PreferenceMatrix, 0, Increment),
		NextScore is CurrentScore + Increment,
		search_best_consensus(NextRemaining, [Candidate| PrefixRev], NextScore, IndexDictionary, PreferenceMatrix, BestScore0, BestRanking0, BestScore1, BestRanking1),
		explore_consensus_candidates(Candidates, Remaining, PrefixRev, CurrentScore, IndexDictionary, PreferenceMatrix, BestScore1, BestRanking1, BestScore, BestRanking).

	select_ordered(Item, [Item| Items], Items) :-
		!.
	select_ordered(Item, [Other| Items], [Other| Remaining]) :-
		select_ordered(Item, Items, Remaining).

	upper_bound(PrefixRev, Remaining, CurrentScore, IndexDictionary, PreferenceMatrix, UpperBound) :-
		prefix_remaining_contribution(PrefixRev, Remaining, IndexDictionary, PreferenceMatrix, 0, PrefixRemainingContribution),
		remaining_pair_upper_bound(Remaining, IndexDictionary, PreferenceMatrix, 0, RemainingPairBound),
		UpperBound is CurrentScore + PrefixRemainingContribution + RemainingPairBound.

	prefix_remaining_contribution([], _Remaining, _IndexDictionary, _PreferenceMatrix, Contribution, Contribution).
	prefix_remaining_contribution([Item| PrefixRev], Remaining, IndexDictionary, PreferenceMatrix, Contribution0, Contribution) :-
		item_remaining_contribution(Remaining, Item, IndexDictionary, PreferenceMatrix, 0, ItemContribution),
		Contribution1 is Contribution0 + ItemContribution,
		prefix_remaining_contribution(PrefixRev, Remaining, IndexDictionary, PreferenceMatrix, Contribution1, Contribution).

	item_remaining_contribution([], _Item, _IndexDictionary, _PreferenceMatrix, Contribution, Contribution).
	item_remaining_contribution([Other| Remaining], Item, IndexDictionary, PreferenceMatrix, Contribution0, Contribution) :-
		preference_weight(Item, Other, IndexDictionary, PreferenceMatrix, Weight),
		Contribution1 is Contribution0 + Weight,
		item_remaining_contribution(Remaining, Item, IndexDictionary, PreferenceMatrix, Contribution1, Contribution).

	remaining_pair_upper_bound([], _IndexDictionary, _PreferenceMatrix, Bound, Bound).
	remaining_pair_upper_bound([Item| Items], IndexDictionary, PreferenceMatrix, Bound0, Bound) :-
		item_pair_upper_bound(Items, Item, IndexDictionary, PreferenceMatrix, 0, ItemBound),
		Bound1 is Bound0 + ItemBound,
		remaining_pair_upper_bound(Items, IndexDictionary, PreferenceMatrix, Bound1, Bound).

	item_pair_upper_bound([], _Item, _IndexDictionary, _PreferenceMatrix, Bound, Bound).
	item_pair_upper_bound([Other| Items], Item, IndexDictionary, PreferenceMatrix, Bound0, Bound) :-
		preference_weight(Item, Other, IndexDictionary, PreferenceMatrix, WeightForward),
		preference_weight(Other, Item, IndexDictionary, PreferenceMatrix, WeightBackward),
		Bound1 is Bound0 + max(WeightForward, WeightBackward),
		item_pair_upper_bound(Items, Item, IndexDictionary, PreferenceMatrix, Bound1, Bound).

	prefix_contribution([], _Candidate, _IndexDictionary, _PreferenceMatrix, Contribution, Contribution).
	prefix_contribution([Item| PrefixRev], Candidate, IndexDictionary, PreferenceMatrix, Contribution0, Contribution) :-
		preference_weight(Item, Candidate, IndexDictionary, PreferenceMatrix, Weight),
		Contribution1 is Contribution0 + Weight,
		prefix_contribution(PrefixRev, Candidate, IndexDictionary, PreferenceMatrix, Contribution1, Contribution).

	preference_weight(Item1, Item2, IndexDictionary, PreferenceMatrix, Weight) :-
		dictionary_lookup(Item1, Index1, IndexDictionary),
		dictionary_lookup(Item2, Index2, IndexDictionary),
		^^matrix_entry(PreferenceMatrix, Index1, Index2, Weight).

	scores_from_consensus(Items, ConsensusRanking, Scores) :-
		length(ConsensusRanking, Count),
		HighestScore is Count - 1,
		dictionary_new(ScoreDictionary0),
		score_dictionary(ConsensusRanking, HighestScore, ScoreDictionary0, ScoreDictionary),
		^^ordered_scores(Items, ScoreDictionary, Scores).

	score_dictionary([], _Score, ScoreDictionary, ScoreDictionary).
	score_dictionary([Item| Items], Score, ScoreDictionary0, ScoreDictionary) :-
		dictionary_insert(ScoreDictionary0, Item, Score, ScoreDictionary1),
		NextScore is Score - 1,
		score_dictionary(Items, NextScore, ScoreDictionary1, ScoreDictionary).

	score_ranker_model(kemeny_young).

	score_ranker_label('Kemeny-Young').

	score_ranker_term(Items, Scores, Diagnostics, kemeny_young_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		integer(Score),
		Score >= 0.

	valid_option(tie_breaking(Value)) :-
		once((Value == term_order; Value == declaration_order)).

	default_option(tie_breaking(term_order)).

:- end_object.
