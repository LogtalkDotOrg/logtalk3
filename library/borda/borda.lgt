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


:- object(borda,
	imports([ranking_dataset_common, score_ranker_model_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Borda grouped-ranking ranker. Learns one deterministic score per item from a dataset object implementing the ``ranking_dataset_protocol`` protocol by summing per-group Borda points and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		remarks is [
			'Algorithm' - 'Borda ranks each item by summing, across groups, the number of same-group items with strictly lower relevance.',
			'Score semantics' - 'Within each group, an item receives one point for every same-group item with strictly lower relevance when using ``tie_scoring(standard)`` and the average of the minimum and maximum tied positions when using ``tie_scoring(fractional)``.',
			'Missing relevance semantics' - 'Missing relevance facts are treated as zero by default using the ``missing_relevance(zero)`` option and can be rejected using ``missing_relevance(error)``.',
			'Tie-breaking' - 'Ranking ties are broken deterministically using the standard term order of the item identifiers after sorting by descending score.',
			'Dataset requirements' - 'The training dataset must declare each group once, use only declared groups and items in relevance judgments, and assign non-negative integer relevance values.',
			'Ranker representation' - 'The learned ranker is represented by default as ``borda_ranker(Items, Scores, Diagnostics)`` where ``Scores`` stores ``Item-Score`` pairs and ``Diagnostics`` stores metadata such as the training dataset summary.'
		],
		see_also is [ranking_dataset_protocol, ranker_protocol, copeland]
	]).

	:- uses(avltree, [
		as_list/2 as dictionary_as_list/2,
		insert/4 as dictionary_insert/4,
		new/1 as dictionary_new/1
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_grouped_dataset(Dataset, DatasetSummary),
		^^grouped_dataset_items(Dataset, Items),
		^^grouped_dataset_groups(Dataset, Groups),
		^^option(missing_relevance(MissingRelevance), Options),
		^^option(tie_scoring(TieScoring), Options),
		^^initialize_scores(Items, Scores0),
		borda_scores(Groups, Dataset, MissingRelevance, TieScoring, Scores0, ScoresDictionary),
		^^ordered_scores(Items, ScoresDictionary, Scores),
		^^build_score_ranker(Items, Scores, Options, DatasetSummary, Ranker).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	borda_scores([], _Dataset, _MissingRelevance, _TieScoring, Scores, Scores).
	borda_scores([Group| Groups], Dataset, MissingRelevance, TieScoring, Scores0, Scores) :-
		^^grouped_dataset_item_relevances(Dataset, Group, MissingRelevance, ItemRelevances),
		^^grouped_dataset_relevance_frequencies(ItemRelevances, FrequencyDictionary),
		relevance_points(FrequencyDictionary, TieScoring, PointsDictionary),
		^^grouped_dataset_item_relevance_scores(ItemRelevances, PointsDictionary, ItemScores),
		^^accumulate_item_scores(ItemScores, Scores0, Scores1),
		borda_scores(Groups, Dataset, MissingRelevance, TieScoring, Scores1, Scores).

	relevance_points(FrequencyDictionary, TieScoring, PointsDictionary) :-
		dictionary_as_list(FrequencyDictionary, FrequencyEntries),
		dictionary_new(PointsDictionary0),
		relevance_points(FrequencyEntries, TieScoring, 0, PointsDictionary0, PointsDictionary).

	relevance_points([], _TieScoring, _LowerCount, PointsDictionary, PointsDictionary).
	relevance_points([Relevance-Count| FrequencyEntries], standard, LowerCount0, PointsDictionary0, PointsDictionary) :-
		!,
		dictionary_insert(PointsDictionary0, Relevance, LowerCount0, PointsDictionary1),
		LowerCount is LowerCount0 + Count,
		relevance_points(FrequencyEntries, standard, LowerCount, PointsDictionary1, PointsDictionary).
	relevance_points([Relevance-Count| FrequencyEntries], fractional, LowerCount0, PointsDictionary0, PointsDictionary) :-
		Points is float(LowerCount0 + (Count - 1) / 2),
		dictionary_insert(PointsDictionary0, Relevance, Points, PointsDictionary1),
		LowerCount is LowerCount0 + Count,
		relevance_points(FrequencyEntries, fractional, LowerCount, PointsDictionary1, PointsDictionary).

	score_ranker_model(borda).

	score_ranker_label('Borda').

	score_ranker_term(Items, Scores, Diagnostics, borda_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		number(Score),
		Score >= 0.

	valid_option(missing_relevance(Value)) :-
		once((Value == zero; Value == error)).
	valid_option(tie_scoring(Value)) :-
		once((Value == standard; Value == fractional)).

	default_option(missing_relevance(zero)).
	default_option(tie_scoring(standard)).

:- end_object.
