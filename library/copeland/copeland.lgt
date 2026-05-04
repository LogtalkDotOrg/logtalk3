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


:- object(copeland,
	imports([ranking_dataset_common, score_ranker_model_common])).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-05-04,
		comment is 'Copeland pairwise preference ranker. Learns one deterministic score per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by comparing aggregated head-to-head outcomes for each observed matchup and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, bradley_terry]
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_matchups(Dataset, Matchups),
		^^initialize_scores(Items, Scores0),
		copeland_scores(Matchups, Scores0, ScoresDictionary),
		^^ordered_scores(Items, ScoresDictionary, Scores),
		^^build_score_ranker(Items, Scores, Options, DatasetSummary, Ranker).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Scores, _Diagnostics),
		^^rank_by_scores(Scores, Candidates, Ranking).

	copeland_scores([], Scores, Scores).
	copeland_scores([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], Scores0, Scores) :-
		compare_scores(Item1Wins, Item2Wins, Item1Delta, Item2Delta),
		^^update_score(Scores0, Item1, Item1Delta, Scores1),
		^^update_score(Scores1, Item2, Item2Delta, Scores2),
		copeland_scores(Matchups, Scores2, Scores).

	compare_scores(Item1Wins, Item2Wins, 1, -1) :-
		Item1Wins > Item2Wins,
		!.
	compare_scores(Item1Wins, Item2Wins, -1, 1) :-
		Item1Wins < Item2Wins,
		!.
	compare_scores(_Item1Wins, _Item2Wins, 0, 0).

	score_ranker_model(copeland).

	score_ranker_label('Copeland').

	score_ranker_term(Items, Scores, Diagnostics, copeland_ranker(Items, Scores, Diagnostics)).

	valid_score(Score) :-
		integer(Score).

	valid_option(_Option) :-
		fail.

	default_option(_Option) :-
		fail.

	fix_option(_Option, _FixedOption) :-
		fail.

:- end_object.
