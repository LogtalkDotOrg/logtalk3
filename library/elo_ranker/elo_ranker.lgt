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


:- object(elo_ranker,
	imports([ranking_dataset_common, score_ranker_model_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Elo pairwise preference ranker. Learns one deterministic rating per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by replaying the observed preference stream using a deterministic batch Elo update rule, and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, colley_ranker, rank_centrality]
	]).

	:- uses(avltree, [
		lookup/3 as dictionary_lookup/3
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^option(initial_rating(InitialRating), Options),
		(	Items = [Item] ->
			singleton_ranker(Item, InitialRating, Options, DatasetSummary, Ranker)
		;	^^pairwise_dataset_preferences(Dataset, Preferences),
			validate_integer_preferences(Preferences),
			^^option(k_factor(KFactor), Options),
			^^option(rating_scale(RatingScale), Options),
			initialize_ratings(Items, InitialRating, Ratings0),
			rate_preferences(Preferences, KFactor, RatingScale, Ratings0, RatingsDictionary),
			^^ordered_scores(Items, RatingsDictionary, Ratings),
			^^build_score_ranker(Items, Ratings, Options, DatasetSummary, Ranker)
		).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Ratings, _Diagnostics),
		^^rank_by_scores(Ratings, Candidates, Ranking).

	singleton_ranker(Item, InitialRating, Options, DatasetSummary, elo_ranker([Item], [Item-InitialRating], [
		model(elo_ranker),
		options(Options),
		dataset_summary(DatasetSummary)
	])).

	initialize_ratings(Items, InitialRating, Ratings) :-
		^^initialize_scores(Items, Ratings0),
		initial_rating_deltas(Items, InitialRating, Deltas),
		^^accumulate_item_scores(Deltas, Ratings0, Ratings).

	initial_rating_deltas([], _InitialRating, []).
	initial_rating_deltas([Item| Items], InitialRating, [Item-InitialRating| Deltas]) :-
		initial_rating_deltas(Items, InitialRating, Deltas).

	validate_integer_preferences([]).
	validate_integer_preferences([p(_Winner, _Loser, Weight)| Preferences]) :-
		(	integer(Weight) ->
			true
		;	type_error(integer, Weight)
		),
		validate_integer_preferences(Preferences).

	rate_preferences([], _KFactor, _RatingScale, Ratings, Ratings).
	rate_preferences([p(Winner, Loser, Weight)| Preferences], KFactor, RatingScale, Ratings0, Ratings) :-
		rate_integer_games(Weight, Winner, Loser, KFactor, RatingScale, Ratings0, Ratings1),
		rate_preferences(Preferences, KFactor, RatingScale, Ratings1, Ratings).

	rate_integer_games(0, _Winner, _Loser, _KFactor, _RatingScale, Ratings, Ratings) :-
		!.
	rate_integer_games(Count, Winner, Loser, KFactor, RatingScale, Ratings0, Ratings) :-
		Count > 0,
		rate_game(Winner, Loser, KFactor, RatingScale, Ratings0, Ratings1),
		RemainingCount is Count - 1,
		rate_integer_games(RemainingCount, Winner, Loser, KFactor, RatingScale, Ratings1, Ratings).

	rate_game(Winner, Loser, KFactor, RatingScale, Ratings0, Ratings) :-
		dictionary_lookup(Winner, WinnerRating, Ratings0),
		dictionary_lookup(Loser, LoserRating, Ratings0),
		expected_score(WinnerRating, LoserRating, RatingScale, ExpectedWinnerScore),
		Delta is KFactor * (1.0 - ExpectedWinnerScore),
		^^update_score(Ratings0, Winner, Delta, Ratings1),
		^^update_score(Ratings1, Loser, -Delta, Ratings).

	expected_score(Rating, OpponentRating, RatingScale, ExpectedScore) :-
		Exponent is (OpponentRating - Rating) / RatingScale,
		Odds is 10.0 ** Exponent,
		ExpectedScore is 1.0 / (1.0 + Odds).

	score_ranker_model(elo_ranker).

	score_ranker_label('Elo').

	score_ranker_term(Items, Ratings, Diagnostics, elo_ranker(Items, Ratings, Diagnostics)).

	valid_score(Score) :-
		number(Score).

	valid_option(initial_rating(Value)) :-
		number(Value).
	valid_option(k_factor(Value)) :-
		number(Value),
		Value > 0.0.
	valid_option(rating_scale(Value)) :-
		number(Value),
		Value > 0.0.

	default_option(initial_rating(1500.0)).
	default_option(k_factor(32.0)).
	default_option(rating_scale(400.0)).

:- end_object.
