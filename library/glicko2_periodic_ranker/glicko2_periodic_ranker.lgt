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


:- object(glicko2_periodic_ranker,
	imports([ranking_dataset_common, score_ranker_model_common, glicko2_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Multi-period Glicko-2 ranker over temporal pairwise game datasets with explicit rating periods and per-game results.',
		see_also is [temporal_pairwise_ranking_dataset_protocol, ranker_protocol, glicko2_ranker]
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
	]).

	:- uses(list, [
		last/2, length/2, memberchk/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_temporal_pairwise_dataset(Dataset, DatasetSummary),
		^^temporal_pairwise_dataset_items(Dataset, Items),
		^^temporal_pairwise_dataset_periods(Dataset, Periods),
		^^option(initial_rating(InitialRating), Options),
		^^option(initial_deviation(InitialDeviation), Options),
		^^option(initial_volatility(InitialVolatility), Options),
		^^option(tau(Tau), Options),
		^^option(volatility_tolerance(VolatilityTolerance), Options),
		^^to_mu(InitialRating, InitialMu),
		^^to_phi(InitialDeviation, InitialPhi),
		dictionary_new(Ratings0),
		dictionary_new(Deviations0),
		dictionary_new(Volatilities0),
		rate_periods(Periods, Dataset, Items, InitialMu, InitialPhi, InitialVolatility, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings1, Deviations1, Volatilities1),
		ensure_initialized_items(Items, InitialMu, InitialPhi, InitialVolatility, Ratings1, Ratings2, Deviations1, Deviations2, Volatilities1, Volatilities2),
		ordered_rating_pairs(Items, Ratings2, Ratings),
		ordered_deviation_pairs(Items, Deviations2, Deviations),
		ordered_pairs(Items, Volatilities2, Volatilities),
		build_ranker(Items, Ratings, Deviations, Volatilities, Options, DatasetSummary, Periods, Ranker).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Ratings, _Diagnostics),
		^^rank_by_scores(Ratings, Candidates, Ranking).

	build_ranker(Items, Ratings, Deviations, Volatilities, Options, DatasetSummary, Periods, glicko2_periodic_ranker(Items, Ratings, [
		model(glicko2_periodic_ranker),
		options(Options),
		rating_deviations(Deviations),
		volatilities(Volatilities),
		periods_processed(PeriodsProcessed),
		final_period(FinalPeriod),
		dataset_summary(DatasetSummary)
	])) :-
		length(Periods, PeriodsProcessed),
		last(Periods, FinalPeriod).

	valid_score_ranker_diagnostics(Items, _Ratings, Diagnostics) :-
		^^valid_ranker_metadata(glicko2_periodic_ranker, Diagnostics),
		memberchk(rating_deviations(Deviations), Diagnostics),
		valid_positive_item_values(Items, Deviations),
		memberchk(volatilities(Volatilities), Diagnostics),
		valid_positive_item_values(Items, Volatilities),
		memberchk(periods_processed(PeriodsProcessed), Diagnostics),
		integer(PeriodsProcessed),
		PeriodsProcessed > 0,
		memberchk(final_period(FinalPeriod), Diagnostics),
		nonvar(FinalPeriod).

	valid_positive_item_values(Items, Pairs) :-
		^^valid_item_value_pairs(Items, Pairs),
		valid_positive_values(Pairs).

	valid_positive_values([]).
	valid_positive_values([_Item-Value| Pairs]) :-
		number(Value),
		Value > 0.0,
		valid_positive_values(Pairs).

	ensure_initialized_items([], _InitialMu, _InitialPhi, _InitialVolatility, Ratings, Ratings, Deviations, Deviations, Volatilities, Volatilities).
	ensure_initialized_items([Item| Items], InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings, Deviations0, Deviations, Volatilities0, Volatilities) :-
		ensure_initialized_item(Item, InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings1, Deviations0, Deviations1, Volatilities0, Volatilities1),
		ensure_initialized_items(Items, InitialMu, InitialPhi, InitialVolatility, Ratings1, Ratings, Deviations1, Deviations, Volatilities1, Volatilities).

	ensure_initialized_period_players([], _InitialMu, _InitialPhi, _InitialVolatility, Ratings, Ratings, Deviations, Deviations, Volatilities, Volatilities).
	ensure_initialized_period_players([game(Item1, Item2, _Score)| Games], InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings, Deviations0, Deviations, Volatilities0, Volatilities) :-
		ensure_initialized_item(Item1, InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings1, Deviations0, Deviations1, Volatilities0, Volatilities1),
		ensure_initialized_item(Item2, InitialMu, InitialPhi, InitialVolatility, Ratings1, Ratings2, Deviations1, Deviations2, Volatilities1, Volatilities2),
		ensure_initialized_period_players(Games, InitialMu, InitialPhi, InitialVolatility, Ratings2, Ratings, Deviations2, Deviations, Volatilities2, Volatilities).

	ensure_initialized_item(Item, InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings, Deviations0, Deviations, Volatilities0, Volatilities) :-
		(	dictionary_lookup(Item, _Rating, Ratings0) ->
			Ratings = Ratings0,
			Deviations = Deviations0,
			Volatilities = Volatilities0
		;	dictionary_insert(Ratings0, Item, InitialMu, Ratings),
			dictionary_insert(Deviations0, Item, InitialPhi, Deviations),
			dictionary_insert(Volatilities0, Item, InitialVolatility, Volatilities)
		).

	active_items([], _Ratings, []).
	active_items([Item| Items], Ratings, ActiveItems) :-
		(	dictionary_lookup(Item, _Rating, Ratings) ->
			ActiveItems = [Item| Rest]
		;	ActiveItems = Rest
		),
		active_items(Items, Ratings, Rest).

	rate_periods([], _Dataset, _Items, _InitialMu, _InitialPhi, _InitialVolatility, Ratings, Deviations, Volatilities, _Tau, _VolatilityTolerance, Ratings, Deviations, Volatilities).
	rate_periods([Period| Periods], Dataset, Items, InitialMu, InitialPhi, InitialVolatility, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings, Deviations, Volatilities) :-
		^^temporal_pairwise_dataset_games(Dataset, Period, Games),
		ensure_initialized_period_players(Games, InitialMu, InitialPhi, InitialVolatility, Ratings0, RatingsSeed, Deviations0, DeviationsSeed, Volatilities0, VolatilitiesSeed),
		active_items(Items, RatingsSeed, ActiveItems),
		rate_period_items(ActiveItems, Games, RatingsSeed, DeviationsSeed, VolatilitiesSeed, Tau, VolatilityTolerance, Ratings1, Deviations1, Volatilities1),
		rate_periods(Periods, Dataset, Items, InitialMu, InitialPhi, InitialVolatility, Ratings1, Deviations1, Volatilities1, Tau, VolatilityTolerance, Ratings, Deviations, Volatilities).

	rate_period_items(Items, Games, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings, Deviations, Volatilities) :-
		dictionary_new(Ratings1),
		dictionary_new(Deviations1),
		dictionary_new(Volatilities1),
		rate_period_items(Items, Games, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings1, Ratings, Deviations1, Deviations, Volatilities1, Volatilities).

	rate_period_items([], _Games, _Ratings0, _Deviations0, _Volatilities0, _Tau, _VolatilityTolerance, Ratings, Ratings, Deviations, Deviations, Volatilities, Volatilities).
	rate_period_items([Item| Items], Games, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, RatingsAcc0, RatingsAcc, DeviationsAcc0, DeviationsAcc, VolatilitiesAcc0, VolatilitiesAcc) :-
		item_period_results(Games, Item, ItemResults),
		^^update_item_parameters(Item, ItemResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, UpdatedRating, UpdatedDeviation, UpdatedVolatility),
		dictionary_insert(RatingsAcc0, Item, UpdatedRating, RatingsAcc1),
		dictionary_insert(DeviationsAcc0, Item, UpdatedDeviation, DeviationsAcc1),
		dictionary_insert(VolatilitiesAcc0, Item, UpdatedVolatility, VolatilitiesAcc1),
		rate_period_items(Items, Games, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, RatingsAcc1, RatingsAcc, DeviationsAcc1, DeviationsAcc, VolatilitiesAcc1, VolatilitiesAcc).

	item_period_results([], _Item, []).
	item_period_results([game(Item1, Item2, Score)| Games], Item, ItemResults) :-
		(	Item == Item1 ->
			ItemResults = [game(Item2, Score, 1)| Rest]
		;	Item == Item2 ->
			OpponentScore is 1.0 - Score,
			ItemResults = [game(Item1, OpponentScore, 1)| Rest]
		;	ItemResults = Rest
		),
		item_period_results(Games, Item, Rest).

	ordered_rating_pairs([], _Ratings0, []).
	ordered_rating_pairs([Item| Items], Ratings0, [Item-Rating| Ratings]) :-
		dictionary_lookup(Item, Mu, Ratings0),
		^^to_rating(Mu, Rating),
		ordered_rating_pairs(Items, Ratings0, Ratings).

	ordered_deviation_pairs([], _Deviations0, []).
	ordered_deviation_pairs([Item| Items], Deviations0, [Item-Deviation| Deviations]) :-
		dictionary_lookup(Item, Phi, Deviations0),
		^^to_deviation(Phi, Deviation),
		ordered_deviation_pairs(Items, Deviations0, Deviations).

	ordered_pairs([], _Dictionary, []).
	ordered_pairs([Item| Items], Dictionary, [Item-Value| Pairs]) :-
		dictionary_lookup(Item, Value, Dictionary),
		ordered_pairs(Items, Dictionary, Pairs).

	score_ranker_model(glicko2_periodic_ranker).

	score_ranker_label('Glicko-2 Periodic').

	score_ranker_term(Items, Ratings, Diagnostics, glicko2_periodic_ranker(Items, Ratings, Diagnostics)).

	valid_score(Score) :-
		number(Score).

	valid_option(initial_rating(Value)) :-
		number(Value).
	valid_option(initial_deviation(Value)) :-
		number(Value),
		Value > 0.0.
	valid_option(initial_volatility(Value)) :-
		number(Value),
		Value > 0.0.
	valid_option(tau(Value)) :-
		number(Value),
		Value > 0.0.
	valid_option(volatility_tolerance(Value)) :-
		number(Value),
		Value > 0.0.

	default_option(initial_rating(1500.0)).
	default_option(initial_deviation(350.0)).
	default_option(initial_volatility(0.06)).
	default_option(tau(0.5)).
	default_option(volatility_tolerance(1.0e-6)).

:- end_object.
