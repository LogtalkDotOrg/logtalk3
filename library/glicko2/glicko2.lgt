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


:- object(glicko2,
	imports([ranking_dataset_common, score_ranker_model_common])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Glicko-2 pairwise preference ranker. Learns one deterministic rating per item from a dataset object implementing the ``pairwise_ranking_dataset_protocol`` protocol by applying one Glicko-2 rating-period update over the aggregated pairwise outcomes and returns a self-describing ranker term with diagnostics that can be used for ranking and export.',
		remarks is [
			'Algorithm' - 'Applies the standard Glicko-2 rating, rating-deviation, and volatility update equations over a single synthetic rating period built from the aggregated pairwise outcomes of the dataset.',
			'Weight semantics' - 'Preference weights must be positive integers and are interpreted as repeated unit outcomes inside the same synthetic rating period.',
			'Batch semantics' - 'Because the current pairwise dataset protocol does not encode timestamps or rating periods, the implementation treats the whole dataset as a single deterministic Glicko-2 rating period.',
			'Dataset requirements' - 'The current implementation requires a well-formed connected pairwise dataset so that learned ratings remain globally comparable across all ranked items.',
			'Ranker representation' - 'The learned ranker is represented by default as ``glicko2_ranker(Items, Ratings, Diagnostics)`` where ``Ratings`` stores ``Item-Rating`` pairs and ``Diagnostics`` stores metadata such as the effective options, per-item rating deviations, per-item volatilities, and the training dataset summary.'
		],
		see_also is [pairwise_ranking_dataset_protocol, ranker_protocol, elo, rank_centrality]
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
	]).

	:- uses(list, [
		length/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_pairwise_dataset(Dataset, DatasetSummary),
		^^pairwise_dataset_items(Dataset, Items),
		^^option(initial_rating(InitialRating), Options),
		^^option(initial_deviation(InitialDeviation), Options),
		^^option(initial_volatility(InitialVolatility), Options),
		^^pairwise_dataset_preferences(Dataset, Preferences),
		validate_integer_preferences(Preferences),
		^^pairwise_dataset_matchups(Dataset, Matchups),
		^^option(tau(Tau), Options),
		^^option(volatility_tolerance(VolatilityTolerance), Options),
		initialize_parameters(Items, InitialRating, InitialDeviation, InitialVolatility, Ratings0, Deviations0, Volatilities0),
		build_period_results(Matchups, PeriodResults),
		rate_period(Items, PeriodResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings1, Deviations1, Volatilities1),
		ordered_rating_pairs(Items, Ratings1, Ratings),
		ordered_deviation_pairs(Items, Deviations1, Deviations),
		ordered_pairs(Items, Volatilities1, Volatilities),
		build_ranker(Items, Ratings, Deviations, Volatilities, Options, DatasetSummary, Ranker).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Ratings, _Diagnostics),
		^^rank_by_scores(Ratings, Candidates, Ranking).

	build_ranker(Items, Ratings, Deviations, Volatilities, Options, DatasetSummary, glicko2_ranker(Items, Ratings, [
		model(glicko2),
		options(Options),
		rating_deviations(Deviations),
		volatilities(Volatilities),
		dataset_summary(DatasetSummary)
	])).

	validate_integer_preferences([]).
	validate_integer_preferences([p(_Winner, _Loser, Weight)| Preferences]) :-
		(   integer(Weight) ->
			true
		;   type_error(integer, Weight)
		),
		validate_integer_preferences(Preferences).

	initialize_parameters(Items, InitialRating, InitialDeviation, InitialVolatility, Ratings, Deviations, Volatilities) :-
		to_mu(InitialRating, InitialMu),
		to_phi(InitialDeviation, InitialPhi),
		dictionary_new(Ratings0),
		dictionary_new(Deviations0),
		dictionary_new(Volatilities0),
		initialize_parameters(Items, InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings, Deviations0, Deviations, Volatilities0, Volatilities).

	initialize_parameters([], _InitialMu, _InitialPhi, _InitialVolatility, Ratings, Ratings, Deviations, Deviations, Volatilities, Volatilities).
	initialize_parameters([Item| Items], InitialMu, InitialPhi, InitialVolatility, Ratings0, Ratings, Deviations0, Deviations, Volatilities0, Volatilities) :-
		dictionary_insert(Ratings0, Item, InitialMu, Ratings1),
		dictionary_insert(Deviations0, Item, InitialPhi, Deviations1),
		dictionary_insert(Volatilities0, Item, InitialVolatility, Volatilities1),
		initialize_parameters(Items, InitialMu, InitialPhi, InitialVolatility, Ratings1, Ratings, Deviations1, Deviations, Volatilities1, Volatilities).

	build_period_results(Matchups, PeriodResults) :-
		dictionary_new(PeriodResults0),
		build_period_results(Matchups, PeriodResults0, PeriodResults).

	build_period_results([], PeriodResults, PeriodResults).
	build_period_results([matchup(Item1, Item2, Item1Wins, Item2Wins)| Matchups], PeriodResults0, PeriodResults) :-
		Games is Item1Wins + Item2Wins,
		update_period_results(PeriodResults0, Item1, game(Item2, Item1Wins, Games), PeriodResults1),
		update_period_results(PeriodResults1, Item2, game(Item1, Item2Wins, Games), PeriodResults2),
		build_period_results(Matchups, PeriodResults2, PeriodResults).

	update_period_results(PeriodResults0, Item, Game, PeriodResults) :-
		(   dictionary_lookup(Item, Games0, PeriodResults0) ->
			Games = [Game| Games0]
		;   Games = [Game]
		),
		dictionary_insert(PeriodResults0, Item, Games, PeriodResults).

	rate_period(Items, PeriodResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings, Deviations, Volatilities) :-
		dictionary_new(Ratings1),
		dictionary_new(Deviations1),
		dictionary_new(Volatilities1),
		rate_period(Items, PeriodResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, Ratings1, Ratings, Deviations1, Deviations, Volatilities1, Volatilities).

	rate_period([], _PeriodResults, _Ratings0, _Deviations0, _Volatilities0, _Tau, _VolatilityTolerance, Ratings, Ratings, Deviations, Deviations, Volatilities, Volatilities).
	rate_period([Item| Items], PeriodResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, RatingsAcc0, RatingsAcc, DeviationsAcc0, DeviationsAcc, VolatilitiesAcc0, VolatilitiesAcc) :-
		item_period_results(Item, PeriodResults, ItemResults),
		update_item_parameters(Item, ItemResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, UpdatedRating, UpdatedDeviation, UpdatedVolatility),
		dictionary_insert(RatingsAcc0, Item, UpdatedRating, RatingsAcc1),
		dictionary_insert(DeviationsAcc0, Item, UpdatedDeviation, DeviationsAcc1),
		dictionary_insert(VolatilitiesAcc0, Item, UpdatedVolatility, VolatilitiesAcc1),
		rate_period(Items, PeriodResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, RatingsAcc1, RatingsAcc, DeviationsAcc1, DeviationsAcc, VolatilitiesAcc1, VolatilitiesAcc).

	item_period_results(Item, PeriodResults, ItemResults) :-
		(   dictionary_lookup(Item, ItemResults, PeriodResults) ->
			true
		;   ItemResults = []
		).

	update_item_parameters(Item, ItemResults, Ratings0, Deviations0, Volatilities0, Tau, VolatilityTolerance, UpdatedRating, UpdatedDeviation, UpdatedVolatility) :-
		dictionary_lookup(Item, Rating, Ratings0),
		dictionary_lookup(Item, Deviation, Deviations0),
		dictionary_lookup(Item, Volatility, Volatilities0),
		(   ItemResults == [] ->
			UpdatedVolatility = Volatility,
			DeviationStar is sqrt(Deviation * Deviation + UpdatedVolatility * UpdatedVolatility),
			UpdatedRating = Rating,
			UpdatedDeviation = DeviationStar
		;   rating_period_terms(ItemResults, Rating, Ratings0, Deviations0, 0.0, VarianceInverse, 0.0, ImprovementSum),
			Variance is 1.0 / VarianceInverse,
			Delta is Variance * ImprovementSum,
			update_volatility(Deviation, Volatility, Delta, Variance, Tau, VolatilityTolerance, UpdatedVolatility),
			DeviationStar is sqrt(Deviation * Deviation + UpdatedVolatility * UpdatedVolatility),
			UpdatedDeviation is 1.0 / sqrt(1.0 / (DeviationStar * DeviationStar) + 1.0 / Variance),
			UpdatedRating is Rating + UpdatedDeviation * UpdatedDeviation * ImprovementSum
		).

	rating_period_terms([], _Rating, _Ratings0, _Deviations0, VarianceInverse, VarianceInverse, ImprovementSum, ImprovementSum).
	rating_period_terms([game(Opponent, Score, Games)| ItemResults], Rating, Ratings0, Deviations0, VarianceInverse0, VarianceInverse, ImprovementSum0, ImprovementSum) :-
		dictionary_lookup(Opponent, OpponentRating, Ratings0),
		dictionary_lookup(Opponent, OpponentDeviation, Deviations0),
		glicko2_g(OpponentDeviation, GFactor),
		expected_score(Rating, OpponentRating, OpponentDeviation, ExpectedScore),
		VarianceInverse1 is VarianceInverse0 + Games * GFactor * GFactor * ExpectedScore * (1.0 - ExpectedScore),
		ImprovementSum1 is ImprovementSum0 + GFactor * (Score - Games * ExpectedScore),
		rating_period_terms(ItemResults, Rating, Ratings0, Deviations0, VarianceInverse1, VarianceInverse, ImprovementSum1, ImprovementSum).

	update_volatility(Deviation, Volatility, Delta, Variance, Tau, VolatilityTolerance, UpdatedVolatility) :-
		LogVolatilitySquared is log(Volatility * Volatility),
		choose_search_boundary(Deviation, Delta, Variance, LogVolatilitySquared, Tau, Boundary),
		volatility_function(LogVolatilitySquared, Deviation, Delta, Variance, LogVolatilitySquared, Tau, ValueAtA),
		volatility_function(Boundary, Deviation, Delta, Variance, LogVolatilitySquared, Tau, ValueAtBoundary),
		refine_volatility(LogVolatilitySquared, Boundary, ValueAtA, ValueAtBoundary, Deviation, Delta, Variance, LogVolatilitySquared, Tau, VolatilityTolerance, Root),
		UpdatedVolatility is exp(Root / 2.0).

	choose_search_boundary(Deviation, Delta, Variance, _LogVolatilitySquared, _Tau, Boundary) :-
		DeltaSquared is Delta * Delta,
		Threshold is Deviation * Deviation + Variance,
		DeltaSquared > Threshold,
		!,
		Boundary is log(DeltaSquared - Threshold).
	choose_search_boundary(Deviation, Delta, Variance, LogVolatilitySquared, Tau, Boundary) :-
		find_search_boundary(1, Deviation, Delta, Variance, LogVolatilitySquared, Tau, Boundary).

	find_search_boundary(K, Deviation, Delta, Variance, LogVolatilitySquared, Tau, Boundary) :-
		Candidate is LogVolatilitySquared - K * Tau,
		volatility_function(Candidate, Deviation, Delta, Variance, LogVolatilitySquared, Tau, Value),
		(   Value < 0.0 ->
			NextK is K + 1,
			find_search_boundary(NextK, Deviation, Delta, Variance, LogVolatilitySquared, Tau, Boundary)
		;   Boundary = Candidate
		).

	refine_volatility(Left, Right, _LeftValue, _RightValue, _Deviation, _Delta, _Variance, _LogVolatilitySquared, _Tau, VolatilityTolerance, Left) :-
		abs(Right - Left) =< VolatilityTolerance,
		!.
	refine_volatility(Left, Right, LeftValue, RightValue, Deviation, Delta, Variance, LogVolatilitySquared, Tau, VolatilityTolerance, Root) :-
		(   abs(RightValue - LeftValue) =< 1.0e-12 ->
			Candidate is (Left + Right) / 2.0
		;   Candidate is Left + (Left - Right) * LeftValue / (RightValue - LeftValue)
		),
		volatility_function(Candidate, Deviation, Delta, Variance, LogVolatilitySquared, Tau, CandidateValue),
		(   CandidateValue * RightValue < 0.0 ->
			NextLeft = Right,
			NextLeftValue = RightValue
		;   NextLeft = Left,
			NextLeftValue is LeftValue / 2.0
		),
		NextRight = Candidate,
		NextRightValue = CandidateValue,
		refine_volatility(NextLeft, NextRight, NextLeftValue, NextRightValue, Deviation, Delta, Variance, LogVolatilitySquared, Tau, VolatilityTolerance, Root).

	volatility_function(Value, Deviation, Delta, Variance, LogVolatilitySquared, Tau, Result) :-
		ExpValue is exp(Value),
		Numerator is ExpValue * (Delta * Delta - Deviation * Deviation - Variance - ExpValue),
		Denominator is 2.0 * (Deviation * Deviation + Variance + ExpValue) * (Deviation * Deviation + Variance + ExpValue),
		Result is Numerator / Denominator - (Value - LogVolatilitySquared) / (Tau * Tau).

	glicko2_g(Deviation, GFactor) :-
		GFactor is 1.0 / sqrt(1.0 + 3.0 * Deviation * Deviation / (pi * pi)).

	expected_score(Rating, OpponentRating, OpponentDeviation, ExpectedScore) :-
		glicko2_g(OpponentDeviation, GFactor),
		ExpectedScore is 1.0 / (1.0 + exp(-GFactor * (Rating - OpponentRating))).

	to_mu(Rating, Mu) :-
		scale_factor(ScaleFactor),
		Mu is (Rating - 1500.0) / ScaleFactor.

	to_phi(Deviation, Phi) :-
		scale_factor(ScaleFactor),
		Phi is Deviation / ScaleFactor.

	to_rating(Mu, Rating) :-
		scale_factor(ScaleFactor),
		Rating is 1500.0 + ScaleFactor * Mu.

	to_deviation(Phi, Deviation) :-
		scale_factor(ScaleFactor),
		Deviation is ScaleFactor * Phi.

	scale_factor(173.7178).

	ordered_rating_pairs([], _Ratings0, []).
	ordered_rating_pairs([Item| Items], Ratings0, [Item-Rating| Ratings]) :-
		dictionary_lookup(Item, Mu, Ratings0),
		to_rating(Mu, Rating),
		ordered_rating_pairs(Items, Ratings0, Ratings).

	ordered_deviation_pairs([], _Deviations0, []).
	ordered_deviation_pairs([Item| Items], Deviations0, [Item-Deviation| Deviations]) :-
		dictionary_lookup(Item, Phi, Deviations0),
		to_deviation(Phi, Deviation),
		ordered_deviation_pairs(Items, Deviations0, Deviations).

	ordered_pairs([], _Dictionary, []).
	ordered_pairs([Item| Items], Dictionary, [Item-Value| Pairs]) :-
		dictionary_lookup(Item, Value, Dictionary),
		ordered_pairs(Items, Dictionary, Pairs).

	score_ranker_model(glicko2).

	score_ranker_label('Glicko-2').

	score_ranker_term(Items, Ratings, Diagnostics, glicko2_ranker(Items, Ratings, Diagnostics)).

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
