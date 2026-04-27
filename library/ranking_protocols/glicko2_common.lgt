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


:- category(glicko2_common).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Shared internal Glicko-2 numeric helpers for rating updates, volatility search, and scale conversions.'
	]).

	:- protected(update_item_parameters/10).
	:- protected(to_mu/2).
	:- protected(to_phi/2).
	:- protected(to_rating/2).
	:- protected(to_deviation/2).

	:- uses(avltree, [
		lookup/3 as dictionary_lookup/3
	]).

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

:- end_category.
