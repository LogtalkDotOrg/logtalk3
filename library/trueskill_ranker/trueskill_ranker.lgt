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


:- object(trueskill_ranker,
	imports([ranking_dataset_common, score_ranker_model_common, normal_distribution_common])).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-08-02,
		comment is 'TrueSkill ranker for ordered multiplayer matches with ranked teams, draws, and weighted player participation.',
		see_also is [multiplayer_ranking_dataset_protocol, ranker_protocol, glicko2_periodic_ranker]
	]).

	:- uses(avltree, [
		insert/4 as dictionary_insert/4, lookup/3 as dictionary_lookup/3, new/1 as dictionary_new/1
	]).

	:- uses(list, [
		length/2, memberchk/2, reverse/2
	]).

	learn(Dataset, Ranker) :-
		learn(Dataset, Ranker, []).

	learn(Dataset, Ranker, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		^^validate_multiplayer_dataset(Dataset, DatasetSummary),
		^^multiplayer_dataset_items(Dataset, Items),
		^^multiplayer_dataset_events(Dataset, Events),
		^^option(initial_mean(InitialMean), Options),
		^^option(initial_deviation(InitialDeviation), Options),
		^^option(performance_deviation(PerformanceDeviation), Options),
		^^option(dynamics_factor(DynamicsFactor), Options),
		^^option(draw_probability(DrawProbability), Options),
		^^option(conservative_multiplier(ConservativeMultiplier), Options),
		^^option(maximum_iterations(MaximumIterations), Options),
		^^option(tolerance(Tolerance), Options),
		initialize_parameters(Items, InitialMean, InitialDeviation, Means0, Variances0),
		process_events(Events, PerformanceDeviation, DynamicsFactor, DrawProbability, MaximumIterations, Tolerance, Means0, Means, Variances0, Variances, converged, Convergence, 0, Iterations, 0, MaximumMatchIterations, 0.0, FinalDelta),
		ordered_parameters(Items, Means, Variances, ConservativeMultiplier, SkillMeans, SkillDeviations, Exposures),
		length(Events, MatchesProcessed),
		Ranker = trueskill_ranker(Items, Exposures, [
			model(trueskill_ranker),
			options(Options),
			skill_means(SkillMeans),
			skill_deviations(SkillDeviations),
			matches_processed(MatchesProcessed),
			convergence(Convergence),
			iterations(Iterations),
			maximum_match_iterations(MaximumMatchIterations),
			final_delta(FinalDelta),
			dataset_summary(DatasetSummary)
		]).

	rank(Ranker, Candidates, Ranking) :-
		^^score_ranker_data(Ranker, _Items, Exposures, _Diagnostics),
		^^rank_by_scores(Exposures, Candidates, Ranking).

	initialize_parameters(Items, InitialMean, InitialDeviation, Means, Variances) :-
		InitialVariance is InitialDeviation * InitialDeviation,
		dictionary_new(Means0),
		dictionary_new(Variances0),
		initialize_parameters(Items, InitialMean, InitialVariance, Means0, Means, Variances0, Variances).

	initialize_parameters([], _InitialMean, _InitialVariance, Means, Means, Variances, Variances).
	initialize_parameters([Item| Items], InitialMean, InitialVariance, Means0, Means, Variances0, Variances) :-
		dictionary_insert(Means0, Item, InitialMean, Means1),
		dictionary_insert(Variances0, Item, InitialVariance, Variances1),
		initialize_parameters(Items, InitialMean, InitialVariance, Means1, Means, Variances1, Variances).

	process_events([], _PerformanceDeviation, _DynamicsFactor, _DrawProbability, _MaximumIterations, _Tolerance, Means, Means, Variances, Variances, Convergence, Convergence, Iterations, Iterations, MaximumMatchIterations, MaximumMatchIterations, FinalDelta, FinalDelta).
	process_events([match(_Match, Teams0)| Events], PerformanceDeviation, DynamicsFactor, DrawProbability, MaximumIterations, Tolerance, Means0, Means, Variances0, Variances, Convergence0, Convergence, Iterations0, Iterations, MaximumMatchIterations0, MaximumMatchIterations, FinalDelta0, FinalDelta) :-
		inflate_team_variances(Teams0, DynamicsFactor, Variances0, InflatedVariances),
		order_teams(Teams0, Teams),
		build_comparisons(Teams, 1, Comparisons),
		optimize_match(MaximumIterations, Tolerance, Comparisons, PerformanceDeviation, DrawProbability, Means0, Means1, InflatedVariances, Variances1, MatchConvergence, MatchIterations, MatchDelta),
		combine_convergence(Convergence0, MatchConvergence, Convergence1),
		Iterations1 is Iterations0 + MatchIterations,
		maximum_number(MaximumMatchIterations0, MatchIterations, MaximumMatchIterations1),
		maximum_number(FinalDelta0, MatchDelta, FinalDelta1),
		process_events(Events, PerformanceDeviation, DynamicsFactor, DrawProbability, MaximumIterations, Tolerance, Means1, Means, Variances1, Variances, Convergence1, Convergence, Iterations1, Iterations, MaximumMatchIterations1, MaximumMatchIterations, FinalDelta1, FinalDelta).

	inflate_team_variances([], _DynamicsFactor, Variances, Variances).
	inflate_team_variances([team(_Team, _Rank, Members)| Teams], DynamicsFactor, Variances0, Variances) :-
		inflate_member_variances(Members, DynamicsFactor, Variances0, Variances1),
		inflate_team_variances(Teams, DynamicsFactor, Variances1, Variances).

	inflate_member_variances([], _DynamicsFactor, Variances, Variances).
	inflate_member_variances([Item-_Weight| Members], DynamicsFactor, Variances0, Variances) :-
		dictionary_lookup(Item, Variance0, Variances0),
		Variance is Variance0 + DynamicsFactor * DynamicsFactor,
		dictionary_insert(Variances0, Item, Variance, Variances1),
		inflate_member_variances(Members, DynamicsFactor, Variances1, Variances).

	order_teams(Teams, OrderedTeams) :-
		order_teams(Teams, [], OrderedTeams).

	order_teams([], OrderedTeams, OrderedTeams).
	order_teams([Team| Teams], OrderedTeams0, OrderedTeams) :-
		insert_team(OrderedTeams0, Team, OrderedTeams1),
		order_teams(Teams, OrderedTeams1, OrderedTeams).

	insert_team([], Team, [Team]).
	insert_team([team(NextName, NextRank, NextMembers)| Teams], team(Name, Rank, Members), OrderedTeams) :-
		(	Rank < NextRank ->
			OrderedTeams = [team(Name, Rank, Members), team(NextName, NextRank, NextMembers)| Teams]
		;	OrderedTeams = [team(NextName, NextRank, NextMembers)| Rest],
			insert_team(Teams, team(Name, Rank, Members), Rest)
		).

	build_comparisons([_Team], _Index, []) :-
		!.
	build_comparisons([team(_LeftTeam, LeftRank, LeftMembers), team(_RightTeam, RightRank, RightMembers)| Teams], Index, [comparison(Index, LeftRank, LeftMembers, RightRank, RightMembers)| Comparisons]) :-
		NextIndex is Index + 1,
		build_comparisons([team(right, RightRank, RightMembers)| Teams], NextIndex, Comparisons).

	optimize_match(MaximumIterations, Tolerance, Comparisons, PerformanceDeviation, DrawProbability, Means0, Means, Variances0, Variances, Convergence, Iterations, FinalDelta) :-
		dictionary_new(Messages0),
		optimize_match(1, MaximumIterations, Tolerance, Comparisons, PerformanceDeviation, DrawProbability, Means0, Means, Variances0, Variances, Messages0, _Messages, Convergence, Iterations, FinalDelta).

	optimize_match(Iteration, MaximumIterations, Tolerance, Comparisons, PerformanceDeviation, DrawProbability, Means0, Means, Variances0, Variances, Messages0, Messages, Convergence, Iterations, FinalDelta) :-
		update_comparisons(Comparisons, PerformanceDeviation, DrawProbability, Means0, Means1, Variances0, Variances1, Messages0, Messages1, 0.0, ForwardDelta),
		reverse(Comparisons, ReverseComparisons),
		update_comparisons(ReverseComparisons, PerformanceDeviation, DrawProbability, Means1, Means2, Variances1, Variances2, Messages1, Messages2, 0.0, ReverseDelta),
		maximum_number(ForwardDelta, ReverseDelta, Delta),
		(	Delta =< Tolerance ->
			Means = Means2,
			Variances = Variances2,
			Messages = Messages2,
			Convergence = converged,
			Iterations = Iteration,
			FinalDelta = Delta
		;	Iteration >= MaximumIterations ->
			Means = Means2,
			Variances = Variances2,
			Messages = Messages2,
			Convergence = not_converged,
			Iterations = Iteration,
			FinalDelta = Delta
		;	NextIteration is Iteration + 1,
			optimize_match(NextIteration, MaximumIterations, Tolerance, Comparisons, PerformanceDeviation, DrawProbability, Means2, Means, Variances2, Variances, Messages2, Messages, Convergence, Iterations, FinalDelta)
		).

	update_comparisons([], _PerformanceDeviation, _DrawProbability, Means, Means, Variances, Variances, Messages, Messages, Delta, Delta).
	update_comparisons([Comparison| Comparisons], PerformanceDeviation, DrawProbability, Means0, Means, Variances0, Variances, Messages0, Messages, Delta0, Delta) :-
		update_comparison(Comparison, PerformanceDeviation, DrawProbability, Means0, Means1, Variances0, Variances1, Messages0, Messages1, ComparisonDelta),
		maximum_number(Delta0, ComparisonDelta, Delta1),
		update_comparisons(Comparisons, PerformanceDeviation, DrawProbability, Means1, Means, Variances1, Variances, Messages1, Messages, Delta1, Delta).

	update_comparison(comparison(Index, LeftRank, LeftMembers, RightRank, RightMembers), PerformanceDeviation, DrawProbability, Means0, Means, Variances0, Variances, Messages0, Messages, Delta) :-
		comparison_players(LeftMembers, 1.0, LeftPlayers),
		comparison_players(RightMembers, -1.0, RightPlayers),
		append_players(LeftPlayers, RightPlayers, Players),
		comparison_cavities(Players, Index, Means0, Variances0, Messages0, Cavities, 0.0, DifferenceMean, 0.0, DifferenceVariance0, 0.0, WeightSquares),
		DifferenceVariance is DifferenceVariance0 + PerformanceDeviation * PerformanceDeviation * WeightSquares,
		DifferenceDeviation is sqrt(DifferenceVariance),
		draw_margin(DrawProbability, PerformanceDeviation, WeightSquares, DrawMargin),
		comparison_correction(LeftRank, RightRank, DifferenceMean, DifferenceDeviation, DrawMargin, VCorrection, WCorrection),
		update_cavities(Cavities, Index, DifferenceDeviation, VCorrection, WCorrection, Means0, Means, Variances0, Variances, Messages0, Messages, 0.0, Delta).

	comparison_players([], _Side, []).
	comparison_players([Item-Weight| Members], Side, [player(Item, Coefficient)| Players]) :-
		Coefficient is Side * Weight,
		comparison_players(Members, Side, Players).

	append_players([], Players, Players).
	append_players([Player| Players], Tail, [Player| Appended]) :-
		append_players(Players, Tail, Appended).

	comparison_cavities([], _Index, _Means, _Variances, _Messages, [], DifferenceMean, DifferenceMean, DifferenceVariance, DifferenceVariance, WeightSquares, WeightSquares).
	comparison_cavities([player(Item, Coefficient)| Players], Index, Means, Variances, Messages, [cavity(Item, Coefficient, Mean, Variance, OldMean, OldVariance)| Cavities], DifferenceMean0, DifferenceMean, DifferenceVariance0, DifferenceVariance, WeightSquares0, WeightSquares) :-
		dictionary_lookup(Item, OldMean, Means),
		dictionary_lookup(Item, OldVariance, Variances),
		message_parameters(Messages, Index, Item, MessagePrecision, MessagePrecisionMean),
		OldPrecision is 1.0 / OldVariance,
		CavityPrecision is OldPrecision - MessagePrecision,
		CavityPrecisionMean is OldMean * OldPrecision - MessagePrecisionMean,
		Mean is CavityPrecisionMean / CavityPrecision,
		Variance is 1.0 / CavityPrecision,
		DifferenceMean1 is DifferenceMean0 + Coefficient * Mean,
		DifferenceVariance1 is DifferenceVariance0 + Coefficient * Coefficient * Variance,
		WeightSquares1 is WeightSquares0 + Coefficient * Coefficient,
		comparison_cavities(Players, Index, Means, Variances, Messages, Cavities, DifferenceMean1, DifferenceMean, DifferenceVariance1, DifferenceVariance, WeightSquares1, WeightSquares).

	message_parameters(Messages, Index, Item, Precision, PrecisionMean) :-
		(	dictionary_lookup(message(Index, Item), message(Precision, PrecisionMean), Messages) ->
			true
		;	Precision = 0.0,
			PrecisionMean = 0.0
		).

	draw_margin(0.0, _PerformanceDeviation, _WeightSquares, 0.0) :-
		!.
	draw_margin(DrawProbability, PerformanceDeviation, WeightSquares, DrawMargin) :-
		Probability is (DrawProbability + 1.0) / 2.0,
		^^standard_normal_quantile(Probability, Quantile),
		DrawMargin is Quantile * PerformanceDeviation * sqrt(WeightSquares).

	comparison_correction(LeftRank, RightRank, DifferenceMean, DifferenceDeviation, DrawMargin, VCorrection, WCorrection) :-
		(	LeftRank =:= RightRank ->
			draw_correction(DifferenceMean, DifferenceDeviation, DrawMargin, VCorrection, WCorrection)
		;	win_correction(DifferenceMean, DifferenceDeviation, DrawMargin, VCorrection, WCorrection)
		).

	win_correction(DifferenceMean, DifferenceDeviation, DrawMargin, VCorrection, WCorrection) :-
		X is (DifferenceMean - DrawMargin) / DifferenceDeviation,
		(	X < -8.0 ->
			Tail is -X,
			VCorrection is Tail + 1.0 / Tail,
			WCorrection = 1.0
		;	X > 8.0 ->
			VCorrection = 0.0,
			WCorrection = 0.0
		;	^^standard_normal_density(X, Density),
			^^standard_normal_distribution(X, Probability0),
			positive_denominator(Probability0, Probability),
			VCorrection is Density / Probability,
			RawWCorrection is VCorrection * (VCorrection + X),
			bounded_w_correction(RawWCorrection, WCorrection)
		).

	draw_correction(DifferenceMean, DifferenceDeviation, DrawMargin, VCorrection, WCorrection) :-
		Lower is (-DrawMargin - DifferenceMean) / DifferenceDeviation,
		Upper is (DrawMargin - DifferenceMean) / DifferenceDeviation,
		^^standard_normal_distribution(Upper, UpperProbability),
		^^standard_normal_distribution(Lower, LowerProbability),
		Probability0 is UpperProbability - LowerProbability,
		positive_denominator(Probability0, Probability),
		^^standard_normal_density(Lower, LowerDensity),
		^^standard_normal_density(Upper, UpperDensity),
		VCorrection is (LowerDensity - UpperDensity) / Probability,
		RawWCorrection is VCorrection * VCorrection + (Upper * UpperDensity - Lower * LowerDensity) / Probability,
		bounded_w_correction(RawWCorrection, WCorrection).

	positive_denominator(Value, Denominator) :-
		(	Value > 1.0e-300 ->
			Denominator = Value
		;	Denominator = 1.0e-300
		).

	bounded_w_correction(Value, BoundedValue) :-
		(	Value < 0.0 ->
			BoundedValue = 0.0
		;	Value > 0.999999999999 ->
			BoundedValue = 0.999999999999
		;	BoundedValue = Value
		).

	update_cavities([], _Index, _DifferenceDeviation, _VCorrection, _WCorrection, Means, Means, Variances, Variances, Messages, Messages, Delta, Delta).
	update_cavities([cavity(Item, Coefficient, Mean, Variance, OldMean, OldVariance)| Cavities], Index, DifferenceDeviation, VCorrection, WCorrection, Means0, Means, Variances0, Variances, Messages0, Messages, Delta0, Delta) :-
		UpdatedMean is Mean + Variance * Coefficient / DifferenceDeviation * VCorrection,
		VarianceMultiplier0 is 1.0 - Variance * Coefficient * Coefficient / (DifferenceDeviation * DifferenceDeviation) * WCorrection,
		positive_variance_multiplier(VarianceMultiplier0, VarianceMultiplier),
		UpdatedVariance is Variance * VarianceMultiplier,
		CavityPrecision is 1.0 / Variance,
		CavityPrecisionMean is Mean * CavityPrecision,
		UpdatedPrecision is 1.0 / UpdatedVariance,
		MessagePrecision is UpdatedPrecision - CavityPrecision,
		MessagePrecisionMean is UpdatedMean * UpdatedPrecision - CavityPrecisionMean,
		dictionary_insert(Means0, Item, UpdatedMean, Means1),
		dictionary_insert(Variances0, Item, UpdatedVariance, Variances1),
		dictionary_insert(Messages0, message(Index, Item), message(MessagePrecision, MessagePrecisionMean), Messages1),
		MeanDelta is abs(UpdatedMean - OldMean),
		DeviationDelta is abs(sqrt(UpdatedVariance) - sqrt(OldVariance)),
		maximum_number(MeanDelta, DeviationDelta, ParameterDelta),
		maximum_number(Delta0, ParameterDelta, Delta1),
		update_cavities(Cavities, Index, DifferenceDeviation, VCorrection, WCorrection, Means1, Means, Variances1, Variances, Messages1, Messages, Delta1, Delta).

	positive_variance_multiplier(Value, Multiplier) :-
		(	Value > 1.0e-12 ->
			Multiplier = Value
		;	Multiplier = 1.0e-12
		).

	ordered_parameters([], _Means, _Variances, _ConservativeMultiplier, [], [], []).
	ordered_parameters([Item| Items], Means, Variances, ConservativeMultiplier, [Item-Mean| SkillMeans], [Item-Deviation| SkillDeviations], [Item-Exposure| Exposures]) :-
		dictionary_lookup(Item, Mean, Means),
		dictionary_lookup(Item, Variance, Variances),
		Deviation is sqrt(Variance),
		Exposure is Mean - ConservativeMultiplier * Deviation,
		ordered_parameters(Items, Means, Variances, ConservativeMultiplier, SkillMeans, SkillDeviations, Exposures).

	combine_convergence(not_converged, _MatchConvergence, not_converged) :-
		!.
	combine_convergence(_Convergence, not_converged, not_converged) :-
		!.
	combine_convergence(converged, converged, converged).

	maximum_number(Left, Right, Maximum) :-
		(	Left >= Right ->
			Maximum = Left
		;	Maximum = Right
		).

	score_ranker_model(trueskill_ranker).

	score_ranker_label('TrueSkill').

	score_ranker_term(Items, Exposures, Diagnostics, trueskill_ranker(Items, Exposures, Diagnostics)).

	valid_score(Score) :-
		number(Score).

	valid_score_ranker_diagnostics(Items, _Exposures, Diagnostics) :-
		^^valid_ranker_metadata(trueskill_ranker, Diagnostics),
		memberchk(skill_means(Means), Diagnostics),
		^^valid_item_value_pairs(Items, Means),
		valid_numeric_values(Means),
		memberchk(skill_deviations(Deviations), Diagnostics),
		^^valid_item_value_pairs(Items, Deviations),
		valid_positive_values(Deviations),
		memberchk(matches_processed(MatchesProcessed), Diagnostics),
		integer(MatchesProcessed),
		MatchesProcessed >= 0,
		memberchk(convergence(Convergence), Diagnostics),
		once((Convergence == converged; Convergence == not_converged)),
		memberchk(iterations(Iterations), Diagnostics),
		integer(Iterations),
		Iterations >= 0,
		memberchk(maximum_match_iterations(MaximumMatchIterations), Diagnostics),
		integer(MaximumMatchIterations),
		MaximumMatchIterations >= 0,
		memberchk(final_delta(FinalDelta), Diagnostics),
		number(FinalDelta),
		FinalDelta >= 0.0.

	valid_numeric_values([]).
	valid_numeric_values([_Item-Value| Values]) :-
		number(Value),
		valid_numeric_values(Values).

	valid_positive_values([]).
	valid_positive_values([_Item-Value| Values]) :-
		number(Value),
		Value > 0.0,
		valid_positive_values(Values).

	valid_option(initial_mean(Value)) :-
		number(Value).
	valid_option(initial_deviation(Value)) :-
		number(Value),
		Value > 0.0.
	valid_option(performance_deviation(Value)) :-
		number(Value),
		Value > 0.0.
	valid_option(dynamics_factor(Value)) :-
		number(Value),
		Value >= 0.0.
	valid_option(draw_probability(Value)) :-
		number(Value),
		Value >= 0.0,
		Value < 1.0.
	valid_option(conservative_multiplier(Value)) :-
		number(Value),
		Value >= 0.0.
	valid_option(maximum_iterations(Value)) :-
		integer(Value),
		Value > 0.
	valid_option(tolerance(Value)) :-
		number(Value),
		Value > 0.0.

	default_option(initial_mean(25.0)).
	default_option(initial_deviation(8.333333333333334)).
	default_option(performance_deviation(4.166666666666667)).
	default_option(dynamics_factor(0.08333333333333333)).
	default_option(draw_probability(0.10)).
	default_option(conservative_multiplier(3.0)).
	default_option(maximum_iterations(100)).
	default_option(tolerance(1.0e-6)).

:- end_object.
