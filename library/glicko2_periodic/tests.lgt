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

:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Unit tests for the "glicko2_periodic" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(glicko2_periodic).

	cleanup :-
		^^clean_file('test_output.pl').

	test(glicko2_periodic_learn_2, deterministic(ground(Ranker))) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker).

	test(glicko2_periodic_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::rank(Ranker, [alpha, beta, gamma], Ranking).

	test(glicko2_periodic_two_period_chain_scores_2, deterministic((abs(Alpha - 1720.3171984281494) =< 1.0e-6, abs(Beta - 1500.0) =< 1.0e-6, abs(Gamma - 1279.6828015718506) =< 1.0e-6))) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(glicko2_periodic_draw_scores_2, deterministic((abs(Alpha - 1500.0) =< 1.0e-6, abs(Beta - 1500.0) =< 1.0e-6))) :-
		glicko2_periodic::learn(temporal_draws, Ranker),
		glicko2_periodic::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(glicko2_periodic_draw_diagnostics_2, deterministic((abs(AlphaDeviation - 290.31896161384265) =< 1.0e-6, abs(BetaDeviation - 290.31896161384265) =< 1.0e-6, abs(AlphaVolatility - 0.05999896144314354) =< 1.0e-9, abs(BetaVolatility - 0.05999896144314354) =< 1.0e-9))) :-
		glicko2_periodic::learn(temporal_draws, Ranker),
		glicko2_periodic::diagnostics(Ranker, Diagnostics),
		memberchk(rating_deviations(Deviations), Diagnostics),
		memberchk(volatilities(Volatilities), Diagnostics),
		memberchk(alpha-AlphaDeviation, Deviations),
		memberchk(beta-BetaDeviation, Deviations),
		memberchk(alpha-AlphaVolatility, Volatilities),
		memberchk(beta-BetaVolatility, Volatilities).

	test(glicko2_periodic_idle_periods_scores_2, deterministic((abs(Alpha - 1432.8938834123048) =< 1.0e-6, abs(Beta - 1567.1061165876952) =< 1.0e-6))) :-
		glicko2_periodic::learn(temporal_idle_periods, Ranker),
		glicko2_periodic::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(glicko2_periodic_late_activity_scores_2, deterministic((abs(Alpha - 1662.3108939062977) =< 1.0e-6, abs(Beta - 1337.6891060937023) =< 1.0e-6))) :-
		glicko2_periodic::learn(temporal_late_activity, Ranker),
		glicko2_periodic::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(glicko2_periodic_late_activity_diagnostics_2, deterministic((abs(AlphaDeviation - 290.31896371798047) =< 1.0e-6, abs(BetaDeviation - 290.31896371798047) =< 1.0e-6, abs(AlphaVolatility - 0.05999967537233814) =< 1.0e-9, abs(BetaVolatility - 0.05999967537233814) =< 1.0e-9))) :-
		glicko2_periodic::learn(temporal_late_activity, Ranker),
		glicko2_periodic::diagnostics(Ranker, Diagnostics),
		memberchk(rating_deviations(Deviations), Diagnostics),
		memberchk(volatilities(Volatilities), Diagnostics),
		memberchk(alpha-AlphaDeviation, Deviations),
		memberchk(beta-BetaDeviation, Deviations),
		memberchk(alpha-AlphaVolatility, Volatilities),
		memberchk(beta-BetaVolatility, Volatilities).

	test(glicko2_periodic_diagnostics_2, deterministic) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::diagnostics(Ranker, Diagnostics),
		memberchk(model(glicko2_periodic), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(initial_rating(1500.0), Options),
		memberchk(initial_deviation(350.0), Options),
		memberchk(initial_volatility(0.06), Options),
		memberchk(tau(0.5), Options),
		memberchk(volatility_tolerance(1.0e-6), Options),
		memberchk(periods_processed(2), Diagnostics),
		memberchk(final_period(round2), Diagnostics),
		memberchk(rating_deviations(_Deviations), Diagnostics),
		memberchk(volatilities(_Volatilities), Diagnostics).

	test(glicko2_periodic_ranker_options_2, deterministic) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::ranker_options(Ranker, Options),
		memberchk(initial_rating(1500.0), Options),
		memberchk(initial_deviation(350.0), Options),
		memberchk(initial_volatility(0.06), Options),
		memberchk(tau(0.5), Options),
		memberchk(volatility_tolerance(1.0e-6), Options).

	test(glicko2_periodic_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma])) :-
		^^file_path('test_output.pl', File),
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::export_to_file(temporal_two_period_chain, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		glicko2_periodic::rank(LoadedRanker, [alpha, beta, gamma], Ranking).

	test(glicko2_periodic_invalid_option_error, error(domain_error(option, passes(2)))) :-
		glicko2_periodic::learn(temporal_two_period_chain, _Ranker, [passes(2)]).

	test(glicko2_periodic_non_list_options_error, error(type_error(list, options))) :-
		glicko2_periodic::learn(temporal_two_period_chain, _Ranker, options).

	test(glicko2_periodic_duplicate_periods_error, error(domain_error(unique_periods, [round1, round1]))) :-
		glicko2_periodic::learn(malformed_duplicate_periods, _Ranker).

	test(glicko2_periodic_unknown_period_error, error(existence_error(period, round2))) :-
		glicko2_periodic::learn(malformed_temporal_unknown_period, _Ranker).

	test(glicko2_periodic_unknown_item_error, error(existence_error(item, phantom))) :-
		glicko2_periodic::learn(malformed_temporal_unknown_item, _Ranker).

	test(glicko2_periodic_self_game_error, error(domain_error(distinct_items, alpha-alpha))) :-
		glicko2_periodic::learn(malformed_temporal_self_game, _Ranker).

	test(glicko2_periodic_illegal_score_error, error(domain_error(game_score, 0.75))) :-
		glicko2_periodic::learn(malformed_temporal_illegal_score, _Ranker).

	test(glicko2_periodic_disconnected_error, error(domain_error(connected_temporal_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		glicko2_periodic::learn(disconnected_temporal_pairwise, _Ranker).

	test(glicko2_periodic_rank_variable_candidate_error, error(instantiation_error)) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::rank(Ranker, [_Candidate, beta], _Ranking).

	test(glicko2_periodic_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::rank(Ranker, [alpha, phantom], _Ranking).

	test(glicko2_periodic_print_ranker_1, deterministic) :-
		glicko2_periodic::learn(temporal_two_period_chain, Ranker),
		glicko2_periodic::print_ranker(Ranker).

:- end_object.
