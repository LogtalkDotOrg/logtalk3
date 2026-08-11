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
		date is 2026-08-11,
		comment is 'Unit tests for the "trueskill_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(trueskill_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(trueskill_learn_2, deterministic(ground(Ranker))) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker).

	test(trueskill_one_on_one_win_posterior, deterministic((abs(AlphaMean - 29.395831) =< 1.0e-5, abs(BetaMean - 20.604169) =< 1.0e-5, abs(AlphaDeviation - 7.171476) =< 1.0e-5, abs(BetaDeviation - 7.171476) =< 1.0e-5))) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(skill_means(Means), Diagnostics),
		memberchk(skill_deviations(Deviations), Diagnostics),
		memberchk(alpha-AlphaMean, Means),
		memberchk(beta-BetaMean, Means),
		memberchk(alpha-AlphaDeviation, Deviations),
		memberchk(beta-BetaDeviation, Deviations).

	test(trueskill_one_on_one_draw_posterior, deterministic((abs(AlphaMean - 25.0) =< 1.0e-10, abs(BetaMean - 25.0) =< 1.0e-10, AlphaDeviation < 8.333333333333334, abs(AlphaDeviation - BetaDeviation) =< 1.0e-10))) :-
		trueskill_ranker::learn(trueskill_one_on_one_draw, Ranker),
		trueskill_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(skill_means(Means), Diagnostics),
		memberchk(skill_deviations(Deviations), Diagnostics),
		memberchk(alpha-AlphaMean, Means),
		memberchk(beta-BetaMean, Means),
		memberchk(alpha-AlphaDeviation, Deviations),
		memberchk(beta-BetaDeviation, Deviations).

	test(trueskill_free_for_all_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		trueskill_ranker::learn(trueskill_free_for_all, Ranker),
		trueskill_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(trueskill_team_and_draw_rank_3, deterministic(Ranking == [alpha, gamma, beta, epsilon, delta])) :-
		trueskill_ranker::learn(multiplayer_matches, Ranker),
		trueskill_ranker::rank(Ranker, [alpha, beta, gamma, delta, epsilon], Ranking).

	test(trueskill_inactive_item_retains_prior, deterministic((Mean =:= 25.0, Deviation =:= 8.333333333333334, Exposure =:= 0.0))) :-
		trueskill_ranker::learn(multiplayer_matches, Ranker),
		trueskill_ranker::scores(Ranker, Scores),
		trueskill_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(epsilon-Exposure, Scores),
		memberchk(skill_means(Means), Diagnostics),
		memberchk(skill_deviations(Deviations), Diagnostics),
		memberchk(epsilon-Mean, Means),
		memberchk(epsilon-Deviation, Deviations).

	test(trueskill_custom_conservative_multiplier, deterministic((abs(AlphaExposure - AlphaMean) =< 1.0e-12, abs(BetaExposure - BetaMean) =< 1.0e-12))) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker, [conservative_multiplier(0.0)]),
		trueskill_ranker::scores(Ranker, Scores),
		trueskill_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(skill_means(Means), Diagnostics),
		memberchk(alpha-AlphaExposure, Scores),
		memberchk(beta-BetaExposure, Scores),
		memberchk(alpha-AlphaMean, Means),
		memberchk(beta-BetaMean, Means).

	test(trueskill_diagnostics_2, deterministic) :-
		trueskill_ranker::learn(multiplayer_matches, Ranker),
		trueskill_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(model(trueskill_ranker), Diagnostics),
		memberchk(matches_processed(2), Diagnostics),
		memberchk(convergence(converged), Diagnostics),
		memberchk(iterations(4), Diagnostics),
		memberchk(maximum_match_iterations(2), Diagnostics),
		memberchk(dataset_summary([items(5), matches(2), teams(4), participations(6), connected_components(2), isolated_items([epsilon])]), Diagnostics).

	test(trueskill_ranker_options_2, deterministic) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::ranker_options(Ranker, Options),
		memberchk(initial_mean(25.0), Options),
		memberchk(draw_probability(0.10), Options),
		memberchk(maximum_iterations(100), Options).

	test(trueskill_invalid_option_error, error(domain_error(option, draw_probability(1.0)))) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, _Ranker, [draw_probability(1.0)]).

	test(trueskill_invalid_dataset_error, error(domain_error(participation_weight, 1.5))) :-
		trueskill_ranker::learn(multiplayer_invalid_weight, _Ranker).

	test(trueskill_rank_variable_candidate_error, error(instantiation_error)) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(trueskill_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(trueskill_valid_ranker_1, deterministic) :-
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::valid_ranker(Ranker).

	test(trueskill_invalid_valid_ranker_1, fail) :-
		trueskill_ranker::valid_ranker(fake_ranker([alpha], [alpha-0.0], [model(fake)])).

	test(trueskill_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta])) :-
		^^file_path('test_output.pl', File),
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::export_to_file(trueskill_one_on_one_win, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		trueskill_ranker::rank(LoadedRanker, [beta, alpha], Ranking).

	test(trueskill_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		trueskill_ranker::learn(trueskill_one_on_one_win, Ranker),
		trueskill_ranker::print_ranker(Ranker).

:- end_object.
