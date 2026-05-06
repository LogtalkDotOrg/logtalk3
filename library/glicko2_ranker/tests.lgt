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

:- object(singleton_pairwise,
	implements(pairwise_ranking_dataset_protocol)).

	item(alpha).

:- end_object.


:- object(fractional_weight_pairwise,
	implements(pairwise_ranking_dataset_protocol)).

	item(alpha).
	item(beta).

	preference(alpha, beta, 1.5).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "glicko2_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(glicko2_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(glicko2_learn_2, deterministic(ground(Ranker))) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker).

	test(glicko2_learn_3_custom_options, deterministic) :-
		glicko2_ranker::learn(regular_head_to_head, glicko2_ranker(_Items, _Ratings, Diagnostics), [initial_rating(1400.0), initial_deviation(300.0), initial_volatility(0.07), tau(0.4), volatility_tolerance(1.0e-7)]),
		memberchk(options(Options), Diagnostics),
		memberchk(initial_rating(1400.0), Options),
		memberchk(initial_deviation(300.0), Options),
		memberchk(initial_volatility(0.07), Options),
		memberchk(tau(0.4), Options),
		memberchk(volatility_tolerance(1.0e-7), Options).

	test(glicko2_singleton_scores_2, deterministic(Scores == [alpha-1500.0])) :-
		glicko2_ranker::learn(singleton_pairwise, Ranker),
		glicko2_ranker::scores(Ranker, Scores).

	test(glicko2_singleton_diagnostics_2, deterministic((abs(AlphaDeviation - 350.15516610002004) =< 1.0e-6, abs(AlphaVolatility - 0.06) =< 1.0e-12))) :-
		glicko2_ranker::learn(singleton_pairwise, Ranker),
		glicko2_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(rating_deviations(Deviations), Diagnostics),
		memberchk(volatilities(Volatilities), Diagnostics),
		memberchk(alpha-AlphaDeviation, Deviations),
		memberchk(alpha-AlphaVolatility, Volatilities).

	test(glicko2_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(glicko2_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		glicko2_ranker::learn(head_to_head, Ranker),
		glicko2_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(glicko2_two_item_scores_2, deterministic((abs(Alpha - 1667.5290852910684) =< 1.0e-6, abs(Beta - 1332.4709147089316) =< 1.0e-6))) :-
		glicko2_ranker::learn(two_item_head_to_head, Ranker),
		glicko2_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(glicko2_head_to_head_scores_2, deterministic((abs(Alpha - 1925.6651571318193) =< 1.0e-6, abs(Beta - 1601.8269195999296) =< 1.0e-6, abs(Gamma - 1500.0) =< 1.0e-6, abs(Delta - 1061.1487464939246) =< 1.0e-6))) :-
		glicko2_ranker::learn(head_to_head, Ranker),
		glicko2_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores).

	test(glicko2_cyclic_pairwise_scores_2, deterministic((abs(Alpha - 1500.0) =< 1.0e-6, abs(Beta - 1500.0) =< 1.0e-6, abs(Gamma - 1500.0) =< 1.0e-6))) :-
		glicko2_ranker::learn(cyclic_pairwise, Ranker),
		glicko2_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(glicko2_two_item_diagnostics_2, deterministic((abs(AlphaDeviation - 208.56031831575942) =< 1.0e-6, abs(BetaDeviation - 208.56031831575942) =< 1.0e-6, abs(AlphaVolatility - 0.05999861670525175) =< 1.0e-9, abs(BetaVolatility - 0.05999861670525175) =< 1.0e-9))) :-
		glicko2_ranker::learn(two_item_head_to_head, Ranker),
		glicko2_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(rating_deviations(Deviations), Diagnostics),
		memberchk(volatilities(Volatilities), Diagnostics),
		memberchk(alpha-AlphaDeviation, Deviations),
		memberchk(beta-BetaDeviation, Deviations),
		memberchk(alpha-AlphaVolatility, Volatilities),
		memberchk(beta-BetaVolatility, Volatilities).

	test(glicko2_diagnostics_2, deterministic) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(model(glicko2_ranker), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(initial_rating(1500.0), Options),
		memberchk(initial_deviation(350.0), Options),
		memberchk(initial_volatility(0.06), Options),
		memberchk(tau(0.5), Options),
		memberchk(volatility_tolerance(1.0e-6), Options),
		memberchk(rating_deviations(_Deviations), Diagnostics),
		memberchk(volatilities(_Volatilities), Diagnostics).

	test(glicko2_diagnostic_2, true) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::diagnostic(Ranker, model(glicko2_ranker)).

	test(glicko2_ranker_options_2, deterministic) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::ranker_options(Ranker, Options),
		memberchk(initial_rating(1500.0), Options),
		memberchk(initial_deviation(350.0), Options),
		memberchk(initial_volatility(0.06), Options),
		memberchk(tau(0.5), Options),
		memberchk(volatility_tolerance(1.0e-6), Options).

	test(glicko2_export_to_clauses_4, deterministic(ground(Clause))) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(glicko2_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		glicko2_ranker::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(glicko2_invalid_option_error, error(domain_error(option, passes(2)))) :-
		glicko2_ranker::learn(regular_head_to_head, _Ranker, [passes(2)]).

	test(glicko2_fractional_weight_error, error(type_error(integer, 1.5))) :-
		glicko2_ranker::learn(fractional_weight_pairwise, _Ranker).

	test(glicko2_non_list_options_error, error(type_error(list, options))) :-
		glicko2_ranker::learn(regular_head_to_head, _Ranker, options).

	test(glicko2_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		glicko2_ranker::learn(malformed_pairwise, _Ranker).

	test(glicko2_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		glicko2_ranker::learn(malformed_duplicate_items, _Ranker).

	test(glicko2_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		glicko2_ranker::learn(malformed_self_preference, _Ranker).

	test(glicko2_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		glicko2_ranker::learn(malformed_non_positive_weight, _Ranker).

	test(glicko2_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		glicko2_ranker::learn(disconnected_pairwise, _Ranker).

	test(glicko2_rank_variable_candidate_error, error(instantiation_error)) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(glicko2_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(glicko2_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::rank(Ranker, alpha, _Ranking).

	test(glicko2_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(glicko2_valid_ranker_1, deterministic) :-
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::valid_ranker(Ranker).

	test(glicko2_invalid_valid_ranker_1, fail) :-
		glicko2_ranker::valid_ranker(fake_ranker([alpha], [alpha-1500.0], [model(fake)])).

	test(glicko2_rank_invalid_ranker_error, error(domain_error(glicko2_ranker, fake_ranker([alpha], [alpha-1500.0], [model(fake)])))) :-
		glicko2_ranker::rank(fake_ranker([alpha], [alpha-1500.0], [model(fake)]), [alpha], _Ranking).

	test(glicko2_scores_invalid_ranker_error, error(domain_error(glicko2_ranker, glicko2_ranker([alpha, beta], [alpha-high, beta-1400.0], [model(glicko2_ranker), options([initial_rating(1500.0), initial_deviation(350.0), initial_volatility(0.06), tau(0.5), volatility_tolerance(1.0e-6)]), rating_deviations([alpha-350.0, beta-350.0]), volatilities([alpha-0.06, beta-0.06]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		glicko2_ranker::scores(glicko2_ranker([alpha, beta], [alpha-high, beta-1400.0], [model(glicko2_ranker), options([initial_rating(1500.0), initial_deviation(350.0), initial_volatility(0.06), tau(0.5), volatility_tolerance(1.0e-6)]), rating_deviations([alpha-350.0, beta-350.0]), volatilities([alpha-0.06, beta-0.06]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(glicko2_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		glicko2_ranker::learn(regular_head_to_head, Ranker),
		glicko2_ranker::print_ranker(Ranker).

:- end_object.
