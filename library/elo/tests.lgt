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
		date is 2026-04-25,
		comment is 'Unit tests for the "elo" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(elo).

	cleanup :-
		^^clean_file('test_output.pl').

	test(elo_learn_2, deterministic(ground(Ranker))) :-
		elo::learn(regular_head_to_head, Ranker).

	test(elo_learn_3_custom_options, deterministic((memberchk(options(Options), Diagnostics), memberchk(initial_rating(1400.0), Options), memberchk(k_factor(24.0), Options), memberchk(rating_scale(200.0), Options)))) :-
		elo::learn(regular_head_to_head, elo_ranker(_Items, _Ratings, Diagnostics), [initial_rating(1400.0), k_factor(24.0), rating_scale(200.0)]).

	test(elo_singleton_rating_2, deterministic(Ratings == [alpha-1500.0])) :-
		elo::learn(singleton_pairwise, Ranker),
		elo::ratings(Ranker, Ratings).

	test(elo_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(elo_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		elo::learn(head_to_head, Ranker),
		elo::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(elo_two_item_ratings_2, deterministic((memberchk(alpha-Alpha, Ratings), memberchk(beta-Beta, Ratings), abs(Alpha - 1523.8009426289977) =< 1.0e-6, abs(Beta - 1476.1990573710023) =< 1.0e-6))) :-
		elo::learn(two_item_head_to_head, Ranker),
		elo::ratings(Ranker, Ratings).

	test(elo_diagnostics_2, deterministic((memberchk(model(elo), Diagnostics), memberchk(options(Options), Diagnostics), memberchk(initial_rating(1500.0), Options), memberchk(k_factor(32.0), Options), memberchk(rating_scale(400.0), Options)))) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::diagnostics(Ranker, Diagnostics).

	test(elo_diagnostic_2, true) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::diagnostic(Ranker, model(elo)).

	test(elo_ranker_options_2, deterministic((memberchk(initial_rating(1500.0), Options), memberchk(k_factor(32.0), Options), memberchk(rating_scale(400.0), Options)))) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::ranker_options(Ranker, Options).

	test(elo_export_to_clauses_4, deterministic(ground(Clause))) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(elo_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		elo::learn(regular_head_to_head, Ranker),
		elo::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		elo::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(elo_invalid_option_error, error(domain_error(option, passes(2)))) :-
		elo::learn(regular_head_to_head, _Ranker, [passes(2)]).

	test(elo_fractional_weight_error, error(type_error(integer, 1.5))) :-
		elo::learn(fractional_weight_pairwise, _Ranker).

	test(elo_non_list_options_error, error(type_error(list, options))) :-
		elo::learn(regular_head_to_head, _Ranker, options).

	test(elo_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		elo::learn(malformed_pairwise, _Ranker).

	test(elo_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		elo::learn(malformed_duplicate_items, _Ranker).

	test(elo_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		elo::learn(malformed_self_preference, _Ranker).

	test(elo_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		elo::learn(malformed_non_positive_weight, _Ranker).

	test(elo_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		elo::learn(disconnected_pairwise, _Ranker).

	test(elo_rank_variable_candidate_error, error(instantiation_error)) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::rank(Ranker, [_Candidate, beta], _Ranking).

	test(elo_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::rank(Ranker, [alpha, phantom], _Ranking).

	test(elo_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::rank(Ranker, alpha, _Ranking).

	test(elo_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(elo_rank_invalid_ranker_error, error(domain_error(elo_ranker, fake_ranker([alpha], [alpha-1500.0], [model(fake)])))) :-
		elo::rank(fake_ranker([alpha], [alpha-1500.0], [model(fake)]), [alpha], _Ranking).

	test(elo_ratings_invalid_ranker_error, error(domain_error(elo_ranker, elo_ranker([alpha, beta], [alpha-high, beta-1400.0], [model(elo), options([initial_rating(1500.0), k_factor(32.0), rating_scale(400.0)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		elo::ratings(elo_ranker([alpha, beta], [alpha-high, beta-1400.0], [model(elo), options([initial_rating(1500.0), k_factor(32.0), rating_scale(400.0)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Ratings).

	test(elo_print_ranker_1, deterministic) :-
		elo::learn(regular_head_to_head, Ranker),
		elo::print_ranker(Ranker).

:- end_object.
