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
		date is 2026-05-07,
		comment is 'Unit tests for the "elo_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(elo_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(elo_learn_2, deterministic(ground(Ranker))) :-
		elo_ranker::learn(regular_head_to_head, Ranker).

		test(elo_learn_3_custom_options, deterministic([InitialRating, KFactor, RatingScale] == [1400.0, 24.0, 200.0])) :-
		elo_ranker::learn(regular_head_to_head, elo_ranker(_Items, _Ratings, Diagnostics), [initial_rating(1400.0), k_factor(24.0), rating_scale(200.0)]),
		memberchk(options(Options), Diagnostics),
				memberchk(initial_rating(InitialRating), Options),
				memberchk(k_factor(KFactor), Options),
				memberchk(rating_scale(RatingScale), Options).

	test(elo_singleton_scores_2, deterministic(Scores == [alpha-1500.0])) :-
		elo_ranker::learn(singleton_pairwise, Ranker),
		elo_ranker::scores(Ranker, Scores).

	test(elo_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(elo_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		elo_ranker::learn(head_to_head, Ranker),
		elo_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(elo_two_item_scores_2, deterministic((abs(Alpha - 1523.8009426289977) =< 1.0e-6, abs(Beta - 1476.1990573710023) =< 1.0e-6))) :-
		elo_ranker::learn(two_item_head_to_head, Ranker),
		elo_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

		test(elo_diagnostics_2, deterministic([Model, InitialRating, KFactor, RatingScale] == [elo_ranker, 1500.0, 32.0, 400.0])) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::diagnostics(Ranker, Diagnostics),
				memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
				memberchk(initial_rating(InitialRating), Options),
				memberchk(k_factor(KFactor), Options),
				memberchk(rating_scale(RatingScale), Options).

	test(elo_diagnostic_2, true) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::diagnostic(Ranker, model(elo_ranker)).

		test(elo_ranker_options_2, deterministic([InitialRating, KFactor, RatingScale] == [1500.0, 32.0, 400.0])) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::ranker_options(Ranker, Options),
				memberchk(initial_rating(InitialRating), Options),
				memberchk(k_factor(KFactor), Options),
				memberchk(rating_scale(RatingScale), Options).

	test(elo_export_to_clauses_4, deterministic(ground(Clause))) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(elo_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		elo_ranker::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(elo_invalid_option_error, error(domain_error(option, passes(2)))) :-
		elo_ranker::learn(regular_head_to_head, _Ranker, [passes(2)]).

	test(elo_fractional_weight_error, error(type_error(integer, 1.5))) :-
		elo_ranker::learn(fractional_weight_pairwise, _Ranker).

	test(elo_non_list_options_error, error(type_error(list, options))) :-
		elo_ranker::learn(regular_head_to_head, _Ranker, options).

	test(elo_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		elo_ranker::learn(malformed_pairwise, _Ranker).

	test(elo_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		elo_ranker::learn(malformed_duplicate_items, _Ranker).

	test(elo_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		elo_ranker::learn(malformed_self_preference, _Ranker).

	test(elo_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		elo_ranker::learn(malformed_non_positive_weight, _Ranker).

	test(elo_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		elo_ranker::learn(disconnected_pairwise, _Ranker).

	test(elo_rank_variable_candidate_error, error(instantiation_error)) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(elo_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(elo_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::rank(Ranker, alpha, _Ranking).

	test(elo_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(elo_valid_ranker_1, deterministic) :-
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::valid_ranker(Ranker).

	test(elo_invalid_valid_ranker_1, fail) :-
		elo_ranker::valid_ranker(fake_ranker([alpha], [alpha-1500.0], [model(fake)])).

	test(elo_rank_invalid_ranker_error, error(domain_error(elo_ranker, fake_ranker([alpha], [alpha-1500.0], [model(fake)])))) :-
		elo_ranker::rank(fake_ranker([alpha], [alpha-1500.0], [model(fake)]), [alpha], _Ranking).

	test(elo_scores_invalid_ranker_error, error(domain_error(elo_ranker, elo_ranker([alpha, beta], [alpha-high, beta-1400.0], [model(elo_ranker), options([initial_rating(1500.0), k_factor(32.0), rating_scale(400.0)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		elo_ranker::scores(elo_ranker([alpha, beta], [alpha-high, beta-1400.0], [model(elo_ranker), options([initial_rating(1500.0), k_factor(32.0), rating_scale(400.0)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(elo_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		elo_ranker::learn(regular_head_to_head, Ranker),
		elo_ranker::print_ranker(Ranker).

:- end_object.
