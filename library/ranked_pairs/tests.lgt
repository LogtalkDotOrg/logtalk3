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


:- object(strong_path_pairwise,
	implements(pairwise_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).

	preference(alpha, beta, 10).
	preference(beta, alpha, 1).
	preference(beta, gamma, 10).
	preference(gamma, beta, 1).
	preference(gamma, alpha, 6).
	preference(alpha, gamma, 5).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-05,
		comment is 'Unit tests for the "ranked_pairs" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(ranked_pairs).

	cleanup :-
		^^clean_file('test_output.pl').

	test(ranked_pairs_learn_2, deterministic(ground(Ranker))) :-
		ranked_pairs::learn(regular_head_to_head, Ranker).

	test(ranked_pairs_singleton_scores_2, deterministic(Scores == [alpha-0])) :-
		ranked_pairs::learn(singleton_pairwise, Ranker),
		ranked_pairs::scores(Ranker, Scores).

	test(ranked_pairs_singleton_locked_pairs_2, deterministic(LockedPairs == [])) :-
		ranked_pairs::learn(singleton_pairwise, Ranker),
		ranked_pairs::locked_pairs(Ranker, LockedPairs).

	test(ranked_pairs_learn_3_custom_options, deterministic(memberchk(options([victory_strength(margins), tie_breaking(term_order)]), Diagnostics))) :-
		ranked_pairs::learn(regular_head_to_head, ranked_pairs_ranker(_Items, _Scores, Diagnostics), [victory_strength(margins)]).

	test(ranked_pairs_learn_3_tie_breaking_option, deterministic(memberchk(options([tie_breaking(declaration_order), victory_strength(winning_votes)]), Diagnostics))) :-
		ranked_pairs::learn(regular_head_to_head, ranked_pairs_ranker(_Items, _Scores, Diagnostics), [tie_breaking(declaration_order)]).

	test(ranked_pairs_learn_3_unknown_option_error, error(domain_error(option, tie_policy(ignore_missing)))) :-
		ranked_pairs::learn(regular_head_to_head, _Ranker, [tie_policy(ignore_missing)]).

	test(ranked_pairs_learn_3_invalid_tie_breaking_error, error(domain_error(option, tie_breaking(random)))) :-
		ranked_pairs::learn(regular_head_to_head, _Ranker, [tie_breaking(random)]).

	test(ranked_pairs_learn_3_non_list_options_error, error(type_error(list, options))) :-
		ranked_pairs::learn(regular_head_to_head, _Ranker, options).

	test(ranked_pairs_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(ranked_pairs_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		ranked_pairs::learn(head_to_head, Ranker),
		ranked_pairs::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(ranked_pairs_cyclic_pairwise_scores_2, deterministic((memberchk(alpha-2, Scores), memberchk(beta-1, Scores), memberchk(gamma-0, Scores)))) :-
		ranked_pairs::learn(cyclic_pairwise, Ranker),
		ranked_pairs::scores(Ranker, Scores).

	test(ranked_pairs_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		ranked_pairs::learn(cyclic_pairwise, Ranker),
		ranked_pairs::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(ranked_pairs_strong_path_pairwise_scores_2, deterministic((memberchk(alpha-2, Scores), memberchk(beta-1, Scores), memberchk(gamma-0, Scores)))) :-
		ranked_pairs::learn(strong_path_pairwise, Ranker),
		ranked_pairs::scores(Ranker, Scores).

	test(ranked_pairs_strong_path_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		ranked_pairs::learn(strong_path_pairwise, Ranker),
		ranked_pairs::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(ranked_pairs_locked_pairs_2, deterministic(LockedPairs == [lock(alpha, beta, 10), lock(beta, gamma, 10)])) :-
		ranked_pairs::learn(strong_path_pairwise, Ranker),
		ranked_pairs::locked_pairs(Ranker, LockedPairs).

	test(ranked_pairs_condorcet_divergence_rank_3, deterministic(Ranking == [alpha, delta, beta, gamma])) :-
		ranked_pairs::learn(condorcet_divergence_pairwise, Ranker),
		ranked_pairs::rank(Ranker, [gamma, beta, delta, alpha], Ranking).

	test(ranked_pairs_condorcet_divergence_locked_pairs_2, deterministic(LockedPairs == [lock(alpha, beta, 1), lock(alpha, delta, 1), lock(alpha, gamma, 1), lock(beta, gamma, 1), lock(delta, beta, 1)])) :-
		ranked_pairs::learn(condorcet_divergence_pairwise, Ranker),
		ranked_pairs::locked_pairs(Ranker, LockedPairs).

	test(ranked_pairs_condorcet_divergence_declaration_order_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		ranked_pairs::learn(condorcet_divergence_pairwise, Ranker, [tie_breaking(declaration_order)]),
		ranked_pairs::rank(Ranker, [gamma, beta, delta, alpha], Ranking).

	test(ranked_pairs_condorcet_divergence_declaration_order_locked_pairs_2, deterministic(LockedPairs == [lock(alpha, beta, 1), lock(alpha, gamma, 1), lock(alpha, delta, 1), lock(beta, gamma, 1), lock(gamma, delta, 1)])) :-
		ranked_pairs::learn(condorcet_divergence_pairwise, Ranker, [tie_breaking(declaration_order)]),
		ranked_pairs::locked_pairs(Ranker, LockedPairs).

	test(condorcet_methods_divergence_rankings, deterministic((SchulzeRanking == [alpha, beta, delta, gamma], RankedPairsRanking == [alpha, delta, beta, gamma], SchulzeRanking \== RankedPairsRanking))) :-
		schulze::learn(condorcet_divergence_pairwise, SchulzeRanker),
		schulze::rank(SchulzeRanker, [gamma, beta, delta, alpha], SchulzeRanking),
		ranked_pairs::learn(condorcet_divergence_pairwise, RankedPairsRanker),
		ranked_pairs::rank(RankedPairsRanker, [gamma, beta, delta, alpha], RankedPairsRanking).

	test(ranked_pairs_diagnostics_2, deterministic((memberchk(model(ranked_pairs), Diagnostics), memberchk(options([victory_strength(winning_votes), tie_breaking(term_order)]), Diagnostics)))) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::diagnostics(Ranker, Diagnostics).

	test(ranked_pairs_diagnostic_2, true) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::diagnostic(Ranker, model(ranked_pairs)).

	test(ranked_pairs_ranker_options_2, deterministic(Options == [victory_strength(winning_votes), tie_breaking(term_order)])) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::ranker_options(Ranker, Options).

	test(ranked_pairs_export_to_clauses_4, deterministic(ground(Clause))) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(ranked_pairs_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		ranked_pairs::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(ranked_pairs_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		ranked_pairs::learn(malformed_pairwise, _Ranker).

	test(ranked_pairs_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		ranked_pairs::learn(malformed_duplicate_items, _Ranker).

	test(ranked_pairs_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		ranked_pairs::learn(malformed_self_preference, _Ranker).

	test(ranked_pairs_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		ranked_pairs::learn(malformed_non_positive_weight, _Ranker).

	test(ranked_pairs_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		ranked_pairs::learn(disconnected_pairwise, _Ranker).

	test(ranked_pairs_rank_variable_candidate_error, error(instantiation_error)) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::rank(Ranker, [_Candidate, beta], _Ranking).

	test(ranked_pairs_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::rank(Ranker, [alpha, phantom], _Ranking).

	test(ranked_pairs_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::rank(Ranker, alpha, _Ranking).

	test(ranked_pairs_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(ranked_pairs_valid_ranker_1, deterministic) :-
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::valid_ranker(Ranker).

	test(ranked_pairs_invalid_valid_ranker_1, fail) :-
		ranked_pairs::valid_ranker(fake_ranker([alpha], [alpha-0], [model(fake)])).

	test(ranked_pairs_rank_invalid_ranker_error, error(domain_error(ranked_pairs_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		ranked_pairs::rank(fake_ranker([alpha], [alpha-0], [model(fake)]), [alpha], _Ranking).

	test(ranked_pairs_scores_invalid_ranker_error, error(domain_error(ranked_pairs_ranker, ranked_pairs_ranker([alpha, beta], [alpha-1.5, beta-0], [model(ranked_pairs), options([victory_strength(winning_votes), tie_breaking(term_order)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		ranked_pairs::scores(ranked_pairs_ranker([alpha, beta], [alpha-1.5, beta-0], [model(ranked_pairs), options([victory_strength(winning_votes), tie_breaking(term_order)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(ranked_pairs_locked_pairs_invalid_ranker_error, error(domain_error(ranked_pairs_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		ranked_pairs::locked_pairs(fake_ranker([alpha], [alpha-0], [model(fake)]), _LockedPairs).

	test(ranked_pairs_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		ranked_pairs::learn(regular_head_to_head, Ranker),
		ranked_pairs::print_ranker(Ranker).

:- end_object.
