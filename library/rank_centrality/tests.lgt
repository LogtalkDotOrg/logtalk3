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


:- object(asymmetric_pairwise,
	implements(pairwise_ranking_dataset_protocol)).

	item(alpha).
	item(beta).
	item(gamma).

	preference(alpha, beta, 9).
	preference(beta, alpha, 1).
	preference(alpha, gamma, 8).
	preference(gamma, alpha, 2).
	preference(beta, gamma, 6).
	preference(gamma, beta, 4).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-25,
		comment is 'Unit tests for the "rank_centrality" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(rank_centrality).

	cleanup :-
		^^clean_file('test_output.pl').

	test(rank_centrality_learn_2, deterministic(ground(Ranker))) :-
		rank_centrality::learn(regular_head_to_head, Ranker).

	test(rank_centrality_singleton_scores_2, deterministic(Scores == [alpha-1.0])) :-
		rank_centrality::learn(singleton_pairwise, Ranker),
		rank_centrality::scores(Ranker, Scores).

	test(rank_centrality_singleton_diagnostics_2, deterministic((memberchk(convergence(converged), Diagnostics), memberchk(iterations(0), Diagnostics), memberchk(final_delta(0.0), Diagnostics), memberchk(maximum_degree(0), Diagnostics)))) :-
		rank_centrality::learn(singleton_pairwise, Ranker),
		rank_centrality::diagnostics(Ranker, Diagnostics).

	test(rank_centrality_learn_3_custom_options, deterministic(memberchk(options([maximum_iterations(50), tolerance(1.0e-9)]), Diagnostics))) :-
		rank_centrality::learn(regular_head_to_head, rank_centrality_ranker(_Items, _Scores, Diagnostics), [maximum_iterations(50), tolerance(1.0e-9)]).

	test(rank_centrality_learn_3_unknown_option_error, error(domain_error(option, tie_policy(ignore_missing)))) :-
		rank_centrality::learn(regular_head_to_head, _Ranker, [tie_policy(ignore_missing)]).

	test(rank_centrality_learn_3_non_list_options_error, error(type_error(list, options))) :-
		rank_centrality::learn(regular_head_to_head, _Ranker, options).

	test(rank_centrality_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(rank_centrality_rank_subset_3, deterministic(Ranking == [alpha, gamma, delta])) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::rank(Ranker, [gamma, delta, alpha], Ranking).

	test(rank_centrality_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), memberchk(delta-Delta, Scores), Alpha > Beta, Beta > Gamma, Gamma > Delta, Sum is Alpha + Beta + Gamma + Delta, abs(Sum - 1.0) =< 1.0e-6))) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::scores(Ranker, Scores).

	test(rank_centrality_two_item_closed_form_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), abs(Alpha - 0.75) =< 1.0e-6, abs(Beta - 0.25) =< 1.0e-6))) :-
		rank_centrality::learn(two_item_head_to_head, Ranker),
		rank_centrality::scores(Ranker, Scores).

	test(rank_centrality_cyclic_pairwise_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), abs(Alpha - Beta) =< 1.0e-6, abs(Beta - Gamma) =< 1.0e-6, abs(Gamma - (1.0/3.0)) =< 1.0e-6))) :-
		rank_centrality::learn(cyclic_pairwise, Ranker),
		rank_centrality::scores(Ranker, Scores).

	test(rank_centrality_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		rank_centrality::learn(cyclic_pairwise, Ranker),
		rank_centrality::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(rank_centrality_asymmetric_pairwise_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), abs(Alpha - 0.7383177570) =< 1.0e-6, abs(Beta - 0.1214953271) =< 1.0e-6, abs(Gamma - 0.1401869159) =< 1.0e-6, Alpha > Gamma, Gamma > Beta))) :-
		rank_centrality::learn(asymmetric_pairwise, Ranker),
		rank_centrality::scores(Ranker, Scores).

	test(rank_centrality_diagnostics_2, deterministic((memberchk(model(rank_centrality), Diagnostics), memberchk(convergence(converged), Diagnostics), memberchk(maximum_degree(3), Diagnostics)))) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::diagnostics(Ranker, Diagnostics).

	test(rank_centrality_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::diagnostics(Ranker, Diagnostics),
		findall(Diagnostic, rank_centrality::diagnostic(Ranker, Diagnostic), Enumerated).

	test(rank_centrality_ranker_options_2, deterministic(Options == [maximum_iterations(5000), tolerance(1.0e-8)])) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::ranker_options(Ranker, Options).

	test(rank_centrality_maximum_iterations_exhausted_diagnostics_2, deterministic((memberchk(convergence(maximum_iterations_exhausted), Diagnostics), memberchk(iterations(1), Diagnostics), memberchk(final_delta(FinalDelta), Diagnostics), FinalDelta > 0.0, memberchk(options([maximum_iterations(1), tolerance(1.0e-12)]), Diagnostics)))) :-
		rank_centrality::learn(asymmetric_pairwise, Ranker, [maximum_iterations(1), tolerance(1.0e-12)]),
		rank_centrality::diagnostics(Ranker, Diagnostics).

	test(rank_centrality_export_to_clauses_4, deterministic(ground(Clause))) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(rank_centrality_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		rank_centrality::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(rank_centrality_export_to_file_4_round_trip, deterministic((LoadedScores == Scores, LoadedDiagnostics == Diagnostics))) :-
		^^file_path('test_output.pl', File),
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::scores(Ranker, Scores),
		rank_centrality::diagnostics(Ranker, Diagnostics),
		rank_centrality::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		rank_centrality::scores(LoadedRanker, LoadedScores),
		rank_centrality::diagnostics(LoadedRanker, LoadedDiagnostics).

	test(rank_centrality_non_regular_pairwise_error, error(domain_error(rank_centrality_regular_dataset, _))) :-
		rank_centrality::learn(head_to_head, _Ranker).

	test(rank_centrality_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		rank_centrality::learn(malformed_pairwise, _Ranker).

	test(rank_centrality_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		rank_centrality::learn(malformed_duplicate_items, _Ranker).

	test(rank_centrality_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		rank_centrality::learn(malformed_self_preference, _Ranker).

	test(rank_centrality_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		rank_centrality::learn(malformed_non_positive_weight, _Ranker).

	test(rank_centrality_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		rank_centrality::learn(disconnected_pairwise, _Ranker).

	test(rank_centrality_rank_variable_candidate_error, error(instantiation_error)) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::rank(Ranker, [_Candidate, beta], _Ranking).

	test(rank_centrality_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::rank(Ranker, [alpha, phantom], _Ranking).

	test(rank_centrality_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::rank(Ranker, alpha, _Ranking).

	test(rank_centrality_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(rank_centrality_rank_invalid_ranker_error, error(domain_error(rank_centrality_ranker, fake_ranker([alpha], [alpha-1.0], [model(fake)])))) :-
		rank_centrality::rank(fake_ranker([alpha], [alpha-1.0], [model(fake)]), [alpha], _Ranking).

	test(rank_centrality_rank_non_normalized_ranker_accepted, deterministic(Ranking == [alpha, beta])) :-
		rank_centrality::rank(rank_centrality_ranker([alpha, beta], [alpha-0.8, beta-0.3], [model(rank_centrality), options([maximum_iterations(5000), tolerance(1.0e-8)]), convergence(converged), iterations(3), final_delta(0.0), maximum_degree(1), dataset_summary([items(2), preferences(4), connected_components(1), isolated_items([])])]), [alpha, beta], Ranking).

	test(rank_centrality_scores_non_normalized_ranker_accepted, deterministic(Scores == [alpha-0.8, beta-0.3])) :-
		rank_centrality::scores(rank_centrality_ranker([alpha, beta], [alpha-0.8, beta-0.3], [model(rank_centrality), options([maximum_iterations(5000), tolerance(1.0e-8)]), convergence(converged), iterations(3), final_delta(0.0), maximum_degree(1), dataset_summary([items(2), preferences(4), connected_components(1), isolated_items([])])]), Scores).

	test(rank_centrality_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		rank_centrality::learn(regular_head_to_head, Ranker),
		rank_centrality::print_ranker(Ranker).

:- end_object.
