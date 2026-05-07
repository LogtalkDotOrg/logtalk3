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
		date is 2026-05-07,
		comment is 'Unit tests for the "regularized_bradley_terry_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(regularized_bradley_terry_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(regularized_bradley_terry_learn_2, deterministic(ground(Ranker))) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker).

	test(regularized_bradley_terry_learn_3_custom_options, deterministic([MaximumIterations, Tolerance, GammaPrior, Prior] == [50, 1.0e-7, gamma(3.0, 4.0), gamma(3.0, 4.0)])) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, regularized_bt_ranker(_Items, _Strengths, Diagnostics), [maximum_iterations(50), tolerance(1.0e-7), gamma_prior(gamma(3.0, 4.0))]),
		memberchk(options(Options), Diagnostics),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(gamma_prior(GammaPrior), Options),
		memberchk(prior(Prior), Diagnostics).

	test(regularized_bradley_terry_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(regularized_bradley_terry_rank_irregular_pairwise_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		regularized_bradley_terry_ranker::learn(head_to_head, Ranker),
		regularized_bradley_terry_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(regularized_bradley_terry_rank_subset_3, deterministic(Ranking == [alpha, gamma, delta])) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::rank(Ranker, [gamma, delta, alpha], Ranking).

	test(regularized_bradley_terry_scores_2, deterministic((Alpha > Beta, Beta > Gamma, Gamma > Delta, Delta > 0.0))) :-
		regularized_bradley_terry_ranker::learn(head_to_head, Ranker),
		regularized_bradley_terry_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores).

	test(regularized_bradley_terry_diagnostics_2, deterministic([Model, Prior, Convergence] == [regularized_bradley_terry_ranker, gamma(2.0, 2.0), converged])) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(prior(Prior), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics).

	test(regularized_bradley_terry_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::diagnostics(Ranker, Diagnostics),
		findall(Diagnostic, regularized_bradley_terry_ranker::diagnostic(Ranker, Diagnostic), Enumerated).

	test(regularized_bradley_terry_two_item_closed_form_scores_2, deterministic((abs(Alpha - (2.0/3.0)) =< 1.0e-6, abs(Beta - (1.0/3.0)) =< 1.0e-6))) :-
		regularized_bradley_terry_ranker::learn(two_item_head_to_head, Ranker),
		regularized_bradley_terry_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(regularized_bradley_terry_cyclic_pairwise_scores_2, deterministic((abs(Alpha - Beta) =< 1.0e-6, abs(Beta - Gamma) =< 1.0e-6))) :-
		regularized_bradley_terry_ranker::learn(cyclic_pairwise, Ranker),
		regularized_bradley_terry_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(regularized_bradley_terry_sparse_preferences_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma]]))) :-
		regularized_bradley_terry_ranker::learn(sparse_preferences, _Ranker).

	test(regularized_bradley_terry_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		regularized_bradley_terry_ranker::learn(malformed_pairwise, _Ranker).

	test(regularized_bradley_terry_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		regularized_bradley_terry_ranker::learn(malformed_duplicate_items, _Ranker).

	test(regularized_bradley_terry_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		regularized_bradley_terry_ranker::learn(malformed_self_preference, _Ranker).

	test(regularized_bradley_terry_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		regularized_bradley_terry_ranker::learn(malformed_non_positive_weight, _Ranker).

	test(regularized_bradley_terry_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		regularized_bradley_terry_ranker::learn(disconnected_pairwise, _Ranker).

	test(regularized_bradley_terry_invalid_gamma_prior_shape_error, error(domain_error(option, gamma_prior(gamma(1.0, 2.0))))) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, _Ranker, [gamma_prior(gamma(1.0, 2.0))]).

	test(regularized_bradley_terry_export_to_clauses_4, deterministic(ground(Clause))) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(regularized_bradley_terry_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		regularized_bradley_terry_ranker::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(regularized_bradley_terry_rank_variable_candidate_error, error(instantiation_error)) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(regularized_bradley_terry_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(regularized_bradley_terry_valid_ranker_1, deterministic) :-
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::valid_ranker(Ranker).

	test(regularized_bradley_terry_invalid_valid_ranker_1, fail) :-
		regularized_bradley_terry_ranker::valid_ranker(fake_ranker([alpha], [alpha-1.0], [model(fake)])).

	test(regularized_bradley_terry_rank_invalid_ranker_error, error(domain_error(regularized_bradley_terry_ranker, fake_ranker([alpha], [alpha-1.0], [model(fake)])))) :-
		regularized_bradley_terry_ranker::rank(fake_ranker([alpha], [alpha-1.0], [model(fake)]), [alpha], _Ranking).

	test(regularized_bradley_terry_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		regularized_bradley_terry_ranker::learn(regular_head_to_head, Ranker),
		regularized_bradley_terry_ranker::print_ranker(Ranker).

:- end_object.
