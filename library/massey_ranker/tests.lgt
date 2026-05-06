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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "massey_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(massey_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(massey_learn_2, deterministic(ground(Ranker))) :-
		massey_ranker::learn(regular_head_to_head, Ranker).

	test(massey_learn_3_empty_options, deterministic(ground(Ranker))) :-
		massey_ranker::learn(regular_head_to_head, Ranker, []).

	test(massey_learn_3_non_empty_options_error, error(domain_error(option, prior(flat)))) :-
		massey_ranker::learn(regular_head_to_head, _Ranker, [prior(flat)]).

	test(massey_singleton_scores_2, deterministic(Scores == [alpha-0.0])) :-
		massey_ranker::learn(singleton_pairwise, Ranker),
		massey_ranker::scores(Ranker, Scores).

	test(massey_two_item_scores_2, deterministic((abs(Alpha - 0.25) =< 1.0e-6, abs(Beta + 0.25) =< 1.0e-6))) :-
		massey_ranker::learn(two_item_head_to_head, Ranker),
		massey_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(massey_regular_head_to_head_scores_2, deterministic((abs(Alpha - 0.42454672245467223) =< 1.0e-6, abs(Beta - 0.05662482566248254) =< 1.0e-6, abs(Gamma + 0.12329149232914928) =< 1.0e-6, abs(Delta + 0.3578800557880056) =< 1.0e-6))) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores).

	test(massey_head_to_head_scores_2, deterministic((abs(Alpha - 0.6694915254237287) =< 1.0e-6, abs(Beta - 0.19491525423728803) =< 1.0e-6, abs(Gamma + 0.14406779661016958) =< 1.0e-6, abs(Delta + 0.7203389830508475) =< 1.0e-6))) :-
		massey_ranker::learn(head_to_head, Ranker),
		massey_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores).

	test(massey_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(massey_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		massey_ranker::learn(head_to_head, Ranker),
		massey_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(massey_cyclic_pairwise_scores_2, deterministic((abs(Alpha) =< 1.0e-6, abs(Beta) =< 1.0e-6, abs(Gamma) =< 1.0e-6))) :-
		massey_ranker::learn(cyclic_pairwise, Ranker),
		massey_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(massey_scores_zero_sum_2, deterministic(abs(Total) =< 1.0e-9)) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores),
		Total is Alpha + Beta + Gamma + Delta.

	test(massey_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		massey_ranker::learn(cyclic_pairwise, Ranker),
		massey_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(massey_diagnostics_2, deterministic(true)) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(model(massey_ranker), Diagnostics),
		memberchk(options([]), Diagnostics).

	test(massey_diagnostic_2, true) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::diagnostic(Ranker, model(massey_ranker)).

	test(massey_ranker_options_2, deterministic(Options == [])) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::ranker_options(Ranker, Options).

	test(massey_export_to_clauses_4, deterministic(ground(Clause))) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(massey_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		massey_ranker::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(massey_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		massey_ranker::learn(malformed_pairwise, _Ranker).

	test(massey_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		massey_ranker::learn(malformed_duplicate_items, _Ranker).

	test(massey_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		massey_ranker::learn(malformed_self_preference, _Ranker).

	test(massey_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		massey_ranker::learn(malformed_non_positive_weight, _Ranker).

	test(massey_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		massey_ranker::learn(disconnected_pairwise, _Ranker).

	test(massey_rank_variable_candidate_error, error(instantiation_error)) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(massey_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(massey_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::rank(Ranker, alpha, _Ranking).

	test(massey_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(massey_valid_ranker_1, deterministic) :-
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::valid_ranker(Ranker).

	test(massey_invalid_valid_ranker_1, fail) :-
		massey_ranker::valid_ranker(fake_ranker([alpha], [alpha-0.0], [model(fake)])).

	test(massey_rank_invalid_ranker_error, error(domain_error(massey_ranker, fake_ranker([alpha], [alpha-0.0], [model(fake)])))) :-
		massey_ranker::rank(fake_ranker([alpha], [alpha-0.0], [model(fake)]), [alpha], _Ranking).

	test(massey_scores_invalid_ranker_error, error(domain_error(massey_ranker, massey_ranker([alpha, beta], [alpha-foo, beta-0.0], [model(massey_ranker), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		massey_ranker::scores(massey_ranker([alpha, beta], [alpha-foo, beta-0.0], [model(massey_ranker), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(massey_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		massey_ranker::learn(regular_head_to_head, Ranker),
		massey_ranker::print_ranker(Ranker).

:- end_object.
