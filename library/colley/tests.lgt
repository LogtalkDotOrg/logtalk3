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
		date is 2026-04-26,
		comment is 'Unit tests for the "colley" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(colley).

	cleanup :-
		^^clean_file('test_output.pl').

	test(colley_learn_2, deterministic(ground(Ranker))) :-
		colley::learn(regular_head_to_head, Ranker).

	test(colley_learn_3_empty_options, deterministic(ground(Ranker))) :-
		colley::learn(regular_head_to_head, Ranker, []).

	test(colley_learn_3_non_empty_options_error, error(domain_error(option, prior(flat)))) :-
		colley::learn(regular_head_to_head, _Ranker, [prior(flat)]).

	test(colley_singleton_scores_2, deterministic(Scores == [alpha-0.5])) :-
		colley::learn(singleton_pairwise, Ranker),
		colley::scores(Ranker, Scores).

	test(colley_two_item_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), abs(Alpha - 0.6) =< 1.0e-6, abs(Beta - 0.4) =< 1.0e-6))) :-
		colley::learn(two_item_head_to_head, Ranker),
		colley::scores(Ranker, Scores).

	test(colley_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(colley_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		colley::learn(head_to_head, Ranker),
		colley::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(colley_cyclic_pairwise_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), abs(Alpha - 0.5) =< 1.0e-6, abs(Beta - 0.5) =< 1.0e-6, abs(Gamma - 0.5) =< 1.0e-6))) :-
		colley::learn(cyclic_pairwise, Ranker),
		colley::scores(Ranker, Scores).

	test(colley_diagnostics_2, deterministic((memberchk(model(colley), Diagnostics), memberchk(options([]), Diagnostics)))) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::diagnostics(Ranker, Diagnostics).

	test(colley_diagnostic_2, true) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::diagnostic(Ranker, model(colley)).

	test(colley_ranker_options_2, deterministic(Options == [])) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::ranker_options(Ranker, Options).

	test(colley_export_to_clauses_4, deterministic(ground(Clause))) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(colley_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		colley::learn(regular_head_to_head, Ranker),
		colley::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		colley::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(colley_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		colley::learn(malformed_pairwise, _Ranker).

	test(colley_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		colley::learn(malformed_duplicate_items, _Ranker).

	test(colley_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		colley::learn(malformed_self_preference, _Ranker).

	test(colley_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		colley::learn(malformed_non_positive_weight, _Ranker).

	test(colley_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		colley::learn(disconnected_pairwise, _Ranker).

	test(colley_rank_variable_candidate_error, error(instantiation_error)) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::rank(Ranker, [_Candidate, beta], _Ranking).

	test(colley_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::rank(Ranker, [alpha, phantom], _Ranking).

	test(colley_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::rank(Ranker, alpha, _Ranking).

	test(colley_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(colley_rank_invalid_ranker_error, error(domain_error(colley_ranker, fake_ranker([alpha], [alpha-0.5], [model(fake)])))) :-
		colley::rank(fake_ranker([alpha], [alpha-0.5], [model(fake)]), [alpha], _Ranking).

	test(colley_scores_invalid_ranker_error, error(domain_error(colley_ranker, colley_ranker([alpha, beta], [alpha-1.1, beta- -0.1], [model(colley), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		colley::scores(colley_ranker([alpha, beta], [alpha-1.1, beta- -0.1], [model(colley), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(colley_print_ranker_1, deterministic) :-
		colley::learn(regular_head_to_head, Ranker),
		colley::print_ranker(Ranker).

:- end_object.
