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
		date is 2026-05-05,
		comment is 'Unit tests for the "copeland" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(copeland).

	cleanup :-
		^^clean_file('test_output.pl').

	test(copeland_learn_2, deterministic(ground(Ranker))) :-
		copeland::learn(regular_head_to_head, Ranker).

	test(copeland_learn_3_empty_options, deterministic(ground(Ranker))) :-
		copeland::learn(regular_head_to_head, Ranker, []).

	test(copeland_learn_3_non_empty_options_error, error(domain_error(option, tie_policy(ignore_missing)))) :-
		copeland::learn(regular_head_to_head, _Ranker, [tie_policy(ignore_missing)]).

	test(copeland_learn_3_non_list_options_error, error(type_error(list, options))) :-
		copeland::learn(regular_head_to_head, _Ranker, options).

	test(copeland_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(copeland_rank_accepts_non_regular_pairwise_data, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		copeland::learn(head_to_head, Ranker),
		copeland::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(copeland_scores_2, deterministic((memberchk(alpha-3, Scores), memberchk(beta-1, Scores), memberchk(gamma- -1, Scores), memberchk(delta- -3, Scores)))) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::scores(Ranker, Scores).

	test(copeland_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		copeland::learn(cyclic_pairwise, Ranker),
		copeland::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(copeland_diagnostics_2, deterministic((memberchk(model(copeland), Diagnostics), memberchk(options([]), Diagnostics)))) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::diagnostics(Ranker, Diagnostics).

	test(copeland_diagnostic_2, true) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::diagnostic(Ranker, model(copeland)).

	test(copeland_ranker_options_2, deterministic(Options == [])) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::ranker_options(Ranker, Options).

	test(copeland_export_to_clauses_4, deterministic(ground(Clause))) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(copeland_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		copeland::learn(regular_head_to_head, Ranker),
		copeland::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		copeland::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(copeland_export_to_file_4_round_trip, deterministic((LoadedScores == Scores, LoadedDiagnostics == Diagnostics))) :-
		^^file_path('test_output.pl', File),
		copeland::learn(regular_head_to_head, Ranker),
		copeland::scores(Ranker, Scores),
		copeland::diagnostics(Ranker, Diagnostics),
		copeland::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		copeland::scores(LoadedRanker, LoadedScores),
		copeland::diagnostics(LoadedRanker, LoadedDiagnostics).

	test(copeland_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		copeland::learn(malformed_pairwise, _Ranker).

	test(copeland_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		copeland::learn(malformed_duplicate_items, _Ranker).

	test(copeland_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		copeland::learn(malformed_self_preference, _Ranker).

	test(copeland_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		copeland::learn(malformed_non_positive_weight, _Ranker).

	test(copeland_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		copeland::learn(disconnected_pairwise, _Ranker).

	test(copeland_rank_variable_candidate_error, error(instantiation_error)) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::rank(Ranker, [_Candidate, beta], _Ranking).

	test(copeland_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::rank(Ranker, [alpha, phantom], _Ranking).

	test(copeland_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::rank(Ranker, alpha, _Ranking).

	test(copeland_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(copeland_valid_ranker_1, deterministic) :-
		copeland::learn(regular_head_to_head, Ranker),
		copeland::valid_ranker(Ranker).

	test(copeland_invalid_valid_ranker_1, fail) :-
		copeland::valid_ranker(fake_ranker([alpha], [alpha-1], [model(fake)])).

	test(copeland_rank_invalid_ranker_error, error(domain_error(copeland_ranker, fake_ranker([alpha], [alpha-1], [model(fake)])))) :-
		copeland::rank(fake_ranker([alpha], [alpha-1], [model(fake)]), [alpha], _Ranking).

	test(copeland_rank_non_integer_score_ranker_error, error(domain_error(copeland_ranker, copeland_ranker([alpha, beta], [alpha-1.5, beta-0], [model(copeland), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		copeland::rank(copeland_ranker([alpha, beta], [alpha-1.5, beta-0], [model(copeland), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), [alpha, beta], _Ranking).

	test(copeland_scores_non_integer_score_ranker_error, error(domain_error(copeland_ranker, copeland_ranker([alpha, beta], [alpha-1.5, beta-0], [model(copeland), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		copeland::scores(copeland_ranker([alpha, beta], [alpha-1.5, beta-0], [model(copeland), options([]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(copeland_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		copeland::learn(regular_head_to_head, Ranker),
		copeland::print_ranker(Ranker).

:- end_object.
