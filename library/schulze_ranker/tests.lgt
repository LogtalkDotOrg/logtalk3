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
		date is 2026-05-06,
		comment is 'Unit tests for the "schulze_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(schulze_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(schulze_learn_2, deterministic(ground(Ranker))) :-
		schulze_ranker::learn(regular_head_to_head, Ranker).

	test(schulze_singleton_scores_2, deterministic(Scores == [alpha-0])) :-
		schulze_ranker::learn(singleton_pairwise, Ranker),
		schulze_ranker::scores(Ranker, Scores).

	test(schulze_singleton_strongest_paths_2, deterministic(StrongestPaths == [])) :-
		schulze_ranker::learn(singleton_pairwise, Ranker),
		schulze_ranker::strongest_paths(Ranker, StrongestPaths).

	test(schulze_learn_3_custom_options, deterministic(memberchk(options([victory_strength(margins)]), Diagnostics))) :-
		schulze_ranker::learn(regular_head_to_head, schulze_ranker(_Items, _Scores, Diagnostics), [victory_strength(margins)]).

	test(schulze_learn_3_unknown_option_error, error(domain_error(option, tie_policy(ignore_missing)))) :-
		schulze_ranker::learn(regular_head_to_head, _Ranker, [tie_policy(ignore_missing)]).

	test(schulze_learn_3_non_list_options_error, error(type_error(list, options))) :-
		schulze_ranker::learn(regular_head_to_head, _Ranker, options).

	test(schulze_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(schulze_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		schulze_ranker::learn(head_to_head, Ranker),
		schulze_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(schulze_cyclic_pairwise_scores_2, deterministic((memberchk(alpha-0, Scores), memberchk(beta-0, Scores), memberchk(gamma-0, Scores)))) :-
		schulze_ranker::learn(cyclic_pairwise, Ranker),
		schulze_ranker::scores(Ranker, Scores).

	test(schulze_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		schulze_ranker::learn(cyclic_pairwise, Ranker),
		schulze_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(schulze_strong_path_pairwise_scores_2, deterministic((memberchk(alpha-2, Scores), memberchk(beta-1, Scores), memberchk(gamma-0, Scores)))) :-
		schulze_ranker::learn(strong_path_pairwise, Ranker),
		schulze_ranker::scores(Ranker, Scores).

	test(schulze_strong_path_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		schulze_ranker::learn(strong_path_pairwise, Ranker),
		schulze_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(schulze_strongest_paths_2, deterministic((memberchk(path(alpha, beta, 10), StrongestPaths), memberchk(path(alpha, gamma, 10), StrongestPaths), memberchk(path(beta, alpha, 6), StrongestPaths), memberchk(path(beta, gamma, 10), StrongestPaths), memberchk(path(gamma, alpha, 6), StrongestPaths), memberchk(path(gamma, beta, 6), StrongestPaths)))) :-
		schulze_ranker::learn(strong_path_pairwise, Ranker),
		schulze_ranker::strongest_paths(Ranker, StrongestPaths).

	test(schulze_diagnostics_2, deterministic((memberchk(model(schulze_ranker), Diagnostics), memberchk(options([victory_strength(winning_votes)]), Diagnostics)))) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::diagnostics(Ranker, Diagnostics).

	test(schulze_diagnostic_2, true) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::diagnostic(Ranker, model(schulze_ranker)).

	test(schulze_ranker_options_2, deterministic(Options == [victory_strength(winning_votes)])) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::ranker_options(Ranker, Options).

	test(schulze_export_to_clauses_4, deterministic(ground(Clause))) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(schulze_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		schulze_ranker::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(schulze_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		schulze_ranker::learn(malformed_pairwise, _Ranker).

	test(schulze_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		schulze_ranker::learn(malformed_duplicate_items, _Ranker).

	test(schulze_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		schulze_ranker::learn(malformed_self_preference, _Ranker).

	test(schulze_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		schulze_ranker::learn(malformed_non_positive_weight, _Ranker).

	test(schulze_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		schulze_ranker::learn(disconnected_pairwise, _Ranker).

	test(schulze_rank_variable_candidate_error, error(instantiation_error)) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(schulze_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(schulze_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::rank(Ranker, alpha, _Ranking).

	test(schulze_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(schulze_valid_ranker_1, deterministic) :-
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::valid_ranker(Ranker).

	test(schulze_invalid_valid_ranker_1, fail) :-
		schulze_ranker::valid_ranker(fake_ranker([alpha], [alpha-0], [model(fake)])).

	test(schulze_rank_invalid_ranker_error, error(domain_error(schulze_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		schulze_ranker::rank(fake_ranker([alpha], [alpha-0], [model(fake)]), [alpha], _Ranking).

	test(schulze_scores_invalid_ranker_error, error(domain_error(schulze_ranker, schulze_ranker([alpha, beta], [alpha-1.5, beta-0], [model(schulze_ranker), options([victory_strength(winning_votes)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		schulze_ranker::scores(schulze_ranker([alpha, beta], [alpha-1.5, beta-0], [model(schulze_ranker), options([victory_strength(winning_votes)]), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(schulze_strongest_paths_invalid_ranker_error, error(domain_error(schulze_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		schulze_ranker::strongest_paths(fake_ranker([alpha], [alpha-0], [model(fake)]), _StrongestPaths).

	test(schulze_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		schulze_ranker::learn(regular_head_to_head, Ranker),
		schulze_ranker::print_ranker(Ranker).

:- end_object.
