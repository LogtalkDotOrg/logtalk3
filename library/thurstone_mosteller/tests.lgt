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
		date is 2026-04-27,
		comment is 'Unit tests for the "thurstone_mosteller" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(thurstone_mosteller).

	cleanup :-
		^^clean_file('test_output.pl').

	test(thurstone_mosteller_learn_2, deterministic(ground(Ranker))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker).

	test(thurstone_mosteller_learn_3_empty_options, deterministic(ground(Ranker))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker, []).

	test(thurstone_mosteller_learn_3_non_empty_options_error, error(domain_error(option, prior(flat)))) :-
		thurstone_mosteller::learn(regular_head_to_head, _Ranker, [prior(flat)]).

	test(thurstone_mosteller_learn_3_non_list_options_error, error(type_error(list, options))) :-
		thurstone_mosteller::learn(regular_head_to_head, _Ranker, options).

	test(thurstone_mosteller_singleton_scores_2, deterministic(Scores == [alpha-0.0])) :-
		thurstone_mosteller::learn(singleton_pairwise, Ranker),
		thurstone_mosteller::scores(Ranker, Scores).

	test(thurstone_mosteller_two_item_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), abs(Alpha - 0.26220025635) =< 1.0e-6, abs(Beta + 0.26220025635) =< 1.0e-6))) :-
		thurstone_mosteller::learn(two_item_head_to_head, Ranker),
		thurstone_mosteller::scores(Ranker, Scores).

	test(thurstone_mosteller_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(thurstone_mosteller_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		thurstone_mosteller::learn(head_to_head, Ranker),
		thurstone_mosteller::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(thurstone_mosteller_cyclic_pairwise_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), abs(Alpha) =< 1.0e-6, abs(Beta) =< 1.0e-6, abs(Gamma) =< 1.0e-6))) :-
		thurstone_mosteller::learn(cyclic_pairwise, Ranker),
		thurstone_mosteller::scores(Ranker, Scores).

	test(thurstone_mosteller_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		thurstone_mosteller::learn(cyclic_pairwise, Ranker),
		thurstone_mosteller::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(thurstone_mosteller_diagnostics_2, deterministic((memberchk(model(thurstone_mosteller), Diagnostics), memberchk(options([]), Diagnostics), memberchk(fit(weighted_least_squares), Diagnostics), memberchk(continuity_correction(0.5), Diagnostics)))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::diagnostics(Ranker, Diagnostics).

	test(thurstone_mosteller_diagnostic_2, true) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::diagnostic(Ranker, model(thurstone_mosteller)).

	test(thurstone_mosteller_ranker_options_2, deterministic(Options == [])) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::ranker_options(Ranker, Options).

	test(thurstone_mosteller_export_to_clauses_4, deterministic(ground(Clause))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(thurstone_mosteller_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		thurstone_mosteller::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(thurstone_mosteller_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		thurstone_mosteller::learn(malformed_pairwise, _Ranker).

	test(thurstone_mosteller_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		thurstone_mosteller::learn(malformed_duplicate_items, _Ranker).

	test(thurstone_mosteller_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		thurstone_mosteller::learn(malformed_self_preference, _Ranker).

	test(thurstone_mosteller_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		thurstone_mosteller::learn(malformed_non_positive_weight, _Ranker).

	test(thurstone_mosteller_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		thurstone_mosteller::learn(disconnected_pairwise, _Ranker).

	test(thurstone_mosteller_rank_variable_candidate_error, error(instantiation_error)) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::rank(Ranker, [_Candidate, beta], _Ranking).

	test(thurstone_mosteller_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::rank(Ranker, [alpha, phantom], _Ranking).

	test(thurstone_mosteller_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::rank(Ranker, alpha, _Ranking).

	test(thurstone_mosteller_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(thurstone_mosteller_valid_ranker_1, deterministic) :-
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::valid_ranker(Ranker).

	test(thurstone_mosteller_invalid_valid_ranker_1, fail) :-
		thurstone_mosteller::valid_ranker(fake_ranker([alpha], [alpha-0.0], [model(fake)])).

	test(thurstone_mosteller_rank_invalid_ranker_error, error(domain_error(thurstone_mosteller_ranker, fake_ranker([alpha], [alpha-0.0], [model(fake)])))) :-
		thurstone_mosteller::rank(fake_ranker([alpha], [alpha-0.0], [model(fake)]), [alpha], _Ranking).

	test(thurstone_mosteller_scores_invalid_ranker_error, error(domain_error(thurstone_mosteller_ranker, thurstone_mosteller_ranker([alpha, beta], [alpha-high, beta-0.0], [model(thurstone_mosteller), options([]), fit(weighted_least_squares), continuity_correction(0.5), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		thurstone_mosteller::scores(thurstone_mosteller_ranker([alpha, beta], [alpha-high, beta-0.0], [model(thurstone_mosteller), options([]), fit(weighted_least_squares), continuity_correction(0.5), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(thurstone_mosteller_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		thurstone_mosteller::learn(regular_head_to_head, Ranker),
		thurstone_mosteller::print_ranker(Ranker).

:- end_object.
