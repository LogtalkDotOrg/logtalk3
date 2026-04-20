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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-04-20,
		comment is 'Unit tests for the "bradley_terry" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cover(bradley_terry).

	cleanup :-
		^^clean_file('test_output.pl').

	test(bradley_terry_learn_2, deterministic(ground(Ranker))) :-
		bradley_terry::learn(head_to_head, Ranker).

	test(bradley_terry_learn_3_custom_options, deterministic(memberchk(options([maximum_iterations(50), tolerance(1.0e-7)]), Diagnostics))) :-
		bradley_terry::learn(head_to_head, bt_ranker(_Items, _Strengths, Diagnostics), [maximum_iterations(50), tolerance(1.0e-7)]).

	test(bradley_terry_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(bradley_terry_rank_subset_3, deterministic(Ranking == [alpha, gamma, delta])) :-
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::rank(Ranker, [gamma, delta, alpha], Ranking).

	test(bradley_terry_strengths_2, deterministic((memberchk(alpha-_, Strengths), memberchk(delta-_, Strengths)))) :-
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::strengths(Ranker, Strengths).

	test(bradley_terry_diagnostics_2, deterministic((memberchk(model(bradley_terry), Diagnostics), memberchk(convergence(converged), Diagnostics)))) :-
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::diagnostics(Ranker, Diagnostics).

	test(bradley_terry_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::diagnostics(Ranker, Diagnostics),
		findall(Diagnostic, bradley_terry::diagnostic(Ranker, Diagnostic), Enumerated).

	test(bradley_terry_sparse_preferences_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma]]))) :-
		bradley_terry::learn(sparse_preferences, _Ranker).

	test(bradley_terry_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		bradley_terry::learn(malformed_pairwise, _Ranker).

	test(bradley_terry_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		bradley_terry::learn(malformed_duplicate_items, _Ranker).

	test(bradley_terry_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		bradley_terry::learn(malformed_self_preference, _Ranker).

	test(bradley_terry_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		bradley_terry::learn(malformed_non_positive_weight, _Ranker).

	test(bradley_terry_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		bradley_terry::learn(disconnected_pairwise, _Ranker).

	test(bradley_terry_export_to_clauses_4, deterministic(ground(Clause))) :-
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::export_to_clauses(head_to_head, Ranker, ranker, [Clause]).

	test(bradley_terry_export_to_file_4_loaded, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		^^file_path('test_output.pl', File),
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::export_to_file(head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		bradley_terry::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking).

	test(bradley_terry_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		bradley_terry::learn(head_to_head, Ranker),
		bradley_terry::print_ranker(Ranker).

:- end_object.
