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


:- object(singleton_measurements,
	implements(pairwise_measurement_dataset_protocol)).

	item(alpha).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-27,
		comment is 'Unit tests for the "hodge_rank" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(hodge_rank).

	cleanup :-
		^^clean_file('test_output.pl').

	test(hodge_rank_learn_2, deterministic(ground(Ranker))) :-
		hodge_rank::learn(regular_measurements, Ranker).

	test(hodge_rank_learn_3_empty_options, deterministic(ground(Ranker))) :-
		hodge_rank::learn(regular_measurements, Ranker, []).

	test(hodge_rank_learn_3_non_empty_options_error, error(domain_error(option, prior(flat)))) :-
		hodge_rank::learn(regular_measurements, _Ranker, [prior(flat)]).

	test(hodge_rank_singleton_scores_2, deterministic(Scores == [alpha-0.0])) :-
		hodge_rank::learn(singleton_measurements, Ranker),
		hodge_rank::scores(Ranker, Scores).

	test(hodge_rank_two_item_scores_2, deterministic((abs(Alpha - 1.0) =< 1.0e-6, abs(Beta + 1.0) =< 1.0e-6))) :-
		hodge_rank::learn(two_item_measurements, Ranker),
		hodge_rank::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores).

	test(hodge_rank_regular_measurements_scores_2, deterministic((abs(Alpha - 1.5) =< 1.0e-6, abs(Beta - 0.5) =< 1.0e-6, abs(Gamma + 0.5) =< 1.0e-6, abs(Delta + 1.5) =< 1.0e-6))) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores).

	test(hodge_rank_scores_zero_sum_2, deterministic(abs(Total) =< 1.0e-9)) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores),
		memberchk(delta-Delta, Scores),
		Total is Alpha + Beta + Gamma + Delta.

	test(hodge_rank_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(hodge_rank_cyclic_scores_2, deterministic((abs(Alpha) =< 1.0e-6, abs(Beta) =< 1.0e-6, abs(Gamma) =< 1.0e-6))) :-
		hodge_rank::learn(cyclic_measurements, Ranker),
		hodge_rank::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(hodge_rank_cyclic_residuals_2, deterministic((Residuals == [residual(alpha, beta, 1.0, 1.0), residual(beta, gamma, 1.0, 1.0), residual(gamma, alpha, 1.0, 1.0)], abs(ResidualNorm - 1.7320508075688772) =< 1.0e-6))) :-
		hodge_rank::learn(cyclic_measurements, Ranker),
		hodge_rank::residuals(Ranker, Residuals),
		hodge_rank::diagnostics(Ranker, Diagnostics),
		memberchk(residual_norm(ResidualNorm), Diagnostics).

	test(hodge_rank_regular_measurements_residuals_2, deterministic(Residuals == [residual(alpha, beta, 0.0, 1.0), residual(alpha, gamma, 0.0, 1.0), residual(alpha, delta, 0.0, 1.0), residual(beta, gamma, 0.0, 1.0), residual(beta, delta, 0.0, 1.0), residual(gamma, delta, 0.0, 1.0)])) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::residuals(Ranker, Residuals).

	test(hodge_rank_diagnostics_2, deterministic) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::diagnostics(Ranker, Diagnostics),
		memberchk(model(hodge_rank), Diagnostics),
		memberchk(options([]), Diagnostics),
		memberchk(residuals(_Residuals), Diagnostics),
		memberchk(residual_norm(0.0), Diagnostics),
		memberchk(dataset_summary(_Summary), Diagnostics).

	test(hodge_rank_diagnostic_2, true) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::diagnostic(Ranker, model(hodge_rank)).

	test(hodge_rank_ranker_options_2, deterministic(Options == [])) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::ranker_options(Ranker, Options).

	test(hodge_rank_export_to_clauses_4, deterministic(ground(Clause))) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::export_to_clauses(regular_measurements, Ranker, ranker, [Clause]).

	test(hodge_rank_export_to_file_4_loaded, deterministic((Ranking == [alpha, beta, gamma, delta], Residuals == [residual(alpha, beta, 0.0, 1.0), residual(alpha, gamma, 0.0, 1.0), residual(alpha, delta, 0.0, 1.0), residual(beta, gamma, 0.0, 1.0), residual(beta, delta, 0.0, 1.0), residual(gamma, delta, 0.0, 1.0)]))) :-
		^^file_path('test_output.pl', File),
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::export_to_file(regular_measurements, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		hodge_rank::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking),
		hodge_rank::residuals(LoadedRanker, Residuals).

	test(hodge_rank_malformed_measurement_unknown_item_error, error(existence_error(item, phantom))) :-
		hodge_rank::learn(malformed_measurement_unknown_item, _Ranker).

	test(hodge_rank_malformed_measurement_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		hodge_rank::learn(malformed_measurement_duplicate_items, _Ranker).

	test(hodge_rank_malformed_measurement_self_error, error(domain_error(distinct_items, alpha-alpha))) :-
		hodge_rank::learn(malformed_measurement_self, _Ranker).

	test(hodge_rank_malformed_measurement_non_numeric_error, error(type_error(number, foo))) :-
		hodge_rank::learn(malformed_measurement_non_numeric, _Ranker).

	test(hodge_rank_malformed_measurement_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		hodge_rank::learn(malformed_measurement_non_positive_weight, _Ranker).

	test(hodge_rank_disconnected_measurements_error, error(domain_error(connected_pairwise_measurement_dataset, [[alpha, beta], [gamma, delta]]))) :-
		hodge_rank::learn(disconnected_measurements, _Ranker).

	test(hodge_rank_rank_variable_candidate_error, error(instantiation_error)) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::rank(Ranker, [_Candidate, beta], _Ranking).

	test(hodge_rank_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::rank(Ranker, [alpha, phantom], _Ranking).

	test(hodge_rank_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::rank(Ranker, alpha, _Ranking).

	test(hodge_rank_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(hodge_rank_valid_ranker_1, deterministic) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::valid_ranker(Ranker).

	test(hodge_rank_invalid_valid_ranker_1, fail) :-
		hodge_rank::valid_ranker(fake_ranker([alpha], [alpha-0.0], [model(fake)])).

	test(hodge_rank_rank_invalid_ranker_error, error(domain_error(hodge_rank_ranker, fake_ranker([alpha], [alpha-0.0], [model(fake)])))) :-
		hodge_rank::rank(fake_ranker([alpha], [alpha-0.0], [model(fake)]), [alpha], _Ranking).

	test(hodge_rank_residuals_invalid_ranker_error, error(domain_error(hodge_rank_ranker, hodge_rank_ranker([alpha], [alpha-0.0], [model(hodge_rank), options([]), residuals(foo), residual_norm(0.0), dataset_summary([items(1), measurements(0), connected_components(1), isolated_items([alpha])])])))) :-
		hodge_rank::residuals(hodge_rank_ranker([alpha], [alpha-0.0], [model(hodge_rank), options([]), residuals(foo), residual_norm(0.0), dataset_summary([items(1), measurements(0), connected_components(1), isolated_items([alpha])])]), _Residuals).

	test(hodge_rank_scores_invalid_ranker_error, error(domain_error(hodge_rank_ranker, hodge_rank_ranker([alpha, beta], [alpha-foo, beta-0.0], [model(hodge_rank), options([]), residuals([]), residual_norm(0.0), dataset_summary([items(2), measurements(1), connected_components(1), isolated_items([])])])))) :-
		hodge_rank::scores(hodge_rank_ranker([alpha, beta], [alpha-foo, beta-0.0], [model(hodge_rank), options([]), residuals([]), residual_norm(0.0), dataset_summary([items(2), measurements(1), connected_components(1), isolated_items([])])]), _Scores).

	test(hodge_rank_print_ranker_1, deterministic) :-
		hodge_rank::learn(regular_measurements, Ranker),
		hodge_rank::print_ranker(Ranker).

:- end_object.
