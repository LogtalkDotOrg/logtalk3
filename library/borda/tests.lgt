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
		comment is 'Unit tests for the "borda" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(borda).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_fractional.pl').

	test(borda_learn_2, deterministic(ground(Ranker))) :-
		borda::learn(ranked_ballots, Ranker).

	test(borda_learn_3_empty_options, deterministic(ground(Ranker))) :-
		borda::learn(ranked_ballots, Ranker, []).

	test(borda_learn_3_custom_options, deterministic((memberchk(options(Options), Diagnostics), memberchk(missing_relevance(error), Options), memberchk(tie_scoring(fractional), Options)))) :-
		borda::learn(tied_grouped, borda_ranker(_Items, _Scores, Diagnostics), [missing_relevance(error), tie_scoring(fractional)]).

	test(borda_learn_3_unsupported_tie_scoring_error, error(domain_error(option, tie_scoring(ceiling)))) :-
		borda::learn(ranked_ballots, _Ranker, [tie_scoring(ceiling)]).

	test(borda_learn_3_variable_missing_relevance_error, error(domain_error(option, missing_relevance(_)))) :-
		borda::learn(ranked_ballots, _Ranker, [missing_relevance(_)]).

	test(borda_learn_3_variable_tie_scoring_error, error(domain_error(option, tie_scoring(_)))) :-
		borda::learn(ranked_ballots, _Ranker, [tie_scoring(_)]).

	test(borda_learn_3_non_list_options_error, error(type_error(list, options))) :-
		borda::learn(ranked_ballots, _Ranker, options).

	test(borda_rank_3, deterministic(Ranking == [beta, alpha, gamma])) :-
		borda::learn(ranked_ballots, Ranker),
		borda::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(borda_rank_search_results_3, deterministic(Ranking == [doc_alpha, doc_delta, doc_beta, doc_epsilon, doc_gamma, doc_zeta])) :-
		borda::learn(search_results, Ranker),
		borda::rank(Ranker, [doc_zeta, doc_alpha, doc_beta, doc_delta, doc_epsilon, doc_gamma], Ranking).

	test(borda_scores_2, deterministic((memberchk(alpha-2, Scores), memberchk(beta-3, Scores), memberchk(gamma-1, Scores)))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::scores(Ranker, Scores).

	test(borda_tied_grouped_scores_2, deterministic((memberchk(alpha-1, Scores), memberchk(beta-1, Scores), memberchk(gamma-0, Scores)))) :-
		borda::learn(tied_grouped, Ranker),
		borda::scores(Ranker, Scores).

	test(borda_fractional_tied_grouped_scores_2, deterministic((memberchk(alpha-1.5, Scores), memberchk(beta-1.5, Scores), memberchk(gamma-0.0, Scores)))) :-
		borda::learn(tied_grouped, Ranker, [tie_scoring(fractional)]),
		borda::scores(Ranker, Scores).

	test(borda_fractional_tied_grouped_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		borda::learn(tied_grouped, Ranker, [tie_scoring(fractional)]),
		borda::rank(Ranker, [gamma, beta, alpha], Ranking).

	test(borda_sparse_grouped_default_zero_2, deterministic((memberchk(alpha-2, Scores), memberchk(beta-1, Scores), memberchk(gamma-0, Scores)))) :-
		borda::learn(sparse_grouped_relevance, Ranker),
		borda::scores(Ranker, Scores).

	test(borda_sparse_grouped_missing_relevance_error, error(existence_error(relevance, ballot_one-gamma))) :-
		borda::learn(sparse_grouped_relevance, _Ranker, [missing_relevance(error)]).

	test(borda_reordered_grouped_items_scores_order_2, deterministic(Scores == [beta-2, alpha-2, gamma-0])) :-
		borda::learn(reordered_grouped_items, Ranker),
		borda::scores(Ranker, Scores).

	test(borda_diagnostics_2, deterministic((memberchk(model(borda), Diagnostics), memberchk(options(Options), Diagnostics), memberchk(missing_relevance(zero), Options), memberchk(tie_scoring(standard), Options)))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::diagnostics(Ranker, Diagnostics).

	test(borda_diagnostic_2, true) :-
		borda::learn(ranked_ballots, Ranker),
		borda::diagnostic(Ranker, model(borda)).

	test(borda_ranker_options_2, deterministic((memberchk(missing_relevance(zero), Options), memberchk(tie_scoring(standard), Options)))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::ranker_options(Ranker, Options).

	test(borda_ranker_options_fractional_2, deterministic((memberchk(missing_relevance(error), Options), memberchk(tie_scoring(fractional), Options)))) :-
		borda::learn(tied_grouped, Ranker, [missing_relevance(error), tie_scoring(fractional)]),
		borda::ranker_options(Ranker, Options).

	test(borda_export_to_clauses_4, deterministic(ground(Clause))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::export_to_clauses(ranked_ballots, Ranker, ranker, [Clause]).

	test(borda_export_to_file_4_loaded, deterministic(Ranking == [beta, alpha, gamma])) :-
		^^file_path('test_output.pl', File),
		borda::learn(ranked_ballots, Ranker),
		borda::export_to_file(ranked_ballots, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		borda::rank(LoadedRanker, [gamma, beta, alpha], Ranking).

	test(borda_export_to_file_4_non_default_options_round_trip, deterministic((LoadedScores == Scores, LoadedDiagnostics == Diagnostics, Ranking == [alpha, beta, gamma]))) :-
		^^file_path('test_output_fractional.pl', File),
		borda::learn(tied_grouped, Ranker, [missing_relevance(error), tie_scoring(fractional)]),
		borda::scores(Ranker, Scores),
		borda::diagnostics(Ranker, Diagnostics),
		borda::export_to_file(tied_grouped, Ranker, ranker_fractional, File),
		logtalk_load(File),
		{ranker_fractional(LoadedRanker)},
		borda::scores(LoadedRanker, LoadedScores),
		borda::diagnostics(LoadedRanker, LoadedDiagnostics),
		borda::rank(LoadedRanker, [gamma, beta, alpha], Ranking).

	test(borda_malformed_grouped_error, error(domain_error(non_negative_integer, high))) :-
		borda::learn(malformed_grouped, _Ranker).

	test(borda_rank_variable_candidate_error, error(instantiation_error)) :-
		borda::learn(ranked_ballots, Ranker),
		borda::rank(Ranker, [_Candidate, beta], _Ranking).

	test(borda_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::rank(Ranker, [alpha, phantom], _Ranking).

	test(borda_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::rank(Ranker, alpha, _Ranking).

	test(borda_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		borda::learn(ranked_ballots, Ranker),
		borda::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(borda_valid_ranker_1, deterministic) :-
		borda::learn(ranked_ballots, Ranker),
		borda::valid_ranker(Ranker).

	test(borda_invalid_valid_ranker_1, fail) :-
		borda::valid_ranker(fake_ranker([alpha], [alpha-1], [model(fake)])).

	test(borda_rank_invalid_ranker_error, error(domain_error(borda_ranker, fake_ranker([alpha], [alpha-1], [model(fake)])))) :-
		borda::rank(fake_ranker([alpha], [alpha-1], [model(fake)]), [alpha], _Ranking).

	test(borda_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		borda::learn(ranked_ballots, Ranker),
		borda::print_ranker(Ranker).

:- end_object.
