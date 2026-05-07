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
		comment is 'Unit tests for the "borda_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(borda_ranker).

	cleanup :-
		^^clean_file('test_output.pl'),
		^^clean_file('test_output_fractional.pl').

	test(borda_learn_2, deterministic(ground(Ranker))) :-
		borda_ranker::learn(ranked_ballots, Ranker).

	test(borda_learn_3_empty_options, deterministic(ground(Ranker))) :-
		borda_ranker::learn(ranked_ballots, Ranker, []).

		test(borda_learn_3_custom_options, deterministic([MissingRelevance, TieScoring] == [error, fractional])) :-
		borda_ranker::learn(tied_grouped, borda_ranker(_Items, _Scores, Diagnostics), [missing_relevance(error), tie_scoring(fractional)]),
		memberchk(options(Options), Diagnostics),
				memberchk(missing_relevance(MissingRelevance), Options),
				memberchk(tie_scoring(TieScoring), Options).

	test(borda_learn_3_unsupported_tie_scoring_error, error(domain_error(option, tie_scoring(ceiling)))) :-
		borda_ranker::learn(ranked_ballots, _Ranker, [tie_scoring(ceiling)]).

	test(borda_learn_3_variable_missing_relevance_error, error(domain_error(option, missing_relevance(_)))) :-
		borda_ranker::learn(ranked_ballots, _Ranker, [missing_relevance(_)]).

	test(borda_learn_3_variable_tie_scoring_error, error(domain_error(option, tie_scoring(_)))) :-
		borda_ranker::learn(ranked_ballots, _Ranker, [tie_scoring(_)]).

	test(borda_learn_3_non_list_options_error, error(type_error(list, options))) :-
		borda_ranker::learn(ranked_ballots, _Ranker, options).

	test(borda_rank_3, deterministic(Ranking == [beta, alpha, gamma])) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(borda_rank_search_results_3, deterministic(Ranking == [doc_alpha, doc_delta, doc_beta, doc_epsilon, doc_gamma, doc_zeta])) :-
		borda_ranker::learn(search_results, Ranker),
		borda_ranker::rank(Ranker, [doc_zeta, doc_alpha, doc_beta, doc_delta, doc_epsilon, doc_gamma], Ranking).

		test(borda_scores_2, deterministic([AlphaScore, BetaScore, GammaScore] == [2, 3, 1])) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::scores(Ranker, Scores),
				memberchk(alpha-AlphaScore, Scores),
				memberchk(beta-BetaScore, Scores),
				memberchk(gamma-GammaScore, Scores).

		test(borda_tied_grouped_scores_2, deterministic([AlphaScore, BetaScore, GammaScore] == [1, 1, 0])) :-
		borda_ranker::learn(tied_grouped, Ranker),
		borda_ranker::scores(Ranker, Scores),
				memberchk(alpha-AlphaScore, Scores),
				memberchk(beta-BetaScore, Scores),
				memberchk(gamma-GammaScore, Scores).

		test(borda_fractional_tied_grouped_scores_2, deterministic([AlphaScore, BetaScore, GammaScore] == [1.5, 1.5, 0.0])) :-
		borda_ranker::learn(tied_grouped, Ranker, [tie_scoring(fractional)]),
		borda_ranker::scores(Ranker, Scores),
				memberchk(alpha-AlphaScore, Scores),
				memberchk(beta-BetaScore, Scores),
				memberchk(gamma-GammaScore, Scores).

	test(borda_fractional_tied_grouped_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		borda_ranker::learn(tied_grouped, Ranker, [tie_scoring(fractional)]),
		borda_ranker::rank(Ranker, [gamma, beta, alpha], Ranking).

		test(borda_sparse_grouped_default_zero_2, deterministic([AlphaScore, BetaScore, GammaScore] == [2, 1, 0])) :-
		borda_ranker::learn(sparse_grouped_relevance, Ranker),
		borda_ranker::scores(Ranker, Scores),
				memberchk(alpha-AlphaScore, Scores),
				memberchk(beta-BetaScore, Scores),
				memberchk(gamma-GammaScore, Scores).

	test(borda_sparse_grouped_missing_relevance_error, error(existence_error(relevance, ballot_one-gamma))) :-
		borda_ranker::learn(sparse_grouped_relevance, _Ranker, [missing_relevance(error)]).

	test(borda_reordered_grouped_items_scores_order_2, deterministic(Scores == [beta-2, alpha-2, gamma-0])) :-
		borda_ranker::learn(reordered_grouped_items, Ranker),
		borda_ranker::scores(Ranker, Scores).

		test(borda_diagnostics_2, deterministic([Model, MissingRelevance, TieScoring] == [borda_ranker, zero, standard])) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::diagnostics(Ranker, Diagnostics),
				memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
				memberchk(missing_relevance(MissingRelevance), Options),
				memberchk(tie_scoring(TieScoring), Options).

	test(borda_diagnostic_2, true) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::diagnostic(Ranker, model(borda_ranker)).

		test(borda_ranker_options_2, deterministic([MissingRelevance, TieScoring] == [zero, standard])) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::ranker_options(Ranker, Options),
				memberchk(missing_relevance(MissingRelevance), Options),
				memberchk(tie_scoring(TieScoring), Options).

		test(borda_ranker_options_fractional_2, deterministic([MissingRelevance, TieScoring] == [error, fractional])) :-
		borda_ranker::learn(tied_grouped, Ranker, [missing_relevance(error), tie_scoring(fractional)]),
		borda_ranker::ranker_options(Ranker, Options),
				memberchk(missing_relevance(MissingRelevance), Options),
				memberchk(tie_scoring(TieScoring), Options).

	test(borda_export_to_clauses_4, deterministic(ground(Clause))) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::export_to_clauses(ranked_ballots, Ranker, ranker, [Clause]).

	test(borda_export_to_file_4_loaded, deterministic(Ranking == [beta, alpha, gamma])) :-
		^^file_path('test_output.pl', File),
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::export_to_file(ranked_ballots, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		borda_ranker::rank(LoadedRanker, [gamma, beta, alpha], Ranking).

	test(borda_export_to_file_4_non_default_options_round_trip, deterministic((LoadedScores == Scores, LoadedDiagnostics == Diagnostics, Ranking == [alpha, beta, gamma]))) :-
		^^file_path('test_output_fractional.pl', File),
		borda_ranker::learn(tied_grouped, Ranker, [missing_relevance(error), tie_scoring(fractional)]),
		borda_ranker::scores(Ranker, Scores),
		borda_ranker::diagnostics(Ranker, Diagnostics),
		borda_ranker::export_to_file(tied_grouped, Ranker, ranker_fractional, File),
		logtalk_load(File),
		{ranker_fractional(LoadedRanker)},
		borda_ranker::scores(LoadedRanker, LoadedScores),
		borda_ranker::diagnostics(LoadedRanker, LoadedDiagnostics),
		borda_ranker::rank(LoadedRanker, [gamma, beta, alpha], Ranking).

	test(borda_malformed_grouped_error, error(domain_error(non_negative_integer, high))) :-
		borda_ranker::learn(malformed_grouped, _Ranker).

	test(borda_rank_variable_candidate_error, error(instantiation_error)) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(borda_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(borda_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::rank(Ranker, alpha, _Ranking).

	test(borda_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(borda_valid_ranker_1, deterministic) :-
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::valid_ranker(Ranker).

	test(borda_invalid_valid_ranker_1, fail) :-
		borda_ranker::valid_ranker(fake_ranker([alpha], [alpha-1], [model(fake)])).

	test(borda_rank_invalid_ranker_error, error(domain_error(borda_ranker, fake_ranker([alpha], [alpha-1], [model(fake)])))) :-
		borda_ranker::rank(fake_ranker([alpha], [alpha-1], [model(fake)]), [alpha], _Ranking).

	test(borda_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		borda_ranker::learn(ranked_ballots, Ranker),
		borda_ranker::print_ranker(Ranker).

:- end_object.
