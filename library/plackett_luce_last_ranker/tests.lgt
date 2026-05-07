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


:- object(regular_tied_grouped,
	implements(ranking_dataset_protocol)).

	group(ballot_one).
	group(ballot_two).
	group(ballot_three).

	item(ballot_one, alpha).
	item(ballot_one, beta).
	item(ballot_one, gamma).

	item(ballot_two, alpha).
	item(ballot_two, beta).
	item(ballot_two, gamma).

	item(ballot_three, alpha).
	item(ballot_three, beta).
	item(ballot_three, gamma).

	relevance(ballot_one, alpha, 1).
	relevance(ballot_one, beta, 1).
	relevance(ballot_one, gamma, 0).

	relevance(ballot_two, alpha, 1).
	relevance(ballot_two, gamma, 1).
	relevance(ballot_two, beta, 0).

	relevance(ballot_three, beta, 1).
	relevance(ballot_three, gamma, 1).
	relevance(ballot_three, alpha, 0).

:- end_object.


:- object(asymmetric_tied_grouped,
	implements(ranking_dataset_protocol)).

	group(ballot_one).
	group(ballot_two).
	group(ballot_three).
	group(ballot_four).

	item(ballot_one, alpha).
	item(ballot_one, beta).
	item(ballot_one, gamma).

	item(ballot_two, alpha).
	item(ballot_two, beta).
	item(ballot_two, gamma).

	item(ballot_three, alpha).
	item(ballot_three, beta).
	item(ballot_three, gamma).

	item(ballot_four, alpha).
	item(ballot_four, beta).
	item(ballot_four, gamma).

	relevance(ballot_one, alpha, 1).
	relevance(ballot_one, beta, 1).
	relevance(ballot_one, gamma, 0).

	relevance(ballot_two, alpha, 1).
	relevance(ballot_two, gamma, 1).
	relevance(ballot_two, beta, 0).

	relevance(ballot_three, beta, 1).
	relevance(ballot_three, gamma, 1).
	relevance(ballot_three, alpha, 0).

	relevance(ballot_four, alpha, 1).
	relevance(ballot_four, beta, 0).
	relevance(ballot_four, gamma, 0).

:- end_object.


:- object(asymmetric_tied_grouped_three,
	implements(ranking_dataset_protocol)).

	group(ballot_one).
	group(ballot_two).
	group(ballot_three).
	group(ballot_four).
	group(ballot_five).

	item(ballot_one, alpha).
	item(ballot_one, beta).
	item(ballot_one, gamma).

	item(ballot_two, alpha).
	item(ballot_two, beta).
	item(ballot_two, gamma).

	item(ballot_three, alpha).
	item(ballot_three, beta).
	item(ballot_three, gamma).

	item(ballot_four, alpha).
	item(ballot_four, beta).
	item(ballot_four, gamma).

	item(ballot_five, alpha).
	item(ballot_five, beta).
	item(ballot_five, gamma).

	relevance(ballot_one, alpha, 1).
	relevance(ballot_one, beta, 1).
	relevance(ballot_one, gamma, 0).

	relevance(ballot_two, alpha, 1).
	relevance(ballot_two, gamma, 1).
	relevance(ballot_two, beta, 0).

	relevance(ballot_three, beta, 1).
	relevance(ballot_three, gamma, 1).
	relevance(ballot_three, alpha, 0).

	relevance(ballot_four, alpha, 1).
	relevance(ballot_four, beta, 0).
	relevance(ballot_four, gamma, 0).

	relevance(ballot_five, alpha, 1).
	relevance(ballot_five, beta, 1).
	relevance(ballot_five, gamma, 0).

:- end_object.


:- object(singleton_grouped,
	implements(ranking_dataset_protocol)).

	group(ballot_one).

	item(ballot_one, alpha).

	relevance(ballot_one, alpha, 1).

:- end_object.


:- object(singleton_grouped_missing_relevance,
	implements(ranking_dataset_protocol)).

	group(ballot_one).

	item(ballot_one, alpha).

:- end_object.


:- object(plackett_luce_last_empty_grouped,
	implements(ranking_dataset_protocol)).

	group(ballot_one).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "plackett_luce_last_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(plackett_luce_last_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(plackett_luce_last_learn_2, deterministic(ground(Ranker))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker).

	test(plackett_luce_last_singleton_scores_2, deterministic(Scores == [alpha-1.0])) :-
		plackett_luce_last_ranker::learn(singleton_grouped, Ranker),
		plackett_luce_last_ranker::scores(Ranker, Scores).

	test(plackett_luce_last_singleton_diagnostics_2, deterministic([Convergence, Iterations, FinalDelta] == [converged, 0, 0.0])) :-
		plackett_luce_last_ranker::learn(singleton_grouped, Ranker),
		plackett_luce_last_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics).

	test(plackett_luce_last_singleton_missing_relevance_error, error(existence_error(relevance, ballot_one-alpha))) :-
		plackett_luce_last_ranker::learn(singleton_grouped_missing_relevance, _Ranker, [missing_relevance(error)]).

	test(plackett_luce_last_learn_3_custom_options, deterministic([MaximumIterations, Tolerance, MissingRelevance] == [50, 1.0e-7, error])) :-
		plackett_luce_last_ranker::learn(ranked_ballots, plackett_luce_last_ranker(_Items, _Strengths, Diagnostics), [maximum_iterations(50), tolerance(1.0e-7), missing_relevance(error)]),
		memberchk(options(Options), Diagnostics),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(missing_relevance(MissingRelevance), Options).

	test(plackett_luce_last_rank_3, deterministic(true)) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::rank(Ranker, [gamma, alpha, beta], [beta, alpha, gamma]).

	test(plackett_luce_last_rank_subset_3, deterministic(true)) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::rank(Ranker, [gamma, beta], [beta, gamma]).

	test(plackett_luce_last_scores_2, deterministic((Beta > Alpha, Alpha > Gamma, Sum is Alpha + Beta + Gamma, abs(Sum - 1.0) =< 1.0e-6))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(plackett_luce_last_regular_tied_grouped_scores_2, deterministic((abs(Alpha - Beta) =< 1.0e-6, abs(Beta - Gamma) =< 1.0e-6, abs(Gamma - (1.0/3.0)) =< 1.0e-6))) :-
		plackett_luce_last_ranker::learn(regular_tied_grouped, Ranker),
		plackett_luce_last_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(plackett_luce_last_regular_tied_grouped_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		plackett_luce_last_ranker::learn(regular_tied_grouped, Ranker),
		plackett_luce_last_ranker::rank(Ranker, [gamma, beta, alpha], Ranking).

	test(plackett_luce_last_asymmetric_tied_grouped_scores_2, deterministic((Alpha > Beta, abs(Alpha - 0.5351837211255078) =< 1.0e-6, abs(Beta - 0.23240813943724614) =< 1.0e-6, abs(Gamma - 0.23240813943724614) =< 1.0e-6))) :-
		plackett_luce_last_ranker::learn(asymmetric_tied_grouped, Ranker),
		plackett_luce_last_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(plackett_luce_last_asymmetric_tied_grouped_three_scores_2, deterministic((Alpha > Beta, Beta > Gamma, abs(Alpha - 0.5792050820671106) =< 1.0e-6, abs(Beta - 0.2651655355571285) =< 1.0e-6, abs(Gamma - 0.1556293823757608) =< 1.0e-6))) :-
		plackett_luce_last_ranker::learn(asymmetric_tied_grouped_three, Ranker),
		plackett_luce_last_ranker::scores(Ranker, Scores),
		memberchk(alpha-Alpha, Scores),
		memberchk(beta-Beta, Scores),
		memberchk(gamma-Gamma, Scores).

	test(plackett_luce_last_diagnostics_2, deterministic([Model, Convergence, Groups] == [plackett_luce_last_ranker, converged, 2])) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(dataset_summary(Summary), Diagnostics),
		memberchk(groups(Groups), Summary).

	test(plackett_luce_last_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::diagnostics(Ranker, Diagnostics),
		findall(Diagnostic, plackett_luce_last_ranker::diagnostic(Ranker, Diagnostic), Enumerated).

	test(plackett_luce_last_ranker_options_2, deterministic([MaximumIterations, Tolerance, MissingRelevance] == [5000, 1.0e-6, zero])) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::ranker_options(Ranker, Options),
		memberchk(maximum_iterations(MaximumIterations), Options),
		memberchk(tolerance(Tolerance), Options),
		memberchk(missing_relevance(MissingRelevance), Options).

	test(plackett_luce_last_maximum_iterations_exhausted_diagnostics_2, deterministic((FinalDelta > 0.0, Convergence == maximum_iterations_exhausted, Iterations == 1))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker, [maximum_iterations(1), tolerance(1.0e-12)]),
		plackett_luce_last_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(convergence(Convergence), Diagnostics),
		memberchk(iterations(Iterations), Diagnostics),
		memberchk(final_delta(FinalDelta), Diagnostics).

	test(plackett_luce_last_export_to_clauses_4, deterministic(ground(Clause))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::export_to_clauses(ranked_ballots, Ranker, ranker, [Clause]).

	test(plackett_luce_last_export_to_file_4_loaded, deterministic(Ranking == [beta, alpha, gamma])) :-
		^^file_path('test_output.pl', File),
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::export_to_file(ranked_ballots, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		plackett_luce_last_ranker::rank(LoadedRanker, [alpha, beta, gamma], Ranking).

	test(plackett_luce_last_tied_grouped_non_regular_error, error(domain_error(plackett_luce_last_regular_dataset, _))) :-
		plackett_luce_last_ranker::learn(tied_grouped, _Ranker).

	test(plackett_luce_last_sparse_grouped_missing_relevance_error, error(existence_error(relevance, ballot_one-gamma))) :-
		plackett_luce_last_ranker::learn(sparse_grouped_relevance, _Ranker, [missing_relevance(error)]).

	test(plackett_luce_last_search_results_non_regular_error, error(domain_error(plackett_luce_last_regular_dataset, _))) :-
		plackett_luce_last_ranker::learn(search_results, _Ranker).

	test(plackett_luce_last_malformed_grouped_error, error(domain_error(non_negative_integer, high))) :-
		plackett_luce_last_ranker::learn(malformed_grouped, _Ranker).

	test(plackett_luce_last_empty_group_error, error(domain_error(non_empty_group, ballot_one))) :-
		plackett_luce_last_ranker::learn(plackett_luce_last_empty_grouped, _Ranker).

	test(plackett_luce_last_rank_variable_candidate_error, error(instantiation_error)) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(plackett_luce_last_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(plackett_luce_last_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::rank(Ranker, alpha, _Ranking).

	test(plackett_luce_last_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(plackett_luce_last_valid_ranker_1, deterministic) :-
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::valid_ranker(Ranker).

	test(plackett_luce_last_invalid_valid_ranker_1, fail) :-
		plackett_luce_last_ranker::valid_ranker(fake_ranker([alpha], [alpha-1.0], [model(fake)])).

	test(plackett_luce_last_rank_invalid_ranker_error, error(domain_error(plackett_luce_last_ranker, fake_ranker([alpha], [alpha-1.0], [model(fake)])))) :-
		plackett_luce_last_ranker::rank(fake_ranker([alpha], [alpha-1.0], [model(fake)]), [alpha], _Ranking).

	test(plackett_luce_last_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		plackett_luce_last_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_last_ranker::print_ranker(Ranker).

:- end_object.
