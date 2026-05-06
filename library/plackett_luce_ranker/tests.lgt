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


:- object(plackett_luce_empty_grouped,
	implements(ranking_dataset_protocol)).

	group(ballot_one).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-06,
		comment is 'Unit tests for the "plackett_luce_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(plackett_luce_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(plackett_luce_learn_2, deterministic(ground(Ranker))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker).

	test(plackett_luce_singleton_scores_2, deterministic(Scores == [alpha-1.0])) :-
		plackett_luce_ranker::learn(singleton_grouped, Ranker),
		plackett_luce_ranker::scores(Ranker, Scores).

	test(plackett_luce_singleton_diagnostics_2, deterministic((memberchk(convergence(converged), Diagnostics), memberchk(iterations(0), Diagnostics), memberchk(final_delta(0.0), Diagnostics)))) :-
		plackett_luce_ranker::learn(singleton_grouped, Ranker),
		plackett_luce_ranker::diagnostics(Ranker, Diagnostics).

	test(plackett_luce_singleton_missing_relevance_error, error(existence_error(relevance, ballot_one-alpha))) :-
		plackett_luce_ranker::learn(singleton_grouped_missing_relevance, _Ranker, [missing_relevance(error)]).

	test(plackett_luce_learn_3_custom_options, deterministic((memberchk(options(Options), Diagnostics), memberchk(maximum_iterations(50), Options), memberchk(tolerance(1.0e-7), Options), memberchk(missing_relevance(error), Options)))) :-
		plackett_luce_ranker::learn(ranked_ballots, plackett_luce_ranker(_Items, _Strengths, Diagnostics), [maximum_iterations(50), tolerance(1.0e-7), missing_relevance(error)]).

	test(plackett_luce_rank_3, deterministic(Best == beta)) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::rank(Ranker, [gamma, alpha, beta], [Best| _Ranking]).

	test(plackett_luce_rank_subset_3, deterministic(Ranking == [beta, gamma])) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::rank(Ranker, [gamma, beta], Ranking).

	test(plackett_luce_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), Beta > Alpha, Beta > Gamma, Sum is Alpha + Beta + Gamma, abs(Sum - 1.0) =< 1.0e-6))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::scores(Ranker, Scores).

	test(plackett_luce_regular_tied_grouped_scores_2, deterministic((memberchk(alpha-Alpha, Scores), memberchk(beta-Beta, Scores), memberchk(gamma-Gamma, Scores), abs(Alpha - Beta) =< 1.0e-6, abs(Beta - Gamma) =< 1.0e-6, abs(Gamma - (1.0/3.0)) =< 1.0e-6))) :-
		plackett_luce_ranker::learn(regular_tied_grouped, Ranker),
		plackett_luce_ranker::scores(Ranker, Scores).

	test(plackett_luce_diagnostics_2, deterministic((memberchk(model(plackett_luce_ranker), Diagnostics), memberchk(convergence(converged), Diagnostics), memberchk(dataset_summary(Summary), Diagnostics), memberchk(groups(2), Summary)))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::diagnostics(Ranker, Diagnostics).

	test(plackett_luce_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::diagnostics(Ranker, Diagnostics),
		findall(Diagnostic, plackett_luce_ranker::diagnostic(Ranker, Diagnostic), Enumerated).

	test(plackett_luce_ranker_options_2, deterministic((memberchk(maximum_iterations(5000), Options), memberchk(tolerance(1.0e-6), Options), memberchk(missing_relevance(zero), Options)))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::ranker_options(Ranker, Options).

	test(plackett_luce_maximum_iterations_exhausted_diagnostics_2, deterministic((memberchk(convergence(maximum_iterations_exhausted), Diagnostics), memberchk(iterations(1), Diagnostics), memberchk(final_delta(FinalDelta), Diagnostics), FinalDelta > 0.0))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker, [maximum_iterations(1), tolerance(1.0e-12)]),
		plackett_luce_ranker::diagnostics(Ranker, Diagnostics).

	test(plackett_luce_export_to_clauses_4, deterministic(ground(Clause))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::export_to_clauses(ranked_ballots, Ranker, ranker, [Clause]).

	test(plackett_luce_export_to_file_4_loaded, deterministic(Best == beta)) :-
		^^file_path('test_output.pl', File),
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::export_to_file(ranked_ballots, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		plackett_luce_ranker::rank(LoadedRanker, [alpha, beta, gamma], [Best| _Ranking]).

	test(plackett_luce_tied_grouped_non_regular_error, error(domain_error(plackett_luce_regular_dataset, _))) :-
		plackett_luce_ranker::learn(tied_grouped, _Ranker).

	test(plackett_luce_sparse_grouped_missing_relevance_error, error(existence_error(relevance, ballot_one-gamma))) :-
		plackett_luce_ranker::learn(sparse_grouped_relevance, _Ranker, [missing_relevance(error)]).

	test(plackett_luce_search_results_non_regular_error, error(domain_error(plackett_luce_regular_dataset, _))) :-
		plackett_luce_ranker::learn(search_results, _Ranker).

	test(plackett_luce_malformed_grouped_error, error(domain_error(non_negative_integer, high))) :-
		plackett_luce_ranker::learn(malformed_grouped, _Ranker).

	test(plackett_luce_empty_group_error, error(domain_error(non_empty_group, ballot_one))) :-
		plackett_luce_ranker::learn(plackett_luce_empty_grouped, _Ranker).

	test(plackett_luce_rank_variable_candidate_error, error(instantiation_error)) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(plackett_luce_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(plackett_luce_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::rank(Ranker, alpha, _Ranking).

	test(plackett_luce_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(plackett_luce_valid_ranker_1, deterministic) :-
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::valid_ranker(Ranker).

	test(plackett_luce_invalid_valid_ranker_1, fail) :-
		plackett_luce_ranker::valid_ranker(fake_ranker([alpha], [alpha-1.0], [model(fake)])).

	test(plackett_luce_rank_invalid_ranker_error, error(domain_error(plackett_luce_ranker, fake_ranker([alpha], [alpha-1.0], [model(fake)])))) :-
		plackett_luce_ranker::rank(fake_ranker([alpha], [alpha-1.0], [model(fake)]), [alpha], _Ranking).

	test(plackett_luce_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		plackett_luce_ranker::learn(ranked_ballots, Ranker),
		plackett_luce_ranker::print_ranker(Ranker).

:- end_object.
