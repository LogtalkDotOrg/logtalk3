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


:- object(declaration_tie_pairwise,
	implements(pairwise_ranking_dataset_protocol)).

	item(gamma).
	item(alpha).
	item(beta).

	preference(gamma, alpha, 1).
	preference(alpha, beta, 1).
	preference(beta, gamma, 1).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-07,
		comment is 'Unit tests for the "kemeny_young_ranker" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(kemeny_young_ranker).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kemeny_young_learn_2, deterministic(ground(Ranker))) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker).

	test(kemeny_young_singleton_scores_2, deterministic(Scores == [alpha-0])) :-
		kemeny_young_ranker::learn(singleton_pairwise, Ranker),
		kemeny_young_ranker::scores(Ranker, Scores).

	test(kemeny_young_singleton_consensus_ranking_2, deterministic(ConsensusRanking == [alpha])) :-
		kemeny_young_ranker::learn(singleton_pairwise, Ranker),
		kemeny_young_ranker::consensus_ranking(Ranker, ConsensusRanking).

	test(kemeny_young_singleton_consensus_score_2, deterministic(ConsensusScore == 0)) :-
		kemeny_young_ranker::learn(singleton_pairwise, Ranker),
		kemeny_young_ranker::consensus_score(Ranker, ConsensusScore).

	test(kemeny_young_learn_3_custom_options, deterministic(Options == [tie_breaking(declaration_order)])) :-
		kemeny_young_ranker::learn(regular_head_to_head, kemeny_young_ranker(_Items, _Scores, Diagnostics), [tie_breaking(declaration_order)]),
		memberchk(options(Options), Diagnostics).

	test(kemeny_young_learn_3_unknown_option_error, error(domain_error(option, tie_policy(ignore_missing)))) :-
		kemeny_young_ranker::learn(regular_head_to_head, _Ranker, [tie_policy(ignore_missing)]).

	test(kemeny_young_learn_3_invalid_tie_breaking_error, error(domain_error(option, tie_breaking(random)))) :-
		kemeny_young_ranker::learn(regular_head_to_head, _Ranker, [tie_breaking(random)]).

	test(kemeny_young_learn_3_non_list_options_error, error(type_error(list, options))) :-
		kemeny_young_ranker::learn(regular_head_to_head, _Ranker, options).

	test(kemeny_young_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(kemeny_young_regular_head_to_head_scores_2, deterministic([AlphaScore, BetaScore, GammaScore, DeltaScore] == [3, 2, 1, 0])) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::scores(Ranker, Scores),
		memberchk(alpha-AlphaScore, Scores),
		memberchk(beta-BetaScore, Scores),
		memberchk(gamma-GammaScore, Scores),
		memberchk(delta-DeltaScore, Scores).

	test(kemeny_young_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		kemeny_young_ranker::learn(head_to_head, Ranker),
		kemeny_young_ranker::rank(Ranker, [gamma, delta, beta, alpha], Ranking).

	test(kemeny_young_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		kemeny_young_ranker::learn(cyclic_pairwise, Ranker),
		kemeny_young_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(kemeny_young_cyclic_pairwise_consensus_score_2, deterministic(ConsensusScore == 2)) :-
		kemeny_young_ranker::learn(cyclic_pairwise, Ranker),
		kemeny_young_ranker::consensus_score(Ranker, ConsensusScore).

	test(kemeny_young_condorcet_divergence_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		kemeny_young_ranker::learn(condorcet_divergence_pairwise, Ranker),
		kemeny_young_ranker::rank(Ranker, [gamma, beta, delta, alpha], Ranking).

	test(kemeny_young_condorcet_divergence_consensus_score_2, deterministic(ConsensusScore == 5)) :-
		kemeny_young_ranker::learn(condorcet_divergence_pairwise, Ranker),
		kemeny_young_ranker::consensus_score(Ranker, ConsensusScore).

	test(kemeny_young_term_order_tie_breaking_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		kemeny_young_ranker::learn(declaration_tie_pairwise, Ranker),
		kemeny_young_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(kemeny_young_declaration_order_tie_breaking_rank_3, deterministic(Ranking == [gamma, alpha, beta])) :-
		kemeny_young_ranker::learn(declaration_tie_pairwise, Ranker, [tie_breaking(declaration_order)]),
		kemeny_young_ranker::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(kemeny_young_declaration_order_consensus_ranking_2, deterministic(ConsensusRanking == [gamma, alpha, beta])) :-
		kemeny_young_ranker::learn(declaration_tie_pairwise, Ranker, [tie_breaking(declaration_order)]),
		kemeny_young_ranker::consensus_ranking(Ranker, ConsensusRanking).

	test(kemeny_young_diagnostics_2, deterministic([Model, Options, ConsensusRanking, ConsensusScore] == [kemeny_young_ranker, [tie_breaking(term_order)], [alpha, beta, gamma, delta], 35])) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::diagnostics(Ranker, Diagnostics),
		memberchk(model(Model), Diagnostics),
		memberchk(options(Options), Diagnostics),
		memberchk(consensus_ranking(ConsensusRanking), Diagnostics),
		memberchk(consensus_score(ConsensusScore), Diagnostics).

	test(kemeny_young_diagnostic_2, true) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::diagnostic(Ranker, model(kemeny_young_ranker)).

	test(kemeny_young_ranker_options_2, deterministic(Options == [tie_breaking(term_order)])) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::ranker_options(Ranker, Options).

	test(kemeny_young_export_to_clauses_4, deterministic(ground(Clause))) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(kemeny_young_export_to_file_4_loaded, deterministic((Ranking == [alpha, beta, gamma, delta], ConsensusScore == 35))) :-
		^^file_path('test_output.pl', File),
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		kemeny_young_ranker::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking),
		kemeny_young_ranker::consensus_score(LoadedRanker, ConsensusScore).

	test(kemeny_young_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		kemeny_young_ranker::learn(malformed_pairwise, _Ranker).

	test(kemeny_young_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		kemeny_young_ranker::learn(malformed_duplicate_items, _Ranker).

	test(kemeny_young_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		kemeny_young_ranker::learn(malformed_self_preference, _Ranker).

	test(kemeny_young_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		kemeny_young_ranker::learn(malformed_non_positive_weight, _Ranker).

	test(kemeny_young_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		kemeny_young_ranker::learn(disconnected_pairwise, _Ranker).

	test(kemeny_young_rank_variable_candidate_error, error(instantiation_error)) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::rank(Ranker, [_Candidate, beta], _Ranking).

	test(kemeny_young_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::rank(Ranker, [alpha, phantom], _Ranking).

	test(kemeny_young_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::rank(Ranker, alpha, _Ranking).

	test(kemeny_young_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(kemeny_young_valid_ranker_1, deterministic) :-
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::valid_ranker(Ranker).

	test(kemeny_young_invalid_valid_ranker_1, fail) :-
		kemeny_young_ranker::valid_ranker(fake_ranker([alpha], [alpha-0], [model(fake)])).

	test(kemeny_young_rank_invalid_ranker_error, error(domain_error(kemeny_young_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		kemeny_young_ranker::rank(fake_ranker([alpha], [alpha-0], [model(fake)]), [alpha], _Ranking).

	test(kemeny_young_scores_invalid_ranker_error, error(domain_error(kemeny_young_ranker, kemeny_young_ranker([alpha, beta], [alpha-1.5, beta-0], [model(kemeny_young_ranker), options([tie_breaking(term_order)]), consensus_ranking([alpha, beta]), consensus_score(1), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		kemeny_young_ranker::scores(kemeny_young_ranker([alpha, beta], [alpha-1.5, beta-0], [model(kemeny_young_ranker), options([tie_breaking(term_order)]), consensus_ranking([alpha, beta]), consensus_score(1), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(kemeny_young_consensus_score_invalid_ranker_error, error(domain_error(kemeny_young_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		kemeny_young_ranker::consensus_score(fake_ranker([alpha], [alpha-0], [model(fake)]), _ConsensusScore).

	test(kemeny_young_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		kemeny_young_ranker::learn(regular_head_to_head, Ranker),
		kemeny_young_ranker::print_ranker(Ranker).

:- end_object.
