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
		date is 2026-04-27,
		comment is 'Unit tests for the "kemeny_young" library.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(kemeny_young).

	cleanup :-
		^^clean_file('test_output.pl').

	test(kemeny_young_learn_2, deterministic(ground(Ranker))) :-
		kemeny_young::learn(regular_head_to_head, Ranker).

	test(kemeny_young_singleton_scores_2, deterministic(Scores == [alpha-0])) :-
		kemeny_young::learn(singleton_pairwise, Ranker),
		kemeny_young::scores(Ranker, Scores).

	test(kemeny_young_singleton_consensus_ranking_2, deterministic(ConsensusRanking == [alpha])) :-
		kemeny_young::learn(singleton_pairwise, Ranker),
		kemeny_young::consensus_ranking(Ranker, ConsensusRanking).

	test(kemeny_young_singleton_consensus_score_2, deterministic(ConsensusScore == 0)) :-
		kemeny_young::learn(singleton_pairwise, Ranker),
		kemeny_young::consensus_score(Ranker, ConsensusScore).

	test(kemeny_young_learn_3_custom_options, deterministic(memberchk(options([tie_breaking(declaration_order)]), Diagnostics))) :-
		kemeny_young::learn(regular_head_to_head, kemeny_young_ranker(_Items, _Scores, Diagnostics), [tie_breaking(declaration_order)]).

	test(kemeny_young_learn_3_unknown_option_error, error(domain_error(option, tie_policy(ignore_missing)))) :-
		kemeny_young::learn(regular_head_to_head, _Ranker, [tie_policy(ignore_missing)]).

	test(kemeny_young_learn_3_invalid_tie_breaking_error, error(domain_error(option, tie_breaking(random)))) :-
		kemeny_young::learn(regular_head_to_head, _Ranker, [tie_breaking(random)]).

	test(kemeny_young_learn_3_non_list_options_error, error(type_error(list, options))) :-
		kemeny_young::learn(regular_head_to_head, _Ranker, options).

	test(kemeny_young_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(kemeny_young_regular_head_to_head_scores_2, deterministic((memberchk(alpha-3, Scores), memberchk(beta-2, Scores), memberchk(gamma-1, Scores), memberchk(delta-0, Scores)))) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::scores(Ranker, Scores).

	test(kemeny_young_head_to_head_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		kemeny_young::learn(head_to_head, Ranker),
		kemeny_young::rank(Ranker, [gamma, delta, beta, alpha], Ranking).

	test(kemeny_young_cyclic_pairwise_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		kemeny_young::learn(cyclic_pairwise, Ranker),
		kemeny_young::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(kemeny_young_cyclic_pairwise_consensus_score_2, deterministic(ConsensusScore == 2)) :-
		kemeny_young::learn(cyclic_pairwise, Ranker),
		kemeny_young::consensus_score(Ranker, ConsensusScore).

	test(kemeny_young_condorcet_divergence_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		kemeny_young::learn(condorcet_divergence_pairwise, Ranker),
		kemeny_young::rank(Ranker, [gamma, beta, delta, alpha], Ranking).

	test(kemeny_young_condorcet_divergence_consensus_score_2, deterministic(ConsensusScore == 5)) :-
		kemeny_young::learn(condorcet_divergence_pairwise, Ranker),
		kemeny_young::consensus_score(Ranker, ConsensusScore).

	test(kemeny_young_term_order_tie_breaking_rank_3, deterministic(Ranking == [alpha, beta, gamma])) :-
		kemeny_young::learn(declaration_tie_pairwise, Ranker),
		kemeny_young::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(kemeny_young_declaration_order_tie_breaking_rank_3, deterministic(Ranking == [gamma, alpha, beta])) :-
		kemeny_young::learn(declaration_tie_pairwise, Ranker, [tie_breaking(declaration_order)]),
		kemeny_young::rank(Ranker, [gamma, alpha, beta], Ranking).

	test(kemeny_young_declaration_order_consensus_ranking_2, deterministic(ConsensusRanking == [gamma, alpha, beta])) :-
		kemeny_young::learn(declaration_tie_pairwise, Ranker, [tie_breaking(declaration_order)]),
		kemeny_young::consensus_ranking(Ranker, ConsensusRanking).

	test(kemeny_young_diagnostics_2, deterministic((memberchk(model(kemeny_young), Diagnostics), memberchk(options([tie_breaking(term_order)]), Diagnostics), memberchk(consensus_ranking([alpha, beta, gamma, delta]), Diagnostics), memberchk(consensus_score(35), Diagnostics)))) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::diagnostics(Ranker, Diagnostics).

	test(kemeny_young_diagnostic_2, true) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::diagnostic(Ranker, model(kemeny_young)).

	test(kemeny_young_ranker_options_2, deterministic(Options == [tie_breaking(term_order)])) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::ranker_options(Ranker, Options).

	test(kemeny_young_export_to_clauses_4, deterministic(ground(Clause))) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::export_to_clauses(regular_head_to_head, Ranker, ranker, [Clause]).

	test(kemeny_young_export_to_file_4_loaded, deterministic((Ranking == [alpha, beta, gamma, delta], ConsensusScore == 35))) :-
		^^file_path('test_output.pl', File),
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::export_to_file(regular_head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		kemeny_young::rank(LoadedRanker, [alpha, beta, gamma, delta], Ranking),
		kemeny_young::consensus_score(LoadedRanker, ConsensusScore).

	test(kemeny_young_malformed_pairwise_error, error(existence_error(item, phantom))) :-
		kemeny_young::learn(malformed_pairwise, _Ranker).

	test(kemeny_young_duplicate_items_error, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		kemeny_young::learn(malformed_duplicate_items, _Ranker).

	test(kemeny_young_self_preference_error, error(domain_error(distinct_items, alpha-alpha))) :-
		kemeny_young::learn(malformed_self_preference, _Ranker).

	test(kemeny_young_non_positive_weight_error, error(domain_error(positive_number, 0))) :-
		kemeny_young::learn(malformed_non_positive_weight, _Ranker).

	test(kemeny_young_disconnected_pairwise_error, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		kemeny_young::learn(disconnected_pairwise, _Ranker).

	test(kemeny_young_rank_variable_candidate_error, error(instantiation_error)) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::rank(Ranker, [_Candidate, beta], _Ranking).

	test(kemeny_young_rank_unknown_candidate_error, error(existence_error(item, phantom))) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::rank(Ranker, [alpha, phantom], _Ranking).

	test(kemeny_young_rank_non_list_candidates_error, error(type_error(list, alpha))) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::rank(Ranker, alpha, _Ranking).

	test(kemeny_young_rank_duplicate_candidates_error, error(domain_error(unique_candidates, [alpha, alpha, beta]))) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::rank(Ranker, [alpha, alpha, beta], _Ranking).

	test(kemeny_young_valid_ranker_1, deterministic) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::valid_ranker(Ranker).

	test(kemeny_young_invalid_valid_ranker_1, fail) :-
		kemeny_young::valid_ranker(fake_ranker([alpha], [alpha-0], [model(fake)])).

	test(kemeny_young_rank_invalid_ranker_error, error(domain_error(kemeny_young_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		kemeny_young::rank(fake_ranker([alpha], [alpha-0], [model(fake)]), [alpha], _Ranking).

	test(kemeny_young_scores_invalid_ranker_error, error(domain_error(kemeny_young_ranker, kemeny_young_ranker([alpha, beta], [alpha-1.5, beta-0], [model(kemeny_young), options([tie_breaking(term_order)]), consensus_ranking([alpha, beta]), consensus_score(1), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])])))) :-
		kemeny_young::scores(kemeny_young_ranker([alpha, beta], [alpha-1.5, beta-0], [model(kemeny_young), options([tie_breaking(term_order)]), consensus_ranking([alpha, beta]), consensus_score(1), dataset_summary([items(2), preferences(1), connected_components(1), isolated_items([])])]), _Scores).

	test(kemeny_young_consensus_score_invalid_ranker_error, error(domain_error(kemeny_young_ranker, fake_ranker([alpha], [alpha-0], [model(fake)])))) :-
		kemeny_young::consensus_score(fake_ranker([alpha], [alpha-0], [model(fake)]), _ConsensusScore).

	test(kemeny_young_print_ranker_1, deterministic) :-
		kemeny_young::learn(regular_head_to_head, Ranker),
		kemeny_young::print_ranker(Ranker).

:- end_object.
