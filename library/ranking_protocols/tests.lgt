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


:- object(ranking_test_support,
	imports(ranking_dataset_common)).

	:- public(rank_candidates/3).
	:- public(group_tie_blocks/4).
	:- public(win_totals/2).

	:- uses(list, [
		length/2, member/2, memberchk/2, sort/4
	]).

	:- uses(pairs, [
		values/2
	]).

	win_totals(Dataset, Totals) :-
		::pairwise_dataset_win_totals(Dataset, Totals).

	group_tie_blocks(Dataset, Group, MissingRelevance, TieBlocks) :-
		::grouped_dataset_tie_blocks(Dataset, Group, MissingRelevance, TieBlocks).

	rank_candidates(Strengths, Candidates, Ranking) :-
		findall(
			pair(NegStrength, Item)-Item,
			(
				member(Item, Candidates),
				memberchk(Item-Strength, Strengths),
				NegStrength is -Strength
			),
			Pairs
		),
		sort(1, @=<, Pairs, SortedPairs),
		values(SortedPairs, Ranking).

:- end_object.


:- object(sample_ranker,
	implements(ranker_protocol),
	imports([ranking_dataset_common, ranker_common])).

	:- uses(list, [
		member/2
	]).

	learn(Dataset, sample_ranker(Strengths, Diagnostics)) :-
		^^validate_pairwise_dataset(Dataset, Summary),
		ranking_test_support::win_totals(Dataset, Strengths),
		Diagnostics = [
			model(sample_ranker),
			options([]),
			convergence(not_applicable),
			iterations(0),
			final_delta(0.0),
			dataset_summary(Summary)
		].

	rank(sample_ranker(Strengths, _Diagnostics), Candidates, Ranking) :-
		ranking_test_support::rank_candidates(Strengths, Candidates, Ranking).

	ranker_scores_data(sample_ranker(Strengths, _Diagnostics), Strengths).

	ranker_diagnostics_data(sample_ranker(_Strengths, Diagnostics), Diagnostics).

	ranker_export_template(_Dataset, _Ranker, Functor, Template) :-
		Template =.. [Functor, 'Ranker'].

	ranker_term_template(sample_ranker(_Strengths, _Diagnostics), sample_ranker('Strengths', 'Diagnostics')).

	export_to_clauses(_Dataset, Ranker, Functor, [Clause]) :-
		Clause =.. [Functor, Ranker].

	print_ranker(Ranker) :-
		^^print_ranker_template(Ranker),
		writeq(Ranker), nl.

:- end_object.


:- object(ranking_empty_grouped,
	implements(ranking_dataset_protocol)).

	group(ballot_one).

:- end_object.


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-26,
		comment is 'Smoke tests for the "ranking_protocols" library.'
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	cleanup :-
		^^clean_file('test_output.pl').

	% Pairwise dataset protocol smoke tests.

	test(head_to_head_items, deterministic(Items == [alpha, beta, gamma, delta])) :-
		findall(Item, head_to_head::item(Item), Items).

	test(head_to_head_preferences_count, deterministic(Count == 6)) :-
		findall(Winner-Loser, head_to_head::preference(Winner, Loser, _Weight), Preferences),
		length(Preferences, Count).

	test(head_to_head_validation, deterministic) :-
		ranking_test_support::validate_pairwise_dataset(head_to_head).

	test(malformed_pairwise_validation, error(existence_error(item, phantom))) :-
		ranking_test_support::validate_pairwise_dataset(malformed_pairwise).

	test(malformed_duplicate_items_validation, error(domain_error(unique_items, [alpha, alpha, beta]))) :-
		ranking_test_support::validate_pairwise_dataset(malformed_duplicate_items).

	test(malformed_self_preference_validation, error(domain_error(distinct_items, alpha-alpha))) :-
		ranking_test_support::validate_pairwise_dataset(malformed_self_preference).

	test(malformed_non_positive_weight_validation, error(domain_error(positive_number, 0))) :-
		ranking_test_support::validate_pairwise_dataset(malformed_non_positive_weight).

	test(disconnected_pairwise_validation, error(domain_error(connected_pairwise_dataset, [[alpha, beta], [gamma, delta]]))) :-
		ranking_test_support::validate_pairwise_dataset(disconnected_pairwise).

	test(cyclic_pairwise_validation, deterministic) :-
		ranking_test_support::validate_pairwise_dataset(cyclic_pairwise).

	test(sparse_preferences_summary, deterministic(memberchk(isolated_items([gamma]), Summary))) :-
		ranking_test_support::pairwise_dataset_summary(sparse_preferences, Summary).

	% Grouped dataset protocol smoke tests.

	test(search_results_groups, deterministic(Groups == [query_one, query_two])) :-
		findall(Group, search_results::group(Group), Groups).

	test(search_results_group_items, deterministic((memberchk(doc_alpha, QueryOneItems), memberchk(doc_gamma, QueryOneItems)))) :-
		findall(Item, search_results::item(query_one, Item), QueryOneItems).

	test(search_results_relevance, true) :-
		search_results::relevance(query_one, doc_alpha, 3),
		search_results::relevance(query_two, doc_zeta, 0).

	test(search_results_validation, deterministic) :-
		ranking_test_support::validate_grouped_dataset(search_results).

	test(tied_grouped_tie_blocks, deterministic(TieBlocks == [tie_block(1, [alpha, beta]), tie_block(0, [gamma])])) :-
		ranking_test_support::group_tie_blocks(tied_grouped, ballot_one, zero, TieBlocks).

	test(sparse_grouped_tie_blocks_default_zero, deterministic(TieBlocks == [tie_block(2, [alpha]), tie_block(1, [beta]), tie_block(0, [gamma])])) :-
		ranking_test_support::group_tie_blocks(sparse_grouped_relevance, ballot_one, zero, TieBlocks).

	test(malformed_grouped_validation, error(domain_error(non_negative_integer, high))) :-
		ranking_test_support::validate_grouped_dataset(malformed_grouped).

	test(empty_grouped_validation, error(domain_error(non_empty_group, ballot_one))) :-
		ranking_test_support::validate_grouped_dataset(ranking_empty_grouped).

	% Sample ranker protocol smoke tests.

	test(sample_ranker_learn_2, deterministic(ground(Ranker))) :-
		sample_ranker::learn(head_to_head, Ranker).

	test(sample_ranker_rank_3, deterministic(Ranking == [alpha, beta, gamma, delta])) :-
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::rank(Ranker, [alpha, beta, gamma, delta], Ranking).

	test(sample_ranker_scores_2, deterministic(Scores == [alpha-10, beta-5, gamma-4, delta-0])) :-
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::scores(Ranker, Scores).

	test(sample_ranker_diagnostics_2, deterministic(memberchk(model(sample_ranker), Diagnostics))) :-
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::diagnostics(Ranker, Diagnostics).

	test(sample_ranker_diagnostic_2_enumerates, deterministic(Enumerated == Diagnostics)) :-
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::diagnostics(Ranker, Diagnostics),
		findall(Diagnostic, sample_ranker::diagnostic(Ranker, Diagnostic), Enumerated).

	test(sample_ranker_export_to_clauses_4, deterministic(Clause == ranker(sample_ranker([alpha-10, beta-5, gamma-4, delta-0], [model(sample_ranker), options([]), convergence(not_applicable), iterations(0), final_delta(0.0), dataset_summary([items(4), preferences(6), connected_components(1), isolated_items([])])])))) :-
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::export_to_clauses(head_to_head, Ranker, ranker, [Clause]).

	test(sample_ranker_export_to_file_4_header, deterministic(HeaderLines == ['% exported ranker predicate: ranker/1', '% training dataset: head_to_head', '% diagnostics: [model(sample_ranker),options([]),convergence(not_applicable),iterations(0),final_delta(0.0),dataset_summary([items(4),preferences(6),connected_components(1),isolated_items([])])]', '% ranker(Ranker)'])) :-
		^^file_path('test_output.pl', File),
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::export_to_file(head_to_head, Ranker, ranker, File),
		header_lines(File, HeaderLines).

	test(sample_ranker_export_to_file_4, deterministic(memberchk(model(sample_ranker), Diagnostics))) :-
		^^file_path('test_output.pl', File),
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::export_to_file(head_to_head, Ranker, ranker, File),
		logtalk_load(File),
		{ranker(LoadedRanker)},
		LoadedRanker = sample_ranker(_Strengths, Diagnostics).

	test(sample_ranker_print_ranker_1, deterministic) :-
		^^suppress_text_output,
		sample_ranker::learn(head_to_head, Ranker),
		sample_ranker::print_ranker(Ranker).

	header_lines(File, Lines) :-
		open(File, read, Stream),
		read_line_atom(Stream, Line1),
		read_line_atom(Stream, Line2),
		read_line_atom(Stream, Line3),
		read_line_atom(Stream, Line4),
		close(Stream),
		Lines = [Line1, Line2, Line3, Line4].

	read_line_atom(Stream, Line) :-
		get_code(Stream, Code),
		(	Code == -1 ->
			Line = end_of_file
		;	read_line_codes(Code, Stream, Codes),
			atom_codes(Line, Codes)
		).

	read_line_codes(-1, _Stream, []) :-
		!.
	read_line_codes(10, _Stream, []) :-
		!.
	read_line_codes(13, Stream, Codes) :-
		!,
		get_code(Stream, NextCode),
		(	NextCode == 10 ->
			Codes = []
		;	read_line_codes(NextCode, Stream, Codes)
		).
	read_line_codes(Code, Stream, [Code| Codes]) :-
		get_code(Stream, NextCode),
		read_line_codes(NextCode, Stream, Codes).

:- end_object.
