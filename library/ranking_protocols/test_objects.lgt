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


:- object(condorcet_test_support,
	imports([ranking_dataset_common, condorcet_victory_common])).

	:- public(indexed_items/2).
	:- public(direct_strengths/3).
	:- public(zero_square_matrix/2).
	:- public(matrix_entry/4).
	:- public(update_matrix_entry/5).

	:- uses(avltree, [
		as_dictionary/2
	]).

	:- uses(list, [
		length/2
	]).

	indexed_items(Dataset, IndexPairs) :-
		^^pairwise_dataset_items(Dataset, Items),
		^^index_items(Items, 1, IndexPairs).

	direct_strengths(Dataset, VictoryStrength, DirectStrengths) :-
		^^validate_pairwise_dataset(Dataset, _Summary),
		^^pairwise_dataset_items(Dataset, Items),
		^^pairwise_dataset_matchups(Dataset, Matchups),
		^^index_items(Items, 1, IndexPairs),
		as_dictionary(IndexPairs, IndexDictionary),
		length(Items, Count),
		^^build_direct_strengths(Matchups, IndexDictionary, Count, VictoryStrength, DirectStrengths).

	zero_square_matrix(Count, Matrix) :-
		^^zero_matrix(Count, Matrix).

	matrix_entry(Matrix, RowIndex, ColumnIndex, Value) :-
		^^matrix_entry(Matrix, RowIndex, ColumnIndex, Value).

	update_matrix_entry(Matrix, RowIndex, ColumnIndex, Value, UpdatedMatrix) :-
		^^set_matrix_entry(Matrix, RowIndex, ColumnIndex, Value, UpdatedMatrix).

:- end_object.


:- object(sample_ranker,
	implements(ranker_protocol),
	imports([ranking_dataset_common, ranker_common])).

	:- uses(list, [
		member/2, memberchk/2
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

	check_ranker(sample_ranker(Strengths, Diagnostics)) :-
		valid_strengths(Strengths),
		^^valid_ranker_metadata(sample_ranker, Diagnostics),
		memberchk(convergence(not_applicable), Diagnostics),
		memberchk(iterations(0), Diagnostics),
		memberchk(final_delta(0.0), Diagnostics).

	valid_strengths([]).
	valid_strengths([Item-Strength| Strengths]) :-
		nonvar(Item),
		number(Strength),
		Strength >= 0,
		valid_strengths(Strengths).

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
