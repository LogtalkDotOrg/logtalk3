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


:- object(fisher_score_feature_selector,
	imports(filter_feature_selector_common)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Selects continuous features for categorical targets using Fisher scores, top-k selection, or a score threshold.'
	]).

	:- private(check_declarations/1).
	:- mode(check_declarations(+list(pair)), one_or_error).
	:- info(check_declarations/1, [
		comment is 'Checks that every feature is declared continuous.',
		argnames is ['Declarations'],
		exceptions is [
			'A feature is not declared continuous' - domain_error(feature_type, 'Feature-Declaration')
		]
	]).

	filter_model(fisher_score_feature_selector).

	default_option(selection_strategy(top_k(10))).

	valid_option(selection_strategy(Strategy)) :-
		(	(Strategy == all; Strategy == largest_gap) ->
			true
		;	Strategy = top_k(Count) ->
			integer(Count),
			Count > 0
		;	Strategy = threshold(Threshold),
			number(Threshold)
		).

	filter_selection(largest_gap, Scores, Selected) :-
		!,
		largest_gap_count(Scores, Count),
		^^select_top_k(Scores, Count, Selected).
	filter_selection(Strategy, Scores, Selected) :-
		^^filter_selection(Strategy, Scores, Selected).

	largest_gap_count([], 0).
	largest_gap_count([_-Score| Scores], Count) :-
		(	Score > 0 ->
			gap_boundary(Scores, Score, 1, 0, 0, Count)
		;	Count = 0
		).

	gap_boundary([], _Previous, Position, Gap, Boundary, Count) :-
		(	Gap > 0 ->
			Count = Boundary
		;	Count = Position
		).
	gap_boundary([_-Score| Scores], Previous, Position, Gap0, Boundary0, Count) :-
		Difference is Previous - Score,
		(	Difference > Gap0 ->
			Gap = Difference,
			Boundary = Position
		;	Gap = Gap0,
			Boundary = Boundary0
		),
		(	Score > 0 ->
			Next is Position + 1,
			gap_boundary(Scores, Score, Next, Gap, Boundary, Count)
		;	Count = Boundary
		).

	filter_scoring_metric(_Options, fisher_score).

	filter_validate_dataset(Dataset, _Features, _Examples) :-
		findall(Feature-Declaration, Dataset::attribute_values(Feature, Declaration), Declarations),
		check_declarations(Declarations).

	check_declarations([]).
	check_declarations([Feature-Declaration| Declarations]) :-
		(	Declaration == continuous ->
			true
		;	domain_error(feature_type, Feature-Declaration)
		),
		check_declarations(Declarations).

:- end_object.
