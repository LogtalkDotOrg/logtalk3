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


:- object(chi_square_feature_selector,
	imports([filter_feature_selector_common, feature_discretization])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Selects categorical or discretized continuous features using Pearson chi-square or Cramer V.'
	]).

	:- private(score_columns/3).
	:- mode(score_columns(+list(compound), +atom, -list(pair)), one).
	:- info(score_columns/3, [
		comment is 'Scores prepared columns using sparse contingency counts.',
		argnames is ['Columns', 'Criterion', 'Scores']
	]).

	filter_model(chi_square_feature_selector).

	filter_scoring_metric(Options, Metric) :-
		^^option(score_variant(Variant), Options),
		(	Variant == raw ->
			Metric = chi_square_score
		;
			Metric = cramers_v_score
		).

	filter_feature_scores(Dataset, Features, Examples, Options, Scores, [scoring_metric(Metric)| Diagnostics]) :-
		^^prepare_feature_columns(Dataset, Features, Examples, Options, per_feature, Columns, Diagnostics),
		filter_scoring_metric(Options, Metric),
		(	Metric == chi_square_score ->
			Criterion = chi_square
		;
			Criterion = cramers_v
		),
		score_columns(Columns, Criterion, Unsorted),
		^^sort_by_decreasing_score(Unsorted, Scores).

	default_option(score_variant(raw)).
	default_option(discretization(equal_frequency(10))).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(score_variant(Variant)) :-
		once((Variant == raw; Variant == normalized)).
	valid_option(discretization(Specification)) :-
		^^valid_feature_discretization(Specification).
	valid_option(feature_discretization(Feature, Specification)) :-
		atomic(Feature),
		^^valid_feature_discretization(Specification).
	valid_option(Option) :-
		^^valid_option(Option).

	score_columns([], _Criterion, []).
	score_columns([column(Feature, _Specification, Pairs, _CategoryCount)| Columns], Criterion, [Feature-Score| Scores]) :-
		^^contingency_counts(Pairs, Counts),
		^^contingency_score(Criterion, Counts, Score),
		score_columns(Columns, Criterion, Scores).

:- end_object.
