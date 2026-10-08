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
		comment is 'Selects categorical or discretized continuous features using Pearson or Yates chi-square, or uncorrected or bias-corrected Cramer V.'
	]).

	:- private(score_columns/4).
	:- mode(score_columns(+list(compound), +atom, +term, -list(pair)), one_or_error).
	:- info(score_columns/4, [
		comment is 'Scores prepared columns using sparse contingency counts.',
		argnames is ['Columns', 'Criterion', 'ExpectedCountPolicy', 'Scores'],
		exceptions is [
			'A nondegenerate table has an expectation below the requested minimum' - domain_error(chi_square_expected_count, 'Feature-expected(ObservedMinimum,RequiredMinimum)')
		]
	]).

	:- private(check_expected_counts/3).
	:- mode(check_expected_counts(+term, +atomic, +compound), one_or_error).
	:- info(check_expected_counts/3, [
		comment is 'Checks the requested minimum expectation for nondegenerate tables.',
		argnames is ['Policy', 'Feature', 'Counts'],
		exceptions is [
			'A nondegenerate table has an expectation below the requested minimum' - domain_error(chi_square_expected_count, 'Feature-expected(ObservedMinimum,RequiredMinimum)')
		]
	]).

	filter_model(chi_square_feature_selector).

	filter_scoring_metric(Options, Metric) :-
		^^option(score_variant(Variant), Options),
		(	Variant == raw ->
			Metric = chi_square_score
		;	Variant == normalized ->
			Metric = cramers_v_score
		;	Variant == yates ->
			Metric = chi_square_yates_score
		;	Metric = cramers_v_bias_corrected_score
		).

	filter_feature_scores(Dataset, Features, Examples, Options, Scores, [scoring_metric(Metric)| Diagnostics]) :-
		^^option(preparation_mode(Mode), Options),
		^^prepare_feature_columns(Dataset, Features, Examples, Options, Mode, Columns, Diagnostics),
		filter_scoring_metric(Options, Metric),
		(	Metric == chi_square_score ->
			Criterion = chi_square
		;	Metric == cramers_v_score ->
			Criterion = cramers_v
		;	Metric == chi_square_yates_score ->
			Criterion = chi_square_yates
		;	Criterion = cramers_v_bias_corrected
		),
		^^option(expected_count_policy(Policy), Options),
		score_columns(Columns, Criterion, Policy, Unsorted),
		^^sort_by_decreasing_score(Unsorted, Scores).

	default_option(score_variant(raw)).
	default_option(preparation_mode(per_feature)).
	default_option(expected_count_policy(ignore)).
	default_option(discretization(equal_frequency(10))).
	default_option(Option) :-
		^^default_option(Option).

	valid_option(score_variant(Variant)) :-
		once((Variant == raw; Variant == normalized; Variant == yates; Variant == bias_corrected)).
	valid_option(preparation_mode(Mode)) :-
		once((Mode == per_feature; Mode == joint)).
	valid_option(expected_count_policy(Policy)) :-
		(	Policy == ignore ->
			true
		;	Policy = minimum(Minimum),
			number(Minimum),
			Minimum > 0
		).
	valid_option(discretization(Specification)) :-
		^^valid_feature_discretization(Specification).
	valid_option(feature_discretization(Feature, Specification)) :-
		atomic(Feature),
		^^valid_feature_discretization(Specification).
	valid_option(Option) :-
		^^valid_option(Option).

	filter_validate_diagnostics(Options, Diagnostics) :-
		^^option(preparation_mode(Mode), Options),
		^^filter_valid_preparation_diagnostics(Mode, Diagnostics).

	score_columns([], _Criterion, _Policy, []).
	score_columns([column(Feature, _Specification, Pairs, _CategoryCount)| Columns], Criterion, Policy, [Feature-Score| Scores]) :-
		^^contingency_counts(Pairs, Counts),
		check_expected_counts(Policy, Feature, Counts),
		^^contingency_score(Criterion, Counts, Score),
		score_columns(Columns, Criterion, Policy, Scores).

	check_expected_counts(Policy, Feature, Counts) :-
		(	Policy == ignore ->
			true
		;	Counts = contingency(Total, Rows, Columns, _Cells),
			avltree::size(Rows, RowCount),
			avltree::size(Columns, ColumnCount),
			(	(Total < 2; RowCount < 2; ColumnCount < 2) ->
				true
			;	Policy = minimum(Required),
				^^contingency_min_expected_count(Counts, Observed),
				(	Observed >= Required ->
					true
				;	domain_error(chi_square_expected_count, Feature-expected(Observed, Required))
				)
			)
		).

:- end_object.
