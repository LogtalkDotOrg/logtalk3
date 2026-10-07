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
		date is 2026-10-07,
		comment is 'Unit tests for the chi-square feature selector library.'
	]).

	:- uses(lgtunit, [op(700, xfx, =~=), (=~=)/2, assertion/1]).
	:- uses(list, [memberchk/2]).

	cover(chi_square_feature_selector).

	cleanup :-
		^^clean_file('test_chi_square_selector_output.pl').

	test(chi_square_feature_selector_raw_reference, deterministic) :-
		chi_square_feature_selector::learn(feature_selection_categorical_dataset, Selector),
		chi_square_feature_selector::check_selector(Selector),
		chi_square_feature_selector::feature_scores(Selector, [signal-Signal, noise-Noise, constant-Constant]),
		assertion(Signal =~= 4.0),
		assertion(Noise =~= 0.0),
		assertion(Constant =~= 0.0).

	test(chi_square_feature_selector_normalized_reference, deterministic(Score =~= 1.0)) :-
		chi_square_feature_selector::learn(feature_selection_categorical_dataset, Selector, [score_variant(normalized)]),
		chi_square_feature_selector::check_selector(Selector),
		chi_square_feature_selector::feature_scores(Selector, [signal-Score| _]),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(scoring_metric(cramers_v_score), Diagnostics).

	test(chi_square_feature_selector_distinct_normalization, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(unique), Raw),
		chi_square_feature_selector::feature_scores(Raw, [signal-RawScore| _]),
		chi_square_feature_selector::learn(chi_square_dataset(unique), Normalized, [score_variant(normalized)]),
		chi_square_feature_selector::feature_scores(Normalized, [signal-NormalizedScore| _]),
		assertion(RawScore =~= 4.0),
		assertion(NormalizedScore =~= 1.0).

	test(chi_square_feature_selector_equal_width, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [discretization(equal_width(2))]),
		chi_square_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 4.0),
		assertion(Copy =~= Signal),
		assertion(Constant =~= 0.0),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([signal-equal_width(2), copy-categorical, constant-categorical]), Diagnostics),
		memberchk(occupied_categories([signal-2, copy-2, constant-1]), Diagnostics).

	test(chi_square_feature_selector_equal_frequency, deterministic(Score =~= 4.0)) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [discretization(equal_frequency(2))]),
		chi_square_feature_selector::feature_scores(Selector, [signal-Score| _]).

	test(chi_square_feature_selector_continuous_default, deterministic) :-
		chi_square_feature_selector::learn(feature_demo, Selector),
		chi_square_feature_selector::check_selector(Selector),
		assertion(ground(Selector)),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([f1-equal_frequency(10), f2-equal_frequency(10), f3-equal_frequency(10)]), Diagnostics).

	test(chi_square_feature_selector_ties, deterministic(Selected == [signal])) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [discretization(equal_width(2)), selection_strategy(top_k(1))]),
		chi_square_feature_selector::selected_features(Selector, Selected).

	test(chi_square_feature_selector_all, deterministic(Selected == [signal, copy, constant])) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [selection_strategy(all)]),
		chi_square_feature_selector::selected_features(Selector, Selected).

	test(chi_square_feature_selector_threshold, deterministic(Selected == [signal, copy])) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [discretization(equal_frequency(2)), selection_strategy(threshold(0.5))]),
		chi_square_feature_selector::selected_features(Selector, Selected).

	test(chi_square_feature_selector_empty_selection, deterministic(Selected == [])) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [selection_strategy(threshold(5))]),
		chi_square_feature_selector::selected_features(Selector, Selected).

	test(chi_square_feature_selector_repeated_options, deterministic) :-
		Options = [score_variant(normalized), score_variant(raw), discretization(equal_width(2)), discretization(equal_frequency(1)), selection_strategy(top_k(1)), selection_strategy(all)],
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, Options),
		chi_square_feature_selector::selected_features(Selector, [signal]),
		chi_square_feature_selector::selector_options(Selector, Options),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(scoring_metric(cramers_v_score), Diagnostics),
		memberchk(discretization([signal-equal_width(2), copy-categorical, constant-categorical]), Diagnostics).

	test(chi_square_feature_selector_repeated_overrides, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [feature_discretization(signal, equal_width(1)), feature_discretization(signal, equal_frequency(2))]),
		chi_square_feature_selector::feature_scores(Selector, Scores),
		memberchk(signal-Score, Scores),
		assertion(Score =~= 0.0),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([signal-equal_width(1), copy-categorical, constant-categorical]), Diagnostics).

	test(chi_square_feature_selector_categorical_override, deterministic(Score =~= 4.0)) :-
		chi_square_feature_selector::learn(chi_square_dataset(mixed), Selector, [feature_discretization(signal, categorical)]),
		chi_square_feature_selector::feature_scores(Selector, [signal-Score| _]).

	test(chi_square_feature_selector_missing, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(missing), Selector, [discretization(equal_width(2))]),
		chi_square_feature_selector::check_selector(Selector),
		chi_square_feature_selector::feature_scores(Selector, [signal-Score| _]),
		assertion(Score =~= 4.0),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(complete_cases([signal-4, copy-3, constant-5]), Diagnostics),
		memberchk(example_count(6), Diagnostics),
		memberchk(occupied_categories([signal-2, copy-2, constant-1]), Diagnostics),
		memberchk(preparation_mode(per_feature), Diagnostics).

	test(chi_square_feature_selector_all_missing, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(all_missing), Selector, [score_variant(normalized)]),
		chi_square_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 0.0),
		assertion(Copy =~= 0.0),
		assertion(Constant =~= 0.0),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(complete_cases([signal-0, copy-0, constant-0]), Diagnostics),
		memberchk(occupied_categories([signal-0, copy-0, constant-0]), Diagnostics).

	test(chi_square_feature_selector_single_class, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(single_class), Selector),
		chi_square_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 0.0),
		assertion(Copy =~= 0.0),
		assertion(Constant =~= 0.0).

	test(chi_square_feature_selector_option_api, true) :-
		chi_square_feature_selector::default_option(score_variant(raw)),
		chi_square_feature_selector::default_option(discretization(equal_frequency(10))),
		chi_square_feature_selector::default_option(selection_strategy(top_k(10))),
		chi_square_feature_selector::valid_option(selection_strategy(all)),
		chi_square_feature_selector::valid_option(selection_strategy(threshold(0.5))),
		chi_square_feature_selector::valid_option(feature_discretization(signal, categorical)),
		chi_square_feature_selector::current_predicate(valid_option/1),
		chi_square_feature_selector::current_predicate(default_option/1).

	test(chi_square_feature_selector_diagnostics, deterministic) :-
		chi_square_feature_selector::learn(feature_selection_categorical_dataset, Selector),
		functor(Selector, chi_square_feature_selector, 3),
		assertion(ground(Selector)),
		chi_square_feature_selector::valid_selector(Selector),
		chi_square_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(model(chi_square_feature_selector), Diagnostics),
		memberchk(candidate_count(3), Diagnostics),
		memberchk(selected_count(3), Diagnostics),
		memberchk(scoring_metric(chi_square_score), Diagnostics),
		memberchk(complete_cases([signal-4, noise-4, constant-4]), Diagnostics),
		findall(Metric, chi_square_feature_selector::diagnostic(Selector, scoring_metric(Metric)), [chi_square_score]).

	test(chi_square_feature_selector_unknown_override, error(domain_error(unknown_feature, unknown))) :-
		chi_square_feature_selector::learn(feature_demo, _, [feature_discretization(unknown, equal_width(2))]).

	test(chi_square_feature_selector_invalid_variant, error(domain_error(option, score_variant(bad)))) :-
		chi_square_feature_selector::learn(feature_demo, _, [score_variant(bad)]).

	test(chi_square_feature_selector_invalid_bins, error(domain_error(option, discretization(equal_width(0))))) :-
		chi_square_feature_selector::learn(feature_demo, _, [discretization(equal_width(0))]).

	test(chi_square_feature_selector_invalid_override, error(domain_error(option, feature_discretization(f1, bad)))) :-
		chi_square_feature_selector::learn(feature_demo, _, [feature_discretization(f1, bad)]).

	test(chi_square_feature_selector_unbound_configuration, error(domain_error(option, discretization(equal_frequency(_))))) :-
		chi_square_feature_selector::learn(feature_demo, _, [discretization(equal_frequency(_))]).

	test(chi_square_feature_selector_invalid_later_override, error(domain_error(option, feature_discretization(f1, equal_width(0))))) :-
		chi_square_feature_selector::learn(feature_demo, _, [feature_discretization(f1, equal_width(2)), feature_discretization(f1, equal_width(0))]).

	test(chi_square_feature_selector_invalid_strategy, error(domain_error(option, selection_strategy(top_k(0))))) :-
		chi_square_feature_selector::learn(feature_demo, _, [selection_strategy(top_k(0))]).

	test(chi_square_feature_selector_outside_domain, error(domain_error(feature_value, signal-9))) :-
		chi_square_feature_selector::learn(chi_square_dataset(outside_domain), _).

	test(chi_square_feature_selector_bad_number, error(type_error(number, bad))) :-
		chi_square_feature_selector::learn(chi_square_dataset(bad_number), _).

	test(chi_square_feature_selector_bad_target, error(type_error(atomic, label(x)))) :-
		chi_square_feature_selector::learn(chi_square_dataset(bad_target), _).

	test(chi_square_feature_selector_empty_domain, error(domain_error(feature_type, signal-[]))) :-
		chi_square_feature_selector::learn(chi_square_dataset(empty_domain), _).

	test(chi_square_feature_selector_compound_domain, error(type_error(atomic, label(b)))) :-
		chi_square_feature_selector::learn(chi_square_dataset(compound_domain), _).

	test(chi_square_feature_selector_unbound_domain, error(instantiation_error)) :-
		chi_square_feature_selector::learn(chi_square_dataset(unbound_domain), _).

	test(chi_square_feature_selector_binning_category_codes, error(type_error(number, a))) :-
		chi_square_feature_selector::learn(feature_selection_categorical_dataset, _, [feature_discretization(signal, equal_width(2))]).

	test(chi_square_feature_selector_partial_model, deterministic(var(Scores))) :-
		assertion(\+ chi_square_feature_selector::valid_selector(chi_square_feature_selector(Scores, [], []))).

	test(chi_square_feature_selector_unbound_model, error(instantiation_error)) :-
		chi_square_feature_selector::check_selector(_).

	test(chi_square_feature_selector_wrong_selection, fail) :-
		chi_square_feature_selector::learn(feature_demo, chi_square_feature_selector(Scores, _, Diagnostics)),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector(Scores, [], Diagnostics)).

	test(chi_square_feature_selector_duplicate_scores, fail) :-
		chi_square_feature_selector::learn(feature_demo, chi_square_feature_selector([First| Scores], Selected, Diagnostics)),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector([First, First| Scores], Selected, Diagnostics)).

	test(chi_square_feature_selector_unsorted_scores, fail) :-
		chi_square_feature_selector::learn(feature_selection_categorical_dataset, chi_square_feature_selector([First, Second, Third], Selected, Diagnostics)),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector([Third, Second, First], Selected, Diagnostics)).

	test(chi_square_feature_selector_wrong_metric, fail) :-
		chi_square_feature_selector::learn(feature_demo, chi_square_feature_selector(Scores, Selected, Diagnostics)),
		list::select(scoring_metric(chi_square_score), Diagnostics, Rest),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector(Scores, Selected, [scoring_metric(cramers_v_score)| Rest])).

	test(chi_square_feature_selector_invalid_model_options, fail) :-
		chi_square_feature_selector::learn(feature_demo, chi_square_feature_selector(Scores, Selected, Diagnostics)),
		list::select(options(_), Diagnostics, Rest),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector(Scores, Selected, [options([score_variant(bad)])| Rest])).

	test(chi_square_feature_selector_missing_counts, fail) :-
		chi_square_feature_selector::learn(feature_demo, chi_square_feature_selector(Scores, Selected, Diagnostics)),
		list::select(complete_cases(_), Diagnostics, Rest),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector(Scores, Selected, Rest)).

	test(chi_square_feature_selector_excess_counts, fail) :-
		chi_square_feature_selector::learn(feature_demo, chi_square_feature_selector(Scores, Selected, Diagnostics)),
		list::select(complete_cases(_), Diagnostics, Rest),
		chi_square_feature_selector::valid_selector(chi_square_feature_selector(Scores, Selected, [complete_cases([f1-21, f2-20, f3-20])| Rest])).

	test(chi_square_feature_selector_nonperfect_reference, deterministic) :-
		chi_square_feature_selector::learn(chi_square_dataset(weak), Raw),
		chi_square_feature_selector::feature_scores(Raw, Scores),
		memberchk(signal-RawScore, Scores),
		assertion(RawScore =~= 2.0),
		chi_square_feature_selector::learn(chi_square_dataset(weak), Normalized, [score_variant(normalized)]),
		chi_square_feature_selector::feature_scores(Normalized, NormalizedScores),
		memberchk(signal-NormalizedScore, NormalizedScores),
		assertion(NormalizedScore =~= 0.5).

	test(chi_square_feature_selector_multiclass_reference, deterministic) :-
		Expected is sqrt(0.5),
		chi_square_feature_selector::learn(chi_square_dataset(multiclass), Raw),
		chi_square_feature_selector::feature_scores(Raw, Scores),
		memberchk(signal-RawScore, Scores),
		assertion(RawScore =~= 6.0),
		chi_square_feature_selector::learn(chi_square_dataset(multiclass), Normalized, [score_variant(normalized)]),
		chi_square_feature_selector::feature_scores(Normalized, NormalizedScores),
		memberchk(signal-NormalizedScore, NormalizedScores),
		assertion(NormalizedScore =~= Expected).

	test(chi_square_feature_selector_malformed_model_error, error(domain_error(selector, chi_square_feature_selector([], [], [])))) :-
		chi_square_feature_selector::check_selector(chi_square_feature_selector([], [], [])).

	test(chi_square_feature_selector_export, deterministic(Loaded == Selector)) :-
		chi_square_feature_selector::learn(feature_demo, Selector, [score_variant(normalized)]),
		chi_square_feature_selector::export_to_clauses(feature_demo, Selector, chi_square_model, [chi_square_model(Loaded)]),
		chi_square_feature_selector::check_selector(Loaded).

	test(chi_square_feature_selector_print, true) :-
		^^suppress_text_output,
		chi_square_feature_selector::learn(feature_demo, Selector),
		chi_square_feature_selector::print_selector(Selector).

	test(chi_square_feature_selector_file_export, true(Loaded == Selector)) :-
		^^file_path('test_chi_square_selector_output.pl', File),
		chi_square_feature_selector::learn(feature_demo, Selector),
		chi_square_feature_selector::export_to_file(feature_demo, Selector, chi_square_exported_model, File),
		logtalk_load(File),
		{chi_square_exported_model(Loaded)},
		chi_square_feature_selector::check_selector(Loaded).

:- end_object.
