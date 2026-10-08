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
		comment is 'Unit tests for the mutual information feature selector library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(mutual_information_feature_selector).

	cleanup :-
		^^clean_file('test_mutual_information_selector_output.pl').

	test(mutual_information_feature_selector_raw_reference, deterministic) :-
		mutual_information_feature_selector::learn(feature_selection_categorical_dataset, Selector),
		mutual_information_feature_selector::check_selector(Selector),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Signal, noise-Noise, constant-Constant]),
		assertion(Signal =~= 1.0),
		assertion(Noise =~= 0.0),
		assertion(Constant =~= 0.0).

	test(mutual_information_feature_selector_joint_missing, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(missing), Selector, [preparation_mode(joint)]),
		mutual_information_feature_selector::check_selector(Selector),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 1.0),
		assertion(Copy =~= 1.0),
		assertion(Constant =~= 0.0),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(complete_cases([signal-2, copy-2, constant-2]), Diagnostics),
		memberchk(usable_example_count(2), Diagnostics),
		memberchk(excluded_example_count(4), Diagnostics).

	test(mutual_information_feature_selector_joint_empty, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(all_missing), Selector, [preparation_mode(joint), score_variant(normalized)]),
		mutual_information_feature_selector::check_selector(Selector),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 0.0),
		assertion(Copy =~= 0.0),
		assertion(Constant =~= 0.0).

	test(mutual_information_feature_selector_joint_bad_counts, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(missing), mutual_information_feature_selector(Scores, Selected, Diagnostics), [preparation_mode(joint)]),
		once(list::select(usable_example_count(2), Diagnostics, Rest)),
		assertion(\+ mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, Selected, [usable_example_count(3)| Rest]))).

	test(mutual_information_feature_selector_joint_export, deterministic(Loaded == Selector)) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(missing), Selector, [preparation_mode(joint)]),
		mutual_information_feature_selector::export_to_clauses(mutual_information_dataset(missing), Selector, joint_model, [joint_model(Loaded)]),
		mutual_information_feature_selector::check_selector(Loaded).

	test(mutual_information_feature_selector_normalized_reference, deterministic(Score =~= 1.0)) :-
		mutual_information_feature_selector::learn(feature_selection_categorical_dataset, Selector, [score_variant(normalized)]),
		mutual_information_feature_selector::check_selector(Selector),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Score| _]),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(scoring_metric(symmetrical_uncertainty_score), Diagnostics).

	test(mutual_information_feature_selector_distinct_normalization, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(unique), Raw),
		mutual_information_feature_selector::feature_scores(Raw, [signal-RawScore| _]),
		mutual_information_feature_selector::learn(mutual_information_dataset(unique), Normalized, [score_variant(normalized)]),
		mutual_information_feature_selector::feature_scores(Normalized, Scores),
		memberchk(signal-NormalizedScore, Scores),
		Expected is 2 / 3,
		assertion(RawScore =~= 1.0),
		assertion(NormalizedScore =~= Expected).

	test(mutual_information_feature_selector_equal_width, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [discretization(equal_width(2))]),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 1.0),
		assertion(Copy =~= Signal),
		assertion(Constant =~= 0.0),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([signal-equal_width(2), copy-categorical, constant-categorical]), Diagnostics),
		memberchk(occupied_categories([signal-2, copy-2, constant-1]), Diagnostics).

	test(mutual_information_feature_selector_equal_frequency, deterministic(Score =~= 1.0)) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [discretization(equal_frequency(2))]),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Score| _]).

	test(mutual_information_feature_selector_continuous_default, deterministic) :-
		mutual_information_feature_selector::learn(feature_demo, Selector),
		mutual_information_feature_selector::check_selector(Selector),
		assertion(ground(Selector)),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([f1-equal_frequency(10), f2-equal_frequency(10), f3-equal_frequency(10)]), Diagnostics).

	test(mutual_information_feature_selector_ties, deterministic(Selected == [signal])) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [discretization(equal_width(2)), selection_strategy(top_k(1))]),
		mutual_information_feature_selector::selected_features(Selector, Selected).

	test(mutual_information_feature_selector_all, deterministic(Selected == [signal, copy, constant])) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [selection_strategy(all)]),
		mutual_information_feature_selector::selected_features(Selector, Selected).

	test(mutual_information_feature_selector_threshold, deterministic(Selected == [signal, copy])) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [discretization(equal_frequency(2)), selection_strategy(threshold(0.5))]),
		mutual_information_feature_selector::selected_features(Selector, Selected).

	test(mutual_information_feature_selector_empty_selection, deterministic(Selected == [])) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [selection_strategy(threshold(2))]),
		mutual_information_feature_selector::selected_features(Selector, Selected).

	test(mutual_information_feature_selector_repeated_options, deterministic) :-
		Options = [score_variant(normalized), score_variant(raw), discretization(equal_width(2)), discretization(equal_frequency(1)), selection_strategy(top_k(1)), selection_strategy(all), preparation_mode(per_feature)],
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, Options),
		mutual_information_feature_selector::selected_features(Selector, [signal]),
		mutual_information_feature_selector::selector_options(Selector, Options),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(scoring_metric(symmetrical_uncertainty_score), Diagnostics),
		memberchk(discretization([signal-equal_width(2), copy-categorical, constant-categorical]), Diagnostics).

	test(mutual_information_feature_selector_repeated_overrides, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [feature_discretization(signal, equal_width(1)), feature_discretization(signal, equal_frequency(2))]),
		mutual_information_feature_selector::feature_scores(Selector, Scores),
		memberchk(signal-Score, Scores),
		assertion(Score =~= 0.0),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(discretization([signal-equal_width(1), copy-categorical, constant-categorical]), Diagnostics).

	test(mutual_information_feature_selector_categorical_override, deterministic(Score =~= 1.0)) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(mixed), Selector, [feature_discretization(signal, categorical)]),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Score| _]).

	test(mutual_information_feature_selector_missing, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(missing), Selector, [discretization(equal_width(2))]),
		mutual_information_feature_selector::check_selector(Selector),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Score| _]),
		assertion(Score =~= 1.0),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(complete_cases([signal-4, copy-3, constant-5]), Diagnostics),
		memberchk(example_count(6), Diagnostics),
		memberchk(occupied_categories([signal-2, copy-2, constant-1]), Diagnostics),
		memberchk(preparation_mode(per_feature), Diagnostics).

	test(mutual_information_feature_selector_all_missing, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(all_missing), Selector, [score_variant(normalized)]),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 0.0),
		assertion(Copy =~= 0.0),
		assertion(Constant =~= 0.0),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(complete_cases([signal-0, copy-0, constant-0]), Diagnostics),
		memberchk(occupied_categories([signal-0, copy-0, constant-0]), Diagnostics).

	test(mutual_information_feature_selector_single_class, deterministic) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(single_class), Selector),
		mutual_information_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 0.0),
		assertion(Copy =~= 0.0),
		assertion(Constant =~= 0.0).

	test(mutual_information_feature_selector_option_api, true) :-
		mutual_information_feature_selector::default_option(score_variant(raw)),
		mutual_information_feature_selector::default_option(discretization(equal_frequency(10))),
		mutual_information_feature_selector::default_option(selection_strategy(top_k(10))),
		mutual_information_feature_selector::valid_option(selection_strategy(all)),
		mutual_information_feature_selector::valid_option(selection_strategy(threshold(0.5))),
		mutual_information_feature_selector::valid_option(feature_discretization(signal, categorical)),
		mutual_information_feature_selector::current_predicate(valid_option/1),
		mutual_information_feature_selector::current_predicate(default_option/1).

	test(mutual_information_feature_selector_diagnostics, deterministic) :-
		mutual_information_feature_selector::learn(feature_selection_categorical_dataset, Selector),
		functor(Selector, mutual_information_feature_selector, 3),
		assertion(ground(Selector)),
		mutual_information_feature_selector::valid_selector(Selector),
		mutual_information_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(model(mutual_information_feature_selector), Diagnostics),
		memberchk(candidate_count(3), Diagnostics),
		memberchk(selected_count(3), Diagnostics),
		memberchk(scoring_metric(mutual_information_score), Diagnostics),
		memberchk(complete_cases([signal-4, noise-4, constant-4]), Diagnostics),
		findall(Metric, mutual_information_feature_selector::diagnostic(Selector, scoring_metric(Metric)), [mutual_information_score]).

	test(mutual_information_feature_selector_unknown_override, error(domain_error(unknown_feature, unknown))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [feature_discretization(unknown, equal_width(2))]).

	test(mutual_information_feature_selector_invalid_variant, error(domain_error(option, score_variant(bad)))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [score_variant(bad)]).

	test(mutual_information_feature_selector_invalid_bins, error(domain_error(option, discretization(equal_width(0))))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [discretization(equal_width(0))]).

	test(mutual_information_feature_selector_invalid_override, error(domain_error(option, feature_discretization(f1, bad)))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [feature_discretization(f1, bad)]).

	test(mutual_information_feature_selector_unbound_configuration, error(domain_error(option, discretization(equal_frequency(_))))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [discretization(equal_frequency(_))]).

	test(mutual_information_feature_selector_invalid_later_override, error(domain_error(option, feature_discretization(f1, equal_width(0))))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [feature_discretization(f1, equal_width(2)), feature_discretization(f1, equal_width(0))]).

	test(mutual_information_feature_selector_invalid_strategy, error(domain_error(option, selection_strategy(top_k(0))))) :-
		mutual_information_feature_selector::learn(feature_demo, _, [selection_strategy(top_k(0))]).

	test(mutual_information_feature_selector_outside_domain, error(domain_error(feature_value, signal-9))) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(outside_domain), _).

	test(mutual_information_feature_selector_bad_number, error(type_error(number, bad))) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(bad_number), _).

	test(mutual_information_feature_selector_bad_target, error(type_error(atomic, label(x)))) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(bad_target), _).

	test(mutual_information_feature_selector_empty_domain, error(domain_error(feature_type, signal-[]))) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(empty_domain), _).

	test(mutual_information_feature_selector_compound_domain, error(type_error(atomic, label(b)))) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(compound_domain), _).

	test(mutual_information_feature_selector_unbound_domain, error(instantiation_error)) :-
		mutual_information_feature_selector::learn(mutual_information_dataset(unbound_domain), _).

	test(mutual_information_feature_selector_binning_category_codes, error(type_error(number, a))) :-
		mutual_information_feature_selector::learn(feature_selection_categorical_dataset, _, [feature_discretization(signal, equal_width(2))]).

	test(mutual_information_feature_selector_partial_model, deterministic(var(Scores))) :-
		assertion(\+ mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, [], []))).

	test(mutual_information_feature_selector_unbound_model, error(instantiation_error)) :-
		mutual_information_feature_selector::check_selector(_).

	test(mutual_information_feature_selector_wrong_selection, fail) :-
		mutual_information_feature_selector::learn(feature_demo, mutual_information_feature_selector(Scores, _, Diagnostics)),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, [], Diagnostics)).

	test(mutual_information_feature_selector_duplicate_scores, fail) :-
		mutual_information_feature_selector::learn(feature_demo, mutual_information_feature_selector([First| Scores], Selected, Diagnostics)),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector([First, First| Scores], Selected, Diagnostics)).

	test(mutual_information_feature_selector_unsorted_scores, fail) :-
		mutual_information_feature_selector::learn(feature_selection_categorical_dataset, mutual_information_feature_selector([First, Second, Third], Selected, Diagnostics)),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector([Third, Second, First], Selected, Diagnostics)).

	test(mutual_information_feature_selector_wrong_metric, fail) :-
		mutual_information_feature_selector::learn(feature_demo, mutual_information_feature_selector(Scores, Selected, Diagnostics)),
		list::select(scoring_metric(mutual_information_score), Diagnostics, Rest),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, Selected, [scoring_metric(symmetrical_uncertainty_score)| Rest])).

	test(mutual_information_feature_selector_invalid_model_options, fail) :-
		mutual_information_feature_selector::learn(feature_demo, mutual_information_feature_selector(Scores, Selected, Diagnostics)),
		list::select(options(_), Diagnostics, Rest),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, Selected, [options([score_variant(bad)])| Rest])).

	test(mutual_information_feature_selector_missing_counts, fail) :-
		mutual_information_feature_selector::learn(feature_demo, mutual_information_feature_selector(Scores, Selected, Diagnostics)),
		list::select(complete_cases(_), Diagnostics, Rest),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, Selected, Rest)).

	test(mutual_information_feature_selector_excess_counts, fail) :-
		mutual_information_feature_selector::learn(feature_demo, mutual_information_feature_selector(Scores, Selected, Diagnostics)),
		list::select(complete_cases(_), Diagnostics, Rest),
		mutual_information_feature_selector::valid_selector(mutual_information_feature_selector(Scores, Selected, [complete_cases([f1-21, f2-20, f3-20])| Rest])).

	test(mutual_information_feature_selector_nonperfect_reference, deterministic) :-
		Expected is 0.75 * log(1.5) / log(2) + 0.25 * log(0.5) / log(2),
		mutual_information_feature_selector::learn(mutual_information_dataset(weak), Raw),
		mutual_information_feature_selector::feature_scores(Raw, Scores),
		memberchk(signal-RawScore, Scores),
		assertion(RawScore =~= Expected),
		mutual_information_feature_selector::learn(mutual_information_dataset(weak), Normalized, [score_variant(normalized)]),
		mutual_information_feature_selector::feature_scores(Normalized, NormalizedScores),
		memberchk(signal-NormalizedScore, NormalizedScores),
		assertion(NormalizedScore =~= Expected).

	test(mutual_information_feature_selector_multiclass_reference, deterministic) :-
		Entropy is log(3) / log(2),
		Information is Entropy - 2 / 3,
		Expected is Information / Entropy,
		mutual_information_feature_selector::learn(mutual_information_dataset(multiclass), Raw),
		mutual_information_feature_selector::feature_scores(Raw, Scores),
		memberchk(signal-RawScore, Scores),
		assertion(RawScore =~= Information),
		mutual_information_feature_selector::learn(mutual_information_dataset(multiclass), Normalized, [score_variant(normalized)]),
		mutual_information_feature_selector::feature_scores(Normalized, NormalizedScores),
		memberchk(signal-NormalizedScore, NormalizedScores),
		assertion(NormalizedScore =~= Expected).

	test(mutual_information_feature_selector_malformed_model_error, error(domain_error(selector, mutual_information_feature_selector([], [], [])))) :-
		mutual_information_feature_selector::check_selector(mutual_information_feature_selector([], [], [])).

	test(mutual_information_feature_selector_export, deterministic(Loaded == Selector)) :-
		mutual_information_feature_selector::learn(feature_demo, Selector, [score_variant(normalized)]),
		mutual_information_feature_selector::export_to_clauses(feature_demo, Selector, mutual_information_model, [mutual_information_model(Loaded)]),
		mutual_information_feature_selector::check_selector(Loaded).

	test(mutual_information_feature_selector_print, true) :-
		^^suppress_text_output,
		mutual_information_feature_selector::learn(feature_demo, Selector),
		mutual_information_feature_selector::print_selector(Selector).

	test(mutual_information_feature_selector_file_export, true(Loaded == Selector)) :-
		^^file_path('test_mutual_information_selector_output.pl', File),
		mutual_information_feature_selector::learn(feature_demo, Selector),
		mutual_information_feature_selector::export_to_file(feature_demo, Selector, mutual_information_exported_model, File),
		logtalk_load(File),
		{mutual_information_exported_model(Loaded)},
		mutual_information_feature_selector::check_selector(Loaded).

:- end_object.
