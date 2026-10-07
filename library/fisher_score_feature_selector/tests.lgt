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
		comment is 'Unit tests for the Fisher score feature selector library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(fisher_score_feature_selector).
	cover(filter_feature_selector_common).

	cleanup :-
		^^clean_file('test_fisher_output.pl').

	test(fisher_score_feature_selector_learn, deterministic(Selected == [f1])) :-
		fisher_score_feature_selector::learn(feature_demo, Selector, [selection_strategy(top_k(1))]),
		fisher_score_feature_selector::check_selector(Selector),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_default, deterministic(Selected == [f1, f2, f3])) :-
		fisher_score_feature_selector::learn(feature_demo, Selector),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_threshold, deterministic(Selected == [f1])) :-
		fisher_score_feature_selector::learn(feature_demo, Selector, [selection_strategy(threshold(10))]),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_repeated_options, deterministic(Selected == [f1])) :-
		fisher_score_feature_selector::learn(feature_demo, Selector, [selection_strategy(top_k(1)), selection_strategy(all)]),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_diagnostics, deterministic) :-
		fisher_score_feature_selector::learn(feature_demo, Selector),
		fisher_score_feature_selector::diagnostics(Selector, Diagnostics),
		assertion(ground(Diagnostics)),
		memberchk(candidate_count(3), Diagnostics),
		memberchk(selected_count(3), Diagnostics),
		memberchk(complete_cases([f1-20, f2-20, f3-20]), Diagnostics).

	test(fisher_score_feature_selector_categorical_declaration, error(domain_error(feature_type, signal-[a, b]))) :-
		fisher_score_feature_selector::learn(feature_selection_categorical_dataset, _).

	test(fisher_score_feature_selector_invalid_option, error(domain_error(option, selection_strategy(top_k(0))))) :-
		fisher_score_feature_selector::learn(feature_demo, _, [selection_strategy(top_k(0))]).

	test(fisher_score_feature_selector_partial_model, deterministic(var(Scores))) :-
		Selector = fisher_score_feature_selector(Scores, [], []),
		assertion(\+ fisher_score_feature_selector::valid_selector(Selector)).

	test(fisher_score_feature_selector_wrong_selection, fail) :-
		fisher_score_feature_selector::learn(feature_demo, fisher_score_feature_selector(Scores, _Selected, Diagnostics)),
		fisher_score_feature_selector::valid_selector(fisher_score_feature_selector(Scores, [], Diagnostics)).

	test(fisher_score_feature_selector_export, deterministic(Loaded == Selector)) :-
		fisher_score_feature_selector::learn(feature_demo, Selector),
		fisher_score_feature_selector::export_to_clauses(feature_demo, Selector, fisher_model, [fisher_model(Loaded)]),
		fisher_score_feature_selector::check_selector(Loaded).

	test(fisher_score_feature_selector_stable_ties, deterministic(Selected == [signal])) :-
		fisher_score_feature_selector::learn(fisher_multiclass_dataset, Selector, [selection_strategy(top_k(1))]),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_scores, deterministic) :-
		fisher_score_feature_selector::learn(fisher_multiclass_dataset, Selector),
		fisher_score_feature_selector::feature_scores(Selector, [signal-Signal, copy-Copy, constant-Constant]),
		assertion(Signal =~= 15.0),
		assertion(Copy =~= Signal),
		assertion(Constant =~= 0.0).

	test(fisher_score_feature_selector_missing_counts, deterministic(Counts == [signal-4, copy-2, constant-4])) :-
		fisher_score_feature_selector::learn(fisher_missing_dataset, Selector),
		fisher_score_feature_selector::check_selector(Selector),
		fisher_score_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(complete_cases(Counts), Diagnostics).

	test(fisher_score_feature_selector_invalid_count, error(domain_error(selector, Selector))) :-
		fisher_score_feature_selector::learn(feature_demo, fisher_score_feature_selector(Scores, Selected, [Model, Count, Options, Candidates, _SelectedCount| Extra])),
		Selector = fisher_score_feature_selector(Scores, Selected, [Model, Count, Options, Candidates, selected_count(bad)| Extra]),
		fisher_score_feature_selector::check_selector(Selector).

	test(fisher_score_feature_selector_duplicate_scores, fail) :-
		fisher_score_feature_selector::learn(feature_demo, fisher_score_feature_selector([First| Scores], Selected, Diagnostics)),
		fisher_score_feature_selector::valid_selector(fisher_score_feature_selector([First, First| Scores], Selected, Diagnostics)).

	test(fisher_score_feature_selector_unsorted_scores, fail) :-
		fisher_score_feature_selector::learn(feature_demo, fisher_score_feature_selector([First, Second, Third], Selected, Diagnostics)),
		fisher_score_feature_selector::valid_selector(fisher_score_feature_selector([Third, Second, First], Selected, Diagnostics)).

	test(fisher_score_feature_selector_options, deterministic) :-
		fisher_score_feature_selector::default_option(selection_strategy(top_k(10))),
		fisher_score_feature_selector::valid_option(selection_strategy(threshold(0.5))),
		fisher_score_feature_selector::learn(feature_demo, Selector, [selection_strategy(top_k(1)), selection_strategy(all)]),
		fisher_score_feature_selector::selector_options(Selector, [selection_strategy(top_k(1)), selection_strategy(all)]).

	test(fisher_score_feature_selector_all_strategy, deterministic(Selected == [signal, copy, constant])) :-
		fisher_score_feature_selector::learn(fisher_multiclass_dataset, Selector, [selection_strategy(all)]),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_empty_threshold, deterministic(Selected == [])) :-
		fisher_score_feature_selector::learn(fisher_multiclass_dataset, Selector, [selection_strategy(threshold(100))]),
		fisher_score_feature_selector::selected_features(Selector, Selected).

	test(fisher_score_feature_selector_print, true) :-
		^^suppress_text_output,
		fisher_score_feature_selector::learn(feature_demo, Selector),
		fisher_score_feature_selector::print_selector(Selector).

	test(fisher_score_feature_selector_file_export, true(Loaded == Selector)) :-
		^^file_path('test_fisher_output.pl', File),
		fisher_score_feature_selector::learn(feature_demo, Selector),
		fisher_score_feature_selector::export_to_file(feature_demo, Selector, fisher_exported_model, File),
		logtalk_load(File),
		{fisher_exported_model(Loaded)},
		fisher_score_feature_selector::check_selector(Loaded).

:- end_object.
