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
		comment is 'Unit tests for the binary Relief feature selector library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(relief_feature_selector).
	cover(relief_feature_selector_common).

	cleanup :-
		^^clean_file('test_relief_output.pl').

	test(relief_feature_selector_numeric_reference, deterministic(Score =~= 0.5)) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relief_feature_selector_independent_reference, deterministic) :-
		binary(Dataset),
		relief_test_reference::scores(Dataset, binary, Expected, []),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(relief_feature_selector_negative, deterministic(Score =~= -1.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-a, [signal-0]-b, [signal-1]-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relief_feature_selector_duplicates, deterministic(Score =~= 1.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-0]-a, [signal-1]-b, [signal-1]-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relief_feature_selector_missing_reference, deterministic) :-
		missing(Dataset),
		Options = [missing_values(probabilistic)],
		relief_test_reference::scores(Dataset, binary, Expected, Options),
		relief_feature_selector::learn(Dataset, Selector, Options),
		relief_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(relief_feature_selector_sampling_restore, deterministic) :-
		binary(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		relief_feature_selector::learn(Dataset, First, [sample_size(20)]),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After),
		relief_feature_selector::learn(Dataset, Second, [sample_size(20)]),
		assertion(First == Second).

	test(relief_feature_selector_all_no_rng, deterministic) :-
		binary(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		relief_feature_selector::learn(Dataset, _),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relief_feature_selector_repeated_options, deterministic(Selected == [])) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, Selector, [selection_strategy(threshold(2)), selection_strategy(all)]),
		relief_feature_selector::selected_features(Selector, Selected),
		relief_feature_selector::check_selector(Selector).

	test(relief_feature_selector_invalid_population, error(domain_error(relief_population, _))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-b]),
		relief_feature_selector::learn(Dataset, _).

	test(relief_feature_selector_fixed_neighbor, error(domain_error(option, number_of_neighbors(2)))) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, _, [number_of_neighbors(2)]).

	test(relief_feature_selector_export, deterministic(Loaded == Selector)) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::export_to_clauses(Dataset, Selector, saved, [saved(Loaded)]).

	test(relief_feature_selector_partial, deterministic(var(Scores))) :-
		assertion(\+ relief_feature_selector::valid_selector(relief_feature_selector(Scores, [], []))).

	test(relief_feature_selector_xor_interaction, deterministic) :-
		Dataset = relief_test_dataset([left-[0, 1], right-[0, 1]], [
			[left-0, right-0]-a, [left-1, right-1]-a, [left-0, right-1]-b, [left-1, right-0]-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, Scores),
		memberchk(left-Left, Scores),
		memberchk(right-Right, Scores),
		assertion(Left =~= -0.5),
		assertion(Right =~= -0.5).

	test(relief_feature_selector_stable_ties, deterministic(Selected == [z, a])) :-
		ties(Dataset),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::selected_features(Selector, Selected).

	test(relief_feature_selector_top_k, deterministic(Selected == [z])) :-
		ties(Dataset),
		relief_feature_selector::learn(Dataset, Selector, [selection_strategy(top_k(1))]),
		relief_feature_selector::selected_features(Selector, Selected).

	test(relief_feature_selector_all, deterministic(Selected == [z, a])) :-
		ties(Dataset),
		relief_feature_selector::learn(Dataset, Selector, [selection_strategy(all)]),
		relief_feature_selector::selected_features(Selector, Selected).

	test(relief_feature_selector_signed_threshold, deterministic(Selected == [signal])) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-a, [signal-0]-b, [signal-1]-b]),
		relief_feature_selector::learn(Dataset, Selector, [selection_strategy(threshold(-1))]),
		relief_feature_selector::selected_features(Selector, Selected).

	test(relief_feature_selector_complete_cases_unknown_target, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal-0]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b, []-a, [signal-100]-Unknown]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(eligible_count(4), Diagnostics),
		memberchk(excluded_count(2), Diagnostics),
		memberchk(samples([1, 2, 3, 4]), Diagnostics),
		assertion(var(Unknown)).

	test(relief_feature_selector_probabilistic_nonmutation, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal-0]-a, [signal-Missing]-a, [signal-4]-b, [signal-5]-b]),
		copy_term(Dataset, Before),
		relief_feature_selector::learn(Dataset, _, [missing_values(probabilistic)]),
		assertion(var(Missing)),
		assertion(lgtunit::variant(Dataset, Before)).

	test(relief_feature_selector_all_missing, deterministic) :-
		Dataset = relief_test_dataset([number-continuous, color-[red]], [[]-a, []-a, []-b, []-b]),
		relief_feature_selector::learn(Dataset, Selector, [missing_values(probabilistic)]),
		relief_feature_selector::feature_scores(Selector, [number-Number, color-Color]),
		assertion(Number =~= 0.0),
		assertion(Color =~= 0.0).

	test(relief_feature_selector_constant, deterministic(Score =~= 0.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-7]-a, [signal-7]-a, [signal-7]-b, [signal-7]-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relief_feature_selector_extreme_range, deterministic(Score =~= 0.5)) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal- -1.0e308]-a, [signal- -6.0e307]-a, [signal-6.0e307]-b, [signal-1.0e308]-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relief_feature_selector_categorical_identity, deterministic(Score =~= 1.0)) :-
		Dataset = relief_test_dataset([signal-[1, 1.0]], [[signal-1]-a, [signal-1]-a, [signal-1.0]-b, [signal-1.0]-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relief_feature_selector_fallback, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous, color-[red, blue]], [
			[signal-0, color-red]-a, [signal-1, color-blue]-a, []-b, []-b]),
		Options = [missing_values(probabilistic)],
		relief_test_reference::scores(Dataset, binary, Expected, Options),
		relief_feature_selector::learn(Dataset, Selector, Options),
		relief_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(relief_feature_selector_rank_reduction, deterministic) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, First, [neighbor_weighting(rank(2))]),
		relief_feature_selector::learn(Dataset, Second),
		relief_feature_selector::feature_scores(First, Scores),
		relief_feature_selector::feature_scores(Second, Expected),
		compare_scores(Scores, Expected).

	test(relief_feature_selector_samples_full_pool, deterministic) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, Selector, [sample_size(1)]),
		relief_feature_selector::check_selector(Selector),
		relief_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(samples([Position]), Diagnostics),
		memberchk(eligible_count(4), Diagnostics),
		memberchk(population(classes([a-2, b-2])), Diagnostics),
		assertion(Position >= 1),
		assertion(Position =< 4).

	test(relief_feature_selector_different_seed, deterministic(First \== Second)) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, First, [sample_size(30), random_seed(12345)]),
		relief_feature_selector::learn(Dataset, Second, [sample_size(30), random_seed(67890)]).

	test(relief_feature_selector_rng_failure, deterministic) :-
		binary(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		assertion(\+ relief_feature_selector::learn(Dataset, impossible, [sample_size(5)])),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relief_feature_selector_rng_error, deterministic) :-
		binary(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		catch(relief_feature_selector::learn(Dataset, _, [sample_size(0)]), Error, true),
		assertion(nonvar(Error)),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relief_feature_selector_defaults_public, deterministic) :-
		relief_feature_selector::default_option(selection_strategy(top_k(10))),
		relief_feature_selector::default_option(sample_size(all)),
		relief_feature_selector::default_option(random_seed(1357911)),
		relief_feature_selector::default_option(missing_values(complete_case)),
		relief_feature_selector::valid_option(selection_strategy(threshold(-2))).

	test(relief_feature_selector_incomplete_option, deterministic(var(Size))) :-
		assertion(\+ relief_feature_selector::valid_option(sample_size(Size))).

	test(relief_feature_selector_bad_seed, error(domain_error(option, random_seed(0)))) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, _, [random_seed(0)]).

	test(relief_feature_selector_bad_missing, error(domain_error(option, missing_values(other)))) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, _, [missing_values(other)]).

	test(relief_feature_selector_bad_option_type, error(type_error(compound, invalid))) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, _, [invalid]).

	test(relief_feature_selector_variable_options, error(instantiation_error)) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, _, _).

	test(relief_feature_selector_bad_feature_type, error(domain_error(feature_type, signal-ordinal))) :-
		Dataset = relief_test_dataset([signal-ordinal], [[signal-0]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b]),
		relief_feature_selector::learn(Dataset, _).

	test(relief_feature_selector_bad_numeric_value, error(type_error(number, bad))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-bad]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b]),
		relief_feature_selector::learn(Dataset, _).

	test(relief_feature_selector_bad_target_type, error(type_error(atomic, class(a)))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-class(a), [signal-1]-a, [signal-4]-b, [signal-5]-b]),
		relief_feature_selector::learn(Dataset, _).

	test(relief_feature_selector_three_classes, error(domain_error(relief_population, _))) :-
		Dataset = relief_test_dataset([], [[]-a, []-a, []-b, []-b, []-c, []-c]),
		relief_feature_selector::learn(Dataset, _).

	test(relief_feature_selector_empty_candidates, deterministic(Scores == [])) :-
		Dataset = relief_test_dataset([], [[]-a, []-a, []-b, []-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::check_selector(Selector),
		relief_feature_selector::feature_scores(Selector, Scores).

	test(relief_feature_selector_wrong_functor, fail) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector(Scores, Selected, Diagnostics)),
		relief_feature_selector::valid_selector(relieff_feature_selector(Scores, Selected, Diagnostics)).

	test(relief_feature_selector_wrong_selection, fail) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector(Scores, _, Diagnostics)),
		relief_feature_selector::valid_selector(relief_feature_selector(Scores, [], Diagnostics)).

	test(relief_feature_selector_duplicate_scores, fail) :-
		ties(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector([First, _], Selected, Diagnostics)),
		relief_feature_selector::valid_selector(relief_feature_selector([First, First], Selected, Diagnostics)).

	test(relief_feature_selector_reversed_ties, fail) :-
		ties(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector([First, Second], Selected, Diagnostics)),
		relief_feature_selector::valid_selector(relief_feature_selector([Second, First], Selected, Diagnostics)).

	test(relief_feature_selector_bad_eligible_count, fail) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, eligible_count(4), eligible_count(9), Bad),
		relief_feature_selector::valid_selector(relief_feature_selector(Scores, Selected, Bad)).

	test(relief_feature_selector_bad_samples, fail) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, samples([1, 2, 3, 4]), samples([1, 2, 3, 99]), Bad),
		relief_feature_selector::valid_selector(relief_feature_selector(Scores, Selected, Bad)).

	test(relief_feature_selector_bad_priors, fail) :-
		binary(Dataset),
		relief_feature_selector::learn(Dataset, relief_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, population(classes([a-2, b-2])), population(classes([a-1, b-3])), Bad),
		relief_feature_selector::valid_selector(relief_feature_selector(Scores, Selected, Bad)).

	test(relief_feature_selector_variable_model, error(instantiation_error)) :-
		relief_feature_selector::check_selector(_).

	test(relief_feature_selector_ground_diagnostics, deterministic) :-
		missing(Dataset),
		relief_feature_selector::learn(Dataset, Selector, [missing_values(probabilistic), sample_size(12)]),
		relief_feature_selector::check_selector(Selector),
		relief_feature_selector::diagnostics(Selector, Diagnostics),
		assertion(ground(Selector)),
		memberchk(variant(binary), Diagnostics),
		memberchk(eligible_count(6), Diagnostics),
		memberchk(candidate_count(2), Diagnostics).

	test(relief_feature_selector_file_export, true(Loaded == Selector)) :-
		^^file_path('test_relief_output.pl', File),
		binary(Dataset),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::export_to_file(Dataset, Selector, relief_saved_model, File),
		logtalk_load(File),
		{relief_saved_model(Loaded)},
		relief_feature_selector::check_selector(Loaded).

	test(relief_feature_selector_print, true) :-
		^^suppress_text_output,
		binary(Dataset),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::print_selector(Selector).

	test(relief_feature_selector_loader_reflection, true) :-
		relief_feature_selector::predicate_property(learn(_, _, _), defined_in(relief_feature_selector_common)),
		relief_feature_selector::predicate_property(export_to_file(_, _, _, _), declared_in(feature_selector_protocol)).

	test(relief_feature_selector_default_top_ten_all_scores, deterministic) :-
		Names = [z, a, b, c, d, e, f, g, h, i, j, k],
		findall(Name-continuous, list::member(Name, Names), Declarations),
		findall(Name-0, list::member(Name, Names), Zero),
		findall(Name-1, list::member(Name, Names), One),
		Dataset = relief_test_dataset(Declarations, [Zero-a, Zero-a, One-b, One-b]),
		relief_feature_selector::learn(Dataset, Selector),
		relief_feature_selector::feature_scores(Selector, Scores),
		list::length(Scores, 12),
		relief_feature_selector::selected_features(Selector, Selected),
		assertion(Selected == [z, a, b, c, d, e, f, g, h, i]).

	test(relief_feature_selector_sampling_internal_failure_restore, deterministic) :-
		fast_random(xoshiro128pp)::get_seed(Before),
		assertion(\+ relief_sampling_probe::sample(1, 1357911, [], _)),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relief_feature_selector_sampling_internal_exception_restore, deterministic) :-
		fast_random(xoshiro128pp)::get_seed(Before),
		catch(relief_sampling_probe::sample(invalid, 1357911, [row(1, a, [])], _), Error, true),
		assertion(nonvar(Error)),
		assertion(Error = error(type_error(evaluable, _), _)),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relief_feature_selector_unknown_category, error(domain_error(feature_value, color-green))) :-
		Dataset = relief_test_dataset([color-[red, blue]], [[color-red]-a, [color-red]-a, [color-blue]-b, [color-green]-b]),
		relief_feature_selector::learn(Dataset, _).

	test(relief_feature_selector_empty_domain, error(domain_error(feature_type, color-[]))) :-
		Dataset = relief_test_dataset([color-[]], [[color-red]-a, [color-red]-a, [color-blue]-b, [color-blue]-b]),
		relief_feature_selector::learn(Dataset, _).

	% auxiliary predicates

	binary(relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b])).

	ties(relief_test_dataset([z-continuous, a-continuous], [
		[z-0, a-0]-a, [z-0, a-0]-a, [z-1, a-1]-b, [z-1, a-1]-b])).

	missing(relief_test_dataset([signal-continuous, color-[red, blue]], [
		[signal-0, color-red]-a, [color-blue]-a, [signal-1]-a,
		[signal-4, color-blue]-b, []-b, [signal-5, color-blue]-b])).

	compare_scores([], _Expected).
	compare_scores([Feature-Score| Scores], Expected) :-
		memberchk(Feature-Reference, Expected),
		assertion(Score =~= Reference),
		compare_scores(Scores, Expected).

	replace_term([Head| Tail], Old, New, [New| Tail]) :-
		Head == Old,
		!.
	replace_term([Head| Tail], Old, New, [Head| Rest]) :-
		replace_term(Tail, Old, New, Rest).

:- end_object.
