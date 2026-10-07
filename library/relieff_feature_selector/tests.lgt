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


:- object(tests, extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-07,
		comment is 'Unit tests for the multiclass ReliefF feature selector library.'
	]).
	:- uses(lgtunit, [op(700, xfx, =~=), (=~=)/2, assertion/1]).
	:- uses(list, [memberchk/2]).

	cover(relieff_feature_selector).
	cover(relief_feature_selector_common).

	cleanup :-
		^^clean_file('test_relieff_output.pl').

	test(relieff_feature_selector_imbalanced_reference, deterministic) :-
		multiclass(Dataset),
		Options = [number_of_neighbors(2)],
		relief_test_reference::scores(Dataset, multiclass, Expected, Options),
		relieff_feature_selector::learn(Dataset, Selector, Options),
		relieff_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(relieff_feature_selector_rank_reference, deterministic) :-
		multiclass(Dataset),
		Options = [number_of_neighbors(10), neighbor_weighting(rank(2))],
		relief_test_reference::scores(Dataset, multiclass, Expected, Options),
		relieff_feature_selector::learn(Dataset, Selector, Options),
		relieff_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(relieff_feature_selector_binary_reduction, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b]),
		relieff_feature_selector::learn(Dataset, First, [number_of_neighbors(1)]),
		relief_feature_selector::learn(Dataset, Second),
		relieff_feature_selector::feature_scores(First, FirstScores),
		relief_feature_selector::feature_scores(Second, SecondScores),
		compare_scores(FirstScores, SecondScores).

	test(relieff_feature_selector_missing_reference, deterministic) :-
		missing(Dataset),
		Options = [missing_values(probabilistic), number_of_neighbors(10)],
		relief_test_reference::scores(Dataset, multiclass, Expected, Options),
		relieff_feature_selector::learn(Dataset, Selector, Options),
		relieff_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(relieff_feature_selector_large_k, deterministic) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, First, [number_of_neighbors(10)]),
		relieff_feature_selector::learn(Dataset, Second, [number_of_neighbors(100)]),
		relieff_feature_selector::feature_scores(First, Scores),
		relieff_feature_selector::feature_scores(Second, Expected),
		compare_scores(Scores, Expected).

	test(relieff_feature_selector_sampling_restore, deterministic) :-
		multiclass(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		relieff_feature_selector::learn(Dataset, First, [sample_size(20)]),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After),
		relieff_feature_selector::learn(Dataset, Second, [sample_size(20)]),
		assertion(First == Second).

	test(relieff_feature_selector_all_no_rng, deterministic) :-
		multiclass(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		relieff_feature_selector::learn(Dataset, _),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relieff_feature_selector_repeated_options, deterministic) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [number_of_neighbors(1), number_of_neighbors(20)]),
		relieff_feature_selector::learn(Dataset, Reference, [number_of_neighbors(1)]),
		relieff_feature_selector::feature_scores(Selector, Scores),
		relieff_feature_selector::feature_scores(Reference, Expected),
		compare_scores(Scores, Expected).

	test(relieff_feature_selector_invalid_population, error(domain_error(relief_population, _))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-a, [signal-2]-b]),
		relieff_feature_selector::learn(Dataset, _).

	test(relieff_feature_selector_invalid_rank, error(domain_error(option, neighbor_weighting(rank(0))))) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, _, [neighbor_weighting(rank(0))]).

	test(relieff_feature_selector_export, deterministic(Loaded == Selector)) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::export_to_clauses(Dataset, Selector, saved, [saved(Loaded)]).

	test(relieff_feature_selector_partial, deterministic(var(Scores))) :-
		assertion(\+ relieff_feature_selector::valid_selector(relieff_feature_selector(Scores, [], []))).

	test(relieff_feature_selector_imbalanced_prior_number, deterministic(Score =~= Expected)) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [number_of_neighbors(2)]),
		relieff_feature_selector::feature_scores(Selector, [signal-Score]),
		Expected is 2 / 5.

	test(relieff_feature_selector_negative, deterministic(Score =~= -0.5)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-1]-a, [signal-0]-b, [signal-1]-b]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relieff_feature_selector_duplicates_self, deterministic(Score =~= 1.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-a, [signal-0]-a, [signal-1]-b, [signal-1]-b]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relieff_feature_selector_xor_interaction, deterministic) :-
		Dataset = relief_test_dataset([left-[0, 1], right-[0, 1]], [
			[left-0, right-0]-a, [left-1, right-1]-a, [left-0, right-1]-b, [left-1, right-0]-b]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::feature_scores(Selector, [left-Left, right-Right]),
		assertion(Left =~= -0.5),
		assertion(Right =~= -0.5).

	test(relieff_feature_selector_stable_ties, deterministic(Selected == [z, a])) :-
		ties(Dataset),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::selected_features(Selector, Selected).

	test(relieff_feature_selector_top_k, deterministic(Selected == [z])) :-
		ties(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [selection_strategy(top_k(1))]),
		relieff_feature_selector::selected_features(Selector, Selected).

	test(relieff_feature_selector_all, deterministic(Selected == [z, a])) :-
		ties(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [selection_strategy(all)]),
		relieff_feature_selector::selected_features(Selector, Selected).

	test(relieff_feature_selector_threshold, deterministic(Selected == [])) :-
		ties(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [selection_strategy(threshold(2))]),
		relieff_feature_selector::selected_features(Selector, Selected).

	test(relieff_feature_selector_complete_cases_unknown_target, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal-0]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b, []-a, [signal-100]-Unknown]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(eligible_count(4), Diagnostics),
		memberchk(excluded_count(2), Diagnostics),
		memberchk(samples([1, 2, 3, 4]), Diagnostics),
		assertion(var(Unknown)).

	test(relieff_feature_selector_probabilistic_nonmutation, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal-0]-a, [signal-Missing]-a, [signal-4]-b, [signal-5]-b]),
		copy_term(Dataset, Before),
		relieff_feature_selector::learn(Dataset, _, [missing_values(probabilistic)]),
		assertion(var(Missing)),
		assertion(lgtunit::variant(Dataset, Before)).

	test(relieff_feature_selector_all_missing, deterministic) :-
		Dataset = relief_test_dataset([number-continuous, color-[red]], [[]-a, []-a, []-b, []-b, []-c, []-c]),
		relieff_feature_selector::learn(Dataset, Selector, [missing_values(probabilistic)]),
		relieff_feature_selector::feature_scores(Selector, [number-Number, color-Color]),
		assertion(Number =~= 0.0),
		assertion(Color =~= 0.0).

	test(relieff_feature_selector_constant, deterministic(Score =~= 0.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-7]-a, [signal-7]-a, [signal-7]-b, [signal-7]-b]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::feature_scores(Selector, [signal-Score]).

	test(relieff_feature_selector_sample_full_pool_priors, deterministic) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [sample_size(1)]),
		relieff_feature_selector::check_selector(Selector),
		relieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(samples([Position]), Diagnostics),
		memberchk(eligible_count(7), Diagnostics),
		memberchk(population(classes([a-2, b-3, c-2])), Diagnostics),
		assertion(Position >= 1),
		assertion(Position =< 7).

	test(relieff_feature_selector_sampling_with_replacement, deterministic) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [sample_size(100)]),
		relieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(samples(Samples), Diagnostics),
		list::length(Samples, 100),
		sort(Samples, Unique),
		list::length(Unique, Count),
		assertion(Count =< 7).

	test(relieff_feature_selector_rng_failure, deterministic) :-
		multiclass(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		assertion(\+ relieff_feature_selector::learn(Dataset, impossible, [sample_size(5)])),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relieff_feature_selector_rng_error, deterministic) :-
		multiclass(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		catch(relieff_feature_selector::learn(Dataset, _, [sample_size(0)]), Error, true),
		assertion(nonvar(Error)),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(relieff_feature_selector_public_defaults, deterministic) :-
		relieff_feature_selector::default_option(number_of_neighbors(10)),
		relieff_feature_selector::default_option(neighbor_weighting(uniform)),
		relieff_feature_selector::valid_option(neighbor_weighting(rank(3))).

	test(relieff_feature_selector_frozen_options, deterministic) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [number_of_neighbors(1), number_of_neighbors(20)]),
		relieff_feature_selector::selector_options(Selector, Options),
		Options = [number_of_neighbors(1), number_of_neighbors(20)| _],
		relieff_feature_selector::check_selector(Selector).

	test(relieff_feature_selector_incomplete_option, deterministic(var(Size))) :-
		assertion(\+ relieff_feature_selector::valid_option(sample_size(Size))).

	test(relieff_feature_selector_bad_neighbor_type, error(domain_error(option, number_of_neighbors(2.5)))) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, _, [number_of_neighbors(2.5)]).

	test(relieff_feature_selector_unknown_option, error(domain_error(option, distance_metric(euclidean)))) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, _, [distance_metric(euclidean)]).

	test(relieff_feature_selector_bad_feature_type, error(domain_error(feature_type, signal-ordinal))) :-
		Dataset = relief_test_dataset([signal-ordinal], [[signal-0]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b]),
		relieff_feature_selector::learn(Dataset, _).

	test(relieff_feature_selector_bad_numeric_value, error(type_error(number, bad))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-bad]-a, [signal-1]-a, [signal-4]-b, [signal-5]-b]),
		relieff_feature_selector::learn(Dataset, _).

	test(relieff_feature_selector_one_class, error(domain_error(relief_population, _))) :-
		Dataset = relief_test_dataset([], [[]-a, []-a]),
		relieff_feature_selector::learn(Dataset, _).

	test(relieff_feature_selector_no_eligible_rows, error(domain_error(relief_population, _))) :-
		Dataset = relief_test_dataset([signal-continuous], [[]-a, []-a, []-b, []-b]),
		relieff_feature_selector::learn(Dataset, _).

	test(relieff_feature_selector_empty_candidates, deterministic(Scores == [])) :-
		Dataset = relief_test_dataset([], [[]-a, []-a, []-b, []-b]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::check_selector(Selector),
		relieff_feature_selector::feature_scores(Selector, Scores).

	test(relieff_feature_selector_wrong_functor, fail) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector(Scores, Selected, Diagnostics)),
		relieff_feature_selector::valid_selector(relief_feature_selector(Scores, Selected, Diagnostics)).

	test(relieff_feature_selector_wrong_selection, fail) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector(Scores, _, Diagnostics)),
		relieff_feature_selector::valid_selector(relieff_feature_selector(Scores, [], Diagnostics)).

	test(relieff_feature_selector_duplicate_scores, fail) :-
		ties(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector([First, _], Selected, Diagnostics)),
		relieff_feature_selector::valid_selector(relieff_feature_selector([First, First], Selected, Diagnostics)).

	test(relieff_feature_selector_reversed_ties, fail) :-
		ties(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector([First, Second], Selected, Diagnostics)),
		relieff_feature_selector::valid_selector(relieff_feature_selector([Second, First], Selected, Diagnostics)).

	test(relieff_feature_selector_bad_selected_count, fail) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, selected_count(1), selected_count(99), Bad),
		relieff_feature_selector::valid_selector(relieff_feature_selector(Scores, Selected, Bad)).

	test(relieff_feature_selector_bad_samples, fail) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, samples([1, 2, 3, 4, 5, 6, 7]), samples([1]), Bad),
		relieff_feature_selector::valid_selector(relieff_feature_selector(Scores, Selected, Bad)).

	test(relieff_feature_selector_bad_priors, fail) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, population(classes([a-2, b-3, c-2])), population(classes([a-2, b-3, c-3])), Bad),
		relieff_feature_selector::valid_selector(relieff_feature_selector(Scores, Selected, Bad)).

	test(relieff_feature_selector_bad_variant, fail) :-
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, relieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, variant(multiclass), variant(binary), Bad),
		relieff_feature_selector::valid_selector(relieff_feature_selector(Scores, Selected, Bad)).

	test(relieff_feature_selector_variable_model, error(instantiation_error)) :-
		relieff_feature_selector::check_selector(_).

	test(relieff_feature_selector_ground_diagnostics, deterministic) :-
		missing(Dataset),
		relieff_feature_selector::learn(Dataset, Selector, [missing_values(probabilistic), sample_size(12)]),
		relieff_feature_selector::check_selector(Selector),
		relieff_feature_selector::diagnostics(Selector, Diagnostics),
		assertion(ground(Selector)),
		memberchk(variant(multiclass), Diagnostics),
		memberchk(eligible_count(8), Diagnostics),
		memberchk(candidate_count(2), Diagnostics).

	test(relieff_feature_selector_file_export, true(Loaded == Selector)) :-
		^^file_path('test_relieff_output.pl', File),
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::export_to_file(Dataset, Selector, relieff_saved_model, File),
		logtalk_load(File),
		{relieff_saved_model(Loaded)},
		relieff_feature_selector::check_selector(Loaded).

	test(relieff_feature_selector_print, true) :-
		^^suppress_text_output,
		multiclass(Dataset),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::print_selector(Selector).

	test(relieff_feature_selector_loader_reflection, true) :-
		relieff_feature_selector::predicate_property(learn(_, _, _), defined_in(relief_feature_selector_common)),
		relieff_feature_selector::predicate_property(valid_option(_), declared_in(options_protocol)).

	test(relieff_feature_selector_default_top_ten_all_scores, deterministic) :-
		Names = [z, a, b, c, d, e, f, g, h, i, j, k],
		findall(Name-continuous, list::member(Name, Names), Declarations),
		findall(Name-0, list::member(Name, Names), Zero),
		findall(Name-1, list::member(Name, Names), One),
		Dataset = relief_test_dataset(Declarations, [Zero-a, Zero-a, One-b, One-b]),
		relieff_feature_selector::learn(Dataset, Selector),
		relieff_feature_selector::feature_scores(Selector, Scores),
		list::length(Scores, 12),
		relieff_feature_selector::selected_features(Selector, Selected),
		assertion(Selected == [z, a, b, c, d, e, f, g, h, i]).

	test(relieff_feature_selector_unknown_category, error(domain_error(feature_value, color-green))) :-
		Dataset = relief_test_dataset([color-[red, blue]], [[color-red]-a, [color-red]-a, [color-blue]-b, [color-green]-b]),
		relieff_feature_selector::learn(Dataset, _, [missing_values(probabilistic)]).

	multiclass(relief_test_dataset([signal-continuous], [
		[signal-0]-a, [signal-1]-a, [signal-3]-b, [signal-4]-b, [signal-5]-b, [signal-8]-c, [signal-9]-c])).

	ties(relief_test_dataset([z-continuous, a-continuous], [
		[z-0, a-0]-a, [z-0, a-0]-a, [z-1, a-1]-b, [z-1, a-1]-b])).

	missing(relief_test_dataset([signal-continuous, color-[red, blue]], [
		[signal-0, color-red]-a, [color-blue]-a, [signal-1]-a,
		[signal-4, color-blue]-b, []-b, [signal-5, color-blue]-b,
		[color-red]-c, []-c])).

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
