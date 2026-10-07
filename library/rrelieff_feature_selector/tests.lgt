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
		comment is 'Unit tests for the RReliefF regression feature selector library.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, =~=), (=~=)/2, assertion/1
	]).

	:- uses(list, [
		memberchk/2
	]).

	cover(rrelieff_feature_selector).
	cover(relief_feature_selector_common).

	cleanup :-
		^^clean_file('test_rrelieff_output.pl').

	test(rrelieff_feature_selector_uniform_reference, deterministic) :-
		regression(Dataset),
		Options = [number_of_neighbors(2), neighbor_weighting(uniform)],
		relief_test_reference::scores(Dataset, regression, Expected, Options),
		rrelieff_feature_selector::learn(Dataset, Selector, Options),
		rrelieff_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(rrelieff_feature_selector_rank_reference, deterministic) :-
		regression(Dataset),
		Options = [number_of_neighbors(10), neighbor_weighting(rank(2))],
		relief_test_reference::scores(Dataset, regression, Expected, Options),
		rrelieff_feature_selector::learn(Dataset, Selector, Options),
		rrelieff_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(rrelieff_feature_selector_constant_target, deterministic(Score =~= 0.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-2, [signal-1]-2, [signal-2]-2]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Score]),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(degeneracy(constant_target), Diagnostics),
		rrelieff_feature_selector::check_selector(Selector).

	test(rrelieff_feature_selector_two_rows, deterministic(Score =~= 0.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-0, [signal-1]-1]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Score]),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(degeneracy(zero_conditioning_mass), Diagnostics).

	test(rrelieff_feature_selector_missing_reference, deterministic) :-
		missing(Dataset),
		Options = [missing_values(probabilistic), number_of_neighbors(10), neighbor_weighting(uniform)],
		relief_test_reference::scores(Dataset, regression, Expected, Options),
		rrelieff_feature_selector::learn(Dataset, Selector, Options),
		rrelieff_feature_selector::feature_scores(Selector, Scores),
		compare_scores(Scores, Expected).

	test(rrelieff_feature_selector_large_k, deterministic) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, First, [number_of_neighbors(10)]),
		rrelieff_feature_selector::learn(Dataset, Second, [number_of_neighbors(100)]),
		rrelieff_feature_selector::feature_scores(First, Scores),
		rrelieff_feature_selector::feature_scores(Second, Expected),
		compare_scores(Scores, Expected).

	test(rrelieff_feature_selector_sampling_restore, deterministic) :-
		regression(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		rrelieff_feature_selector::learn(Dataset, First, [sample_size(20)]),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After),
		rrelieff_feature_selector::learn(Dataset, Second, [sample_size(20)]),
		assertion(First == Second).

	test(rrelieff_feature_selector_all_no_rng, deterministic) :-
		regression(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		rrelieff_feature_selector::learn(Dataset, _),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(rrelieff_feature_selector_repeated_options, deterministic) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [number_of_neighbors(1), number_of_neighbors(20)]),
		rrelieff_feature_selector::learn(Dataset, Reference, [number_of_neighbors(1)]),
		rrelieff_feature_selector::feature_scores(Selector, Scores),
		rrelieff_feature_selector::feature_scores(Reference, Expected),
		compare_scores(Scores, Expected).

	test(rrelieff_feature_selector_invalid_target, error(type_error(number, bad))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-0, [signal-1]-bad]),
		rrelieff_feature_selector::learn(Dataset, _).

	test(rrelieff_feature_selector_export, deterministic(Loaded == Selector)) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::export_to_clauses(Dataset, Selector, saved, [saved(Loaded)]).

	test(rrelieff_feature_selector_partial, deterministic(var(Scores))) :-
		assertion(\+ rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, [], []))).

	test(rrelieff_feature_selector_exact_conditional_reference, deterministic) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [neighbor_weighting(uniform)]),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Signal, noise-Noise]),
		ExpectedSignal is 11 / 35,
		ExpectedNoise is -8 / 35,
		assertion(Signal =~= ExpectedSignal),
		assertion(Noise =~= ExpectedNoise).

	test(rrelieff_feature_selector_zero_mass_nonconstant, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-0, [signal-0]-0, [signal-1]-1, [signal-1]-1]),
		rrelieff_feature_selector::learn(Dataset, Selector, [number_of_neighbors(1)]),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Score]),
		assertion(Score =~= 0.0),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(degeneracy(zero_conditioning_mass), Diagnostics),
		rrelieff_feature_selector::check_selector(Selector).

	test(rrelieff_feature_selector_duplicates_self, deterministic(Score =~= 1.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-0, [signal-0]-0, [signal-1]-1, [signal-1]-1]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Score]).

	test(rrelieff_feature_selector_stable_ties, deterministic(Selected == [z, a])) :-
		ties(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::selected_features(Selector, Selected).

	test(rrelieff_feature_selector_top_k, deterministic(Selected == [z])) :-
		ties(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [selection_strategy(top_k(1))]),
		rrelieff_feature_selector::selected_features(Selector, Selected).

	test(rrelieff_feature_selector_all, deterministic(Selected == [z, a])) :-
		ties(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [selection_strategy(all)]),
		rrelieff_feature_selector::selected_features(Selector, Selected).

	test(rrelieff_feature_selector_signed_threshold, deterministic(Selected == [signal, noise])) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [selection_strategy(threshold(-1)), neighbor_weighting(uniform)]),
		rrelieff_feature_selector::selected_features(Selector, Selected).

	test(rrelieff_feature_selector_empty_threshold, deterministic(Selected == [])) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [selection_strategy(threshold(2))]),
		rrelieff_feature_selector::selected_features(Selector, Selected).

	test(rrelieff_feature_selector_complete_cases_unknown_target, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal-0]-0, [signal-1]-1, [signal-4]-4, [signal-5]-5, []-7, [signal-100]-Unknown]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(eligible_count(4), Diagnostics),
		memberchk(excluded_count(2), Diagnostics),
		memberchk(samples([1, 2, 3, 4]), Diagnostics),
		assertion(var(Unknown)).

	test(rrelieff_feature_selector_probabilistic_nonmutation, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal-0]-0, [signal-Missing]-1, [signal-4]-4, [signal-5]-5]),
		copy_term(Dataset, Before),
		rrelieff_feature_selector::learn(Dataset, _, [missing_values(probabilistic)]),
		assertion(var(Missing)),
		assertion(lgtunit::variant(Dataset, Before)).

	test(rrelieff_feature_selector_all_missing, deterministic) :-
		Dataset = relief_test_dataset([number-continuous, color-[red]], [[]-0, []-1, []-2, []-3]),
		rrelieff_feature_selector::learn(Dataset, Selector, [missing_values(probabilistic)]),
		rrelieff_feature_selector::feature_scores(Selector, [number-Number, color-Color]),
		assertion(Number =~= 0.0),
		assertion(Color =~= 0.0).

	test(rrelieff_feature_selector_constant_column, deterministic(Score =~= 0.0)) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-7]-0, [signal-7]-1, [signal-7]-2, [signal-7]-3]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Score]).

	test(rrelieff_feature_selector_extreme_target_and_feature_range, deterministic) :-
		Dataset = relief_test_dataset([signal-continuous], [
			[signal- -1.0e308]- -1.0e308, [signal- -5.0e307]- -5.0e307,
			[signal-5.0e307]-5.0e307, [signal-1.0e308]-1.0e308]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::check_selector(Selector),
		rrelieff_feature_selector::feature_scores(Selector, [signal-Score]),
		assertion(Score > 0.0).

	test(rrelieff_feature_selector_sample_full_pool, deterministic) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [sample_size(1)]),
		rrelieff_feature_selector::check_selector(Selector),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(samples([Position]), Diagnostics),
		memberchk(eligible_count(4), Diagnostics),
		memberchk(population(regression(target_range(0, 4), _)), Diagnostics),
		assertion(Position >= 1),
		assertion(Position =< 4).

	test(rrelieff_feature_selector_sampling_with_replacement, deterministic) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [sample_size(100)]),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		memberchk(samples(Samples), Diagnostics),
		list::length(Samples, 100),
		sort(Samples, Unique),
		list::length(Unique, Count),
		assertion(Count =< 4).

	test(rrelieff_feature_selector_rng_failure, deterministic) :-
		regression(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		assertion(\+ rrelieff_feature_selector::learn(Dataset, impossible, [sample_size(5)])),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(rrelieff_feature_selector_rng_error, deterministic) :-
		regression(Dataset),
		fast_random(xoshiro128pp)::get_seed(Before),
		catch(rrelieff_feature_selector::learn(Dataset, _, [sample_size(0)]), Error, true),
		assertion(nonvar(Error)),
		fast_random(xoshiro128pp)::get_seed(After),
		assertion(Before == After).

	test(rrelieff_feature_selector_public_defaults, deterministic) :-
		rrelieff_feature_selector::default_option(number_of_neighbors(10)),
		rrelieff_feature_selector::default_option(neighbor_weighting(rank(2))),
		rrelieff_feature_selector::valid_option(neighbor_weighting(uniform)).

	test(rrelieff_feature_selector_frozen_options, deterministic) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [number_of_neighbors(1), number_of_neighbors(20)]),
		rrelieff_feature_selector::selector_options(Selector, Options),
		Options = [number_of_neighbors(1), number_of_neighbors(20)| _],
		rrelieff_feature_selector::check_selector(Selector).

	test(rrelieff_feature_selector_incomplete_option, deterministic(var(Size))) :-
		assertion(\+ rrelieff_feature_selector::valid_option(sample_size(Size))).

	test(rrelieff_feature_selector_bad_neighbor_count, error(domain_error(option, number_of_neighbors(0)))) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, _, [number_of_neighbors(0)]).

	test(rrelieff_feature_selector_bad_rank_type, error(domain_error(option, neighbor_weighting(rank(1.5))))) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, _, [neighbor_weighting(rank(1.5))]).

	test(rrelieff_feature_selector_bad_feature_type, error(domain_error(feature_type, signal-ordinal))) :-
		Dataset = relief_test_dataset([signal-ordinal], [[signal-0]-0, [signal-1]-1]),
		rrelieff_feature_selector::learn(Dataset, _).

	test(rrelieff_feature_selector_bad_numeric_value, error(type_error(number, bad))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-bad]-0, [signal-1]-1]),
		rrelieff_feature_selector::learn(Dataset, _).

	test(rrelieff_feature_selector_single_row, error(domain_error(relief_population, regression-1))) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-0]),
		rrelieff_feature_selector::learn(Dataset, _).

	test(rrelieff_feature_selector_no_eligible_rows, error(domain_error(relief_population, regression-0))) :-
		Dataset = relief_test_dataset([signal-continuous], [[]-0, []-1]),
		rrelieff_feature_selector::learn(Dataset, _).

	test(rrelieff_feature_selector_empty_candidates, deterministic(Scores == [])) :-
		Dataset = relief_test_dataset([], [[]-0, []-1, []-2]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::check_selector(Selector),
		rrelieff_feature_selector::feature_scores(Selector, Scores).

	test(rrelieff_feature_selector_wrong_functor, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, Selected, Diagnostics)),
		rrelieff_feature_selector::valid_selector(relief_feature_selector(Scores, Selected, Diagnostics)).

	test(rrelieff_feature_selector_wrong_selection, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, _, Diagnostics)),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, [], Diagnostics)).

	test(rrelieff_feature_selector_duplicate_scores, fail) :-
		ties(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector([First, _], Selected, Diagnostics)),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector([First, First], Selected, Diagnostics)).

	test(rrelieff_feature_selector_reversed_ties, fail) :-
		ties(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector([First, Second], Selected, Diagnostics)),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector([Second, First], Selected, Diagnostics)).

	test(rrelieff_feature_selector_bad_candidate_count, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, candidate_count(2), candidate_count(99), Bad),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, Selected, Bad)).

	test(rrelieff_feature_selector_bad_mass, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, Selected, Diagnostics)),
		memberchk(population(Population), Diagnostics),
		replace_term(Diagnostics, population(Population), population(regression(target_range(0, 4), conditioning_mass(1, 1))), Bad),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, Selected, Bad)).

	test(rrelieff_feature_selector_bad_range, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, Selected, Diagnostics)),
		memberchk(population(regression(_, Mass)), Diagnostics),
		memberchk(population(Population), Diagnostics),
		replace_term(Diagnostics, population(Population), population(regression(target_range(4, 0), Mass)), Bad),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, Selected, Bad)).

	test(rrelieff_feature_selector_bad_degeneracy, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, degeneracy(none), degeneracy(constant_target), Bad),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, Selected, Bad)).

	test(rrelieff_feature_selector_degenerate_nonzero_score, fail) :-
		Dataset = relief_test_dataset([signal-continuous], [[signal-0]-2, [signal-1]-2]),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(_, Selected, Diagnostics)),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector([signal-1], Selected, Diagnostics)).

	test(rrelieff_feature_selector_bad_samples, fail) :-
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, rrelieff_feature_selector(Scores, Selected, Diagnostics)),
		replace_term(Diagnostics, samples([1, 2, 3, 4]), samples([1]), Bad),
		rrelieff_feature_selector::valid_selector(rrelieff_feature_selector(Scores, Selected, Bad)).

	test(rrelieff_feature_selector_variable_model, error(instantiation_error)) :-
		rrelieff_feature_selector::check_selector(_).

	test(rrelieff_feature_selector_ground_diagnostics, deterministic) :-
		missing(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector, [missing_values(probabilistic), sample_size(12)]),
		rrelieff_feature_selector::check_selector(Selector),
		rrelieff_feature_selector::diagnostics(Selector, Diagnostics),
		assertion(ground(Selector)),
		memberchk(variant(regression), Diagnostics),
		memberchk(eligible_count(6), Diagnostics),
		memberchk(candidate_count(2), Diagnostics).

	test(rrelieff_feature_selector_file_export, true(Loaded == Selector)) :-
		^^file_path('test_rrelieff_output.pl', File),
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::export_to_file(Dataset, Selector, rrelieff_saved_model, File),
		logtalk_load(File),
		{rrelieff_saved_model(Loaded)},
		rrelieff_feature_selector::check_selector(Loaded).

	test(rrelieff_feature_selector_print, true) :-
		^^suppress_text_output,
		regression(Dataset),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::print_selector(Selector).

	test(rrelieff_feature_selector_loader_reflection, true) :-
		rrelieff_feature_selector::predicate_property(learn(_, _, _), defined_in(relief_feature_selector_common)),
		rrelieff_feature_selector::predicate_property(valid_option(_), declared_in(options_protocol)).

	test(rrelieff_feature_selector_default_top_ten_all_scores, deterministic) :-
		Names = [z, a, b, c, d, e, f, g, h, i, j, k],
		findall(Name-continuous, list::member(Name, Names), Declarations),
		findall(Name-0, list::member(Name, Names), Zero),
		findall(Name-1, list::member(Name, Names), One),
		Dataset = relief_test_dataset(Declarations, [Zero-0, Zero-0, One-1, One-1]),
		rrelieff_feature_selector::learn(Dataset, Selector),
		rrelieff_feature_selector::feature_scores(Selector, Scores),
		list::length(Scores, 12),
		rrelieff_feature_selector::selected_features(Selector, Selected),
		assertion(Selected == [z, a, b, c, d, e, f, g, h, i]).

	test(rrelieff_feature_selector_unknown_category, error(domain_error(feature_value, color-green))) :-
		Dataset = relief_test_dataset([color-[red, blue]], [[color-red]-0, [color-green]-1]),
		rrelieff_feature_selector::learn(Dataset, _).

	% auxiliary predicates

	regression(relief_test_dataset([signal-continuous, noise-continuous], [
		[signal-0, noise-1]-0, [signal-1, noise-0]-1, [signal-3, noise-1]-3, [signal-4, noise-0]-4])).

	ties(relief_test_dataset([z-continuous, a-continuous], [
		[z-0, a-0]-0, [z-0, a-0]-0, [z-1, a-1]-1, [z-1, a-1]-1])).

	missing(relief_test_dataset([signal-continuous, color-[red, blue]], [
		[signal-0, color-red]-0, [color-blue]-1, [signal-1]-2,
		[signal-4, color-blue]-4, []-5, [signal-5, color-blue]-6])).

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
