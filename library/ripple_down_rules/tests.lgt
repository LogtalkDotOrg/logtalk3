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
		date is 2026-08-06,
		comment is 'Unit tests for the Ripple-Down Rules library.'
	]).

	:- uses([
		single_classification_ripple_down_rules as scrdr,
		multi_classification_ripple_down_rules as mcrdr,
		generalized_ripple_down_rules as grdr
	]).

	cover(single_classification_ripple_down_rules).
	cover(multi_classification_ripple_down_rules).
	cover(generalized_ripple_down_rules).

	test(scrdr_default_options, deterministic(Conclusion == none)) :-
		scrdr::new(Model),
		scrdr::classify(Model, case, Conclusion).

	test(scrdr_custom_options, deterministic(Conclusion == unknown)) :-
		scrdr::new(Model, [default(unknown)]),
		scrdr::classify(Model, case, Conclusion).

	test(scrdr_variable_options, error(instantiation_error)) :-
		scrdr::new(_, _).

	test(scrdr_invalid_options_list, error(type_error(list, default(none)))) :-
		scrdr::new(_, default(none)).

	test(scrdr_unknown_option, error(domain_error(option, maximum_cycles(2)))) :-
		scrdr::new(_, [maximum_cycles(2)]).

	test(scrdr_classic_correction, deterministic(BeforeConclusion-AfterConclusion == tennis-squash)) :-
		tennis::tennis_models(Before, After),
		tennis::squash_storm_case(Case),
		scrdr::classify(Before, Case, BeforeConclusion),
		scrdr::classify(After, Case, AfterConclusion).

	test(scrdr_classic_persistent_model, deterministic(Conclusion == tennis)) :-
		tennis::tennis_models(Before, _),
		tennis::squash_storm_case(Case),
		scrdr::classify(Before, Case, Conclusion).

	test(scrdr_classic_rule_structure, deterministic(Edges == [root,except])) :-
		tennis::tennis_models(_, After),
		scrdr::as_list(After, [rule(1, none, RootEdge, _, _, _), rule(2, 1, ExceptEdge, _, _, _)]),
		Edges = [RootEdge, ExceptEdge].

	test(scrdr_classic_trace, deterministic(Trace == [evaluated(1,true,false),evaluated(2,true,true)])) :-
		tennis::tennis_models(_, After),
		tennis::squash_storm_case(Case),
		scrdr::classify(After, Case, squash, Trace).

	test(scrdr_classic_size, deterministic(Size == 2)) :-
		tennis::tennis_models(_, After),
		scrdr::size(After, Size).

	test(scrdr_cornerstone_protection, error(domain_error(ripple_down_rule_condition_for_cornerstone, _))) :-
		tennis::tennis_models(Before, _),
		tennis::squash_storm_case(Case),
		scrdr::revise(Before, Case, replace, tennis::sunny_cool, tennis::squash, _).

	test(scrdr_variable_correction, error(instantiation_error)) :-
		scrdr::new(Model),
		tennis::tennis_cornerstone(Case),
		scrdr::revise(Model, Case, _, tennis::sunny_cool, tennis::tennis, _).

	test(scrdr_invalid_correction, error(domain_error(ripple_down_rule_correction, add))) :-
		scrdr::new(Model),
		tennis::tennis_cornerstone(Case),
		scrdr::revise(Model, Case, add, tennis::sunny_cool, tennis::tennis, _).

	test(mcrdr_teaching_correction, deterministic(BeforeConclusions-StoppedConclusions-AfterConclusions == [tennis,kite_flying]-[kite_flying]-[kite_flying,squash])) :-
		tennis::mcrdr_models(Before, Stopped, After),
		tennis::squash_storm_case(Case),
		mcrdr::classify(Before, Case, BeforeConclusions),
		mcrdr::classify(Stopped, Case, StoppedConclusions),
		mcrdr::classify(After, Case, AfterConclusions).

	test(mcrdr_default_options, deterministic(Conclusions == [])) :-
		mcrdr::new(Model),
		mcrdr::classify(Model, case, Conclusions).

	test(mcrdr_custom_options, deterministic(Conclusions == [unknown])) :-
		mcrdr::new(Model, [default([unknown])]),
		mcrdr::classify(Model, case, Conclusions).

	test(mcrdr_invalid_default_option, error(domain_error(option, default(unknown)))) :-
		mcrdr::new(_, [default(unknown)]).

	test(mcrdr_teaching_persistent_model, deterministic(Conclusions == [tennis,kite_flying])) :-
		tennis::mcrdr_models(Before, _, _),
		tennis::squash_storm_case(Case),
		mcrdr::classify(Before, Case, Conclusions).

	test(mcrdr_teaching_rule_structure, deterministic(Edges == [top,remove,top,top])) :-
		tennis::mcrdr_models(_, _, After),
		mcrdr::as_list(After, Descriptors),
		descriptor_edges(Descriptors, Edges).

	test(mcrdr_teaching_size, deterministic(Size == 4)) :-
		tennis::mcrdr_models(_, _, After),
		mcrdr::size(After, Size).

	test(mcrdr_variable_correction, error(instantiation_error)) :-
		mcrdr::new(Model),
		tennis::tennis_cornerstone(Case),
		mcrdr::revise(Model, Case, _, tennis::sunny_cool, tennis::tennis, _).

	test(mcrdr_invalid_correction, error(domain_error(ripple_down_rule_correction, replace))) :-
		mcrdr::new(Model),
		tennis::tennis_cornerstone(Case),
		mcrdr::revise(Model, Case, replace, tennis::sunny_cool, tennis::tennis, _).

	test(grdr_fixed_point, deterministic(Conclusions == [equipment-[racket],sport-squash])) :-
		tennis::grdr_model(Model),
		tennis::squash_storm_case(Case),
		grdr::classify(Model, Case, Conclusions).

	test(grdr_default_options, deterministic(Options == [maximum_cycles(16)])) :-
		grdr::default_options(Options).

	test(grdr_custom_options, true) :-
		grdr::new(Model, [maximum_cycles(2)]),
		grdr::valid_model(Model).

	test(grdr_invalid_maximum_cycles_option, error(domain_error(option, maximum_cycles(0)))) :-
		grdr::new(_, [maximum_cycles(0)]).

	test(grdr_keyed_atomic_correction, deterministic(Conclusion == squash)) :-
		grdr::new(EmptyGRDRModel),
		scrdr::new(EmptySCRDRModel),
		grdr::put(EmptyGRDRModel, sport, EmptySCRDRModel, Before),
		tennis::squash_storm_case(Case),
		grdr::revise(Before, sport-Case, replace, tennis::windy_humid, tennis::squash, After),
		grdr::get(After, sport, Sport),
		scrdr::classify(Sport, Case, Conclusion).

	test(grdr_fixed_point_passes, deterministic(PassValues == [1-[]-squash,2-[racket]-squash,3-[racket]-squash])) :-
		tennis::grdr_model(Model),
		tennis::squash_storm_case(Case),
		grdr::classify(Model, Case, _, Trace),
		pass_values(Trace, PassValues).

	test(grdr_key_order, deterministic(Keys == [equipment,sport])) :-
		tennis::grdr_model(Model),
		grdr::as_list(Model, [key(Key1, _, _), key(Key2, _, _)]),
		Keys = [Key1, Key2].

	test(grdr_size, deterministic(Size == 2)) :-
		tennis::grdr_model(Model),
		grdr::size(Model, Size).

	test(grdr_remove_persistent, deterministic(Old-New == 2-1)) :-
		tennis::grdr_model(Model),
		grdr::remove(Model, equipment, NewModel),
		grdr::size(Model, Old),
		grdr::size(NewModel, New).

	test(zoo_representative_classes, deterministic(Classes == [aardvark-1,chicken-2,pitviper-3,bass-4,frog-5,flea-6,clam-7])) :-
		findall(Name-Class, zoo::representative(Name, _, Class), Classes).

	test(zoo_representative_feature_count, true) :-
		zoo::representative(_, Features, _),
		list::length(Features, 16).

	test(zoo_scrdr_classifications, deterministic(Predictions == [aardvark-mammal,chicken-bird,pitviper-reptile,bass-fish,frog-amphibian,flea-insect,clam-invertebrate])) :-
		zoo::scrdr_model(Model),
		findall(Name-Prediction, (zoo::representative(Name, Case, _), scrdr::classify(Model, Case, Prediction)), Predictions).

	test(zoo_scrdr_nested_else_size, deterministic(Size == 7)) :-
		zoo::scrdr_model(Model),
		scrdr::size(Model, Size).

	test(zoo_grdr_composition, deterministic(Conclusions == [habitat-[aquatic],species-fish])) :-
		zoo::grdr_model(Model),
		zoo::representative(bass, Case, 4),
		grdr::classify(Model, Case, Conclusions).

	test(grdr_cycle_non_convergence, error(domain_error(grdr_non_convergence, maximum_cycles(3)))) :-
		grdr_cycle::model(Model),
		grdr::classify(Model, [], _).

	test(robot_containment_relational, deterministic(Conclusion == contained(wheel,chassis))) :-
		robot_containment::model(Model),
		robot_containment::case(wheel, Case),
		scrdr::classify(Model, Case, Conclusion).

	test(robot_containment_default, deterministic(Conclusion == uncontained)) :-
		robot_containment::model(Model),
		robot_containment::case(loose_bolt, Case),
		scrdr::classify(Model, Case, Conclusion).

	test(furniture_recognition_multiple_labels, deterministic(Conclusions == [drawer,cabinet])) :-
		furniture_recognition::model(Model),
		furniture_recognition::scene(Case),
		mcrdr::classify(Model, Case, Conclusions).

	test(mutagenicity_structural_cases, deterministic(Positive-Negative == mutagenic-non_mutagenic)) :-
		mutagenicity::model(Model),
		mutagenicity::molecule(nitrobenzene, PositiveCase),
		mutagenicity::molecule(ethanol, NegativeCase),
		scrdr::classify(Model, PositiveCase, Positive),
		scrdr::classify(Model, NegativeCase, Negative).

	descriptor_edges([], []).
	descriptor_edges([rule(_, _, Edge, _, _, _)| Descriptors], [Edge| Edges]) :-
		descriptor_edges(Descriptors, Edges).

	pass_values([], []).
	pass_values([pass(Cycle, [key(equipment, Equipment, _), key(sport, Sport, _)])| Passes], [Cycle-Equipment-Sport| Values]) :-
		pass_values(Passes, Values).

:- end_object.
