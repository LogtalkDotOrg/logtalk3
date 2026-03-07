%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-06,
		comment is 'Unit tests for the "mutation_testing" tool.'
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	cover(mutation_testing).
	cover(mutation_testing_messages).
	cover(mutator_common).
	% default mutators
	cover(fail_insertion(_, _, _, _)).
	cover(predicate_negation(_, _, _, _)).
	cover(relational_operator_replacement(_, _, _, _)).
	cover(arithmetic_operator_replacement(_, _, _, _)).
	cover(truth_literal_flip(_, _, _, _)).
	cover(head_arguments_mutation(_, _, _, _)).
	cover(head_arguments_reordering(_, _, _, _)).
	cover(clause_order_reordering(_, _, _, _)).

	% predicate_mutants/3 tests

	test(mt_predicate_mutants_3_01, deterministic) :-
		mutation_testing::predicate_mutants(mt_sample, check/1, Mutants),
		^^assertion(Mutants \== []),
		forall(member(mutant(mt_sample, check/1, _, _), Mutants), true).

	% predicate_mutants/4 tests

	test(mt_predicate_mutants_4_01, deterministic) :-
		mutation_testing::predicate_mutants(mt_sample, check/1, Mutants, [mutators([fail_insertion])]),
		Mutants = [mutant(mt_sample, check/1, fail_insertion, _)| _].

	% entity_mutants/2 tests

	test(mt_mutants_2_01, deterministic(Mutants \== [])) :-
		mutation_testing::entity_mutants(mt_other_sample, Mutants).

	% entity_mutants/3 tests

	test(mt_entity_mutants_3_01, subsumes(mutant(mt_other_sample, _, fail_insertion, _), Mutant)) :-
		mutation_testing::entity_mutants(mt_other_sample, Mutants, [mutators([fail_insertion])]),
		memberchk(Mutant, Mutants).

	test(mt_entity_mutants_occurrence_01, deterministic(Mutants == [mutant(mt_other_sample, check/1, fail_insertion, 1)])) :-
		mutation_testing::entity_mutants(mt_other_sample, Mutants, [mutators([fail_insertion])]).

	test(mt_entity_mutants_ordering_01, deterministic) :-
		mutation_testing::entity_mutants(mt_other_sample, Mutants, [mutators([fail_insertion, predicate_negation])]),
		^^assertion(Mutants == [
			mutant(mt_other_sample, check/1, fail_insertion, 1),
			mutant(mt_other_sample, check/1, predicate_negation, 1)
		]).

	test(mt_entity_mutants_occurrence_02, deterministic) :-
		mutation_testing::entity_mutants(mt_other_sample, Mutants, [mutators([fail_insertion, predicate_negation, relational_operator_replacement])]),
		memberchk(mutant(mt_other_sample, check/1, fail_insertion, 1), Mutants),
		memberchk(mutant(mt_other_sample, check/1, predicate_negation, 1), Mutants),
		memberchk(mutant(mt_other_sample, check/1, relational_operator_replacement, 1), Mutants).

	test(mt_entity_mutants_head_arguments_mutation_occurrence_01, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, Mutants, [mutators([head_arguments_mutation])]),
		memberchk(mutant(mt_sample, head_bound_atom_integer/2, head_arguments_mutation, _Occurrence), Mutants).

	% library_mutants/3 tests

	test(mt_library_mutants_3_01, deterministic(Mutants \== [])) :-
		loaded_test_library(Library),
		mutation_testing::library_mutants(Library, Mutants, [include_entities([mt_other_sample]), mutators([fail_insertion])]).

	test(mt_directory_mutants_3_01, deterministic(Mutants \== [])) :-
		loaded_test_directory(Directory),
		mutation_testing::directory_mutants(Directory, Mutants, [include_entities([mt_other_sample]), mutators([fail_insertion])]).

	% report_predicate/4 tests

	test(mt_report_predicate_4_01, deterministic) :-
		mutation_testing::report_predicate(mt_other_sample, check/1, report(mt_other_sample, summary(Total, _Killed, _Survived, _Untested, _Timeout, _NoCoverage, _Errors, _Score, _Threshold, _Passed), Results), [
			mutators([fail_insertion]),
			sampling(count(1)),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(Total == 1),
		^^assertion(subsumes_term([mutant_result(1, _, _)], Results)).

	% report_entity/3 tests

	test(mt_report_entity_3_01, deterministic) :-
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors, _Score, Threshold, Passed), Results), [
			mutators([fail_insertion]),
			sampling(count(3)),
			threshold(0.0),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(Total == 3),
		Accounted is Killed + Survived + Untested + Timeout + NoCoverage + Errors,
		^^assertion(Accounted == Total),
		^^assertion(Threshold == 0.0),
		^^assertion(Passed == true),
		^^assertion(Results = [mutant_result(1, _, _)| _]).

	test(mt_report_entity_3_02, deterministic) :-
		mt_mutation_state::set(original),
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors, _Score, _Threshold, _Passed), _), [
			mutators([fail_insertion, predicate_negation]),
			sampling(count(2)),
			threshold(0.0),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(Total == 2),
		Accounted is Killed + Survived + Untested + Timeout + NoCoverage + Errors,
		^^assertion(Accounted == Total),
		% verify subprocess execution didn't affect main process state
		mt_mutation_state::get(State),
		^^assertion(State == original).

	% report_library/3 tests

	test(mt_report_library_3_01, deterministic) :-
		loaded_test_library(Library),
		mutation_testing::report_library(Library, report(library(Library), [report(mt_other_sample, summary(Total, _Killed, _Survived, _Untested, _Timeout, _NoCoverage, _Errors, _Score, _Threshold, _Passed), Results)]), [
			include_entities([mt_other_sample]),
			mutators([fail_insertion]),
			sampling(count(1)),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(Total == 1),
		^^assertion(subsumes_term([mutant_result(1, _, _)], Results)).

	% report_directory/3 tests

	test(mt_report_directory_3_01, deterministic) :-
		loaded_test_directory(Directory),
		mutation_testing::report_directory(Directory, report(directory(Directory), [report(mt_other_sample, summary(Total, _Killed, _Survived, _Untested, _Timeout, _NoCoverage, _Errors, _Score, _Threshold, _Passed), Results)]), [
			include_entities([mt_other_sample]),
			mutators([fail_insertion]),
			sampling(count(1)),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(Total == 1),
		^^assertion(subsumes_term([mutant_result(1, _, _)], Results)).

	% predicate/2 tests

	test(mt_predicate_2_01, false) :-
		mutation_testing::predicate(mt_sample, missing/1).

	% predicate/3 tests

	test(mt_predicate_3_01, deterministic) :-
		mutation_testing::predicate(mt_sample, check/1, [
			mutators([fail_insertion]),
			sampling(count(1)),
			threshold(0.0),
			tester_file_name('subprocess_tester.lgt')
		]).

	test(mt_predicate_3_02, fail) :-
		mutation_testing::predicate(mt_sample, missing/1, []).

	% entity/2 tests

	test(mt_entity_2_01, deterministic) :-
		mutation_testing::entity(mt_sample, [
			mutators([fail_insertion]),
			sampling(count(3)),
			threshold(0.0),
			tester_file_name('subprocess_tester.lgt')
		]).

	% library/1 tests

	test(mt_library_1_02, fail) :-
		mutation_testing::library('this_library_should_not_exist_for_tests').

	% library/2 tests

	test(mt_library_2_01, deterministic) :-
		loaded_test_library(Library),
		mutation_testing::library(Library, [
			include_entities([this_entity_is_not_loaded]),
			mutators([fail_insertion]),
			sampling(count(1)),
			threshold(0.0),
			tester_file_name('subprocess_tester.lgt')
		]).

	% directory/1 tests

	test(mt_directory_1_02, fail) :-
		mutation_testing::directory('/this_directory_should_not_exist_for_tests').

	% directory/2 tests

	test(mt_directory_2_01, deterministic) :-
		loaded_test_directory(Directory),
		mutation_testing::directory(Directory, [
			include_entities([this_entity_is_not_loaded]),
			mutators([fail_insertion]),
			sampling(count(1)),
			threshold(0.0),
			tester_file_name('subprocess_tester.lgt')
		]).

	% default mutator tests

	test(mt_mutator_fail_insertion_01, deterministic) :-
		load_with_hook(fail_insertion(mt_sample, check/1, 1, false)),
		^^assertion(\+ mt_sample::check(1)),
		^^assertion(mt_other_sample::check(1)),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::check(1)),
		^^assertion(mt_other_sample::check(1)).

	test(mt_mutator_fail_insertion_02, deterministic) :-
		load_with_hook(fail_insertion(mt_sample, multi_clause/1, 1, false)),
		^^assertion(mt_sample::multi_clause_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::multi_clause_check).

	test(mt_mutator_fail_insertion_03, deterministic) :-
		load_with_hook(fail_insertion(mt_sample, multi_clause/1, 2, false)),
		^^assertion(\+ mt_sample::multi_clause_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::multi_clause_check).

	test(mt_mutator_fail_insertion_dcg_01, deterministic) :-
		load_with_hook(fail_insertion(mt_dcg_sample, dcg_multi//0, 2, false)),
		^^assertion(\+ mt_dcg_sample::dcg_multi_check(b)),
		^^assertion(mt_dcg_sample::dcg_multi_check(a)),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_dcg_sample::dcg_multi_check(b)).

	test(mt_mutator_predicate_negation_01, deterministic) :-
		load_with_hook(predicate_negation(mt_sample, check/1, 1, false)),
		^^assertion(\+ mt_sample::check(1)),
		^^assertion(mt_other_sample::check(1)),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::check(1)),
		^^assertion(mt_other_sample::check(1)).

	test(mt_mutator_predicate_negation_02, deterministic) :-
		load_with_hook(predicate_negation(mt_sample, neg_multi/1, 1, false)),
		^^assertion(mt_sample::neg_multi_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::neg_multi_check).

	test(mt_mutator_predicate_negation_dcg_01, deterministic) :-
		load_with_hook(predicate_negation(mt_dcg_sample, dcg_guard//0, 1, false)),
		^^assertion(\+ mt_dcg_sample::dcg_guard_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_dcg_sample::dcg_guard_check).

	test(mt_mutator_relational_operator_replacement_01, deterministic) :-
		load_with_hook(relational_operator_replacement(mt_sample, target/2, 1, false)),
		^^assertion(\+ mt_sample::check(1)),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::check(1)).

	test(mt_mutator_relational_operator_replacement_02, deterministic) :-
		load_with_hook(relational_operator_replacement(mt_sample, term_target/2, 1, false)),
		^^assertion(\+ mt_sample::term_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::term_check).

	test(mt_mutator_relational_operator_replacement_03, deterministic) :-
		load_with_hook(relational_operator_replacement(mt_sample, disj_cmp/1, 1, false)),
		^^assertion(\+ mt_sample::disj_cmp_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::disj_cmp_check).

	test(mt_mutator_relational_operator_replacement_dcg_01, deterministic) :-
		load_with_hook(relational_operator_replacement(mt_dcg_sample, dcg_guard//0, 1, false)),
		^^assertion(\+ mt_dcg_sample::dcg_guard_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_dcg_sample::dcg_guard_check).

	test(mt_mutator_arithmetic_operator_replacement_01, deterministic) :-
		load_with_hook(arithmetic_operator_replacement(mt_sample, target/2, 1, false)),
		^^assertion(\+ mt_sample::check(1)),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::check(1)).

	test(mt_mutator_arithmetic_operator_replacement_02, deterministic) :-
		load_with_hook(arithmetic_operator_replacement(mt_sample, arithmetic_value/1, 1, false)),
		mt_sample::arithmetic_value(Value),
		^^assertion(Value == 7),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		mt_sample::arithmetic_value(OriginalValue),
		^^assertion(OriginalValue == 5).

	test(mt_mutator_arithmetic_operator_replacement_dcg_01, deterministic) :-
		load_with_hook(arithmetic_operator_replacement(mt_dcg_sample, dcg_arithmetic//1, 1, false)),
		mt_dcg_sample::dcg_arithmetic_check(Value),
		^^assertion(Value == 0),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		mt_dcg_sample::dcg_arithmetic_check(OriginalValue),
		^^assertion(OriginalValue == 2).

	test(mt_mutator_truth_literal_flip_01, deterministic) :-
		load_with_hook(truth_literal_flip(mt_sample, always_true/0, 1, false)),
		^^assertion(\+ mt_sample::always_true),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::always_true).

	test(mt_mutator_truth_literal_flip_02, deterministic) :-
		load_with_hook(truth_literal_flip(mt_sample, truth_choice/0, 1, false)),
		^^assertion(\+ mt_sample::truth_choice),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::truth_choice).

	test(mt_mutator_truth_literal_flip_dcg_01, deterministic) :-
		load_with_hook(truth_literal_flip(mt_dcg_sample, dcg_truth//0, 1, false)),
		^^assertion(\+ mt_dcg_sample::dcg_truth_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_dcg_sample::dcg_truth_check).

	test(mt_mutator_head_arguments_mutation_01, deterministic) :-
		load_with_hook(head_arguments_mutation(mt_sample, head_bound_atom_integer/2, 1, false)),
		^^assertion(\+ mt_sample::head_bound_atom_integer_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::head_bound_atom_integer_check).

	test(mt_mutator_head_arguments_mutation_dcg_01, deterministic) :-
		load_with_hook(head_arguments_mutation(mt_dcg_sample, dcg_head_bound//1, 1, false)),
		^^assertion(\+ mt_dcg_sample::dcg_head_bound_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_dcg_sample::dcg_head_bound_check).

	test(mt_mutator_head_arguments_reordering_01, deterministic) :-
		load_with_hook(head_arguments_reordering(mt_sample, pair_match/2, 1, false)),
		^^assertion(\+ mt_sample::pair_match_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_sample::pair_match_check).

	test(mt_mutator_head_arguments_reordering_dcg_01, deterministic) :-
		load_with_hook(head_arguments_reordering(mt_dcg_sample, dcg_pair//2, 1, false)),
		^^assertion(\+ mt_dcg_sample::dcg_pair_check),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		^^assertion(mt_dcg_sample::dcg_pair_check).

	test(mt_mutator_clause_order_reordering_01, deterministic) :-
		load_with_hook(clause_order_reordering(mt_sample, ordered_choice/1, 1, false)),
		mt_sample::ordered_choice_check(Value),
		^^assertion(Value == second),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		mt_sample::ordered_choice_check(OriginalValue),
		^^assertion(OriginalValue == first).

	test(mt_mutator_clause_order_reordering_dcg_01, deterministic) :-
		load_with_hook(clause_order_reordering(mt_dcg_sample, dcg_ordered_choice//1, 1, false)),
		mt_dcg_sample::dcg_ordered_choice_check(Value),
		^^assertion(Value == second),
		logtalk_load(test_entities, [reload(always), source_data(on)]),
		mt_dcg_sample::dcg_ordered_choice_check(OriginalValue),
		^^assertion(OriginalValue == first).

	% options validation tests

	test(mt_max_mutators_option_01, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [
			max_mutators(1)
		]).

	test(mt_mutators_option_01, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([fail_insertion])]).

	test(mt_mutators_option_02, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([predicate_negation])]).

	test(mt_mutators_option_03, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([relational_operator_replacement])]).

	test(mt_mutators_option_04, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([arithmetic_operator_replacement])]).

	test(mt_mutators_option_05, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([truth_literal_flip])]).

	test(mt_mutators_option_06, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([head_arguments_mutation])]).

	test(mt_mutators_option_07, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([head_arguments_reordering])]).

	test(mt_mutators_option_08, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, _, [mutators([clause_order_reordering])]).

	test(mt_max_mutations_per_mutator_option_01, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, Mutants, [
			mutators([fail_insertion]),
			max_mutations_per_mutator(1)
		]),
		^^assertion(\+ member(mutant(mt_sample, multi_clause/1, fail_insertion, 2), Mutants)).

	test(mt_max_mutations_per_mutator_option_02, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, Mutants, [
			mutators([fail_insertion]),
			max_mutations_per_mutator(all)
		]),
		^^assertion(member(mutant(mt_sample, multi_clause/1, fail_insertion, 2), Mutants)).

	test(mt_max_mutations_per_mutator_option_03, deterministic) :-
		mutation_testing::entity_mutants(mt_sample, Mutants, [
			mutators([fail_insertion, predicate_negation]),
			max_mutations_per_mutator(1)
		]),
		once(member(mutant(mt_sample, check/1, fail_insertion, 1), Mutants)),
		once(member(mutant(mt_sample, check/1, predicate_negation, 1), Mutants)).

	test(mt_threshold_option_01, deterministic) :-
		mutation_testing::report_predicate(mt_other_sample, check/1, _, [
			mutators([fail_insertion]),
			threshold(75.0),
			tester_file_name('subprocess_tester.lgt')
		]).

	test(mt_verbose_option_01, deterministic) :-
		mutation_testing::report_predicate(mt_other_sample, check/1, _, [
			mutators([relational_operator_replacement]),
			verbose(true),
			tester_file_name('subprocess_tester.lgt')
		]).

	test(mt_print_mutated_term_option_01, deterministic) :-
		mutation_testing::report_predicate(mt_other_sample, check/1, _, [
			mutators([relational_operator_replacement]),
			print_mutated_term(true),
			verbose(true),
			tester_file_name('subprocess_tester.lgt')
		]).

	test(mt_timeout_option_01, deterministic) :-
		mutation_testing::report_predicate(mt_other_sample, check/1, _, [
			mutators([relational_operator_replacement]),
			timeout(1),
			tester_file_name('subprocess_tester.lgt')
		]).

	test(mt_sampling_option_01, deterministic) :-
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors, _Score, _Threshold, _Passed), Results), [
			mutators([arithmetic_operator_replacement, relational_operator_replacement]),
			sampling(count(3)),
			seed(1234),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(total, Total == 3),
		Accounted is Killed + Survived + Untested + Timeout + NoCoverage + Errors,
		^^assertion(accounted, Accounted == 3),
		^^assertion(Results = [mutant_result(1, _, _), mutant_result(2, _, _), mutant_result(3, _, _)]).

	test(mt_sampling_seed_reproducibility_01, deterministic) :-
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total1, _Killed1, _Survived1, _Untested1, _Timeout1, _NoCoverage1, _Errors1, _Score1, _Threshold1, _Passed1), Results1), [
			mutators([arithmetic_operator_replacement, relational_operator_replacement]),
			sampling(count(4)),
			seed(20260303),
			tester_file_name('subprocess_tester.lgt')
		]),
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total2, _Killed2, _Survived2, _Untested2, _Timeout2, _NoCoverage2, _Errors2, _Score2, _Threshold2, _Passed2), Results2), [
			mutators([arithmetic_operator_replacement, relational_operator_replacement]),
			sampling(count(4)),
			seed(20260303),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(total1, Total1 == 4),
		^^assertion(total2, Total2 == 4),
		^^assertion(results, Results1 == Results2).

	test(mt_sampling_all_seed_invariance_01, deterministic) :-
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total1, _Killed1, _Survived1, _Untested1, _Timeout1, _NoCoverage1, _Errors1, _Score1, _Threshold1, _Passed1), Results1), [
			mutators([arithmetic_operator_replacement, relational_operator_replacement]),
			sampling(all),
			seed(1),
			tester_file_name('subprocess_tester.lgt')
		]),
		mutation_testing::report_entity(mt_sample, report(mt_sample, summary(Total2, _Killed2, _Survived2, _Untested2, _Timeout2, _NoCoverage2, _Errors2, _Score2, _Threshold2, _Passed2), Results2), [
			mutators([arithmetic_operator_replacement, relational_operator_replacement]),
			sampling(all),
			seed(9999),
			tester_file_name('subprocess_tester.lgt')
		]),
		^^assertion(total, Total1 == Total2),
		^^assertion(results, Results1 == Results2).

	% auxiliary predicates

	load_with_hook(Hook) :-
		catch(Hook::reset, _, true),
		logtalk_load(test_entities, [hook(Hook), reload(always), source_data(on)]).

	loaded_test_library(mt_sample_library) :-
		object_property(mt_sample, file(_, Directory)),
		{retractall(logtalk_library_path(mt_sample_library, _))},
		{asserta(logtalk_library_path(mt_sample_library, Directory))}.

	loaded_test_directory(Directory) :-
		object_property(mt_sample, file(File)),
		logtalk::loaded_file_property(File, directory(Directory)).

:- end_object.
