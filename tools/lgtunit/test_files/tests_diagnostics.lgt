%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests_diagnostics,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-31,
		comment is 'Unit tests for the lgtunit diagnostics protocol implementation.'
	]).

	:- uses(lgtunit, [
		assertion/1,
		diagnostics_tool/5,
		diagnostic_target/1,
		diagnostic_rule/5,
		diagnostic_rules/1,
		diagnostic/2, diagnostic/3,
		diagnostics/2, diagnostics/3,
		diagnostics_summary/2, diagnostics_summary/3,
		diagnostics_preflight/2, diagnostics_preflight/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	test(lgtunit_targets_01, deterministic) :-
		assertion(diagnostic_target(all)),
		assertion(diagnostic_target(file(_))),
		assertion(diagnostic_target(tests(_))).

	test(lgtunit_tool_info_01, deterministic) :-
		object_property(lgtunit, info(Info)),
		memberchk(version(Major:Minor:Patch), Info),
		atomic_list_concat([Major, Minor, Patch], '.', Version),
		diagnostics_tool(lgtunit, lgtunit, Version, 'https://logtalk.org/', Properties),
		assertion(member(guid(_), Properties)),
		assertion(member(include_git_metadata(true), Properties)),
		assertion(member(count_key(totalWarnings), Properties)),
		assertion(member(fingerprint_algorithm(canonical_warning_v1), Properties)).

	test(lgtunit_rules_01, deterministic) :-
		diagnostic_rules(Rules),
		assertion(length(Rules, 8)),
		assertion(diagnostic_target(entity(_))),
		assertion(member(diagnostic_rule(assertion_is_always_true, _, _, warning, []), Rules)),
		assertion(diagnostic_target(directory(_))),
		assertion(diagnostic_target(rdirectory(_))),
		assertion(diagnostic_target(library(_))),
		assertion(diagnostic_target(rlibrary(_))),
		assertion(diagnostic_rule(non_instantiated_test_option, _, _, warning, [])).

	test(lgtunit_diagnostics_entity_01, deterministic) :-
		setof(Diagnostic, diagnostic(entity(lgtunit_diagnostics_fixture), Diagnostic), Diagnostics),
		assertion(length(Diagnostics, 8)).

	test(lgtunit_diagnostics_file_01, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		diagnostics(file(File), Diagnostics),
		assertion(length(Diagnostics, 8)),
		findall(RuleId, member(diagnostic(RuleId, _, _, _, _, _, _, _), Diagnostics), RuleIds0),
		sort(RuleIds0, RuleIds),
		assertion(RuleIds == [assertion_is_always_error, assertion_is_always_false, assertion_is_always_true, assertion_uses_unification, invalid_test_option, no_code_coverage_for_protocols, non_instantiated_test_option, unknown_entity_declared_covered]).

	test(lgtunit_diagnostics_directory_01, deterministic) :-
		source_tree_directories(_ToolsDirectory, Directory),
		diagnostics(directory(Directory), Diagnostics),
		assertion(length(Diagnostics, 8)).

	test(lgtunit_diagnostics_directory_02, deterministic) :-
		source_tree_directories(Directory, _LgtunitDirectory),
		diagnostics(directory(Directory), Diagnostics),
		assertion(Diagnostics == []).

	test(lgtunit_diagnostics_rdirectory_01, deterministic) :-
		source_tree_directories(Directory, _LgtunitDirectory),
		diagnostics(rdirectory(Directory), Diagnostics),
		assertion(length(Diagnostics, 8)).

	test(lgtunit_diagnostics_library_01, deterministic) :-
		with_source_tree_library_paths((
			diagnostics(library(lgtunit), Diagnostics),
			assertion(length(Diagnostics, 8))
		)).

	test(lgtunit_diagnostics_library_02, deterministic) :-
		with_source_tree_library_paths((
			diagnostics(library(tools), Diagnostics),
			assertion(Diagnostics == [])
		)).

	test(lgtunit_diagnostics_rlibrary_01, deterministic) :-
		with_source_tree_library_paths((
			diagnostics(rlibrary(tools), Diagnostics),
			assertion(length(Diagnostics, 8))
		)).

	test(lgtunit_diagnostics_tests_01, deterministic) :-
		setof(Diagnostic, diagnostic(tests(lgtunit_diagnostics_fixture), Diagnostic), Diagnostics),
		assertion(length(Diagnostics, 8)).

	test(lgtunit_diagnostics_summary_01, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		diagnostics_summary(file(File), Summary),
		Summary = diagnostics_summary(file(File), 1, 8, Breakdown, [context_summary(context(object, lgtunit_diagnostics_fixture), 8, ContextBreakdown)]),
		assertion(Breakdown == diagnostic_breakdown([rule_count(assertion_is_always_error, 1), rule_count(assertion_is_always_false, 1), rule_count(assertion_is_always_true, 1), rule_count(assertion_uses_unification, 1), rule_count(invalid_test_option, 1), rule_count(no_code_coverage_for_protocols, 1), rule_count(non_instantiated_test_option, 1), rule_count(unknown_entity_declared_covered, 1)], [severity_count(warning, 8)], [confidence_count(not_applicable, 8)])),
		assertion(ContextBreakdown == Breakdown).

	test(lgtunit_preflight_01, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		diagnostics_preflight(file(File), Issues),
		assertion(Issues == []).

	test(lgtunit_diagnostics_options_01, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		diagnostics(file(File), Diagnostics, [explanations(true)]),
		assertion(length(Diagnostics, 8)),
		forall(
			member(diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, _File, _Lines, Properties), Diagnostics),
			assertion(member(explanation(_), Properties))
		).

	test(lgtunit_diagnostics_options_02, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		findall(Diagnostic, diagnostic(file(File), Diagnostic, [explanations(true)]), Enumerated),
		assertion(length(Enumerated, 8)),
		forall(
			member(diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, _File, _Lines, Properties), Enumerated),
			assertion(member(explanation(_), Properties))
		).

	test(lgtunit_diagnostics_options_03, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		diagnostics_summary(file(File), diagnostics_summary(file(File), 1, 8, _Breakdown, _ContextSummaries), [explanations(true)]),
		true.

	test(lgtunit_diagnostics_options_04, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		diagnostics_preflight(file(File), Issues0, [explanations(false)]),
		assertion(Issues0 == []).

	source_tree_directories(ToolsDirectory, LgtunitDirectory) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		atom_concat(ToolsDirectory, 'lgtunit/test_files/diagnostics_fixture.lgt', File),
		atom_concat(ToolsDirectory, 'lgtunit/test_files/', LgtunitDirectory).

	with_source_tree_library_paths(Goal) :-
		{findall(Path, logtalk_library_path(tools, Path), ToolsPaths)},
		{findall(Path, logtalk_library_path(lgtunit, Path), LgtunitPaths)},
		source_tree_directories(ToolsDirectory, LgtunitDirectory),
		{retractall(logtalk_library_path(tools, _))},
		{retractall(logtalk_library_path(lgtunit, _))},
		{assertz(logtalk_library_path(tools, ToolsDirectory))},
		{assertz(logtalk_library_path(lgtunit, LgtunitDirectory))},
		catch(
			(	call(Goal) ->
				Success = true
			;	Success = false
			),
			Error,
			(	restore_library_paths(tools, ToolsPaths),
				restore_library_paths(lgtunit, LgtunitPaths),
				throw(Error)
			)
		),
		restore_library_paths(tools, ToolsPaths),
		restore_library_paths(lgtunit, LgtunitPaths),
		Success == true.

	restore_library_paths(Library, Paths) :-
		{retractall(logtalk_library_path(Library, _))},
		forall(member(Path, Paths), {assertz(logtalk_library_path(Library, Path))}).

:- end_object.
