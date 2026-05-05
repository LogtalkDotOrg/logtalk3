%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2016 Barry Evans <barryevans@kyndi.com>
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
		version is 0:9:1,
		author is 'Barry Evans and Paulo Moura',
		date is 2026-04-01,
		comment is 'Unit tests for the "dead_code_scanner" tool.'
	]).

	:- uses(dead_code_scanner, [
		diagnostics/2, diagnostics/3, diagnostic/2, diagnostic/3,
		diagnostics_summary/2, diagnostics_summary/3,
		diagnostics_preflight/2, diagnostics_preflight/3,
		predicates/2, predicates/3, predicate/2, predicate/3,
		all/0,
		rlibrary/1, library/1,
		rdirectory/2, rdirectory/1, directory/1,
		file/1,
		entity/2, entity/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	cover(dead_code_scanner).

	% category tests

	test(dcs_tool_info_01, deterministic) :-
		object_property(dead_code_scanner, info(Info)),
		memberchk(version(Major:Minor:Patch), Info),
		atomic_list_concat([Major, Minor, Patch], '.', Version),
		dead_code_scanner::diagnostics_tool(dead_code_scanner, dead_code_scanner, Version, 'https://logtalk.org/', Properties),
		^^assertion(member(guid(_), Properties)),
		^^assertion(member(fingerprint_algorithm(canonical_finding_v1), Properties)),
		^^assertion(member(automation_id(target), Properties)),
		^^assertion(member(include_invocations(true), Properties)),
		^^assertion(member(include_git_metadata(true), Properties)),
		^^assertion(member(include_version_control_provenance(true), Properties)).

	test(dcs_stand_alone_category_01, deterministic) :-
		predicates(stand_alone_category, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_category_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_category, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_category_01, deterministic) :-
		predicates(category, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_category_02, deterministic) :-
		setof(Predicate, predicate(category, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_findings_entity_01, deterministic) :-
		diagnostics(entity(category), Diagnostics),
		setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(category, category, Predicate, Diagnostic)), Predicates),
		setof(RuleId-Confidence, Severity^Message^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, context(category, category), File, Lines, Properties), Diagnostics), Metadata),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]),
		^^assertion(Metadata == [local_dead_code-medium]).

	test(dcs_findings_entity_02, deterministic) :-
		diagnostics(entity(category), Diagnostics, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(category, category, Predicate, Diagnostic)), Predicates),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	test(dcs_findings_entity_03, deterministic) :-
		diagnostics(entity(category), Diagnostics, [waive_findings([
			dead_predicate(local_dead_code, medium, _, category, category, dead_predicate/0, _, _),
			dead_predicate(local_dead_code, medium, _, category, category, dead_non_terminal//0, _, _)
		])]),
		setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(category, category, Predicate, Diagnostic)), Predicates),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	test(dcs_finding_entity_01, deterministic) :-
		setof(Diagnostic, diagnostic(entity(category), Diagnostic), Diagnostics),
		setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(category, category, Predicate, Diagnostic)), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_finding_entity_02, deterministic) :-
		setof(Diagnostic, diagnostic(entity(category), Diagnostic, [waive_findings([dead_predicate(local_dead_code, medium, _, category, category, dead_predicate_1/0, _, _)])]), Diagnostics),
		setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(category, category, Predicate, Diagnostic)), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_diagnostics_options_01, deterministic) :-
		diagnostics(entity(category), Diagnostics, [explanations(true)]),
		^^assertion(length(Diagnostics, 5)).

	test(dcs_diagnostics_options_02, deterministic) :-
		findall(Diagnostic, diagnostic(entity(category), Diagnostic, [explanations(false)]), Enumerated),
		^^assertion(length(Enumerated, 5)).

	test(dcs_diagnostics_options_03, deterministic) :-
		diagnostics_summary(entity(category), diagnostics_summary(entity(category), 1, 5, _Breakdown, _ContextSummaries), [explanations(true)]),
		true.

	test(dcs_diagnostics_options_04, deterministic) :-
		diagnostics_preflight(entity(category), Issues),
		diagnostics_preflight(entity(category), Issues0, [explanations(false)]),
		^^assertion(Issues0 == Issues).

	test(dcs_summary_entity_01, deterministic) :-
		diagnostics_summary(entity(category), Summary),
		^^assertion(Summary == diagnostics_summary(entity(category), 1, 5, diagnostic_breakdown([rule_count(local_dead_code, 5)], [severity_count(note, 5)], [confidence_count(medium, 5)]), [context_summary(context(category, category), 5, diagnostic_breakdown([rule_count(local_dead_code, 5)], [severity_count(note, 5)], [confidence_count(medium, 5)]))])).

	test(dcs_summary_entity_02, deterministic) :-
		diagnostics_summary(entity(category), Summary, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		^^assertion(Summary == diagnostics_summary(entity(category), 1, 3, diagnostic_breakdown([rule_count(local_dead_code, 3)], [severity_count(note, 3)], [confidence_count(medium, 3)]), [context_summary(context(category, category), 3, diagnostic_breakdown([rule_count(local_dead_code, 3)], [severity_count(note, 3)], [confidence_count(medium, 3)]))])).

	test(dcs_summary_entity_03, deterministic) :-
		diagnostics_summary(entity(category), Summary, [waive_findings([dead_predicate(local_dead_code, medium, _, category, category, dead_predicate_1/0, _, _)])]),
		^^assertion(Summary == diagnostics_summary(entity(category), 1, 4, diagnostic_breakdown([rule_count(local_dead_code, 4)], [severity_count(note, 4)], [confidence_count(medium, 4)]), [context_summary(context(category, category), 4, diagnostic_breakdown([rule_count(local_dead_code, 4)], [severity_count(note, 4)], [confidence_count(medium, 4)]))])).

	test(dcs_summary_all_01, deterministic) :-
		diagnostics(all, Diagnostics),
		diagnostics_summary(all, diagnostics_summary(all, TotalContexts, TotalDiagnostics, _Breakdown, ContextSummaries)),
		setof(Context, RuleId^Severity^Confidence^Message^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), Contexts),
		findall(Context, member(context_summary(Context, _DiagnosticsCount, _ContextBreakdown), ContextSummaries), SummaryContexts),
		^^assertion(length(Diagnostics, TotalDiagnostics)),
		^^assertion(length(Contexts, TotalContexts)),
		^^assertion(SummaryContexts == Contexts).

	test(dcs_category_03, deterministic) :-
		predicates(category, Predicates, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	test(dcs_category_04, deterministic) :-
		setof(Predicate, predicate(category, Predicate, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]), Predicates),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	% prototype tests

	test(dcs_stand_alone_prototype_01, deterministic) :-
		predicates(stand_alone_prototype, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_prototype_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_prototype, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_prototype_01, deterministic) :-
		predicates(prototype, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_prototype_02, deterministic) :-
		setof(Predicate, predicate(prototype, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% class and instance tests

	test(dcs_stand_alone_class_01, deterministic) :-
		predicates(stand_alone_class, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_class_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_class, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_class_01, deterministic) :-
		predicates(class, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_class_02, deterministic) :-
		setof(Predicate, predicate(class, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_with_metaclass_01, deterministic) :-
		predicates(subclass_with_metaclass, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_with_metaclass_02, deterministic) :-
		setof(Predicate, predicate(subclass_with_metaclass, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_01, deterministic) :-
		predicates(subclass, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_02, deterministic) :-
		setof(Predicate, predicate(subclass, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_instance_01, deterministic) :-
		predicates(instance, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_instance_02, deterministic) :-
		setof(Predicate, predicate(instance, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	test(dcs_predicates_2_01, deterministic) :-
		predicates(lgtunit, _).

	test(dcs_entity_1_01, deterministic) :-
		entity(lgtunit).

	test(dcs_entity_2_01, deterministic) :-
		entity(category, [exclude_predicates([dead_predicate/0])]).

	test(dcs_entity_1_02, deterministic) :-
		entity(lgtunit_messages).

	test(dcs_all_0_01, deterministic) :-
		all.

	test(dcs_library_1_01, deterministic) :-
		library(lgtunit).

	test(dcs_rlibrary_1_01, deterministic) :-
		rlibrary(lgtunit).

	test(dcs_file_1_01, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File).

	test(dcs_directory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory).

	test(dcs_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory).

	% tests for non-called predicates listed in uses/2 and use_module/2 directives

	:- if(current_logtalk_flag(modules, supported)).

		test(dcs_uses_directive_01, deterministic) :-
			setof(Predicate, predicate(predicate_directives, Predicate), Predicates),
			^^assertion(Predicates == [some_module:bar/2, some_module:baz/2, list::app/3, list::member/2, logtalk::dbg/1]).

		test(dcs_findings_uses_directive_01, deterministic) :-
			setof(Diagnostic, diagnostic(entity(predicate_directives), Diagnostic), Diagnostics),
			setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(object, predicate_directives, Predicate, Diagnostic)), Predicates),
			setof(Classification, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_classification(Diagnostic, Classification)), ClassifiedPredicates),
			^^assertion(Predicates == [some_module:bar/2, some_module:baz/2, list::app/3, list::member/2, logtalk::dbg/1]),
			^^assertion(ClassifiedPredicates == [classification(unused_use_module_resource, some_module:bar/2), classification(unused_use_module_resource, some_module:baz/2), classification(unused_uses_resource, list::app/3), classification(unused_uses_resource, list::member/2), classification(unused_uses_resource, logtalk::dbg/1)]).

	:- else.

		test(dcs_uses_directive_01, deterministic) :-
			setof(Predicate, predicate(predicate_directives, Predicate), Predicates),
			^^assertion(Predicates == [list::app/3, list::member/2, logtalk::dbg/1]).

		test(dcs_findings_uses_directive_01, deterministic) :-
			setof(Diagnostic, diagnostic(entity(predicate_directives), Diagnostic), Diagnostics),
			setof(Predicate, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_predicate(object, predicate_directives, Predicate, Diagnostic)), Predicates),
			setof(Classification, Diagnostic^(member(Diagnostic, Diagnostics), diagnostic_classification(Diagnostic, Classification)), ClassifiedPredicates),
			^^assertion(Predicates == [list::app/3, list::member/2, logtalk::dbg/1]),
			^^assertion(ClassifiedPredicates == [classification(unused_uses_resource, list::app/3), classification(unused_uses_resource, list::member/2), classification(unused_uses_resource, logtalk::dbg/1)]).

	:- endif.

	% tests for option validation

	test(dcs_exclude_directories_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_directories([foo, bar])]).

	test(dcs_exclude_files_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_files([foo, bar])]).

	test(dcs_exclude_libraries_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_libraries([foo, bar])]).

	test(dcs_exclude_entities_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_entities([foo, bar])]).

	test(dcs_exclude_predicates_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_predicates([foo/1, bar//2])]).

	test(dcs_waive_findings_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [waive_findings([dead_predicate(local_dead_code, medium, _, object, foo, bar/1, _, _)])]).

	test(dcs_preflight_01, deterministic) :-
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, on),
		set_logtalk_flag(optimize, on),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		diagnostics_preflight(file(test_entities), Issues),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		^^assertion(Issues == []).

	test(dcs_preflight_02, deterministic) :-
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, off),
		set_logtalk_flag(optimize, off),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		diagnostics_preflight(file(test_entities), Issues),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		^^assertion(length(Issues, 2)),
		^^assertion(member(preflight_issue(missing_analysis_prerequisite, note, _, context(file, _), _, 0-0, [prerequisite(optimize)]), Issues)),
		^^assertion(member(preflight_issue(missing_analysis_prerequisite, warning, _, context(file, _), _, 0-0, [prerequisite(source_data)]), Issues)).

	test(dcs_preflight_warning_01, deterministic) :-
		retractall(preflight_warning(_)),
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, on),
		set_logtalk_flag(optimize, on),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		file(test_entities),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		\+ preflight_warning(_).

	test(dcs_preflight_warning_02, deterministic) :-
		retractall(preflight_warning(_)),
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, off),
		set_logtalk_flag(optimize, off),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		file(test_entities),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		setof(Warning, preflight_warning(Warning), Warnings),
		^^assertion(subsumes_term([
			missing_analysis_prerequisite(_, optimize),
			missing_analysis_prerequisite(_, source_data)
		], Warnings)).

	test(dcs_local_dead_code_confidence_01, deterministic) :-
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, on),
		set_logtalk_flag(optimize, on),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		diagnostics(file(test_entities), Diagnostics),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		^^assertion(member(diagnostic(local_dead_code, warning, high, _, context(category, category), _, _, Properties), Diagnostics)),
		^^assertion(member(finding_properties(FindingProperties), Properties)),
		^^assertion(member(optimize(on), FindingProperties)).

	% suppress all messages from the "dead_code_scanner"
	% component to not pollute the unit tests output

	:- private(preflight_warning/1).
	:- dynamic(preflight_warning/1).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _Kind, dead_code_scanner, _Tokens) :-
		Message = missing_analysis_prerequisite(_, _),
		assertz(preflight_warning(Message)).
	logtalk::message_hook(_Message, _Kind, dead_code_scanner, _Tokens).

	diagnostic_predicate(EntityKind, Entity, Predicate, diagnostic(_RuleId, _Severity, _Confidence, _Message, context(EntityKind, Entity), _File, _Lines, Properties)) :-
		member(predicate(Predicate), Properties).

	diagnostic_classification(diagnostic(RuleId, _Severity, _Confidence, _Message, _Context, _File, _Lines, Properties), classification(RuleId, Predicate)) :-
		member(predicate(Predicate), Properties).

:- end_object.
