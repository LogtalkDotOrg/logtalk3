%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Paulo Moura',
		date is 2026-04-01,
		comment is 'Unit tests for the "lgtdoc" tool.'
	]).

	cover(lgtdoc).

	:- private(xml_docs_directory_/1).
	:- dynamic(xml_docs_directory_/1).

	:- uses(lgtdoc, [
		all/1, all/0,
		rlibraries/2, rlibraries/1,
		rlibrary/2, rlibrary/1,
		libraries/2, libraries/1,
		library/2, library/1,
		rdirectories/2, rdirectories/1,
		rdirectory/2, rdirectory/1,
		directories/2, directories/1,
		directory/2, directory/1,
		files/2, files/1,
		file/2, file/1,
		diagnostics_tool/5,
		diagnostic_rules/1,
		diagnostic/3,
		diagnostics/2, diagnostics/3,
		diagnostics_summary/2, diagnostics_summary/3,
		diagnostics_preflight/2, diagnostics_preflight/3
	]).

	:- uses(lgtunit, [
		deterministic/1
	]).

	:- uses(os, [
		directory_files/3, delete_file/1, delete_directory_and_contents/1,
		directory_exists/1,
		working_directory/1, path_concat/3
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	setup :-
		retractall(xml_docs_directory_(_)),
		working_directory(Directory),
		path_concat(Directory, 'xml_docs/', XMLDocsDirectory),
		assertz(xml_docs_directory_(XMLDocsDirectory)).

	cleanup :-
		xml_docs_directory_(XMLDocsDirectory),
		delete_directory_and_contents(XMLDocsDirectory).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	test(lgtdoc_all_1_01, deterministic) :-
		all([]).

	test(lgtdoc_all_0_01, deterministic) :-
		all.

	test(lgtdoc_libraries_1_01, deterministic) :-
		libraries([lgtunit,packs]).

	test(lgtdoc_library_1_01, deterministic) :-
		library(lgtunit).

	test(lgtdoc_rlibraries_1_01, deterministic) :-
		rlibraries([lgtunit,packs]).

	test(lgtdoc_rlibrary_1_01, deterministic) :-
		rlibrary(lgtunit).

	test(lgtdoc_files_1_01, deterministic) :-
		object_property(lgtunit, file(File1)),
		category_property(lgtunit_messages, file(File2)),
		files([File1, File2]).

	test(lgtdoc_file_1_01, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File).

	test(lgtdoc_directories_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory1),
		logtalk::expand_library_path(lgtdoc, Directory2),
		directories([Directory1, Directory2]).

	test(lgtdoc_directory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory).

	test(lgtdoc_rdirectories_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory1),
		logtalk::expand_library_path(lgtdoc, Directory2),
		rdirectories([Directory1, Directory2]).

	test(lgtdoc_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory).

	test(lgtdoc_tool_info_01, deterministic) :-
		object_property(lgtdoc, info(Info)),
		memberchk(version(Major:Minor:Patch), Info),
		atomic_list_concat([Major, Minor, Patch], '.', Version),
		diagnostics_tool(lgtdoc, lgtdoc, Version, 'https://logtalk.org/', Properties),
		^^assertion(member(guid(_), Properties)),
		^^assertion(member(include_git_metadata(true), Properties)).

	test(lgtdoc_targets_01, deterministic) :-
		^^assertion(lgtdoc::diagnostic_target(all)),
		^^assertion(lgtdoc::diagnostic_target(file(_))),
		^^assertion(lgtdoc::diagnostic_target(files(_))),
		^^assertion(lgtdoc::diagnostic_target(directory(_))),
		^^assertion(lgtdoc::diagnostic_target(directories(_))),
		^^assertion(lgtdoc::diagnostic_target(rdirectory(_))),
		^^assertion(lgtdoc::diagnostic_target(rdirectories(_))),
		^^assertion(lgtdoc::diagnostic_target(library(_))),
		^^assertion(lgtdoc::diagnostic_target(libraries(_))),
		^^assertion(lgtdoc::diagnostic_target(rlibrary(_))),
		^^assertion(lgtdoc::diagnostic_target(rlibraries(_))).

	test(lgtdoc_rules_01, deterministic) :-
		diagnostic_rules(Rules),
		^^assertion(length(Rules, 8)).

	test(lgtdoc_file_caches_diagnostics_01, deterministic) :-
		object_property(lgtdoc_diagnostics_fixture(_), file(File)),
		file(File),
		xml_docs_directory_(XMLDocsDirectory),
		directory_files(XMLDocsDirectory, XMLFiles, [paths(absolute), extensions(['.xml'])]),
		^^assertion(XMLFiles \== []),
		delete_directory_and_contents(XMLDocsDirectory),
		diagnostics(file(File), Diagnostics),
		^^assertion(length(Diagnostics, 11)),
		^^assertion(\+ os::directory_exists(XMLDocsDirectory)).

	test(lgtdoc_diagnostics_file_01, deterministic) :-
		object_property(lgtdoc_diagnostics_fixture(_), file(File)),
		diagnostics(file(File), Diagnostics),
		^^assertion(length(Diagnostics, 11)),
		findall(RuleId, member(diagnostic(RuleId, _, _, _, _, _, _, _), Diagnostics), RuleIds0),
		sort(RuleIds0, RuleIds),
		^^assertion(RuleIds == [date_in_the_future, missing_entity_info_key, missing_predicate_directive, missing_predicate_info_key, missing_punctuation, non_standard_exception]).

	test(lgtdoc_diagnostics_summary_file_01, deterministic) :-
		object_property(lgtdoc_diagnostics_fixture(_), file(File)),
		diagnostics_summary(file(File), Summary),
		Summary = diagnostics_summary(file(File), 1, 11, Breakdown, [context_summary(context(object, lgtdoc_diagnostics_fixture(_)), 11, ContextBreakdown)]),
		^^assertion(Breakdown == diagnostic_breakdown([rule_count(date_in_the_future, 1), rule_count(missing_entity_info_key, 2), rule_count(missing_predicate_directive, 2), rule_count(missing_predicate_info_key, 2), rule_count(missing_punctuation, 3), rule_count(non_standard_exception, 1)], [severity_count(warning, 11)], [confidence_count(not_applicable, 11)])),
		^^assertion(ContextBreakdown == Breakdown).

	test(lgtdoc_diagnostics_summary_files_01, deterministic) :-
		object_property(lgtdoc_diagnostics_fixture(_), file(FixtureFile)),
		object_property(lgtdoc, file(ToolFile)),
		diagnostics(files([FixtureFile, ToolFile]), Diagnostics),
		diagnostics_summary(files([FixtureFile, ToolFile]), diagnostics_summary(files([FixtureFile, ToolFile]), TotalContexts, TotalDiagnostics, _Breakdown, ContextSummaries)),
		setof(Context, RuleId^Severity^Confidence^Message^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), Contexts),
		findall(Context, member(context_summary(Context, _DiagnosticsCount, _ContextBreakdown), ContextSummaries), SummaryContexts),
		^^assertion(length(Diagnostics, TotalDiagnostics)),
		^^assertion(length(Contexts, TotalContexts)),
		^^assertion(SummaryContexts == Contexts).

	test(lgtdoc_diagnostics_preflight_file_01, deterministic) :-
		object_property(lgtdoc_diagnostics_fixture(_), file(File)),
		diagnostics_preflight(file(File), Issues),
		^^assertion(Issues == []).

	test(lgtdoc_diagnostics_options_file_01, deterministic) :-
		^^file_path('diagnostics_fixture.lgt', File),
		diagnostics(file(File), Diagnostics, [explanations(true)]),
		^^assertion(length(Diagnostics, 11)).

	test(lgtdoc_diagnostics_options_file_02, deterministic) :-
		^^file_path('diagnostics_fixture.lgt', File),
		findall(Diagnostic, diagnostic(file(File), Diagnostic, [explanations(false)]), Enumerated),
		^^assertion(length(Enumerated, 11)).

	test(lgtdoc_diagnostics_options_file_03, deterministic) :-
		^^file_path('diagnostics_fixture.lgt', File),
		diagnostics_summary(file(File), diagnostics_summary(file(File), 1, 11, _Breakdown, _ContextSummaries), [explanations(true)]),
		true.

	test(lgtdoc_diagnostics_options_file_04, deterministic) :-
		^^file_path('diagnostics_fixture.lgt', File),
		diagnostics_preflight(file(File), Issues0, [explanations(false)]),
		^^assertion(Issues0 == []).

	% suppress all messages from the "lgtdoc" tool
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, lgtdoc, _Tokens).

:- end_object.
