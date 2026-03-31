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
		date is 2026-03-31,
		comment is 'Unit tests for the linter_reporter tool.'
	]).

	:- uses(linter_reporter, [
		enable/0, enable/1, disable/0, reset/0, warning/1, warnings/1, summary/1,
		diagnostic_target/1, diagnostic_rule/5, diagnostic_rules/1,
		diagnostic/2, diagnostic/3,
		diagnostics/2, diagnostics/3,
		diagnostics_summary/2, diagnostics_summary/3,
		diagnostics_preflight/2, diagnostics_preflight/3
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	cover(linter_reporter).

	test(lr_tool_info_01, deterministic) :-
		object_property(linter_reporter, info(Info)),
		memberchk(version(Major:Minor:Patch), Info),
		atomic_list_concat([Major, Minor, Patch], '.', Version),
		linter_reporter::diagnostics_tool(linter_reporter, linter_reporter, Version, 'https://logtalk.org/', Properties),
		assertion(member(guid(_), Properties)),
		assertion(member(fingerprint_algorithm(canonical_warning_v1), Properties)),
		assertion(member(count_key(totalWarnings), Properties)).

	test(lr_targets_01, deterministic) :-
		assertion(diagnostic_target(all)),
		assertion(diagnostic_target(entity(_))),
		assertion(diagnostic_target(file(_))),
		assertion(diagnostic_target(directory(_))),
		assertion(diagnostic_target(rdirectory(_))),
		assertion(diagnostic_target(library(_))),
		assertion(diagnostic_target(rlibrary(_))).

	test(lr_targets_02, deterministic) :-
		prepare_target_run(false),
		diagnostics(entity(misspell), Diagnostics),
		assertion(Diagnostics \== []),
		forall(
			member(diagnostic(_RuleId, _Severity, _Confidence, _Message, Context, _File, _Lines, _Properties), Diagnostics),
			assertion(Context == context(object, misspell))
		).

	test(lr_targets_03, deterministic) :-
		prepare_target_run(false),
		diagnostics(file(errors(warnings)), Diagnostics),
		logtalk::expand_library_path(errors, Directory),
		atom_concat(Directory, 'warnings.lgt', File),
		assertion(Diagnostics \== []),
		forall(
			member(diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, DiagnosticFile, _Lines, _Properties), Diagnostics),
			assertion(DiagnosticFile == File)
		).

	test(lr_targets_04, deterministic) :-
		prepare_target_run(false),
		diagnostics(rlibrary(tools), Diagnostics),
		assertion(Diagnostics == []).

	test(lr_targets_05, deterministic) :-
		prepare_target_run(false),
		logtalk::expand_library_path(tools, Directory),
		diagnostics(rdirectory(Directory), Diagnostics),
		assertion(Diagnostics == []).

	test(lr_targets_06, deterministic) :-
		prepare_target_run(false),
		diagnostics(library(errors), Diagnostics),
		assertion(Diagnostics \== []),
		assertion(member(diagnostic(_RuleId, _Severity, _Confidence, _Message, context(object, misspell), _File, _Lines, _Properties), ErrorsDiagnostics)).

	test(lr_targets_07, deterministic) :-
		prepare_run(false),
		diagnostics(file(errors(warnings)), Diagnostics),
		logtalk::expand_library_path(errors, Directory),
		atom_concat(Directory, 'warnings.lgt', File),
		assertion(Diagnostics \== []),
		assertion(member(diagnostic(singleton_variables, _Severity, _Confidence, _Message, context(file, File), File, _Lines, _Properties), Diagnostics)),
		assertion(member(diagnostic(singleton_variables, _Severity, _Confidence, _Message, context(object, singletons(_)), File, _Lines, _Properties), Diagnostics)).

	test(lr_warnings_01, deterministic) :-
		prepare_run(false),
		warnings(Warnings),
		length(Warnings, Count),
		assertion(Count > 0),
		setof(RuleId, File^Lines^Context^Properties^Flag^member(linter_warning(Flag, RuleId, File, Lines, Context, Properties), Warnings), RuleIds),
		assertion(member(unknown_predicate_called_but_not_defined, RuleIds)),
		assertion(member(singleton_variables, RuleIds)),
		assertion(member(suspicious_call, RuleIds)).

	test(lr_warnings_02, deterministic) :-
		prepare_run(false),
		warnings(Warnings),
		forall(
			member(linter_warning(_Flag, _RuleId, _File, _Lines, _Context, Properties), Warnings),
			assertion(\+ member(explanation(_), Properties))
		).

	test(lr_warnings_03, deterministic) :-
		prepare_run(true),
		warnings(Warnings),
		assertion(Warnings \== []),
		assertion(warning_with_explanation(Warnings)).

	test(lr_diagnostics_01, deterministic) :-
		prepare_run(false),
		diagnostics(all, Diagnostics),
		length(Diagnostics, Count),
		assertion(Count > 0),
		setof(RuleId, Severity^Confidence^Message^Context^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), RuleIds),
		assertion(member(unknown_predicate_called_but_not_defined, RuleIds)),
		assertion(member(singleton_variables, RuleIds)),
		assertion(member(suspicious_call, RuleIds)).

	test(lr_diagnostics_02, deterministic) :-
		prepare_run(false),
		diagnostics(all, Diagnostics),
		forall(
			member(diagnostic(_RuleId, warning, not_applicable, _Message, _Context, _File, _Lines, Properties), Diagnostics),
			(	assertion(ground(Properties)),
				assertion(member(flag(_), Properties)),
				assertion(\+ member(explanation(_), Properties))
			)
		).

	test(lr_diagnostics_03, deterministic) :-
		prepare_run(true),
		setof(Diagnostic, diagnostic(all, Diagnostic), Diagnostics),
		assertion(Diagnostics \== []),
		assertion(diagnostic_with_explanation(Diagnostics)).

	test(lr_diagnostics_options_01, deterministic) :-
		prepare_run(false),
		diagnostics(all, Diagnostics, [explanations(true)]),
		length(Diagnostics, Count),
		assertion(Count > 0).

	test(lr_diagnostics_options_02, deterministic) :-
		prepare_run(false),
		findall(Diagnostic, diagnostic(all, Diagnostic, [explanations(false)]), Enumerated),
		assertion(Enumerated \== []).

	test(lr_diagnostics_options_03, deterministic) :-
		prepare_run(false),
		diagnostics_summary(all, diagnostics_summary(all, TotalContexts, TotalDiagnostics, _Breakdown, _ContextSummaries), [explanations(true)]),
		assertion(TotalContexts > 0),
		assertion(TotalDiagnostics > 0).

	test(lr_diagnostics_options_04, deterministic) :-
		prepare_run(false),
		diagnostics_preflight(all, Issues, [explanations(false)]),
		assertion(Issues == []).

	test(lr_rules_01, deterministic) :-
		prepare_run(true),
		diagnostic_rules(Rules),
		assertion(Rules \== []),
		assertion(member(diagnostic_rule(singleton_variables, _, _, warning, []), Rules)),
		assertion(diagnostic_rule(singleton_variables, _, _, warning, [])).

	test(lr_summary_01, deterministic) :-
		prepare_run(false),
		summary(summary(TotalWarnings, RuleCounts, FlagCounts)),
		assertion(TotalWarnings > 0),
		assertion(member(rule_count(singleton_variables, _), RuleCounts)),
		assertion(member(flag_count(singleton_variables, _), FlagCounts)).

	test(lr_diagnostics_summary_01, deterministic) :-
		prepare_run(false),
		diagnostics_summary(all, diagnostics_summary(all, TotalContexts, TotalDiagnostics, diagnostic_breakdown(RuleCounts, SeverityCounts, ConfidenceCounts), ContextSummaries)),
		assertion(TotalContexts > 0),
		assertion(TotalDiagnostics > 0),
		assertion(member(rule_count(singleton_variables, _), RuleCounts)),
		assertion(SeverityCounts == [severity_count(warning, TotalDiagnostics)]),
		assertion(ConfidenceCounts == [confidence_count(not_applicable, TotalDiagnostics)]),
		assertion(ContextSummaries \== []).

	test(lr_diagnostics_summary_02, deterministic) :-
		prepare_run(false),
		diagnostics(all, Diagnostics),
		diagnostics_summary(all, diagnostics_summary(all, TotalContexts, TotalDiagnostics, _Breakdown, ContextSummaries)),
		setof(Context, RuleId^Severity^Confidence^Message^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), Contexts),
		findall(Context, member(context_summary(Context, _DiagnosticsCount, _ContextBreakdown), ContextSummaries), SummaryContexts),
		assertion(length(Diagnostics, TotalDiagnostics)),
		assertion(length(Contexts, TotalContexts)),
		assertion(SummaryContexts == Contexts).

	test(lr_preflight_01, deterministic) :-
		prepare_run(false),
		diagnostics_preflight(all, Issues),
		assertion(Issues == []).

	test(lr_warning_01, true) :-
		prepare_run(true),
		forall(
			warning(linter_warning(_Flag, _RuleId, _File, _Lines, _Context, Properties)),
			(	assertion(ground(Properties)),
				assertion(member(message(_), Properties))
			)
		).

	prepare_run(Explanations) :-
		^^clean_file('linter_warnings.sarif'),
		reset,
		enable([explanations(Explanations)]),
		logtalk_load([errors(warnings), errors(main_include_compiler_warning)], [reload(always)]),
		disable.

	prepare_target_run(Explanations) :-
		^^clean_file('linter_warnings.sarif'),
		reset,
		enable([explanations(Explanations)]),
		logtalk_load([errors(warnings), errors(main_include_compiler_warning)], [reload(always)]),
		logtalk_load(lgtunit('test_files/diagnostics_fixture'), [hook(lgtunit), reload(always)]),
		disable.

	% not all warnings have explanations
	warning_with_explanation(Warnings) :-
		member(linter_warning(_Flag, _RuleId, _File, _Lines, _Context, Properties), Warnings),
		memberchk(explanation(_), Properties),
		!.

	diagnostic_with_explanation(Diagnostics) :-
		member(diagnostic(_RuleId, _Severity, _Confidence, _Message, _Context, _File, _Lines, Properties), Diagnostics),
		memberchk(explanation(_), Properties),
		!.

:- end_object.
