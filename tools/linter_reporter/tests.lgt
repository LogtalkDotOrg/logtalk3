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
		date is 2026-03-29,
		comment is 'Unit tests for the linter_reporter tool.'
	]).

	:- uses(linter_reporter, [
		enable/0, disable/0, reset/0, warning/1, warnings/1, summary/1, report/1
	]).

	:- uses(json, [
		parse/2 as json_parse/2
	]).

	:- uses(json_schema, [
		parse/2 as json_schema_parse/2,
		validate/2 as json_schema_validate/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(os, [
		delete_file/1, file_exists/1
	]).

	cover(linter_reporter).

	cleanup :-
		^^clean_file('linter_reporter.sarif').

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

	test(lr_report_01, deterministic) :-
		prepare_run(false),
		warnings(Warnings),
		report(atom(Atom)),
		assertion(ground(Atom)),
		json_parse(atom(Atom), SARIF),
		SARIF = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[Run]},
		sarif_run_ok(Run, Rules, _Properties, Results),
		assertion(Rules \== []),
		length(Warnings, Count),
		assertion(length(Results, Count)).

	test(lr_report_02, deterministic) :-
		prepare_run(true),
		^^file_path('linter_reporter.sarif', ReportFile),
		report(file(ReportFile)),
		assertion(os::file_exists(ReportFile)),
		report(atom(Atom)),
		json_parse(atom(Atom), SARIF),
		SARIF = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[Run]},
		sarif_run_ok(Run, _Rules, _Properties, Results),
		assertion(Results \== []).

	test(lr_report_schema_01, deterministic) :-
		prepare_run(false),
		report(atom(Atom)),
		json_parse(atom(Atom), SARIF),
		validate_sarif(SARIF).

	test(lr_report_schema_02, deterministic) :-
		prepare_run(true),
		^^file_path('linter_reporter.sarif', ReportFile),
		report(file(ReportFile)),
		json_parse(file(ReportFile), SARIF),
		validate_sarif(SARIF).

	test(lr_summary_01, deterministic) :-
		prepare_run(false),
		summary(summary(TotalWarnings, RuleCounts, FlagCounts)),
		assertion(TotalWarnings > 0),
		assertion(member(rule_count(singleton_variables, _), RuleCounts)),
		assertion(member(flag_count(singleton_variables, _), FlagCounts)).

	test(lr_warning_01, true) :-
		prepare_run(true),
		forall(
			warning(linter_warning(_Flag, _RuleId, _File, _Lines, _Context, Properties)),
			(	assertion(ground(Properties)),
				assertion(member(message(_), Properties))
			)
		).

	prepare_run(Explainations) :-
		^^file_path('linter_reporter.sarif', ReportFile),
		^^clean_file(ReportFile),
		{set_logtalk_flag(linter_reporter_file, ReportFile)},
		{set_logtalk_flag(linter_reporter_include_explanations, Explainations)},
		reset,
		enable,
		logtalk_load([errors(warnings), errors(main_include_compiler_warning)], [reload(always)]),
		disable.

	% not all warnings have explanations
	warning_with_explanation(Warnings) :-
		member(linter_warning(_Flag, _RuleId, _File, _Lines, _Context, Properties), Warnings),
		memberchk(explanation(_), Properties),
		!.

	validate_sarif(SARIF) :-
		^^file_path('sarif-schema-2.1.0.json', Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, SARIF).

	sarif_run_ok({
		tool-{driver-{name-linter_reporter, informationUri-'https://logtalk.org/', version-'0.1.0', rules-Rules}},
		automationDetails-{id-linter_reporter, guid-_},
		properties-Properties,
		results-Results
	}, Rules, Properties, Results).

:- end_object.
