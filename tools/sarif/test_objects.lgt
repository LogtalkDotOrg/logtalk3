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


:- object(sarif_provenance_fixture,
	imports(tool_diagnostics_common),
	implements(tool_diagnostics_protocol)).

	:- uses(list, [
		length/2, member/2
	]).

	diagnostics_tool(sarif_provenance_fixture, sarif_provenance_fixture, '1.0.0', 'https://logtalk.org/', [
		count_key(diagnosticsCount),
		include_version_control_provenance(true)
	]).

	diagnostic_rule(mock_rule, 'Mock rule.', 'Mock rule for version control provenance regression tests.', warning, []).

	diagnostics(Target, Diagnostics, []) :-
		findall(Diagnostic, target_diagnostic(Target, Diagnostic), Diagnostics).

	diagnostics(Target, Diagnostics) :-
		diagnostics(Target, Diagnostics, []).

	diagnostic(Target, Diagnostic, []) :-
		target_diagnostic(Target, Diagnostic).

	diagnostic(Target, Diagnostic) :-
		diagnostic(Target, Diagnostic, []).

	diagnostics_summary(Target, diagnostics_summary(Target, TotalContexts, TotalDiagnostics, Breakdown, ContextSummaries), []) :-
		diagnostics(Target, Diagnostics, []),
		length(Diagnostics, TotalDiagnostics),
		^^diagnostics_breakdown(Diagnostics, Breakdown),
		^^context_summaries(Diagnostics, ContextSummaries),
		length(ContextSummaries, TotalContexts).

	diagnostics_summary(Target, Summary) :-
		diagnostics_summary(Target, Summary, []).

	diagnostics_preflight(_Target, [], []).

	diagnostics_preflight(Target, Issues) :-
		diagnostics_preflight(Target, Issues, []).

	target_diagnostic(file(File), diagnostic(mock_rule, warning, not_applicable, 'Mock provenance diagnostic.', context(file, File), File, 1-1, [])).

	diagnostic_target(file(_)).

	diagnostic_rules(Rules) :-
		findall(
			diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
			diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
			Rules
		).

:- end_object.


:- object(sarif_windows_path_fixture,
	imports(tool_diagnostics_common),
	implements(tool_diagnostics_protocol)).

	:- uses(list, [
		length/2, member/2
	]).

	diagnostics_tool(sarif_windows_path_fixture, sarif_windows_path_fixture, '1.0.0', 'https://logtalk.org/', [
		count_key(diagnosticsCount)
	]).

	diagnostic_rule(mock_rule, 'Mock rule.', 'Mock rule for Windows path URI regression tests.', warning, []).

	diagnostics(Target, Diagnostics, []) :-
		findall(Diagnostic, target_diagnostic(Target, Diagnostic), Diagnostics).

	diagnostics(Target, Diagnostics) :-
		diagnostics(Target, Diagnostics, []).

	diagnostic(Target, Diagnostic, []) :-
		target_diagnostic(Target, Diagnostic).

	diagnostic(Target, Diagnostic) :-
		diagnostic(Target, Diagnostic, []).

	diagnostics_summary(Target, diagnostics_summary(Target, TotalContexts, TotalDiagnostics, Breakdown, ContextSummaries), []) :-
		diagnostics(Target, Diagnostics, []),
		length(Diagnostics, TotalDiagnostics),
		^^diagnostics_breakdown(Diagnostics, Breakdown),
		^^context_summaries(Diagnostics, ContextSummaries),
		length(ContextSummaries, TotalContexts).

	diagnostics_summary(Target, Summary) :-
		diagnostics_summary(Target, Summary, []).

	diagnostics_preflight(_Target, [], []).

	diagnostics_preflight(Target, Issues) :-
		diagnostics_preflight(Target, Issues, []).

	target_diagnostic(directory('C:\\Temp\\app'), diagnostic(mock_rule, warning, not_applicable, 'Mock Windows drive diagnostic.', context(file, 'C:\\Temp\\app\\src\\file.lgt'), 'C:\\Temp\\app\\src\\file.lgt', 1-1, [])).
	target_diagnostic(directory('\\\\server\\share\\app'), diagnostic(mock_rule, warning, not_applicable, 'Mock Windows UNC diagnostic.', context(file, '\\\\server\\share\\app\\src\\file.lgt'), '\\\\server\\share\\app\\src\\file.lgt', 1-1, [])).

	diagnostic_target(directory(_)).

	diagnostic_rules(Rules) :-
		findall(
			diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
			diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
			Rules
		).

:- end_object.
