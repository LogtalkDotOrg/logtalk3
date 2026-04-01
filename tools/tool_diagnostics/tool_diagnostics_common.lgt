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


:- category(tool_diagnostics_common,
	implements(tool_diagnostics_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-01,
		comment is 'Default definitions for diagnostics targets, rule enumeration, and summary/breakdown helpers.'
	]).

	:- protected(context_summaries/2).
	:- mode(context_summaries(+list(compound), -list(compound)), one).
	:- info(context_summaries/2, [
		comment is 'Returns a list of context summary terms for a list of diagnostics.',
		argnames is ['Diagnostics', 'ContextSummaries']
	]).

	:- protected(diagnostics_breakdown/2).
	:- mode(diagnostics_breakdown(+list(compound), -compound), one).
	:- info(diagnostics_breakdown/2, [
		comment is 'Returns a breakdown term with rule, severity, and confidence counts for a list of diagnostics.',
		argnames is ['Diagnostics', 'Breakdown']
	]).

	:- uses(list, [
		length/2, member/2
	]).

	diagnostic_target(all).
	diagnostic_target(entity(_)).
	diagnostic_target(file(_)).
	diagnostic_target(directory(_)).
	diagnostic_target(rdirectory(_)).
	diagnostic_target(library(_)).
	diagnostic_target(rlibrary(_)).

	diagnostic_rules(Rules) :-
		findall(
			diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
			::diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
			Rules
		).

	context_summaries(Diagnostics, ContextSummaries) :-
		(	setof(Context, RuleId^Severity^Confidence^Message^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), Contexts) ->
			context_summaries(Contexts, Diagnostics, ContextSummaries)
		;	ContextSummaries = []
		).

	context_summaries([], _Diagnostics, []).
	context_summaries([Context| Contexts], Diagnostics, [context_summary(Context, DiagnosticsCount, Breakdown)| ContextSummaries]) :-
		context_diagnostics(Diagnostics, Context, ContextDiagnostics),
		length(ContextDiagnostics, DiagnosticsCount),
		diagnostics_breakdown(ContextDiagnostics, Breakdown),
		context_summaries(Contexts, Diagnostics, ContextSummaries).

	context_diagnostics([], _Context, []).
	context_diagnostics([Diagnostic| Diagnostics], Context, ContextDiagnostics) :-
		(	Diagnostic = diagnostic(_, _, _, _, Context, _, _, _) ->
			ContextDiagnostics = [Diagnostic| Rest]
		;	ContextDiagnostics = Rest
		),
		context_diagnostics(Diagnostics, Context, Rest).

	diagnostics_breakdown(Diagnostics, diagnostic_breakdown(RuleCounts, SeverityCounts, ConfidenceCounts)) :-
		diagnostic_rule_counts(Diagnostics, RuleCounts),
		diagnostic_severity_counts(Diagnostics, SeverityCounts),
		diagnostic_confidence_counts(Diagnostics, ConfidenceCounts).

	diagnostic_rule_counts(Diagnostics, RuleCounts) :-
		(	setof(RuleId, Severity^Confidence^Message^Context^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), RuleIds) ->
			diagnostic_rule_counts(RuleIds, Diagnostics, RuleCounts)
		;	RuleCounts = []
		).

	diagnostic_rule_counts([], _Diagnostics, []).
	diagnostic_rule_counts([RuleId| RuleIds], Diagnostics, [rule_count(RuleId, Count)| RuleCounts]) :-
		diagnostic_rule_count(Diagnostics, RuleId, 0, Count),
		diagnostic_rule_counts(RuleIds, Diagnostics, RuleCounts).

	diagnostic_rule_count([], _RuleId, Count, Count).
	diagnostic_rule_count([diagnostic(RuleId, _Severity, _Confidence, _Message, _Context, _File, _Lines, _Properties)| Diagnostics], RuleId, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		diagnostic_rule_count(Diagnostics, RuleId, Count1, Count).
	diagnostic_rule_count([_| Diagnostics], RuleId, Count0, Count) :-
		diagnostic_rule_count(Diagnostics, RuleId, Count0, Count).

	diagnostic_severity_counts(Diagnostics, SeverityCounts) :-
		(	setof(Severity, RuleId^Confidence^Message^Context^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), Severities) ->
			diagnostic_severity_counts(Severities, Diagnostics, SeverityCounts)
		;	SeverityCounts = []
		).

	diagnostic_severity_counts([], _Diagnostics, []).
	diagnostic_severity_counts([Severity| Severities], Diagnostics, [severity_count(Severity, Count)| SeverityCounts]) :-
		diagnostic_severity_count(Diagnostics, Severity, 0, Count),
		diagnostic_severity_counts(Severities, Diagnostics, SeverityCounts).

	diagnostic_severity_count([], _Severity, Count, Count).
	diagnostic_severity_count([diagnostic(_RuleId, Severity, _Confidence, _Message, _Context, _File, _Lines, _Properties)| Diagnostics], Severity, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		diagnostic_severity_count(Diagnostics, Severity, Count1, Count).
	diagnostic_severity_count([_| Diagnostics], Severity, Count0, Count) :-
		diagnostic_severity_count(Diagnostics, Severity, Count0, Count).

	diagnostic_confidence_counts(Diagnostics, ConfidenceCounts) :-
		(	setof(Confidence, RuleId^Severity^Message^Context^File^Lines^Properties^member(diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties), Diagnostics), Confidences) ->
			diagnostic_confidence_counts(Confidences, Diagnostics, ConfidenceCounts)
		;	ConfidenceCounts = []
		).

	diagnostic_confidence_counts([], _Diagnostics, []).
	diagnostic_confidence_counts([Confidence| Confidences], Diagnostics, [confidence_count(Confidence, Count)| ConfidenceCounts]) :-
		diagnostic_confidence_count(Diagnostics, Confidence, 0, Count),
		diagnostic_confidence_counts(Confidences, Diagnostics, ConfidenceCounts).

	diagnostic_confidence_count([], _Confidence, Count, Count).
	diagnostic_confidence_count([diagnostic(_RuleId, _Severity, Confidence, _Message, _Context, _File, _Lines, _Properties)| Diagnostics], Confidence, Count0, Count) :-
		!,
		Count1 is Count0 + 1,
		diagnostic_confidence_count(Diagnostics, Confidence, Count1, Count).
	diagnostic_confidence_count([_| Diagnostics], Confidence, Count0, Count) :-
		diagnostic_confidence_count(Diagnostics, Confidence, Count0, Count).

:- end_category.
