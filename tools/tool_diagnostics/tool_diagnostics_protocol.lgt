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


:- protocol(tool_diagnostics_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-31,
		comment is 'Common machine-readable diagnostics protocol for developer tools.',
		remarks is [
			'Targets and options' - 'Implementing objects should enumerate supported targets using ``diagnostic_target/1`` and predicates taking an ``Options`` argument must accept ``explanations(Boolean)``.',
			'Tool metadata predicate' - 'Tool metadata is exposed using ``diagnostics_tool/5``.',
			'Rule descriptor predicate' - 'Rule descriptors are exposed using ``diagnostic_rule/5`` and collected in stable order using ``diagnostic_rules/1``.',
			'Diagnostic and preflight terms' - 'Findings and analysis prerequisites are represented using ``diagnostic/8`` and ``preflight_issue/7`` terms.',
			'Summary term' - 'Summaries are represented using ``diagnostics_summary/5`` terms with supporting breakdown and count terms.'
		]
	]).

	:- public(diagnostics_tool/5).
	:- mode(diagnostics_tool(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_one).
	:- info(diagnostics_tool/5, [
		comment is 'Returns tool metadata.',
		argnames is ['Id', 'Name', 'Version', 'InformationURI', 'Properties']
	]).

	:- public(diagnostic_target/1).
	:- mode(diagnostic_target(?nonvar), zero_or_more).
	:- info(diagnostic_target/1, [
		comment is 'Enumerates supported diagnostics target patterns.',
		argnames is ['Target']
	]).

	:- public(diagnostic_rule/5).
	:- mode(diagnostic_rule(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(diagnostic_rule/5, [
		comment is 'Enumerates diagnostic rule descriptors.',
		argnames is ['RuleId', 'ShortDescription', 'FullDescription', 'DefaultSeverity', 'Properties']
	]).

	:- public(diagnostic_rules/1).
	:- mode(diagnostic_rules(-list(compound)), one).
	:- info(diagnostic_rules/1, [
		comment is 'Returns all supported diagnostic rule descriptors in a stable order.',
		argnames is ['Rules']
	]).

	:- public(diagnostic/3).
	:- mode(diagnostic(+nonvar, -compound, +list(compound)), zero_or_more).
	:- info(diagnostic/3, [
		comment is 'Enumerates, by backtracking, diagnostics for a target using the given options. Diagnostics are returned using terms of the form ``diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties)``. All implementations must accept the common option ``explanations(Boolean)``.',
		argnames is ['Target', 'Diagnostic', 'Options']
	]).

	:- public(diagnostic/2).
	:- mode(diagnostic(+nonvar, -compound), zero_or_more).
	:- info(diagnostic/2, [
		comment is 'Enumerates, by backtracking, diagnostics for a target using default options. Diagnostics are returned using terms of the form ``diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties)``.',
		argnames is ['Target', 'Diagnostic']
	]).

	:- public(diagnostics/3).
	:- mode(diagnostics(+nonvar, -list(compound), +list(compound)), one).
	:- info(diagnostics/3, [
		comment is 'Returns an ordered set of diagnostics for a target using the given options. Diagnostics are returned using terms of the form ``diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties)``. All implementations must accept the common option ``explanations(Boolean)``.',
		argnames is ['Target', 'Diagnostics', 'Options']
	]).

	:- public(diagnostics/2).
	:- mode(diagnostics(+nonvar, -list(compound)), one).
	:- info(diagnostics/2, [
		comment is 'Returns an ordered set of diagnostics for a target using default options. Diagnostics are returned using terms of the form ``diagnostic(RuleId, Severity, Confidence, Message, Context, File, Lines, Properties)``.',
		argnames is ['Target', 'Diagnostics']
	]).

	:- public(diagnostics_summary/3).
	:- mode(diagnostics_summary(+nonvar, -compound, +list(compound)), one).
	:- info(diagnostics_summary/3, [
		comment is 'Returns a machine-readable summary for a target using the given options. The summary counts diagnostics only and does not include preflight issues. All implementations must accept the common option ``explanations(Boolean)``.',
		argnames is ['Target', 'Summary', 'Options']
	]).

	:- public(diagnostics_summary/2).
	:- mode(diagnostics_summary(+nonvar, -compound), one).
	:- info(diagnostics_summary/2, [
		comment is 'Returns a machine-readable summary for a target using default options. The summary counts diagnostics only and does not include preflight issues.',
		argnames is ['Target', 'Summary']
	]).

	:- public(diagnostics_preflight/3).
	:- mode(diagnostics_preflight(+nonvar, -list(compound), +list(compound)), one).
	:- info(diagnostics_preflight/3, [
		comment is 'Returns an ordered set of machine-readable preflight issues for a target using the given options. Preflight issues are returned using terms of the form ``preflight_issue(Id, Severity, Message, Context, File, Lines, Properties)``. All implementations must accept the common option ``explanations(Boolean)``.',
		argnames is ['Target', 'Issues', 'Options']
	]).

	:- public(diagnostics_preflight/2).
	:- mode(diagnostics_preflight(+nonvar, -list(compound)), one).
	:- info(diagnostics_preflight/2, [
		comment is 'Returns an ordered set of machine-readable preflight issues for a target using default options. Preflight issues are returned using terms of the form ``preflight_issue(Id, Severity, Message, Context, File, Lines, Properties)``.',
		argnames is ['Target', 'Issues']
	]).

:- end_protocol.
