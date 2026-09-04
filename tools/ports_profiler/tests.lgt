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
		date is 2026-09-04,
		comment is 'Unit tests for the "ports_profiler" tool.'
	]).

	:- uses(ports_profiler, [
		start/0, stop/0,
		data/0, data/1, port/5,
		reset/0, reset/1,
		diagnostics_tool/5, diagnostic_rule/5, diagnostic_rules/1,
		diagnostic/2, diagnostic/3, diagnostics/2, diagnostics/3,
		diagnostics_summary/2, diagnostics_preflight/2, diagnostics_preflight/3
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(os, [
		path_concat/3
	]).

	setup :-
		start,
		foo::solutions,
		mode_cases::run.

	cleanup :-
		stop.

	test(ports_profiler_data_0_01, deterministic) :-
		^^suppress_text_output,
		data.

	test(ports_profiler_data_1_01, deterministic) :-
		^^suppress_text_output,
		data(foo).

	test(ports_profiler_data_1_02, deterministic) :-
		^^suppress_text_output,
		data(non_existent).

	test(ports_profiler_port_5_01, true(Functor/Arity-Count == solutions/0-1)) :-
		port(fact, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_02, true(Functor/Arity-Count == solutions/0-1)) :-
		port(rule, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_03, true(Functor/Arity-Count == solutions/0-1)) :-
		port(call, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_04, true(Functor/Arity-Count == solutions/0-1)) :-
		port(exit, foo, Functor, Arity, Count).

	test(ports_profiler_port_5_05, true(Data == [baz/1-2, qux/1-6])) :-
		setof(Functor/Arity-Count, port(fact, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_06, true(Data == [bar/2-1, baz/1-1, qux/1-2])) :-
		setof(Functor/Arity-Count, port(call, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_07, true(Data == [bar/2-1, baz/1-1, qux/1-2])) :-
		setof(Functor/Arity-Count, port(exit, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_08, true(Data == [bar/2-5, baz/1-1, qux/1-4])) :-
		setof(Functor/Arity-Count, port(nd_exit, bar, Functor, Arity, Count), Data).

	test(ports_profiler_port_5_09, true(Data == [bar/2-5, baz/1-1, qux/1-4])) :-
		setof(Functor/Arity-Count, port(redo, bar, Functor, Arity, Count), Data).

	test(ports_profiler_diagnostics_tool_5_01, deterministic) :-
		diagnostics_tool(ports_profiler, ports_profiler, '3.0.0', 'https://logtalk.org/', Properties),
		assertion(member(guid(_), Properties)),
		assertion(member(fingerprint_algorithm(canonical_finding_v1), Properties)).

	test(ports_profiler_diagnostic_rules_1_01, deterministic) :-
		diagnostic_rules(Rules),
		assertion(
			subsumes_term(
				[diagnostic_rule(unexpected_non_determinism, _, _, warning, [_])],
				Rules
			)
		).

	test(ports_profiler_diagnostic_2_01, true(Count == 7)) :-
		findall(Diagnostic, diagnostic(entity(mode_cases), Diagnostic), Diagnostics),
		length(Diagnostics, Count).

	test(ports_profiler_diagnostic_3_01, true(Count == 7)) :-
		findall(Diagnostic, diagnostic(entity(mode_cases), Diagnostic, [explanations(true)]), Diagnostics),
		length(Diagnostics, Count).

	test(ports_profiler_diagnostics_2_01, deterministic) :-
		diagnostics(entity(bar), Diagnostics),
		assertion(member(diagnostic(unexpected_non_determinism, warning, high, _, context(object, bar), _, _, Properties), Diagnostics)),
		assertion(member(predicate(bar/2), Properties)),
		assertion(member(port_count(fact, 0), Properties)),
		assertion(member(port_count(rule, 1), Properties)),
		assertion(member(port_count(call, 1), Properties)),
		assertion(member(port_count(exit, 1), Properties)),
		assertion(member(port_count(nd_exit, 5), Properties)),
		assertion(member(port_count(fail, 0), Properties)),
		assertion(member(port_count(redo, 5), Properties)),
		assertion(member(port_count(exception, 0), Properties)).

	test(ports_profiler_diagnostics_2_02, deterministic) :-
		diagnostics(entity(mode_cases), Diagnostics),
		findall(Predicate, (member(diagnostic(unexpected_non_determinism, warning, high, _, _, _, _, Properties), Diagnostics), member(predicate(Predicate), Properties)), Predicates),
		assertion(Predicates == [
			det_zero/1,
			det_zero_or_one/1,
			det_one/1,
			det_zero_or_error/1,
			det_one_or_error/1,
			det_zero_or_one_or_error/1,
			det_error/1
		]).

	test(ports_profiler_diagnostics_3_01, true(Count == 1)) :-
		diagnostics(entity(bar), Diagnostics, [explanations(false)]),
		length(Diagnostics, Count).

	test(ports_profiler_diagnostics_non_terminal_01, deterministic) :-
		once(bar::parse),
		diagnostics(entity(bar), Diagnostics),
		assertion(member(diagnostic(unexpected_non_determinism, warning, high, _, _, _, _, Properties), Diagnostics)),
		assertion(member(predicate(token//1), Properties)),
		assertion(member(compiled_predicate(token/3), Properties)).

	test(ports_profiler_diagnostics_targets_01, deterministic) :-
		this(This),
		object_property(This, file(_, Directory)),
		diagnostics(entity(bar), EntityDiagnostics),
		path_concat(Directory, test_entities, FilePath),
		diagnostics(file(FilePath), FileDiagnostics),
		diagnostics(directory(Directory), DirectoryDiagnostics),
		assertion(EntityDiagnostics \== []),
		assertion(FileDiagnostics \== []),
		assertion(DirectoryDiagnostics == FileDiagnostics).

	test(ports_profiler_diagnostics_summary_2_01, deterministic) :-
		diagnostics_summary(entity(bar), Summary),
		assertion(Summary == diagnostics_summary(entity(bar), 1, 2, diagnostic_breakdown([rule_count(unexpected_non_determinism, 2)], [severity_count(warning, 2)], [confidence_count(high, 2)]), [context_summary(context(object, bar), 2, diagnostic_breakdown([rule_count(unexpected_non_determinism, 2)], [severity_count(warning, 2)], [confidence_count(high, 2)]))])).

	test(ports_profiler_diagnostics_preflight_2_01, deterministic) :-
		diagnostics_preflight(entity(non_existent), Issues),
		assertion(
			subsumes_term(
				[preflight_issue(no_profiling_data, warning, _, context(tool, ports_profiler), '', 0-0, [prerequisite(profiling_data)])],
				Issues
			)
		).

	test(ports_profiler_diagnostics_preflight_3_01, deterministic) :-
		diagnostics_preflight(entity(foo), Issues, [explanations(false)]),
		assertion(Issues == []).

	test(ports_profiler_diagnostics_preflight_3_02, deterministic) :-
		diagnostics_preflight(entity(ports_profiler), Issues),
		assertion(member(preflight_issue(missing_debug_mode, warning, _, context(object, ports_profiler), _, 0-0, [prerequisite(debug)]), Issues)),
		assertion(member(preflight_issue(no_profiling_data, warning, _, context(tool, ports_profiler), '', 0-0, [prerequisite(profiling_data)]), Issues)).

	test(ports_profiler_reset_0_01, deterministic) :-
		reset.

	test(ports_profiler_reset_1_01, deterministic) :-
		reset(foo).

	test(ports_profiler_reset_1_02, deterministic) :-
		reset(non_existent).

:- end_object.
