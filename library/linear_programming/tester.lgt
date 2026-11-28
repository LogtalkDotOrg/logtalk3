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


:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(	Dialect == eclipse
	;	Dialect == gnu
	;	Dialect == sicstus
	;	Dialect == shumway
	;	Dialect == swi
	;	Dialect == trealla
	;	Dialect == xvm
	)
)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(types(loader)),
		logtalk_load(character_sets(loader)),
		logtalk_load(format(loader)),
		logtalk_load(options(loader)),
		logtalk_load(linear_algebra(loader)),
		logtalk_load(os(loader)),
		logtalk_load(process(loader)),
		logtalk_load(reader(loader)),
		logtalk_load([
			lp_format,
			linear_programming_protocol,
			linear_programming_types,
			linear_programming_common,
			linear_programming_process,
			linear_programming_cli,
			cbc,
			glpk,
			highs,
			scip,
			simplex,
			milp_branch_and_bound
		], [
			debug(on),
			source_data(on)
		]),
		logtalk_load(lgtunit(loader)),
		logtalk_load([
			tests,
			lp_format_tests,
			external_tests
		], [
			hook(lgtunit)
		]),
		lgtunit::run_test_sets([
			tests,
			lp_format_tests,
			external_tests(glpk),
			external_tests(cbc),
			external_tests(highs),
			external_tests(scip)
		])
	)).

:- else.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(types(loader)),
		logtalk_load(format(loader)),
		logtalk_load(options(loader)),
		logtalk_load(linear_algebra(loader)),
		logtalk_load([
			linear_programming_protocol,
			linear_programming_types,
			linear_programming_common,
			simplex,
			milp_branch_and_bound
		], [
			debug(on),
			source_data(on)
		]),
		logtalk_load(lgtunit(loader)),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.
