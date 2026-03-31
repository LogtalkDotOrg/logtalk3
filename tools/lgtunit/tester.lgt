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


:- initialization((
	set_logtalk_flag(report, warnings),
	set_logtalk_flag(optimize, on),
	logtalk_load(loader),
	logtalk_load([
		'test_files/diagnostics_fixture',
		'test_files/tests_diagnostics',
		'test_files/tests_parametric',
		'test_files/tests_dialects',
		'test_files/tests_skipped',
		'test_files/tests_selected',
		'test_files/tests_utils',
		'test_files/tests_io_predicates'
	], [
		hook(lgtunit)
	]),
	lgtunit::run_test_sets([
		tests_diagnostics,
		tests_parametric(1),
		tests_dialects,
		tests_skipped,
		tests_selected,
		tests_utils,
		tests_io_predicates
	])
)).
