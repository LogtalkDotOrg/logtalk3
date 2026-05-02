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


:- if(current_logtalk_flag(prolog_dialect, xvm)).
	:- set_prolog_flag(enable_occurs_check, false).
:- endif.


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(dead_code_scanner(loader)),
	logtalk_load(dead_code_scanner(test_entities), [source_data(on), unknown_entities(silent)]),
	logtalk_load(linter_reporter(loader)),
	logtalk_load(lgtdoc(loader)),
	logtalk_load(lgtdoc(diagnostics_fixture), [source_data(on)]),
	logtalk_load(tool_diagnostics(loader)),
	logtalk_load(git(loader)),
	logtalk_load(json(loader)),
	logtalk_load(json_schema(loader)),
	logtalk_load(os(loader)),
	logtalk_load(reader(loader)),
	logtalk_load(term_io(loader)),
	logtalk_load(url(loader)),
	logtalk_load(uuid(loader)),
	logtalk_load(sarif, [debug(on), source_data(on)]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(lgtunit('test_files/diagnostics_fixture'), [hook(lgtunit)]),
	logtalk_load(tests, [hook(lgtunit), optimize(on)]),
	tests::run
)).
