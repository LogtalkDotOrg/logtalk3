%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2021 Paulo Moura <pmoura@logtalk.org>
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


:- if(current_prolog_flag(bounded, true)).

	:- initialization((write('(not applicable)'), nl)).

:- else.

	:- if(\+ current_logtalk_flag(encoding_directive, unsupported)).

		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(basic_types(loader)),
			logtalk_load(cbor, [source_data(on), debug(on)]),
			logtalk_load(lgtunit(loader)),
			logtalk_load([tests_common, tests_utf_8], [hook(lgtunit)]),
			lgtunit::run_test_sets([
				tests_common,
				tests_utf_8
			])
		)).

	:- else.

		:- initialization((
			set_logtalk_flag(report, warnings),
			logtalk_load(basic_types(loader)),
			logtalk_load(cbor, [source_data(on), debug(on)]),
			logtalk_load(lgtunit(loader)),
			logtalk_load(tests_common, [hook(lgtunit)]),
			tests_common::run
		)).

	:- endif.

:- endif.
