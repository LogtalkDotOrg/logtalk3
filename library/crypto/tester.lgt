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


:- if(current_prolog_flag(bounded, false)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(basic_types(loader)),
		logtalk_load(base64(loader)),
		logtalk_load(hmac(loader)),
		logtalk_load(os(loader)),
		logtalk_load(random(loader)),
		logtalk_load(crypto, [debug(on), source_data(on), complements(restrict)]),
		logtalk_load(xchacha20_poly1305, [debug(on), source_data(on)]),
		logtalk_load(ed25519, [debug(on), source_data(on)]),
		logtalk_load(x25519, [debug(on), source_data(on)]),
		logtalk_load(authenticated_channel, [debug(on), source_data(on)]),
		logtalk_load(lgtunit(loader)),
		logtalk_load([tests, xchacha20_poly1305_tests, ed25519_tests, x25519_tests, authenticated_channel_tests], [hook(lgtunit)]),
		lgtunit::run_test_sets([
			tests,
			xchacha20_poly1305_tests,
			ed25519_tests,
			x25519_tests,
			authenticated_channel_tests
		])
	)).

:- else.

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(basic_types(loader)),
		logtalk_load(base64(loader)),
		logtalk_load(hmac(loader)),
		logtalk_load(os(loader)),
		logtalk_load(random(loader)),
		logtalk_load(crypto, [debug(on), source_data(on)]),
		logtalk_load(lgtunit(loader)),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- endif.
