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
	(	Dialect == eclipse; Dialect == gnu;
		Dialect == sicstus; Dialect == swi;
		Dialect == trealla,
		current_prolog_flag(version_data, trealla(Major, Minor, Patch, _)),
		v(Major, Minor, Patch) @>= v(2, 90, 3);
		Dialect == xvm
	)
)).

	:- initialization((
		set_logtalk_flag(report, warnings),
		logtalk_load(lgtunit(loader)),
		logtalk_load(dates(loader)),
		logtalk_load(ids(loader)),
		logtalk_load(options(loader)),
		logtalk_load(http_client(loader)),
		logtalk_load(http_cookies(loader)),
		logtalk_load(http_router(loader)),
		logtalk_load([
			http_cookie_jar,
			http_client_session,
			http_server_session,
			http_server_session_handler,
			http_router_server_session
		], [
			debug(on),
			source_data(on)
		]),
		logtalk_load(test_objects),
		logtalk_load(tests, [hook(lgtunit)]),
		tests::run
	)).

:- else.

	:- initialization((
		write('(not applicable)'), nl
	)).

:- endif.
