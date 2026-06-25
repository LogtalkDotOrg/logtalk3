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
	current_logtalk_flag(sockets, supported),
	current_prolog_flag(bounded, false)
)).

	:- initialization((
		logtalk_load(yaml(loader)),
		logtalk_load(open_api(loader)),
		logtalk_load(rest(loader)),
		logtalk_load(http_client(loader)),
		logtalk_load(http_websocket(loader)),
		logtalk_load([
			open_ai_catalog,
			open_ai_protocols,
			open_ai_api,
			open_ai_event_stream,
			open_ai_client,
			open_ai_server,
			open_ai
		], [
			optimize(on)
		])
	)).

:- else.

	:- initialization((write('(open_ai library not available for your backend Prolog compiler)'), nl)).

:- endif.

