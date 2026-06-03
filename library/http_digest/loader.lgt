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
	logtalk_load(crypto(loader)),
	logtalk_load(dates(loader)),
	logtalk_load(hashes(loader)),
	logtalk_load(hmac(loader)),
	logtalk_load(options(loader)),
	logtalk_load(http_core(loader)),
	logtalk_load([http_digest_verifier_protocol, http_digest, http_server_digest_handler], [optimize(on)])
)).

:- if((
	current_logtalk_flag(sockets, supported),
	current_prolog_flag(bounded, false)
)).

	:- initialization((
		logtalk_load(http_cookies(loader)),
		logtalk_load(http_client(loader)),
		logtalk_load(http_session(http_cookie_jar), [optimize(on)]),
		logtalk_load(http_client_digest_session, [optimize(on)])
	)).

:- endif.
