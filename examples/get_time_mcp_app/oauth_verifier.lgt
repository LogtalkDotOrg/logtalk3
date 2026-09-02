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


:- object(get_time_oauth_verifier,
	implements(http_oauth_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Example',
		date is 2026-09-02,
		comment is 'Fixed-token OAuth verifier for the protected get-time MCP example. For demonstration only.'
	]).

	verify('get-time-demo-token', 'https://127.0.0.1:8443/mcp', oauth_token_info([
		source(example),
		scopes([get_time]),
		audience_validation(exact('https://127.0.0.1:8443/mcp')),
		claims([subject(example_client)])
	])).

:- end_object.
