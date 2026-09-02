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


:- object(mcp_server_test_oauth_verifier,
	implements(http_oauth_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'OAuth verifier used by the MCP Streamable HTTP transport tests.'
	]).

	verify('read-token', 'https://api.example.com/mcp', oauth_token_info([
		source(test),
		scopes([read]),
		audience_validation(exact('https://api.example.com/mcp')),
		claims([subject(alice)])
	])).

	verify('write-token', 'https://api.example.com/mcp', oauth_token_info([
		source(test),
		scopes([read, write]),
		audience_validation(exact('https://api.example.com/mcp')),
		claims([subject(alice)])
	])).

:- end_object.
