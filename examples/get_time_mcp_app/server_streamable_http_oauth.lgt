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


%  Entry point for running the OAuth-protected get_time MCP App example.
%  Streamable HTTP (MCP 2026-07-28 protocol semantics, HTTPS transport).
%  Listens on https://127.0.0.1:8443/mcp by default.

:- initialization((
	logtalk_load(loader),
	mcp_server::start(get_time, get_time, [
		spec('2026-07-28'),
		transport(streamable_http),
		server_title('OAuth-protected get time MCP App demo (2026-07-28)'),
		http_port(8443),
		http_bind('127.0.0.1'),
		http_path('/mcp'),
		http_origin_check(false),
		http_server_options([
			scheme(https),
			temporary_tls_credentials('get_time_mcp_')
		]),
		oauth(
			get_time_oauth_verifier,
			'https://127.0.0.1:8443/mcp',
			[
				authorization_servers(['https://identity.example.com']),
				scopes_supported([get_time]),
				resource_name('Get time MCP App demo')
			],
			[required_scopes([get_time])]
		)
	])
)).
