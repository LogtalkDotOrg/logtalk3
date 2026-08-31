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


% entry point for running the factorial MCP server over Streamable HTTP
% (MCP 2026-07-28 protocol semantics, HTTP transport);
% listens on http://127.0.0.1:8080/mcp by default.

:- initialization((
	logtalk_load(loader),
	mcp_server::start(factorial_mcp, factorial_mcp, [
		spec('2025-06-18'),
		transport(streamable_http),
		server_title('Factorial (2025-06-18)'),
		instructions('Compute the factorial of a non-negative integer.'),
		http_port(8080),
		http_bind('127.0.0.1'),
		http_path('/mcp'),
		http_origin_check(false)
	])
)).
