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
	logtalk_load(basic_types(loader)),
	logtalk_load(json_rpc(loader)),
	logtalk_load(options(loader)),
	logtalk_load(term_io(loader)),
	logtalk_load(http_server(loader)),
	logtalk_load([
		mcp_tool_protocol,
		mcp_prompt_protocol,
		mcp_resource_protocol,
		mcp_multiround_protocol,
		mcp_cache_protocol,
		mcp_ui_protocol,
		mcp_server_adapter_protocol,
		mcp_server_spec_protocol,
		mcp_server_application,
		mcp_server_2025_06_18_spec,
		mcp_server_2025_11_25_spec,
		mcp_server_2026_07_28_spec,
		mcp_server_stdio_transport,
		mcp_server_2025_06_18_adapter,
		mcp_server_2026_07_28_adapter,
		mcp_server
	], [
		optimize(on)
	])
)).

:- if(current_logtalk_flag(threads, supported)).

	:- initialization((
		logtalk_load(format(loader)),
		logtalk_load(http_server(loader)),
		logtalk_load([
			mcp_server_streamable_http_transport
		], [
			optimize(on)
		])
	)).

:- endif.
