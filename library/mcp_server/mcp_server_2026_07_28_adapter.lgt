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


:- object(mcp_server_2026_07_28_adapter,
	implements(mcp_server_adapter_protocol)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-08-26,
		comment is 'Legacy MCP 2026-07-28 stdio transport adapter for backwards compatibility. Delegates protocol handling to ``mcp_server_2026_07_28_spec``.'
	]).

	spec('2026-07-28').

	start(Application, Input, Output, UserOptions) :-
		Options = [stdio_input(Input), stdio_output(Output)| UserOptions],
		mcp_server_2026_07_28_spec::prepare(Application, Options),
		(	conforms_to_protocol(Application, mcp_tool_protocol),
			Application::capabilities(Capabilities) ->
			true
		;	Capabilities = []
		),
		LoopOptions = [
			application(Application),
			application_capabilities(Capabilities),
			stdio_input(Input),
			stdio_output(Output)
			| UserOptions
		],
		catch(
			mcp_server_2026_07_28_spec::run_stdio_loop(Input, Output, LoopOptions),
			Error,
			(cleanup, throw(Error))
		),
		cleanup.

	notify(Event) :-
		mcp_server_2026_07_28_spec::notify(Event).

	cleanup :-
		mcp_server_2026_07_28_spec::cleanup.

:- end_object.
