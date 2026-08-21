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


:- object(mcp_server_2025_06_18_adapter,
	implements(mcp_server_adapter_protocol)).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-08-21,
		comment is 'Legacy MCP 2025-06-18 stdio transport adapter for backwards compatibility. Delegates protocol handling to ``mcp_server_2025_06_18_spec`` and renders ``reply/1`` outcomes on the output stream.'
	]).

	:- uses(json_rpc, [
		write_message/2, read_message/2
	]).

	spec('2025-06-18').

	start(Application, Input, Output, UserOptions) :-
		Transport = [stdio_input(Input), stdio_output(Output)| UserOptions],
		mcp_server_2025_06_18_spec::prepare(Application, Transport),
		server_loop(Input, Output, Transport),
		cleanup.

	notify(Event) :-
		mcp_server_2025_06_18_spec::notify(Event).

	cleanup :-
		mcp_server_2025_06_18_spec::cleanup.

	server_loop(Input, Output, Transport) :-
		(	catch(
				read_message(Input, Message),
				Error,
				(writeq(user_error, Error), nl(user_error), fail)
			) ->
			mcp_server_2025_06_18_spec::handle_message(Message, Transport, Outcome),
			render_outcome(Outcome, Output),
			server_loop(Input, Output, Transport)
		;	true
		).

	render_outcome(reply(Response), Output) :-
		!,
		write_message(Output, Response).
	render_outcome(accepted, _) :-
		!.
	render_outcome(no_reply, _) :-
		!.
	render_outcome(_, _).

:- end_object.
