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


:- object(mcp_server_stdio_transport,
	implements(mcp_server_adapter_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-21,
		comment is 'Stdio transport adapter for MCP. Reads newline-delimited JSON-RPC from the input stream and writes responses to the output stream. Supports specs 2025-06-18 and 2026-07-28 selected via the ``spec/1`` option and delegated to ``mcp_server_2025_06_18_spec`` or ``mcp_server_2026_07_28_spec``.'
	]).

	:- uses(json_rpc, [
		write_message/2, read_message/2
	]).
	:- uses(list, [
		member/2, append/3
	]).

	:- private(active_protocol_/1).
	:- dynamic(active_protocol_/1).
	:- mode(active_protocol_(-object_identifier), zero_or_one).
	:- info(active_protocol_/1, [
		comment is 'Protocol handler object selected for the current session.',
		argnames is ['Protocol']
	]).

	spec(Version) :-
		(	active_protocol_(Protocol) ->
			Protocol::spec(Version)
		;	Version = '2025-06-18'
		).

	start(Application, Input, Output, UserOptions) :-
		select_protocol(UserOptions, Protocol),
		retractall(active_protocol_(_)),
		assertz(active_protocol_(Protocol)),
		Options = [stdio_input(Input), stdio_output(Output)| UserOptions],
		Protocol::prepare(Application, Options),
		(	catch(Application::capabilities(Capabilities), _, fail) -> true
		;	Capabilities = []
		),
		% Session options first so member/2 prefers them over trailing defaults
		append(
			[
				application(Application),
				application_capabilities(Capabilities),
				stdio_input(Input),
				stdio_output(Output)
				| UserOptions
			],
			[instructions(''), cache_ttl(0), cache_scope(private)],
			LoopOptions
		),
		catch(
			run_loop(Protocol, Input, Output, LoopOptions),
			Error,
			(cleanup, throw(Error))
		),
		cleanup.

	notify(Event) :-
		(	active_protocol_(Protocol) ->
			Protocol::notify(Event)
		;	true
		).

	cleanup :-
		(	retract(active_protocol_(Protocol)) ->
			catch(Protocol::cleanup, _, true)
		;	true
		).

	select_protocol(Options, Protocol) :-
		(	member(spec('2026-07-28'), Options) ->
			Version = '2026-07-28'
		;	member(spec('2025-06-18'), Options) ->
			Version = '2025-06-18'
		;	member(spec(Version), Options) ->
			true
		;	Version = '2025-06-18'
		),
		(	protocol_object(Version, Protocol) ->
			true
		;	throw(error(domain_error(protocol_version, Version), mcp_server_stdio_transport::start/4))
		).

	protocol_object('2025-06-18', mcp_server_2025_06_18_spec).
	protocol_object('2026-07-28', mcp_server_2026_07_28_spec).

	run_loop(mcp_server_2026_07_28_spec, Input, Output, Options) :-
		!,
		mcp_server_2026_07_28_spec::run_stdio_loop(Input, Output, Options).
	run_loop(Protocol, Input, Output, Options) :-
		server_loop(Protocol, Input, Output, Options).

	server_loop(Protocol, Input, Output, Options) :-
		(	catch(
				read_message(Input, Message),
				Error,
				(writeq(user_error, Error), nl(user_error), fail)
			) ->
			Protocol::handle_message(Message, Options, Outcome),
			render_outcome(Outcome, Output),
			server_loop(Protocol, Input, Output, Options)
		;	true
		).

	render_outcome(reply(Response), Output) :-
		!,
		write_message(Output, Response).
	render_outcome(reply_with_progress(Events, Final), Output) :-
		!,
		write_events(Events, Output),
		write_message(Output, Final).
	render_outcome(subscribe(_SubId, _Filters, Messages), Output) :-
		!,
		write_events(Messages, Output).
	render_outcome(accepted, _) :-
		!.
	render_outcome(no_reply, _) :-
		!.
	render_outcome(_, _).

	write_events([], _).
	write_events([Event| Events], Output) :-
		write_message(Output, Event),
		write_events(Events, Output).

:- end_object.
