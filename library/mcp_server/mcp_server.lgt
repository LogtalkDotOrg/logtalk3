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


:- object(mcp_server,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-14,
		comment is 'MCP (Model Context Protocol) server facade for Logtalk applications. Selects a specification adapter (default ``mcp_server_2025_06_18_adapter``) and delegates the server lifecycle to it. Preserves the public ``start/2-5`` entry points. Implements adapter selection via the ``protocol_adapter(Adapter)`` option.',
		remarks is [
			'MCP specifications' - 'Supports the Model Context Protocol 2025-06-18 (default) and 2026-07-28 (via ``protocol_adapter(mcp_server_2026_07_28_adapter)``).',
			'Transport' - 'Uses stdio (standard input/output) with one newline-delimited JSON-RPC message per line as defined by the MCP specification.',
			'Adapter selection' - 'A server instance is pinned to one specification adapter. Existing ``start/2-5`` calls default to the 2025-06-18 adapter, preserving source and wire compatibility.',
			'Options' - 'Common options: ``server_version(Version)``, ``server_title(Title)``, ``protocol_adapter(Adapter)``. Additional options may be interpreted by the chosen adapter.'
		]
	]).

	:- public(start/2).
	:- mode(start(+atom, +object_identifier), one).
	:- info(start/2, [
		comment is 'Starts the MCP server with the given server name and application object using the default 2025-06-18 adapter and default options. Blocks until the client disconnects.',
		argnames is ['Name', 'Application']
	]).

	:- public(start/3).
	:- mode(start(+atom, +object_identifier, +list), one).
	:- info(start/3, [
		comment is 'Starts the MCP server with the given server name, application object, and options. Supported options include ``server_version(Version)``, ``server_title(Title)``, and ``protocol_adapter(Adapter)`` (defaults to ``mcp_server_2025_06_18_adapter``).',
		argnames is ['Name', 'Application', 'Options']
	]).

	:- public(start/4).
	:- mode(start(+atom, +object_identifier, +stream, +stream), one).
	:- info(start/4, [
		comment is 'Starts the MCP server with custom input and output streams using default options and the default adapter.',
		argnames is ['Name', 'Application', 'Input', 'Output']
	]).

	:- public(start/5).
	:- mode(start(+atom, +object_identifier, +stream, +stream, +list), one).
	:- info(start/5, [
		comment is 'Starts the MCP server with custom input and output streams and options.',
		argnames is ['Name', 'Application', 'Input', 'Output', 'Options']
	]).

	:- public(notify/1).
	:- mode(notify(+compound), zero_or_one).
	:- info(notify/1, [
		comment is 'Publishes an application event to the active adapter. Canonical events: ``tools_list_changed``, ``prompts_list_changed``, ``resources_list_changed``, ``resource_updated(URI)``. Delegated to the currently active adapter (if any).',
		argnames is ['Event']
	]).

	:- private(active_adapter_/1).
	:- dynamic(active_adapter_/1).
	:- mode(active_adapter_(?object_identifier), zero_or_one).
	:- info(active_adapter_/1, [
		comment is 'Currently active adapter object.',
		argnames is ['Adapter']
	]).

	:- uses(list, [member/2]).

	% Public entry points

	start(Name, Application) :-
		start(Name, Application, []).

	start(Name, Application, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		normalize_options(Name, Options0, Options),
		current_input(Input),
		current_output(Output),
		run_adapter(Application, Input, Output, Options).

	start(Name, Application, Input, Output) :-
		start(Name, Application, Input, Output, []).

	start(Name, Application, Input, Output, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		normalize_options(Name, Options0, Options),
		run_adapter(Application, Input, Output, Options).

	notify(Event) :-
		(	active_adapter_(Adapter) ->
			Adapter::notify(Event)
		;	true
		).

	% Adapter selection and lifecycle

	run_adapter(Application, Input, Output, Options) :-
		select_adapter(Options, Adapter),
		retractall(active_adapter_(_)),
		assertz(active_adapter_(Adapter)),
		(	catch(
				Adapter::start(Application, Input, Output, Options),
				Error,
				(cleanup_adapter(Adapter), throw(Error))
			) ->
			cleanup_adapter(Adapter)
		;	cleanup_adapter(Adapter)
		).

	select_adapter(Options, Adapter) :-
		(	member(protocol_adapter(Adapter0), Options) ->
			(	conforms_to_protocol(Adapter0, mcp_server_adapter_protocol) ->
				Adapter = Adapter0
			;	throw(error(domain_error(mcp_server_adapter, Adapter0), mcp_server::start/5))
			)
		;	Adapter = mcp_server_2025_06_18_adapter
		).

	cleanup_adapter(Adapter) :-
		retractall(active_adapter_(_)),
		catch(Adapter::cleanup, _, true).

	normalize_options(Name, Options0, Options) :-
		(	member(server_name(_), Options0) ->
			Options = Options0
		;	Options = [server_name(Name)| Options0]
		).

	% Options

	default_option(server_version('1.0.0')).
	default_option(server_title('logtalk-mcp-server')).
	default_option(protocol_adapter(mcp_server_2025_06_18_adapter)).

	valid_option(server_name(Name)) :-
		atom(Name).
	valid_option(server_version(Version)) :-
		atom(Version).
	valid_option(server_title(Title)) :-
		atom(Title).
	valid_option(protocol_adapter(Adapter)) :-
		callable(Adapter),
		conforms_to_protocol(Adapter, mcp_server_adapter_protocol).
	valid_option(instructions(Instructions)) :-
		atom(Instructions).
	valid_option(cache_ttl(TTL)) :-
		number(TTL),
		TTL >= 0.
	valid_option(cache_scope(Scope)) :-
		once((Scope == (public); Scope == private)).

:- end_object.
