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
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'MCP server facade. Selects spec and transport (stdio or Streamable HTTP).',
		remarks is [
			'Specs' - '``''2025-06-18''`` (default), ``''2025-11-25''``, and ``''2026-07-28''`` via ``spec/1``.',
			'Transports' - '``stdio`` (default) and ``streamable_http`` via ``transport/1``.',
			'OAuth' - 'Streamable HTTP servers can be protected using the ``oauth/4`` option.'
		]
	]).

	:- public(start/2).
	:- mode(start(+atom, +object_identifier), one).
	:- info(start/2, [
		comment is 'Starts the MCP server with default options.',
		argnames is ['Name', 'Application']
	]).

	:- public(start/3).
	:- mode(start(+atom, +object_identifier, +list), one_or_error).
	:- info(start/3, [
		comment is 'Starts the MCP server with options (``spec/1``, ``transport/1``, HTTP options, ...).',
		argnames is ['Name', 'Application', 'Options'],
		exceptions is [
			'The ``Options`` list specifies an invalid MCP server configuration' - domain_error(mcp_server_configuration, 'Version-Transport')
		]
	]).

	:- public(start/4).
	:- mode(start(+atom, +object_identifier, +stream, +stream), one).
	:- info(start/4, [
		comment is 'Starts with custom streams and default options.',
		argnames is ['Name', 'Application', 'Input', 'Output']
	]).

	:- public(start/5).
	:- mode(start(+atom, +object_identifier, +stream, +stream, +list), one_or_error).
	:- info(start/5, [
		comment is 'Starts with custom streams and options.',
		argnames is ['Name', 'Application', 'Input', 'Output', 'Options'],
		exceptions is [
			'The ``Options`` list specifies a unsupported MCP server configuration' - domain_error(mcp_server_configuration, 'Version-Transport')
		]
	]).

	:- public(notify/1).
	:- mode(notify(+compound), zero_or_one).
	:- info(notify/1, [
		comment is 'Publishes an application event to the active adapter.',
		argnames is ['Event']
	]).

	:- private(active_transport_object_/1).
	:- dynamic(active_transport_object_/1).
	:- mode(active_transport_object_(?object_identifier), zero_or_one).
	:- info(active_transport_object_/1, [
		comment is 'Currently active transport object.',
		argnames is ['TransportObject']
	]).

	:- uses(list, [
		member/2
	]).

	start(Name, Application) :-
		start(Name, Application, []).

	start(Name, Application, UserOptions) :-
		context(Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		normalize_options(Name, Options0, Options),
		validate_transport_options(Options),
		current_input(Input),
		current_output(Output),
		start_transport(Application, Input, Output, Context, Options).

	start(Name, Application, Input, Output) :-
		start(Name, Application, Input, Output, []).

	start(Name, Application, Input, Output, UserOptions) :-
		context(Context),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		normalize_options(Name, Options0, Options),
		validate_transport_options(Options),
		start_transport(Application, Input, Output, Context, Options).

	notify(Event) :-
		(	active_transport_object_(TransportObject) ->
			TransportObject::notify(Event)
		;	true
		).

	start_transport(Application, Input, Output, Context, Options) :-
		^^option(spec(Spec), Options),
		^^option(transport(Transport), Options),
		resolve_transport(Spec, Transport, Context, TransportObject),
		retractall(active_transport_object_(_)),
		assertz(active_transport_object_(TransportObject)),
		(	catch(
				TransportObject::start(Application, Input, Output, Options),
				Error,
				(cleanup_transport(TransportObject), throw(Error))
			) ->
			cleanup_transport(TransportObject)
		;	cleanup_transport(TransportObject)
		).

	resolve_transport('2025-06-18', stdio, _, mcp_server_stdio_transport) :-
		!.
	resolve_transport('2025-11-25', stdio, _, mcp_server_stdio_transport) :-
		!.
	resolve_transport('2026-07-28', stdio, _, mcp_server_stdio_transport) :-
		!.
	resolve_transport('2025-06-18', streamable_http, _, mcp_server_streamable_http_transport) :-
		!.
	resolve_transport('2025-11-25', streamable_http, _, mcp_server_streamable_http_transport) :-
		!.
	resolve_transport('2026-07-28', streamable_http, _, mcp_server_streamable_http_transport) :-
		!.
	resolve_transport(Spec, Transport, Context, _) :-
		throw(error(domain_error(mcp_server_configuration, Spec-Transport), Context)).

	cleanup_transport(TransportObject) :-
		retractall(active_transport_object_(_)),
		catch(TransportObject::cleanup, _, true).

	normalize_options(Name, Options0, Options) :-
		(	member(server_name(_), Options0) ->
			Options = Options0
		;	Options = [server_name(Name)| Options0]
		).

	validate_transport_options(Options) :-
		(	member(oauth(_, _, _, _), Options) ->
			(	member(transport(streamable_http), Options) ->
				true
			;	domain_error(mcp_server_configuration, oauth-stdio)
			)
		;	true
		).

	default_option(server_version('1.0.0')).
	default_option(server_title('logtalk-mcp-server')).
	default_option(spec('2025-06-18')).
	default_option(transport(stdio)).

	valid_option(server_name(Name)) :-
		atom(Name).
	valid_option(server_version(Version)) :-
		atom(Version).
	valid_option(server_title(Title)) :-
		atom(Title).
	valid_option(server_description(Description)) :-
		atom(Description).
	valid_option(spec(Spec)) :-
		once((Spec == '2025-06-18'; Spec == '2025-11-25'; Spec == '2026-07-28')).
	valid_option(transport(Transport)) :-
		once((Transport == stdio; Transport == streamable_http)).
	valid_option(instructions(Instructions)) :-
		atom(Instructions).
	valid_option(cache_ttl(TTL)) :-
		number(TTL),
		TTL >= 0.
	valid_option(cache_scope(Scope)) :-
		once((Scope == (public); Scope == private)).
	valid_option(http_port(Port)) :-
		integer(Port), 0 < Port, Port =< 65535.
	valid_option(http_bind(Bind)) :-
		atom(Bind).
	valid_option(http_path(Path)) :-
		atom(Path).
	valid_option(http_origin_check(Flag)) :-
		once((Flag == true; Flag == false)).
	:- if(current_logtalk_flag(threads, supported)).
		valid_option(http_server_options(Options)) :-
			http_server::valid_options(Options).
	:- endif.
	valid_option(oauth(Verifier, ProtectedResource, MetadataDescriptors, ProtectOptions)) :-
		nonvar(Verifier),
		atom(ProtectedResource),
		type::valid(list(compound), MetadataDescriptors),
		type::valid(list(compound), ProtectOptions).

:- end_object.
