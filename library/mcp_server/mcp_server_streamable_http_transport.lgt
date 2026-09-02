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


:- object(mcp_server_streamable_http_transport,
	implements(mcp_server_transport_protocol),
	imports(mcp_server_application)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'Implements the Streamable HTTP transport for MCP servers. Uses Logtalk ``http_server::serve_until_shutdown/5`` and a dedicated ``http_handler_protocol`` handler object. Supports specs 2025-06-18, 2025-11-25, and 2026-07-28 selected via the ``spec/1`` option and delegated to the matching ``mcp_server_*_spec`` object. Supports optional OAuth protection and protected-resource metadata publication using the ``oauth/4`` option. Long-lived subscriptions/listen streams emit periodic SSE comment keep-alives (``http_sse_keepalive/1``). Requires a multi-threaded backend for subscriptions/listen.'
	]).

	:- threaded.

	:- public(prepare/2).
	:- mode(prepare(+object_identifier, +list), one_or_error).
	:- info(prepare/2, [
		comment is 'Initializes adapter state for ``Application`` with ``Options`` without opening a listener. Used by unit tests and by embeddings by pairing with ``handle_mcp_request/4`` and ``cleanup/0``.',
		argnames is ['Application', 'Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(attach_sse_stream/1).
	:- mode(attach_sse_stream(+stream), one).
	:- info(attach_sse_stream/1, [
		comment is 'Registers a live output stream for incremental SSE writes. Headers must already have been written. Each ``emit_progress/5`` call writes and flushes immediately.',
		argnames is ['Stream']
	]).

	:- public(detach_sse_stream/0).
	:- mode(detach_sse_stream, one).
	:- info(detach_sse_stream/0, [
		comment is 'Clears any live SSE output stream registration.'
	]).

	:- public(sse_headers/1).
	:- mode(sse_headers(-list), one).
	:- info(sse_headers/1, [
		comment is 'HTTP headers for an SSE response body.',
		argnames is ['Headers']
	]).

	:- public(current_options/1).
	:- mode(current_options(-list), zero_or_one).
	:- info(current_options/1, [
		comment is 'Unified with the options list established by ``prepare/2`` or ``start/4``.',
		argnames is ['Options']
	]).

	:- public(handle_mcp_request/4).
	:- mode(handle_mcp_request(+atom, +list, +atom, -compound), one).
	:- info(handle_mcp_request/4, [
		comment is 'Handles one MCP HTTP request. ``Method`` is an uppercase HTTP method atom. ``Headers`` is a list of ``Name-Value`` pairs. ``Body`` is the raw request body atom. ``HTTPResponse`` is ``http_response(Status, Headers, BodyAtom)`` or ``http_response(already_sent, Headers, BodyAtom)``.',
		argnames is ['Method', 'Headers', 'Body', 'HTTPResponse']
	]).

	:- public(emit_progress/5).
	:- mode(emit_progress(+term, +term, +number, +number, +atom), one).
	:- info(emit_progress/5, [
		comment is 'Emits a notifications/progress event. In live mode the SSE record is written and flushed immediately on the attached stream. In buffered mode the event is queued until ``finalize_response/4``.',
		argnames is ['Token', 'RequestId', 'ProgressValue', 'Total', 'Message']
	]).

	:- private(running_/0).
	:- dynamic(running_/0).
	:- mode(running_, zero_or_one).
	:- info(running_/0, [
		comment is 'True while the adapter has been prepared or is serving requests.'
	]).

	:- private(server_options_/1).
	:- dynamic(server_options_/1).
	:- mode(server_options_(-list), zero_or_one).
	:- info(server_options_/1, [
		comment is 'Merged server options for the active prepare/start session.',
		argnames is ['Options']
	]).

	:- private(shutdown_control_/1).
	:- dynamic(shutdown_control_/1).
	:- mode(shutdown_control_(-nonvar), zero_or_one).

	:- info(shutdown_control_/1, [
		comment is 'Token passed to ``http_server::request_shutdown/1`` when stopping the listener.',
		argnames is ['Control']
	]).

	% subscription_(SubscriptionId, RequestId, Filters, Stream)
	% The listen worker blocks with threaded_wait(subscription_msg(SubscriptionId, Msg)).
	% notify/1 and cancellation use threaded_notify/1 with the same message term.
	:- private(subscription_/4).
	:- dynamic(subscription_/4).
	:- mode(subscription_(-atom, -nonvar, -list, -nonvar), zero_or_more).
	:- info(subscription_/4, [
		comment is 'Active subscriptions/listen registration. Synchronization uses ``threaded_wait/1`` and ``threaded_notify/1`` tagged by ``SubscriptionId``.',
		argnames is ['SubscriptionId', 'RequestId', 'Filters', 'Stream']
	]).

	:- private(progress_events_/1).
	:- dynamic(progress_events_/1).
	:- mode(progress_events_(-list), zero_or_one).
	:- info(progress_events_/1, [
		comment is 'Buffered notifications/progress events when SSE mode is not live.',
		argnames is ['Events']
	]).

	:- private(progress_token_/1).
	:- dynamic(progress_token_/1).
	:- mode(progress_token_(-nonvar), zero_or_one).
	:- info(progress_token_/1, [
		comment is 'progressToken from the current request, or none.',
		argnames is ['Token']
	]).

	:- private(sse_output_/1).
	:- dynamic(sse_output_/1).
	:- mode(sse_output_(-stream), zero_or_one).
	:- info(sse_output_/1, [
		comment is 'Live response stream for incremental SSE writes.',
		argnames is ['Stream']
	]).

	:- private(sse_mode_/1).
	:- dynamic(sse_mode_/1).
	:- mode(sse_mode_(-atom), zero_or_one).
	:- info(sse_mode_/1, [
		comment is 'Current SSE delivery mode: live, buffered, or none.',
		argnames is ['Mode']
	]).

	:- uses(format, [
		format/3
	]).

	:- uses(json_rpc, [
		response/3, error_response/4, is_request/1, is_notification/1, id/2, method/2, params/2
	]).

	:- uses(json, [
		parse/2, generate/2
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(os, [
		sleep/1
	]).

	spec(Version) :-
		(	server_options_(Options),
			member(spec(Version), Options) ->
			true
		;	Version = '2026-07-28'
		).

	start(Application, _Input, _Output, UserOptions) :-
		prepare(Application, UserOptions),
		server_options_(Options),
		catch(http_server_loop(Options), Error, (cleanup, throw(Error))),
		cleanup.

	prepare(Application, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		validate_oauth_configuration(Options0),
		(	conforms_to_protocol(Application, mcp_tool_protocol),
			Application::capabilities(Capabilities) ->
			true
		;	Capabilities = []
		),
		Options = [application(Application), application_capabilities(Capabilities)| Options0],
		setup_state(Options),
		(	member(spec('2025-11-25'), Options) ->
			mcp_server_2025_11_25_spec::prepare(Application, Options)
		;	member(spec('2025-06-18'), Options) ->
			mcp_server_2025_06_18_spec::prepare(Application, Options)
		;	mcp_server_2026_07_28_spec::prepare(Application, Options)
		).

	validate_oauth_configuration(Options) :-
		(	member(oauth(Verifier, ProtectedResource, MetadataDescriptors, ProtectOptions), Options) ->
			check_oauth_verifier(Verifier),
			check_oauth_protect_options(ProtectOptions),
			http_oauth_metadata::well_known_url(ProtectedResource, ResourceMetadata),
			http_oauth_metadata::document(
				ProtectedResource, MetadataDescriptors, _Document,
				[required_members([authorization_servers])]
			),
			http_oauth::unauthorized_response(_Challenge, _Response, [
				protected_resource(ProtectedResource),
				resource_metadata(ResourceMetadata)
			| ProtectOptions
			])
		; true
		).

	check_oauth_verifier(Verifier) :-
		(	current_object(Verifier) ->
			(	conforms_to_protocol(Verifier, http_oauth_verifier_protocol) ->
				true
			;	domain_error(http_oauth_verifier, Verifier)
			)
		;	existence_error(http_oauth_verifier, Verifier)
		).

	check_oauth_protect_options([]).
	check_oauth_protect_options([Option| Options]) :-
		functor(Option, Name, Arity),
		(	(Name == protected_resource; Name == resource_metadata), Arity =:= 1 ->
			domain_error(mcp_server_oauth_reserved_option, Option)
		;	check_oauth_protect_options(Options)
		).

	% notify/1 never fails the caller. Per-subscriber errors are isolated:
	% a dead stream or queue causes that subscription to be dropped; other
	% subscribers still receive the event.
	notify(Event) :-
		catch(
			(	findall(
					subscription_(SubscriptionId, RequestId, Filters, Stream),
					subscription_(SubscriptionId, RequestId, Filters, Stream),
					Subscriptions
				),
				dispatch_event(Subscriptions, Event)
			),
			Error,
			log_notify_error(Event, Error)
		),
		!.
	notify(_Event).

	log_notify_error(Event, Error) :-
		catch(
			format(user_error,
				'~w: notify(~w) failed: ~w~n',
				[mcp_server_streamable_http_transport, Event, Error]),
			_,
			true
		).

	cleanup :-
		catch(mcp_server_2025_06_18_spec::cleanup, _, true),
		catch(mcp_server_2025_11_25_spec::cleanup, _, true),
		catch(mcp_server_2026_07_28_spec::cleanup, _, true),
		(	retract(shutdown_control_(Control)) ->
			catch(http_server::request_shutdown(Control), _, true)
		;	true
		),
		% Stop all live subscriptions
		findall(SubscriptionId, subscription_(SubscriptionId, _, _, _), SubscriptionIds),
		stop_all_subscriptions(SubscriptionIds),
		retractall(running_),
		retractall(server_options_(_)),
		retractall(subscription_(_,_,_,_)),
		retractall(progress_events_(_)),
		retractall(progress_token_(_)),
		retractall(sse_output_(_)),
		retractall(sse_mode_(_)).

	stop_all_subscriptions([]).
	stop_all_subscriptions([SubscriptionId| SubscriptionIds]) :-
		signal_subscription_stop(SubscriptionId),
		stop_all_subscriptions(SubscriptionIds).

	signal_subscription_stop(SubscriptionId) :-
		threaded_notify(subscription_msg(SubscriptionId, stop)).

	setup_state(Options) :-
		cleanup,
		assertz(server_options_(Options)),
		assertz(running_).

	% live SSE stream registration (called by the HTTP handler)

	attach_sse_stream(Stream) :-
		retractall(sse_output_(_)),
		retractall(sse_mode_(_)),
		assertz(sse_output_(Stream)),
		assertz(sse_mode_(live)).

	detach_sse_stream :-
		retractall(sse_output_(_)),
		retractall(sse_mode_(_)).

	sse_headers([
		'Content-Type'-'text/event-stream; charset=utf-8',
		'Cache-Control'-'no-cache',
		'X-Accel-Buffering'-'no',
		'Connection'-'close'
	]).

	% HTTP listener via Logtalk http_server library

	http_server_loop(Options) :-
		^^option(http_port(Port), Options),
		^^option(http_bind(Bind), Options),
		^^option(http_path(Path), Options),
		^^option(http_server_options(HTTPServerOptions), Options),
		http_server_scheme(HTTPServerOptions, Scheme),
		http_handler(Options, Handler),
		Control = mcp_http_shutdown(Port),
		retractall(shutdown_control_(_)),
		assertz(shutdown_control_(Control)),
		format(
			user_error,
			'~w: listening on ~w://~w:~w~w~n',
			[mcp_server_streamable_http_transport, Scheme, Bind, Port, Path]
		),
		% per_connection workers so subscriptions/listen can block while
		% other requests (e.g. notifications/cancelled, notify side-effects)
		% are still accepted on the same port
		(	catch(
				http_server::serve_until_shutdown(
					Bind, Port,
					Handler,
					Control,
					[workers(per_connection)| HTTPServerOptions]
				),
				Error,
				(	format(user_error, 'SERVE EXIT ~q~n', [Error]),
					flush_output(user_error),
					throw(Error)
				)
			) ->
			true
		;	format(user_error, 'UNEXPTECTED FAILURE~n', []),
			flush_output(user_error)
		).

	http_server_scheme(Options, Scheme) :-
		(	member(scheme(Scheme0), Options) ->
			Scheme = Scheme0
		;	Scheme = http
		).

	http_handler(Options, Handler) :-
		(	member(oauth(Verifier, ProtectedResource, MetadataDescriptors, ProtectOptions), Options) ->
			http_oauth_metadata::well_known_url(ProtectedResource, ResourceMetadata),
			Handler = http_server_core_oauth_endpoint_handler(
				ProtectedResource,
				MetadataDescriptors,
				[required_members([authorization_servers])],
				Verifier,
				mcp_streamable_http_handler,
				[
					protected_resource(ProtectedResource),
					resource_metadata(ResourceMetadata)
				| ProtectOptions
				]
			)
		;	Handler = mcp_streamable_http_handler
		).

	current_options(Options) :-
		server_options_(Options).

	handle_mcp_request(Method, Headers, Body, HTTPResponse) :-
		(	Method == 'POST' ->
			handle_post(Headers, Body, HTTPResponse)
		;	HTTPResponse = http_response(405,
				['Allow'-'POST', 'Content-Type'-'text/plain; charset=utf-8'],
				'Method Not Allowed')
		).

	handle_post(Headers, Body, HTTPResponse) :-
		(	\+ validate_origin(Headers) ->
			HTTPResponse = http_response(403, ['Content-Type'-'text/plain; charset=utf-8'], text_body('Forbidden'))
		;	catch(parse(atom(Body), Message), _, fail) ->
			handle_http_message(Message, Headers, HTTPResponse)
		;	HTTPResponse = http_response(400, ['Content-Type'-'text/plain; charset=utf-8'], text_body('Invalid JSON-RPC body'))
		).

	handle_http_message(Message, Headers, HTTPResponse) :-
		(	is_notification(Message) ->
			handle_notification(Message),
			HTTPResponse = http_response(202, [], text_body(''))
		;	is_request(Message) ->
			id(Message, Id),
			(	valid_request_id(Id) ->
				method(Message, Method),
				(	params(Message, Params0) ->
					Params = Params0
				;	Params = {}
				),
				spec(Version),
				(	(Version == '2025-06-18' ; Version == '2025-11-25') ->
					handle_via_protocol_2025(Message, HTTPResponse)
				;	http_needs_stream_path(Method, Params, Headers) ->
					(	validate_2026(Method, Headers, Params, Id, Err) ->
						HTTPResponse = Err
					;	dispatch_method(Method, Params, Id, HTTPResponse)
					)
				;	handle_via_protocol_2026(Message, Headers, HTTPResponse)
				)
			;	HTTPResponse = http_response(400, ['Content-Type'-'text/plain; charset=utf-8'], text_body('Invalid request id'))
			)
		;	HTTPResponse = http_response(400, ['Content-Type'-'text/plain; charset=utf-8'], text_body('Not a JSON-RPC request or notification'))
		).

	valid_request_id(Id) :-
		(	atom(Id) ->
			Id \== ''
		;	integer(Id)
		).

	% protocol-handler bridge (non-SSE path)

	% SSE path only when the request itself needs a stream: subscriptions/listen
	% or a progressToken. Compliant clients always send Accept including
	% text/event-stream; that alone must not force SSE (server/discover and
	% ordinary tools/call must stay application/json for clients like Postman).
	http_needs_stream_path('subscriptions/listen', _, _) :-
		!.
	http_needs_stream_path(_Method, Params, _Headers) :-
		^^has_pair(Params, '_meta', Meta),
		^^has_pair(Meta, progressToken, _).

	handle_via_protocol_2025(Message, HTTPResponse) :-
		server_options_(Options),
		(	member(spec('2025-11-25'), Options) ->
			Protocol = mcp_server_2025_11_25_spec
		;	Protocol = mcp_server_2025_06_18_spec
		),
		catch(
			(	Protocol::handle_message(Message, Options, Outcome),
				render_protocol_outcome(Outcome, HTTPResponse)
			),
			Error,
			(	(	is_request(Message) -> id(Message, Id) ; Id = null ),
				json_error(Id, -32603, Error, HTTPResponse)
			)
		).

	handle_via_protocol_2026(Message, Headers, HTTPResponse) :-
		(	is_request(Message) ->
			id(Message, Id),
			method(Message, Method),
			(	params(Message, Params0) ->
				Params = Params0
			;	Params = {}
			),
			(	validate_2026(Method, Headers, Params, Id, ErrorResponse) ->
				HTTPResponse = ErrorResponse
			;	server_options_(Options),
				mcp_server_2026_07_28_spec::handle_message(Message, Options, Outcome),
				render_protocol_outcome_2026(Outcome, HTTPResponse)
			)
		;	server_options_(Options),
			mcp_server_2026_07_28_spec::handle_message(Message, Options, Outcome),
			render_protocol_outcome_2026(Outcome, HTTPResponse)
		).

	render_protocol_outcome(reply(Response), http_response(200, ['Content-Type'-'application/json; charset=utf-8'], json_body(Response))) :-
		!.
	render_protocol_outcome(accepted, http_response(202, [], text_body(''))) :-
		!.
	render_protocol_outcome(no_reply, http_response(202, [], text_body(''))) :-
		!.
	render_protocol_outcome(_, http_response(500, ['Content-Type'-'text/plain; charset=utf-8'], text_body('Internal protocol outcome error'))).

	render_protocol_outcome_2026(reply(Response), http_response(200, ['Content-Type'-'application/json; charset=utf-8'], json_body(Response))) :-
		!.
	render_protocol_outcome_2026(reply_with_progress(_Events, Final), HTTPResponse) :-
		!,
		render_protocol_outcome_2026(reply(Final), HTTPResponse).
	render_protocol_outcome_2026(subscribe(_SubscriptionId, _Filters, Messages), HTTPResponse) :-
		!,
		(	Messages = [AckResponse| _] ->
			render_protocol_outcome_2026(reply(AckResponse), HTTPResponse)
		;	HTTPResponse = http_response(202, [], text_body(''))
		).
	render_protocol_outcome_2026(accepted, http_response(202, [], text_body(''))) :-
		!.
	render_protocol_outcome_2026(no_reply, http_response(202, [], text_body(''))) :-
		!.
	render_protocol_outcome_2026(_, http_response(500, ['Content-Type'-'text/plain; charset=utf-8'], text_body('Internal protocol outcome error'))).

	% succeeds when the request is invalid, binding ErrorResponse
	% fails when the request passes 2026 transport/metadata checks
	%
	% MCP 2026-07-28 Streamable HTTP requires (SEP-2243, SEP-2575):
	%   - params._meta with protocolVersion + clientCapabilities
	%   - MCP-Protocol-Version header matching _meta version
	%   - Mcp-Method header matching the JSON-RPC method
	%   - Mcp-Name header when the method names a tool/prompt/resource
	% Header mismatch uses error code -32020 (HeaderMismatch).
	validate_2026(Method, Headers, Params, Id, ErrorResponse) :-
		(	^^has_pair(Params, '_meta', Meta) ->
			validate_2026_meta(Method, Headers, Params, Meta, Id, ErrorResponse)
		;	json_error(Id, -32602, 'Missing required params._meta', ErrorResponse)
		).

	validate_2026_meta(Method, Headers, Params, Meta, Id, ErrorResponse) :-
		(	^^has_pair(Meta, 'io.modelcontextprotocol/protocolVersion', Version) ->
			validate_2026_version(Method, Headers, Params, Meta, Version, Id, ErrorResponse)
		;	json_error(Id, -32602, 'Missing required protocolVersion in _meta', ErrorResponse)
		).

	validate_2026_version(Method, Headers, Params, Meta, Version, Id, ErrorResponse) :-
		(	Version == '2026-07-28' ->
			validate_2026_protocol_header(Method, Headers, Params, Meta, Version, Id, ErrorResponse)
		;	json_error_data(Id, -32022, 'Unsupported protocol version', {supported-['2026-07-28'], requested-Version}, ErrorResponse)
		).

	validate_2026_protocol_header(Method, Headers, Params, Meta, Version, Id, ErrorResponse) :-
		(	header_value(Headers, mcp_protocol_version, HeaderVersion) ->
			(	HeaderVersion == Version ->
				validate_2026_method_header(Method, Headers, Params, Meta, Id, ErrorResponse)
			;	json_error_data(Id, -32020, 'HeaderMismatch: MCP-Protocol-Version',
					{header-'MCP-Protocol-Version', expected-Version, actual-HeaderVersion}, ErrorResponse)
			)
		;	json_error(Id, -32602, 'Missing required MCP-Protocol-Version header', ErrorResponse)
		).

	% Mcp-Method must match the JSON-RPC method (case-sensitive atom equality after
	% normalizing common casing). Missing header is an invalid params error; wrong
	% value is HeaderMismatch (-32020).
	validate_2026_method_header(Method, Headers, Params, Meta, Id, ErrorResponse) :-
		(	header_value(Headers, mcp_method, HeaderMethod0) ->
			normalize_mcp_method_header(HeaderMethod0, HeaderMethod),
			(	HeaderMethod == Method ->
				validate_2026_name_header(Method, Headers, Params, Meta, Id, ErrorResponse)
			;	json_error_data(Id, -32020, 'HeaderMismatch: Mcp-Method',
					{header-'Mcp-Method', expected-Method, actual-HeaderMethod0}, ErrorResponse)
			)
		;	json_error(Id, -32602, 'Missing required Mcp-Method header', ErrorResponse)
		).

	normalize_mcp_method_header(Header, Method) :-
		atom(Header),
		!,
		Method = Header.
	normalize_mcp_method_header(Header, Method) :-
		write_to_atom(Header, Method).

	% Mcp-Name is required for methods that name a tool, prompt, or resource
	% (tools/call, prompts/get, resources/read). Value must match params.name or
	% params.uri as appropriate.
	validate_2026_name_header(Method, Headers, Params, Meta, Id, ErrorResponse) :-
		method_requires_mcp_name(Method, ParamKey),
		!,
		(	header_value(Headers, mcp_name, HName0) ->
			normalize_mcp_method_header(HName0, HName),
			(	^^has_pair(Params, ParamKey, Expected),
				HName == Expected ->
				validate_2026_param_headers(Method, Headers, Params, Meta, Id, ErrorResponse)
			;	^^has_pair(Params, ParamKey, Expected) ->
				json_error_data(Id, -32020, 'HeaderMismatch: Mcp-Name',
					{header-'Mcp-Name', expected-Expected, actual-HName0}, ErrorResponse)
			;	% body missing the name/uri field — leave that to the method handler
				validate_2026_param_headers(Method, Headers, Params, Meta, Id, ErrorResponse)
			)
		;	json_error(Id, -32602, 'Missing required Mcp-Name header', ErrorResponse)
		).
	validate_2026_name_header(Method, Headers, Params, Meta, Id, ErrorResponse) :-
		validate_2026_param_headers(Method, Headers, Params, Meta, Id, ErrorResponse).

	method_requires_mcp_name('tools/call', name).
	method_requires_mcp_name('prompts/get', name).
	method_requires_mcp_name('resources/read', uri).

	% SEP-2243: when a tool inputSchema property carries ``x-mcp-header``, the
	% client MUST mirror that argument into an ``Mcp-Param-{suffix}`` header.
	% Validate present argument values against headers; mismatch -> -32020.
	validate_2026_param_headers('tools/call', Headers, Params, Meta, Id, ErrorResponse) :-
		!,
		(	^^has_pair(Params, name, ToolName),
			^^has_pair(Params, arguments, Args) ->
			(	mcp_param_header_mismatch(ToolName, Args, Headers, Mismatch) ->
				Mismatch = mismatch(HeaderName, Expected, Actual),
				json_error_data(Id, -32020, 'HeaderMismatch: Mcp-Param',
					{header-HeaderName, expected-Expected, actual-Actual}, ErrorResponse)
			;	mcp_param_header_missing(ToolName, Args, Headers, Missing) ->
				json_error(Id, -32602, Missing, ErrorResponse)
			;	validate_2026_capabilitiess('tools/call', Meta, Id, ErrorResponse)
			)
		;	validate_2026_capabilitiess('tools/call', Meta, Id, ErrorResponse)
		).
	validate_2026_param_headers(Method, _Headers, _Params, Meta, Id, ErrorResponse) :-
		validate_2026_capabilitiess(Method, Meta, Id, ErrorResponse).

	mcp_param_header_mismatch(ToolName, Args, Headers, mismatch(HeaderName, Expected, Actual)) :-
		resolve_tool_input_schema(ToolName, Schema),
		header_annotated_params(Schema, Annotated),
		member(ParamName-Suffix, Annotated),
		atom_concat('Mcp-Param-', Suffix, HeaderName),
		^^has_pair(Args, ParamName, Expected0),
		normalize_mcp_param_value(Expected0, Expected),
		header_value(Headers, HeaderName, Actual0),
		normalize_mcp_param_value(Actual0, Actual),
		Expected \== Actual,
		!.

	mcp_param_header_missing(ToolName, Args, Headers, Message) :-
		resolve_tool_input_schema(ToolName, Schema),
		header_annotated_params(Schema, Annotated),
		member(ParamName-Suffix, Annotated),
		^^has_pair(Args, ParamName, _),
		atom_concat('Mcp-Param-', Suffix, HeaderName),
		\+ header_value(Headers, HeaderName, _),
		!,
		atom_concat('Missing required header ', HeaderName, Message).

	resolve_tool_input_schema(ToolName, Schema) :-
		server_options_(Options),
		^^option(application(Application), Options),
		Application::tools(Tools),
		member(tool(ToolName, Functor, Arity), Tools),
		(	conforms_to_protocol(Application, mcp_tool_protocol),
			Application::input_schema(ToolName, Schema) ->
			true
		;	^^tool_input_schema(Application, Functor, Arity, Schema)
		).

	header_annotated_params(Schema, Annotated) :-
		(	^^has_pair(Schema, properties, Props) ->
			^^curly_to_pairs(Props, Pairs),
			findall(
				ParamName-Suffix,
				(	member(ParamName-PropSchema, Pairs),
					^^has_pair(PropSchema, 'x-mcp-header', Suffix0),
					atom(Suffix0),
					Suffix0 \== '',
					Suffix = Suffix0
				),
				Annotated
			)
		;	Annotated = []
		).

	normalize_mcp_param_value(Value, Atom) :-
		(	atom(Value) ->
			Atom = Value
		;	integer(Value) ->
			number_codes(Value, Codes),
			atom_codes(Atom, Codes)
		;	float(Value) ->
			write_to_atom(Value, Atom)
		;	Value == @true ->
			Atom = true
		;	Value == @false ->
			Atom = false
		;	write_to_atom(Value, Atom)
		).

	validate_2026_capabilitiess(Method, Meta, Id, ErrorResponse) :-
		(	^^has_pair(Meta, 'io.modelcontextprotocol/clientCapabilities', _) ->
			check_capabilities(Method, Meta, Id, ErrorResponse)
		;	json_error(Id, -32602, 'Missing required clientCapabilities in _meta', ErrorResponse)
		).

	% check_capabilities/4 succeeds only when binding ErrorResponse (invalid request).
	% tools/prompts/resources are server capabilities; clients need not advertise them
	% (-32021 is reserved for genuine missing *client* capabilities). Currently a no-op.
	check_capabilities(_Method, _Meta, _Id, _ErrorResponse) :-
		fail.

	dispatch_method(Method, Params, Id, HTTPResponse) :-
		catch(
			do_dispatch(Method, Params, Id, HTTPResponse),
			Error,
			json_error(Id, -32603, Error, HTTPResponse)
		).

	do_dispatch(Method, Params, Id, HTTPResponse) :-
		(	do_dispatch_(Method, Params, Id, HTTPResponse) ->
			true
		;	json_error(Id, -32601, 'Method not found', HTTPResponse)
		).

	do_dispatch_('server/discover', Params, Id, HTTPResponse) :-
		handle_discover(Params, Id, HTTPResponse).
	do_dispatch_(initialize, _, Id, HTTPResponse) :-
		json_error(Id, -32600, 'initialize is not used in MCP 2026-07-28; use server/discover', HTTPResponse).
	% ping was removed in MCP 2026-07-28 (SEP-2575); fall through to method not found
	do_dispatch_('tools/list', Params, Id, HTTPResponse) :-
		handle_tools_list(Params, Id, HTTPResponse).
	do_dispatch_('tools/call', Params, Id, HTTPResponse) :-
		handle_tools_call(Params, Id, HTTPResponse).
	do_dispatch_('prompts/list', Params, Id, HTTPResponse) :-
		handle_prompts_list(Params, Id, HTTPResponse).
	do_dispatch_('prompts/get', Params, Id, HTTPResponse) :-
		handle_prompts_get(Params, Id, HTTPResponse).
	do_dispatch_('resources/list', Params, Id, HTTPResponse) :-
		handle_resources_list(Params, Id, HTTPResponse).
	do_dispatch_('resources/read', Params, Id, HTTPResponse) :-
		handle_resources_read(Params, Id, HTTPResponse).
	do_dispatch_('subscriptions/listen', Params, Id, HTTPResponse) :-
		handle_subscriptions_listen(Params, Id, HTTPResponse).

	handle_notification(Message) :-
		method(Message, Method),
		(	Method == 'notifications/cancelled' ->
			handle_cancelled(Message)
		;	true
		).

	handle_cancelled(Message) :-
		(	params(Message, Params),
			^^has_pair(Params, requestId, RequestId) ->
			forall(
				subscription_(SubscriptionId, RequestId, _, _),
				signal_subscription_stop(SubscriptionId)
			),
			retractall(subscription_(_, RequestId, _, _))
		;	true
		).

	handle_discover(_, Id, HTTPResponse) :-
		server_options_(Options),
		^^option(server_name(Name), Options),
		^^option(server_version(Version), Options),
		^^option(server_title(Title), Options),
		^^option(instructions(Instructions), Options),
		^^option(application_capabilities(Caps), Options),
		build_capabilities(Caps, Capabilities),
		resolve_cache(discover, {}, TTL, Scope, Options),
		ServerInfo = {name-Name, title-Title, version-Version},
		Meta = {'io.modelcontextprotocol/serverInfo'-ServerInfo},
		(	Instructions == '' ->
			Result = {
				supportedVersions-['2026-07-28'], capabilities-Capabilities,
				resultType-complete, ttlMs-TTL, cacheScope-Scope, '_meta'-Meta
			}
		;	Result = {
				supportedVersions-['2026-07-28'], capabilities-Capabilities,
				instructions-Instructions, resultType-complete, ttlMs-TTL,
				cacheScope-Scope, '_meta'-Meta
			}
		),
		json_result(Id, Result, HTTPResponse).

	build_capabilities(AppCaps, Capabilities) :-
		Base = [tools-{}],
		(	member(prompts, AppCaps) ->
			Capabilities0 = [prompts-{}| Base]
		;	Capabilities0 = Base
		),
		(	member(resources, AppCaps) ->
			Capabilities1 = [resources-{}|Capabilities0]
		;	Capabilities1 = Capabilities0
		),
		Capabilities2 = [subscriptions-{}|Capabilities1],
		% MCP Apps extension (io.modelcontextprotocol/ui)
		(	member(ui, AppCaps) ->
			UIExt = {'io.modelcontextprotocol/ui'-{mimeTypes-['text/html;profile=mcp-app']}},
			Capabilities3 = [extensions-UIExt| Capabilities2]
		;	Capabilities3 = Capabilities2
		),
		^^pairs_to_curly(Capabilities3, Capabilities).

	handle_tools_list(_, Id, HTTPResponse) :-
		server_options_(Options),
		^^option(application(Appplication), Options),
		Appplication::tools(Tools),
		^^tool_descriptors_to_json(Tools, Appplication, JsonTools),
		resolve_cache(tools_list, {}, TTL, Scope, Options),
		json_result(Id, {tools-JsonTools, resultType-complete, ttlMs-TTL, cacheScope-Scope}, HTTPResponse).

	handle_tools_call(Params, Id, HTTPResponse) :-
		(	^^has_pair(Params, name, ToolName) ->
			(	^^has_pair(Params, arguments, Args) ->
				true
			;	Args = {}
			),
			extract_progress_token(Params, ProgressToken),
			extract_client_capabilities(Params, ClientCaps),
			extract_input_responses(Params, InputResponses),
			extract_request_state(Params, RequestState),
			begin_progress(ProgressToken),
			(	catch(
					do_tools_call(ToolName, Args, ClientCaps, InputResponses, RequestState, ProgressToken, Id, HTTPResponse),
					Error,
					(json_error(Id, -32603, Error, HTTPResponse), Fail = true)
				) ->
				(	nonvar(Fail) ->
					true
				;	nonvar(HTTPResponse) ->
					true
					;	format_tool_result(failure, Id, ProgressToken, HTTPResponse)
				)
			;	json_error(Id, -32603, 'Tool execution failed', HTTPResponse)
			),
			end_progress
		;	json_error(Id, -32602, 'Missing tool name', HTTPResponse)
		).

	do_tools_call(ToolName, Args, ClientCaps, InputResponses, RequestState, ProgressToken, Id, HTTPResponse) :-
		server_options_(Options),
		^^option(application(Application), Options),
		(	Application::tools(Tools),
			member(tool(ToolName, Functor, Arity), Tools) ->
			^^curly_to_pairs(Args, ArgPairs),
			make_progress_closure(ProgressToken, Id, Progress),
			Context = request_context(ClientCaps, InputResponses, RequestState, Progress),
			(	conforms_to_protocol(Application, mcp_multiround_protocol),
				Application::tool_call_round(ToolName, ArgPairs, Context, RoundResult) ->
				handle_round_tool_result(RoundResult, Id, ProgressToken, HTTPResponse)
			;	(	catch(
						^^try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, Args, Result),
						Error,
						Result = error(Error)
					) ->
					format_tool_result(Result, Id, ProgressToken, HTTPResponse)
				;	json_error(Id, -32603, 'Tool execution failed', HTTPResponse)
				)
			)
		;	json_error(Id, -32602, 'Unknown tool', HTTPResponse)
		).

	handle_round_tool_result(complete(Result), Id, ProgressToken, HTTPResponse) :-
		!,
		format_tool_result(Result, Id, ProgressToken, HTTPResponse).
	handle_round_tool_result(input_required(InputRequests, RequestState), Id, _ProgressToken, HTTPResponse) :-
		!,
		% input_required is returned as JSON (not SSE) per 2026 rules for non-complete
		(	(	InputRequests = [_| _] ; RequestState \== none
			),
			catch(input_requests_to_json(InputRequests, JsonRequests), _, fail) ->
			(	RequestState == none ->
				Result = {resultType-input_required, inputRequests-JsonRequests}
			;	Result = {resultType-input_required, inputRequests-JsonRequests, requestState-RequestState}
			),
			json_result(Id, Result, HTTPResponse)
		;	json_error(Id, -32603, 'Invalid multi-round result: must include nonempty inputRequests or a requestState', HTTPResponse)
		).
	handle_round_tool_result(Other, Id, _, HTTPResponse) :-
		json_error(Id, -32603, Other, HTTPResponse).

	format_tool_result(Result, Id, ProgressToken, HTTPResponse) :-
		tool_result_body(Result, Id, BodyTerm),
		finalize_response(Id, BodyTerm, ProgressToken, HTTPResponse).

	tool_result_body(text(Text), Id, Msg) :-
		!,
		response({content-[{type-text, text-Text}], resultType-complete}, Id, Msg).
	tool_result_body(error(Error), Id, Msg) :-
		!,
		(atom(Error) -> Text = Error ; write_to_atom(Error, Text)),
		response({content-[{type-text, text-Text}], isError- @true, resultType-complete}, Id, Msg).
	tool_result_body(failure, Id, Msg) :-
		!,
		response({content-[{type-text, text-'Tool predicate failed'}], isError- @true, resultType-complete}, Id, Msg).
	tool_result_body(results(Items), Id, Msg) :-
		!,
		^^format_content_items(Items, Content),
		response({content-Content, resultType-complete}, Id, Msg).
	tool_result_body(structured(SC), Id, Msg) :-
		!,
		write_to_atom(SC, Text),
		response({content-[{type-text, text-Text}], structuredContent-SC, resultType-complete}, Id, Msg).
	tool_result_body(structured(Items, SC), Id, Msg) :-
		!,
		^^format_content_items(Items, Content),
		response({content-Content, structuredContent-SC, resultType-complete}, Id, Msg).
	tool_result_body(Other, Id, Msg) :-
		(atom(Other) -> Text = Other ; write_to_atom(Other, Text)),
		error_response(-32603, Text, Id, Msg).

	% progress / SSE

	extract_progress_token(Params, Token) :-
		(	^^has_pair(Params, '_meta', Meta),
			^^has_pair(Meta, progressToken, Token0) ->
			Token = Token0
		;	Token = none
		).

	extract_client_capabilities(Params, Caps) :-
		(	^^has_pair(Params, '_meta', Meta),
			^^has_pair(Meta, 'io.modelcontextprotocol/clientCapabilities', Caps0) ->
			Caps = Caps0
		;	Caps = {}
		).

	extract_input_responses(Params, Responses) :-
		(	^^has_pair(Params, inputResponses, Raw) ->
			normalize_input_responses(Raw, Responses)
		;	Responses = []
		).

	normalize_input_responses([], []) :-
		!.
	normalize_input_responses([Item| Items], [input_response(Key, Value)| Out]) :-
		!,
		(	^^has_pair(Item, key, Key) -> true ; Key = unknown),
		(	^^has_pair(Item, value, Value) -> true
		;	^^has_pair(Item, action, Action) ->
			(	Action == accept, ^^has_pair(Item, content, Content) -> Value = accept(Content)
			;	Action == accept -> Value = accept({})
			;	Action == decline -> Value = decline
			;	Value = cancel
			)
		;	Value = Item
		),
		normalize_input_responses(Items, Out).
	normalize_input_responses(_, []).

	extract_request_state(Params, State) :-
		(	^^has_pair(Params, requestState, State0) ->
			State = State0
		;	State = none
		).

	begin_progress(none) :-
		!,
		retractall(progress_events_(_)),
		retractall(progress_token_(_)),
		% do not clear sse_output_/sse_mode_ — handler may have attached already
		assertz(progress_token_(none)),
		assertz(progress_events_([])),
		(	sse_mode_(_) ->
			true
		;	assertz(sse_mode_(none))
		).
	begin_progress(Token) :-
		retractall(progress_events_(_)),
		retractall(progress_token_(_)),
		assertz(progress_token_(Token)),
		assertz(progress_events_([])),
		(	sse_output_(_) ->
			retractall(sse_mode_(_)),
			assertz(sse_mode_(live))
		;	retractall(sse_mode_(_)),
			assertz(sse_mode_(buffered))
		).

	% sse_output_/sse_mode_ are cleared by the handler via detach_sse_stream/0
	end_progress :-
		retractall(progress_events_(_)),
		retractall(progress_token_(_)).

	make_progress_closure(none, _, Progress) :-
		!,
		Progress = [_,_,_]>>(true).
	make_progress_closure(Token, RequestId, Progress) :-
		Progress = {Token, RequestId}/[ProgressValue, Total, Message]>>(
			mcp_server_streamable_http_transport::emit_progress(Token, RequestId, ProgressValue, Total, Message)
		).

	emit_progress(Token, _RequestId, ProgressValue, Total, Message) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/progress',
			params-{
				progressToken-Token,
				progress-ProgressValue,
				total-Total,
				message-Message
			}
		},
		(	sse_mode_(live),
			sse_output_(Stream) ->
			write_sse_data_event(Stream, Notification),
			flush_output(Stream)
		;	(	retract(progress_events_(Events0)) ->
				append(Events0, [Notification], Events1),
				assertz(progress_events_(Events1))
			;	assertz(progress_events_([Notification]))
			)
		).

	% write a single SSE data record and optionally flush
	write_sse_data_event(Stream, Term) :-
		json_serialize(Term, Atom),
		atom_codes(Atom, Codes),
		atom_codes('data: ', Prefix),
		% SSE record terminator: blank line (LF LF)
		Suffix = [10, 10],
		put_codes(Prefix, Stream),
		put_codes(Codes, Stream),
		put_codes(Suffix, Stream).

	% SSE comment line (keep-alive). Per the SSE specification, lines that
	% begin with a colon carry no event data and must be ignored by clients.
	% MCP 2026-07-28 encourages periodic comments on long-lived streams
	% (notably subscriptions/listen) so intermediaries do not idle-close them.
	write_sse_comment(Stream) :-
		% ":\r\n"
		put_codes([0':, 13, 10], Stream),
		flush_output(Stream).

	put_codes([], _Stream).
	put_codes([Code| Codes], Stream) :-
		(	catch(put_byte(Stream, Code), _, fail) ->
			true
		;	put_code(Stream, Code)
		),
		put_codes(Codes, Stream).

	% finalize: live mode writes the final event to the stream; buffered mode
	% returns a complete SSE body; no-token mode returns application/json
	finalize_response(_Id, FinalMsg, none, HTTPResponse) :-
		!,
		HTTPResponse = http_response(200, ['Content-Type'-'application/json; charset=utf-8'], json_body(FinalMsg)).
	finalize_response(_Id, FinalMsg, _Token, HTTPResponse) :-
		sse_mode_(live),
		sse_output_(Stream),
		!,
		% final JSON-RPC response as last SSE event
		write_sse_data_event(Stream, FinalMsg),
		flush_output(Stream),
		% body already streamed; signal handler that response was sent
		sse_headers(Headers),
		HTTPResponse = http_response(already_sent, Headers, text_body('')).
	finalize_response(_Id, FinalMsg, _Token, HTTPResponse) :-
		% buffered fallback — SSE text body (not JSON term)
		(	progress_events_(Events) ->
			true
		;	Events = []
		),
		sse_body(Events, FinalMsg, Body),
		sse_headers(Headers),
		HTTPResponse = http_response(200, Headers, text_body(Body)).

	sse_body(Events, FinalMsg, Body) :-
		sse_events_codes(Events, FinalMsg, Codes),
		atom_codes(Body, Codes).

	sse_events_codes([], FinalMsg, Codes) :-
		sse_data_event_codes(FinalMsg, Codes).
	sse_events_codes([Event| Events], FinalMsg, Codes) :-
		sse_data_event_codes(Event, Codes0),
		sse_events_codes(Events, FinalMsg, Codes1),
		append(Codes0, Codes1, Codes).

	sse_data_event_codes(Term, Codes) :-
		json_serialize(Term, Atom),
		atom_codes(Atom, JSONCodes),
		atom_codes('data: ', Prefix),
		% SSE record terminator: blank line (LF LF)
		Suffix = [10, 10],
		append(Prefix, JSONCodes, Tmp),
		append(Tmp, Suffix, Codes).

	input_requests_to_json([], []).
	input_requests_to_json([input_request(Key, Request)| InputRequests], [Json| JsonRest]) :-
		request_to_json(Request, Key, Json),
		input_requests_to_json(InputRequests, JsonRest).

	request_to_json(form_elicitation(Message, Schema), Key, Json) :-
		Json = {key-Key, method-'elicitation/create', params-{message-Message, requestedSchema-Schema}}.
	request_to_json(url_elicitation(Message, URL), Key, Json) :-
		Json = {key-Key, method-'elicitation/create', params-{message-Message, url-URL}}.
	request_to_json(sampling(Messages, ModelPreferences, SystemPrompt, IncludeContext), Key, Json) :-
		Json = {key-Key, method-'sampling/createMessage', params-{
			messages-Messages,
			modelPreferences-ModelPreferences,
			systemPrompt-SystemPrompt,
			includeContext-IncludeContext
		}}.
	request_to_json(roots, Key, Json) :-
		Json = {key-Key, method-'roots/list', params-{}}.

	handle_prompts_list(_, Id, HTTPResponse) :-
		server_options_(Options),
		^^option(application(Application), Options),
		(	conforms_to_protocol(Application, mcp_prompt_protocol),
			Application::prompts(Descriptors) ->
			^^prompt_descriptors_to_json(Descriptors, Json)
		;	Json = []
		),
		resolve_cache(prompts_list, {}, TTL, Scope, Options),
		json_result(Id, {prompts-Json, resultType-complete, ttlMs-TTL, cacheScope-Scope}, HTTPResponse).

	handle_prompts_get(Params, Id, HTTPResponse) :-
		^^has_pair(Params, name, Name),
		!,
		(	^^has_pair(Params, arguments, Args) ->
			true
		;	Args = {}
		),
		extract_progress_token(Params, ProgressToken),
		begin_progress(ProgressToken),
		server_options_(Options),
		^^option(application(App), Options),
		^^curly_to_pairs(Args, ArgPairs),
		make_progress_closure(ProgressToken, Id, Progress),
		extract_client_capabilities(Params, ClientCaps),
		extract_input_responses(Params, InputResponses),
		extract_request_state(Params, RequestState),
		Context = request_context(ClientCaps, InputResponses, RequestState, Progress),
		(	catch(
				App::prompt_get_round(Name, ArgPairs, Context, RoundResult),
				error(existence_error(procedure, _), _),
				fail
			) ->
			(	RoundResult = complete(Result) ->
				format_prompt_result(Result, Id, ProgressToken, HTTPResponse)
			;	RoundResult = input_required(Reqs, State) ->
				handle_round_tool_result(input_required(Reqs, State), Id, ProgressToken, HTTPResponse)
			;	json_error(Id, -32603, RoundResult, HTTPResponse)
			)
		;	(	catch(App::prompt_get(Name, ArgPairs, Result), Error, Result = error(Error)) ->
				format_prompt_result(Result, Id, ProgressToken, HTTPResponse)
			;	json_error(Id, -32603, 'Prompt execution failed', HTTPResponse)
			)
		),
		end_progress.
	handle_prompts_get(_Params, Id, HTTPResponse) :-
		json_error(Id, -32602, 'Missing prompt name', HTTPResponse).

	format_prompt_result(Result, Id, ProgressToken, HTTPResponse) :-
		prompt_result_body(Result, Id, BodyTerm),
		finalize_response(Id, BodyTerm, ProgressToken, HTTPResponse).

	prompt_result_body(messages(Ms), Id, Msg) :-
		!,
		^^format_prompt_messages(Ms, Json),
		response({messages-Json, resultType-complete}, Id, Msg).
	prompt_result_body(messages(Desc, Ms), Id, Msg) :-
		!,
		^^format_prompt_messages(Ms, Json),
		response({description-Desc, messages-Json, resultType-complete}, Id, Msg).
	prompt_result_body(error(E), Id, Msg) :-
		!,
		(	atom(E) ->
			T = E
		;	write_to_atom(E, T)
		),
		error_response(-32603, T, Id, Msg).
	prompt_result_body(Other, Id, Msg) :-
		(	atom(Other) ->
			T = Other
		;	write_to_atom(Other, T)
		),
		error_response(-32603, T, Id, Msg).

	handle_resources_list(_, Id, HTTPResponse) :-
		server_options_(Options),
		^^option(application(Application), Options),
		(	conforms_to_protocol(Application, mcp_resource_protocol),
			Application::resources(Descriptors) ->
			^^resource_descriptors_to_json(Descriptors, Application, Json)
		;	Json = []
		),
		resolve_cache(resources_list, {}, TTL, Scope, Options),
		json_result(Id, {resources-Json, resultType-complete, ttlMs-TTL, cacheScope-Scope}, HTTPResponse).

	handle_resources_read(Params, Id, HTTPResponse) :-
		^^has_pair(Params, uri, URI),
		!,
		extract_progress_token(Params, ProgressToken),
		begin_progress(ProgressToken),
		server_options_(Options),
		^^option(application(Application), Options),
		make_progress_closure(ProgressToken, Id, Progress),
		extract_client_capabilities(Params, ClientCaps),
		extract_input_responses(Params, InputResponses),
		extract_request_state(Params, RequestState),
		Context = request_context(ClientCaps, InputResponses, RequestState, Progress),
		(	conforms_to_protocol(Application, mcp_multiround_protocol),
			Application::resource_read_round(URI, [], Context, RoundResult) ->
			(	RoundResult = complete(Result) ->
				format_resource_result(Result, URI, Id, Options, ProgressToken, HTTPResponse)
			;	RoundResult = input_required(Reqs, State) ->
				handle_round_tool_result(input_required(Reqs, State), Id, ProgressToken, HTTPResponse)
			;	json_error(Id, -32603, RoundResult, HTTPResponse)
			)
		;	(	catch(Application::resource_read(URI, [], Result), Error, Result = error(Error)) ->
				format_resource_result(Result, URI, Id, Options, ProgressToken, HTTPResponse)
			;	json_error(Id, -32603, 'Resource read failed', HTTPResponse)
			)
		),
		end_progress.
	handle_resources_read(_Params, Id, HTTPResponse) :-
		json_error(Id, -32602, 'Missing resource uri', HTTPResponse).

	format_resource_result(Result, URI, Id, Options, ProgressToken, HTTPResponse) :-
		resource_result_body(Result, URI, Id, Options, BodyTerm),
		finalize_response(Id, BodyTerm, ProgressToken, HTTPResponse).

	resource_result_body(contents(Contents), URI, Id, Options, Message) :-
		!,
		^^format_resource_contents(Contents, Json),
		resolve_cache(resources_read, URI, TTL, Scope, Options),
		response({contents-Json, resultType-complete, ttlMs-TTL, cacheScope-Scope}, Id, Message).
	resource_result_body(error(Error), _, Id, _, Message) :-
		!,
		(	atom(Error) ->
			Text = Error
		;	write_to_atom(Error, Text)
		),
		error_response(-32603, Text, Id, Message).
	resource_result_body(Other, _, Id, _, Message) :-
		(	atom(Other) ->
			Text = Other
		;	write_to_atom(Other, Text)
		),
		error_response(-32603, Text, Id, Message).

	% subscriptions/listen — long-lived SSE stream
	%
	% Flow:
	% 1. Require a live SSE output stream (handler attaches it for listen).
	% 2. Acknowledge with a JSON-RPC result (SSE data event) and an optional
	%    notifications/subscriptions/acknowledged notification.
	% 3. Register the subscription and block on threaded_wait/1.
	% 4. Other workers call threaded_notify/1 for events or stop (cancel/cleanup).
	% 5. The wait loop writes matching events to the live SSE stream.

	handle_subscriptions_listen(Params, Id, HTTPResponse) :-
		(	^^has_pair(Params, filters, Filters0) ->
			Filters = Filters0
		;	Filters = []
		),
		(	atom(Id) ->
			S0 = Id
		;	number_codes(Id, Codes), atom_codes(S0, Codes)
		),
		atom_concat('sub_', S0, SubscriptionId),
		% live stream is required for a useful subscription
		(	sse_output_(Stream),
			sse_mode_(live) ->
			true
		;	% attempt buffered-only acknowledgment (client gets id but no push)
			Stream = none
		),
		AckResult = {
			resultType-complete,
			subscriptionId-SubscriptionId
		},
		response(AckResult, Id, AckMsg),
		AckNotification = {
			jsonrpc-'2.0',
			method-'notifications/subscriptions/acknowledged',
			params-{'subscriptionId'-SubscriptionId}
		},
		(	Stream \== none ->
			write_sse_data_event(Stream, AckMsg),
			flush_output(Stream),
			write_sse_data_event(Stream, AckNotification),
			flush_output(Stream),
			assertz(subscription_(SubscriptionId, Id, Filters, Stream)),
			start_subscription_keepalive(SubscriptionId),
			% block until cancelled / stream dies / cleanup (other threads notify)
			subscription_wait_loop(SubscriptionId, Stream),
			retractall(subscription_(SubscriptionId, _, _, _)),
			sse_headers(Headers),
			HTTPResponse = http_response(already_sent, Headers, text_body(''))
		;	% no stream: return JSON ack only (notify/1 cannot push)
			assertz(subscription_(SubscriptionId, Id, Filters, none)),
			json_result(Id, AckResult, HTTPResponse)
		).

	% block until stop or stream write failure; requires multi-threading:
	% notify/1, cancellation, and the keep-alive worker call threaded_notify/1
	subscription_wait_loop(SubscriptionId, Stream) :-
		catch(
			threaded_wait(subscription_msg(SubscriptionId, Msg)),
			_,
			Msg = stop
		),
		(	Msg == stop ->
			true
		;	Msg == keepalive ->
			(	write_subscription_keepalive(SubscriptionId, Stream) ->
				subscription_wait_loop(SubscriptionId, Stream)
			;	true  % stream dead
			)
		;	Msg = event(Event) ->
			(	push_subscription_event(SubscriptionId, Stream, Event) ->
				subscription_wait_loop(SubscriptionId, Stream)
			;	true  % stream dead
			)
		;	subscription_wait_loop(SubscriptionId, Stream)
		).

	% start a detached worker that periodically notifies keepalive for SubscriptionId
	% (interval 0 or negative disables keep-alive)
	start_subscription_keepalive(SubscriptionId) :-
		(	server_options_(Options),
			^^option(http_sse_keepalive(Seconds), Options),
			Seconds > 0 ->
			catch(
				threaded_ignore(subscription_keepalive_loop(SubscriptionId, Seconds)),
				Error,
				log_subscription_error(SubscriptionId, keepalive_start, Error)
			)
		;	true
		).

	subscription_keepalive_loop(SubscriptionId, Seconds) :-
		sleep(Seconds),
		(	subscription_(SubscriptionId, _, _, _) ->
			threaded_notify(subscription_msg(SubscriptionId, keepalive)),
			subscription_keepalive_loop(SubscriptionId, Seconds)
		;	true
		).

	write_subscription_keepalive(SubscriptionId, Stream) :-
		Stream \== none,
		catch(
			write_sse_comment(Stream),
			Error,
			(	log_subscription_error(SubscriptionId, keepalive, Error),
				fail
			)
		).

	% succeeds if the SSE write+flush completed; fails on stream errors
	push_subscription_event(SubscriptionId, Stream, Event) :-
		Stream \== none,
		event_to_notification(Event, SubscriptionId, Notification),
		catch(
			(	write_sse_data_event(Stream, Notification),
				flush_output(Stream)
			),
			Error,
			(	log_subscription_error(SubscriptionId, push, Error),
				fail
			)
		).

	% notify/1 fan-out — never fails; drops dead subscriptions
	dispatch_event([], _).
	dispatch_event([Subscription| Subscriptions], Event) :-
		dispatch_one(Subscription, Event),
		dispatch_event(Subscriptions, Event).

	dispatch_one(subscription_(SubscriptionId, RequestId, Filters, Stream), Event) :-
		(	event_matches(Event, Filters) ->
			(	Stream \== none ->
				threaded_notify(subscription_msg(SubscriptionId, event(Event)))
			;	drop_subscription(SubscriptionId, RequestId),
				log_subscription_error(SubscriptionId, drop, delivery_failed)
			)
		;	true
		).

	% best-effort removal of a dead subscription
	drop_subscription(SubscriptionId, RequestId) :-
		signal_subscription_stop(SubscriptionId),
		retractall(subscription_(SubscriptionId, RequestId, _, _)),
		retractall(subscription_(SubscriptionId, _, _, _)).

	log_subscription_error(SubscriptionId, Op, Error) :-
		format(
			user_error,
			'~w: subscription ~w ~w error: ~w~n',
			[mcp_server_streamable_http_transport, SubscriptionId, Op, Error]
		).

	event_matches(_, []) :-
		% empty filters match all
		!.
	event_matches(Event, Filters) :-
		event_type(Event, Type),
		member(F, Filters),
		filter_matches_type(F, Type),
		!.

	% Accept short names (tools), camelCase opt-in types from SEP-2575
	% (toolsListChanged, ...), and {type-...} objects.
	filter_matches_type(Filter, Type) :-
		^^has_pair(Filter, type, Type0),
		!,
		filter_type_atom(Type0, Type).
	filter_matches_type(Filter, Type) :-
		atom(Filter),
		filter_type_atom(Filter, Type).

	filter_type_atom(tools, tools).
	filter_type_atom(toolsListChanged, tools).
	filter_type_atom(prompts, prompts).
	filter_type_atom(promptsListChanged, prompts).
	filter_type_atom(resources, resources).
	filter_type_atom(resourcesListChanged, resources).
	filter_type_atom(resourceSubscriptions, resources).

	event_type(tools_list_changed, tools).
	event_type(prompts_list_changed, prompts).
	event_type(resources_list_changed, resources).
	event_type(resource_updated(_), resources).

	event_to_notification(tools_list_changed, SubscriptionId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/tools/list_changed',
			params-{'_meta'-{'io.modelcontextprotocol/subscriptionId'-SubscriptionId}}
		}.
	event_to_notification(prompts_list_changed, SubscriptionId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/prompts/list_changed',
			params-{'_meta'-{'io.modelcontextprotocol/subscriptionId'-SubscriptionId}}
		}.
	event_to_notification(resources_list_changed, SubscriptionId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/resources/list_changed',
			params-{'_meta'-{'io.modelcontextprotocol/subscriptionId'-SubscriptionId}}
		}.
	event_to_notification(resource_updated(URI), SubscriptionId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/resources/updated',
			params-{uri-URI, '_meta'-{'io.modelcontextprotocol/subscriptionId'-SubscriptionId}}
		}.

	resolve_cache(Op, Req, TTL, Scope, Options) :-
		^^option(application(Application), Options),
		(	conforms_to_protocol(Application, mcp_cache_protocol),
			Application::cache_policy(Op, Req, T0, S0),
			integer(T0), T0 >= 0, memberchk(S0, [public, private]) ->
			TTL = T0, Scope = S0
		;	^^option(cache_ttl(TTL), Options),
			^^option(cache_scope(Scope), Options)
		).

	json_result(Id, Result, http_response(200, ['Content-Type'-'application/json; charset=utf-8'], json_body(Msg))) :-
		response(Result, Id, Msg).

	json_error(Id, Code, Message, http_response(200, ['Content-Type'-'application/json; charset=utf-8'], json_body(JSON))) :-
		(	atom(Message) ->
			Msg = Message
		;	write_to_atom(Message, Msg)
		),
		error_response(Code, Msg, Id, JSON).

	json_error_data(Id, Code, Message, Data, http_response(200, ['Content-Type'-'application/json; charset=utf-8'], json_body(JSON))) :-
		(	atom(Message) ->
			Msg = Message
		;	write_to_atom(Message, Msg)
		),
		JSON = {jsonrpc-'2.0', id-Id, error-{code-Code, message-Msg, data-Data}}.

	header_value(Headers, Name, Value) :-
		member(Name-Value, Headers),
		!.
	header_value(Headers, Name, Value) :-
		atom_codes(Name, NameCodes), maplist_lower(NameCodes, NameLowerCodes), atom_codes(LowerName, NameLowerCodes),
		member(Header-Value, Headers),
		atom_codes(Header, HeaderCodes), maplist_lower(HeaderCodes, HeaderLowerCodes), atom_codes(LowerHeader, HeaderLowerCodes),
		LowerName == LowerHeader,
		!.

	maplist_lower([], []).
	maplist_lower([C|Cs], [L|Ls]) :-
		(	C >= 65, C =< 90 ->
			L is C + 32
		;	C =:= 45 ->
			L = 95
		;	L = C
		),
		maplist_lower(Cs, Ls).

	validate_origin(Headers) :-
		server_options_(Options),
		^^option(http_origin_check(Check), Options),
		(	Check == false -> true
		;	(	header_value(Headers, 'Origin', Origin) ->
				(	sub_atom(Origin, 0, _, _, 'http://localhost') -> true
				;	sub_atom(Origin, 0, _, _, 'http://127.0.0.1') -> true
				;	sub_atom(Origin, 0, _, _, 'https://localhost') -> true
				;	fail
				)
			;	true
			)
		).

	json_serialize(Term, Atom) :-
		generate(atom(Atom), Term).

	default_option(server_name('logtalk-mcp-server')).
	default_option(server_version('1.0.0')).
	default_option(server_title('logtalk-mcp-server')).
	default_option(instructions('')).
	default_option(cache_ttl(0)).
	default_option(cache_scope(private)).
	default_option(spec('2026-07-28')).
	default_option(http_port(8080)).
	default_option(http_bind('127.0.0.1')).
	default_option(http_path('/mcp')).
	default_option(http_origin_check(true)).
	default_option(http_sse_keepalive(15)).
	default_option(http_server_options([])).

	valid_option(transport(Transport)) :-
		once((Transport == stdio; Transport == streamable_http)).
	valid_option(server_name(Name)) :-
		atom(Name).
	valid_option(server_version(Version)) :-
		atom(Version).
	valid_option(server_title(Title)) :-
		atom(Title).
	valid_option(instructions(Instructions)) :-
		atom(Instructions).
	valid_option(cache_ttl(TTL)) :-
		number(TTL), TTL >= 0.
	valid_option(cache_scope(Scope)) :-
		once((Scope == (public) ; Scope == private)).
	valid_option(spec(Spec)) :-
		once((Spec == '2025-06-18'; Spec == '2025-11-25'; Spec == '2026-07-28')).
	valid_option(http_port(Port)) :-
		integer(Port), 0 < Port, Port =< 65535.
	valid_option(http_bind(Bind)) :-
		atom(Bind).
	valid_option(http_path(Path)) :-
		atom(Path).
	valid_option(http_origin_check(Flag)) :-
		once((Flag == true ; Flag == false)).
	valid_option(http_sse_keepalive(Seconds)) :-
		number(Seconds), Seconds >= 0.
	valid_option(http_server_options(Options)) :-
		http_server::valid_options(Options).
	valid_option(oauth(Verifier, ProtectedResource, MetadataDescriptors, ProtectOptions)) :-
		nonvar(Verifier),
		atom(ProtectedResource),
		type::valid(list(compound), MetadataDescriptors),
		type::valid(list(compound), ProtectOptions).

:- end_object.


:- object(mcp_streamable_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2026-08-26,
		comment is 'HTTP handler bridging http_server to mcp_server_streamable_http_transport. Uses http_core response/5 with json(Term) bodies for application/json.'
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(list, [
		append/3, member/2, valid/1 as is_list/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- uses(format, [
		format/3
	]).

	:- uses(json, [
		parse/2
	]).

	handle(Request, Response) :-
		catch(
			handle_checked(Request, Response0),
			Error,
			(	format(user_error, 'mcp_streamable_http_handler: ~q~n', [Error]),
				flush_output(user_error),
				plain_text_response(500, 'Internal Server Error', Response0)
			)
		),
		(	nonvar(Response0) ->
			(	http_core::is_response(Response0) ->
				Response = Response0
			;	format(user_error, 'INVALID RESP ~q~n', [Response0]),
				flush_output(user_error),
				plain_text_response(500, 'Invalid response term', Response)
			)
		;	plain_text_response(500, 'Internal Server Error', Response)
		).

	handle_checked(Request, Response) :-
		extract(Request, Method, Path, Headers, Body),
		(	mcp_server_streamable_http_transport::current_options(Opts),
			member(http_path(Expected), Opts) ->
			true
		;	Expected = '/mcp'
		),
		(	path_matches(Path, Expected) ->
			handle_path(Request, Method, Headers, Body, Response)
		;	plain_text_response(404, 'Not Found', Response)
		).

	path_matches(Path, Expected) :-
		Path == Expected,
		!.
	path_matches(Path, Expected) :-
		atom(Path),
		atom(Expected),
		atom_concat(Expected, '/', Path).

	handle_path(Request, Method, Headers, Body, Response) :-
		(	wants_sse(Headers, Body),
			open_live_sse_stream(Request, Stream, StreamKind) ->
			mcp_server_streamable_http_transport::sse_headers(SSEHeaders),
			start_streaming_response(Stream, StreamKind, SSEHeaders, Response0),
			mcp_server_streamable_http_transport::attach_sse_stream(Stream),
			catch(
				mcp_server_streamable_http_transport::handle_mcp_request(Method, Headers, Body, HTTPResp),
				Error,
				(	mcp_server_streamable_http_transport::detach_sse_stream,
					throw(Error)
				)
			),
			mcp_server_streamable_http_transport::detach_sse_stream,
			finish_streaming_response(Stream, StreamKind, HTTPResp, Response0, Response)
		;	% Non-SSE buffered path
			mcp_server_streamable_http_transport::handle_mcp_request(
				Method, Headers, Body, HTTPResp
			),
			http_response_to_core(HTTPResp, Response)
		).

	% Convert internal http_response/3 into validated http_core response/5
	http_response_to_core(http_response(already_sent, _, _), Response) :-
		!,
		http_core::response(
			http(1,1),
			status(200, 'OK'),
			[],
			empty,
			[],
			Response
		).
	http_response_to_core(http_response(Code, _Headers, json_body(Term)), Response) :-
		!,
		once(status_phrase(Code, Phrase)),
		http_core::response(
			http(1,1),
			status(Code, Phrase),
			[content_type-media_type('application/json', [])],
			content('application/json', json(Term)),
			[],
			Response
		).
	http_response_to_core(http_response(Code, Headers0, text_body(Text0)), Response) :-
		!,
		once(status_phrase(Code, Phrase)),
		(	atom(Text0) -> Text = Text0 ; write_to_atom(Text0, Text) ),
		media_type_from_headers(Headers0, Media),
		(	Media == 'application/json' ->
			% legacy atom JSON payload
			(	Text == '' ->
				Body = empty
			;	parse(atom(Text), Term),
				Body = content('application/json', json(Term))
			)
		;	(	Text == '' ->
				Body = empty
			;	Body = content(Media, text(Text))
			)
		),
		http_core::response(
			http(1,1),
			status(Code, Phrase),
			[content_type-media_type(Media, [])],
			Body,
			[],
			Response
		).
	http_response_to_core(http_response(Code, Headers0, Body0), Response) :-
		% Backward compatible: atom / codes body
		once(status_phrase(Code, Phrase)),
		media_type_from_headers(Headers0, Media),
		(	var(Body0) ->
			Body = empty
		;	Body0 == '' ->
			Body = empty
		;	atom(Body0), Media == 'application/json' ->
			parse(atom(Body0), Term),
			Body = content('application/json', json(Term))
		;	atom(Body0) ->
			Body = content(Media, text(Body0))
		;	is_list(Body0) ->
			atom_codes(Atom, Body0),
			(	Media == 'application/json' ->
				parse(atom(Atom), Term),
				Body = content('application/json', json(Term))
			;	Body = content(Media, text(Atom))
			)
		;	write_to_atom(Body0, Atom),
			Body = content(Media, text(Atom))
		),
		http_core::response(
			http(1,1),
			status(Code, Phrase),
			[content_type-media_type(Media, [])],
			Body,
			[],
			Response
		).

	plain_text_response(StatusCode, Text, Response) :-
		once(status_phrase(StatusCode, Phrase)),
		http_core::response(
			http(1,1),
			status(StatusCode, Phrase),
			[content_type-media_type('text/plain', [])],
			content('text/plain', text(Text)),
			[],
			Response
		).

	status_phrase(200, 'OK').
	status_phrase(202, 'Accepted').
	status_phrase(204, 'No Content').
	status_phrase(400, 'Bad Request').
	status_phrase(403, 'Forbidden').
	status_phrase(404, 'Not Found').
	status_phrase(405, 'Method Not Allowed').
	status_phrase(500, 'Internal Server Error').
	status_phrase(_, 'OK').

	media_type_from_headers(Headers, Media) :-
		(	member('Content-Type'-Type, Headers),
			atom(Type) ->
			(	sub_atom(Type, Before, 1, _, ';') ->
				sub_atom(Type, 0, Before, _, Media0)
			;	Media0 = Type
			),
			atom_codes(Media0, Codes),
			trim_spaces(Codes, Trimmed),
			atom_codes(Media, Trimmed)
		;	member(content_type-media_type(Media, _), Headers) ->
			true
		;	Media = 'application/json'
		).

	trim_spaces([C|Cs], Out) :-
		(C =:= 32 ; C =:= 9),
		!,
		trim_spaces(Cs, Out).
	trim_spaces(Codes, Codes).

	% Open a live SSE response only for progress or subscriptions/listen.
	% Do not key off Accept: text/event-stream — clients advertise it on every POST.
	wants_sse(_Headers, Body) :-
		(	sub_atom(Body, _, _, _, 'progressToken') ->
			true
		;	sub_atom(Body, _, _, _, 'subscriptions/listen') ->
			true
		;	fail
		).

	open_live_sse_stream(Request, Stream, StreamKind) :-
		(	request_output_stream(Request, Out) ->
			Stream = Out,
			StreamKind = connection
		;	open_response_pipe(Read, Write) ->
			Stream = Write,
			StreamKind = pipe(Read)
		;	fail
		).

	request_output_stream(Request, Output) :-
		(	Request = request(_M, _P, _H, _B, connection(Connection)) ->
			connection_output(Connection, Output)
		;	Request = request(_M, _P, _H, _B, _Rest),
			arg(5, Request, Extra),
			extra_connection(Extra, Connection) ->
			connection_output(Connection, Output)
		;	functor(Request, _, Arity), Arity >= 5,
			arg(5, Request, MaybeConn),
			MaybeConn = connection(Connection) ->
			connection_output(Connection, Output)
		;	fail
		).

	extra_connection(connection(C), C) :-
		!.
	extra_connection(Extra, C) :-
		compound(Extra),
		arg(1, Extra, C0),
		C0 = C.

	connection_output(Connection, Output) :-
		(	current_object(http_socket_transport),
			catch(http_socket_transport::connection_streams(Connection, _In, Output), _, fail) ->
			true
		;	current_object(http_process_transport),
			catch(http_process_transport::connection_streams(Connection, _In, Output), _, fail) ->
			true
		;	Connection = connection(_Sock, _In, Output) -> true
		;	fail
		).

	open_response_pipe(Read, Write) :-
		(	current_predicate(pipe/2) ->
			{pipe(Read, Write)}
		;	current_predicate(open_pipe_stream/2) ->
			{open_pipe_stream(Read, Write)}
		;	fail
		).

	start_streaming_response(Stream, connection, Headers0, Response) :-
		!,
		write_status_and_headers(Stream, 200, Headers0),
		flush_output(Stream),
		http_core::response(
			http(1,1),
			status(200, 'OK'),
			[content_type-media_type('text/event-stream', [])],
			empty,
			[],
			Response
		).
	start_streaming_response(_Stream, pipe(_Read), _Headers0, Response) :-
		http_core::response(
			http(1,1),
			status(200, 'OK'),
			[content_type-media_type('text/event-stream', [])],
			content('text/event-stream', text('')),
			[],
			Response
		).

	finish_streaming_response(Stream, connection, HTTPResp, Response0, Response) :-
		!,
		(	HTTPResp = http_response(already_sent, _, _) ->
			true
		;	HTTPResp = http_response(_, _, json_body(Term)) ->
			json::generate(atom(Atom), Term),
			atom_codes(Atom, Codes),
			write_codes(Codes, Stream),
			flush_output(Stream)
		;	HTTPResp = http_response(_, _, text_body(Body)),
			atom(Body), Body \== '' ->
			atom_codes(Body, Codes),
			write_codes(Codes, Stream),
			flush_output(Stream)
		;	HTTPResp = http_response(_, _, Body),
			atom(Body), Body \== '' ->
			atom_codes(Body, Codes),
			write_codes(Codes, Stream),
			flush_output(Stream)
		;	true
		),
		Response = Response0.
	finish_streaming_response(Stream, pipe(_Read), HTTPResp, Response0, Response) :-
		(	HTTPResp = http_response(already_sent, _, _) ->
			true
		;	HTTPResp = http_response(_, _, text_body(Body)),
			atom(Body), Body \== '' ->
			atom_codes(Body, Codes),
			write_codes(Codes, Stream)
		;	HTTPResp = http_response(_, _, Body),
			atom(Body), Body \== '' ->
			atom_codes(Body, Codes),
			write_codes(Codes, Stream)
		;	true
		),
		catch(close(Stream), _, true),
		Response = Response0.

	write_status_and_headers(Stream, Status, Headers) :-
		number_codes(Status, StatusCodes),
		atom_codes('HTTP/1.1 ', Prefix),
		Ok = [32, 79, 75, 13, 10],
		CRLF = [13, 10],
		write_codes(Prefix, Stream),
		write_codes(StatusCodes, Stream),
		write_codes(Ok, Stream),
		write_header_lines(Headers, Stream),
		write_codes(CRLF, Stream).

	write_header_lines([], _Stream).
	write_header_lines([Name-Value| Headers], Stream) :-
		header_line_atoms(Name, Value, NameAtom, ValueAtom),
		atom_codes(NameAtom, NC),
		atom_codes(ValueAtom, VC),
		atom_codes(': ', Colon),
		CRLF = [13, 10],
		write_codes(NC, Stream),
		write_codes(Colon, Stream),
		write_codes(VC, Stream),
		write_codes(CRLF, Stream),
		write_header_lines(Headers, Stream).

	header_line_atoms(content_type-media_type(Media, _), _, 'Content-Type', Media) :-
		!.
	header_line_atoms(Name, Value, Name, ValueAtom) :-
		atom(Name),
		!,
		(	atom(Value) -> ValueAtom = Value
		;	write_to_atom(Value, ValueAtom)
		).
	header_line_atoms(Name, Value, NameAtom, ValueAtom) :-
		write_to_atom(Name, NameAtom),
		(	atom(Value) -> ValueAtom = Value
		;	write_to_atom(Value, ValueAtom)
		).

	write_codes([], _Stream) :-
		!.
	write_codes([C|Cs], Stream) :-
		(	catch(put_byte(Stream, C), _, fail) -> true
		;	put_code(Stream, C)
		),
		write_codes(Cs, Stream).

	extract(Request, Method, Path, Headers, Body) :-
		(	Request = request(M0, Path0, _HTTPVersion, Headers0, Content, Meta) ->
			upcase(M0, Method),
			normalize_path(Path0, Path),
			normalize_headers(Headers0, Headers1),
			(	is_list(Meta) ->
				normalize_meta_headers(Meta, Headers2),
				append(Headers1, Headers2, Headers)
			;	Headers = Headers1
			),
			normalize_body(Content, Meta, Body)
		;	Request = request(M0, Path0, Headers0, Body0) ->
			upcase(M0, Method),
			normalize_path(Path0, Path),
			normalize_headers(Headers0, Headers),
			body_atom(Body0, Body)
		;	Request = request(M0, Path0, Headers0, Body0, _) ->
			upcase(M0, Method),
			normalize_path(Path0, Path),
			normalize_headers(Headers0, Headers),
			body_atom(Body0, Body)
		;	arg(1, Request, M0),
			arg(2, Request, Path0),
			(arg(3, Request, Headers0) -> true ; Headers0 = []),
			(arg(4, Request, Body0) -> true ; Body0 = ''),
			upcase(M0, Method),
			normalize_path(Path0, Path),
			normalize_headers(Headers0, Headers),
			body_atom(Body0, Body)
		).

	normalize_path(origin(Path), Path) :-
		!,
		atom(Path).
	normalize_path(Path, Path) :-
		atom(Path).

	normalize_body(content(_MediaType, json(Term)), _Meta, Body) :-
		!,
		(	current_object(json) ->
			json::generate(atom(Body), Term)
		;	write_to_atom(Term, Body)
		).
	normalize_body(content(_MediaType, text(Text)), _Meta, Body) :-
		!,
		(	atom(Text) -> Body = Text ; write_to_atom(Text, Body) ).
	normalize_body(content(_MediaType, binary(Codes)), _Meta, Body) :-
		is_list(Codes),
		!,
		atom_codes(Body, Codes).
	normalize_body(content(_MediaType, Body0), _Meta, Body) :-
		atom(Body0),
		!,
		Body = Body0.
	normalize_body(content(_MediaType, Codes), _Meta, Body) :-
		is_list(Codes),
		!,
		atom_codes(Body, Codes).
	normalize_body(_Content, Meta, Body) :-
		is_list(Meta),
		member(entity_body_bytes(Codes), Meta),
		!,
		atom_codes(Body, Codes).
	normalize_body(_Content, _Meta, '').

	normalize_headers([], []).
	normalize_headers([H| Hs], [N| Ns]) :-
		normalize_header(H, N),
		normalize_headers(Hs, Ns).

	normalize_header(Name-Value, AtomName-AtomValue) :-
		!,
		header_name_atom(Name, AtomName),
		header_value_atom(Value, AtomValue).
	normalize_header(Other, Other).

	normalize_meta_headers([], []).
	normalize_meta_headers([content_type(Type, _Params)| Meta], ['Content-Type'-Type| Headers]) :-
		!,
		normalize_meta_headers(Meta, Headers).
	normalize_meta_headers([content_length(N)| Meta], ['Content-Length'-A| Headers]) :-
		!,
		(number(N) -> number_codes(N, C), atom_codes(A, C) ; atom(N) -> A = N ; write_to_atom(N, A)),
		normalize_meta_headers(Meta, Headers).
	normalize_meta_headers([_| Meta], Headers) :-
		normalize_meta_headers(Meta, Headers).

	header_name_atom(Name, Atom) :-
		(	atom(Name) ->
			Atom = Name
		;	write_to_atom(Name, Atom)
		).

	header_value_atom(media_type(Type, _Params), Type) :-
		!,
		atom(Type).
	header_value_atom(host(Host, Port), Atom) :-
		!,
		(	atom(Host) -> H = Host ; write_to_atom(Host, H) ),
		(	integer(Port) -> number_codes(Port, PC), atom_codes(P, PC) ; write_to_atom(Port, P) ),
		atomic_list_concat([H, ':', P], Atom).
	header_value_atom(Value, Value) :-
		atom(Value),
		!.
	header_value_atom(Value, Atom) :-
		integer(Value),
		!,
		number_codes(Value, Codes),
		atom_codes(Atom, Codes).
	header_value_atom(Value, Atom) :-
		write_to_atom(Value, Atom).

	upcase(A0, A) :-
		(atom(A0) -> atom_codes(A0, C) ; write_to_atom(A0, T), atom_codes(T, C)),
		map_upper(C, U), atom_codes(A, U).

	map_upper([], []).
	map_upper([C|Cs], [U|Us]) :-
		(C >= 97, C =< 122 -> U is C - 32 ; U = C), map_upper(Cs, Us).

	body_atom(B0, B) :-
		(	atom(B0) -> B = B0
		;	var(B0) -> B = ''
		;	B0 == [] -> B = ''
		;	is_list(B0) -> atom_codes(B, B0)
		;	write_to_atom(B0, B)
		).

:- end_object.
