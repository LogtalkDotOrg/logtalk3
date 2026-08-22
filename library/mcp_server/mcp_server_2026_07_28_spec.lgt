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


:- object(mcp_server_2026_07_28_spec,
	implements(mcp_server_spec_protocol),
	imports(mcp_server_application)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-22,
		comment is 'MCP 2026-07-28 protocol handler. Returns reply/1, reply_with_progress/2, subscribe/3, accepted, or no_reply outcomes. Does not write to streams; transports render outcomes.'
	]).

	:- uses(json_rpc, [
		response/3, error_response/4, method_not_found/2, is_request/1, is_notification/1, id/2, method/2,
		params/2, write_message/2, read_message/2
	]).

	:- uses(list, [
		member/2, memberchk/2, length/2, append/3
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	% dynamic state

	:- private(output_stream_/1).
	:- dynamic(output_stream_/1).

	:- private(subscription_/3).
	:- dynamic(subscription_/3).
	% subscription_(SubId, RequestId, Filters)

	:- private(running_/0).
	:- dynamic(running_/0).

	:- private(reply_outcome_/1).
	:- dynamic(reply_outcome_/1).

	:- private(progress_buffer_/1).
	:- dynamic(progress_buffer_/1).

	:- private(current_options_/1).
	:- dynamic(current_options_/1).

	% implemented spec

	spec('2026-07-28').

	prepare(Application, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		(	catch(Application::capabilities(Capabilities), _, fail) -> true
		;	Capabilities = []
		),
		Options = [application(Application), application_capabilities(Capabilities)| Options0],
		retractall(current_options_(_)),
		assertz(current_options_(Options)),
		setup_state(user_output).

	:- public(run_stdio_loop/3).
	:- mode(run_stdio_loop(+stream, +stream, +list), one).
	:- info(run_stdio_loop/3, [
		comment is 'Stdio read/dispatch loop used by mcp_server_2026_07_28_adapter.',
		argnames is ['Input', 'Output', 'Options']
	]).

	run_stdio_loop(Input, Output, Options) :-
		retractall(current_options_(_)),
		assertz(current_options_(Options)),
		setup_state(Output),
		catch(
			server_loop(Input, Output, Options),
			Error,
			(cleanup, throw(Error))
		).

	handle_message(Message, Options, Outcome) :-
		retractall(reply_outcome_(_)),
		retractall(progress_buffer_(_)),
		assertz(progress_buffer_([])),
		retractall(current_options_(_)),
		assertz(current_options_(Options)),
		(	is_notification(Message) ->
			handle_notification(Message, _),
			Outcome = accepted
		;	is_request(Message) ->
			handle_request(Message, _, user_output, Options),
			collect_outcome(Outcome)
		;	Outcome = no_reply
		).

	collect_outcome(Outcome) :-
		(	retract(reply_outcome_(Outcome0)) -> true
		;	Outcome0 = no_reply
		),
		(	retract(progress_buffer_(Events)) -> true ; Events = [] ),
		(	Outcome0 = reply(Final), Events = [_| _] ->
			Outcome = reply_with_progress(Events, Final)
		;	Outcome0 = subscribe(_, _, _) ->
			Outcome = Outcome0
		;	Outcome = Outcome0
		).

	notify(Event) :-
		(	output_stream_(Output) ->
			findall(SubId-Filters, subscription_(SubId, _, Filters), Subs),
			dispatch_event(Subs, Event, Output)
		;	true
		).

	cleanup :-
		retractall(running_),
		retractall(output_stream_(_)),
		retractall(subscription_(_, _, _)),
		retractall(reply_outcome_(_)),
		retractall(progress_buffer_(_)),
		retractall(current_options_(_)).

	% setup

	setup_state(Output) :-
		% do not call full cleanup/0 here: run_stdio_loop/prepare may have
		% already installed current_options_/1 and other session state
		retractall(running_),
		retractall(output_stream_(_)),
		retractall(reply_outcome_(_)),
		retractall(progress_buffer_(_)),
		assertz(output_stream_(Output)),
		assertz(running_).

	% synchronous server loop
	%
	% reader accepts one message at a time and dispatches requests to
	% completion before reading the next message

	server_loop(Input, Output, Options) :-
		(	running_ ->
			(	catch(read_message(Input, Message), Error, (log_error(Error), fail)) ->
				handle_incoming(Message, Input, Output, Options),
				server_loop(Input, Output, Options)
			;	% EOF
				true
			)
		;	true
		).

	log_error(Error) :-
		writeq(user_error, Error), nl(user_error).

	handle_incoming(Message, Input, Output, Options) :-
		retractall(reply_outcome_(_)),
		retractall(progress_buffer_(_)),
		assertz(progress_buffer_([])),
		(	is_request(Message) ->
			handle_request(Message, Input, Output, Options),
			collect_outcome(Outcome),
			render_stdio_outcome(Outcome, Output)
		;	is_notification(Message) ->
			handle_notification(Message, Output)
		;	true
		).

	render_stdio_outcome(reply(Response), Output) :-
		!,
		write_message(Output, Response),
		flush_output(Output).
	render_stdio_outcome(reply_with_progress(Events, Final), Output) :-
		!,
		forall(member(E, Events), (write_message(Output, E), flush_output(Output))),
		write_message(Output, Final),
		flush_output(Output).
	render_stdio_outcome(subscribe(_Id, _Filters, Messages), Output) :-
		!,
		forall(member(M, Messages), (write_message(Output, M), flush_output(Output))).
	render_stdio_outcome(accepted, _) :-
		!.
	render_stdio_outcome(no_reply, _) :-
		!.
	render_stdio_outcome(_, _).

	% request handling with validation

	handle_request(Message, _Input, Output, Options) :-
		id(Message, Id),
		(	valid_request_id(Id) ->
			method(Message, Method),
			(	params(Message, Params0) ->
				Params = Params0
			;	Params = {}
			),
			(	validate_2026_request(Method, Params, Id, Output) ->
				dispatch_method(Method, Params, Id, Output, Options)
			;	% Validation already recorded an error outcome
				true
			)
		;	% ignore invalid id (cannot send a correlated response)
			true
		).

	valid_request_id(Id) :-
		(	atom(Id), Id \== '' -> true
		;	integer(Id) -> true
		;	fail
		).

	% 2026 request validation

	validate_2026_request(Method, Params, Id, Output) :-
		% _meta is required
		(	^^has_pair(Params, '_meta', Meta) ->
			true
		;	send_error(Id, -32602, 'Missing required params._meta', Output),
			!, fail
		),
		% protocol version
		(	^^has_pair(Meta, 'io.modelcontextprotocol/protocolVersion', Version) ->
			true
		;	send_error(Id, -32602, 'Missing required protocolVersion in _meta', Output),
			!, fail
		),
		(	Version == '2026-07-28' ->
			true
		;	ErrorData = {supported-['2026-07-28'], requested-Version},
			send_error_data(Id, -32022, 'Unsupported protocol version', ErrorData, Output),
			!, fail
		),
		% client capabilities
		(	^^has_pair(Meta, 'io.modelcontextprotocol/clientCapabilities', _) ->
			true
		;	send_error(Id, -32602, 'Missing required clientCapabilities in _meta', Output),
			!, fail
		),
		% method-specific capability checks
		check_method_capabilities(Method, Meta, Id, Output).

	check_method_capabilities(Method, Meta, Id, Output) :-
		^^has_pair(Meta, 'io.modelcontextprotocol/clientCapabilities', ClientCaps),
		(	method_requires_capability(Method, Required) ->
			(	has_capability(ClientCaps, Required) ->
				true
			;	ErrorData = {requiredCapabilities-[Required]},
				send_error_data(Id, -32021, 'Missing required client capability', ErrorData, Output),
				!,
				fail
			)
		;	true
		).

	method_requires_capability('tools/call', tools).
	method_requires_capability('tools/list', tools).
	method_requires_capability('prompts/get', prompts).
	method_requires_capability('prompts/list', prompts).
	method_requires_capability('resources/read', resources).
	method_requires_capability('resources/list', resources).
	method_requires_capability('subscriptions/listen', subscriptions).

	has_capability(ClientCaps, Cap) :-
		^^has_pair(ClientCaps, Cap, _).

	% method dispatch

	dispatch_method(Method, Params, Id, Output, Options) :-
		catch(
			do_dispatch(Method, Params, Id, Output, Options),
			Error,
			send_error(Id, -32603, Error, Output)
		).

	do_dispatch(Method, Params, Id, Output, Options) :-
		(	do_dispatch_(Method, Params, Id, Output, Options) ->
			true
		;	method_not_found(Id, ErrorResponse),
			write_locked(Output, ErrorResponse)
		).

	do_dispatch_('server/discover', Params, Id, Output, Options) :-
		handle_discover(Params, Id, Output, Options).
	do_dispatch_(initialize, _Params, Id, Output, _) :-
		send_error(Id, -32600, 'initialize is not used in MCP 2026-07-28; use server/discover', Output).
	do_dispatch_(ping, _Params, Id, Output, _) :-
		send_complete(Id, {}, Output).
	do_dispatch_('tools/list', Params, Id, Output, Options) :-
		handle_tools_list(Params, Id, Output, Options).
	do_dispatch_('tools/call', Params, Id, Output, Options) :-
		handle_tools_call(Params, Id, Output, Options).
	do_dispatch_('prompts/list', Params, Id, Output, Options) :-
		handle_prompts_list(Params, Id, Output, Options).
	do_dispatch_('prompts/get', Params, Id, Output, Options) :-
		handle_prompts_get(Params, Id, Output, Options).
	do_dispatch_('resources/list', Params, Id, Output, Options) :-
		handle_resources_list(Params, Id, Output, Options).
	do_dispatch_('resources/read', Params, Id, Output, Options) :-
		handle_resources_read(Params, Id, Output, Options).
	do_dispatch_('subscriptions/listen', Params, Id, Output, _) :-
		handle_subscriptions_listen(Params, Id, Output).

	% notifications

	handle_notification(Message, _Output) :-
		method(Message, Method),
		(	Method == 'notifications/cancelled' ->
			handle_cancelled(Message)
		;	Method == 'notifications/initialized' ->
			% not used in 2026; ignore
			true
		;	true
		).

	% best-effort cancellation: drop any subscription opened under this request id;
	% under synchronous dispatch, ordinary in-flight requests cannot be interrupted
	% mid-handler; their responses may already have been written
	handle_cancelled(Message) :-
		(	params(Message, Params),
			^^has_pair(Params, requestId, ReqId) ->
			retractall(subscription_(_, ReqId, _))
		;	true
		).

	% server/discover

	handle_discover(_Params, Id, Output, Options) :-
		^^option(server_name(Name), Options, server_name('logtalk-mcp-server')),
		^^option(server_version(Version), Options, server_version('1.0.0')),
		^^option(server_title(Title), Options, server_title('logtalk-mcp-server')),
		^^option(instructions(Instructions), Options, instructions('')),
		^^option(application_capabilities(ApplicationCapabilities), Options, application_capabilities([])),
		build_server_capabilities(ApplicationCapabilities, Capabilities),
		resolve_cache(discover, {}, TTL, Scope, Options),
		ServerInfo = {name-Name, title-Title, version-Version},
		Meta = {'io.modelcontextprotocol/serverInfo'-ServerInfo},
		(	Instructions == '' ->
			Result0 = {
				supportedVersions-['2026-07-28'],
				capabilities-Capabilities,
				resultType-complete,
				ttlMs-TTL,
				cacheScope-Scope,
				'_meta'-Meta
			}
		;	Result0 = {
				supportedVersions-['2026-07-28'],
				capabilities-Capabilities,
				instructions-Instructions,
				resultType-complete,
				ttlMs-TTL,
				cacheScope-Scope,
				'_meta'-Meta
			}
		),
		send_result(Id, Result0, Output).

	build_server_capabilities(ApplicationCapabilities, Capabilities) :-
		Base = [tools-{}],
		(	member(prompts, ApplicationCapabilities) ->
			Capabilities0 = [prompts-{}| Base]
		;	Capabilities0 = Base
		),
		(	member(resources, ApplicationCapabilities) ->
			Capabilities1 = [resources-{}| Capabilities0]
		;	Capabilities1 = Capabilities0
		),
		% subscriptions are always offered by the 2026 adapter
		Capabilities2 = [subscriptions-{}| Capabilities1],
		^^pairs_to_curly(Capabilities2, Capabilities).

	% tools/list

	handle_tools_list(_Params, Id, Output, Options) :-
		^^option(application(Application), Options),
		Application::tools(ToolDescriptors),
		^^tool_descriptors_to_json(ToolDescriptors, Application, JsonTools),
		resolve_cache(tools_list, {}, TTL, Scope, Options),
		Result = {
			tools-JsonTools,
			resultType-complete,
			ttlMs-TTL,
			cacheScope-Scope
		},
		send_result(Id, Result, Output).

	% tools/call (with MRTR)

	handle_tools_call(Params, Id, Output, Options) :-
		(	^^has_pair(Params, name, ToolName) ->
			true
		;	send_error(Id, -32602, 'Missing tool name', Output),
			!
		),
		(	^^has_pair(Params, arguments, ToolArguments) ->
			true
		;	ToolArguments = {}
		),
		extract_input_responses(Params, InputResponses),
		extract_request_state(Params, RequestState),
		extract_progress_token(Params, ProgressToken),
		extract_client_capabilities(Params, ClientCaps),
		^^option(application(Application), Options),
		(	Application::tools(ToolDescriptors),
			member(tool(ToolName, Functor, Arity), ToolDescriptors) ->
			execute_tool_round(
				Application, ToolName, Functor, Arity,
				ToolArguments, ClientCaps, InputResponses, RequestState,
				ProgressToken, Id, Output, Options
			)
		;	send_error(Id, -32602, 'Unknown tool', Output)
		).

	execute_tool_round(Application, ToolName, Functor, Arity, ToolArguments,
			ClientCaps, InputResponses, RequestState, ProgressToken, Id, Output, Options) :-
		^^curly_to_pairs(ToolArguments, ArgPairs),
		make_progress_closure(ProgressToken, Id, Output, Progress),
		Context = request_context(ClientCaps, InputResponses, RequestState, Progress),
		(	catch(
				Application::tool_call_round(ToolName, ArgPairs, Context, RoundResult),
				error(existence_error(procedure, _), _),
				fail
			) ->
			handle_round_result(RoundResult, tool, ToolName, Id, Output, Options)
		;	% Fall back to tool_call/3 or auto-dispatch, wrap as complete
			(	catch(
					^^try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result),
					Error,
					Result = error(Error)
				) ->
				handle_round_result(complete(Result), tool, ToolName, Id, Output, Options)
			;	handle_round_result(complete(error('Tool execution failed')), tool, ToolName, Id, Output, Options)
			)
		).

	% prompts/list

	handle_prompts_list(_Params, Id, Output, Options) :-
		^^option(application(Application), Options),
		(	catch(Application::prompts(PromptDescriptors), _, fail) ->
			^^prompt_descriptors_to_json(PromptDescriptors, JsonPrompts)
		;	JsonPrompts = []
		),
		resolve_cache(prompts_list, {}, TTL, Scope, Options),
		Result = {
			prompts-JsonPrompts,
			resultType-complete,
			ttlMs-TTL,
			cacheScope-Scope
		},
		send_result(Id, Result, Output).

	% prompts/get (with MRTR)

	handle_prompts_get(Params, Id, Output, Options) :-
		(	^^has_pair(Params, name, PromptName) ->
			true
		;	send_error(Id, -32602, 'Missing prompt name', Output),
			!
		),
		(	^^has_pair(Params, arguments, PromptArguments) ->
			true
		;	PromptArguments = {}
		),
		extract_input_responses(Params, InputResponses),
		extract_request_state(Params, RequestState),
		extract_progress_token(Params, ProgressToken),
		extract_client_capabilities(Params, ClientCaps),
		^^option(application(Application), Options),
		(	catch(Application::prompts(PromptDescriptors), _, fail),
			(member(prompt(PromptName, _, _), PromptDescriptors)
			; member(prompt(PromptName, _, _, _), PromptDescriptors)) ->
			execute_prompt_round(
				Application, PromptName, PromptArguments,
				ClientCaps, InputResponses, RequestState,
				ProgressToken, Id, Output, Options
			)
		;	send_error(Id, -32602, 'Unknown prompt', Output)
		).

	execute_prompt_round(Application, PromptName, PromptArguments,
			ClientCaps, InputResponses, RequestState, ProgressToken, Id, Output, Options) :-
		^^curly_to_pairs(PromptArguments, ArgPairs),
		make_progress_closure(ProgressToken, Id, Output, Progress),
		Context = request_context(ClientCaps, InputResponses, RequestState, Progress),
		(	catch(
				Application::prompt_get_round(PromptName, ArgPairs, Context, RoundResult),
				error(existence_error(procedure, _), _),
				fail
			) ->
			handle_round_result(RoundResult, prompt, PromptName, Id, Output, Options)
		;	(	catch(
					Application::prompt_get(PromptName, ArgPairs, Result),
					Error,
					Result = error(Error)
				) ->
				handle_round_result(complete(Result), prompt, PromptName, Id, Output, Options)
			;	send_error(Id, -32603, 'Prompt execution failed', Output)
			)
		).

	% resources/list

	handle_resources_list(_Params, Id, Output, Options) :-
		^^option(application(Application), Options),
		(	catch(Application::resources(ResourceDescriptors), _, fail) ->
			^^resource_descriptors_to_json(ResourceDescriptors, JsonResources)
		;	JsonResources = []
		),
		resolve_cache(resources_list, {}, TTL, Scope, Options),
		Result = {
			resources-JsonResources,
			resultType-complete,
			ttlMs-TTL,
			cacheScope-Scope
		},
		send_result(Id, Result, Output).

	% resources/read (with MRTR + cache)

	handle_resources_read(Params, Id, Output, Options) :-
		^^has_pair(Params, uri, URI),
		!,
		extract_input_responses(Params, InputResponses),
		extract_request_state(Params, RequestState),
		extract_progress_token(Params, ProgressToken),
		extract_client_capabilities(Params, ClientCaps),
		^^option(application(Application), Options),
		(	catch(Application::resources(ResourceDescriptors), _, fail),
			(	member(resource(URI, _, _, _), ResourceDescriptors)
			;	member(resource(URI, _, _, _, _), ResourceDescriptors)
			) ->
			execute_resource_round(
				Application, URI,
				ClientCaps, InputResponses, RequestState,
				ProgressToken, Id, Output, Options
			)
		;	send_error(Id, -32602, 'Unknown resource', Output)
		).
	handle_resources_read(_Params, Id, Output, _Options) :-
		send_error(Id, -32602, 'Missing resource uri', Output).

	execute_resource_round(Application, URI, ClientCaps, InputResponses, RequestState, ProgressToken, Id, Output, Options) :-
		make_progress_closure(ProgressToken, Id, Output, Progress),
		Context = request_context(ClientCaps, InputResponses, RequestState, Progress),
		(	catch(
				Application::resource_read_round(URI, [], Context, RoundResult),
				error(existence_error(procedure, _), _),
				fail
			) ->
			handle_round_result(RoundResult, resource, URI, Id, Output, Options)
		;	(	catch(
					Application::resource_read(URI, [], Result),
					Error,
					Result = error(Error)
				) ->
				handle_round_result(complete(Result), resource, URI, Id, Output, Options)
			;	send_error(Id, -32603, 'Resource read failed', Output)
			)
		).

	% round result handling (complete vs input_required)

	handle_round_result(complete(Result), Kind, Key, Id, Output, Options) :-
		!,
		format_complete_result(Result, Kind, Key, Id, Output, Options).
	handle_round_result(input_required(InputRequests, RequestState), _Kind, _Key, Id, Output, _) :-
		!,
		validate_input_required(InputRequests, RequestState, Id, Output).
	handle_round_result(Other, _Kind, _Key, Id, Output, _) :-
		send_error(Id, -32603, Other, Output).

	validate_input_required(InputRequests, RequestState, Id, Output) :-
		(	(InputRequests = [_| _] ; RequestState \== none) ->
			(	unique_keys(InputRequests) ->
				input_requests_to_json(InputRequests, JsonRequests),
				(	RequestState == none ->
					Result = {
						resultType-input_required,
						inputRequests-JsonRequests
					}
				;	Result = {
						resultType-input_required,
						inputRequests-JsonRequests,
						requestState-RequestState
					}
				),
				send_result(Id, Result, Output)
			;	send_error(Id, -32602, 'Duplicate input request keys', Output)
			)
		;	send_error(Id, -32602, 'input_required must have nonempty requests or non-none state', Output)
		).

	unique_keys(Requests) :-
		findall(K, member(input_request(K, _), Requests), Keys),
		sort(Keys, Sorted),
		length(Keys, N),
		length(Sorted, N).

	input_requests_to_json([], []).
	input_requests_to_json([input_request(Key, Request)| Rest], [Json| JsonRest]) :-
		request_to_json(Request, Key, Json),
		input_requests_to_json(Rest, JsonRest).

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

	% format complete results for 2026 wire shape

	format_complete_result(text(Text), tool, _, Id, Output, _) :-
		!,
		Content = [{type-text, text-Text}],
		Result = {content-Content, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(error(Error), tool, _, Id, Output, _) :-
		!,
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		Content = [{type-text, text-ErrorText}],
		Result = {content-Content, isError- @true, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(results(Items), tool, _, Id, Output, _) :-
		!,
		^^format_content_items(Items, Content),
		Result = {content-Content, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(structured(StructuredContent), tool, _, Id, Output, _) :-
		!,
		write_to_atom(StructuredContent, Text),
		Content = [{type-text, text-Text}],
		Result = {content-Content, structuredContent-StructuredContent, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(structured(Items, StructuredContent), tool, _, Id, Output, _) :-
		!,
		^^format_content_items(Items, Content),
		Result = {content-Content, structuredContent-StructuredContent, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(messages(Messages), prompt, _, Id, Output, _) :-
		!,
		^^format_prompt_messages(Messages, JsonMessages),
		Result = {messages-JsonMessages, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(messages(Description, Messages), prompt, _, Id, Output, _) :-
		!,
		^^format_prompt_messages(Messages, JsonMessages),
		Result = {description-Description, messages-JsonMessages, resultType-complete},
		send_result(Id, Result, Output).
	format_complete_result(contents(Contents), resource, URI, Id, Output, Options) :-
		!,
		^^format_resource_contents(Contents, JsonContents),
		resolve_cache(resources_read, URI, TTL, Scope, Options),
		Result = {
			contents-JsonContents,
			resultType-complete,
			ttlMs-TTL,
			cacheScope-Scope
		},
		send_result(Id, Result, Output).
	format_complete_result(error(Error), prompt, _, Id, Output, _) :-
		!,
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		send_error(Id, -32603, ErrorText, Output).
	format_complete_result(error(Error), resource, _, Id, Output, _) :-
		!,
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		send_error(Id, -32603, ErrorText, Output).
	format_complete_result(Other, _, _, Id, Output, _) :-
		send_error(Id, -32603, Other, Output).

	% progress

	make_progress_closure(none, _, _, Progress) :-
		!,
		Progress = [_,_,_]>>(true).
	make_progress_closure(Token, RequestId, Output, Progress) :-
		Progress = {Token, RequestId, Output}/[ProgressValue, Total, Message]>>(
			mcp_server_2026_07_28_spec::emit_progress(Token, RequestId, Output, ProgressValue, Total, Message)
		).

	:- public(emit_progress/6).
	:- mode(emit_progress(+term, +term, +stream, +number, +number, +atom), one).
	:- info(emit_progress/6, [
		comment is 'Buffers a notifications/progress event or invokes progress_hook/1 from current options.',
		argnames is ['Token', 'RequestId', 'Output', 'ProgressValue', 'Total', 'Message']
	]).

	emit_progress(Token, _RequestId, _Output, ProgressValue, Total, Message) :-
		Params = {
			progressToken-Token,
			progress-ProgressValue,
			total-Total,
			message-Message
		},
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/progress',
			params-Params
		},
		(	current_options_(Options),
			member(progress_hook(Hook), Options) ->
			catch(call(Hook, Notification), _, true)
		;	(	retract(progress_buffer_(Events0)) -> true ; Events0 = [] ),
			append(Events0, [Notification], Events1),
			assertz(progress_buffer_(Events1))
		).

	% subscriptions

	handle_subscriptions_listen(Params, Id, _Output) :-
		(	^^has_pair(Params, filters, Filters0) ->
			Filters = Filters0
		;	Filters = []
		),
		(	atom(Id) -> SubId = Id
		;	number_codes(Id, Codes), atom_codes(SubId, Codes)
		),
		atom_concat('sub_', SubId, SubscriptionId),
		AckResult = {resultType-complete, subscriptionId-SubscriptionId},
		response(AckResult, Id, AckResponse),
		AckNotification = {
			jsonrpc-'2.0',
			method-'notifications/subscriptions/acknowledged',
			params-{'subscriptionId'-SubscriptionId}
		},
		assertz(subscription_(SubscriptionId, Id, Filters)),
		retractall(reply_outcome_(_)),
		assertz(reply_outcome_(subscribe(SubscriptionId, Filters, [AckResponse, AckNotification]))).

	dispatch_event([], _Event, _).
	dispatch_event([SubId-Filters| Rest], Event, Output) :-
		(	event_matches(Filters, Event) ->
			event_to_notification(Event, SubId, Notification),
			write_message(Output, Notification),
			flush_output(Output)
		;	true
		),
		dispatch_event(Rest, Event, Output).

	event_matches([], _Event) :-
		% empty filters match all
		!.
	event_matches(Filters, Event) :-
		event_type(Event, Type),
		member(Filter, Filters),
		(	^^has_pair(Filter, type, Type) ->
			true
		;	Filter == Type
		),
		!.

	event_type(tools_list_changed, tools).
	event_type(prompts_list_changed, prompts).
	event_type(resources_list_changed, resources).
	event_type(resource_updated(_), resources).

	event_to_notification(tools_list_changed, SubId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/tools/list_changed',
			params-{'_meta'-{'io.modelcontextprotocol/subscriptionId'-SubId}}
		}.
	event_to_notification(prompts_list_changed, SubId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/prompts/list_changed',
			params-{'_meta'-{'io.modelcontextprotocol/subscriptionId'-SubId}}
		}.
	event_to_notification(resources_list_changed, SubId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/resources/list_changed',
			params-{'_meta'-{'io.modelcontextprotocol/subscriptionId'-SubId}}
		}.
	event_to_notification(resource_updated(URI), SubId, Notification) :-
		Notification = {
			jsonrpc-'2.0',
			method-'notifications/resources/updated',
			params-{uri-URI, '_meta'-{'io.modelcontextprotocol/subscriptionId'-SubId}}
		}.

	% cache resolution

	resolve_cache(Operation, Request, TTL, Scope, Options) :-
		(	member(application(Application), Options) ->
			(	catch(Application::cache_policy(Operation, Request, TTL0, Scope0), _, fail),
				integer(TTL0), TTL0 >= 0,
				memberchk(Scope0, [public, private]) ->
				TTL = TTL0, Scope = Scope0
			;	^^option(cache_ttl(TTL), Options, cache_ttl(0)),
				^^option(cache_scope(Scope), Options, cache_scope(private))
			)
		;	^^option(cache_ttl(TTL), Options, cache_ttl(0)),
			^^option(cache_scope(Scope), Options, cache_scope(private))
		).

	% parameter extraction helpers

	extract_input_responses(Params, Responses) :-
		(	^^has_pair(Params, inputResponses, Raw) ->
			normalize_input_responses(Raw, Responses)
		;	Responses = []
		).

	normalize_input_responses([], []) :-
		!.
	normalize_input_responses([Item| Items], [input_response(Key, Value)| Responses]) :-
		!,
		(	^^has_pair(Item, key, Key) ->
			true
		;	Key = unknown
		),
		(	^^has_pair(Item, value, Value) ->
			true
		;	^^has_pair(Item, action, Action) ->
			(	Action == accept,
				^^has_pair(Item, content, Content) ->
				Value = accept(Content)
			;	Action == accept ->
				Value = accept({})
			;	Action == decline ->
				Value = decline
			;	Value = cancel
			)
		;	Value = Item
		),
		normalize_input_responses(Items, Responses).
	normalize_input_responses(_, []).

	extract_request_state(Params, State) :-
		(	^^has_pair(Params, requestState, State0) ->
			State = State0
		;	State = none
		).

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

	% output predicates

	% record final JSON-RPC response (or non-progress notification) as outcome
	write_locked(_Output, Message) :-
		retractall(reply_outcome_(_)),
		assertz(reply_outcome_(reply(Message))).

	send_result(Id, Result, Output) :-
		response(Result, Id, Response),
		write_locked(Output, Response).

	send_complete(Id, {}, Output) :-
		!,
		send_result(Id, {resultType-complete}, Output).
	send_complete(Id, ResultBody, Output) :-
		% Ensure resultType is present
		(	^^has_pair(ResultBody, resultType, _) ->
			Result = ResultBody
		;	% Merge resultType-complete
			ResultBody = {Pairs},
			Result = {(resultType-complete, Pairs)}
		),
		send_result(Id, Result, Output).

	send_error(Id, Code, Message, Output) :-
		(	atom(Message) ->
			MessageAtom = Message
		;	write_to_atom(Message, MessageAtom)
		),
		error_response(Code, MessageAtom, Id, Response),
		write_locked(Output, Response).

	send_error_data(Id, Code, Message, Data, Output) :-
		(	atom(Message) ->
			MessageAtom = Message
		;	write_to_atom(Message, MessageAtom)
		),
		ErrorObj = {code-Code, message-MessageAtom, data-Data},
		Response = {jsonrpc-'2.0', id-Id, error-ErrorObj},
		write_locked(Output, Response).

	default_option(server_name('logtalk-mcp-server')).
	default_option(server_version('1.0.0')).
	default_option(server_title('logtalk-mcp-server')).
	default_option(instructions('')).
	default_option(cache_ttl(0)).
	default_option(cache_scope(private)).

	valid_option(protocol_adapter(Adapter)) :-
		callable(Adapter),
		conforms_to_protocol(Adapter, mcp_server_adapter_protocol).
	valid_option(server_name(Name)) :-
		atom(Name).
	valid_option(server_version(Version)) :-
		atom(Version).
	valid_option(server_title(Title)) :-
		atom(Title).
	valid_option(instructions(Instructions)) :-
		atom(Instructions).
	valid_option(cache_ttl(TTL)) :-
		number(TTL),
		TTL >= 0.
	valid_option(cache_scope(Scope)) :-
		once((Scope == (public); Scope == private)).
	% pass-through options
	valid_option(stdio_input(_)).
	valid_option(stdio_output(_)).
	valid_option(application(_)).
	valid_option(application_capabilities(_)).
	valid_option(progress_hook(_)).
	valid_option(spec(_)).
	valid_option(transport(_)).
	valid_option(protocol_adapter(_)).
	valid_option(http_port(_)).
	valid_option(http_bind(_)).
	valid_option(http_path(_)).
	valid_option(http_origin_check(_)).
	valid_option(http_sse_keepalive(_)).

:- end_object.
