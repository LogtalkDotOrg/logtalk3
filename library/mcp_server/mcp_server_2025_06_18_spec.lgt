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


:- object(mcp_server_2025_06_18_spec,
	implements(mcp_server_spec_protocol),
	imports(mcp_server_application)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-24,
		comment is 'MCP 2025-06-18 protocol handler. Transport-agnostic message handling; returns abstract outcomes for stdio or Streamable HTTP transports to render. Synchronous elicitation requires ``stdio_input/1`` and ``stdio_output/1`` options.'
	]).

	:- public(current_options/1).
	:- mode(current_options(-list), zero_or_one).
	:- info(current_options/1, [
		comment is 'Merged options established by prepare/2.',
		argnames is ['Options']
	]).

	:- public(elicit_request/5).
	:- mode(elicit_request(+stream, +stream, +atom, +compound, --compound), one).
	:- info(elicit_request/5, [
		comment is 'Sends an elicitation/create request to the client and reads the response (2025-06-18 synchronous model).',
		argnames is ['Input', 'Output', 'Message', 'RequestedSchema', 'Answer']
	]).

	% dynamic state (spec-owned)

	:- private(initialized_/0).
	:- dynamic(initialized_/0).
	:- mode(initialized_, zero_or_one).
	:- info(initialized_/0, [
		comment is 'Initialization completed flag.'
	]).

	:- private(elicit_counter_/1).
	:- dynamic(elicit_counter_/1).
	:- mode(elicit_counter_(-non_negative_integer), one).
	:- info(elicit_counter_/1, [
		comment is 'Elicitation current counter.',
		argnames is ['Counter']
	]).

	:- private(client_capabilities_/1).
	:- dynamic(client_capabilities_/1).
	:- mode(client_capabilities_(-nonvar), one).
	:- info(client_capabilities_/1, [
		comment is 'Client capabilities.',
		argnames is ['Capabilities']
	]).

	:- private(server_options_/1).
	:- dynamic(server_options_/1).
	:- mode(server_options_(-list(compound)), one).
	:- info(server_options_/1, [
		comment is 'Server options.',
		argnames is ['Options']
	]).

	:- uses(json_rpc, [
		request/4, response/3, error_response/4, method_not_found/2, invalid_params/2, is_request/1,
		is_notification/1, is_response/1, id/2, method/2, params/2, result/2, write_message/2,
		read_message/2
	]).
	:- uses(list, [
		last/2, member/2
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(user, [
		atomic_concat/3
	]).

	% implemented spec

	spec('2025-06-18').

	prepare(Application, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options0),
		(	catch(Application::capabilities(Capabilities), _, fail) -> true
		;	Capabilities = []
		),
		Options = [application(Application), application_capabilities(Capabilities)| Options0],
		retractall(server_options_(_)),
		assertz(server_options_(Options)),
		setup_state.

	current_options(Options) :-
		server_options_(Options).

	% the 2025-06-18 spec does not support list-changed / resource-updated
	% notifications in the form used by later specifications; ignore silently
	notify(_Event).

	cleanup :-
		retractall(initialized_),
		retractall(elicit_counter_(_)),
		retractall(client_capabilities_(_)),
		retractall(server_options_(_)).

	setup_state :-
		retractall(initialized_),
		retractall(elicit_counter_(_)),
		retractall(client_capabilities_(_)),
		assertz(elicit_counter_(0)),
		assertz(client_capabilities_({})).

	handle_message(Message, TransportOptions, Outcome) :-
		(	server_options_(Base) ->
			true
		;	Base = TransportOptions
		),
		merge_transport_options(Base, TransportOptions, Options),
		(	is_request(Message) ->
			handle_request(Message, Options, Outcome)
		;	is_notification(Message) ->
			handle_notification(Message),
			Outcome = accepted
		;	Outcome = no_reply
		).

	merge_transport_options(Base, Transport, Options) :-
		(	member(stdio_input(In), Transport) ->
			Options0 = [stdio_input(In)| Base]
		;	Options0 = Base
		),
		(	member(stdio_output(Out), Transport) ->
			Options = [stdio_output(Out)| Options0]
		;	Options = Options0
		).

	handle_request(Message, Options, Outcome) :-
		method(Message, Method),
		id(Message, Id),
		(	Method == initialize ->
			handle_initialize(Message, Id, Options, Outcome)
		;	Method == ping ->
			handle_ping(Id, Outcome)
		;	Method == 'tools/list' ->
			handle_tools_list(Id, Options, Outcome)
		;	Method == 'tools/call' ->
			handle_tools_call(Message, Id, Options, Outcome)
		;	Method == 'prompts/list' ->
			handle_prompts_list(Id, Options, Outcome)
		;	Method == 'prompts/get' ->
			handle_prompts_get(Message, Id, Options, Outcome)
		;	Method == 'resources/list' ->
			handle_resources_list(Id, Options, Outcome)
		;	Method == 'resources/read' ->
			handle_resources_read(Message, Id, Options, Outcome)
		;	method_not_found(Id, ErrorResponse),
			Outcome = reply(ErrorResponse)
		).

	handle_notification(Message) :-
		method(Message, Method),
		(	Method == 'notifications/initialized' ->
			retractall(initialized_),
			assertz(initialized_)
		;	Method == 'notifications/cancelled' ->
			true
		;	true
		).

	% supported protocol versions

	supported_protocol_versions(['2025-06-18']).

	% initialize

	handle_initialize(Message, Id, Options, Outcome) :-
		(	params(Message, Params),
			^^has_pair(Params, protocolVersion, ClientVersion) ->
			true
		;	ClientVersion = ''
		),
		(	^^has_pair(Params, capabilities, ClientCapabilities) ->
			retractall(client_capabilities_(_)),
			assertz(client_capabilities_(ClientCapabilities))
		;	true
		),
		supported_protocol_versions(Supported),
		(	best_supported_version(Supported, ClientVersion, NegotiatedVersion) ->
			true
		;	last(Supported, NegotiatedVersion)
		),
		^^option(server_name(Name), Options),
		^^option(server_version(Version), Options),
		^^option(server_title(Title), Options),
		^^option(application_capabilities(ApplicationCapabilities), Options),
		build_capabilities(ApplicationCapabilities, Capabilities),
		(	Title == '' ->
			ServerInfo = {name-Name, version-Version}
		;	ServerInfo = {name-Name, title-Title, version-Version}
		),
		Result = {
			protocolVersion-NegotiatedVersion,
			capabilities-Capabilities,
			serverInfo-ServerInfo
		},
		response(Result, Id, Response),
		Outcome = reply(Response).

	best_supported_version(Supported, ClientVersion, Best) :-
		best_supported_version(Supported, ClientVersion, '', Best),
		Best \== ''.

	best_supported_version([], _, Best, Best).
	best_supported_version([Version| Versions], ClientVersion, Best0, Best) :-
		(	Version @=< ClientVersion, Version @> Best0 ->
			best_supported_version(Versions, ClientVersion, Version, Best)
		;	best_supported_version(Versions, ClientVersion, Best0, Best)
		).

	build_capabilities(ApplicationCapabilities, Capabilities) :-
		Capabilities0 = [tools-{}],
		(	member(prompts, ApplicationCapabilities) ->
			Capabilities1 = [prompts-{}| Capabilities0]
		;	Capabilities1 = Capabilities0
		),
		(	member(resources, ApplicationCapabilities) ->
			Capabilities2 = [resources-{}| Capabilities1]
		;	Capabilities2 = Capabilities1
		),
		% MCP Apps extension (io.modelcontextprotocol/ui)
		(	member(ui, ApplicationCapabilities) ->
			UIExt = {'io.modelcontextprotocol/ui'-{mimeTypes-['text/html;profile=mcp-app']}},
			Capabilities3 = [extensions-UIExt| Capabilities2]
		;	Capabilities3 = Capabilities2
		),
		^^pairs_to_curly(Capabilities3, Capabilities).

	% ping

	handle_ping(Id, Outcome) :-
		response({}, Id, Response),
		Outcome = reply(Response).

	% tools/list

	handle_tools_list(Id, Options, Outcome) :-
		^^option(application(Application), Options),
		Application::tools(ToolDescriptors),
		^^tool_descriptors_to_json(ToolDescriptors, Application, JsonTools),
		Result = {tools-JsonTools},
		response(Result, Id, Response),
		Outcome = reply(Response).

	% tools/call

	handle_tools_call(Message, Id, Options, Outcome) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	^^has_pair(Params, name, ToolName) ->
			true
		;	invalid_params(Id, ErrorResponse),
			Outcome = reply(ErrorResponse), !
		),
		(	^^has_pair(Params, arguments, ToolArguments) ->
			true
		;	ToolArguments = {}
		),
		^^option(application(Application), Options),
		(	Application::tools(ToolDescriptors),
			member(tool(ToolName, Functor, Arity), ToolDescriptors) ->
			execute_tool_call(Application, ToolName, Functor, Arity, ToolArguments, Id, Options, Outcome)
		;	error_response(-32601, 'Tool not found', Id, ErrorResponse),
			Outcome = reply(ErrorResponse)
		).

	execute_tool_call(Application, ToolName, Functor, Arity, ToolArguments, Id, Options, Outcome) :-
		(	catch(
				execute_tool(Application, ToolName, Functor, Arity, ToolArguments, Options, ToolResult0),
				Error,
				ToolResult0 = error(Error)
			) ->
			format_tool_result(ToolResult0, Id, Response)
		;	FailContent = [{type-text, text-'Tool execution failed'}],
			response({content-FailContent, isError- @true}, Id, Response)
		),
		Outcome = reply(Response).

	execute_tool(Application, ToolName, Functor, Arity, ToolArguments, Options, Result) :-
		^^curly_to_pairs(ToolArguments, ArgPairs),
		^^option(application_capabilities(ApplicationCapabilities), Options),
		client_capabilities_(ClientCapabilities),
		(	member(elicitation, ApplicationCapabilities),
			^^has_pair(ClientCapabilities, elicitation, _),
			member(stdio_input(Input), Options),
			member(stdio_output(Output), Options) ->
			(	catch(
					(Application::tool_call(ToolName, ArgPairs, {Input, Output}/[Message, Schema, Answer]>>(mcp_server_2025_06_18_spec::elicit_request(Input, Output, Message, Schema, Answer)), Result)),
					error(existence_error(procedure, _), _),
					fail
				) ->
				true
			;	^^try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result)
			)
		;	^^try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result)
		).

	% tool result formatting (2025 shape)

	format_tool_result(text(Text), Id, Response) :-
		Content = [{type-text, text-Text}],
		response({content-Content}, Id, Response).
	format_tool_result(error(Error), Id, Response) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		Content = [{type-text, text-ErrorText}],
		response({content-Content, isError- @true}, Id, Response).
	format_tool_result(results(Items), Id, Response) :-
		^^format_content_items(Items, Content),
		response({content-Content}, Id, Response).
	format_tool_result(structured(StructuredContent), Id, Response) :-
		write_to_atom(StructuredContent, Text),
		Content = [{type-text, text-Text}],
		response({content-Content, structuredContent-StructuredContent}, Id, Response).
	format_tool_result(structured(Items, StructuredContent), Id, Response) :-
		^^format_content_items(Items, Content),
		response({content-Content, structuredContent-StructuredContent}, Id, Response).

	% prompts/list

	handle_prompts_list(Id, Options, Outcome) :-
		^^option(application(Application), Options),
		(	catch(Application::prompts(PromptDescriptors), _, fail) ->
			^^prompt_descriptors_to_json(PromptDescriptors, JsonPrompts)
		;	JsonPrompts = []
		),
		Result = {prompts-JsonPrompts},
		response(Result, Id, Response),
		Outcome = reply(Response).

	% prompts/get

	handle_prompts_get(Message, Id, Options, Outcome) :-
		(	params(Message, Params) -> true ; Params = {} ),
		(	^^has_pair(Params, name, PromptName) ->
			true
		;	invalid_params(Id, ErrorResponse),
			Outcome = reply(ErrorResponse),
			!
		),
		(	^^has_pair(Params, arguments, PromptArguments) ->
			true
		;	PromptArguments = {}
		),
		^^option(application(Application), Options),
		(	catch(Application::prompts(PromptDescriptors), _, fail),
			(	member(prompt(PromptName, _, _), PromptDescriptors)
			;	member(prompt(PromptName, _, _, _), PromptDescriptors)
			) ->
			execute_prompt_get(Application, PromptName, PromptArguments, Id, Outcome)
		;	error_response(-32601, 'Prompt not found', Id, ErrorResponse),
			Outcome = reply(ErrorResponse)
		).

	execute_prompt_get(Application, PromptName, PromptArguments, Id, Outcome) :-
		^^curly_to_pairs(PromptArguments, ArgPairs),
		(	catch(
				Application::prompt_get(PromptName, ArgPairs, PromptResult),
				Error,
				PromptResult = error(Error)
			) ->
			format_prompt_result(PromptResult, Id, Response)
		;	error_response(-32603, 'Prompt execution failed', Id, Response)
		),
		Outcome = reply(Response).

	format_prompt_result(messages(Messages), Id, Response) :-
		^^format_prompt_messages(Messages, JsonMessages),
		response({messages-JsonMessages}, Id, Response).
	format_prompt_result(messages(Description, Messages), Id, Response) :-
		^^format_prompt_messages(Messages, JsonMessages),
		response({description-Description, messages-JsonMessages}, Id, Response).
	format_prompt_result(error(Error), Id, Response) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		error_response(-32603, ErrorText, Id, Response).

	% resources/list

	handle_resources_list(Id, Options, Outcome) :-
		^^option(application(Application), Options),
		(	catch(Application::resources(ResourceDescriptors), _, fail) ->
			^^resource_descriptors_to_json(ResourceDescriptors, Application, JsonResources)
		;	JsonResources = []
		),
		Result = {resources-JsonResources},
		response(Result, Id, Response),
		Outcome = reply(Response).

	% resources/read

	handle_resources_read(Message, Id, Options, Outcome) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	^^has_pair(Params, uri, URI) ->
			true
		;	invalid_params(Id, ErrorResponse),
			Outcome = reply(ErrorResponse), !
		),
		^^option(application(Application), Options),
		(	catch(Application::resources(ResourceDescriptors), _, fail),
			(	member(resource(URI, _, _, _), ResourceDescriptors)
			;	member(resource(URI, _, _, _, _), ResourceDescriptors)
			) ->
			execute_resource_read(Application, URI, Id, Outcome)
		;	error_response(-32601, 'Resource not found', Id, ErrorResponse),
			Outcome = reply(ErrorResponse)
		).

	execute_resource_read(Application, URI, Id, Outcome) :-
		(	catch(
				Application::resource_read(URI, [], ResourceResult),
				Error,
				ResourceResult = error(Error)
			) ->
			format_resource_result(ResourceResult, Id, Response)
		;	error_response(-32603, 'Resource read failed', Id, Response)
		),
		Outcome = reply(Response).

	format_resource_result(contents(Contents), Id, Response) :-
		^^format_resource_contents(Contents, JsonContents),
		response({contents-JsonContents}, Id, Response).
	format_resource_result(error(Error), Id, Response) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		error_response(-32603, ErrorText, Id, Response).

	% elicitation (synchronous, 2025 style)

	elicit_request(Input, Output, Message, RequestedSchema, Answer) :-
		generate_elicit_id(ElicitId),
		Params = {message-Message, requestedSchema-RequestedSchema},
		request('elicitation/create', Params, ElicitId, Request),
		write_message(Output, Request),
		read_message(Input, Response),
		(	is_response(Response) ->
			result(Response, ResultObj),
			(	^^has_pair(ResultObj, action, Action) ->
				(	Action == accept ->
					(	^^has_pair(ResultObj, content, Content) ->
						Answer = accept(Content)
					;	Answer = accept({})
					)
				;	Action == decline ->
					Answer = decline
				;	Answer = cancel
				)
			;	Answer = cancel
			)
		;	Answer = cancel
		).

	generate_elicit_id(Id) :-
		retract(elicit_counter_(N0)),
		N is N0 + 1,
		assertz(elicit_counter_(N)),
		atomic_concat(elicit_, N, Id).

	default_option(server_name('logtalk-mcp-server')).
	default_option(server_version('1.0.0')).
	default_option(server_title('logtalk-mcp-server')).

	valid_option(protocol_adapter(Adapter)) :-
		callable(Adapter),
		conforms_to_protocol(Adapter, mcp_server_adapter_protocol).
	valid_option(server_name(Name)) :-
		atom(Name).
	valid_option(server_version(Version)) :-
		atom(Version).
	valid_option(server_title(Title)) :-
		atom(Title).
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
	valid_option(instructions(_)).
	valid_option(cache_ttl(_)).
	valid_option(cache_scope(_)).

:- end_object.
