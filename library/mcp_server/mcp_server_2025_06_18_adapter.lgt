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
	implements(mcp_server_adapter_protocol),
	imports(mcp_server_application)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-14,
		comment is 'MCP 2025-06-18 specification adapter. Owns the initialize handshake, version negotiation, client capability storage, notifications/initialized, synchronous server-initiated elicitation, and the 2025-06-18 result shapes. Does not add resultType, cache fields, or per-request metadata requirements.'
	]).

	:- uses(json_rpc, [
		request/4, response/3, error_response/4, method_not_found/2, invalid_params/2,
		is_request/1, is_notification/1, is_response/1, id/2, method/2, params/2, result/2,
		write_message/2, read_message/2
	]).

	:- uses(list, [
		last/2, member/2
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	% Dynamic state (adapter-owned)

	:- private(initialized_/0).
	:- dynamic(initialized_/0).

	:- private(elicit_counter_/1).
	:- dynamic(elicit_counter_/1).

	:- private(client_capabilities_/1).
	:- dynamic(client_capabilities_/1).

	% Adapter protocol implementation

	protocol_version('2025-06-18').

	start(Application, Input, Output, Options0) :-
%		^^check_options(UserOptions),
%		^^merge_options(UserOptions, Options0),
		(	catch(Application::capabilities(Capabilities), _, fail) ->
			true
		;	Capabilities = []
		),
		Options = [application(Application), application_capabilities(Capabilities)| Options0],
		setup_state,
		server_loop(Input, Output, Options),
		cleanup.

	% the 2025-06-18 spec does not support list-changed / resource-updated
	% notifications in the form used by later specifications; ignore silently
	notify(_Event).

	cleanup :-
		retractall(initialized_),
		retractall(elicit_counter_(_)),
		retractall(client_capabilities_(_)).

	% Setup

	setup_state :-
		cleanup,
		assertz(elicit_counter_(0)),
		assertz(client_capabilities_({})).

	% Main server loop

	server_loop(Input, Output, Options) :-
		(	catch(
				read_message(Input, Message),
				Error,
				(writeq(user_error, Error), nl(user_error), fail)
			) ->
			handle_message(Message, Input, Output, Options),
			server_loop(Input, Output, Options)
		;	true
		).

	handle_message(Message, Input, Output, Options) :-
		(	is_request(Message) ->
			handle_request(Message, Input, Output, Options)
		;	is_notification(Message) ->
			handle_notification(Message)
		;	true
		).

	% Request handlers

	handle_request(Message, Input, Output, Options) :-
		method(Message, Method),
		id(Message, Id),
		(	Method == initialize ->
			handle_initialize(Message, Id, Output, Options)
		;	Method == ping ->
			handle_ping(Id, Output)
		;	Method == 'tools/list' ->
			handle_tools_list(Id, Output, Options)
		;	Method == 'tools/call' ->
			handle_tools_call(Message, Id, Input, Output, Options)
		;	Method == 'prompts/list' ->
			handle_prompts_list(Id, Output, Options)
		;	Method == 'prompts/get' ->
			handle_prompts_get(Message, Id, Output, Options)
		;	Method == 'resources/list' ->
			handle_resources_list(Id, Output, Options)
		;	Method == 'resources/read' ->
			handle_resources_read(Message, Id, Output, Options)
		;	method_not_found(Id, ErrorResponse),
			write_message(Output, ErrorResponse)
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

	% Supported protocol versions

	supported_protocol_versions(['2025-06-18']).

	% Initialize

	handle_initialize(Message, Id, Output, Options) :-
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
		write_message(Output, Response).

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
		^^pairs_to_curly(Capabilities2, Capabilities).

	% Ping

	handle_ping(Id, Output) :-
		response({}, Id, Response),
		write_message(Output, Response).

	% tools/list

	handle_tools_list(Id, Output, Options) :-
		^^option(application(Application), Options),
		Application::tools(ToolDescriptors),
		^^tool_descriptors_to_json(ToolDescriptors, Application, JsonTools),
		Result = {tools-JsonTools},
		response(Result, Id, Response),
		write_message(Output, Response).

	% tools/call

	handle_tools_call(Message, Id, Input, Output, Options) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	^^has_pair(Params, name, ToolName) ->
			true
		;	invalid_params(Id, ErrorResponse),
			write_message(Output, ErrorResponse),
			!
		),
		(	^^has_pair(Params, arguments, ToolArguments) ->
			true
		;	ToolArguments = {}
		),
		^^option(application(Application), Options),
		(	Application::tools(ToolDescriptors),
			member(tool(ToolName, Functor, Arity), ToolDescriptors) ->
			execute_tool_call(Application, ToolName, Functor, Arity, ToolArguments, Id, Input, Output, Options)
		;	error_response(-32601, 'Tool not found', Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	execute_tool_call(Application, ToolName, Functor, Arity, ToolArguments, Id, Input, Output, Options) :-
		(	catch(
				execute_tool(Application, ToolName, Functor, Arity, ToolArguments, Input, Output, ToolResult0, Options),
				Error,
				ToolResult0 = error(Error)
			) ->
			format_tool_result(ToolResult0, Id, Response)
		;	FailContent = [{type-text, text-'Tool execution failed'}],
			response({content-FailContent, isError- @true}, Id, Response)
		),
		write_message(Output, Response).

	execute_tool(Application, ToolName, Functor, Arity, ToolArguments, Input, Output, Result, Options) :-
		^^curly_to_pairs(ToolArguments, ArgPairs),
		^^option(application_capabilities(ApplicationCapabilities), Options),
		client_capabilities_(ClientCapabilities),
		(	member(elicitation, ApplicationCapabilities),
			^^has_pair(ClientCapabilities, elicitation, _) ->
			(	catch(
					(Application::tool_call(ToolName, ArgPairs, {Input, Output}/[Message, Schema, Answer]>>(mcp_server_2025_06_18_adapter::elicit_request(Input, Output, Message, Schema, Answer)), Result)),
					error(existence_error(procedure, _), _),
					fail
				) ->
				true
			;	^^try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result)
			)
		;	^^try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result)
		).

	% Tool result formatting (2025 shape)

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

	handle_prompts_list(Id, Output, Options) :-
		^^option(application(Application), Options),
		(	catch(Application::prompts(PromptDescriptors), _, fail) ->
			^^prompt_descriptors_to_json(PromptDescriptors, JsonPrompts)
		;	JsonPrompts = []
		),
		Result = {prompts-JsonPrompts},
		response(Result, Id, Response),
		write_message(Output, Response).

	% prompts/get

	handle_prompts_get(Message, Id, Output, Options) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	^^has_pair(Params, name, PromptName) ->
			true
		;	invalid_params(Id, ErrorResponse),
			write_message(Output, ErrorResponse),
			!
		),
		(	^^has_pair(Params, arguments, PromptArguments) ->
			true
		;	PromptArguments = {}
		),
		^^option(application(Application), Options),
		(	catch(Application::prompts(PromptDescriptors), _, fail),
			(member(prompt(PromptName, _, _), PromptDescriptors) ; member(prompt(PromptName, _, _, _), PromptDescriptors)) ->
			execute_prompt_get(Application, PromptName, PromptArguments, Id, Output)
		;	error_response(-32601, 'Prompt not found', Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	execute_prompt_get(Application, PromptName, PromptArguments, Id, Output) :-
		^^curly_to_pairs(PromptArguments, ArgPairs),
		(	catch(
				Application::prompt_get(PromptName, ArgPairs, PromptResult),
				Error,
				PromptResult = error(Error)
			) ->
			format_prompt_result(PromptResult, Id, Response)
		;	error_response(-32603, 'Prompt execution failed', Id, Response)
		),
		write_message(Output, Response).

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

	handle_resources_list(Id, Output, Options) :-
		^^option(application(Application), Options),
		(	catch(Application::resources(ResourceDescriptors), _, fail) ->
			^^resource_descriptors_to_json(ResourceDescriptors, JsonResources)
		;	JsonResources = []
		),
		Result = {resources-JsonResources},
		response(Result, Id, Response),
		write_message(Output, Response).

	% resources/read

	handle_resources_read(Message, Id, Output, Options) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	^^has_pair(Params, uri, URI) ->
			true
		;	invalid_params(Id, ErrorResponse),
			write_message(Output, ErrorResponse),
			!
		),
		^^option(application(Application), Options),
		(	catch(Application::resources(ResourceDescriptors), _, fail),
			(member(resource(URI, _, _, _), ResourceDescriptors) ; member(resource(URI, _, _, _, _), ResourceDescriptors)) ->
			execute_resource_read(Application, URI, Id, Output)
		;	error_response(-32601, 'Resource not found', Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	execute_resource_read(Application, URI, Id, Output) :-
		(	catch(
				Application::resource_read(URI, [], ResourceResult),
				Error,
				ResourceResult = error(Error)
			) ->
			format_resource_result(ResourceResult, Id, Response)
		;	error_response(-32603, 'Resource read failed', Id, Response)
		),
		write_message(Output, Response).

	format_resource_result(contents(Contents), Id, Response) :-
		^^format_resource_contents(Contents, JsonContents),
		response({contents-JsonContents}, Id, Response).
	format_resource_result(error(Error), Id, Response) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		error_response(-32603, ErrorText, Id, Response).

	% Elicitation (synchronous, 2025 style)

	:- public(elicit_request/5).
	:- mode(elicit_request(+stream, +stream, +atom, +compound, --compound), one).
	:- info(elicit_request/5, [
		comment is 'Sends an elicitation/create request to the client and reads the response (2025-06-18 synchronous model).',
		argnames is ['Input', 'Output', 'Message', 'RequestedSchema', 'Answer']
	]).

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
		retract(elicit_counter_(N)),
		N1 is N + 1,
		assertz(elicit_counter_(N1)),
		number_codes(N1, NCodes),
		atom_codes(NAtom, NCodes),
		atom_concat(elicit_, NAtom, Id).

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

:- end_object.
