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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2026-02-24,
		comment is 'MCP (Model Context Protocol) server for Logtalk applications. Exposes Logtalk objects implementing the ``mcp_tool_protocol`` and optionally ``mcp_prompt_protocol`` and ``mcp_resource_protocol`` protocols as MCP tool, prompt, and resource providers over stdio transport with Content-Length framing. Implements the MCP 2025-03-26 specification. Uses the ``json_rpc`` library for JSON-RPC 2.0 message handling.',
		remarks is [
			'MCP specification' - 'Implements the Model Context Protocol 2025-03-26: https://spec.modelcontextprotocol.io/specification/2025-03-26/',
			'Transport' - 'Uses stdio (standard input/output) with Content-Length header framing as defined by the MCP specification.',
			'Capabilities' - 'Supports the tools capability (``tools/list`` and ``tools/call``). Optionally supports the prompts capability (``prompts/list`` and ``prompts/get``) when the application declares it via ``capabilities/1`` and implements ``mcp_prompt_protocol``. Optionally supports the resources capability (``resources/list`` and ``resources/read``) when the application declares it via ``capabilities/1`` and implements ``mcp_resource_protocol``. Optionally supports the elicitation capability (``elicitation/create``) when the application declares it via ``capabilities/1``.',
			'Tool discovery' - 'Tools are discovered from objects implementing the ``mcp_tool_protocol``. Tool metadata (descriptions, parameter schemas) is derived from ``info/2`` and ``mode/2`` directives.',
			'Prompt discovery' - 'Prompts are discovered from objects implementing the ``mcp_prompt_protocol``. Prompt metadata (names, descriptions, arguments) is declared via the ``prompts/1`` predicate.',
			'Resource discovery' - 'Resources are discovered from objects implementing the ``mcp_resource_protocol``. Resource metadata (URIs, names, descriptions, MIME types) is declared via the ``resources/1`` predicate.',
			'Auto-dispatch' - 'When an object does not define ``tool_call/3`` or ``tool_call/4`` for a tool, the server auto-dispatches: it calls the predicate as a message to the object, collects output-mode arguments, and returns them as text content.',
			'Elicitation' - 'When the application declares ``elicitation`` in its ``capabilities/1``, the server constructs an elicit closure and passes it to ``tool_call/4``. The closure sends ``elicitation/create`` requests to the client and reads back the user response. This enables interactive tools that can ask the user questions during execution.',
			'Error handling' - 'Predicate failures and exceptions both result in MCP tool-level errors with ``isError`` set to ``true``.'
		]
	]).

	:- public(start/2).
	:- mode(start(+atom, +object_identifier), one).
	:- info(start/2, [
		comment is 'Starts the MCP server with the given server name and application object. The application object must implement the ``mcp_tool_protocol`` protocol. Reads JSON-RPC messages from standard input and writes responses to standard output using Content-Length framing. Blocks until the client disconnects or an exit signal is received.',
		argnames is ['Name', 'Application']
	]).

	:- public(start/3).
	:- mode(start(+atom, +object_identifier, +list), one).
	:- info(start/3, [
		comment is 'Starts the MCP server with the given server name, application object, and options. Currently supported options: ``version(Version)`` to set the server version (default ``''1.0.0''``).',
		argnames is ['Name', 'Application', 'Options']
	]).

	:- public(start/4).
	:- mode(start(+atom, +object_identifier, +stream, +stream), one).
	:- info(start/4, [
		comment is 'Starts the MCP server with custom input and output streams. Useful for testing or non-stdio transports. Uses default options.',
		argnames is ['Name', 'Application', 'Input', 'Output']
	]).

	:- public(start/5).
	:- mode(start(+atom, +object_identifier, +stream, +stream, +list), one).
	:- info(start/5, [
		comment is 'Starts the MCP server with custom input and output streams and options.',
		argnames is ['Name', 'Application', 'Input', 'Output', 'Options']
	]).

	:- public(elicit_request/5).
	:- mode(elicit_request(+stream, +stream, +atom, +compound, --compound), one).
	:- info(elicit_request/5, [
		comment is 'Sends an ``elicitation/create`` request to the MCP client and reads the response. ``Input`` and ``Output`` are the I/O streams. ``Message`` is the prompt text (an atom). ``RequestedSchema`` is a curly-term JSON Schema describing the input to request from the user (e.g., ``{type-object, properties-{answer-{type-string, enum-[yes, no]}}, required-[answer]}``). ``Answer`` is unified with ``accept(Content)`` (where ``Content`` is the user response as a curly-term), ``decline``, or ``cancel``. This predicate is typically not called directly but through the elicit closure passed to ``tool_call/4``.',
		argnames is ['Input', 'Output', 'Message', 'RequestedSchema', 'Answer']
	]).

	:- private(initialized_/0).
	:- dynamic(initialized_/0).

	:- private(application_/1).
	:- dynamic(application_/1).

	:- private(server_name_/1).
	:- dynamic(server_name_/1).

	:- private(server_version_/1).
	:- dynamic(server_version_/1).

	:- private(elicit_counter_/1).
	:- dynamic(elicit_counter_/1).

	:- private(app_capabilities_/1).
	:- dynamic(app_capabilities_/1).

	:- uses(json_rpc, [
		request/4, response/3, error_response/4, method_not_found/2, invalid_params/2, is_request/1,
		is_notification/1, is_response/1, id/2, method/2, params/2, result/2, write_message/2,
		read_message/2
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% Server start

	start(Name, Application) :-
		start(Name, Application, []).

	start(Name, Application, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		setup_state(Name, Application, Options),
		current_input(Input),
		current_output(Output),
		server_loop(Input, Output),
		cleanup_state.

	start(Name, Application, Input, Output) :-
		start(Name, Application, Input, Output, []).

	start(Name, Application, Input, Output, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		setup_state(Name, Application, Options),
		server_loop(Input, Output),
		cleanup_state.

	setup_state(Name, Application, Options) :-
		retractall(initialized_),
		retractall(application_(_)),
		retractall(server_name_(_)),
		retractall(server_version_(_)),
		retractall(elicit_counter_(_)),
		retractall(app_capabilities_(_)),
		assertz(application_(Application)),
		assertz(server_name_(Name)),
%		^^option(server_name(Name), Options),
		^^option(server_version(Version), Options),
		assertz(server_version_(Version)),
		assertz(elicit_counter_(0)),
		% Query application capabilities (default: no extra capabilities)
		(	catch(Application::capabilities(AppCaps), _, fail) ->
			assertz(app_capabilities_(AppCaps))
		;	assertz(app_capabilities_([]))
		).

	cleanup_state :-
		retractall(initialized_),
		retractall(application_(_)),
		retractall(server_name_(_)),
		retractall(server_version_(_)),
		retractall(elicit_counter_(_)),
		retractall(app_capabilities_(_)).

	% Main server loop

	server_loop(Input, Output) :-
		(	catch(
				read_message(Input, Message),
				Error,
				(writeq(user_error, Error),  nl(user_error), fail)
			) ->
			handle_message(Message, Input, Output),
			server_loop(Input, Output)
		;	% End of input or read failure — stop
			true
		).

	% Message dispatch

	handle_message(Message, Input, Output) :-
		(	is_request(Message) ->
			handle_request(Message, Input, Output)
		;	is_notification(Message) ->
			handle_notification(Message)
		;	% Invalid message: ignore per JSON-RPC spec for unrecognized
			true
		).

	% Request handlers

	handle_request(Message, Input, Output) :-
		writeq(user_error, Message), nl(user_error),
		method(Message, Method),
		id(Message, Id),
		(	Method == initialize ->
			handle_initialize(Message, Id, Output)
		;	Method == ping ->
			handle_ping(Id, Output)
		;	Method == 'tools/list' ->
			handle_tools_list(Id, Output)
		;	Method == 'tools/call' ->
			handle_tools_call(Message, Id, Input, Output)
		;	Method == 'prompts/list' ->
			handle_prompts_list(Id, Output)
		;	Method == 'prompts/get' ->
			handle_prompts_get(Message, Id, Output)
		;	Method == 'resources/list' ->
			handle_resources_list(Id, Output)
		;	Method == 'resources/read' ->
			handle_resources_read(Message, Id, Output)
		;	% Unknown method
			method_not_found(Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	% Notification handlers

	handle_notification(Message) :-
		method(Message, Method),
		(	Method == initialized ->
			true  % Client acknowledged initialization
		;	Method == 'notifications/cancelled' ->
			true  % TODO: cancel pending engine if supported
		;	% Unknown notification: ignore
			true
		).

	% Server initialization

	handle_initialize(_Message, Id, Output) :-
		assertz(initialized_),
		server_name_(Name),
		server_version_(Version),
		% Build capabilities based on application requirements
		app_capabilities_(AppCaps),
		build_capabilities(AppCaps, Capabilities),
		Result = {
			protocolVersion-'2025-03-26',
			capabilities-Capabilities,
			serverInfo-{
				name-Name,
				version-Version
			}
		},
		response(Result, Id, Response),
		write_message(Output, Response).

	% Build capabilities curly-term from base tools + application-requested caps
	build_capabilities(AppCaps, Capabilities) :-
		% Start with tools (always present)
		CapsList0 = [tools-{}],
		% Add prompts if requested
		(	member(prompts, AppCaps) ->
			CapsList1 = [prompts-{}| CapsList0]
		;	CapsList1 = CapsList0
		),
		% Add resources if requested
		(	member(resources, AppCaps) ->
			CapsList2 = [resources-{}| CapsList1]
		;	CapsList2 = CapsList1
		),
		% Add elicitation if requested
		(	member(elicitation, AppCaps) ->
			CapsList3 = [elicitation-{}| CapsList2]
		;	CapsList3 = CapsList2
		),
		pairs_to_curly(CapsList3, Capabilities).

	% Server ping

	handle_ping(Id, Output) :-
		response({}, Id, Response),
		write_message(Output, Response).

	% Handle tools/list requests

	handle_tools_list(Id, Output) :-
		application_(Application),
		Application::tools(ToolDescriptors),
		tool_descriptors_to_json(Application, ToolDescriptors, JsonTools),
		Result = {tools-JsonTools},
		response(Result, Id, Response),
		write_message(Output, Response).

	% Convert a list of tool(Name, Functor, Arity) descriptors to MCP JSON tool definitions
	tool_descriptors_to_json(_, [], []).
	tool_descriptors_to_json(Application, [tool(Name, Functor, Arity)| Rest], [JsonTool| JsonRest]) :-
		tool_descriptor_to_json(Application, Name, Functor, Arity, JsonTool),
		tool_descriptors_to_json(Application, Rest, JsonRest).

	tool_descriptor_to_json(Application, Name, Functor, Arity, JsonTool) :-
		% Get description from info/2 directive
		(	tool_predicate_comment(Application, Functor, Arity, Description) ->
			true
		;	Description = ''
		),
		% Build input schema from info/2 and mode/2
		tool_input_schema(Application, Functor, Arity, InputSchema),
		JsonTool = {
			name-Name,
			description-Description,
			inputSchema-InputSchema
		}.

	% Extract comment from predicate info/2
	tool_predicate_comment(Application, Functor, Arity, Comment) :-
		functor(Head, Functor, Arity),
		Application::predicate_property(Head, info(InfoPairs)),
		info_pair_value(InfoPairs, comment, Comment).

	% Build JSON Schema for tool input parameters from info/2 and mode/2
	tool_input_schema(Application, Functor, Arity, InputSchema) :-
		functor(Head, Functor, Arity),
		% Get argument names from info/2
		(	Application::predicate_property(Head, info(InfoPairs)),
			(	info_pair_value(InfoPairs, arguments, Arguments) ->
				pairs_keys(Arguments, ArgNames)
			;	info_pair_value(InfoPairs, argnames, ArgNames)
			) ->
			true
		;	generate_arg_names(1, Arity, ArgNames)
		),
		% Get mode template to identify input arguments
		(	Application::predicate_property(Head, mode(ModeTemplate, _)) ->
			ModeTemplate =.. [_| ModeArgs]
		;	length(ModeArgs, Arity),
			fill_default_modes(Arity, ModeArgs)
		),
		% Build properties and required lists from input-mode arguments only
		build_schema_properties(ArgNames, ModeArgs, 1, Properties, Required),
		(	Properties == [] ->
			InputSchema = {type-object, properties-{}}
		;	pairs_to_curly(Properties, PropertiesCurly),
			InputSchema = {type-object, properties-PropertiesCurly, required-Required}
		).

	% Handle tools/call requests

	handle_tools_call(Message, Id, Input, Output) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	has_pair(Params, name, ToolName) ->
			true
		;	invalid_params(Id, ErrorResponse),
			write_message(Output, ErrorResponse),
			!
		),
		(	has_pair(Params, arguments, ToolArguments) ->
			true
		;	ToolArguments = {}
		),
		application_(Application),
		% Find the tool descriptor
		(	Application::tools(ToolDescriptors),
			member(tool(ToolName, Functor, Arity), ToolDescriptors) ->
			execute_tool_call(Application, ToolName, Functor, Arity, ToolArguments, Id, Input, Output)
		;	% Tool not found
			error_response(-32601, 'Tool not found', Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	execute_tool_call(Application, ToolName, Functor, Arity, ToolArguments, Id, Input, Output) :-
		(	catch(
				execute_tool(Application, ToolName, Functor, Arity, ToolArguments, Input, Output, ToolResult0),
				Error,
				ToolResult0 = error(Error)
			) ->
			format_tool_result(ToolResult0, Id, Response)
		;	% Predicate failed
			FailContent = [{type-text, text-'Tool execution failed'}],
			response({content-FailContent, isError- @true}, Id, Response)
		),
		write_message(Output, Response).

	execute_tool(Application, ToolName, Functor, Arity, ToolArguments, Input, Output, Result) :-
		curly_to_pairs(ToolArguments, ArgPairs),
		app_capabilities_(AppCaps),
		(	member(elicitation, AppCaps) ->
			% Try tool_call/4 with elicit closure first
			(	catch(
					(Application::tool_call(ToolName, ArgPairs, {Input, Output}/[Message, Schema, Answer]>>(mcp_server::elicit_request(Input, Output, Message, Schema, Answer)), Result)),
					error(existence_error(procedure, _), _),
					fail
				) ->
				true
			;	% Fall back to tool_call/3
				try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result)
			)
		;	% No elicitation: try tool_call/3, then auto-dispatch
			try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result)
		).

	try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result) :-
		(	catch(
				Application::tool_call(ToolName, ArgPairs, Result),
				error(existence_error(procedure, _), _),
				fail
			) ->
			true
		;	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result)
		).

	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result) :-
		functor(Goal, Functor, Arity),
		% Get mode information for argument classification
		(	Application::predicate_property(Goal, mode(ModeTemplate, _)) ->
			ModeTemplate =.. [_| ModeArgs]
		;	length(ModeArgs, Arity),
			fill_default_modes(Arity, ModeArgs)
		),
		% Get argument names
		(	Application::predicate_property(Goal, info(InfoPairs)),
			(	info_pair_value(InfoPairs, arguments, Arguments) ->
				pairs_keys(Arguments, ArgNames)
			;	info_pair_value(InfoPairs, argnames, ArgNames)
			) ->
			true
		;	generate_arg_names(1, Arity, ArgNames)
		),
		% Bind input arguments from the tool call arguments
		bind_input_arguments(Goal, ArgNames, ModeArgs, 1, ToolArguments),
		% Call the predicate
		Application::Goal,
		% Collect output arguments
		collect_output_arguments(Goal, ArgNames, ModeArgs, 1, OutputPairs),
		% Format result
		(	OutputPairs == [] ->
			Result = text('Success')
		;	format_output_pairs(OutputPairs, Text),
			Result = text(Text)
		).

	% Tool result formatting

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
		format_content_items(Items, Content),
		response({content-Content}, Id, Response).

	format_content_items([], []).
	format_content_items([text(Text)| Rest], [{type-text, text-Text}| ContentRest]) :-
		format_content_items(Rest, ContentRest).
	format_content_items([error(Error)| Rest], [{type-text, text-ErrorText}| ContentRest]) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		format_content_items(Rest, ContentRest).

	% Handle prompts/list requests

	handle_prompts_list(Id, Output) :-
		application_(Application),
		(	catch(Application::prompts(PromptDescriptors), _, fail) ->
			prompt_descriptors_to_json(PromptDescriptors, JsonPrompts)
		;	JsonPrompts = []
		),
		Result = {prompts-JsonPrompts},
		response(Result, Id, Response),
		write_message(Output, Response).

	% Convert a list of prompt(Name, Description, Arguments) descriptors to MCP JSON prompt definitions
	prompt_descriptors_to_json([], []).
	prompt_descriptors_to_json([prompt(Name, Description, Arguments)| Rest], [JsonPrompt| JsonRest]) :-
		prompt_arguments_to_json(Arguments, JsonArguments),
		JsonPrompt = {name-Name, description-Description, arguments-JsonArguments},
		prompt_descriptors_to_json(Rest, JsonRest).

	prompt_arguments_to_json([], []).
	prompt_arguments_to_json([argument(ArgName, ArgDescription, Required)| Rest], [JsonArg| JsonRest]) :-
		bool_to_json(Required, JsonRequired),
		JsonArg = {name-ArgName, description-ArgDescription, required-JsonRequired},
		prompt_arguments_to_json(Rest, JsonRest).

	bool_to_json(true, @true).
	bool_to_json(false, @false).

	% Handle prompts/get requests

	handle_prompts_get(Message, Id, Output) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	has_pair(Params, name, PromptName) ->
			true
		;	invalid_params(Id, ErrorResponse),
			write_message(Output, ErrorResponse),
			!
		),
		(	has_pair(Params, arguments, PromptArguments) ->
			true
		;	PromptArguments = {}
		),
		application_(Application),
		% Check the prompt exists
		(	catch(Application::prompts(PromptDescriptors), _, fail),
			member(prompt(PromptName, _, _), PromptDescriptors) ->
			execute_prompt_get(Application, PromptName, PromptArguments, Id, Output)
		;	% Prompt not found
			error_response(-32601, 'Prompt not found', Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	execute_prompt_get(Application, PromptName, PromptArguments, Id, Output) :-
		curly_to_pairs(PromptArguments, ArgPairs),
		(	catch(
				Application::prompt_get(PromptName, ArgPairs, PromptResult),
				Error,
				PromptResult = error(Error)
			) ->
			format_prompt_result(PromptResult, Id, Response)
		;	% prompt_get/3 failed
			error_response(-32603, 'Prompt execution failed', Id, Response)
		),
		write_message(Output, Response).

	% Prompt result formatting

	format_prompt_result(messages(Messages), Id, Response) :-
		format_prompt_messages(Messages, JsonMessages),
		response({messages-JsonMessages}, Id, Response).
	format_prompt_result(messages(Description, Messages), Id, Response) :-
		format_prompt_messages(Messages, JsonMessages),
		response({description-Description, messages-JsonMessages}, Id, Response).
	format_prompt_result(error(Error), Id, Response) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		error_response(-32603, ErrorText, Id, Response).

	format_prompt_messages([], []).
	format_prompt_messages([message(Role, text(Text))| Rest], [JsonMsg| JsonRest]) :-
		JsonMsg = {role-Role, content-{type-text, text-Text}},
		format_prompt_messages(Rest, JsonRest).

	% Handle resources/list requests

	handle_resources_list(Id, Output) :-
		application_(Application),
		(	catch(Application::resources(ResourceDescriptors), _, fail) ->
			resource_descriptors_to_json(ResourceDescriptors, JsonResources)
		;	JsonResources = []
		),
		Result = {resources-JsonResources},
		response(Result, Id, Response),
		write_message(Output, Response).

	% Convert a list of resource(URI, Name, Description, MimeType) descriptors to MCP JSON
	resource_descriptors_to_json([], []).
	resource_descriptors_to_json([resource(URI, Name, Description, MimeType)| Rest], [JsonRes| JsonRest]) :-
		JsonRes = {uri-URI, name-Name, description-Description, mimeType-MimeType},
		resource_descriptors_to_json(Rest, JsonRest).

	% Handle resources/read requests

	handle_resources_read(Message, Id, Output) :-
		(	params(Message, Params) ->
			true
		;	Params = {}
		),
		(	has_pair(Params, uri, URI) ->
			true
		;	invalid_params(Id, ErrorResponse),
			write_message(Output, ErrorResponse),
			!
		),
		application_(Application),
		% Check the resource exists
		(	catch(Application::resources(ResourceDescriptors), _, fail),
			member(resource(URI, _, _, _), ResourceDescriptors) ->
			execute_resource_read(Application, URI, Id, Output)
		;	% Resource not found
			error_response(-32601, 'Resource not found', Id, ErrorResponse),
			write_message(Output, ErrorResponse)
		).

	execute_resource_read(Application, URI, Id, Output) :-
		(	catch(
				Application::resource_read(URI, [], ResourceResult),
				Error,
				ResourceResult = error(Error)
			) ->
			format_resource_result(ResourceResult, Id, Response)
		;	% resource_read/3 failed
			error_response(-32603, 'Resource read failed', Id, Response)
		),
		write_message(Output, Response).

	% Resource result formatting

	format_resource_result(contents(Contents), Id, Response) :-
		format_resource_contents(Contents, JsonContents),
		response({contents-JsonContents}, Id, Response).
	format_resource_result(error(Error), Id, Response) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		error_response(-32603, ErrorText, Id, Response).

	format_resource_contents([], []).
	format_resource_contents([text_content(URI, MimeType, Text)| Rest], [JsonContent| JsonRest]) :-
		JsonContent = {uri-URI, mimeType-MimeType, text-Text},
		format_resource_contents(Rest, JsonRest).
	format_resource_contents([blob_content(URI, MimeType, Base64Data)| Rest], [JsonContent| JsonRest]) :-
		JsonContent = {uri-URI, mimeType-MimeType, blob-Base64Data},
		format_resource_contents(Rest, JsonRest).

	% Elicitation

	% Send an elicitation/create request to the client and read the response.
	% Generates a unique request ID, sends the JSON-RPC request, reads the
	% client's response, and extracts the action and content.
	elicit_request(Input, Output, Message, RequestedSchema, Answer) :-
		generate_elicit_id(ElicitId),
		Params = {message-Message, requestedSchema-RequestedSchema},
		request('elicitation/create', Params, ElicitId, Request),
		write_message(Output, Request),
		% Read the client's response (blocking)
		read_message(Input, Response),
		(	is_response(Response) ->
			result(Response, ResultObj),
			(	has_pair(ResultObj, action, Action) ->
				(	Action == accept ->
					(	has_pair(ResultObj, content, Content) ->
						Answer = accept(Content)
					;	Answer = accept({})
					)
				;	Action == decline ->
					Answer = decline
				;	% cancel or unknown
					Answer = cancel
				)
			;	Answer = cancel
			)
		;	% Unexpected message (not a response); treat as cancel
			Answer = cancel
		).

	% Generate a unique elicitation request ID
	generate_elicit_id(Id) :-
		retract(elicit_counter_(N)),
		N1 is N + 1,
		assertz(elicit_counter_(N1)),
		number_codes(N1, NCodes),
		atom_codes(NAtom, NCodes),
		atom_concat(elicit_, NAtom, Id).

	% Argument binding and collection

	% Bind input-mode arguments in Goal from the JSON arguments object
	bind_input_arguments(_, [], [], _, _).
	bind_input_arguments(Goal, [ArgName| ArgNames], [Mode| Modes], N, ToolArguments) :-
		(	is_input_mode(Mode) ->
			(	has_pair(ToolArguments, ArgName, Value) ->
				arg(N, Goal, Value)
			;	true  % Argument not provided; leave unbound
			)
		;	true  % Output argument; leave unbound
		),
		N1 is N + 1,
		bind_input_arguments(Goal, ArgNames, Modes, N1, ToolArguments).

	% Collect output-mode arguments from a called Goal
	collect_output_arguments(_, [], [], _, []).
	collect_output_arguments(Goal, [ArgName| ArgNames], [Mode| Modes], N, Pairs) :-
		(	is_output_mode(Mode) ->
			arg(N, Goal, Value),
			Pairs = [ArgName-Value| RestPairs]
		;	Pairs = RestPairs
		),
		N1 is N + 1,
		collect_output_arguments(Goal, ArgNames, Modes, N1, RestPairs).

	% Mode classification

	% Input modes: +, ++, @
	is_input_mode(Mode) :-
		nonvar(Mode),
		(	Mode = (+_) -> true
		;	Mode = (++_) -> true
		;	Mode = (@_) -> true
		;	Mode == (+) -> true
		;	Mode == (++) -> true
		;	Mode == (@) -> true
		;	fail
		).

	% Output modes: -, --
	is_output_mode(Mode) :-
		nonvar(Mode),
		(	Mode = (-_) -> true
		;	Mode = (--_) -> true
		;	Mode == (-) -> true
		;	Mode == (--) -> true
		;	fail
		).

	% Schema building auxiliary predicates

	build_schema_properties([], [], _, [], []).
	build_schema_properties([ArgName| ArgNames], [Mode| Modes], N, Properties, Required) :-
		N1 is N + 1,
		(	is_input_mode(Mode) ->
			mode_to_json_type(Mode, JsonType),
			Properties = [ArgName-{type-JsonType}| RestProperties],
			Required = [ArgName| RestRequired]
		;	Properties = RestProperties,
			Required = RestRequired
		),
		build_schema_properties(ArgNames, Modes, N1, RestProperties, RestRequired).

	% Map Logtalk mode types to JSON Schema types
	mode_to_json_type(Mode, Type) :-
		(	nonvar(Mode), Mode = (+ModeType) -> true
		;	nonvar(Mode), Mode = (++ModeType) -> true
		;	nonvar(Mode), Mode = (@ModeType) -> true
		;	ModeType = any
		),
		(	var(ModeType) ->
			Type = string
		;	logtalk_type_to_json(ModeType, Type)
		).

	logtalk_type_to_json(integer, integer) :-
		!.
	logtalk_type_to_json(float, number) :-
		!.
	logtalk_type_to_json(number, number) :-
		!.
	logtalk_type_to_json(atom, string) :-
		!.
	logtalk_type_to_json(boolean, boolean) :-
		!.
	logtalk_type_to_json(list, array) :-
		!.
	logtalk_type_to_json(list(_), array) :-
		!.
	logtalk_type_to_json(compound, object) :-
		!.
	logtalk_type_to_json(nonvar, string) :-
		!.
	logtalk_type_to_json(term, string) :-
		!.
	logtalk_type_to_json(chars, string) :-
		!.
	logtalk_type_to_json(codes, string) :-
		!.
	logtalk_type_to_json(_, string).

	% Other auxiliary predicates

	% Generate default argument names: arg1, arg2, ...
	generate_arg_names(N, Arity, []) :-
		N > Arity,
		!.
	generate_arg_names(N, Arity, [ArgName| ArgNames]) :-
		number_codes(N, NCodes),
		atom_codes(NAtom, NCodes),
		atom_concat(arg, NAtom, ArgName),
		N1 is N + 1,
		generate_arg_names(N1, Arity, ArgNames).

	% Fill default modes (all input +)
	fill_default_modes(0, []) :-
		!.
	fill_default_modes(N, [(+)| Modes]) :-
		N > 0,
		N1 is N - 1,
		fill_default_modes(N1, Modes).

	% Extract a value from an info/2 pairs list
	% Logtalk predicate_property/2 returns Key(Value) functors
	info_pair_value([Pair| _], Key, Value) :-
		Pair =.. [Key, Value],
		!.
	info_pair_value([_| Rest], Key, Value) :-
		info_pair_value(Rest, Key, Value).

	% curly-term pair lookup (same as json_rpc internal)
	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

	% Convert curly-term to list of pairs
	curly_to_pairs({}, []) :-
		!.
	curly_to_pairs({Pairs}, List) :-
		curly_pairs_to_list(Pairs, List).

	curly_pairs_to_list((Key-Value, Rest), [Key-Value| RestList]) :-
		!,
		curly_pairs_to_list(Rest, RestList).
	curly_pairs_to_list(Key-Value, [Key-Value]).

	% Convert list of pairs to curly-term
	pairs_to_curly([], {}) :-
		!.
	pairs_to_curly(Pairs, {CurlyPairs}) :-
		list_to_curly_pairs(Pairs, CurlyPairs).

	list_to_curly_pairs([Key-Value], Key-Value) :-
		!.
	list_to_curly_pairs([Key-Value| Rest], (Key-Value, RestCurly)) :-
		list_to_curly_pairs(Rest, RestCurly).

	% Extract keys from a list of Key-Value pairs
	pairs_keys([], []).
	pairs_keys([Key-_| Rest], [Key| Keys]) :-
		pairs_keys(Rest, Keys).

	% Format output argument pairs as text
	format_output_pairs([], '').
	format_output_pairs([Name-Value| Rest], Text) :-
		write_to_atom(Value, ValueAtom),
		(	Rest == [] ->
			atomic_list_concat([Name, ': ', ValueAtom], Text)
		;	atomic_list_concat([Name, ': ', ValueAtom], Line),
			format_output_pairs(Rest, RestText),
			atomic_list_concat([Line, '\n', RestText], Text)
		).

	default_option(server_name('logtalk-mcp-server')).
	default_option(server_version('1.0.0')).

	valid_option(server_name(Name)) :-
		atom(Name).
	valid_option(server_version(Version)) :-
		atom(Version).

:- end_object.
