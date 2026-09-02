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


:- category(mcp_server_application,
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-02,
		comment is 'Common predicates for MCP servers: tool/prompt/resource descriptor conversion, completion request handling, schema derivation from ``info/2`` and ``mode/2`` directives, auto-dispatch, canonical complete-result terms, curly-term predicates, and MCP Apps (``_meta.ui``) metadata.'
	]).

	:- public(tool_descriptors_to_json/3).
	:- mode(tool_descriptors_to_json(+list, +object_identifier, -list), one).
	:- info(tool_descriptors_to_json/3, [
		comment is 'Converts a list of ``tool(Name, Functor, Arity)`` descriptors into MCP JSON tool definitions, deriving titles, descriptions and input schemas from the application object''s ``info/2`` and ``mode/2`` directives.',
		argnames is ['ToolDescriptors', 'Application', 'JsonTools']
	]).

	:- protected(prompt_descriptors_to_json/2).
	:- mode(prompt_descriptors_to_json(+list, -list), one).
	:- info(prompt_descriptors_to_json/2, [
		comment is 'Converts prompt descriptors (3-arg or 4-arg) into MCP JSON prompt definitions.',
		argnames is ['PromptDescriptors', 'JsonPrompts']
	]).

	:- protected(resource_descriptors_to_json/3).
	:- mode(resource_descriptors_to_json(+list, +object_identifier, -list), one).
	:- info(resource_descriptors_to_json/3, [
		comment is 'Converts resource descriptors (4-arg or 5-arg) into MCP JSON resource definitions. When the application defines ``resource_ui_meta/2``, attaches ``_meta.ui`` (MCP Apps).',
		argnames is ['Descriptors', 'Application', 'Resources']
	]).

	:- protected(resource_template_descriptors_to_json/2).
	:- mode(resource_template_descriptors_to_json(+list, -list), one_or_error).
	:- info(resource_template_descriptors_to_json/2, [
		comment is 'Converts resource template descriptors (4-arg or 5-arg) into MCP JSON resource template definitions. No partial list is returned when a descriptor is invalid.',
		argnames is ['Descriptors', 'ResourceTemplates'],
		exceptions is [
			'A descriptor contains an invalid RFC 6570 URI template ``URITemplate``' - domain_error(uri_template, 'URITemplate')
		]
	]).

	:- protected(application_resource_template_uri/2).
	:- mode(application_resource_template_uri(+object_identifier, +atom), zero_or_one).
	:- info(application_resource_template_uri/2, [
		comment is 'Checks that a concrete resource URI matches the literal segments of one of the application resource templates. Full RFC 6570 expression validation and authorization remain application responsibilities.',
		argnames is ['Application', 'URI']
	]).

	:- protected(completion_request/5).
	:- mode(completion_request(+object_identifier, +compound, -compound, -pair, -list), zero_or_one).
	:- info(completion_request/5, [
		comment is 'Validates and normalizes completion request parameters. The normalized reference is ``prompt(Name)`` or ``resource(URI)``, the argument is a ``Name-PartialValue`` pair, and the context is a list of prior argument pairs.',
		argnames is ['Application', 'Params', 'Reference', 'Argument', 'Context']
	]).

	:- protected(completion_result_to_json/2).
	:- mode(completion_result_to_json(+compound, -compound), zero_or_one).
	:- info(completion_result_to_json/2, [
		comment is 'Validates and converts a completion result to its MCP JSON curly-term representation.',
		argnames is ['Result', 'JsonCompletion']
	]).

	:- protected(auto_dispatch_tool/5).
	:- mode(auto_dispatch_tool(+object_identifier, +atom, +integer, +compound, -compound), one).
	:- info(auto_dispatch_tool/5, [
		comment is 'Auto-dispatches a tool call by calling the predicate on the application, collecting output-mode arguments, and returning structured output with a backwards-compatible text content item.',
		argnames is ['Application', 'Functor', 'Arity', 'ToolArguments', 'Result']
	]).

	:- protected(try_tool_call_3/7).
	:- mode(try_tool_call_3(+object_identifier, +atom, +atom, +integer, +list, +compound, -compound), one).
	:- info(try_tool_call_3/7, [
		comment is 'Tries ``tool_call/3``. Falls back to auto-dispatch on failure.',
		argnames is ['Application', 'ToolName', 'Functor', 'Arity', 'ArgPairs', 'ToolArguments', 'Result']
	]).

	:- protected(tool_input_schema/4).
	:- mode(tool_input_schema(+object_identifier, +atom, +integer, -compound), one).
	:- info(tool_input_schema/4, [
		comment is 'Derives a JSON Schema curly-term for tool input arguments from the application object''s ``info/2`` and ``mode/2`` directives. Used by Streamable HTTP ``x-mcp-header`` / ``Mcp-Param-*`` validation when the application does not define ``input_schema/2``.',
		argnames is ['Application', 'Functor', 'Arity', 'InputSchema']
	]).

	:- protected(tool_output_schema/4).
	:- mode(tool_output_schema(+object_identifier, +atom, +integer, -compound), one).
	:- info(tool_output_schema/4, [
		comment is 'Derives a JSON Schema curly-term for tool output arguments from the application object''s ``info/2`` and ``mode/2`` directives.',
		argnames is ['Application', 'Functor', 'Arity', 'OutputSchema']
	]).

	:- protected(format_content_items/2).
	:- mode(format_content_items(+list, -list), one).
	:- info(format_content_items/2, [
		comment is 'Converts canonical content item terms into MCP JSON content array elements.',
		argnames is ['Items', 'Elements']
	]).

	:- protected(format_prompt_messages/2).
	:- mode(format_prompt_messages(+list, -list), one).
	:- info(format_prompt_messages/2, [
		comment is 'Converts message(Role, text(Text)) terms into MCP JSON prompt message objects.',
		argnames is ['Messages', 'JsonMessages']
	]).

	:- protected(format_resource_contents/2).
	:- mode(format_resource_contents(+list, -list), one).
	:- info(format_resource_contents/2, [
		comment is 'Converts text_content/3 and blob_content/3 terms into MCP JSON resource content objects.',
		argnames is ['Contents', 'JsonContents']
	]).

	:- protected(has_pair/3).
	:- mode(has_pair(+compound, +atom, ?term), zero_or_one).
	:- info(has_pair/3, [
		comment is 'Looks up a Key-Value pair inside a curly-term.',
		argnames is ['Curly', 'Key', 'Value']
	]).

	:- protected(curly_to_pairs/2).
	:- mode(curly_to_pairs(+compound, -list), one).
	:- info(curly_to_pairs/2, [
		comment is 'Converts a curly-term to a list of Key-Value pairs.',
		argnames is ['Curly', 'Pairs']
	]).

	:- protected(pairs_to_curly/2).
	:- mode(pairs_to_curly(+list, -compound), one).
	:- info(pairs_to_curly/2, [
		comment is 'Converts a list of Key-Value pairs to a curly-term.',
		argnames is ['Pairs', 'Curly']
	]).

	:- uses(list, [
		length/2, member/2
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(user, [
		atomic_concat/3, atomic_list_concat/2
	]).

	% tool descriptors and schemas

	tool_descriptors_to_json([], _, []).
	tool_descriptors_to_json([tool(Name, Functor, Arity)| ToolDescriptors], Application, [JsonTool| JsonTools]) :-
		tool_descriptor_to_json(Application, Name, Functor, Arity, JsonTool),
		tool_descriptors_to_json(ToolDescriptors, Application, JsonTools).

	tool_descriptor_to_json(Application, Name, Functor, Arity, JsonTool) :-
		(	tool_predicate_title(Application, Functor, Arity, Title) ->
			true
		;	Title = Functor
		),
		(	tool_predicate_comment(Application, Functor, Arity, Description) ->
			true
		;	Description = ''
		),
		(	conforms_to_protocol(Application, mcp_tool_protocol),
			Application::input_schema(Name, InputSchema) ->
			true
		;	tool_input_schema(Application, Functor, Arity, InputSchema)
		),
		(	conforms_to_protocol(Application, mcp_tool_protocol),
			Application::output_schema(Name, OutputSchema) ->
			true
		;	tool_output_schema(Application, Functor, Arity, OutputSchema)
		),
		JsonTool0 = {
				name-Name,
				title-Title,
				description-Description,
				inputSchema-InputSchema,
				outputSchema-OutputSchema
			},
		% MCP Apps: optional _meta.ui from tool_ui/2
		(	conforms_to_protocol(Application, mcp_ui_protocol),
			application_tool_ui_meta(Application, Name, UIMeta) ->
			add_meta_field(JsonTool0, UIMeta, JsonTool)
		;	JsonTool = JsonTool0
		).

	tool_predicate_title(Application, Functor, Arity, Title) :-
		functor(Head, Functor, Arity),
		Application::predicate_property(Head, info(InfoPairs)),
		info_pair_value(InfoPairs, title, Title).

	tool_predicate_comment(Application, Functor, Arity, Comment) :-
		functor(Head, Functor, Arity),
		Application::predicate_property(Head, info(InfoPairs)),
		info_pair_value(InfoPairs, comment, Comment).

	tool_input_schema(Application, Functor, Arity, InputSchema) :-
		tool_argument_names_and_modes(Application, Functor, Arity, ArgNames, ModeArgs),
		build_schema_properties(ArgNames, ModeArgs, 1, Properties, Required),
		(	Properties == [] ->
			InputSchema = {type-object, properties-{}}
		;	pairs_to_curly(Properties, PropertiesCurly),
			InputSchema = {type-object, properties-PropertiesCurly, required-Required}
		).

	tool_output_schema(Application, Functor, Arity, OutputSchema) :-
		tool_argument_names_and_modes(Application, Functor, Arity, ArgNames, ModeArgs),
		build_output_schema_properties(ArgNames, ModeArgs, 1, Properties, Required),
		(	Properties == [] ->
			OutputSchema = {type-object, properties-{}}
		;	pairs_to_curly(Properties, PropertiesCurly),
			OutputSchema = {type-object, properties-PropertiesCurly, required-Required}
		).

	tool_argument_names_and_modes(Application, Functor, Arity, ArgNames, ModeArgs) :-
		functor(Head, Functor, Arity),
		(	Application::predicate_property(Head, info(InfoPairs)),
			(	info_pair_value(InfoPairs, arguments, Arguments) ->
				pairs_keys(Arguments, ArgNames)
			;	info_pair_value(InfoPairs, argnames, ArgNames)
			) ->
			true
		;	generate_argument_names(1, Arity, ArgNames)
		),
		(	Application::predicate_property(Head, mode(ModeTemplate, _)) ->
			ModeTemplate =.. [_| ModeArgs]
		;	length(ModeArgs, Arity),
			fill_default_modes(Arity, ModeArgs)
		).

	% prompt descriptors

	prompt_descriptors_to_json([], []).
	prompt_descriptors_to_json([prompt(Name, Title, Description, Arguments)| Rest], [JsonPrompt| JsonRest]) :-
		!,
		prompt_arguments_to_json(Arguments, JsonArguments),
		JsonPrompt = {name-Name, title-Title, description-Description, arguments-JsonArguments},
		prompt_descriptors_to_json(Rest, JsonRest).
	prompt_descriptors_to_json([prompt(Name, Description, Arguments)| Rest], [JsonPrompt| JsonRest]) :-
		prompt_arguments_to_json(Arguments, JsonArguments),
		JsonPrompt = {name-Name, description-Description, arguments-JsonArguments},
		prompt_descriptors_to_json(Rest, JsonRest).

	prompt_arguments_to_json([], []).
	prompt_arguments_to_json([argument(ArgName, ArgDescription, Required)| Rest], [JsonArg| JsonRest]) :-
		boolean_to_json(Required, JsonRequired),
		JsonArg = {name-ArgName, description-ArgDescription, required-JsonRequired},
		prompt_arguments_to_json(Rest, JsonRest).

	boolean_to_json(true,  @true).
	boolean_to_json(false, @false).

	% resource descriptors

	resource_descriptors_to_json([], _, []).
	resource_descriptors_to_json([Descriptor| Descriptors], Application, [Resource| Resources]) :-
		resource_descriptor_to_json(Application, Descriptor, Resource),
		resource_descriptors_to_json(Descriptors, Application, Resources).

	resource_descriptor_to_json(Application, resource(URI, Name, Title, Description, MimeType), Resource) :-
		!,
		Resource0 = {uri-URI, name-Name, title-Title, description-Description, mimeType-MimeType},
		(	conforms_to_protocol(Application, mcp_ui_protocol),
			application_resource_ui_meta(Application, URI, UIMeta) ->
			add_meta_field(Resource0, UIMeta, Resource)
		;	Resource = Resource0
		).
	resource_descriptor_to_json(Application, resource(URI, Name, Description, MimeType), Resource) :-
		Resource0 = {uri-URI, name-Name, description-Description, mimeType-MimeType},
		(	conforms_to_protocol(Application, mcp_ui_protocol),
			application_resource_ui_meta(Application, URI, UIMeta) ->
			add_meta_field(Resource0, UIMeta, Resource)
		;	Resource = Resource0
		).

	resource_template_descriptors_to_json([], []).
	resource_template_descriptors_to_json([Descriptor| Descriptors], [ResourceTemplate| ResourceTemplates]) :-
		resource_template_uri(Descriptor, URITemplate),
		( 	uri_template(atom)::valid(URITemplate) ->
			true
		;	domain_error(uri_template, URITemplate)
		),
		resource_template_descriptor_to_json(Descriptor, ResourceTemplate),
		resource_template_descriptors_to_json(Descriptors, ResourceTemplates).

	resource_template_descriptor_to_json(resource_template(URITemplate, Name, Title, Description, MimeType), ResourceTemplate) :-
		!,
		ResourceTemplate = {uriTemplate-URITemplate, name-Name, title-Title, description-Description, mimeType-MimeType}.
	resource_template_descriptor_to_json(resource_template(URITemplate, Name, Description, MimeType), ResourceTemplate) :-
		ResourceTemplate = {uriTemplate-URITemplate, name-Name, description-Description, mimeType-MimeType}.

	application_resource_template_uri(Application, URI) :-
		Application::resource_templates(ResourceTemplateDescriptors),
		member(ResourceTemplateDescriptor, ResourceTemplateDescriptors),
		resource_template_uri(ResourceTemplateDescriptor, URITemplate),
		resource_template_matches(URITemplate, URI),
		!.

	resource_template_uri(resource_template(URITemplate, _, _, _), URITemplate).
	resource_template_uri(resource_template(URITemplate, _, _, _, _), URITemplate).

	resource_template_matches(URITemplate, URI) :-
		(	sub_atom(URITemplate, Open, 1, _, '{') ->
			sub_atom(URITemplate, 0, Open, _, Prefix),
			atom_concat(Prefix, RestURI, URI),
			ExpressionStart is Open + 1,
			sub_atom(URITemplate, Close, 1, _, '}'),
			Close >= ExpressionStart,
			TailStart is Close + 1,
			sub_atom(URITemplate, TailStart, _, 0, TailTemplate),
			resource_template_tail_matches(TailTemplate, RestURI)
		;	URITemplate == URI
		).

	resource_template_tail_matches(TailTemplate, URI) :-
		(	sub_atom(TailTemplate, Open, 1, _, '{') ->
			sub_atom(TailTemplate, 0, Open, _, Literal),
			sub_atom(URI, Before, _, After, Literal),
			RestStart is Before + Open,
			sub_atom(URI, RestStart, After, 0, RestURI),
			ExpressionStart is Open + 1,
			sub_atom(TailTemplate, Close, 1, _, '}'),
			Close >= ExpressionStart,
			NextStart is Close + 1,
			sub_atom(TailTemplate, NextStart, _, 0, NextTemplate),
			resource_template_tail_matches(NextTemplate, RestURI)
		;	atom_concat(_, TailTemplate, URI)
		).

	% completion requests and results

	completion_request(Application, Params, Reference, ArgumentName-PartialValue, Context) :-
		conforms_to_protocol(Application, mcp_completion_protocol),
		has_pair(Params, ref, JsonReference),
		has_pair(Params, argument, JsonArgument),
		has_pair(JsonArgument, name, ArgumentName),
		has_pair(JsonArgument, value, PartialValue),
		atom(ArgumentName),
		atom(PartialValue),
		completion_reference(Application, JsonReference, ArgumentName, Reference),
		completion_context(Params, Context).

	completion_reference(Application, JsonReference, ArgumentName, prompt(Name)) :-
		has_pair(JsonReference, type, 'ref/prompt'),
		has_pair(JsonReference, name, Name),
		atom(Name),
		conforms_to_protocol(Application, mcp_prompt_protocol),
		Application::prompts(PromptDescriptors),
		member(PromptDescriptor, PromptDescriptors),
		prompt_descriptor_name_arguments(PromptDescriptor, Name, Arguments),
		member(argument(ArgumentName, _, _), Arguments),
		!.
	completion_reference(Application, JsonReference, ArgumentName, resource(URI)) :-
		has_pair(JsonReference, type, 'ref/resource'),
		has_pair(JsonReference, uri, URI),
		atom(URI),
		conforms_to_protocol(Application, mcp_resource_protocol),
		(	Application::resources(ResourceDescriptors),
			member(ResourceDescriptor, ResourceDescriptors),
			resource_descriptor_uri(ResourceDescriptor, URI) ->
			true
		;	Application::resource_templates(ResourceTemplateDescriptors),
			member(ResourceTemplateDescriptor, ResourceTemplateDescriptors),
			resource_template_uri(ResourceTemplateDescriptor, URI),
			uri_template(atom)::variables(URI, Variables),
			memberchk(ArgumentName, Variables)
		),
		!.

	prompt_descriptor_name_arguments(prompt(Name, _, Arguments), Name, Arguments).
	prompt_descriptor_name_arguments(prompt(Name, _, _, Arguments), Name, Arguments).

	resource_descriptor_uri(resource(URI, _, _, _), URI).
	resource_descriptor_uri(resource(URI, _, _, _, _), URI).

	completion_context(Params, Context) :-
		(	has_pair(Params, context, JsonContext) ->
			(	has_pair(JsonContext, arguments, JsonArguments) ->
				curly_to_pairs(JsonArguments, Context),
				completion_context_pairs(Context)
			;	JsonContext = {},
				Context = []
			)
		;	Context = []
		).

	completion_context_pairs([]).
	completion_context_pairs([Name-Value| Pairs]) :-
		atom(Name),
		atom(Value),
		completion_context_pairs(Pairs).

	completion_result_to_json(completion(Values), JsonCompletion) :-
		completion_values(Values, LimitedValues, Truncated),
		(	Truncated == true ->
			JsonCompletion = {values-LimitedValues, hasMore- @true}
		;	JsonCompletion = {values-LimitedValues}
		).
	completion_result_to_json(completion(Values, Total, HasMore), {values-LimitedValues, total-Total, hasMore-JsonHasMore}) :-
		integer(Total),
		Total >= 0,
		boolean_to_json(HasMore, JsonHasMore0),
		completion_values(Values, LimitedValues, Truncated),
		(	Truncated == true ->
			JsonHasMore = @true
		;	JsonHasMore = JsonHasMore0
		).

	completion_values(Values, LimitedValues, Truncated) :-
		completion_values(Values, 0, LimitedValues, Truncated).

	completion_values([], _, [], false).
	completion_values([Value| Values], Count, LimitedValues, Truncated) :-
		atom(Value),
		(	Count < 100 ->
			LimitedValues = [Value| RestValues],
			NextCount is Count + 1,
			completion_values(Values, NextCount, RestValues, Truncated)
		;	completion_values_atoms(Values),
			LimitedValues = [],
			Truncated = true
		).

	completion_values_atoms([]).
	completion_values_atoms([Value| Values]) :-
		atom(Value),
		completion_values_atoms(Values).

	% auto-dispatch and tool execution predicates

	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result) :-
		catch(
			auto_dispatch_tool_(Application, Functor, Arity, ToolArguments, Result),
			Error,
			Result = error(Error)
		).

	auto_dispatch_tool_(Application, Functor, Arity, ToolArguments, Result) :-
		functor(Goal, Functor, Arity),
		(	Application::predicate_property(Goal, mode(ModeTemplate, _)) ->
			ModeTemplate =.. [_| Modes]
		;	length(Modes, Arity),
			fill_default_modes(Arity, Modes)
		),
		(	Application::predicate_property(Goal, info(InfoPairs)),
			(	info_pair_value(InfoPairs, arguments, Arguments) ->
				pairs_keys(Arguments, Names)
			;	info_pair_value(InfoPairs, argnames, Names)
			) ->
			true
		;	generate_argument_names(1, Arity, Names)
		),
		bind_input_arguments(Names, Modes, 1, Goal, ToolArguments),
		( 	Application::Goal ->
			collect_output_arguments(Names, Modes, 1, Goal, OutputPairs),
			( 	OutputPairs == [] ->
				Result = structured([text('Success')], {})
			;	format_output_pairs(OutputPairs, Text),
				pairs_to_curly(OutputPairs, StructuredContent),
				Result = structured([text(Text)], StructuredContent)
			)
		;	Result = failure
		).

	try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result) :-
		(	conforms_to_protocol(Application, mcp_tool_protocol),
			Application::tool_call(ToolName, ArgPairs, Result) ->
			true
		;	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result)
		).

	% canonical result formatting (content items)

	format_content_items([], []).
	format_content_items([Item| Items], Elements) :-
		format_content_items(Item, Items, Elements).

	format_content_items(text(Text), Items, [Element| Elements]) :-
		Element = {type-text, text-Text},
		format_content_items(Items, Elements).
	format_content_items(error(Error), Items, [Element| Elements]) :-
		(	atom(Error) ->
			ErrorText = Error
		;	write_to_atom(Error, ErrorText)
		),
		Element = {type-text, text-ErrorText},
		format_content_items(Items, Elements).
	format_content_items(resource_link(URI, Name), Items, [Element| Elements]) :-
		Element = {type-resource_link, uri-URI, name-Name},
		format_content_items(Items, Elements).
	format_content_items(resource_link(URI, Name, Description, MimeType), Items, [Element| Elements]) :-
		Element = {type-resource_link, uri-URI, name-Name, description-Description, mimeType-MimeType},
		format_content_items(Items, Elements).

	format_prompt_messages([], []).
	format_prompt_messages([message(Role, text(Text))| Rest], [JsonMsg| JsonRest]) :-
		JsonMsg = {role-Role, content-{type-text, text-Text}},
		format_prompt_messages(Rest, JsonRest).

	format_resource_contents([], []).
	format_resource_contents([Content| Contents], JsonContents) :-
		format_resource_contents(Content, Contents, JsonContents).

	format_resource_contents(text_content(URI, MimeType, Text), Contents, [JsonContent| JsonContents]) :-
		JsonContent = {uri-URI, mimeType-MimeType, text-Text},
		format_resource_contents(Contents, JsonContents).
	format_resource_contents(text_content(URI, MimeType, Text, Meta), Contents, [JsonContent| JsonContents]) :-
		JsonContent = {uri-URI, mimeType-MimeType, text-Text, '_meta'-Meta},
		format_resource_contents(Contents, JsonContents).
	format_resource_contents(blob_content(URI, MimeType, Base64Data), Contents, [JsonContent| JsonContents]) :-
		JsonContent = {uri-URI, mimeType-MimeType, blob-Base64Data},
		format_resource_contents(Contents, JsonContents).
	format_resource_contents(blob_content(URI, MimeType, Base64Data, Meta), Contents, [JsonContent| JsonContents]) :-
		JsonContent = {uri-URI, mimeType-MimeType, blob-Base64Data, '_meta'-Meta},
		format_resource_contents(Contents, JsonContents).

	% argument binding / collection / schema predicates

	bind_input_arguments([], [], _, _, _).
	bind_input_arguments([Name| Names], [Mode| Modes], N, Goal, ToolArguments) :-
		(	is_input_mode(Mode) ->
			(	has_pair(ToolArguments, Name, Value) ->
				arg(N, Goal, Value)
			;	true
			)
		;	true
		),
		N1 is N + 1,
		bind_input_arguments(Names, Modes, N1, Goal, ToolArguments).

	collect_output_arguments([], [], _, _, []).
	collect_output_arguments([Name| Names], [Mode| Modes], N, Goal, Pairs) :-
		(	is_output_mode(Mode) ->
			arg(N, Goal, Value),
			Pairs = [Name-Value| RestPairs]
		;	Pairs = RestPairs
		),
		N1 is N + 1,
		collect_output_arguments(Names, Modes, N1, Goal, RestPairs).

	is_input_mode(+_).
	is_input_mode(++_).
	is_input_mode(@_).
	is_input_mode(+).
	is_input_mode(++).
	is_input_mode(@).

	is_output_mode(-_).
	is_output_mode(--_).
	is_output_mode(-).
	is_output_mode(--).

	build_schema_properties([], [], _, [], []).
	build_schema_properties([Name| Names], [Mode| Modes], N, Properties, Required) :-
		N1 is N + 1,
		(	is_input_mode(Mode) ->
			mode_to_json_type(Mode, JsonType),
			Properties = [Name-{type-JsonType}| RestProperties],
			Required = [Name| RestRequired]
		;	Properties = RestProperties,
			Required = RestRequired
		),
		build_schema_properties(Names, Modes, N1, RestProperties, RestRequired).

	build_output_schema_properties([], [], _, [], []).
	build_output_schema_properties([Name| Names], [Mode| Modes], N, Properties, Required) :-
		N1 is N + 1,
		(	is_output_mode(Mode) ->
			mode_to_json_type(Mode, JsonType),
			Properties = [Name-{type-JsonType}| RestProperties],
			Required = [Name| RestRequired]
		;	Properties = RestProperties,
			Required = RestRequired
		),
		build_output_schema_properties(Names, Modes, N1, RestProperties, RestRequired).

	mode_to_json_type(Mode, Type) :-
		(	nonvar(Mode), Mode = (+ModeType) -> true
		;	nonvar(Mode), Mode = (++ModeType) -> true
		;	nonvar(Mode), Mode = (@ModeType) -> true
		;	nonvar(Mode), Mode = (-ModeType) -> true
		;	nonvar(Mode), Mode = (--ModeType) -> true
		;	ModeType = any
		),
		(	var(ModeType) ->
			Type = string
		;	logtalk_type_to_json(ModeType, Type) ->
			true
		;	Type = string
		).

	logtalk_type_to_json(integer, integer).
	logtalk_type_to_json(float, number).
	logtalk_type_to_json(number, number).
	logtalk_type_to_json(atom, string).
	logtalk_type_to_json(boolean, boolean).
	logtalk_type_to_json(list, array).
	logtalk_type_to_json(list(_), array).
	logtalk_type_to_json(compound, object).
	logtalk_type_to_json(nonvar, string).
	logtalk_type_to_json(term, string).
	logtalk_type_to_json(chars, string).
	logtalk_type_to_json(codes, string).

	generate_argument_names(N, Arity, []) :-
		N > Arity,
		!.
	generate_argument_names(N, Arity, [Name| Names]) :-
		atomic_concat(arg, N, Name),
		N1 is N + 1,
		generate_argument_names(N1, Arity, Names).

	fill_default_modes(0, []) :-
		!.
	fill_default_modes(N, [(+)| Modes]) :-
		N > 0,
		N1 is N - 1,
		fill_default_modes(N1, Modes).

	info_pair_value([Pair| _], Key, Value) :-
		Pair =.. [Key, Value],
		!.
	info_pair_value([_| Pairs], Key, Value) :-
		info_pair_value(Pairs, Key, Value).

	format_output_pairs([], '').
	format_output_pairs([Name-Value| Pairs], Text) :-
		write_to_atom(Value, ValueAtom),
		(	Pairs == [] ->
			atomic_list_concat([Name, ': ', ValueAtom], Text)
		;	atomic_list_concat([Name, ': ', ValueAtom], Line),
			format_output_pairs(Pairs, PairsText),
			atomic_list_concat([Line, '\n', PairsText], Text)
		).

	% curly-term predicates (shared)

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Pairs)) :-
		!,
		curly_member(Pair, Pairs).
	curly_member(Pair, Pair).

	curly_to_pairs({}, []) :-
		!.
	curly_to_pairs({Pairs}, List) :-
		curly_pairs_to_list(Pairs, List).

	curly_pairs_to_list((Key-Value, CurlyPairs), [Key-Value| Pairs]) :-
		!,
		curly_pairs_to_list(CurlyPairs, Pairs).
	curly_pairs_to_list(Key-Value, [Key-Value]).

	pairs_to_curly([], {}) :-
		!.
	pairs_to_curly(Pairs, {CurlyPairs}) :-
		list_to_curly_pairs(Pairs, CurlyPairs).

	list_to_curly_pairs([Key-Value], Key-Value) :-
		!.
	list_to_curly_pairs([Key-Value| Pairs], (Key-Value, CurlyPairs)) :-
		list_to_curly_pairs(Pairs, CurlyPairs).

	pairs_keys([], []).
	pairs_keys([Key-_| Pairs], [Key| Keys]) :-
		pairs_keys(Pairs, Keys).

	% MCP Apps (io.modelcontextprotocol/ui) predicates

	application_tool_ui_meta(Application, ToolName, {ui-UI}) :-
		Application::tool_ui(ToolName, Options),
		Options = [_| _],
		tool_ui_options_to_ui(Options, UI).

	tool_ui_options_to_ui(Options, UI) :-
		findall(Pair, tool_ui_option_pair(Options, Pair), Pairs),
		Pairs = [_| _],
		pairs_to_curly(Pairs, UI).

	tool_ui_option_pair(Options, resourceUri-URI) :-
		member(resource_uri(URI), Options),
		atom(URI).
	tool_ui_option_pair(Options, visibility-Vis) :-
		member(visibility(Vis), Options),
		is_non_empty_list(Vis).

	application_resource_ui_meta(Application, URI, {ui-UI}) :-
		Application::resource_ui_meta(URI, UI),
		nonvar(UI).

	add_meta_field({Pairs0}, UIMeta, {('_meta'-UIMeta, Pairs0)}) :-
		nonvar(Pairs0),
		Pairs0 \== {},
		!.
	add_meta_field({_}, UIMeta, {'_meta'-UIMeta}) :-
		!.
	add_meta_field(Other, _, Other).

	is_non_empty_list([_| _]).

:- end_category.
