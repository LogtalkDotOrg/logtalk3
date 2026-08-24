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
		date is 2026-08-24,
		comment is 'Specification-independent application logic for MCP servers: tool/prompt/resource descriptor conversion, schema derivation from info/2 and mode/2, auto-dispatch, canonical complete-result terms, curly-term predicates, and MCP Apps (``_meta.ui``) metadata. Imported by both the 2025-06-18 and 2026-07-28 adapters.'
	]).

	:- public(tool_descriptors_to_json/3).
	:- mode(tool_descriptors_to_json(+list, +object_identifier, -list), one).
	:- info(tool_descriptors_to_json/3, [
		comment is 'Converts a list of tool(Name, Functor, Arity) descriptors into MCP JSON tool definitions, deriving titles, descriptions and input schemas from the application object''s info/2 and mode/2 directives.',
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

	:- protected(auto_dispatch_tool/5).
	:- mode(auto_dispatch_tool(+object_identifier, +atom, +integer, +compound, -compound), one).
	:- info(auto_dispatch_tool/5, [
		comment is 'Auto-dispatches a tool call by calling the predicate on the application, collecting output-mode arguments, and returning a text result.',
		argnames is ['Application', 'Functor', 'Arity', 'ToolArguments', 'Result']
	]).

	:- protected(try_tool_call_3/7).
	:- mode(try_tool_call_3(+object_identifier, +atom, +atom, +integer, +list, +compound, -compound), one).
	:- info(try_tool_call_3/7, [
		comment is 'Tries tool_call/3; on existence_error falls back to auto-dispatch.',
		argnames is ['Application', 'ToolName', 'Functor', 'Arity', 'ArgPairs', 'ToolArguments', 'Result']
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
		tool_input_schema(Application, Functor, Arity, InputSchema),
		(	catch(Application::output_schema(Name, OutputSchema), _, fail) ->
			JsonTool0 = {
				name-Name,
				title-Title,
				description-Description,
				inputSchema-InputSchema,
				outputSchema-OutputSchema
			}
		;	JsonTool0 = {
				name-Name,
				title-Title,
				description-Description,
				inputSchema-InputSchema
			}
		),
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
		),
		build_schema_properties(ArgNames, ModeArgs, 1, Properties, Required),
		(	Properties == [] ->
			InputSchema = {type-object, properties-{}}
		;	pairs_to_curly(Properties, PropertiesCurly),
			InputSchema = {type-object, properties-PropertiesCurly, required-Required}
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
		bool_to_json(Required, JsonRequired),
		JsonArg = {name-ArgName, description-ArgDescription, required-JsonRequired},
		prompt_arguments_to_json(Rest, JsonRest).

	bool_to_json(true, @true).
	bool_to_json(false, @false).

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

	% auto-dispatch and tool execution predicates

	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result) :-
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
		Application::Goal,
		collect_output_arguments(Names, Modes, 1, Goal, OutputPairs),
		(	OutputPairs == [] ->
			Result = text('Success')
		;	format_output_pairs(OutputPairs, Text),
			Result = text(Text)
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

	mode_to_json_type(Mode, Type) :-
		(	nonvar(Mode), Mode = (+ModeType) -> true
		;	nonvar(Mode), Mode = (++ModeType) -> true
		;	nonvar(Mode), Mode = (@ModeType) -> true
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
