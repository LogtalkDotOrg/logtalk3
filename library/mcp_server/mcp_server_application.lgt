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
		date is 2026-08-14,
		comment is 'Specification-independent application logic for MCP servers: tool/prompt/resource descriptor conversion, schema derivation from info/2 and mode/2, auto-dispatch, canonical complete-result terms, and curly-term helpers. Imported by both the 2025-06-18 and 2026-07-28 adapters.'
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(term_io, [
		write_to_atom/2
	]).

	:- uses(user, [
		atomic_concat/3, atomic_list_concat/2
	]).

	% ---------------------------------------------------------------------
	% Tool descriptors and schemas
	% ---------------------------------------------------------------------

	:- public(tool_descriptors_to_json/3).
	:- mode(tool_descriptors_to_json(+list, +object_identifier, -list), one).
	:- info(tool_descriptors_to_json/3, [
		comment is 'Converts a list of tool(Name, Functor, Arity) descriptors into MCP JSON tool definitions, deriving titles, descriptions and input schemas from the application object''s info/2 and mode/2 directives.',
		argnames is ['ToolDescriptors', 'Application', 'JsonTools']
	]).

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
			JsonTool = {
				name-Name,
				title-Title,
				description-Description,
				inputSchema-InputSchema,
				outputSchema-OutputSchema
			}
		;	JsonTool = {
				name-Name,
				title-Title,
				description-Description,
				inputSchema-InputSchema
			}
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
		;	generate_arg_names(1, Arity, ArgNames)
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

	% ---------------------------------------------------------------------
	% Prompt descriptors
	% ---------------------------------------------------------------------

	:- public(prompt_descriptors_to_json/2).
	:- mode(prompt_descriptors_to_json(+list, -list), one).
	:- info(prompt_descriptors_to_json/2, [
		comment is 'Converts prompt descriptors (3-arg or 4-arg) into MCP JSON prompt definitions.',
		argnames is ['PromptDescriptors', 'JsonPrompts']
	]).

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

	% ---------------------------------------------------------------------
	% Resource descriptors
	% ---------------------------------------------------------------------

	:- public(resource_descriptors_to_json/2).
	:- mode(resource_descriptors_to_json(+list, -list), one).
	:- info(resource_descriptors_to_json/2, [
		comment is 'Converts resource descriptors (4-arg or 5-arg) into MCP JSON resource definitions.',
		argnames is ['Descriptors', 'Resources']
	]).

	resource_descriptors_to_json([], []).
	resource_descriptors_to_json([Descriptor| Descriptors], Resources) :-
		resource_descriptors_to_json(Descriptor, Descriptors, Resources).

	resource_descriptors_to_json(resource(URI, Name, Title, Description, MimeType), Descriptors, [Resource| Resources]) :-
		Resource = {uri-URI, name-Name, title-Title, description-Description, mimeType-MimeType},
		resource_descriptors_to_json(Descriptors, Resources).
	resource_descriptors_to_json(resource(URI, Name, Description, MimeType), Descriptors, [Resource| Resources]) :-
		Resource = {uri-URI, name-Name, description-Description, mimeType-MimeType},
		resource_descriptors_to_json(Descriptors, Resources).

	% ---------------------------------------------------------------------
	% Auto-dispatch and tool execution helpers
	% ---------------------------------------------------------------------

	:- public(auto_dispatch_tool/5).
	:- mode(auto_dispatch_tool(+object_identifier, +atom, +integer, +compound, -compound), one).
	:- info(auto_dispatch_tool/5, [
		comment is 'Auto-dispatches a tool call by calling the predicate on the application, collecting output-mode arguments, and returning a text result.',
		argnames is ['Application', 'Functor', 'Arity', 'ToolArguments', 'Result']
	]).

	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result) :-
		functor(Goal, Functor, Arity),
		(	Application::predicate_property(Goal, mode(ModeTemplate, _)) ->
			ModeTemplate =.. [_| ModeArgs]
		;	length(ModeArgs, Arity),
			fill_default_modes(Arity, ModeArgs)
		),
		(	Application::predicate_property(Goal, info(InfoPairs)),
			(	info_pair_value(InfoPairs, arguments, Arguments) ->
				pairs_keys(Arguments, ArgNames)
			;	info_pair_value(InfoPairs, argnames, ArgNames)
			) ->
			true
		;	generate_arg_names(1, Arity, ArgNames)
		),
		bind_input_arguments(ArgNames, ModeArgs, 1, Goal, ToolArguments),
		Application::Goal,
		collect_output_arguments(ArgNames, ModeArgs, 1, Goal, OutputPairs),
		(	OutputPairs == [] ->
			Result = text('Success')
		;	format_output_pairs(OutputPairs, Text),
			Result = text(Text)
		).

	:- public(try_tool_call_3/7).
	:- mode(try_tool_call_3(+object_identifier, +atom, +atom, +integer, +list, +compound, -compound), one).
	:- info(try_tool_call_3/7, [
		comment is 'Tries tool_call/3; on existence_error falls back to auto-dispatch.',
		argnames is ['Application', 'ToolName', 'Functor', 'Arity', 'ArgPairs', 'ToolArguments', 'Result']
	]).

	try_tool_call_3(Application, ToolName, Functor, Arity, ArgPairs, ToolArguments, Result) :-
		(	catch(
				Application::tool_call(ToolName, ArgPairs, Result),
				error(existence_error(procedure, _), _),
				fail
			) ->
			true
		;	auto_dispatch_tool(Application, Functor, Arity, ToolArguments, Result)
		).

	% ---------------------------------------------------------------------
	% Canonical result formatting (content items)
	% ---------------------------------------------------------------------

	:- public(format_content_items/2).
	:- mode(format_content_items(+list, -list), one).
	:- info(format_content_items/2, [
		comment is 'Converts canonical content item terms into MCP JSON content array elements.',
		argnames is ['Items', 'Elements']
	]).

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

	:- public(format_prompt_messages/2).
	:- mode(format_prompt_messages(+list, -list), one).
	:- info(format_prompt_messages/2, [
		comment is 'Converts message(Role, text(Text)) terms into MCP JSON prompt message objects.',
		argnames is ['Messages', 'JsonMessages']
	]).

	format_prompt_messages([], []).
	format_prompt_messages([message(Role, text(Text))| Rest], [JsonMsg| JsonRest]) :-
		JsonMsg = {role-Role, content-{type-text, text-Text}},
		format_prompt_messages(Rest, JsonRest).

	:- public(format_resource_contents/2).
	:- mode(format_resource_contents(+list, -list), one).
	:- info(format_resource_contents/2, [
		comment is 'Converts text_content/3 and blob_content/3 terms into MCP JSON resource content objects.',
		argnames is ['Contents', 'JsonContents']
	]).

	format_resource_contents([], []).
	format_resource_contents([Content| Contents], JsonContents) :-
		format_resource_contents(Content, Contents, JsonContents).

	format_resource_contents(text_content(URI, MimeType, Text), Contents, [JsonContent| JsonContents]) :-
		JsonContent = {uri-URI, mimeType-MimeType, text-Text},
		format_resource_contents(Contents, JsonContents).
	format_resource_contents(blob_content(URI, MimeType, Base64Data), Contents, [JsonContent| JsonContents]) :-
		JsonContent = {uri-URI, mimeType-MimeType, blob-Base64Data},
		format_resource_contents(Contents, JsonContents).

	% ---------------------------------------------------------------------
	% Argument binding / collection / schema helpers
	% ---------------------------------------------------------------------

	bind_input_arguments([], [], _, _, _).
	bind_input_arguments([ArgName| ArgNames], [Mode| Modes], N, Goal, ToolArguments) :-
		(	is_input_mode(Mode) ->
			(	has_pair(ToolArguments, ArgName, Value) ->
				arg(N, Goal, Value)
			;	true
			)
		;	true
		),
		N1 is N + 1,
		bind_input_arguments(ArgNames, Modes, N1, Goal, ToolArguments).

	collect_output_arguments([], [], _, _, []).
	collect_output_arguments([ArgName| ArgNames], [Mode| Modes], N, Goal, Pairs) :-
		(	is_output_mode(Mode) ->
			arg(N, Goal, Value),
			Pairs = [ArgName-Value| RestPairs]
		;	Pairs = RestPairs
		),
		N1 is N + 1,
		collect_output_arguments(ArgNames, Modes, N1, Goal, RestPairs).

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

	generate_arg_names(N, Arity, []) :-
		N > Arity,
		!.
	generate_arg_names(N, Arity, [ArgName| ArgNames]) :-
		atomic_concat(arg, N, ArgName),
		N1 is N + 1,
		generate_arg_names(N1, Arity, ArgNames).

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

	% ---------------------------------------------------------------------
	% Curly-term helpers (shared)
	% ---------------------------------------------------------------------

	:- public(has_pair/3).
	:- mode(has_pair(+compound, +atom, ?term), zero_or_one).
	:- info(has_pair/3, [
		comment is 'Looks up a Key-Value pair inside a curly-term.',
		argnames is ['Curly', 'Key', 'Value']
	]).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Pairs)) :-
		!,
		curly_member(Pair, Pairs).
	curly_member(Pair, Pair).

	:- public(curly_to_pairs/2).
	:- mode(curly_to_pairs(+compound, -list), one).
	:- info(curly_to_pairs/2, [
		comment is 'Converts a curly-term to a list of Key-Value pairs.',
		argnames is ['Curly', 'Pairs']
	]).

	curly_to_pairs({}, []) :-
		!.
	curly_to_pairs({Pairs}, List) :-
		curly_pairs_to_list(Pairs, List).

	curly_pairs_to_list((Key-Value, CurlyPairs), [Key-Value| Pairs]) :-
		!,
		curly_pairs_to_list(CurlyPairs, Pairs).
	curly_pairs_to_list(Key-Value, [Key-Value]).

	:- public(pairs_to_curly/2).
	:- mode(pairs_to_curly(+list, -compound), one).
	:- info(pairs_to_curly/2, [
		comment is 'Converts a list of Key-Value pairs to a curly-term.',
		argnames is ['Pairs', 'Curly']
	]).

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

:- end_category.
