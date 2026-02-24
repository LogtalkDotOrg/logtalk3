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


% Test application object implementing mcp_tool_protocol

:- object(test_tools,
	implements(mcp_tool_protocol)).

	:- public(factorial/2).
	:- mode(factorial(+integer, -integer), one).
	:- info(factorial/2, [
		comment is 'Computes the factorial of a non-negative integer.',
		argnames is ['N', 'F']
	]).

	:- public(greet/2).
	:- mode(greet(+atom, -atom), one).
	:- info(greet/2, [
		comment is 'Generates a greeting message for the given name.',
		argnames is ['Name', 'Greeting']
	]).

	:- public(add/3).
	:- mode(add(+number, +number, -number), one).
	:- info(add/3, [
		comment is 'Adds two numbers.',
		argnames is ['X', 'Y', 'Sum']
	]).

	:- public(always_fails/1).
	:- mode(always_fails(+atom), zero).
	:- info(always_fails/1, [
		comment is 'A tool that always fails (for testing error handling).',
		argnames is ['Input']
	]).

	tools([
		tool(factorial, factorial, 2),
		tool(greet, greet, 2),
		tool(add, add, 3),
		tool(always_fails, always_fails, 1)
	]).

	factorial(0, 1) :-
		!.
	factorial(N, F) :-
		N > 0,
		N1 is N - 1,
		factorial(N1, F1),
		F is N * F1.

	greet(Name, Greeting) :-
		atom_concat('Hello, ', Name, Temp),
		atom_concat(Temp, '!', Greeting).

	add(X, Y, Sum) :-
		Sum is X + Y.

	always_fails(_) :-
		fail.

:- end_object.


% Test application with custom tool_call/3

:- object(test_custom_tools,
	implements(mcp_tool_protocol)).

	:- public(double/2).
	:- mode(double(+number, -number), one).
	:- info(double/2, [
		comment is 'Doubles a number.',
		argnames is ['N', 'Result']
	]).

	:- uses(list, [
		memberchk/2
	]).

	tools([
		tool(double, double, 2)
	]).

	tool_call(double, Arguments, Result) :-
		memberchk('N'-N, Arguments),
		D is N * 2,
		number_codes(D, Codes),
		atom_codes(DAtom, Codes),
		atom_concat('Double is: ', DAtom, Text),
		Result = text(Text).

	double(N, D) :-
		D is N * 2.

:- end_object.


% Test application with elicitation support

:- object(test_elicit_tools,
	implements(mcp_tool_protocol)).

	:- public(ask_name/0).
	:- mode(ask_name, one).
	:- info(ask_name/0, [
		comment is 'Asks the user for their name via elicitation.'
	]).

	:- public(echo/2).
	:- mode(echo(+atom, -atom), one).
	:- info(echo/2, [
		comment is 'Echoes the input.',
		argnames is ['Input', 'Output']
	]).

	capabilities([elicitation]).

	tools([
		tool(ask_name, ask_name, 0),
		tool(echo, echo, 2)
	]).

	:- meta_predicate(tool_call(*, *, 3, *)).

	tool_call(ask_name, _Arguments, Elicit, Result) :-
		Schema = {type-object, properties-{name-{type-string}}, required-[name]},
		call(Elicit, 'What is your name?', Schema, Answer),
		(	Answer = accept(Content),
			has_pair(Content, name, Name) ->
			atom_concat('Hello, ', Name, Temp),
			atom_concat(Temp, '!', Greeting),
			Result = text(Greeting)
		;	Answer == decline ->
			Result = text('User declined.')
		;	Result = text('Cancelled.')
		).

	echo(Input, Input).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.


% Test application with prompts support

:- object(test_prompt_tools,
	implements([mcp_tool_protocol, mcp_prompt_protocol])).

	:- uses(list, [
		member/2
	]).

	capabilities([prompts]).

	tools([]).

	prompts([
		prompt(greet_prompt, 'Generates a greeting prompt', [
			argument(name, 'The name to greet', true)
		]),
		prompt(simple_prompt, 'A simple prompt with no arguments', []),
		prompt(multi_turn_prompt, 'A multi-turn conversation prompt', [
			argument(topic, 'The topic to discuss', true),
			argument(style, 'The conversation style', false)
		])
	]).

	prompt_get(greet_prompt, Arguments, Result) :-
		(	member(name-Name, Arguments) ->
			atom_concat('Please greet ', Name, Temp),
			atom_concat(Temp, ' warmly.', Text)
		;	Text = 'Please greet someone.'
		),
		Result = messages([message(user, text(Text))]).

	prompt_get(simple_prompt, _Arguments, Result) :-
		Result = messages(
			'A simple test prompt',
			[message(user, text('This is a simple test prompt.'))]
		).

	prompt_get(multi_turn_prompt, Arguments, Result) :-
		(	member(topic-Topic, Arguments) ->
			atom_concat('Let us discuss ', Topic, Temp),
			atom_concat(Temp, '.', UserText)
		;	UserText = 'Let us discuss something.'
		),
		Result = messages([
			message(user, text(UserText)),
			message(assistant, text('I would be happy to discuss that topic. What would you like to know?'))
		]).

:- end_object.


% Test application with both tools and prompts

:- object(test_tools_and_prompts,
	implements([mcp_tool_protocol, mcp_prompt_protocol])).

	capabilities([prompts]).

	:- public(add/3).
	:- mode(add(+number, +number, -number), one).
	:- info(add/3, [
		comment is 'Adds two numbers.',
		argnames is ['X', 'Y', 'Sum']
	]).

	:- uses(list, [
		member/2
	]).

	tools([
		tool(add, add, 3)
	]).

	add(X, Y, Sum) :-
		Sum is X + Y.

	prompts([
		prompt(summarize, 'Summarizes a given text', [
			argument(text, 'The text to summarize', true)
		])
	]).

	prompt_get(summarize, Arguments, Result) :-
		(	member(text-Text, Arguments) ->
			atom_concat('Please summarize the following text:\n\n', Text, PromptText)
		;	PromptText = 'Please summarize a text.'
		),
		Result = messages([message(user, text(PromptText))]).

:- end_object.


% Test application with resources support

:- object(test_resource_tools,
	implements([mcp_tool_protocol, mcp_resource_protocol])).

	capabilities([resources]).

	tools([]).

	resources([
		resource('logtalk://test/greeting', greeting, 'A greeting message', 'text/plain'),
		resource('logtalk://test/config', config, 'Application configuration', 'application/json'),
		resource('logtalk://test/multi', multi, 'Multiple content items', 'text/plain')
	]).

	resource_read('logtalk://test/greeting', _Arguments, Result) :-
		Result = contents([
			text_content('logtalk://test/greeting', 'text/plain', 'Hello from Logtalk!')
		]).

	resource_read('logtalk://test/config', _Arguments, Result) :-
		Result = contents([
			text_content('logtalk://test/config', 'application/json', '{"name": "test", "version": "1.0"}')
		]).

	resource_read('logtalk://test/multi', _Arguments, Result) :-
		Result = contents([
			text_content('logtalk://test/multi', 'text/plain', 'First part'),
			text_content('logtalk://test/multi', 'text/plain', 'Second part')
		]).

:- end_object.


% Test application with tools, prompts, and resources

:- object(test_all_capabilities,
	implements([mcp_tool_protocol, mcp_prompt_protocol, mcp_resource_protocol])).

	capabilities([prompts, resources]).

	:- public(add/3).
	:- mode(add(+number, +number, -number), one).
	:- info(add/3, [
		comment is 'Adds two numbers.',
		argnames is ['X', 'Y', 'Sum']
	]).

	tools([
		tool(add, add, 3)
	]).

	add(X, Y, Sum) :-
		Sum is X + Y.

	prompts([
		prompt(helper, 'A helper prompt', [])
	]).

	prompt_get(helper, _Arguments, Result) :-
		Result = messages([message(user, text('Help me with something.'))]).

	resources([
		resource('logtalk://test/data', data, 'Some data', 'text/plain')
	]).

	resource_read('logtalk://test/data', _Arguments, Result) :-
		Result = contents([
			text_content('logtalk://test/data', 'text/plain', 'Some test data')
		]).

:- end_object.


% Test application with tool titles, output schemas, structured results, and resource links

:- object(test_structured_tools,
	implements([mcp_tool_protocol, mcp_prompt_protocol, mcp_resource_protocol])).

	capabilities([prompts, resources]).

	:- public(divide/3).
	:- mode(divide(+number, +number, -number), zero_or_one).
	:- info(divide/3, [
		comment is 'Divides two numbers.',
		title is 'Division Tool',
		argnames is ['X', 'Y', 'Quotient']
	]).

	:- public(square/2).
	:- mode(square(+number, -number), one).
	:- info(square/2, [
		comment is 'Computes the square of a number.',
		argnames is ['N', 'Square']
	]).

	:- uses(list, [
		member/2
	]).

	tools([
		tool(divide, divide, 3),
		tool(square, square, 2)
	]).

	output_schema(divide, {type-object, properties-{quotient-{type-number}}, required-[quotient]}).

	tool_call(divide, Arguments, Result) :-
		member('X'-X, Arguments),
		member('Y'-Y, Arguments),
		(	Y =:= 0 ->
			Result = error('Division by zero')
		;	Q is float(X / Y),
			Result = structured({quotient-Q})
		).

	tool_call(square, Arguments, Result) :-
		member('N'-N, Arguments),
		S is N * N,
		number_codes(S, Codes),
		atom_codes(SAtom, Codes),
		atom_concat('The square is: ', SAtom, Text),
		Result = structured([text(Text)], {result-S}).

	divide(X, Y, Q) :-
		Y =\= 0,
		Q is X / Y.

	square(N, S) :-
		S is N * N.

	% Prompts with title (4-arg descriptor)
	prompts([
		prompt(explain, 'Explain Tool', 'Explains a concept in detail', [
			argument(concept, 'The concept to explain', true)
		])
	]).

	prompt_get(explain, Arguments, Result) :-
		(	member(concept-Concept, Arguments) ->
			atom_concat('Please explain: ', Concept, Text)
		;	Text = 'Please explain something.'
		),
		Result = messages([message(user, text(Text))]).

	% Resources with title (5-arg descriptor)
	resources([
		resource('logtalk://test/status', status, 'System Status', 'Current system status', 'application/json')
	]).

	resource_read('logtalk://test/status', _Arguments, Result) :-
		Result = contents([
			text_content('logtalk://test/status', 'application/json', '{"status": "ok"}')
		]).

:- end_object.


% Test application with resource_link content items

:- object(test_resource_link_tools,
	implements(mcp_tool_protocol)).

	:- public(find_docs/2).
	:- mode(find_docs(+atom, -atom), one).
	:- info(find_docs/2, [
		comment is 'Finds documentation for a topic.',
		argnames is ['Topic', 'Result']
	]).

	:- uses(list, [
		member/2
	]).

	tools([
		tool(find_docs, find_docs, 2)
	]).

	tool_call(find_docs, Arguments, Result) :-
		member('Topic'-Topic, Arguments),
		atom_concat('logtalk://docs/', Topic, URI),
		Result = results([
			text('Found documentation:'),
			resource_link(URI, Topic),
			resource_link(URI, Topic, 'Documentation page', 'text/html')
		]).

	find_docs(_, found).

:- end_object.


% Test application with multi-result and exception tools

:- object(test_result_tools,
	implements(mcp_tool_protocol)).

	:- public(multi_result/0).
	:- mode(multi_result, one).
	:- info(multi_result/0, [
		comment is 'Returns multiple content items.'
	]).

	:- public(throw_error/0).
	:- mode(throw_error, one).
	:- info(throw_error/0, [
		comment is 'Always throws an error.'
	]).

	tools([
		tool(multi_result, multi_result, 0),
		tool(throw_error, throw_error, 0)
	]).

	tool_call(multi_result, _Arguments, Result) :-
		Result = results([text('Line 1'), text('Line 2')]).

	tool_call(throw_error, _Arguments, _Result) :-
		throw(error(resource_error(memory), test)).

:- end_object.
