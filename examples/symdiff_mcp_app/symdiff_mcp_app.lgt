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


:- object(symdiff_mcp_app,
	implements((mcp_tool_protocol, mcp_resource_protocol, mcp_ui_protocol))).

	:- info([
		version is 1:0:0,
		author is 'Example',
		date is 2026-09-01,
		comment is 'MCP Apps demo exposing symbolic differentiation and simplification through an interactive UI.'
	]).

	:- public(differentiate/3).
	:- mode(differentiate(+atom, -atom, -atom), zero_or_one).
	:- info(differentiate/3, [
		comment is 'Differentiates an expression and returns the derivative and its simplified form.',
		argnames is ['Expression', 'Derivative', 'Simplified']
	]).

	:- public(open_symdiff/0).
	:- mode(open_symdiff, one).
	:- info(open_symdiff/0, [
		title is 'Open Symbolic Differentiation App',
		comment is 'Opens an interactive app where the user can enter an expression to differentiate.'
	]).

	:- uses(list, [
		member/2
	]).

	:- uses(os, [
		path_concat/3
	]).

	:- uses(reader, [
		file_to_codes/2
	]).

	:- uses(term_io, [
		read_term_from_atom/3, write_term_to_atom/3
	]).

	capabilities([resources, ui]).

	tools([
		tool(open_symdiff, open_symdiff, 0),
		tool(differentiate, differentiate, 3)
	]).

	input_schema(differentiate, {
		type-object,
		properties-{
			expression-{
				type-string,
				description-'A symbolic expression using integers, x, +, -, *, **, and log/1.'
			}
		},
		required-[expression]
	}).

	output_schema(open_symdiff, {
		type-object,
		properties-{status-{type-string, enum-[idle]}},
		required-[status]
	}).

	output_schema(differentiate, {
		type-object,
		properties-{
			status-{type-string, enum-[idle, success, error]},
			derivative-{type-string},
			simplified-{type-string},
			error-{type-string}
		},
		required-[status, derivative, simplified, error]
	}).

	tool_call(open_symdiff, _Arguments, structured(
		[text('The symbolic differentiation app is ready for input.')],
		{status-idle}
	)).
	tool_call(differentiate, Arguments, Result) :-
		( member(expression-Expression, Arguments) ->
			tool_result(Expression, Result)
		;
			idle_result(Result)
		).

	tool_result(Expression, Result) :-
		blank_expression(Expression),
		!,
		idle_result(Result).
	tool_result(Expression, Result) :-
		atom(Expression),
		catch(differentiate(Expression, Derivative, Simplified), _, fail),
		!,
		atom_concat('Derivative: ', Derivative, Prefix),
		atom_concat(Prefix, '\nSimplified derivative: ', Prefix2),
		atom_concat(Prefix2, Simplified, Text),
		Result = structured(
			[text(Text)],
			{status-success, derivative-Derivative, simplified-Simplified, error-''}
		).
	tool_result(_, Result) :-
		error_result('Invalid expression. Use integers, x, +, -, *, **, and log/1.', Result).

	idle_result(structured(
		[text('The symbolic differentiation app is ready for input.')],
		{status-idle, derivative-'', simplified-'', error-''}
	)).

	blank_expression(Expression) :-
		atom(Expression),
		atom_codes(Expression, Codes),
		whitespace_codes(Codes).

	whitespace_codes([]).
	whitespace_codes([Code| Codes]) :-
		whitespace_code(Code),
		whitespace_codes(Codes).

	whitespace_code(9).
	whitespace_code(10).
	whitespace_code(13).
	whitespace_code(32).

	error_result(Error, structured(
		[text(Error)],
		{status-error, derivative-'', simplified-'', error-Error}
	)).

	differentiate(ExpressionText, DerivativeText, SimplifiedText) :-
		ExpressionText \== '',
		read_term_from_atom(ExpressionText, Expression, []),
		Expression \== end_of_file,
		ground(Expression),
		% sanitize user input
		valid_expression(Expression),
		Expression::diff(Derivative),
		Derivative::simplify(Simplified),
		write_term_to_atom(Derivative, DerivativeText, [quoted(true)]),
		write_term_to_atom(Simplified, SimplifiedText, [quoted(true)]).

	valid_expression(Expression) :-
		integer(Expression),
		!.
	valid_expression(x) :-
		!.
	valid_expression(Left + Right) :-
		!,
		valid_expression(Left),
		valid_expression(Right).
	valid_expression(Left - Right) :-
		!,
		valid_expression(Left),
		valid_expression(Right).
	valid_expression(Left * Right) :-
		!,
		valid_expression(Left),
		valid_expression(Right).
	valid_expression(Base ** Power) :-
		!,
		valid_expression(Base),
		valid_expression(Power).
	valid_expression(log(Expression)) :-
		valid_expression(Expression).

	tool_ui(open_symdiff, [
		resource_uri('ui://symdiff/mcp-app.html'),
		visibility([model, app])
	]).
	tool_ui(differentiate, [
		visibility([app])
	]).

	resources([
		resource(
			'ui://symdiff/mcp-app.html',
			symdiff_ui,
			'Symbolic Differentiation App',
			'Interactive UI for symbolic differentiation and simplification',
			'text/html;profile=mcp-app'
		)
	]).

	resource_read('ui://symdiff/mcp-app.html', _Params, Result) :-
		mcp_app_html(HTML),
		Result = contents([
			text_content(
				'ui://symdiff/mcp-app.html',
				'text/html;profile=mcp-app',
				HTML
			)
		]).

	mcp_app_html(HTML) :-
		this(This),
		object_property(This, file(_, Directory)),
		path_concat(Directory, 'mcp-app.html', File),
		file_to_codes(File, Codes),
		atom_codes(HTML, Codes).

:- end_object.
