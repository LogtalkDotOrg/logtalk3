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

	factorial(0, 1) :- !.
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

	tools([
		tool(double, double, 2)
	]).

	tool_call(double, Arguments, Result) :-
		member('N'-N, Arguments),
		D is N * 2,
		number_codes(D, Codes),
		atom_codes(DAtom, Codes),
		atom_concat('Double is: ', DAtom, Text),
		Result = text(Text).

	double(N, D) :-
		D is N * 2.

	:- uses(list, [member/2]).

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

	curly_member(Pair, (Pair, _)) :- !.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

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
