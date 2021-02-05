%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Interpreter design pattern:
%
% https://en.wikipedia.org/wiki/Interpreter_pattern


% the Wikipedia page uses as example the following grammar
% (in BNF format) for simple arithmetic expressions written
% in Reverse Polish notation (RPN)
%
% expression ::= plus | minus | variable | number
% plus ::= expression expression '+'
% minus ::= expression expression '-'
% variable ::= 'a' | 'b' | 'c' | ... | 'z'
% digit = '0' | '1' | ... | '9'
% number ::= digit | digit number


% we represent expressions as lists of character
% codes to ensure portability
:- set_prolog_flag(double_quotes, codes).


:- object(interpreter).

	:- public(eval/3).

	:- uses(list, [member/2]).

	% pass variable values using a list of Variable=Value terms
	eval(Expression, Variables, Value) :-
		once(phrase(expression(Variables, Value), Expression)).

	% handle left-recursion in the grammar by
	% tabling the expression//2 non-terminal
	:- table(expression//2).

	expression(Variables, Value) -->
		plus(Variables, Value).
	expression(Variables, Value) -->
		minus(Variables, Value).
	expression(Variables, Value) -->
		variable(Variables, Value).
	expression(_, Value) -->
		number(Value).

	% to simplify, we assume a single space separate expression tokens
	% with no preceding or trailing spaces surrounding the expression

	plus(Variables, Value) -->
		expression(Variables, X), " ", expression(Variables, Y), " ", "+",
		{Value is X + Y}.

	minus(Variables, Value) -->
		expression(Variables, X), " ", expression(Variables, Y), " ", "-",
		{Value is X - Y}.

	variable(Variables, Value) -->
		[Code],
		{0'a =< Code, Code =< 0'z, char_code(Char, Code), member(Char=Value, Variables)}.

	number(Value) -->
		digit(Digit), number(Number),
		{Value is Digit * 10 + Number}.
	number(Value) -->
		digit(Value).

	digit(Value) -->
		[Code],
		{0'0 =< Code, Code =< 0'9, Value is Code - 0'0}.

:- end_object.
