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


:- category(root_finder(_Function_),
	implements(root_finder_protocol),
	extends(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-04,
		comment is 'Common option handling and validation for scalar root finders.',
		parameters is [
			'Function' - 'Object implementing ``univariate_function_protocol``.'
		]
	]).

	:- protected(evaluate_function/2).
	:- mode(evaluate_function(+number, -number), one_or_error).
	:- info(evaluate_function/2, [
		comment is 'Evaluates the function and checks that it returns a number.',
		argnames is ['Argument', 'Value'],
		exceptions is [
			'The function returns a non-numeric value' - domain_error(function_value, 'Value')
		]
	]).

	:- protected(check_bracket/3).
	:- mode(check_bracket(+compound, -number, -number), one_or_error).
	:- info(check_bracket/3, [
		comment is 'Checks and unpacks a root bracket.',
		argnames is ['Bracket', 'Lower', 'Upper'],
		exceptions is [
			'``Bracket`` is a variable' - instantiation_error,
			'``Bracket`` is not a valid increasing numeric bracket' - domain_error(root_bracket, 'Bracket')
		]
	]).

	:- uses(_Function_, [evaluate/2]).

	find_root(Initial, Root) :-
		::find_root(Initial, Root, _Statistics, []).

	find_root(Initial, Root, UserOptions) :-
		::find_root(Initial, Root, _Statistics, UserOptions).

	evaluate_function(Argument, Value) :-
		evaluate(Argument, Value),
		(	number(Value) ->
			true
		;	domain_error(function_value, Value)
		).

	check_bracket(Bracket, _, _) :-
		var(Bracket),
		instantiation_error.
	check_bracket(bracket(Lower, Upper), Lower, Upper) :-
		(	number(Lower), number(Upper) ->
			(	Lower < Upper ->
				true
			;	domain_error(root_bracket, bracket(Lower, Upper))
			)
		;	domain_error(root_bracket, bracket(Lower, Upper))
		),
		!.
	check_bracket(Bracket, _, _) :-
		domain_error(root_bracket, Bracket).

	default_option(tol_x(1.0e-10)).
	default_option(tol_f(1.0e-10)).
	default_option(max_iterations(100)).

	valid_option(tol_x(Tolerance)) :-
		number(Tolerance), Tolerance >= 0.0.
	valid_option(tol_f(Tolerance)) :-
		number(Tolerance), Tolerance >= 0.0.
	valid_option(max_iterations(Iterations)) :-
		integer(Iterations), Iterations > 0.

:- end_category.
