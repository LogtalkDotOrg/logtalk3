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


:- category(linear_programming_types).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-03,
		comment is 'Type definitions for linear-programming model inputs.'
	]).

	:- multifile(type::type/1).
	type::type(linear_expression).
	type::type(linear_programming_bound).
	type::type(linear_programming_constraint_sense).
	type::type(linear_programming_objective_sense).
	type::type(linear_programming_variable_type).

	:- multifile(type::check/2).
	type::check(linear_expression, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	valid_linear_expression(Term) ->
			true
		;	throw(type_error(linear_expression, Term))
		).

	type::check(linear_programming_bound, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	valid_bound(Term) ->
			true
		;	throw(type_error(linear_programming_bound, Term))
		).

	type::check(linear_programming_constraint_sense, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	valid_constraint_sense(Term) ->
			true
		;	throw(domain_error(linear_programming_constraint_sense, Term))
		).

	type::check(linear_programming_objective_sense, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	valid_objective_sense(Term) ->
			true
		;	throw(domain_error(linear_programming_objective_sense, Term))
		).

	type::check(linear_programming_variable_type, Term) :-
		(	var(Term) ->
			throw(instantiation_error)
		;	valid_variable_type(Term) ->
			true
		;	throw(domain_error(linear_programming_variable_type, Term))
		).

	valid_linear_expression([]).
	valid_linear_expression([Coefficient*Variable| Terms]) :-
		number(Coefficient),
		ground(Variable),
		valid_linear_expression(Terms).

	valid_bound(Bound) :-
		number(Bound).
	valid_bound(Bound) :-
		Bound == inf.
	valid_bound(Bound) :-
		Bound == -inf.

	valid_constraint_sense(=<).
	valid_constraint_sense(>=).
	valid_constraint_sense(=).

	valid_objective_sense(minimize).
	valid_objective_sense(maximize).

	valid_variable_type(continuous).
	valid_variable_type(integer).
	valid_variable_type(binary).

:- end_category.
