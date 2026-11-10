%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(souffle_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Canonical Datalog statement validation and source generation for Souffle 2.5.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(integer)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical Datalog statements to character codes.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(souffle_statement, 'Statement')
		]
	]).

	:- public(serialize/4).
	:- mode(serialize(+list, +list, -list(code), -list(compound)), one_or_error).
	:- info(serialize/4, [
		comment is 'Serializes statements with origins and returns ``line(Start, End, Origin)`` source-map entries.',
		argnames is ['Statements', 'Origins', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(souffle_statement, 'Statement'),
			'``Origins`` is a partial list or a list with an element ``Origin`` which is a variable' - instantiation_error,
			'``Origins`` is neither a variable nor a list' - type_error(list, 'Origins'),
			'``Origins`` has an element that is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(souffle_origins, 'Origins')
		]
	]).

	:- public(valid_identifier/1).
	:- mode(valid_identifier(@term), zero_or_one).
	:- info(valid_identifier/1, [
		comment is 'True if the argument is a supported Datalog relation, column, or type identifier (a lowercase or underscore led name).',
		argnames is ['Name']
	]).

	:- public(valid_column_type/1).
	:- mode(valid_column_type(@term), zero_or_one).
	:- info(valid_column_type/1, [
		comment is 'True if the argument is one of the four supported primitive column types.',
		argnames is ['Type']
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	valid_identifier(Name) :-
		atom(Name),
		atom_codes(Name, Codes),
		identifier_codes(Codes).

	valid_column_type(number).
	valid_column_type(unsigned).
	valid_column_type(float).
	valid_column_type(symbol).

	serialize(Statements, Codes) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		serialize_statements(Statements, Codes, []).

	serialize(Statements, Origins, Codes, SourceMap) :-
		context(Context),
		check(ground, Statements-Origins, Context),
		check(list, Statements, Context),
		check(list, Origins, Context),
		(	valid_origins(Statements, Origins) ->
			serialize_located(Statements, Origins, 1, Codes, [], SourceMap)
		;	domain_error(souffle_origins, Origins)
		).

	serialize_statements([], Codes, Codes).
	serialize_statements([Statement| Statements], Codes, Tail) :-
		statement_codes(Statement, Codes, Next),
		serialize_statements(Statements, Next, Tail).

	statement_codes(Statement, Codes, Tail) :-
		(	phrase(statement(Statement), Codes, Tail) ->
			true
		;	domain_error(souffle_statement, Statement)
		).

	valid_origins([], []).
	valid_origins([_| Statements], [Origin| Origins]) :-
		(	Origin == unknown ->
			true
		;	Origin = source(File, Start, End),
			atom(File),
			integer(Start),
			integer(End),
			Start > 0,
			End >= Start
		),
		valid_origins(Statements, Origins).

	serialize_located([], [], _, Codes, Codes, []).
	serialize_located([Statement| Statements], [Origin| Origins], Start, Codes, Tail, [line(Start, End, Origin)| Map]) :-
		statement_codes(Statement, StatementCodes, []),
		count_lines(StatementCodes, Start, Next),
		End is Next - 1,
		append(StatementCodes, Rest, Codes),
		serialize_located(Statements, Origins, Next, Rest, Tail, Map).

	count_lines([], Line, Line).
	count_lines([Code| Codes], Line0, Line) :-
		(	Code =:= 10 ->
			Line1 is Line0 + 1
		;	Line1 = Line0
		),
		count_lines(Codes, Line1, Line).

	% -----------------------------------------------------------------------
	% Statements: ``.decl``/``.input``/``.output``/``.printsize`` directives are
	% newline terminated (no trailing period); facts and rules end in ``. \n``.
	% -----------------------------------------------------------------------

	statement(decl(Name, Columns)) -->
		!, text('.decl '), identifier(Name), [0'(], decl_columns(Columns), [0')], [10].
	statement(input(Name)) -->
		!, text('.input '), identifier(Name), [10].
	statement(output(Name)) -->
		!, text('.output '), identifier(Name), [10].
	statement(printsize(Name)) -->
		!, text('.printsize '), identifier(Name), [10].
	statement(fact(Atom)) -->
		!, souffle_atom(Atom), [0'., 10].
	statement(rule(Head, Body)) -->
		!, souffle_atom(Head), rule_body(Body), [0'., 10].
	statement(comment(Codes)) -->
		!, { valid(codes, Codes) }, text('// '), codes(Codes), [10].
	statement(raw(Codes)) -->
		!, { valid(codes, Codes) }, codes(Codes), [10].

	decl_columns([Name-Type]) -->
		!, identifier(Name), [0':], column_type(Type).
	decl_columns([Name-Type| Columns]) -->
		identifier(Name), [0':], column_type(Type), text(', '), decl_columns(Columns).

	column_type(Type) -->
		{ valid_column_type(Type) }, text(Type).

	rule_body([]) -->
		!, [].
	rule_body(Body) -->
		text(' :- '), body(Body).

	body([]) -->
		[].
	body([Literal| Literals]) -->
		literal(Literal), body_tail(Literals).

	body_tail([]) -->
		[].
	body_tail([Literal| Literals]) -->
		text(', '), literal(Literal), body_tail(Literals).

	literal(not(Atom)) -->
		!, [0'!], souffle_atom(Atom).
	literal(compare(Operator, Left, Right)) -->
		!,
		operand(Left), [0' ], comparison(Operator), [0' ], operand(Right).
	literal(Atom) -->
		souffle_atom(Atom).

	comparison(Operator) -->
		{ comparison_operator(Operator) }, text(Operator).

	comparison_operator('=').
	comparison_operator('!=').
	comparison_operator('<').
	comparison_operator('<=').
	comparison_operator('>').
	comparison_operator('>=').

	% Aggregates are only valid as a ``compare/3`` operand (the Souffle grammar
	% only allows them in a ``Var = aggregate-expr`` binding), never as a plain
	% function argument.
	operand(aggregate(Function, TargetExpr, Body)) -->
		!,
		aggregate_name(Function), aggregate_target(TargetExpr), text(' : { '), aggregate_body(Body), text(' }').
	operand(Term) -->
		term(Term).

	aggregate_name(Function) -->
		{ aggregate_function(Function) }, text(Function).

	aggregate_function(count).
	aggregate_function(sum).
	aggregate_function(max).
	aggregate_function(min).
	aggregate_function(mean).

	aggregate_target(none) -->
		!, [].
	aggregate_target(Term) -->
		[0' ], term(Term).

	aggregate_body([Literal]) -->
		!, literal(Literal).
	aggregate_body([Literal| Literals]) -->
		literal(Literal), text(', '), aggregate_body(Literals).

	souffle_atom(function(Name, Arguments)) -->
		identifier(Name), arguments(Arguments).

	arguments([]) -->
		!, [].
	arguments([Term| Terms]) -->
		[0'(], term(Term), term_tail(Terms), [0')].

	term_tail([]) -->
		[].
	term_tail([Term| Terms]) -->
		[0',], term(Term), term_tail(Terms).

	term(number(Integer)) -->
		!, { integer(Integer) }, integer_text(Integer).
	term(unsigned(Integer)) -->
		!, { integer(Integer), Integer >= 0 }, integer_text(Integer), [0'u].
	term(float(Float)) -->
		!, { float(Float) }, float_text(Float).
	term(string(Codes)) -->
		!, { valid(codes, Codes) }, [0'"], string_codes(Codes), [0'"].
	term(variable(Name)) -->
		!, variable_name(Name).
	term(anonymous) -->
		!, [0'_].
	term(cat(Left, Right)) -->
		!, text('cat('), term(Left), [0',], term(Right), [0')].
	term(arithmetic(Operator, [Left, Right])) -->
		{ arithmetic_operator(Operator) },
		!, [0'(], term(Left), text(Operator), term(Right), [0')].

	arithmetic_operator('+').
	arithmetic_operator('-').
	arithmetic_operator('*').
	arithmetic_operator('/').
	arithmetic_operator('%').

	integer_text(Integer) -->
		{ integer(Integer), number_codes(Integer, Codes) },
		codes(Codes).

	float_text(Float) -->
		{ float(Float), number_codes(Float, Codes) },
		codes(Codes).

	identifier(Name) -->
		{ valid_identifier(Name), atom_codes(Name, Codes) },
		codes(Codes).

	variable_name(Name) -->
		{ atom(Name), atom_codes(Name, [First| Rest]), variable_start(First), name_tail(Rest), Name \== '_' },
		[First], codes(Rest).

	text(Atom) -->
		{ atom_codes(Atom, Codes) }, codes(Codes).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

	string_codes([]) -->
		[].
	string_codes([Code| Codes]) -->
		escaped_code(Code), string_codes(Codes).

	escaped_code(10) -->
		!, [0'\\, 0'n].
	escaped_code(13) -->
		!, [0'\\, 0'r].
	escaped_code(9) -->
		!, [0'\\, 0't].
	escaped_code(0'") -->
		!, [0'\\, 0'"].
	escaped_code(0'\\) -->
		!, [0'\\, 0'\\].
	escaped_code(Code) -->
		{ Code >= 32, Code =\= 127 }, [Code].

	identifier_codes([0'_| Codes]) :-
		!,
		identifier_codes(Codes).
	identifier_codes([First| Rest]) :-
		First >= 0'a,
		First =< 0'z,
		name_tail(Rest).

	variable_start(Code) :-
		(	Code =:= 0'_ ->
			true
		;	Code >= 0'A,
			Code =< 0'Z
		).

	name_tail([]).
	name_tail([Code| Codes]) :-
		(	Code >= 0'a, Code =< 0'z ->
			true
		;	Code >= 0'A, Code =< 0'Z ->
			true
		;	Code >= 0'0, Code =< 0'9 ->
			true
		;	Code =:= 0'_
		),
		name_tail(Codes).

:- end_object.
