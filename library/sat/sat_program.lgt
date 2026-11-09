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


:- object(sat_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Canonical CNF statement validation, DIMACS source generation, and variable table computation.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(code)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical CNF statements to DIMACS character codes, assigning sequential variable numbers by first occurrence.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(sat_statement, 'Statement')
		]
	]).

	:- public(serialize/4).
	:- mode(serialize(+list, +list, -list(code), -list(compound)), one_or_error).
	:- info(serialize/4, [
		comment is 'Serializes statements with origins and returns ``line(Start, End, Origin)`` source-map entries; the generated header occupies line 1.',
		argnames is ['Statements', 'Origins', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(sat_statement, 'Statement'),
			'``Origins`` is a partial list or a list with an element ``Origin`` which is a variable' - instantiation_error,
			'``Origins`` is neither a variable nor a list' - type_error(list, 'Origins'),
			'``Origins`` has an element that is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(sat_origins, 'Origins')
		]
	]).

	:- public(variables/2).
	:- mode(variables(+list, -list(pair)), one_or_error).
	:- info(variables/2, [
		comment is 'Returns the ordered ``Term-Number`` variable table that ``serialize/2`` and ``serialize/4`` assign by first occurrence across clause statements.',
		argnames is ['Statements', 'Variables'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(sat_statement, 'Statement')
		]
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	serialize(Statements, Codes) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		valid_statements(Statements),
		collect_variables(Statements, Variables),
		length(Variables, VariableCount),
		count_clauses(Statements, ClauseCount),
		phrase(header(VariableCount, ClauseCount), HeaderCodes),
		serialize_statements(Statements, Variables, StatementCodes, []),
		append(HeaderCodes, StatementCodes, Codes).

	serialize(Statements, Origins, Codes, SourceMap) :-
		context(Context),
		check(ground, Statements-Origins, Context),
		check(list, Statements, Context),
		check(list, Origins, Context),
		valid_statements(Statements),
		(	valid_origins(Statements, Origins) ->
			collect_variables(Statements, Variables),
			length(Variables, VariableCount),
			count_clauses(Statements, ClauseCount),
			phrase(header(VariableCount, ClauseCount), HeaderCodes),
			serialize_located(Statements, Origins, Variables, 2, StatementCodes, [], SourceMap),
			append(HeaderCodes, StatementCodes, Codes)
		;	domain_error(sat_origins, Origins)
		).

	variables(Statements, Variables) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		valid_statements(Statements),
		collect_variables(Statements, Variables).

	valid_statements([]).
	valid_statements([Statement| Statements]) :-
		(	valid_statement(Statement) ->
			true
		;	domain_error(sat_statement, Statement)
		),
		valid_statements(Statements).

	valid_statement(clause(Literals)) :-
		valid(list, Literals),
		valid_literals(Literals).
	valid_statement(comment(Codes)) :-
		valid(codes, Codes),
		\+ member(10, Codes).
	valid_statement(raw(Codes)) :-
		valid(codes, Codes).

	valid_literals([]).
	valid_literals([Literal| Literals]) :-
		valid_literal(Literal),
		valid_literals(Literals).

	valid_literal(pos(_)).
	valid_literal(neg(_)).

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

	collect_variables(Statements, Variables) :-
		collect_variables(Statements, [], Variables0),
		number_variables(Variables0, 1, Variables).

	collect_variables([], Terms, Terms).
	collect_variables([clause(Literals)| Statements], Terms0, Terms) :-
		!,
		literal_terms(Literals, Terms0, Terms1),
		collect_variables(Statements, Terms1, Terms).
	collect_variables([_| Statements], Terms0, Terms) :-
		collect_variables(Statements, Terms0, Terms).

	literal_terms([], Terms, Terms).
	literal_terms([Literal| Literals], Terms0, Terms) :-
		literal_term(Literal, Term),
		(	member(Term, Terms0) ->
			Terms1 = Terms0
		;	append(Terms0, [Term], Terms1)
		),
		literal_terms(Literals, Terms1, Terms).

	literal_term(pos(Term), Term) :- !.
	literal_term(neg(Term), Term) :- !.

	number_variables([], _, []).
	number_variables([Term| Terms], Number, [Term-Number| Variables]) :-
		Next is Number + 1,
		number_variables(Terms, Next, Variables).

	count_clauses([], 0).
	count_clauses([clause(_)| Statements], Count) :-
		!,
		count_clauses(Statements, Rest),
		Count is Rest + 1.
	count_clauses([_| Statements], Count) :-
		count_clauses(Statements, Count).

	serialize_statements([], _, Codes, Codes).
	serialize_statements([Statement| Statements], Variables, Codes, Tail) :-
		phrase(statement(Statement, Variables), Codes, Next),
		serialize_statements(Statements, Variables, Next, Tail).

	serialize_located([], [], _, _, Codes, Codes, []).
	serialize_located([Statement| Statements], [Origin| Origins], Variables, Start, Codes, Tail, [line(Start, End, Origin)| Map]) :-
		phrase(statement(Statement, Variables), StatementCodes, []),
		count_lines(StatementCodes, Start, Next),
		End is Next - 1,
		append(StatementCodes, Rest, Codes),
		serialize_located(Statements, Origins, Variables, Next, Rest, Tail, Map).

	count_lines([], Line, Line).
	count_lines([Code| Codes], Line0, Line) :-
		(	Code =:= 10 ->
			Line1 is Line0 + 1
		;	Line1 = Line0
		),
		count_lines(Codes, Line1, Line).

	header(VariableCount, ClauseCount) -->
		text('p cnf '), integer_text(VariableCount), [0' ], integer_text(ClauseCount), [10].

	statement(clause([]), _) -->
		!,
		text('0'), [10].
	statement(clause(Literals), Variables) -->
		literals(Literals, Variables), text(' 0'), [10].
	statement(comment(Codes), _) -->
		text('c '), codes(Codes), [10].
	statement(raw(Codes), _) -->
		codes(Codes), [10].

	literals([], _) -->
		[].
	literals([Literal| Literals], Variables) -->
		literal(Literal, Variables), literal_tail(Literals, Variables).

	literal_tail([], _) -->
		[].
	literal_tail([Literal| Literals], Variables) -->
		[0' ], literal(Literal, Variables), literal_tail(Literals, Variables).

	literal(pos(Term), Variables) -->
		!,
		{ memberchk(Term-Number, Variables) },
		integer_text(Number).
	literal(neg(Term), Variables) -->
		{ memberchk(Term-Number, Variables) },
		[0'-], integer_text(Number).

	integer_text(Integer) -->
		{ integer(Integer), Integer >= 0, number_codes(Integer, Codes) },
		codes(Codes).

	text(Atom) -->
		{ atom_codes(Atom, Codes) }, codes(Codes).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

:- end_object.
