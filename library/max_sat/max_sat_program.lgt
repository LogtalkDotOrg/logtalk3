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


:- object(max_sat_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-23,
		comment is 'Canonical weighted partial MaxSAT statement validation, WCNF source generation, and variable table computation.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(code)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical statements to WCNF character codes using the classic ``p wcnf`` format, assigning sequential variable numbers by first occurrence.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(max_sat_statement, 'Statement')
		]
	]).

	:- public(serialize/3).
	:- mode(serialize(+list, +atom, -list(code)), one_or_error).
	:- info(serialize/3, [
		comment is 'Serializes statements using the given WCNF format: ``old`` (``p wcnf`` header with a top weight, hard clauses carrying the top weight) or ``new`` (the format of the MaxSAT Evaluations from 2022, without header, with ``h`` for hard clauses).',
		argnames is ['Statements', 'Format', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(max_sat_statement, 'Statement'),
			'``Format`` is neither ``old`` nor ``new``' - domain_error(wcnf_format, 'Format')
		]
	]).

	:- public(serialize/5).
	:- mode(serialize(+list, +list, +atom, -list(code), -list(compound)), one_or_error).
	:- info(serialize/5, [
		comment is 'Serializes statements with origins and returns ``line(Start, End, Origin)`` source-map entries; with the ``old`` format the generated header occupies line 1.',
		argnames is ['Statements', 'Origins', 'Format', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(max_sat_statement, 'Statement'),
			'``Origins`` has an element that is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(max_sat_origins, 'Origins'),
			'``Format`` is neither ``old`` nor ``new``' - domain_error(wcnf_format, 'Format')
		]
	]).

	:- public(variables/2).
	:- mode(variables(+list, -list(pair)), one_or_error).
	:- info(variables/2, [
		comment is 'Returns the ordered ``Term-Number`` variable table that the serialization predicates assign by first occurrence across hard and soft clauses.',
		argnames is ['Statements', 'Variables'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(max_sat_statement, 'Statement')
		]
	]).

	:- public(top_weight/2).
	:- mode(top_weight(+list, -positive_integer), one_or_error).
	:- info(top_weight/2, [
		comment is 'Returns the top weight used by the ``old`` format: one plus the sum of the weights of the soft clauses.',
		argnames is ['Statements', 'Top']
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		append/3, length/2, member/2, memberchk/2
	]).

	serialize(Statements, Codes) :-
		serialize(Statements, old, Codes).

	serialize(Statements, Format, Codes) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		valid_format(Format),
		valid_statements(Statements),
		collect_variables(Statements, Variables),
		length(Variables, VariableCount),
		count_clauses(Statements, 0, ClauseCount),
		top_weight(Statements, Top),
		(	Format == old ->
			phrase(header(VariableCount, ClauseCount, Top), HeaderCodes)
		;	HeaderCodes = []
		),
		serialize_statements(Statements, Format, Top, Variables, StatementCodes, []),
		append(HeaderCodes, StatementCodes, Codes).

	serialize(Statements, Origins, Format, Codes, SourceMap) :-
		context(Context),
		check(ground, Statements-Origins, Context),
		check(list, Statements, Context),
		check(list, Origins, Context),
		valid_format(Format),
		valid_statements(Statements),
		(	valid_origins(Statements, Origins) ->
			collect_variables(Statements, Variables),
			length(Variables, VariableCount),
			count_clauses(Statements, 0, ClauseCount),
			top_weight(Statements, Top),
			(	Format == old ->
				phrase(header(VariableCount, ClauseCount, Top), HeaderCodes),
				Start = 2
			;	HeaderCodes = [],
				Start = 1
			),
			serialize_located(Statements, Origins, Format, Top, Variables, Start, StatementCodes, [], SourceMap),
			append(HeaderCodes, StatementCodes, Codes)
		;	domain_error(max_sat_origins, Origins)
		).

	variables(Statements, Variables) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		valid_statements(Statements),
		collect_variables(Statements, Variables).

	top_weight(Statements, Top) :-
		sum_weights(Statements, 0, Sum),
		Top is Sum + 1.

	sum_weights([], Sum, Sum).
	sum_weights([Statement| Statements], Sum0, Sum) :-
		(	soft_parts(Statement, Weight, _) ->
			Sum1 is Sum0 + Weight
		;	Sum1 = Sum0
		),
		sum_weights(Statements, Sum1, Sum).

	valid_format(Format) :-
		(	Format == old ->
			true
		;	Format == new ->
			true
		;	domain_error(wcnf_format, Format)
		).

	% soft clauses must be recognized before generic disjunctions
	soft_parts(soft(Weight, Clause), Weight, Clause) :-
		!.
	soft_parts(soft(Clause), 1, Clause).

	valid_statements([]).
	valid_statements([Statement| Statements]) :-
		(	valid_statement(Statement) ->
			true
		;	domain_error(max_sat_statement, Statement)
		),
		valid_statements(Statements).

	valid_statement(false) :-
		!.
	valid_statement(comment(Source)) :-
		!,
		raw_codes(Source, Codes),
		\+ member(10, Codes).
	valid_statement(raw(Source)) :-
		!,
		raw_codes(Source, _).
	valid_statement(Statement) :-
		soft_parts(Statement, Weight, Clause),
		!,
		integer(Weight),
		Weight >= 1,
		valid_clause(Clause).
	valid_statement(Disjunction) :-
		valid_disjunction(Disjunction).

	valid_clause(false) :-
		!.
	valid_clause(Disjunction) :-
		valid_disjunction(Disjunction).

	valid_disjunction((A ; B)) :-
		!,
		valid_disjunction(A),
		valid_disjunction(B).
	valid_disjunction(Literal) :-
		valid_literal(Literal).

	valid_literal(+_) :-
		!.
	valid_literal(-_) :-
		!.
	valid_literal(Term) :-
		callable(Term).

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
	collect_variables([Statement| Statements], Terms0, Terms) :-
		statement_clause(Statement, Clause),
		(	Clause == none ->
			Terms1 = Terms0
		;	Clause == false ->
			Terms1 = Terms0
		;	disjunction_literals(Clause, Literals),
			literal_terms(Literals, Terms0, Terms1)
		),
		collect_variables(Statements, Terms1, Terms).

	% the clause of a statement, or none for comments and raw source
	statement_clause(false, false) :-
		!.
	statement_clause(comment(_), none) :-
		!.
	statement_clause(raw(_), none) :-
		!.
	statement_clause(Statement, Clause) :-
		soft_parts(Statement, _, Clause),
		!.
	statement_clause(Clause, Clause).

	disjunction_literals((A ; B), Literals) :-
		!,
		disjunction_literals(A, LiteralsA),
		disjunction_literals(B, LiteralsB),
		append(LiteralsA, LiteralsB, Literals).
	disjunction_literals(Literal, [Literal]).

	literal_terms([], Terms, Terms).
	literal_terms([Literal| Literals], Terms0, Terms) :-
		literal_term(Literal, Term),
		(	member(Term, Terms0) ->
			Terms1 = Terms0
		;	append(Terms0, [Term], Terms1)
		),
		literal_terms(Literals, Terms1, Terms).

	literal_term(+Term, Term) :-
		!.
	literal_term(-Term, Term) :-
		!.
	literal_term(Term, Term).

	number_variables([], _, []).
	number_variables([Term| Terms], Number, [Term-Number| Variables]) :-
		Next is Number + 1,
		number_variables(Terms, Next, Variables).

	count_clauses([], Count, Count).
	count_clauses([Statement| Statements], Count0, Count) :-
		(	statement_clause(Statement, none) ->
			Count1 is Count0
		;	Count1 is Count0 + 1
		),
		count_clauses(Statements, Count1, Count).

	serialize_statements([], _, _, _, Codes, Codes).
	serialize_statements([Statement| Statements], Format, Top, Variables, Codes, Tail) :-
		phrase(statement(Statement, Format, Top, Variables), Codes, Next),
		serialize_statements(Statements, Format, Top, Variables, Next, Tail).

	serialize_located([], [], _, _, _, _, Codes, Codes, []).
	serialize_located([Statement| Statements], [Origin| Origins], Format, Top, Variables, Start, Codes, Tail, [line(Start, End, Origin)| Map]) :-
		phrase(statement(Statement, Format, Top, Variables), StatementCodes, []),
		count_lines(StatementCodes, Start, Next),
		End is Next - 1,
		append(StatementCodes, Rest, Codes),
		serialize_located(Statements, Origins, Format, Top, Variables, Next, Rest, Tail, Map).

	count_lines([], Line, Line).
	count_lines([Code| Codes], Line0, Line) :-
		(	Code =:= 10 ->
			Line1 is Line0 + 1
		;	Line1 = Line0
		),
		count_lines(Codes, Line1, Line).

	header(VariableCount, ClauseCount, Top) -->
		text('p wcnf '), integer_text(VariableCount), [0' ], integer_text(ClauseCount), [0' ], integer_text(Top), [10].

	statement(comment(Source), _, _, _) -->
		!,
		{ raw_codes(Source, Codes) },
		text('c '), codes(Codes), [10].
	statement(raw(Source), _, _, _) -->
		!,
		{ raw_codes(Source, Codes) },
		codes(Codes), [10].
	statement(false, Format, Top, Variables) -->
		!,
		hard_prefix(Format, Top), clause(false, Variables).
	statement(Statement, _, _, Variables) -->
		{ soft_parts(Statement, Weight, Clause) },
		!,
		integer_text(Weight), [0' ], clause(Clause, Variables).
	statement(Disjunction, Format, Top, Variables) -->
		hard_prefix(Format, Top), clause(Disjunction, Variables).

	hard_prefix(old, Top) -->
		integer_text(Top), [0' ].
	hard_prefix(new, _) -->
		text('h ').

	clause(false, _) -->
		!,
		text('0'), [10].
	clause(Disjunction, Variables) -->
		{ disjunction_literals(Disjunction, Literals) },
		literals(Literals, Variables), text(' 0'), [10].

	literals([], _) -->
		[].
	literals([Literal| Literals], Variables) -->
		literal(Literal, Variables), literal_tail(Literals, Variables).

	literal_tail([], _) -->
		[].
	literal_tail([Literal| Literals], Variables) -->
		[0' ], literal(Literal, Variables), literal_tail(Literals, Variables).

	literal(-Term, Variables) -->
		!,
		{ memberchk(Term-Number, Variables) },
		[0'-], integer_text(Number).
	literal(Literal, Variables) -->
		{ literal_term(Literal, Term), memberchk(Term-Number, Variables) },
		integer_text(Number).

	integer_text(Integer) -->
		{ integer(Integer), Integer >= 0, number_codes(Integer, Codes) },
		codes(Codes).

	text(Atom) -->
		{ atom_codes(Atom, Codes) }, codes(Codes).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

	% accepts an atom, a list of character codes, a list of one-character atoms,
	% or a backend-native string, normalizing any of them to character codes
	raw_codes(Source, Codes) :-
		(	atom(Source) ->
			atom_codes(Source, Codes)
		;	valid(chars, Source) ->
			atom_chars(Atom, Source),
			atom_codes(Atom, Codes)
		;	{'$lgt_string'(Source)} ->
			{'$lgt_string_codes'(Source, Codes)}
		;	valid(codes, Source),
			Codes = Source
		).

:- end_object.
