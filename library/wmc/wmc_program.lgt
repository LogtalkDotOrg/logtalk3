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


:- object(wmc_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-25,
		comment is 'Canonical weighted model counting statement validation, DIMACS CNF source generation with weights using the Model Counting Competition format, and variable table computation.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(code)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical statements to CNF character codes (``p cnf`` header, ``c p weight`` lines when the problem is weighted, and clauses), assigning sequential variable numbers by first occurrence across clauses and weights. In a weighted problem, both literals of every variable get an explicit weight (1 when unspecified).',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(wmc_statement, 'Statement'),
			'The weight of a literal is given more than once' - domain_error(wmc_duplicate_weight, 'Literal')
		]
	]).

	:- public(serialize/4).
	:- mode(serialize(+list, +list, -list(code), -list(compound)), one_or_error).
	:- info(serialize/4, [
		comment is 'Serializes statements with origins and returns ``line(Start, End, Origin)`` source-map entries for the statements that generate lines (weight and probability statements do not).',
		argnames is ['Statements', 'Origins', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(wmc_statement, 'Statement'),
			'The weight of a literal is given more than once' - domain_error(wmc_duplicate_weight, 'Literal'),
			'``Origins`` has an element that is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(wmc_origins, 'Origins')
		]
	]).

	:- public(variables/2).
	:- mode(variables(+list, -list(pair)), one_or_error).
	:- info(variables/2, [
		comment is 'Returns the ordered ``Term-Number`` variable table that the serialization predicates assign by first occurrence across clauses and weights.',
		argnames is ['Statements', 'Variables']
	]).

	:- public(weighted/1).
	:- mode(weighted(+list), zero_or_one).
	:- info(weighted/1, [
		comment is 'True when the statements include a ``weight/2`` or ``probability/2`` statement.',
		argnames is ['Statements']
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
		prepare(Statements, Variables, Weights, ClauseCount),
		phrase(header(Variables, ClauseCount), HeaderCodes),
		weight_lines(Weights, Variables, WeightCodes),
		serialize_statements(Statements, Variables, StatementCodes, []),
		append(HeaderCodes, WeightCodes, PrefixCodes),
		append(PrefixCodes, StatementCodes, Codes).

	serialize(Statements, Origins, Codes, SourceMap) :-
		context(Context),
		check(ground, Statements-Origins, Context),
		check(list, Statements, Context),
		check(list, Origins, Context),
		valid_statements(Statements),
		(	valid_origins(Statements, Origins) ->
			prepare(Statements, Variables, Weights, ClauseCount),
			phrase(header(Variables, ClauseCount), HeaderCodes),
			weight_lines(Weights, Variables, WeightCodes),
			count_lines(HeaderCodes, 1, Line1),
			count_lines(WeightCodes, Line1, Start),
			serialize_located(Statements, Origins, Variables, Start, StatementCodes, [], SourceMap),
			append(HeaderCodes, WeightCodes, PrefixCodes),
			append(PrefixCodes, StatementCodes, Codes)
		;	domain_error(wmc_origins, Origins)
		).

	variables(Statements, Variables) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		valid_statements(Statements),
		collect_variables(Statements, Variables).

	weighted(Statements) :-
		member(Statement, Statements),
		weight_statement(Statement),
		!.

	weight_statement(weight(_, _)).
	weight_statement(probability(_, _)).

	prepare(Statements, Variables, Weights, ClauseCount) :-
		collect_variables(Statements, Variables),
		collect_weights(Statements, Weights),
		count_clauses(Statements, 0, ClauseCount).

	valid_statements([]).
	valid_statements([Statement| Statements]) :-
		(	valid_statement(Statement) ->
			true
		;	domain_error(wmc_statement, Statement)
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
	valid_statement(weight(Literal, Weight)) :-
		!,
		valid_literal(Literal),
		valid_weight(Weight).
	valid_statement(probability(Term, Probability)) :-
		!,
		callable(Term),
		number(Probability),
		Probability >= 0,
		Probability =< 1.
	valid_statement(Disjunction) :-
		valid_disjunction(Disjunction).

	valid_weight(Weight) :-
		number(Weight),
		Weight >= 0.

	valid_disjunction((A ; B)) :-
		!,
		valid_disjunction(A),
		valid_disjunction(B).
	valid_disjunction(Literal) :-
		valid_literal(Literal).

	valid_literal(+Term) :-
		!,
		callable(Term).
	valid_literal(-Term) :-
		!,
		callable(Term).
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

	% variables

	collect_variables(Statements, Variables) :-
		collect_variables(Statements, [], Terms),
		number_variables(Terms, 1, Variables).

	collect_variables([], Terms, Terms).
	collect_variables([Statement| Statements], Terms0, Terms) :-
		statement_literals(Statement, Literals),
		literal_terms(Literals, Terms0, Terms1),
		collect_variables(Statements, Terms1, Terms).

	statement_literals(false, []) :-
		!.
	statement_literals(comment(_), []) :-
		!.
	statement_literals(raw(_), []) :-
		!.
	statement_literals(weight(Literal, _), [Literal]) :-
		!.
	statement_literals(probability(Term, _), [+Term]) :-
		!.
	statement_literals(Disjunction, Literals) :-
		disjunction_literals(Disjunction, Literals).

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

	literal_term(+Term, Term) :- !.
	literal_term(-Term, Term) :- !.
	literal_term(Term, Term).

	number_variables([], _, []).
	number_variables([Term| Terms], Number, [Term-Number| Variables]) :-
		Next is Number + 1,
		number_variables(Terms, Next, Variables).

	% weights: list of Term-weights(Positive, Negative) pairs for the weighted terms
	% (the weight of a literal can be given at most once)

	collect_weights(Statements, Weights) :-
		collect_weights(Statements, [], Weights).

	collect_weights([], Weights, Weights).
	collect_weights([weight(Literal, Weight)| Statements], Weights0, Weights) :-
		!,
		literal_sign(Literal, Term, Sign),
		set_weight(Weights0, Term, Sign, Weight, Literal, Weights1),
		collect_weights(Statements, Weights1, Weights).
	collect_weights([probability(Term, P)| Statements], Weights0, Weights) :-
		!,
		Q is 1 - P,
		set_weight(Weights0, Term, positive, P, +Term, Weights1),
		set_weight(Weights1, Term, negative, Q, -Term, Weights2),
		collect_weights(Statements, Weights2, Weights).
	collect_weights([_| Statements], Weights0, Weights) :-
		collect_weights(Statements, Weights0, Weights).

	literal_sign(+Term, Term, positive) :- !.
	literal_sign(-Term, Term, negative) :- !.
	literal_sign(Term, Term, positive).

	set_weight([], Term, Sign, Weight, _, [Term-Entry]) :-
		new_entry(Sign, Weight, Entry).
	set_weight([Term0-Entry0| Weights0], Term, Sign, Weight, Literal, Weights) :-
		(	Term0 == Term ->
			update_entry(Sign, Weight, Entry0, Literal, Entry),
			Weights = [Term0-Entry| Weights0]
		;	Weights = [Term0-Entry0| Weights1],
			set_weight(Weights0, Term, Sign, Weight, Literal, Weights1)
		).

	new_entry(positive, Weight, weights(Weight, none)).
	new_entry(negative, Weight, weights(none, Weight)).

	update_entry(positive, Weight, weights(Old, Negative), Literal, weights(Weight, Negative)) :-
		(	Old == none ->
			true
		;	domain_error(wmc_duplicate_weight, Literal)
		).
	update_entry(negative, Weight, weights(Positive, Old), Literal, weights(Positive, Weight)) :-
		(	Old == none ->
			true
		;	domain_error(wmc_duplicate_weight, Literal)
		).

	count_clauses([], Count, Count).
	count_clauses([Statement| Statements], Count0, Count) :-
		(	clause_statement(Statement) ->
			Count1 is Count0 + 1
		;	Count1 is Count0
		),
		count_clauses(Statements, Count1, Count).

	clause_statement(Statement) :-
		Statement \= comment(_),
		Statement \= raw(_),
		Statement \= weight(_, _),
		Statement \= probability(_, _).

	% serialization

	serialize_statements([], _, Codes, Codes).
	serialize_statements([Statement| Statements], Variables, Codes, Tail) :-
		phrase(statement(Statement, Variables), Codes, Next),
		serialize_statements(Statements, Variables, Next, Tail).

	serialize_located([], [], _, _, Codes, Codes, []).
	serialize_located([Statement| Statements], [Origin| Origins], Variables, Start, Codes, Tail, Map) :-
		phrase(statement(Statement, Variables), StatementCodes, []),
		append(StatementCodes, Rest, Codes),
		(	StatementCodes == [] ->
			Next = Start,
			Map = Map1
		;	count_lines(StatementCodes, Start, Next),
			End is Next - 1,
			Map = [line(Start, End, Origin)| Map1]
		),
		serialize_located(Statements, Origins, Variables, Next, Rest, Tail, Map1).

	count_lines([], Line, Line).
	count_lines([Code| Codes], Line0, Line) :-
		(	Code =:= 10 ->
			Line1 is Line0 + 1
		;	Line1 = Line0
		),
		count_lines(Codes, Line1, Line).

	header(Variables, ClauseCount) -->
		{ length(Variables, VariableCount) },
		text('p cnf '), integer_text(VariableCount), [0' ], integer_text(ClauseCount), [10].

	% every variable of a weighted problem gets both of its weights written explicitly
	weight_lines([], _, []) :-
		!.
	weight_lines(Weights, Variables, Codes) :-
		phrase(weight_variables(Variables, Weights), Codes).

	weight_variables([], _) -->
		[].
	weight_variables([Term-Number| Variables], Weights) -->
		{ weight_pair(Weights, Term, Positive, Negative) },
		weight_line(Number, Positive),
		{ Negated is -Number },
		weight_line(Negated, Negative),
		weight_variables(Variables, Weights).

	weight_pair(Weights, Term, Positive, Negative) :-
		(	member(Term0-weights(P, N), Weights),
			Term0 == Term ->
			default_weight(P, Positive),
			default_weight(N, Negative)
		;	Positive = 1,
			Negative = 1
		).

	default_weight(none, 1) :-
		!.
	default_weight(Weight, Weight).

	weight_line(Literal, Weight) -->
		text('c p weight '), signed_text(Literal), [0' ], number_text(Weight), text(' 0'), [10].

	statement(comment(Source), _) -->
		!,
		{ raw_codes(Source, Codes) },
		text('c '), codes(Codes), [10].
	statement(raw(Source), _) -->
		!,
		{ raw_codes(Source, Codes) },
		codes(Codes), [10].
	statement(weight(_, _), _) -->
		!,
		[].
	statement(probability(_, _), _) -->
		!,
		[].
	statement(false, _) -->
		!,
		text('0'), [10].
	statement(Disjunction, Variables) -->
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

	signed_text(Integer) -->
		{ Integer < 0, Magnitude is -Integer },
		!,
		[0'-], integer_text(Magnitude).
	signed_text(Integer) -->
		integer_text(Integer).

	integer_text(Integer) -->
		{ integer(Integer), Integer >= 0, number_codes(Integer, Codes) },
		codes(Codes).

	number_text(Number) -->
		{ number_codes(Number, Codes) },
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
