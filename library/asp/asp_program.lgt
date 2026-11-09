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


:- object(asp_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Canonical ASP statement validation and source generation.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(integer)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical ASP statements to character codes.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(asp_statement, 'Statement')
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
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(asp_statement, 'Statement'),
			'``Origins`` is a partial list or a list with an element ``Origin`` which is a variable' - instantiation_error,
			'``Origins`` is neither a variable nor a list' - type_error(list, 'Origins'),
			'``Origins`` has an element that is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(asp_origins, 'Origins')
		]
	]).

	:- public(symbol_codes/2).
	:- mode(symbol_codes(+term, -list(integer)), one_or_error).
	:- info(symbol_codes/2, [
		comment is 'Serializes a canonical ground symbol to ASP character codes.',
		argnames is ['Symbol', 'Codes'],
		exceptions is [
			'``Symbol`` is not ground' - instantiation_error,
			'``Symbol`` is ground but not a supported symbol' - domain_error(asp_symbol, 'Symbol')
		]
	]).

	:- public(valid_identifier/1).
	:- mode(valid_identifier(@term), zero_or_one).
	:- info(valid_identifier/1, [
		comment is 'True if the argument is a supported ASP function or predicate identifier.',
		argnames is ['Name']
	]).

	:- uses(list, [
		append/3
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	valid_identifier(Name) :-
		atom(Name),
		Name \== not,
		atom_codes(Name, Codes),
		identifier_codes(Codes).

	symbol_codes(Symbol, Codes) :-
		context(Context),
		check(ground, Symbol, Context),
		( ground_symbol(Symbol),
			phrase(term(Symbol), Codes) ->
			true
		; domain_error(asp_symbol, Symbol)
		).

	ground_symbol(number(Integer)) :-
		!,
		integer(Integer).
	ground_symbol(string(Codes)) :-
		!,
		valid(codes, Codes).
	ground_symbol(function(Name, Arguments, Sign)) :-
		!,
		valid_identifier(Name),
		once((Sign == positive; Sign == negative)),
		valid(list, Arguments),
		ground_symbols(Arguments).
	ground_symbol(tuple(Terms)) :-
		!,
		valid(list, Terms),
		ground_symbols(Terms).
	ground_symbol(infimum) :-
		!.
	ground_symbol(supremum).

	ground_symbols([]).
	ground_symbols([Symbol| Symbols]) :-
		ground_symbol(Symbol),
		ground_symbols(Symbols).

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
		;	domain_error(asp_origins, Origins)
		).

	serialize_statements([], Codes, Codes).
	serialize_statements([Statement| Statements], Codes, Tail) :-
		statement_codes(Statement, Codes, Next),
		serialize_statements(Statements, Next, Tail).

	statement_codes(Statement, Codes, Tail) :-
		(	phrase(statement(Statement), Codes, Tail) ->
			true
		;	domain_error(asp_statement, Statement)
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

	statement(fact(Atom)) -->
		!, asp_atom(Atom), [0'., 10].
	statement(rule(Head, Body)) -->
		!, head(Head), rule_body(Body), [0'., 10].
	statement(constraint(Body)) -->
		!, [0':, 0'-, 0' ], body(Body), [0'., 10].
	statement(constant(Name, Term)) -->
		!,
		{ ground_expression(Term) },
		text('#const '), identifier(Name), text(' = '), term(Term), [0'., 10].
	statement(show(none)) -->
		!, text('#show.'), [10].
	statement(show(signature(Name, Arity, Sign))) -->
		!,
		{ integer(Arity), Arity >= 0 },
		text('#show '), sign(Sign), identifier(Name), [0'/], integer_text(Arity), [0'., 10].
	statement(show_term(Term, Body)) -->
		!,
		text('#show '), term(Term), condition(Body), [0'., 10].
	statement(minimize(Elements)) -->
		!, text('#minimize {'), objectives(Elements), text('}.'), [10].
	statement(maximize(Elements)) -->
		!, text('#maximize {'), objectives(Elements), text('}.'), [10].
	statement(weak_constraint(Body, Weight, Priority, Tuple)) -->
		!,
		text(':~ '), body(Body), text('. ['), objective_tuple(Weight, Priority, Tuple), [0'], 10].
	statement(raw(Codes)) -->
		!,
		{ valid(codes, Codes) },
		codes(Codes), [10].

	head(atom(Atom)) -->
		!, asp_atom(Atom).
	head(disjunction([Atom| Atoms])) -->
		!, asp_atom(Atom), atom_tail(Atoms).
	head(choice(Lower, Elements, Upper)) -->
		!,
		lower_bound(Lower), [0'{], choice_elements(Elements), [0'}], upper_bound(Upper).

	atom_tail([]) -->
		[].
	atom_tail([Atom| Atoms]) -->
		text('; '), asp_atom(Atom), atom_tail(Atoms).

	lower_bound(none) -->
		!, [].
	lower_bound(Term) -->
		term(Term), [0' ].

	upper_bound(none) -->
		!, [].
	upper_bound(Term) -->
		[0' ], term(Term).

	choice_elements([]) -->
		[].
	choice_elements([conditional(Atom, Body)| Elements]) -->
		asp_atom(Atom), condition(Body), choice_tail(Elements).

	choice_tail([]) -->
		[].
	choice_tail([Element| Elements]) -->
		text('; '), choice_elements([Element| Elements]).

	rule_body([]) -->
		!, [].

	rule_body(Body) --> text(' :- '), body(Body).

	condition([]) -->
		!, [].
	condition(Body) -->
		text(' : '), simple_body(Body).

	body([]) -->
		[].
	body([Literal| Literals]) -->
		literal(Literal), body_tail(Literal, Literals).

	body_tail(_, []) -->
		[].
	body_tail(Previous, [Literal| Literals]) -->
		body_separator(Previous), literal(Literal), body_tail(Literal, Literals).

	body_separator(conditional(_, _)) -->
		!, text('; ').
	body_separator(_) -->
		text(', ').

	simple_body([]) -->
		[].
	simple_body([Literal| Literals]) -->
		simple_literal(Literal), simple_body_tail(Literals).

	simple_body_tail([]) -->
		[].
	simple_body_tail([Literal| Literals]) -->
		text(', '), simple_literal(Literal), simple_body_tail(Literals).

	simple_literal(not(Literal)) -->
		!, text('not '), negated_literal(Literal).
	simple_literal(compare(Operator, Left, Right)) -->
		!,
		term(Left), [0' ], comparison(Operator), [0' ], term(Right).
	simple_literal(Atom) -->
		asp_atom(Atom).

	literal(not(Literal)) -->
		!, text('not '), negated_literal(Literal).
	literal(compare(Operator, Left, Right)) -->
		!,
		operand(Left), [0' ], comparison(Operator), [0' ], operand(Right).
	literal(conditional(Atom, Body)) -->
		!, asp_atom(Atom), text(' : '), simple_body(Body).
	literal(aggregate(Function, Elements)) -->
		!, aggregate(Function, Elements).
	literal(Atom) -->
		asp_atom(Atom).

	negated_literal(not(Atom)) -->
		!, text('not '), asp_atom(Atom).
	negated_literal(compare(Operator, Left, Right)) -->
		!,
		operand(Left), [0' ], comparison(Operator), [0' ], operand(Right).
	negated_literal(Atom) -->
		asp_atom(Atom).

	comparison(Operator) -->
		{ comparison_operator(Operator) },
		text(Operator).

	comparison_operator('=').
	comparison_operator('!=').
	comparison_operator('<').
	comparison_operator('<=').
	comparison_operator('>').
	comparison_operator('>=').

	operand(aggregate(Function, Elements)) -->
		!, aggregate(Function, Elements).
	operand(Term) -->
		term(Term).

	aggregate(Function, Elements) -->
		{ aggregate_name(Function, Name) },
		text(Name), [0'{], aggregate_elements(Elements), [0'}].

	aggregate_elements([]) -->
		[].
	aggregate_elements([aggregate_element(Tuple, Body)| Elements]) -->
		aggregate_tuple(Tuple, Body), aggregate_tail(Elements).

	aggregate_tuple([], Body) -->
		!, [0':], simple_body(Body).
	aggregate_tuple([Term| Terms], Body) -->
		term(Term), term_tail(Terms), condition(Body).

	aggregate_tail([]) -->
		[].
	aggregate_tail([Element| Elements]) -->
		text('; '), aggregate_elements([Element| Elements]).

	aggregate_name(count, '#count').
	aggregate_name(sum, '#sum').
	aggregate_name(sum_plus, '#sum+').
	aggregate_name(min, '#min').
	aggregate_name(max, '#max').

	objectives([]) -->
		[].
	objectives([objective(Weight, Priority, Tuple, Body)| Elements]) -->
		objective_tuple(Weight, Priority, Tuple), condition(Body), objective_tail(Elements).

	objective_tail([]) -->
		[].
	objective_tail([Element| Elements]) -->
		text('; '), objectives([Element| Elements]).

	objective_tuple(Weight, Priority, Tuple) -->
		term(Weight), [0'@], term(Priority), tuple_tail(Tuple).

	tuple_tail([]) -->
		[].
	tuple_tail([Term| Terms]) -->
		[0',], term(Term), tuple_tail(Terms).

	asp_atom(function(Name, Arguments, Sign)) -->
		sign(Sign), identifier(Name), arguments(Arguments).

	sign(positive) -->
		[].
	sign(negative) -->
		[0'-].

	arguments([]) -->
		!, [].
	arguments([Term| Terms]) -->
		[0'(], term(Term), term_tail(Terms), [0')].

	terms([]) -->
		[].
	terms([Term| Terms]) -->
		term(Term), term_tail(Terms).

	term_tail([]) -->
		[].
	term_tail([Term| Terms]) -->
		[0',], term(Term), term_tail(Terms).

	term(number(Integer)) -->
		!, integer_text(Integer).
	term(string(Codes)) -->
		!,
		{ valid(codes, Codes) },
		[0'"], string_codes(Codes), [0'"].
	term(function(Name, Arguments, Sign)) -->
		!, asp_atom(function(Name, Arguments, Sign)).
	term(tuple(Terms)) -->
		!, [0'(], terms(Terms), tuple_comma(Terms), [0')].
	term(infimum) -->
		!, text('#inf').
	term(supremum) -->
		!, text('#sup').
	term(variable(Name)) -->
		!, variable_name(Name).
	term(anonymous) -->
		!, [0'_].
	term(interval(Lower, Upper)) -->
		!, [0'(], term(Lower), text('..'), term(Upper), [0')].
	term(arithmetic(abs, [Term])) -->
		!, [0'|], term(Term), [0'|].
	term(arithmetic('-', [Term])) -->
		!, text('(-'), term(Term), [0')].
	term(arithmetic(Operator, [Left, Right])) -->
		{ arithmetic_operator(Operator) },
		[0'(], term(Left), text(Operator), term(Right), [0')].

	arithmetic_operator('+').
	arithmetic_operator('-').
	arithmetic_operator('*').
	arithmetic_operator('/').
	arithmetic_operator('\\').
	arithmetic_operator('**').

	tuple_comma([_]) -->
		!, [0',].
	tuple_comma(_) -->
		[].

	integer_text(Integer) -->
		{ integer(Integer), number_codes(Integer, Codes) },
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
		!,
		[0'\\, 0'n].
	escaped_code(0'") -->
		!,
		[0'\\, 0'"].
	escaped_code(0'\\) -->
		!,
		[0'\\, 0'\\].
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

	ground_expression(number(_)).
	ground_expression(string(_)).
	ground_expression(infimum).
	ground_expression(supremum).
	ground_expression(function(_, Arguments, _)) :-
		ground_expressions(Arguments).
	ground_expression(tuple(Terms)) :-
		ground_expressions(Terms).
	ground_expression(arithmetic(_, Arguments)) :-
		ground_expressions(Arguments).

	ground_expressions([]).
	ground_expressions([Term| Terms]) :-
		ground_expression(Term),
		ground_expressions(Terms).

:- end_object.
