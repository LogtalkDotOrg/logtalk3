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


:- object(problog_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-12,
		comment is 'Canonical ground ProbLog statement validation and source generation.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(integer)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes a list of fully ground canonical ProbLog statements to character codes.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or contains a variable' - instantiation_error,
			'``Statements`` is not a list' - type_error(list, 'Statements'),
			'A statement or one of its terms is outside the documented ProbLog AST' - domain_error(problog_statement, 'Statement')
		],
		remarks is [
			'Statements' - 'The statement constructors are ``fact(Head)``, ``rule(Head, Body)``, ``probabilistic_fact(P, Head)``, ``probabilistic_rule(P, Head, Body)``, ``annotated_disjunction(Alternatives, Body)``, ``query(Goal)``, ``evidence(Goal, Boolean)``, ``directive(Goal)``, ``comment(Codes)``, and ``raw(Codes)``.',
			'Heads and positive goals' - 'Use ``function(Name, Arguments)``; a zero-arity predicate has an empty argument list. Terms are ``atom(Atom)``, ``number(Number)``, ``variable(Name)``, ``anonymous``, nested ``function(Name, Arguments)``, proper ``list(Elements)``, improper ``list(Elements, Tail)``, and ``arithmetic(Operator, Arguments)``. Logic variables are represented by ground ``variable(Name)`` or ``anonymous`` nodes, preserving variable sharing without putting Prolog variables in the AST.',
			'Goals' - 'Use ``true``, ``function(Name, Arguments)``, ``conjunction(Goals)``, ``disjunction(Goals)``, ``negation(Goal)``, or ``comparison(Operator, Left, Right)``. Arithmetic operators are ``+``, ``-``, ``*``, ``/``, ``//``, ``mod``, and ``**``; comparison operators are ``=``, ``==``, ``\\=``, ``\\==``, ``<``, ``=<``, ``>``, and ``>=``.',
			'Queries and evidence' - '``query(Goal)`` writes a native query directive; ``evidence(Goal, true)`` and ``evidence(Goal, false)`` write positive and negative evidence directives.',
			'Annotated disjunctions' - '``annotated_disjunction(Alternatives, Body)`` uses at least two ``Probability-Head`` list elements and writes ``Probability::Head; ...`` optionally followed by ``:- Body``.',
			'Raw source and comments' - '``raw(Codes)`` passes trusted native source through, adding a final newline when needed. ``comment(Codes)`` emits comment lines. Both use ground character-code lists.',
			'Probabilities' - 'Probabilities are numbers in the inclusive range [0,1]. Annotated-disjunction probabilities must sum to at most one; a residual probability is allowed.'
		]
	]).

	:- public(serialize/4).
	:- mode(serialize(+list, +list, -list(integer), -list(compound)), one_or_error).
	:- info(serialize/4, [
		comment is 'Serializes statements with origins and returns generated ``line(Start, End, Origin)`` source-map entries.',
		argnames is ['Statements', 'Origins', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` or ``Origins`` is a partial list or contains a variable' - instantiation_error,
			'``Statements`` or ``Origins`` is not a list' - type_error(list, 'Statements'),
			'An invalid statement or origin is supplied, or the list lengths differ' - domain_error(problog_origins, 'Origins')
		]
	]).

	:- uses(list, [
		append/3, last/2, length/2
	]).

	:- uses(type, [
		check/3, valid/2
	]).

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
		;	domain_error(problog_origins, Origins)
		).

	serialize_statements([], Codes, Codes).
	serialize_statements([Statement| Statements], Codes, Tail) :-
		statement_codes(Statement, StatementCodes),
		append(StatementCodes, Rest, Codes),
		serialize_statements(Statements, Rest, Tail).

	statement_codes(Statement, Codes) :-
		(	phrase(statement(Statement), Codes) ->
			true
		;	domain_error(problog_statement, Statement)
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
		statement_codes(Statement, StatementCodes),
		count_lines(StatementCodes, Start, Next),
		End is Next - 1,
		append(StatementCodes, Rest, Codes),
		serialize_located(Statements, Origins, Next, Rest, Tail, Map).

	count_lines([], Line, Line).
	count_lines([10| Codes], Line0, Line) :-
		!,
		Line1 is Line0 + 1,
		count_lines(Codes, Line1, Line).
	count_lines([_| Codes], Line0, Line) :-
		count_lines(Codes, Line0, Line).

	statement(fact(Head)) -->
		!, positive_atom(Head), [0'., 10].
	statement(rule(Head, Body)) -->
		!, positive_atom(Head), rule_body(Body), [0'., 10].
	statement(probabilistic_fact(Probability, Head)) -->
		!, probability(Probability), text('::'), positive_atom(Head), [0'., 10].
	statement(probabilistic_rule(Probability, Head, Body)) -->
		!, probability(Probability), text('::'), positive_atom(Head), rule_body(Body), [0'., 10].
	statement(annotated_disjunction(Alternatives, Body)) -->
		!,
		{ valid_alternatives(Alternatives) },
		alternatives(Alternatives), rule_body(Body), [0'., 10].
	statement(query(Goal)) -->
		!, text('query('), goal(Goal), text(').'), [10].
	statement(evidence(Goal, Boolean)) -->
		!, { valid_boolean(Boolean) }, text('evidence('), goal(Goal), [44, 32], boolean(Boolean), text(').'), [10].
	statement(directive(Goal)) -->
		!, text(':- '), goal(Goal), [0'., 10].
	statement(comment(Codes)) -->
		!, { valid(codes, Codes) }, comment_codes(Codes).
	statement(raw(Codes)) -->
		!, { valid(codes, Codes) }, raw_codes(Codes).

	rule_body(true) -->
		!, [].
	rule_body(Body) -->
		[32], text(':- '), goal(Body).

	positive_atom(function(Name, Arguments)) -->
		atom_name(Name),
		arguments(Arguments).

	arguments([]) -->
		!, [].
	arguments([Term| Terms]) -->
		[0'(], term(Term), term_tail(Terms), [0')].

	term_tail([]) -->
		[].
	term_tail([Term| Terms]) -->
		[44, 32], term(Term), term_tail(Terms).

	goal(true) -->
		!, text('true').
	goal(function(Name, Arguments)) -->
		!, positive_atom(function(Name, Arguments)).
	goal(conjunction(Goals)) -->
		!, [0'(], goals(Goals), [0')].
	goal(disjunction(Goals)) -->
		!, [0'(], disjuncts(Goals), [0')].
	goal(negation(Goal)) -->
		!, text('\\+ ('), goal(Goal), [0')].
	goal(comparison(Operator, Left, Right)) -->
		!, term(Left), [32], comparison_operator(Operator), [32], term(Right).

	goals([Goal]) -->
		!, goal(Goal).
	goals([Goal| Goals]) -->
		goal(Goal), text(', '), goals(Goals).

	disjuncts([Goal]) -->
		!, goal(Goal).
	disjuncts([Goal| Goals]) -->
		goal(Goal), text('; '), disjuncts(Goals).

	comparison_operator('=') -->
		!, text('=').
	comparison_operator('==') -->
		!, text('==').
	comparison_operator('\\=') -->
		!, text('\\=').
	comparison_operator('\\==') -->
		!, text('\\==').
	comparison_operator('<') -->
		!, text('<').
	comparison_operator('=<') -->
		!, text('=<').
	comparison_operator('>') -->
		!, text('>').
	comparison_operator('>=') -->
		!, text('>=').

	term(atom(Atom)) -->
		!, atom_text(Atom).
	term(number(Number)) -->
		!, number_text(Number).
	term(variable(Name)) -->
		!, variable_name(Name).
	term(anonymous) -->
		!, [0'_].
	term(function(Name, Arguments)) -->
		!, positive_atom(function(Name, Arguments)).
	term(list(Elements)) -->
		!, [0'[], list_elements(Elements), [0']].
	term(list(Elements, Tail)) -->
		!, { Elements = [_|_] }, [0'[], list_elements(Elements), [0'|, 32], term(Tail), [0']].
	term(arithmetic(Operator, Arguments)) -->
		!,
		{ arithmetic_operator(Operator, Arity), length(Arguments, Arity) },
		arithmetic(Operator, Arguments).

	list_elements([]) -->
		[].
	list_elements([Term]) -->
		!, term(Term).
	list_elements([Term| Terms]) -->
		term(Term), [44, 32], list_elements(Terms).

	arithmetic('+', [Term]) -->
		!, text('+('), term(Term), [0')].
	arithmetic('-', [Term]) -->
		!, text('-('), term(Term), [0')].
	arithmetic(Operator, [Left, Right]) -->
		{ arithmetic_symbol(Operator, Symbol) },
		[0'(], term(Left), [32], text(Symbol), [32], term(Right), [0')].

	arithmetic_operator('+', 1).
	arithmetic_operator('-', 1).
	arithmetic_operator('+', 2).
	arithmetic_operator('-', 2).
	arithmetic_operator('*', 2).
	arithmetic_operator('/', 2).
	arithmetic_operator('//', 2).
	arithmetic_operator(mod, 2).
	arithmetic_operator('**', 2).

	arithmetic_symbol('+', '+').
	arithmetic_symbol('-', '-').
	arithmetic_symbol('*', '*').
	arithmetic_symbol('/', '/').
	arithmetic_symbol('//', '//').
	arithmetic_symbol(mod, 'mod').
	arithmetic_symbol('**', '**').

	alternatives([Probability-Head]) -->
		!, probability(Probability), text('::'), positive_atom(Head).
	alternatives([Probability-Head| Alternatives]) -->
		probability(Probability), text('::'), positive_atom(Head),
		text('; '), alternatives(Alternatives).

	valid_alternatives([First, Second| Alternatives]) :-
		First = Probability-Head,
		valid_probability(Probability),
		valid_positive_atom(Head),
		alternative_sum([Second| Alternatives], Probability, Sum),
		Sum =< 1.

	alternative_sum([], Sum, Sum).
	alternative_sum([Probability-Head| Alternatives], Sum0, Sum) :-
		valid_probability(Probability),
		valid_positive_atom(Head),
		Sum1 is Sum0 + Probability,
		alternative_sum(Alternatives, Sum1, Sum).

	probability(Probability) -->
		{ valid_probability(Probability) },
		number_text(Probability).

	valid_probability(Probability) :-
		number(Probability),
		0 =< Probability, Probability =< 1.

	valid_positive_atom(function(Name, Arguments)) :-
		atom(Name),
		valid_terms(Arguments).

	valid_terms([]).
	valid_terms([Term| Terms]) :-
		valid_term(Term),
		valid_terms(Terms).

	valid_term(atom(Atom)) :-
		atom(Atom).
	valid_term(number(Number)) :-
		number(Number),
		number_codes(Number, [_|_]).
	valid_term(variable(Name)) :-
		valid_variable_name(Name).
	valid_term(anonymous).
	valid_term(function(Name, Arguments)) :-
		atom(Name),
		valid_terms(Arguments).
	valid_term(list(Elements)) :-
		valid_terms(Elements).
	valid_term(list(Elements, Tail)) :-
		Elements = [_|_],
		valid_terms(Elements),
		valid_term(Tail).
	valid_term(arithmetic(Operator, Arguments)) :-
		arithmetic_operator(Operator, Arity),
		length(Arguments, Arity),
		valid_terms(Arguments).

	valid_variable_name(Name) :-
		atom(Name),
		atom_codes(Name, [First| Rest]),
		(	First =:= 0'_ ->
			true
		;	First >= 0'A,
			First =< 0'Z
		),
		name_tail(Rest),
		Name \== '_'.

	variable_name(Name) -->
		{ valid_variable_name(Name), atom_codes(Name, Codes) },
		codes(Codes).

	atom_name(Name) -->
		{ atom(Name) },
		atom_text(Name).

	atom_text(Atom) -->
		{ atom_codes(Atom, Codes), plain_atom(Codes) },
		!, codes(Codes).
	atom_text(Atom) -->
		quoted_atom(Atom).

	plain_atom([First| Rest]) :-
		First >= 0'a,
		First =< 0'z,
		name_tail(Rest).

	quoted_atom(Atom) -->
		{ atom(Atom), atom_codes(Atom, Codes) },
		[39], quoted_codes(Codes), [39].

	quoted_codes([]) -->
		[].
	quoted_codes([39| Codes]) -->
		!, [39, 39], quoted_codes(Codes).
	quoted_codes([0'\\| Codes]) -->
		!, [0'\\, 0'\\], quoted_codes(Codes).
	quoted_codes([10| Codes]) -->
		!, [0'\\, 0'n], quoted_codes(Codes).
	quoted_codes([13| Codes]) -->
		!, [0'\\, 0'r], quoted_codes(Codes).
	quoted_codes([9| Codes]) -->
		!, [0'\\, 0't], quoted_codes(Codes).
	quoted_codes([Code| Codes]) -->
		{ Code >= 32, Code =\= 127 },
		[Code], quoted_codes(Codes).

	number_text(Number) -->
		{ number(Number), number_codes(Number, Codes), Codes = [_|_] },
		codes(Codes).

	boolean(true) -->
		!, text('true').
	boolean(false) -->
		!, text('false').

	valid_boolean(true).
	valid_boolean(false).

	comment_codes(Codes) -->
		comment_line(Codes).

	comment_line([]) -->
		!, text('% '), [10].
	comment_line([10| Codes]) -->
		!, [10], comment_line(Codes).
	comment_line([Code| Codes]) -->
		!, text('% '), [Code], comment_content(Codes).

	comment_content([]) -->
		!, [10].
	comment_content([10| Codes]) -->
		!, [10], comment_line(Codes).
	comment_content([Code| Codes]) -->
		[Code], comment_content(Codes).

	raw_codes(Codes) -->
		{	append(Codes, [10], Appended),
			(	Codes == [] ->
				true
			;	last(Codes, Last),
				Last =\= 10
			)
		},
		codes(Appended).
	raw_codes(Codes) -->
		codes(Codes).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

	text(Atom) -->
		{ atom_codes(Atom, Codes) },
		codes(Codes).

	name_tail([]).
	name_tail([Code| Codes]) :-
		(	0'a =< Code, Code =< 0'z
		;	0'A =< Code, Code =< 0'Z
		;	0'0 =< Code, Code =< 0'9
		;	Code =:= 0'_
		),
		name_tail(Codes).

:- end_object.
