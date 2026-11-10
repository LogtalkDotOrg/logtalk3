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


:- object(souffle_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Opt-in ordinary-clause and directive expansion for pure Datalog problem objects.'
	]).

	:- uses(list, [
		append/3, member/2
	]).

	:- uses(type, [
		valid/2
	]).

	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_relation, implements_protocol(_, souffle_problem_protocol, _)),
		\+ structural_directive(Term),
		!,
		(	logtalk_load_context(variable_names(Term), Names) ->
			true
		;	Names = []
		),
		(	statement(Term, Names, Statement) ->
			souffle_program::serialize([Statement], _),
			origin(Term, Origin),
			Expanded = souffle_statement(Statement, Origin)
		;	domain_error(souffle_syntax, Term)
		).

	structural_directive((:- end_object)).
	structural_directive((:- info(_))).
	structural_directive((:- set_logtalk_flag(_, _))).

	origin(Term, Origin) :-
		(	logtalk_load_context(file, File),
			logtalk_load_context(term_position(Term), Start-End),
			Start > 0,
			End >= Start ->
			Origin = source(File, Start, End)
		;	Origin = unknown
		).

	statement((:- souffle_decl(Name, Columns)), _, decl(Name, Columns)) :-
		!.
	statement((:- souffle_input(Name)), _, input(Name)) :-
		!.
	statement((:- souffle_output(Name)), _, output(Name)) :-
		!.
	statement((:- souffle_printsize(Name)), _, printsize(Name)) :-
		!.
	statement((:- souffle_raw(Source)), _, raw(Codes)) :-
		!,
		normalize_text(Source, Codes).
	statement((:- comment(Source)), _, comment(Codes)) :-
		!,
		normalize_text(Source, Codes).
	statement((:- _), _, _) :-
		!,
		fail.
	statement((Head :- Body), Names, rule(HeadTerm, Literals)) :-
		!,
		positive_atom(Head, Names, HeadTerm),
		body(Body, Names, Literals).
	statement(Fact, Names, fact(Term)) :-
		positive_atom(Fact, Names, Term).

	normalize_text(Source, Codes) :-
		(	atom(Source) ->
			atom_codes(Source, Codes)
		;	valid(chars, Source) ->
			atom_chars(Atom, Source),
			atom_codes(Atom, Codes)
		;	{'$lgt_string'(Source)} ->
			{'$lgt_string_codes'(Source, Codes)}
		;	Codes = Source
		).

	body(Body, _, []) :-
		(	Body == true
		;	Body == []
		),
		!.
	body((Left, Right), Names, Literals) :-
		!,
		body(Left, Names, LeftLiterals),
		body(Right, Names, RightLiterals),
		append(LeftLiterals, RightLiterals, Literals).
	body(Body, Names, Literals) :-
		valid(list, Body),
		!,
		literals(Body, Names, Literals).
	body(Literal, Names, [Term]) :-
		literal(Literal, Names, Term).

	literals([], _, []).
	literals([Literal| Literals], Names, [Term| Terms]) :-
		literal(Literal, Names, Term),
		literals(Literals, Names, Terms).

	literal(not(Literal), Names, not(Term)) :-
		!, positive_atom(Literal, Names, Term).
	literal(\+(Literal), Names, not(Term)) :-
		!, positive_atom(Literal, Names, Term).
	literal(Literal, Names, compare(Operator, LeftTerm, RightTerm)) :-
		nonvar(Literal),
		Literal =.. [Functor, Left, Right],
		comparison(Functor, Operator),
		!,
		operand(Left, Names, LeftTerm),
		operand(Right, Names, RightTerm).
	literal(Atom, Names, Term) :-
		positive_atom(Atom, Names, Term).

	comparison('=', '=').
	comparison('==', '=').
	comparison('\\=', '!=').
	comparison('\\==', '!=').
	comparison('<', '<').
	comparison('=<', '<=').
	comparison('>', '>').
	comparison('>=', '>=').

	operand(Value, Names, aggregate(Function, TargetTerm, Literals)) :-
		nonvar(Value),
		Value = aggregate(Function, Target, Body),
		aggregate_function(Function),
		!,
		(	Target == none ->
			TargetTerm = none
		;	term(Target, Names, TargetTerm)
		),
		body(Body, Names, Literals).
	operand(Value, Names, Term) :-
		term(Value, Names, Term).

	aggregate_function(count).
	aggregate_function(sum).
	aggregate_function(max).
	aggregate_function(min).
	aggregate_function(mean).

	positive_atom(Atom, _, _) :-
		var(Atom),
		instantiation_error.
	positive_atom(Atom, Names, function(Name, Terms)) :-
		callable(Atom),
		Atom =.. [Name| Arguments],
		terms(Arguments, Names, Terms).

	terms([], _, []).
	terms([Value| Values], Names, [Term| Terms]) :-
		term(Value, Names, Term),
		terms(Values, Names, Terms).

	term(Value, Names, Term) :-
		var(Value),
		!,
		(	member(Name=Variable, Names),
			Value == Variable ->
			Term = variable(Name)
		;	Term = anonymous
		).
	term(Value, _, number(Value)) :-
		integer(Value),
		!.
	term(Value, _, float(Value)) :-
		float(Value),
		!.
	term(unsigned(Value), _, unsigned(Value)) :-
		!,
		integer(Value),
		Value >= 0.
	term(string(Codes), _, string(Codes)) :-
		!.
	term(cat(Left, Right), Names, cat(LeftTerm, RightTerm)) :-
		!,
		term(Left, Names, LeftTerm),
		term(Right, Names, RightTerm).
	term(Value, Names, arithmetic(Operator, Terms)) :-
		nonvar(Value),
		Value =.. [Operator| Arguments],
		arithmetic_operator(Operator),
		!,
		terms(Arguments, Names, Terms).
	term(Value, _, _) :-
		domain_error(souffle_syntax, Value).

	arithmetic_operator('+').
	arithmetic_operator('-').
	arithmetic_operator('*').
	arithmetic_operator('/').
	arithmetic_operator('%').

:- end_object.
