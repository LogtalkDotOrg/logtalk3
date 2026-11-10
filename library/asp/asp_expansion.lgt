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


:- object(asp_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Opt-in equivalent ASP directive and clause expansion for pure problem objects.'
	]).

	:- uses(list, [
		append/3, member/2
	]).

	:- uses(type, [
		valid/2
	]).

	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_relation, implements_protocol(_, asp_problem_protocol, _)),
		\+ structural_directive(Term),
		!,
		(	logtalk_load_context(variable_names(Term), Names) ->
			true
		;	Names = []
		),
		(	statement(Term, Names, Statement) ->
			asp_program::serialize([Statement], _),
			origin(Term, Origin),
			Expanded = asp_statement(Statement, Origin)
		;	domain_error(asp_syntax, Term)
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

	statement((:- const(Name, Value)), Names, constant(Name, Term)) :-
		!,
		term(Value, Names, Term).
	statement((:- show(none)), _, show(none)) :-
		!.
	statement((:- show(signature(Name, Arity, Sign))), _, show(signature(Name, Arity, Sign))) :-
		!.
	statement((:- show(Name/Arity)), _, show(signature(Name, Arity, positive))) :-
		!.
	statement((:- show(Value, Body)), Names, show_term(Term, Literals)) :-
		!,
		term(Value, Names, Term),
		body(Body, Names, Literals).
	statement((:- minimize(Elements)), Names, minimize(Objectives)) :-
		!,
		objectives(Elements, Names, Objectives).
	statement((:- maximize(Elements)), Names, maximize(Objectives)) :-
		!,
		objectives(Elements, Names, Objectives).
	statement((:- constraint(Body)), Names, constraint(Literals)) :-
		!,
		body(Body, Names, Literals).
	statement((:- weak_constraint(Body, Weight, Priority, Tuple)), Names, weak_constraint(Literals, WeightTerm, PriorityTerm, Terms)) :-
		!,
		body(Body, Names, Literals),
		term(Weight, Names, WeightTerm),
		term(Priority, Names, PriorityTerm),
		terms(Tuple, Names, Terms).
	statement((:- raw(Source)), _, raw(Source)) :-
		!.
	statement((:- _), _, _) :-
		!,
		fail.
	statement((Head :- Body), Names, rule(ASPHead, Literals)) :-
		!,
		head(Head, Names, ASPHead),
		body(Body, Names, Literals).
	statement(Fact, Names, Statement) :-
		head(Fact, Names, Head),
		(	Head = atom(Atom) ->
			Statement = fact(Atom)
		;	Statement = rule(Head, [])
		).

	head(disjunction(Atoms), Names, disjunction(Terms)) :-
		!,
		atoms(Atoms, Names, Terms).
	head(choice(Lower, Elements, Upper), Names, choice(LowerTerm, Choices, UpperTerm)) :-
		!,
		bound(Lower, Names, LowerTerm),
		choices(Elements, Names, Choices),
		bound(Upper, Names, UpperTerm).
	head(Atom, Names, atom(Term)) :-
		asp_atom(Atom, Names, Term).

	bound(Value, _, none) :-
		Value == none,
		!.
	bound(Value, Names, Term) :-
		term(Value, Names, Term).

	choices([], _, []).
	choices([conditional(Atom, Body)| Elements], Names, [conditional(Term, Literals)| Choices]) :-
		asp_atom(Atom, Names, Term),
		body(Body, Names, Literals),
		choices(Elements, Names, Choices).

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
		!,
		literal(Literal, Names, Term).
	literal(conditional(Atom, Body), Names, conditional(Term, Literals)) :-
		!,
		asp_atom(Atom, Names, Term),
		body(Body, Names, Literals).
	literal(compare(Operator, Left, Right), Names, compare(Operator, LeftTerm, RightTerm)) :-
		!,
		operand(Left, Names, LeftTerm),
		operand(Right, Names, RightTerm).
	literal(aggregate(Function, Elements), Names, aggregate(Function, Terms)) :-
		!,
		aggregate_elements(Elements, Names, Terms).
	literal(Literal, Names, compare(Operator, LeftTerm, RightTerm)) :-
		nonvar(Literal),
		Literal =.. [Functor, Left, Right],
		comparison(Functor, Operator),
		!,
		operand(Left, Names, LeftTerm),
		operand(Right, Names, RightTerm).
	literal(Atom, Names, Term) :-
		asp_atom(Atom, Names, Term).

	comparison('=', '=').
	comparison('\\=', '!=').
	comparison('<', '<').
	comparison('=<', '<=').
	comparison('>', '>').
	comparison('>=', '>=').

	operand(Value, Names, aggregate(Function, Terms)) :-
		nonvar(Value),
		Value = aggregate(Function, Elements),
		!,
		aggregate_elements(Elements, Names, Terms).
	operand(Value, Names, Term) :-
		term(Value, Names, Term).

	aggregate_elements([], _, []).
	aggregate_elements([aggregate_element(Tuple, Body)| Elements], Names, [aggregate_element(Terms, Literals)| Rest]) :-
		terms(Tuple, Names, Terms),
		body(Body, Names, Literals),
		aggregate_elements(Elements, Names, Rest).

	objectives([], _, []).
	objectives([objective(Weight, Priority, Tuple, Body)| Elements], Names, [objective(WeightTerm, PriorityTerm, Terms, Literals)| Rest]) :-
		term(Weight, Names, WeightTerm),
		term(Priority, Names, PriorityTerm),
		terms(Tuple, Names, Terms),
		body(Body, Names, Literals),
		objectives(Elements, Names, Rest).

	atoms([], _, []).
	atoms([Atom| Atoms], Names, [Term| Terms]) :-
		asp_atom(Atom, Names, Term),
		atoms(Atoms, Names, Terms).

	asp_atom(Atom, _, _) :-
		var(Atom),
		instantiation_error.
	asp_atom(neg(Atom), Names, function(Name, Arguments, negative)) :-
		!,
		asp_atom(Atom, Names, function(Name, Arguments, positive)).
	asp_atom(Atom, Names, function(Name, Terms, positive)) :-
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
	term(string(Codes), _, string(Codes)) :-
		!.
	term(tuple(Values), Names, tuple(Terms)) :-
		!,
		terms(Values, Names, Terms).
	term(interval(Lower, Upper), Names, interval(LowerTerm, UpperTerm)) :-
		!,
		term(Lower, Names, LowerTerm),
		term(Upper, Names, UpperTerm).
	term(arithmetic(Operator, Values), Names, arithmetic(Operator, Terms)) :-
		!,
		terms(Values, Names, Terms).
	term(infimum, _, infimum) :-
		!.
	term(supremum, _, supremum) :-
		!.
	term(Value, Names, arithmetic(Operator, Terms)) :-
		nonvar(Value),
		Value =.. [Operator| Arguments],
		arithmetic_operator(Operator),
		!,
		terms(Arguments, Names, Terms).
	term(Value, Names, Term) :-
		asp_atom(Value, Names, Term).

	arithmetic_operator('+').
	arithmetic_operator('-').
	arithmetic_operator('*').
	arithmetic_operator('/').
	arithmetic_operator('\\').
	arithmetic_operator('**').
	arithmetic_operator(abs).

:- end_object.
