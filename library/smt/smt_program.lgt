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


:- object(smt_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Canonical SMT-LIB 2 statement validation, source generation, and response-producing statement classification.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list, -list(code)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical SMT-LIB 2 statements to character codes, auto-declaring ``:produce-models`` and ``:produce-unsat-cores`` options when required and not already set by the caller.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(smt_statement, 'Statement')
		]
	]).

	:- public(serialize/4).
	:- mode(serialize(+list, +list, -list(code), -list(compound)), one_or_error).
	:- info(serialize/4, [
		comment is 'Serializes statements with origins and returns ``line(Start, End, Origin)`` source-map entries; auto-declared option statements use the ``unknown`` origin.',
		argnames is ['Statements', 'Origins', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` is a partial list or a list with an element ``Statement`` which is a variable' - instantiation_error,
			'``Statements`` is neither a partial list nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(smt_statement, 'Statement'),
			'``Origins`` is a partial list or a list with an element ``Origin`` which is a variable' - instantiation_error,
			'``Origins`` is neither a partial list nor a list' - type_error(list, 'Origins'),
			'An element of the list ``Origins`` is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(smt_origins, 'Origins')
		]
	]).

	:- public(response_producing/1).
	:- mode(response_producing(+compound), zero_or_one).
	:- info(response_producing/1, [
		comment is 'Succeeds when ``Statement`` is a command that produces a solver response line (``check-sat``, ``check-sat-assuming``, ``get-model``, ``get-unsat-core``, or ``get-value``).',
		argnames is ['Statement']
	]).

	:- uses(type, [
		check/3, valid/2
	]).

	:- uses(list, [
		append/3, length/2, member/2
	]).

	serialize(Statements, Codes) :-
		context(Context),
		check(ground, Statements, Context),
		check(list, Statements, Context),
		valid_statements(Statements),
		synthetic_statements(Statements, Synthetic),
		append(Synthetic, Statements, AllStatements),
		serialize_statements(AllStatements, Codes, []).

	serialize(Statements, Origins, Codes, SourceMap) :-
		context(Context),
		check(ground, Statements-Origins, Context),
		check(list, Statements, Context),
		check(list, Origins, Context),
		valid_statements(Statements),
		(	valid_origins(Statements, Origins) ->
			synthetic_statements(Statements, Synthetic),
			length(Synthetic, SyntheticCount),
			synthetic_origins(SyntheticCount, SyntheticOrigins),
			append(Synthetic, Statements, AllStatements),
			append(SyntheticOrigins, Origins, AllOrigins),
			serialize_located(AllStatements, AllOrigins, 1, Codes, [], SourceMap)
		;	domain_error(smt_origins, Origins)
		).

	synthetic_origins(0, []) :-
		!.
	synthetic_origins(Count, [unknown| Origins]) :-
		Count > 0,
		Next is Count - 1,
		synthetic_origins(Next, Origins).

	% Global options that affect solving infrastructure must be set before any
	% "set-logic" command; auto-declared options are therefore always prepended.
	synthetic_statements(Statements, Synthetic) :-
		(	needs_option(Statements, get_model, produce_models) ->
			ModelsOption = [set_option(produce_models, true)]
		;	ModelsOption = []
		),
		(	needs_option(Statements, get_unsat_core, produce_unsat_cores) ->
			CoresOption = [set_option(produce_unsat_cores, true)]
		;	(	member(assert_named(_, _), Statements),
				\+ member(set_option(produce_unsat_cores, _), Statements) ->
				CoresOption = [set_option(produce_unsat_cores, true)]
			;	CoresOption = []
			)
		),
		append(ModelsOption, CoresOption, Synthetic).

	needs_option(Statements, Trigger, Keyword) :-
		functor(TriggerTemplate, Trigger, 0),
		member(TriggerTemplate, Statements),
		\+ member(set_option(Keyword, _), Statements).

	valid_statements([]).
	valid_statements([Statement| Statements]) :-
		(	valid_statement(Statement) ->
			true
		;	domain_error(smt_statement, Statement)
		),
		valid_statements(Statements).

	valid_statement(set_logic(Logic)) :-
		!,
		atom(Logic).
	valid_statement(set_option(Keyword, Value)) :-
		!,
		atom(Keyword),
		(atom(Value) ; integer(Value)).
	valid_statement(declare_sort(Name, Arity)) :-
		!,
		atom(Name),
		integer(Arity),
		Arity >= 0.
	valid_statement(define_sort(Name, Params, Definition)) :-
		!,
		atom(Name),
		valid(list(atom), Params),
		valid_sort(Definition).
	valid_statement(declare_const(Name, Sort)) :-
		!,
		atom(Name),
		valid_sort(Sort).
	valid_statement(declare_fun(Name, ArgSorts, Sort)) :-
		!,
		atom(Name),
		valid(list, ArgSorts),
		valid_sorts(ArgSorts),
		valid_sort(Sort).
	valid_statement(define_fun(Name, Params, Sort, Body)) :-
		!,
		atom(Name),
		valid_bindings(Params),
		valid_sort(Sort),
		valid_term(Body).
	valid_statement(assert(Term)) :-
		!,
		valid_term(Term).
	valid_statement(assert_named(Name, Term)) :-
		!,
		atom(Name),
		valid_term(Term).
	valid_statement(push(Count)) :-
		!,
		integer(Count),
		Count >= 0.
	valid_statement(pop(Count)) :-
		!,
		integer(Count),
		Count >= 0.
	valid_statement(check_sat) :-
		!.
	valid_statement(check_sat_assuming(Literals)) :-
		!,
		valid(list, Literals),
		valid_terms(Literals).
	valid_statement(get_model) :-
		!.
	valid_statement(get_unsat_core) :-
		!.
	valid_statement(get_value(Terms)) :-
		!,
		valid(list, Terms),
		Terms \== [],
		valid_terms(Terms).
	valid_statement(comment(Codes)) :-
		!,
		valid(codes, Codes),
		\+ member(10, Codes).
	valid_statement(raw(Codes)) :-
		valid(codes, Codes).

	valid_sorts([]).
	valid_sorts([Sort| Sorts]) :-
		valid_sort(Sort),
		valid_sorts(Sorts).

	valid_sort(Sort) :-
		(	atom(Sort) ->
			true
		;	Sort = bv(Width) ->
			integer(Width),
			Width > 0
		;	Sort = array(Index, Element) ->
			valid_sort(Index),
			valid_sort(Element)
		;	compound(Sort) ->
			Sort =.. [Name| Args],
			atom(Name),
			valid_sorts(Args)
		;	fail
		).

	valid_bindings([]).
	valid_bindings([Name-Sort| Bindings]) :-
		atom(Name),
		valid_sort(Sort),
		valid_bindings(Bindings).

	valid_terms([]).
	valid_terms([Term| Terms]) :-
		valid_term(Term),
		valid_terms(Terms).

	valid_term(Term) :-
		(	number(Term) ->
			true
		;	atom(Term) ->
			true
		;	Term = string(Text) ->
			atom(Text)
		;	Term = bv(Value, Width) ->
			integer(Value), Value >= 0,
			integer(Width), Width > 0
		;	Term = forall(Bindings, Body) ->
			valid_bindings(Bindings),
			valid_term(Body)
		;	Term = exists(Bindings, Body) ->
			valid_bindings(Bindings),
			valid_term(Body)
		;	Term = extract(I, J, Inner) ->
			integer(I), integer(J), I >= J, J >= 0,
			valid_term(Inner)
		;	Term = zero_extend(N, Inner) ->
			integer(N), N >= 0,
			valid_term(Inner)
		;	Term = sign_extend(N, Inner) ->
			integer(N), N >= 0,
			valid_term(Inner)
		;	compound(Term) ->
			Term =.. [Name| Args],
			atom(Name),
			Args \== [],
			valid_terms(Args)
		;	fail
		).

	response_producing(check_sat).
	response_producing(check_sat_assuming(_)).
	response_producing(get_model).
	response_producing(get_unsat_core).
	response_producing(get_value(_)).

	serialize_statements([], Codes, Codes).
	serialize_statements([Statement| Statements], Codes, Tail) :-
		phrase(statement(Statement), Codes, Next),
		serialize_statements(Statements, Next, Tail).

	serialize_located([], [], _, Codes, Codes, []).
	serialize_located([Statement| Statements], [Origin| Origins], Start, Codes, Tail, [line(Start, End, Origin)| Map]) :-
		phrase(statement(Statement), StatementCodes, []),
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

	% Statement grammar.
	statement(set_logic(Logic)) -->
		!,
		text('(set-logic '), text(Logic), text(')'), [10].
	statement(set_option(Keyword, Value)) -->
		!,
		{ dashed(Keyword, Dashed) },
		text('(set-option :'), text(Dashed), [0' ], option_value(Value), text(')'), [10].
	statement(declare_sort(Name, Arity)) -->
		!,
		text('(declare-sort '), text(Name), [0' ], integer_text(Arity), text(')'), [10].
	statement(define_sort(Name, Params, Definition)) -->
		!,
		text('(define-sort '), text(Name), text(' ('), atom_list(Params), text(') '), sort(Definition), text(')'), [10].
	statement(declare_const(Name, Sort)) -->
		!,
		text('(declare-const '), text(Name), [0' ], sort(Sort), text(')'), [10].
	statement(declare_fun(Name, ArgSorts, Sort)) -->
		!,
		text('(declare-fun '), text(Name), text(' ('), sort_list(ArgSorts), text(') '), sort(Sort), text(')'), [10].
	statement(define_fun(Name, Params, Sort, Body)) -->
		!,
		text('(define-fun '), text(Name), text(' ('), bindings(Params), text(') '), sort(Sort), [0' ], term(Body), text(')'), [10].
	statement(assert(Term)) -->
		!,
		text('(assert '), term(Term), text(')'), [10].
	statement(assert_named(Name, Term)) -->
		!,
		text('(assert (! '), term(Term), text(' :named '), text(Name), text('))'), [10].
	statement(push(Count)) -->
		!,
		text('(push '), integer_text(Count), text(')'), [10].
	statement(pop(Count)) -->
		!,
		text('(pop '), integer_text(Count), text(')'), [10].
	statement(check_sat) -->
		!,
		text('(check-sat)'), [10].
	statement(check_sat_assuming(Literals)) -->
		!,
		text('(check-sat-assuming ('), term_list(Literals), text('))'), [10].
	statement(get_model) -->
		!,
		text('(get-model)'), [10].
	statement(get_unsat_core) -->
		!,
		text('(get-unsat-core)'), [10].
	statement(get_value(Terms)) -->
		!,
		text('(get-value ('), term_list(Terms), text('))'), [10].
	statement(comment(Codes)) -->
		!,
		text('; '), codes(Codes), [10].
	statement(raw(Codes)) -->
		codes(Codes), [10].

	option_value(true) -->
		!,
		text(true).
	option_value(false) -->
		!,
		text(false).
	option_value(Value) -->
		{ integer(Value) },
		!,
		integer_text(Value).
	option_value(Value) -->
		text(Value).

	dashed(Keyword, Dashed) :-
		atom_codes(Keyword, Codes),
		replace_underscores(Codes, DashedCodes),
		atom_codes(Dashed, DashedCodes).

	replace_underscores([], []).
	replace_underscores([0'_| Codes], [0'-| DashedCodes]) :-
		!,
		replace_underscores(Codes, DashedCodes).
	replace_underscores([Code| Codes], [Code| DashedCodes]) :-
		replace_underscores(Codes, DashedCodes).

	atom_list([]) -->
		[].
	atom_list([Name| Names]) -->
		text(Name), atom_list_tail(Names).

	atom_list_tail([]) -->
		[].
	atom_list_tail([Name| Names]) -->
		[0' ], text(Name), atom_list_tail(Names).

	sort_list([]) -->
		[].
	sort_list([Sort| Sorts]) -->
		sort(Sort), sort_list_tail(Sorts).

	sort_list_tail([]) -->
		[].
	sort_list_tail([Sort| Sorts]) -->
		[0' ], sort(Sort), sort_list_tail(Sorts).

	bindings([]) -->
		[].
	bindings([Name-Sort| Bindings]) -->
		text('('), text(Name), [0' ], sort(Sort), text(')'), bindings_tail(Bindings).

	bindings_tail([]) -->
		[].
	bindings_tail([Name-Sort| Bindings]) -->
		[0' ], text('('), text(Name), [0' ], sort(Sort), text(')'), bindings_tail(Bindings).

	term_list([]) -->
		[].
	term_list([Term| Terms]) -->
		term(Term), term_list_tail(Terms).

	term_list_tail([]) -->
		[].
	term_list_tail([Term| Terms]) -->
		[0' ], term(Term), term_list_tail(Terms).

	% Sort grammar.
	sort(int) -->
		!,
		text('Int').
	sort(bool) -->
		!,
		text('Bool').
	sort(real) -->
		!,
		text('Real').
	sort(bv(Width)) -->
		!,
		text('(_ BitVec '), integer_text(Width), text(')').
	sort(array(Index, Element)) -->
		!,
		text('(Array '), sort(Index), [0' ], sort(Element), text(')').
	sort(Name) -->
		{ atom(Name) },
		!,
		text(Name).
	sort(Sort) -->
		{ Sort =.. [Name| Args] },
		text('('), text(Name), [0' ], sort_list(Args), text(')').

	% Term grammar.
	term(true) -->
		!,
		text('true').
	term(false) -->
		!,
		text('false').
	term(string(Text)) -->
		!,
		{ atom_codes(Text, Codes), escape_string(Codes, Escaped) },
		[0'"], codes(Escaped), [0'"].
	term(bv(Value, Width)) -->
		!,
		{ bits(Value, Width, Bits) },
		text('#b'), codes(Bits).
	term(forall(Bindings, Body)) -->
		!,
		text('(forall ('), bindings(Bindings), text(') '), term(Body), text(')').
	term(exists(Bindings, Body)) -->
		!,
		text('(exists ('), bindings(Bindings), text(') '), term(Body), text(')').
	term(extract(I, J, Inner)) -->
		!,
		text('((_ extract '), integer_text(I), [0' ], integer_text(J), text(') '), term(Inner), text(')').
	term(zero_extend(N, Inner)) -->
		!,
		text('((_ zero_extend '), integer_text(N), text(') '), term(Inner), text(')').
	term(sign_extend(N, Inner)) -->
		!,
		text('((_ sign_extend '), integer_text(N), text(') '), term(Inner), text(')').
	term(Number) -->
		{ number(Number) },
		!,
		numeral(Number).
	term(Atom) -->
		{ atom(Atom) },
		!,
		text(Atom).
	term(Term) -->
		{ Term =.. [Name| Args] },
		text('('), text(Name), [0' ], term_list(Args), text(')').

	numeral(Number) -->
		{ Number >= 0, number_codes(Number, Codes) },
		!,
		codes(Codes).
	numeral(Number) -->
		{ Positive is -Number, number_codes(Positive, Codes) },
		text('(- '), codes(Codes), text(')').

	bits(Value, Width, Bits) :-
		bits(Value, Width, [], Bits).

	bits(_, 0, Bits, Bits) :-
		!.
	bits(Value, Width, Acc, Bits) :-
		Bit is Value /\ 1,
		(	Bit =:= 0 ->
			Code = 0'0
		;	Code = 0'1
		),
		Rest is Value >> 1,
		Next is Width - 1,
		bits(Rest, Next, [Code| Acc], Bits).

	escape_string([], []).
	escape_string([0'"| Codes], [0'", 0'"| Escaped]) :-
		!,
		escape_string(Codes, Escaped).
	escape_string([Code| Codes], [Code| Escaped]) :-
		escape_string(Codes, Escaped).

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

	integer_text(Integer) -->
		{ integer(Integer), Integer >= 0, number_codes(Integer, Codes) },
		codes(Codes).

	text(Atom) -->
		{ atom_codes(Atom, Codes) },
		codes(Codes).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

:- end_object.
