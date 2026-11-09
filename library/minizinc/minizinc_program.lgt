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


:- object(minizinc_program).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-09,
		comment is 'Canonical MiniZinc statement validation and source generation.'
	]).

	:- public(serialize/2).
	:- mode(serialize(+list(compound), -list(integer)), one_or_error).
	:- info(serialize/2, [
		comment is 'Serializes ground canonical MiniZinc statements to character codes.',
		argnames is ['Statements', 'Codes'],
		exceptions is [
			'``Statements`` is a variable' - instantiation_error,
			'An element ``Statement`` of the list ``Statements`` is a variable' - instantiation_error,
			'``Statements`` is neither a variable nor a list' - type_error(list, 'Statements'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(minizinc_statement, 'Statement')
		]
	]).

	:- public(serialize/4).
	:- mode(serialize(+list(compound), +list, -list(integer), -list(compound)), one_or_error).
	:- info(serialize/4, [
		comment is 'Serializes statements with origins and returns ``line(Start, End, Origin)`` source-map entries.',
		argnames is ['Statements', 'Origins', 'Codes', 'SourceMap'],
		exceptions is [
			'``Statements`` or ``Origins`` is a variable' - instantiation_error,
			'An element ``Statement`` of the list ``Statements`` is a variable' - instantiation_error,
			'An element ``Origin`` of the list ``Origins`` is a variable' - instantiation_error,
			'``Statements`` is neither a variable nor a list' - type_error(list, 'Statements'),
			'``Origins`` is neither a variable nor a list' - type_error(list, 'Origins'),
			'An element ``Statement`` of the list ``Statements`` is not a valid statement' - domain_error(minizinc_statement, 'Statement'),
			'``Origins`` has an element that is neither ``unknown`` nor a valid ``source(File, Start, End)`` term, or its length differs from ``Statements``' - domain_error(minizinc_origins, 'Origins')
		]
	]).

	:- public(valid_identifier/1).
	:- mode(valid_identifier(@term), zero_or_one).
	:- info(valid_identifier/1, [
		comment is 'True if the argument is a supported, non-reserved MiniZinc identifier atom.',
		argnames is ['Name']
	]).

	:- uses(list, [
		append/3, length/2, member/2
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
		;	domain_error(minizinc_origins, Origins)
		).

	serialize_statements([], Codes, Codes).
	serialize_statements([Statement| Statements], Codes, Tail) :-
		statement_codes(Statement, Codes, Next),
		serialize_statements(Statements, Next, Tail).

	statement_codes(Statement, Codes, Tail) :-
		(	phrase(statement(Statement), Codes, Tail) ->
			true
		;	domain_error(minizinc_statement, Statement)
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
	% Top-level statements
	% -----------------------------------------------------------------------

	statement(include(Path)) -->
		!,
		{ atom(Path), atom_codes(Path, PathCodes) },
		text('include "'), string_codes(PathCodes), text('";'), [10].
	statement(decl(TypeInst, Name, Expr)) -->
		!,
		typeinst(TypeInst), [0':, 0' ], identifier(Name), initializer(Expr), [0';, 10].
	statement(array_decl(TypeInst, IndexSets, Name, Expr)) -->
		!,
		text('array['), indexsets(IndexSets), text('] of '), typeinst(TypeInst), [0':, 0' ],
		identifier(Name), initializer(Expr), [0';, 10].
	statement(constraint(Expr)) -->
		!,
		text('constraint '), expr(Expr), [0';, 10].
	statement(solve(Goal)) -->
		!,
		text('solve '), solve_goal(Goal), [0';, 10].
	statement(enum(Name, Cases)) -->
		!,
		text('enum '), identifier(Name), enum_cases(Cases), [0';, 10].
	statement(raw(Codes)) -->
		{ valid(codes, Codes) },
		codes(Codes), [10].

	initializer(none) -->
		!, [].
	initializer(Expr) -->
		text(' = '), expr(Expr).

	indexsets([Range]) -->
		!, indexset(Range).
	indexsets([Range1, Range2]) -->
		indexset(Range1), [0',], indexset(Range2).

	indexset(range(Lo, Hi)) -->
		!, range_bounds(Lo, Hi).
	indexset(id(Name)) -->
		identifier(Name).

	solve_goal(satisfy) -->
		!, text(satisfy).
	solve_goal(minimize(Expr)) -->
		!, text('minimize '), expr(Expr).
	solve_goal(maximize(Expr)) -->
		text('maximize '), expr(Expr).

	enum_cases(declared) -->
		!, [].
	enum_cases(cases([Case| Cases])) -->
		!, text(' = {'), ident_list([Case| Cases]), [0'}].
	enum_cases(anon(CountExpr)) -->
		text(' = _('), expr(CountExpr), [0')].

	ident_list([Name]) -->
		!, identifier(Name).
	ident_list([Name| Names]) -->
		identifier(Name), [0',], ident_list(Names).

	% -----------------------------------------------------------------------
	% Type-insts
	% -----------------------------------------------------------------------

	typeinst(par(Base)) -->
		!, base(Base).
	typeinst(var(Base)) -->
		text('var '), base(Base).

	base(bool) -->
		!, text(bool).
	base(int) -->
		!, text(int).
	base(float) -->
		!, text(float).
	base(string) -->
		!, text(string).
	base(set(Base)) -->
		!, text('set of '), base(Base).
	base(range(Lo, Hi)) -->
		!, range_bounds(Lo, Hi).
	base(id(Name)) -->
		identifier(Name).

	range_bounds(Lo, Hi) -->
		wrapped_expr(Lo), text('..'), wrapped_expr(Hi).

	% -----------------------------------------------------------------------
	% Expressions
	% -----------------------------------------------------------------------

	wrapped_expr(Expr) -->
		{ needs_parens(Expr) },
		!,
		[0'(], expr(Expr), [0')].
	wrapped_expr(Expr) -->
		expr(Expr).

	needs_parens(op(_, _)).
	needs_parens(op(_, _, _)).
	needs_parens(range(_, _)).
	needs_parens(if_then_else(_, _)).
	needs_parens(raw(_)).

	expr(int(Integer)) -->
		!, integer_text(Integer).
	expr(float(Float)) -->
		!, float_text(Float).
	expr(bool(Bool)) -->
		!, bool_text(Bool).
	expr(string(Codes)) -->
		!, [0'"], string_codes(Codes), [0'"].
	expr(anonymous) -->
		!, [0'_].
	expr(id(Name)) -->
		!, identifier(Name).
	expr(range(Lo, Hi)) -->
		!, range_bounds(Lo, Hi).
	expr(set(Elements)) -->
		!, [0'{], expr_list(Elements), [0'}].
	expr(array(Elements)) -->
		!, [0'[], expr_list(Elements), [0']].
	expr(array2d(Rows)) -->
		!,
		{ valid(list, Rows), rows_rectangular(Rows) },
		text('[|'), row_list(Rows), text('|]').
	expr(index(ArrayExpr, Indices)) -->
		!,
		wrapped_expr(ArrayExpr), [0'[], expr_list(Indices), [0']].
	expr(op(Operator, Left, Right)) -->
		!,
		{ binary_operator(Operator) },
		wrapped_expr(Left), [0' ], op_text(Operator), [0' ], wrapped_expr(Right).
	expr(op(Operator, Operand)) -->
		!,
		{ unary_operator(Operator) },
		op_text(Operator), [0' ], wrapped_expr(Operand).
	expr(call(Name, [])) -->
		!, identifier(Name).
	expr(call(Name, Args)) -->
		!, identifier(Name), [0'(], expr_list(Args), [0')].
	expr(comprehension(array, Head, Generators)) -->
		!, [0'[], expr(Head), text(' | '), comp_tail(Generators), [0']].
	expr(comprehension(set, Head, Generators)) -->
		!, [0'{], expr(Head), text(' | '), comp_tail(Generators), [0'}].
	expr(comprehension(call(Name), Head, Generators)) -->
		!,
		identifier(Name), [0'(], comp_tail(Generators), text(')('), expr(Head), [0')].
	expr(if_then_else(Pairs, Else)) -->
		!,
		{ Pairs = [_| _] },
		if_then_chain(Pairs), else_part(Else), text(' endif').
	expr(raw(Codes)) -->
		{ valid(codes, Codes) },
		codes(Codes).

	expr_list([]) -->
		[].
	expr_list([Expr]) -->
		!, expr(Expr).
	expr_list([Expr| Exprs]) -->
		expr(Expr), [0',], expr_list(Exprs).

	row_list([Row]) -->
		!, element_list(Row).
	row_list([Row| Rows]) -->
		element_list(Row), [0'|], row_list(Rows).

	element_list([]) -->
		[].
	element_list([Expr]) -->
		!, expr(Expr).
	element_list([Expr| Exprs]) -->
		expr(Expr), [0',], element_list(Exprs).

	rows_rectangular([]).
	rows_rectangular([Row| Rows]) :-
		length(Row, Length),
		same_length_rows(Rows, Length).

	same_length_rows([], _).
	same_length_rows([Row| Rows], Length) :-
		length(Row, Length),
		same_length_rows(Rows, Length).

	comp_tail([Generator]) -->
		!, generator(Generator).
	comp_tail([Generator| Generators]) -->
		generator(Generator), [0',], comp_tail(Generators).

	generator(generator(Names, Collection, Where)) -->
		gen_names(Names), text(' in '), expr(Collection), where_part(Where).

	gen_names([Name]) -->
		!, gen_name(Name).
	gen_names([Name| Names]) -->
		gen_name(Name), [0',], gen_names(Names).

	gen_name('_') -->
		!, [0'_].
	gen_name(Name) -->
		identifier(Name).

	where_part(none) -->
		!, [].
	where_part(Expr) -->
		text(' where '), expr(Expr).

	if_then_chain([Cond-Then| Rest]) -->
		text('if '), expr(Cond), text(' then '), expr(Then), elseif_rest(Rest).

	elseif_rest([]) -->
		[].
	elseif_rest([Cond-Then| Rest]) -->
		text(' elseif '), expr(Cond), text(' then '), expr(Then), elseif_rest(Rest).

	else_part(none) -->
		!, [].
	else_part(Expr) -->
		text(' else '), expr(Expr).

	binary_operator('<->'). binary_operator('->'). binary_operator('<-').
	binary_operator('\\/'). binary_operator(xor). binary_operator('/\\').
	binary_operator('<'). binary_operator('>'). binary_operator('<=').
	binary_operator('>='). binary_operator('=='). binary_operator('=').
	binary_operator('!='). binary_operator(in). binary_operator(subset).
	binary_operator(superset). binary_operator(union). binary_operator(diff).
	binary_operator(symdiff). binary_operator(intersect). binary_operator('++').
	binary_operator(default). binary_operator('+'). binary_operator('-').
	binary_operator('*'). binary_operator('/'). binary_operator(div).
	binary_operator(mod). binary_operator('^').

	unary_operator('+'). unary_operator('-'). unary_operator(not).

	op_text(Operator) -->
		{ atom(Operator) },
		text(Operator).

	% -----------------------------------------------------------------------
	% Lexical helpers
	% -----------------------------------------------------------------------

	integer_text(Integer) -->
		{ integer(Integer), number_codes(Integer, Codes) },
		codes(Codes).

	float_text(Float) -->
		{ float(Float), number_codes(Float, Codes) },
		codes(Codes).

	bool_text(Bool) -->
		{ once((Bool == true; Bool == false)) },
		text(Bool).

	identifier(Name) -->
		{ valid_identifier(Name), atom_codes(Name, Codes) },
		codes(Codes).

	valid_identifier(Name) :-
		atom(Name),
		Name \== '',
		\+ reserved_keyword(Name),
		atom_codes(Name, Codes),
		identifier_codes(Codes).

	identifier_codes([0'_| Codes]) :-
		!,
		Codes = [First| Rest],
		letter(First),
		name_tail(Rest).
	identifier_codes([First| Rest]) :-
		letter(First),
		name_tail(Rest).

	letter(Code) :-
		Code >= 0'a, Code =< 0'z,
		!.
	letter(Code) :-
		Code >= 0'A, Code =< 0'Z.

	name_tail([]).
	name_tail([Code| Codes]) :-
		(	letter(Code) ->
			true
		;	Code >= 0'0, Code =< 0'9 ->
			true
		;	Code =:= 0'_
		),
		name_tail(Codes).

	reserved_keyword(ann). reserved_keyword(annotation). reserved_keyword(any).
	reserved_keyword(array). reserved_keyword(bool). reserved_keyword(case).
	reserved_keyword(constraint). reserved_keyword(default). reserved_keyword(diff).
	reserved_keyword(div). reserved_keyword(else). reserved_keyword(elseif).
	reserved_keyword(endif). reserved_keyword(enum). reserved_keyword(false).
	reserved_keyword(float). reserved_keyword(function). reserved_keyword(if).
	reserved_keyword(in). reserved_keyword(include). reserved_keyword(infinity).
	reserved_keyword(int). reserved_keyword(intersect). reserved_keyword(let).
	reserved_keyword(list). reserved_keyword(maximize). reserved_keyword(minimize).
	reserved_keyword(mod). reserved_keyword(not). reserved_keyword(of).
	reserved_keyword(opt). reserved_keyword(output). reserved_keyword(par).
	reserved_keyword(predicate). reserved_keyword(record). reserved_keyword(satisfy).
	reserved_keyword(set). reserved_keyword(solve). reserved_keyword(string).
	reserved_keyword(subset). reserved_keyword(superset). reserved_keyword(symdiff).
	reserved_keyword(test). reserved_keyword(then). reserved_keyword(true).
	reserved_keyword(tuple). reserved_keyword(type). reserved_keyword(union).
	reserved_keyword(var). reserved_keyword(variant_record). reserved_keyword(where).
	reserved_keyword(xor).

	text(Atom) -->
		{ atom_codes(Atom, Codes) },
		codes(Codes).

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
	escaped_code(9) -->
		!,
		[0'\\, 0't].
	escaped_code(0'") -->
		!,
		[0'\\, 0'"].
	escaped_code(0'\\) -->
		!,
		[0'\\, 0'\\].
	escaped_code(Code) -->
		{ Code >= 32, Code =\= 127 },
		[Code].

:- end_object.
