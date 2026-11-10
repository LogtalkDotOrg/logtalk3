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


:- object(minizinc_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-10,
		comment is 'Opt-in directive expansion translating MiniZinc declaration directives into canonical statements for pure problem objects.'
	]).

	:- uses(type, [
		valid/2
	]).

	% Only recognized MiniZinc directives are translated; every other term
	% (ordinary clauses, structural and unrelated directives) is left unchanged,
	% so a problem object can still declare auxiliary runtime helper predicates.
	term_expansion((:- Directive), Expanded) :-
		logtalk_load_context(entity_relation, implements_protocol(_, minizinc_problem_protocol, _)),
		directive_statement(Directive, Statement),
		!,
		minizinc_program::serialize([Statement], _),
		origin((:- Directive), Origin),
		Expanded = minizinc_statement(Statement, Origin).

	origin(Term, Origin) :-
		(	logtalk_load_context(file, File),
			logtalk_load_context(term_position(Term), Start-End),
			Start > 0,
			End >= Start ->
			Origin = source(File, Start, End)
		;	Origin = unknown
		).

	directive_statement(include(Path), include(Path)).
	directive_statement(par(Base, Name), decl(par(Base), Name, none)).
	directive_statement(par(Base, Name, Value), decl(par(Base), Name, Value)).
	directive_statement(var(Base, Name), decl(var(Base), Name, none)).
	directive_statement(var(Base, Name, Value), decl(var(Base), Name, Value)).
	directive_statement(array_par(Base, IndexSets, Name), array_decl(par(Base), IndexSets, Name, none)).
	directive_statement(array_par(Base, IndexSets, Name, Value), array_decl(par(Base), IndexSets, Name, Value)).
	directive_statement(array_var(Base, IndexSets, Name), array_decl(var(Base), IndexSets, Name, none)).
	directive_statement(array_var(Base, IndexSets, Name, Value), array_decl(var(Base), IndexSets, Name, Value)).
	directive_statement(constraint(Expr), constraint(Expr)).
	directive_statement(solve(Goal), solve(Goal)).
	directive_statement(enum(Name), enum(Name, declared)).
	directive_statement(enum(Name, Cases), enum(Name, CanonicalCases)) :-
		enum_cases_term(Cases, CanonicalCases).
	directive_statement(raw(Source), raw(Source)).

	enum_cases_term(anon(Expr), anon(Expr)) :-
		!.
	enum_cases_term(Cases, cases(Cases)) :-
		valid(list, Cases).

:- end_object.
