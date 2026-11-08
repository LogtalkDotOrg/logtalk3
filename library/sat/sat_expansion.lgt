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


:- object(sat_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Opt-in equivalent CNF directive and clause expansion for pure problem objects.'
	]).

	:- uses(type, [
		valid/2
	]).

	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_relation, implements_protocol(_, sat_problem_protocol, _)),
		\+ structural_directive(Term),
		!,
		(	statement(Term, Statement) ->
			sat_program::serialize([Statement], _),
			origin(Term, Origin),
			Expanded = cnf_statement(Statement, Origin)
		;	domain_error(sat_syntax, Term)
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

	statement((:- cnf_raw(Source)), raw(Codes)) :-
		!,
		normalize_text(Source, Codes).
	statement((:- comment(Source)), comment(Codes)) :-
		!,
		normalize_text(Source, Codes).
	statement((:- _), _) :-
		!,
		fail.
	statement((_ :- _), _) :-
		!,
		fail.
	statement(clause(Literals), clause(Normalized)) :-
		!,
		(	valid(list, Literals) ->
			normalize_literals(Literals, Normalized)
		;	domain_error(sat_clause, Literals)
		).
	statement(Fact, clause([pos(Fact)])) :-
		callable(Fact).

	normalize_literals([], []).
	normalize_literals([pos(Term)| Literals], [pos(Term)| Normalized]) :-
		!,
		normalize_literals(Literals, Normalized).
	normalize_literals([neg(Term)| Literals], [neg(Term)| Normalized]) :-
		!,
		normalize_literals(Literals, Normalized).
	normalize_literals([Term| Literals], [pos(Term)| Normalized]) :-
		normalize_literals(Literals, Normalized).

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

:- end_object.
