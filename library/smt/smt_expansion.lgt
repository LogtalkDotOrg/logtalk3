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


:- object(smt_expansion,
	implements(expanding)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-11-08,
		comment is 'Opt-in statement expansion for pure problem objects, converting canonical statement facts and the ``raw/1`` and ``comment/1`` directives into ``smt_statement/2`` facts.'
	]).

	term_expansion(Term, Expanded) :-
		logtalk_load_context(entity_relation, implements_protocol(_, smt_problem_protocol, _)),
		\+ structural_directive(Term),
		!,
		(	statement(Term, Statement) ->
			smt_program::serialize([Statement], _),
			origin(Term, Origin),
			Expanded = smt_statement(Statement, Origin)
		;	domain_error(smt_syntax, Term)
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

	statement((:- raw(Source)), raw(Source)) :-
		!.
	statement((:- comment(Source)), comment(Source)) :-
		!.
	statement((:- _), _) :-
		!,
		fail.
	statement((_ :- _), _) :-
		!,
		fail.
	statement(Statement, Statement) :-
		callable(Statement).

:- end_object.
