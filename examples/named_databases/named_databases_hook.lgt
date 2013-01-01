%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(named_databases_hook,
	implements(expanding)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2012/10/26,
		comment is 'Hook object for the named database predicates.']).

	:- if(current_logtalk_flag(prolog_dialect, lean)).

		goal_expansion(db_create(_), true).
		goal_expansion(db_load(Database, File), db_reconsult(File, Database)).
		% the other predicate are built-in predicates in Lean Prolog

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		goal_expansion(db_create(Database), (current_module(Database) -> true; create_module(Database, [], eclipse_language))).
		goal_expansion(db_dynamic(Database, Predicates), '@'(dynamic(Predicates), Database)).
		goal_expansion(db_abolish(Database, Predicate), '@'(abolish(Predicate), Database)).
		goal_expansion(db_asserta(Database, Clause), '@'(asserta(Clause), Database)).
		goal_expansion(db_assertz(Database, Clause), '@'(assertz(Clause), Database)).
		goal_expansion(db_retract(Database, Clause), '@'(retract(Clause), Database)).
		goal_expansion(db_retractall(Database, Head), '@'(retractall(Head), Database)).
		goal_expansion(db_clause(Database, Head, Body), '@'(clause(Head, Body), Database)).
		goal_expansion(db_call(Database, Goal), '@'(Goal, Database)).
		goal_expansion(db_load(Database, File), '@'(compile(File), Database)).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		goal_expansion(db_create(_), true).
		goal_expansion(db_abolish(Database, Predicate), abolish(Database:Predicate)).
		goal_expansion(db_asserta(Database, Clause), asserta(Database:Clause)).
		goal_expansion(db_assertz(Database, Clause), assertz(Database:Clause)).
		goal_expansion(db_retract(Database, Clause), retract(Database:Clause)).
		goal_expansion(db_retractall(Database, Head), retractall(Database:Head)).
		goal_expansion(db_clause(Database, Head, Body), clause(Database:Head, Body)).
		goal_expansion(db_call(Database, Goal), Database:Goal).
		goal_expansion(db_load(Database, File), Database:reconsult(File)).

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

		goal_expansion(db_create(_), true).
		goal_expansion(db_dynamic(Database, Predicates), dynamic(Database:Predicates)).
		goal_expansion(db_abolish(Database, Predicate), abolish(Database:Predicate)).
		goal_expansion(db_asserta(Database, Clause), asserta(Database:Clause)).
		goal_expansion(db_assertz(Database, Clause), assertz(Database:Clause)).
		goal_expansion(db_retract(Database, Clause), retract(Database:Clause)).
		goal_expansion(db_retractall(Database, Head), retractall(Database:Head)).
		goal_expansion(db_clause(Database, Head, Body), clause(Database:Head, Body)).
		goal_expansion(db_call(Database, Goal), Database:Goal).
		goal_expansion(db_load(Database, File), Database:reconsult(File)).

	:- else.

		:- initialization((write('WARNING: named databases not supported on this back-end Prolog compiler!'), nl)).

	:- endif.

:- end_object.
