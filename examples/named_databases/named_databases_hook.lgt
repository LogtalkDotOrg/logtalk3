%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(named_databases_hook,
	implements(expanding)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2016/03/03,
		comment is 'Hook object for the named database predicates.'
	]).

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
		goal_expansion(db_once(Database, Goal), once('@'(Goal, Database))).
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
		goal_expansion(db_once(Database, Goal), once(Database:Goal)).
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
		goal_expansion(db_once(Database, Goal), once(Database:Goal)).
		goal_expansion(db_listing(Database), listing(Database:_)).
		goal_expansion(db_load(Database, File), Database:reconsult(File)).

	:- else.

		:- initialization((write('WARNING: named databases not supported on this back-end Prolog compiler!'), nl)).

	:- endif.

:- end_object.
