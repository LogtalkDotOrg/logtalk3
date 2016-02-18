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


:- if(current_logtalk_flag(prolog_dialect, lean)).

	db_create(_).

	db_load(Database, File) :-
		db_reconsult(File, Database).

	% the other predicate are built-in predicates in Lean Prolog

:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

	db_create(Database) :-
		(	current_module(Database) ->
			true
		;	create_module(Database, [], eclipse_language)
		).

	db_dynamic(Database, Predicate) :-
		'@'(dynamic(Predicate), Database).

	db_abolish(Database, Predicate) :-
		'@'(abolish(Predicate), Database).

	db_asserta(Database, Clause) :-
		'@'(asserta(Clause), Database).

	db_assertz(Database, Clause) :-
		'@'(assertz(Clause), Database).

	db_retract(Database, Clause) :-
		'@'(retract(Clause), Database).

	db_retractall(Database, Head) :-
		'@'(retractall(Head), Database).

	db_clause(Database, Head, Body) :-
		'@'(clause(Head, Body), Database).

	db_call(Database, Goal) :-
		'@'(Goal, Database).

	db_load(Database, File) :-
		'@'(compile(File), Database).

	db_save(Database, File) :-
		open(File, write, Stream),
		current_output(Current),
		set_output(Stream),
		(	'@'(current_predicate(Predicate), Database),
			'@'(is_dynamic(Predicate), Database),
			writeq((:- dynamic(Predicate))), write('.'), nl,
			'@'(listing, Database),
			fail
		;	true
		),
		close(Stream),
		set_output(Current).

	db_clear(Database) :-
		'@'(current_predicate(Predicate), Database),
		'@'(is_dynamic(Predicate), Database),
		'@'(abolish(Predicate), Database),
		fail.
	db_clear(_).

:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	db_create(_).

	db_dynamic(Database, Functor/Arity) :-
		functor(Clause, Functor, Arity),
		assertz(Database:Clause),
		retract(Database:Clause).

	db_abolish(Database, Predicate) :-
		abolish(Database:Predicate).

	db_asserta(Database, Clause) :-
		asserta(Database:Clause).

	db_assertz(Database, Clause) :-
		assertz(Database:Clause).

	db_retract(Database, Clause) :-
		retract(Database:Clause).

	db_retractall(Database, Head) :-
		retractall(Database:Head).

	db_clause(Database, Head, Body) :-
		clause(Database:Head, Body).

	db_call(Database, Goal) :-
		Database:Goal.

	db_load(Database, File) :-
		Database:reconsult(File).

	db_save(Database, File) :-
		open(File, write, Stream),
		current_output(Current),
		set_output(Stream),
		(	current_predicate(Database:Functor/Arity),
			functor(Template, Functor, Arity),
			predicate_property(Database:Template, (dynamic)),
			writeq((:- dynamic(Functor/Arity))), write('.'), nl,
			clause(Database:Template, Body),
			(	Body == true ->
				writeq(Template), write('.'), nl
			;	writeq((Template :- Body)), write('.'), nl
			),
			fail
		;	true
		),
		close(Stream),
		set_output(Current).

	db_clear(Database) :-
		current_predicate(Database:Functor/Arity),
		functor(Template, Functor, Arity),
		predicate_property(Database:Template, (dynamic)),
		abolish(Database:Functor/Arity),
		fail.
	db_clear(_).

:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == swi; Dialect == yap))).

	db_create(_).

	db_dynamic(Database, Predicate) :-
		dynamic(Database:Predicate).

	db_abolish(Database, Predicate) :-
		abolish(Database:Predicate).

	db_asserta(Database, Clause) :-
		asserta(Database:Clause).

	db_assertz(Database, Clause) :-
		assertz(Database:Clause).

	db_retract(Database, Clause) :-
		retract(Database:Clause).

	db_retractall(Database, Head) :-
		retractall(Database:Head).

	db_clause(Database, Head, Body) :-
		clause(Database:Head, Body).

	db_call(Database, Goal) :-
		Database:Goal.

	db_load(Database, File) :-
		Database:reconsult(File).

	db_save(Database, File) :-
		open(File, write, Stream),
		current_output(Current),
		set_output(Stream),
		listing(Database:_),
		close(Stream),
		set_output(Current).

	db_clear(Database) :-
		current_predicate(Database:Predicate),
		abolish(Database:Predicate),
		fail.
	db_clear(_).

:- else.

	:- initialization((write('WARNING: named databases not supported on this back-end Prolog compiler!'), nl)).

:- endif.
