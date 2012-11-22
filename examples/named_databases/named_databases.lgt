%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
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
