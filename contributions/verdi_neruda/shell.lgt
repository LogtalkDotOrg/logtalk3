%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright (c) 2010, Victor Lagerkvist
%  SPDX-License-Identifier: BSD-3-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(shell(_Interpreters_)).

	:- info([
		version is 1:1:0,
		author is 'Victor Lagerkvist and Paulo Moura',
		date is 2019-03-20,
		comment is 'Prolog shell for the interpreters.',
		parnames is ['Interpreters']
	]).

	:- public(init/0).

	init :-
		write('Welcome, noble adventurer, your destiny awaits you!'), nl,
		write('Type "help." for online help.'), nl,
		repl.

	repl :-
		write('>> '),
		flush_output,
		read_term(Command, [variable_names(VarNames)]),
		user_reply(Command, VarNames),
		(	Command == exit ->
			!
		;	repl
		).
	repl :-
		write('no'), nl,
		flush_output,
		repl.

	user_reply(Command, VarNames) :-
		(	functor(Command, prove, _) ->
			dispatch(Command, VarNames),
			write('Type "m." for more solutions or "e." to end proof:'), nl,
			flush_output,
			read(Reply),
			(	Reply == m ->
				fail
			;	!
			)
		;	dispatch(Command, VarNames)
		).

	command(exit, 'Exit Verdi-Neruda, returning to the Prolog top-level.').
	command(halt, 'Shuts down the Prolog system.').
	command(help, 'Prints this message.').
	command(load('Database'), 'Loads the specified database.').
	command(listing('Database'), 'Lists a currently loaded database.').
	command(programs('Database'), 'Prints the currently loaded database predicates.').
	command(databases, 'Prints a list of the available databases.').
	command(interpreters, 'Prints a list of the available meta-interpreters.').
	command(prove('Interpreter', 'Goal', 'Database'), 'Proves Goal with Interpreter using the specified Database.').
	command(prove('Interpreter', 'Goal', 'Limit', 'Database'), 'Proves Goal with Interpreter if Limit is not exceeded.').
	command(benchmark_all('FileName', 'Database'), 'Benchmarks all interpreters. Benchmarks are stored in Database as bench_goal/1 clauses.').
	command(benchmark('Interpreter', 'Goal', 'Database'), 'Benchmarks Interpreter with respect to Goal and prints the number of inferences.').
	:- if(predicate_property(statistics(_,_), built_in)).
		command(benchmark_all('FileName', 'Statistic', 'N', 'Database'), 'Benchmarks all interpreters with Statistic N times. Benchmarks are stored in the database as bench_goal/1 facts or rules.').
		command(benchmark('Interpreter', 'Statistic', 'N', 'Goal', 'Database'), 'Benchmarks Interpreter with respect to Statistic, N and Goal.').
	:- endif.

	dispatch(exit, _).
	dispatch(halt, _) :-
		halt.
	dispatch(help, _) :-
		write_help_message.
	dispatch(load(Database), _) :-
		load_database(Database, rule_expansion(production)).
	dispatch(listing(Database), _) :-
		findall(rule(Head, Body),
			    (
				 Database::rule(Head, Body),
				 numbervars(rule(Head, Body), 0, _)
				),
			   Rules),
		meta::map(write_rule, Rules).
	dispatch(programs(Database), _) :-
		findall(
			Functor/Arity,
			(Database::rule(Head, _), functor(Head, Functor, Arity)),
			Functors),
		list::sort(Functors, SortedFunctors),
		meta::map(writeln, SortedFunctors).
	dispatch(databases, _) :-
		findall(Database, implements_protocol(Database, databasep), Databases),
		meta::map(writeln, Databases).
	dispatch(interpreters, _) :-
		this(shell(Interpreters0)),
		pairs::keys(Interpreters0, Interpreters),
		numbervars(Interpreters, 0, _),
		meta::map(writeln, Interpreters).
	dispatch(prove(Interpreter, Goal, Database), VarNames) :-
		valid_interpreter(Interpreter, Expander),
		load_database(Database, Expander),
		prove(Interpreter, Goal, Database, VarNames).
	dispatch(prove(Interpreter, Goal, Limit, Database), VarNames) :-
		valid_interpreter(Interpreter, Expander),
		load_database(Database, Expander),
		prove(Interpreter, Goal, Limit, Database, VarNames).

	:- if(predicate_property(statistics(_,_), built_in)).

		dispatch(benchmark_all(Name, Statistic, N, Database), _) :-
			open(Name, append, Stream),
			(	this(shell(Interpreters)),
				list::member(Interpreter-_, Interpreters),
				valid_interpreter(Interpreter, Expander),
				nl(Stream),
				write(Stream, Interpreter),
				write(Stream, ':'),
				Database::bench_goal(Goal), %Assumes a set of bench_goal/1 clauses in the database.
				load_database(Database, Expander),
				write_benchmark(Stream, Interpreter, Statistic, N, Goal, Database),
				fail
			;	write('Done.'), nl,
				close(Stream)
			).

	:- endif.

	dispatch(benchmark_all(Name, Database), _) :-
		open(Name, append, Stream),
		(	this(shell(Interpreters)),
			list::member(Interpreter-_, Interpreters),
			valid_interpreter(Interpreter, Expander),
			nl(Stream),
			write(Stream, Interpreter),
			write(Stream, ':'),
			Database::bench_goal(Goal), %Assumes a set of bench_goal/1 clauses in the database.
			load_database(Database, Expander),
			write_benchmark(Stream, Interpreter, Goal, Database),
			fail
		;	write('Done.'), nl,
			close(Stream)
		).

	:- if(predicate_property(statistics(_,_), built_in)).

		dispatch(benchmark(Interpreter, Statistic, N, Goal, Database), _) :-
			valid_interpreter(Interpreter, Expander),
			load_database(Database, Expander),
			(	benchmark(Interpreter, Statistic, N, Goal, Res0, Database)
			;	benchmark_failure(Interpreter, Statistic, N, Goal, Res0, Database),
				write('(failure) ')
			),
			write(Statistic), write(': '),
			Res is Res0/N,
			write(Res), nl.

	:- endif.

	dispatch(benchmark(Interpreter, Goal, Database), _) :-
		valid_interpreter(Interpreter, Expander),
		load_database(Database, Expander),
		current_output(Stream),
		write(Stream, Interpreter),
		write_benchmark(Stream, Interpreter, Goal, Database),
		nl.
	dispatch((Goal, Goals), VarNames) :-
		dispatch(Goal, VarNames),
		dispatch(Goals, VarNames).
	dispatch(Goal, VarNames) :-
		prove(dfs_interpreter, Goal, demodb, VarNames).

	:- if(predicate_property(statistics(_,_), built_in)).

		benchmark(_, _, 0, _, 0, _) :- !.
		benchmark(Interpreter, Statistic, N, Goal, Res, Database) :-
			N1 is N - 1,
			benchmark(Interpreter, Statistic, N1, Goal, Res0, Database),
			statistics(Statistic, Before),
			Interpreter::prove(Goal, 1000000, Database), !,
			statistics(Statistic, After),
			Res is Res0 + (After - Before).

		benchmark_failure(_, _, 0, _, 0, _) :- !.
		benchmark_failure(Interpreter, Statistic, N, Goal, Res, Database) :-
			N1 is N - 1,
			benchmark_failure(Interpreter, Statistic, N1, Goal, Res0, Database),
			statistics(Statistic, Before),
			\+ Interpreter::prove(Goal, 1000000, Database),
			statistics(Statistic, After),
			Res is Res0 + (After - Before).
	:- endif.

	benchmark(Interpreter, Goal, Inferences, Database) :-
		counter::reset,
		Interpreter::prove(Goal, 1000000, Database), !,
		counter::value(Inferences).

	benchmark_failure(Interpreter, Goal, Inferences, Database) :-
		counter::reset,
		\+ Interpreter::prove(Goal, 1000000, Database), !,
		counter::value(Inferences).

	prove(Interpreter, Goal, Database, VarNames) :-
		Interpreter::prove(Goal, Database),
		write_unifiers(VarNames).

	prove(Interpreter, Goal, Limit, Database, VarNames) :-
		Interpreter::prove(Goal, Limit, Database),
		write_unifiers(VarNames).

	write_unifiers(VarNames) :-
		unify_variable_names(VarNames),
		(	VarNames = [] ->
				write('true.'),
				nl
			;	meta::map(writeln, VarNames)
		).

	unify_variable_names([]).
	unify_variable_names([Var = Name| VarNames]) :-
		(	var(Var) ->
			Var = Name
		;	true
		),
		unify_variable_names(VarNames).

	valid_interpreter(Interpreter, Expander) :-
		this(shell(Interpreters)),
		functor(Interpreter, Functor, Arity),			% necessary for the parametric
		functor(InterpreterTemplate, Functor, Arity),	% interpreters
		list::member(InterpreterTemplate - Expander, Interpreters).

	load_database(Database, Expander) :-
		logtalk_load(Database, [hook(Expander), report(off)]).

	write_statistics(Stream, _Statistic, N, Res0) :-
		Res1 is Res0/N,
		Res is floor(Res1),
		write(Stream, Res).

	write_help_message :-
		write('Available commands are:'), nl,
		(	command(Command, Description),
			write(Command), nl, write('  '), write(Description), nl,
			fail
		;	true
		).

	write_rule(rule(Head, Body)) :-
		write(Head),
		write(' '),
		write('if'), nl,
		write_body(Body),
		nl.

	write_body([G]) :-
		!,
		write('    '),
		write(G),
		write('.').
	write_body([]) :-
		!,
		write('    '),
		write(true),
		write('.').
	write_body([G|Gs]) :-
		write('    '),
		write(G),
		write(' '),
		write('and'), nl,
		write_body(Gs).

	write_benchmark(Stream, Interpreter, Statistic, N, Goal, Database) :-
		write(Stream, ' '),
		(	benchmark(Interpreter, Statistic, N, Goal, Res, Database), !
		;	benchmark_failure(Interpreter, Statistic, N, Goal, Res, Database),
			write(Stream, '(F) ')
		),
		write(Stream, Goal),
		write(Stream, '-'),
		write_statistics(Stream, Statistic, N, Res).

	write_benchmark(Stream, Interpreter, Goal, Database) :-
		write(Stream, ' '),
		(	benchmark(Interpreter, Goal, Inferences, Database), !
		;	benchmark_failure(Interpreter,  Goal, Inferences, Database),
			write(Stream, '(F) ')
		),
		write(Stream, Goal),
		write(Stream, '-'),
		write(Stream, Inferences).

	writeln(X) :-
		write(X),
		nl.

	writeln(Stream, X) :-
		write(Stream, X),
		nl(Stream).

:- end_object.



:- object(shell).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2019-03-20,
		comment is 'User frontend to start the application.'
	]).

	:- public(welcome/0).

	welcome :-
		Version = '1.0',
		Author = 'Victor Lagerkvist',
		write('Verdi Neruda version '),
		write(Version),
		write(' by '),
		write(Author),
		write('.'), nl, nl,
		write('Type shell::start to start.'), nl, nl.

	:- public(start/0).

	start :-
		Interpreters = [
			dfs_interpreter - rule_expansion(production),
			bfs_interpreter - rule_expansion(production),
			iddfs_interpreter(_Inc) - rule_expansion(production),
			bup_interpreter - magic_expansion(production),
			a_star_interpreter(_W) - heuristic_expansion(production)
		],
		shell(Interpreters)::init.

:- end_object.
