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


:- object(debugger,
	implements(debuggerp)).

	:- info([
		version is 2.0,
		author is 'Paulo Moura',
		date is 2012/05/23,
		comment is 'Debugger.']).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- private(debugging_/0).
	:- dynamic(debugging_/0).

	:- private(tracing_/0).
	:- dynamic(tracing_/0).

	:- private(skipping_/0).
	:- dynamic(skipping_/0).

	:- private(spying_/2).
	:- dynamic(spying_/2).

	:- private(spying_/4).
	:- dynamic(spying_/4).

	:- private(leashing_/1).
	:- dynamic(leashing_/1).

	:- private(invocation_number_/1).
	:- dynamic(invocation_number_/1).

	:- if((current_logtalk_flag(prolog_dialect, xsb), current_logtalk_flag(threads, supported))).
		:- thread_shared(debugging_/0).
		:- thread_shared(tracing_/0).
		:- thread_shared(skipping_/0).
		:- thread_shared(spying_/2).
		:- thread_shared(spying_/4).
		:- thread_shared(leashing_/1).
		:- thread_shared(invocation_number_/1).
	:- endif.

	reset :-
		nospyall,
		leash(full),
		nodebug,
		reset_invocation_number(_).

	debug :-
		(	debugging_ ->
			write('Debugger is on: showing spy points for all objects compiled in debug mode.'), nl
		;	assertz(debugging_),
			retractall(tracing_),
			reset_invocation_number(_),
			write('Debugger switched on: showing spy points for all objects compiled in debug mode.'), nl
		).

	nodebug :-
	(	debugging_ ->
		retractall(debugging_),
		retractall(tracing_),
		write('Debugger switched off.'), nl
	;	write('Debugger is off.'), nl
	).

	suspend(Tracing) :-
		(	tracing_ ->
			Tracing = true
		;	Tracing = false
		),
		retractall(debugging_),
		retractall(tracing_).

	resume(Tracing) :-
		(	Tracing == true ->
			retractall(tracing_),
			assertz(tracing_)
		;	true
		),
		retractall(debugging_),
		assertz(debugging_).

	trace :-
		(	tracing_ ->
			write('Debugger is on: tracing everything for all objects compiled in debug mode.'), nl
		;	assertz(tracing_),
			retractall(debugging_),
			assertz(debugging_),
			reset_invocation_number(_),
			write('Debugger switched on: tracing everything for all objects compiled in debug mode.'), nl
		).

	notrace :-
		(	tracing_ ->
			retractall(tracing_),
			retractall(debugging_),
			write('Debugger switched off.'), nl
		;	write('Debugger is off.'), nl
		).

	debugging :-
		(	debugging_ ->
			write('Debugger is on: '),
			(	tracing_ ->
				write('tracing everything.'), nl
			;	write('showing spy points.'), nl
			)
		;	write('Debugger is off.'), nl
		), nl,
		(	spying_(_, _) ->
			write('Defined predicate spy points (Functor/Arity):'), nl,
			forall(
				spying_(Functor, Arity),
				(write('    '), writeq(Functor), write('/'), write(Arity), nl))
		;	write('No predicate spy points are defined.'), nl
		), nl,
		(	spying_(_, _, _, _) ->
			write('Defined context spy points (Sender, This, Self, Goal):'), nl,
			forall(
				spying_(Sender, This, Self, Goal),
				(write('    '), pretty_print_spypoint(Sender, This, Self, Goal), nl))
		;	write('No context spy points are defined.'), nl
		), nl,
		write('Leashed ports:'), nl, write('    '),
		(	leashing_(_) ->
			forall(leashing_(Port), (write(Port), write(' ')))
		;	write((none))
		),
		nl.

	debugging(Entity) :-
		(	current_object(Entity) ->
			object_property(Entity, debugging)
		;	current_protocol(Entity) ->
			protocol_property(Entity, debugging)
		;	current_category(Entity) ->
			category_property(Entity, debugging)
		;	fail
		).

	pretty_print_spypoint(Sender, This, Self, Goal) :-
		current_output(Output),
		(	var(Sender) -> write('_, ')
		;	pretty_print_vars_quoted(Output, Sender), write(', ')
		),
		(	var(This) -> write('_, ')
		;	pretty_print_vars_quoted(Output, This), write(', ')
		),
		(	var(Self) -> write('_, ')
		;	pretty_print_vars_quoted(Output, Self), write(', ')
		),
		(	var(Goal) -> write('_')
		;	pretty_print_vars_quoted(Output, Goal)
		).

	pretty_print_vars_quoted(Stream, Term) :-
		\+ \+ (
			numbervars(Term, 0, _),
			write_term(Stream, Term, [numbervars(true), quoted(true)])
		).

	spy(Preds) :-
		nonvar(Preds),
		spy_aux(Preds),
		write('Predicate spy points set.'), nl,
		(	debugging_ ->
			true
		;	debug
		).

	spy_aux([]).
	spy_aux([Functor/Arity| Preds]) :-
		nonvar(Functor),
		nonvar(Arity),
		(	spying_(Functor, Arity) ->
			true
		;	assertz(spying_(Functor, Arity))
		),
		spy_aux(Preds).
	spy_aux(Functor/Arity) :-
		nonvar(Functor),
		nonvar(Arity),
		(	spying_(Functor, Arity) ->
			true
		;	assertz(spying_(Functor, Arity))
		).

	nospy(Preds) :-
		nospy_aux(Preds),
		write('All matching predicate spy points removed.'), nl.

	nospy_aux(Preds) :-
		(	var(Preds) ->
			retractall(spying_(_, _))
		;	nospy_aux2(Preds)
		).

	nospy_aux2([]).
	nospy_aux2([Functor/Arity| Preds]) :-
		retractall(spying_(Functor, Arity)),
		nospy_aux2(Preds).
	nospy_aux2(Functor/Arity) :-
		retractall(spying_(Functor, Arity)).

	spy(Sender, This, Self, Goal) :-
		asserta(spying_(Sender, This, Self, Goal)),
		write('Context spy point set.'), nl,
		(	debugging_ ->
			true
		;	debug
		).

	nospy(Sender, This, Self, Goal) :-
		retractall(spying_(Sender, This, Self, Goal)),
		write('All matching context spy points removed.'), nl.

	nospyall :-
		retractall(spying_(_, _)),
		write('All predicate spy points removed.'), nl,
		retractall(spying_(_, _, _, _)),
		write('All context spy points removed.'), nl.

	leash(Value) :-
		valid_leash_value(Value, Ports),
		retractall(leashing_(_)),
		set_leash_ports(Ports),
		write('Debugger leash ports set to '), write(Ports), nl.

	set_leash_ports([]).
	set_leash_ports([Port| Ports]) :-
		assertz(leashing_(Port)),
		set_leash_ports(Ports).

	leashing_(fact).
	leashing_(rule).
	leashing_(call).
	leashing_(exit).
	leashing_(redo).
	leashing_(fail).
	leashing_(exception).

	valid_leash_value(Shorthand, Ports) :-
		atom(Shorthand),
		Shorthand \== [],
		!,
		leash_shortand_ports(Shorthand, Ports).

	valid_leash_value(Ports, Ports) :-
		nonvar(Ports),
		valid_leash_ports(Ports).

	valid_leash_ports([]).
	valid_leash_ports([Port| Ports]) :-
		nonvar(Port),
		valid_leash_port(Port),
		valid_leash_ports(Ports).

	valid_leash_port(fact).
	valid_leash_port(rule).
	valid_leash_port(call).
	valid_leash_port(exit).
	valid_leash_port(redo).
	valid_leash_port(fail).
	valid_leash_port(exception).

	leash_shortand_ports(none, []).
	leash_shortand_ports(loose, [fact, rule, call]).
	leash_shortand_ports(half, [fact, rule, call, redo]).
	leash_shortand_ports(tight, [fact, rule, call, redo, fail, exception]).
	leash_shortand_ports(full, [fact, rule, call, exit, redo, fail, exception]).

	leashing(Port, Goal, ExCtx, Code) :-
		functor(Port, PortName, _),
		leashing_(PortName) ->
		(	tracing_ ->
			Code = ' '
		;	spying(Port, Goal, ExCtx, Code),
			(	tracing_ ->
				true
			;	assertz(tracing_)
			)
		).

	spying(_, Goal, _, '+') :-
		functor(Goal, Functor, Arity),
		\+ \+ spying_(Functor, Arity),
		!.

	spying(_, Goal, ExCtx, '*') :-
		logtalk::execution_context(ExCtx, Sender, This, Self, _, _),
		\+ \+ spying_(Sender, This, Self, Goal).

	:- multifile(logtalk::debug_handler_provider/1).

	% there can only be one debug handler provided loaded at the same time;
	% the Logtalk runtime uses the logtalk::debug_handler_provider/1 hook
	% predicate for detecting multiple instances of the handler and for
	% better error reporting
	logtalk::debug_handler_provider(This) :-
		this(This).

	:- multifile(logtalk::debug_handler/2).

	logtalk::debug_handler(fact(_, Fact, N), ExCtx) :-
		(	debugging_, \+ skipping_ ->
			port(fact(N), _, Fact, _, ExCtx, Action),
			{Action}
		;	true
		).
	logtalk::debug_handler(rule(_, Head, N), ExCtx) :-
		(	debugging_, \+ skipping_ ->
			port(rule(N), _, Head, _, ExCtx, Action),
			{Action}
		;	true
		).
	logtalk::debug_handler(top_goal(Goal, TGoal), ExCtx) :-
		reset_invocation_number(_),
		logtalk::debug_handler(goal(Goal, TGoal), ExCtx).
	logtalk::debug_handler(goal(Goal, TGoal), ExCtx) :-
		inc_invocation_number(N),
		(	debugging_, \+ skipping_ ->
			(	port(call, N, Goal, _, ExCtx, CAction),
				(	(CAction == ignore; CAction == unify) ->
					true
				;	{CAction},
					catch({TGoal}, Error, exception(N, Goal, Error, ExCtx)),
					(	port(exit, N, Goal, _, ExCtx, EAction),
						{EAction}
					;	port(redo, N, Goal, _, ExCtx, RAction),
						RAction == ignore
					)
				;	retractall(skipping_),
					port(fail, N, Goal, _, ExCtx, _),
					fail
				)
			),
			retractall(skipping_)
		;	{TGoal}
		).

	exception(_, _, logtalk_debugger_aborted, _) :-
		throw(logtalk_debugger_aborted).

	exception(N, Goal, Error, ExCtx) :-
		port(exception, N, Goal, Error, ExCtx, TAction),
		(	TAction == fail ->
			fail
		;	throw(Error)
		).

	port(Port, N, Goal, Error, ExCtx, Action) :-
		debugging_,
		!,
		(	leashing(Port, Goal, ExCtx, Code) ->
			repeat,
				write(Code), write_port_name(Port), write_invocation_number(Port, N), writeq(Goal), write(' ? '),
				catch(read_single_char(Option), _, fail),
			valid_port_option(Option, Port, Code),
			do_port_option(Option, Port, Goal, Error, ExCtx, Action),
			!
		;	(	tracing_ ->
				write(' '), write_port_name(Port), write_invocation_number(Port, N), writeq(Goal), nl
			;	true
			),
			Action = true
		).

	port(_, _, _, _, _, true).

	write_invocation_number(fact(_), _) :- !.
	write_invocation_number(rule(_), _) :- !.
	write_invocation_number(_, N) :-
		write('('), write(N), write(') ').

	write_port_name(fact(N)) :-
		(	N =:= 0 ->
			write(' Fact: ')
		;	write(' Fact: (clause #'), write(N), write(') ')
		).
	write_port_name(rule(N)) :-
		(	N =:= 0 ->
			write(' Rule: ')
		;	write(' Rule: (clause #'), write(N), write(') ')
		).
	write_port_name(call) :-
		write(' Call: ').
	write_port_name(exit) :-
		write(' Exit: ').
	write_port_name(redo) :-
		write(' Redo: ').
	write_port_name(fail) :-
		write(' Fail: ').
	write_port_name(exception) :-
		write(' Exception: ').

	valid_port_option('\r', _, _) :- !.
	valid_port_option('\n', _, _) :- !.
	valid_port_option(' ', _, _) :- !.
	valid_port_option(c, _, _) :- !.
	valid_port_option(l, _, _) :- !.
	valid_port_option(s, _, _) :- !.
	valid_port_option(i, call, _) :- !.
	valid_port_option(i, redo, _) :- !.
	valid_port_option(f, call, _) :- !.
	valid_port_option(f, fact, _) :- !.
	valid_port_option(f, rule, _) :- !.
	valid_port_option(f, redo, _) :- !.
	valid_port_option(u, call, _) :- !.
	valid_port_option(n, _, _) :- !.
	valid_port_option(!, _, _) :- !.
	valid_port_option((@), _, _) :- !.
	valid_port_option(b, _, _) :- !.
	valid_port_option(a, _, _) :- !.
	valid_port_option('Q', _, _) :- !.
	valid_port_option(p, _, _) :- !.
	valid_port_option(d, _, _) :- !.
	valid_port_option(w, _, _) :- !.
	valid_port_option(x, _, _) :- !.
	valid_port_option(h, _, _) :- !.
	valid_port_option((?), _, _) :- !.
	valid_port_option((=), _, _) :- !.
	valid_port_option((*), _, ' ') :- !.
	valid_port_option((+), _, ' ') :- !.
	valid_port_option((-), _, (+)) :- !.
	valid_port_option(e, exception, _) :- !.

	do_port_option('\r', _, _, _, _, true).
	do_port_option('\n', _, _, _, _, true).
	do_port_option(' ', _, _, _, _, true).
	do_port_option(c, _, _, _, _, true).

	do_port_option(l, _, _, _, _, true) :-
		retractall(tracing_).

	do_port_option(s, call, _, _, _, true) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, redo, _, _, _, fail) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, _, _, _, _, true).

	do_port_option(i, _, _, _, _, ignore).

	do_port_option(f, _, _, _, _, fail).

	do_port_option(u, _, Goal, _, _, Result) :-
		write('  |: '),
		read(Term),
		(	Goal = Term ->
			Result = unify
		;	Result = fail
		).

	do_port_option(t, _, _, _, _, _) :-
		(	tracing_ ->
			true
		;	assertz(tracing_)
		),
		fail.

	do_port_option(n, _, _, _, _, true) :-
		nodebug.

	do_port_option((=), _, _, _, _, _) :-
		debugging,
		fail.

	do_port_option((+), _, Goal, _, _, _) :-
		(	Goal = (_ :: Pred) ->
			functor(Pred, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		spy(Functor/Arity),
		fail.

	do_port_option((-), _, Goal, _, _, true) :-
		(	Goal = (_ :: Pred) ->
			functor(Pred, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		nospy(Functor/Arity).

	do_port_option((*), _, Goal, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(GoalTemplate, Functor, Arity),
		write('  Enter a context spy point term formatted as (Sender, This, Self, Goal): '),
		read(Spypoint),
		Spypoint = (Sender, This, Self, GoalTemplate),
		spy(Sender, This, Self, GoalTemplate),
		fail.

	do_port_option((/), _, Goal, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(GoalTemplate, Functor, Arity),
		write('  Enter a context spy point term formatted as (Sender, This, Self, Goal): '),
		read(Spypoint),
		Spypoint = (Sender, This, Self, GoalTemplate),
		nospy(Sender, This, Self, GoalTemplate),
		fail.

	do_port_option(!, Port, Goal, Error, ExCtx, Action) :-
		do_port_option((@), Port, Goal, Error, ExCtx, Action).

	do_port_option((@), _, _, _, _, _) :-
		write('  ?- '),
		read(Goal),
		{once(Goal)},
		fail.

	:- if(predicate_property(break, built_in)).

	do_port_option(b, _, _, _, _, _) :-
		suspend(Tracing),
		break,
		resume(Tracing),
		fail.

	:- else.

	do_port_option(b, _, _, _, _, _) :-
		write('  break/0 not supported by the back-end Prolog compiler.'), nl,
		fail.

	:- endif.

	do_port_option(a, _, _, _, _, _) :-
		retractall(skipping_),
		throw(logtalk_debugger_aborted).

	do_port_option('Q', _, _, _, _, _) :-
		halt.

	do_port_option(p, _, Goal, _, _, _) :-
		% use the {}/1 control construct to avoid compilation warnings on
		% back-end Prolog compilers that don't provide the print/1 predicate
		write('  Current goal: '), catch({print(Goal)}, _, writeq(Goal)), nl,
		fail.

	do_port_option(d, _, Goal, _, _, _) :-
		write('  Current goal: '), write_term(Goal, [quoted(true), ignore_ops(true), numbervars(false)]), nl,
		fail.

	do_port_option(w, _, Goal, _, _, _) :-
		write('  Current goal: '), write_term(Goal, [quoted(true), ignore_ops(false), numbervars(true)]), nl,
		fail.

	do_port_option(x, _, _, _, ExCtx, _) :-
		logtalk::execution_context(ExCtx, Sender, This, Self, MetaCallCtx, Stack),
		write('  Sender:            '), writeq(Sender), nl,
		write('  This:              '), writeq(This), nl,
		write('  Self:              '), writeq(Self), nl,
		write('  Meta-call context: '), writeq(MetaCallCtx), nl,
		write('  Coinduction stack: '), writeq(Stack), nl,
		fail.

	do_port_option(e, _, _, Error, _, _) :-
		write('  Exception term: '), writeq(Error), nl,
		fail.

	do_port_option(h, _, _, _, _, _) :-
		write('  Available options are:'), nl,
		write('      c - creep (go on; you may use also the spacebar, return, or enter keys)'), nl,
		write('      l - leap (continues execution until the next spy point is found)'), nl,
		write('      s - skip (skips debugging for the current goal; only meaningful at call and redo ports)'), nl,
		write('      i - ignore (ignores goal, assumes that it succeeded; only valid at call and redo ports)'), nl,
		write('      f - fail (forces backtracking; may also be used to convert an exception into a failure)'), nl,
		write('      u - unify (reads and unifies a term with the current goal; only valid at the call port)'), nl,
		write('      n - nodebug (turns off debugging)'), nl,
		write('      ! - command (reads and executes a query)'), nl,
		write('      @ - command (reads and executes a query)'), nl,
		write('      b - break (suspends execution and starts new interpreter; type end_of_file to terminate)'), nl,
		write('      a - abort (returns to top level interpreter)'), nl,
		write('      Q - quit (quits Logtalk)'), nl,
		write('      p - print (writes current goal using print/1 if available)'), nl,
		write('      d - display (writes current goal without using operator notation)'), nl,
		write('      w - write (writes current goal quoting atoms if necessary)'), nl,
		write('      x - context (prints execution context)'), nl,
		write('      e - exception (prints exception term thrown by current goal)'), nl,
		write('      = - debugging (prints debugging information)'), nl,
		write('      * - add (adds a context spy point for current goal)'), nl,
		write('      / - remove (removes a context spy point for current goal)'), nl,
		write('      + - add (adds a predicate spy point for current goal)'), nl,
		write('      - - remove (removes a predicate spy point for current goal)'), nl,
		write('      h - help (prints this list of options)'), nl,
		write('      ? - help (prints this list of options)'), nl,
		fail.

	do_port_option((?), Port, Goal, Error, ExCtx, Action) :-
		do_port_option(h, Port, Goal, Error, ExCtx, Action).

	:- if(current_logtalk_flag(threads, supported)).

	inc_invocation_number(New) :-
		with_mutex(debbuger_invocation_number_mutex, inc_invocation_number_aux(New)).

	inc_invocation_number_aux(New) :-
		(	retract(invocation_number_(Old)) ->
			New is Old + 1,
			asserta(invocation_number_(New))
		;	% something weird happen as the previous call should never fail
			reset_invocation_number(New)
		).

	:- else.

	inc_invocation_number(New) :-
		(	retract(invocation_number_(Old)) ->
			New is Old + 1,
			asserta(invocation_number_(New))
		;	% something weird happen as the previous call should never fail
			reset_invocation_number(New)
		).

	:- endif.

	reset_invocation_number(0) :-
		retractall(invocation_number_(_)),
		asserta(invocation_number_(0)).

	:- if(current_logtalk_flag(prolog_dialect, cx)).

	read_single_char(Char) :-
		get_single_char(Code), put_code(Code), char_code(Char, Code),
		(	Code =:= 10 ->
			true
		;	nl
		).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

	read_single_char(Char) :-
		flush(user), tyi(Code), put(Code), nl, char_code(Char, Code).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

	read_single_char(Char) :-
		get_key(Code), char_code(Char, Code), nl.

	:- elif(current_logtalk_flag(prolog_dialect, lean)).

	read_single_char(Char) :-
		kbd_wait(Code),
		put_code(Code),
		nl,
		char_code(Char, Code).

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

	read_single_char(Char) :-
		flush_output, get_code(Code), char_code(Char, Code),
		(	Code =:= 10 ->
			true
		;	skip(10)
		).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

	read_single_char(Char) :-
		get_single_char(Code), put_code(Code), nl, char_code(Char, Code).

	:- else.

	read_single_char(Char) :-
		get_code(Code), char_code(Char, Code),
		(	Code =:= 10 ->
			true
		;	peek_code(10) ->	% hack to workaround the lack of built-in
			get_code(_)			% support for unbuffered character input
		;	true
		).

	:- endif.

:- end_object.
