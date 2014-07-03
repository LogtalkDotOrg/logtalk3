%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
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
		date is 2014/07/03,
		comment is 'Command-line debugger based on an extended procedure box model supporting execution tracing and spy points.'
	]).

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

	% we use the structured printing mechanism in order to allow debugger
	% messages to be intercepted for alternative interaction by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3,
		ask_question/5
	]).

	reset :-
		nospyall,
		leash(full),
		nodebug,
		reset_invocation_number(_).

	debug :-
		(	debugging_ ->
			print_message(comment, debugger, debugger_on_spying)
		;	assertz(debugging_),
			retractall(tracing_),
			reset_invocation_number(_),
			print_message(comment, debugger, debugger_switched_on_spying)
		).

	nodebug :-
		(	debugging_ ->
			retractall(debugging_),
			retractall(tracing_),
			print_message(comment, debugger, debugger_switched_off)
		;	print_message(comment, debugger, debugger_off)
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
			print_message(comment, debugger, debugger_on_tracing)
		;	assertz(tracing_),
			retractall(debugging_),
			assertz(debugging_),
			reset_invocation_number(_),
			print_message(comment, debugger, debugger_switched_on_tracing)
		).

	notrace :-
		(	tracing_ ->
			retractall(tracing_),
			retractall(debugging_),
			print_message(comment, debugger, debugger_switched_off)
		;	print_message(comment, debugger, debugger_off)
		).

	debugging :-
		(	debugging_ ->
			(	tracing_ ->
				print_message(information, debugger, debugger_on_tracing)
			;	print_message(information, debugger, debugger_on_spying)
			)
		;	print_message(information, debugger, debugger_off)
		),
		(	spying_(_, _) ->
			findall(Functor/Arity, spying_(Functor,Arity), PredicateSpyPoints),
			print_message(information, debugger, predicate_spy_points(PredicateSpyPoints))
		;	print_message(information, debugger, no_predicate_spy_points_defined)
		),
		(	spying_(_, _, _, _) ->
			findall((Sender,This,Self,Goal), spying_(Sender,This,Self,Goal), ContextSpyPoints),
			print_message(information, debugger, context_spy_points(ContextSpyPoints))
		;	print_message(information, debugger, no_context_spy_points_defined)
		),
		findall(Port, leashing_(Port), Ports),
		print_message(information, debugger, leashed_ports(Ports)).

	debugging(Entity) :-
		(	current_object(Entity) ->
			object_property(Entity, debugging)
		;	current_protocol(Entity) ->
			protocol_property(Entity, debugging)
		;	current_category(Entity) ->
			category_property(Entity, debugging)
		;	fail
		).

	spy(Preds) :-
		nonvar(Preds),
		spy_aux(Preds),
		print_message(information, debugger, predicate_spy_point_set),
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
		print_message(information, debugger, matching_predicate_spy_points_removed).

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
		print_message(information, debugger, context_spy_point_set),
		(	debugging_ ->
			true
		;	debug
		).

	nospy(Sender, This, Self, Goal) :-
		retractall(spying_(Sender, This, Self, Goal)),
		print_message(comment, debugger, matching_context_spy_points_removed).

	nospyall :-
		retractall(spying_(_, _)),
		print_message(comment, debugger, all_predicate_spy_points_removed),
		retractall(spying_(_, _, _, _)),
		print_message(comment, debugger, all_context_spy_points_removed).

	leash(Value) :-
		valid_leash_value(Value, Ports),
		retractall(leashing_(_)),
		set_leash_ports(Ports),
		print_message(comment, debugger, leashed_ports(Ports)).

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
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		% Qu-Prolog and XSB don't support static multifile predicates
		:- dynamic(logtalk::debug_handler_provider/1).
	:- endif.

	% there can only be one debug handler provider loaded at the same time;
	% the Logtalk runtime uses the logtalk::debug_handler_provider/1 hook
	% predicate for detecting multiple instances of the handler and for
	% better error reporting
	logtalk::debug_handler_provider(debugger).

	:- multifile(logtalk::debug_handler/2).
	:- if((current_logtalk_flag(prolog_dialect, qp); current_logtalk_flag(prolog_dialect, xsb))).
		% Qu-Prolog and XSB don't support static multifile predicates
		:- dynamic(logtalk::debug_handler/2).
	:- endif.

	logtalk::debug_handler(Event, ExCtx) :-
		debug_handler(Event, ExCtx).

	debug_handler(fact(_, Fact, N), ExCtx) :-
		(	debugging_, \+ skipping_ ->
			port(fact(N), _, Fact, _, _, ExCtx, Action),
			{Action}
		;	true
		).
	debug_handler(rule(_, Head, N), ExCtx) :-
		(	debugging_, \+ skipping_ ->
			port(rule(N), _, Head, _, _, ExCtx, Action),
			{Action}
		;	true
		).
	debug_handler(top_goal(Goal, TGoal), ExCtx) :-
		reset_invocation_number(_),
		debug_handler(goal(Goal, TGoal), ExCtx).
	debug_handler(goal(Goal, TGoal), ExCtx) :-
		inc_invocation_number(N),
		(	debugging_, \+ skipping_ ->
			(	port(call, N, Goal, TGoal, _, ExCtx, CAction),
				(	(CAction == ignore; CAction == unify) ->
					true
				;	{CAction},
					catch(call_goal(TGoal, Deterministic), Error, exception(N, Goal, TGoal, Error, ExCtx)),
					(	Deterministic == true ->
						!,
						port(exit, N, Goal, TGoal, _, ExCtx, EAction),
						{EAction}
					;	(	port(exit, N, Goal, TGoal, _, ExCtx, EAction),
							{EAction}
						;	port(redo, N, Goal, TGoal, _, ExCtx, RAction),
							RAction == ignore
						)
					)
				;	retractall(skipping_),
					port(fail, N, Goal, TGoal, _, ExCtx, _),
					fail
				)
			),
			retractall(skipping_)
		;	{TGoal}
		).

	:- if((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == b; Dialect == qp; Dialect == swi; Dialect == yap)
	)).

		call_goal(TGoal, Deterministic) :-
			{setup_call_cleanup(true, TGoal, Deterministic = true)}.

	:- elif((	current_logtalk_flag(prolog_dialect, Dialect),
			(Dialect == cx; Dialect == sicstus; Dialect == xsb)
	)).

		call_goal(TGoal, Deterministic) :-
			{call_cleanup(TGoal, Deterministic = true)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		call_goal(TGoal, Deterministic) :-
			{call_det(TGoal, Deterministic0)},
			(	Deterministic0 == true ->
				Deterministic = Deterministic0
			;	true
			).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		call_goal(TGoal, Deterministic) :-
			{sepia_kernel:get_cut(Before),
			 call(TGoal),
			 sepia_kernel:get_cut(After)},
			(	Before == After ->
				!,
				Deterministic = true
			;	true
			).

	:- else.

		call_goal(TGoal, _) :-
			{TGoal}.

	:- endif.

	exception(_, _, _, logtalk_debugger_aborted, _) :-
		throw(logtalk_debugger_aborted).

	exception(N, Goal, TGoal, Error, ExCtx) :-
		port(exception, N, Goal, TGoal, Error, ExCtx, TAction),
		(	TAction == fail ->
			fail
		;	throw(Error)
		).

	port(Port, N, Goal, TGoal, Error, ExCtx, Action) :-
		debugging_,
		!,
		(	leashing(Port, Goal, ExCtx, Code) ->
			repeat,
				print_message(information, debugger, leashing_port(Code, Port, N, Goal)),
				catch(read_single_char(Option), _, fail),
			valid_port_option(Option, Port, Code),
			do_port_option(Option, Port, Goal, TGoal, Error, ExCtx, Action),
			!
		;	(	tracing_ ->
				print_message(information, debugger, tracing_port(' ', Port, N, Goal))
			;	true
			),
			Action = true
		).

	port(_, _, _, _, _, _, true).

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
	valid_port_option('$', call, _) :- !.
	valid_port_option('$', redo, _) :- !.
	valid_port_option('$', exit, _) :- !.
	valid_port_option('$', fail, _) :- !.
	valid_port_option(x, _, _) :- !.
	valid_port_option(h, _, _) :- !.
	valid_port_option((?), _, _) :- !.
	valid_port_option((=), _, _) :- !.
	valid_port_option((*), _, ' ') :- !.
	valid_port_option((+), _, ' ') :- !.
	valid_port_option((-), _, (+)) :- !.
	valid_port_option(e, exception, _) :- !.

	do_port_option('\r', _, _, _, _, _, true).
	do_port_option('\n', _, _, _, _, _, true).
	do_port_option(' ', _, _, _, _, _, true).
	do_port_option(c, _, _, _, _, _, true).

	do_port_option(l, _, _, _, _, _, true) :-
		retractall(tracing_).

	do_port_option(s, call, _, _, _, _, true) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, redo, _, _, _, _, fail) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, _, _, _, _, _, true).

	do_port_option(i, _, _, _, _, _, ignore).

	do_port_option(f, _, _, _, _, _, fail).

	do_port_option(u, _, Goal, _, _, _, Result) :-
		ask_question(question, debugger, enter_goal, callable, Term),
		(	Goal = Term ->
			Result = unify
		;	Result = fail
		).

	do_port_option(t, _, _, _, _, _, _) :-
		(	tracing_ ->
			true
		;	assertz(tracing_)
		),
		fail.

	do_port_option(n, _, _, _, _, _, true) :-
		nodebug.

	do_port_option((=), _, _, _, _, _, _) :-
		debugging,
		fail.

	do_port_option((+), _, Goal, _, _, _, _) :-
		(	Goal = (_ :: Pred) ->
			functor(Pred, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		spy(Functor/Arity),
		fail.

	do_port_option((-), _, Goal, _, _, _, true) :-
		(	Goal = (_ :: Pred) ->
			functor(Pred, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		nospy(Functor/Arity).

	do_port_option((*), _, Goal, _, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(Template, Functor, Arity),
		ask_question(question, debugger, enter_context_spy_point(Template), '='((Sender,This,Self,Template)), (Sender,This,Self,Template)),
		spy(Sender, This, Self, Template),
		fail.

	do_port_option((/), _, Goal, _, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(Template, Functor, Arity),
		ask_question(question, debugger, enter_context_spy_point(Template), '='((Sender,This,Self,Template)), (Sender,This,Self,Template)),
		nospy(Sender, This, Self, Template),
		fail.

	do_port_option(!, Port, Goal, TGoal, Error, ExCtx, Action) :-
		do_port_option((@), Port, Goal, TGoal, Error, ExCtx, Action).

	do_port_option((@), _, _, _, _, _, _) :-
		ask_question(question, debugger, enter_query, callable, Goal),
		{once(Goal)},
		fail.

	:- if(predicate_property(break, built_in)).

	do_port_option(b, _, _, _, _, _, _) :-
		suspend(Tracing),
		break,
		resume(Tracing),
		fail.

	:- else.

	do_port_option(b, _, _, _, _, _, _) :-
		print_message(warning, debugger, break_not_supported),
		fail.

	:- endif.

	do_port_option(a, _, _, _, _, _, _) :-
		retractall(skipping_),
		throw(logtalk_debugger_aborted).

	do_port_option('Q', _, _, _, _, _, _) :-
		halt.

	do_port_option(p, _, Goal, _, _, _, _) :-
		print_message(information, debugger, print_current_goal(Goal)),
		fail.

	do_port_option(d, _, Goal, _, _, _, _) :-
		print_message(information, debugger, display_current_goal(Goal)),
		fail.

	do_port_option(w, _, Goal, _, _, _, _) :-
		print_message(information, debugger, write_current_goal(Goal)),
		fail.

	do_port_option('$', _, _, TGoal, _, _, _) :-
		print_message(information, debugger, write_compiled_goal(TGoal)),
		fail.

	do_port_option(x, _, _, _, _, ExCtx, _) :-
		logtalk::execution_context(ExCtx, Sender, This, Self, MetaCallCtx, Stack),
		print_message(information, debugger, execution_context(Sender,This,Self,MetaCallCtx,Stack)),
		fail.

	do_port_option(e, _, _, _, Error, _, _) :-
		print_message(information, debugger, write_exception_term(Error)),
		fail.

	do_port_option(h, _, _, _, _, _, _) :-
		print_message(information, debugger, help),
		fail.

	do_port_option((?), Port, Goal, TGoal, Error, ExCtx, Action) :-
		do_port_option(h, Port, Goal, TGoal, Error, ExCtx, Action).

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
