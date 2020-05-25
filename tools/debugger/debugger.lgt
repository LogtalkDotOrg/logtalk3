%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(debugger,
	implements(debuggerp)).

	:- info([
		version is 4:3:0,
		author is 'Paulo Moura',
		date is 2020-05-25,
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

	:- private(quasi_skipping_/0).
	:- dynamic(quasi_skipping_/0).

	:- private(spying_line_number_/2).
	:- dynamic(spying_line_number_/2).

	:- private(spying_predicate_/2).
	:- dynamic(spying_predicate_/2).

	:- private(spying_context_/4).
	:- dynamic(spying_context_/4).

	:- private(leashing_/1).
	:- dynamic(leashing_/1).

	:- private(invocation_number_/1).
	:- dynamic(invocation_number_/1).

	:- private(jump_to_invocation_number_/1).
	:- dynamic(jump_to_invocation_number_/1).

	:- private(zap_to_port_/1).
	:- dynamic(zap_to_port_/1).

	:- private(write_max_depth_/1).
	:- dynamic(write_max_depth_/1).

	:- if((current_logtalk_flag(prolog_dialect, xsb), current_logtalk_flag(threads, supported))).
		:- thread_shared(debugging_/0).
		:- thread_shared(tracing_/0).
		:- thread_shared(skipping_/0).
		:- thread_shared(quasi_skipping_/0).
		:- thread_shared(spying_/2).
		:- thread_shared(spying_/4).
		:- thread_shared(leashing_/1).
		:- thread_shared(invocation_number_/1).
	:- endif.

	% we use the structured printing and question asking mechanisms to allow debugger
	% input and output to be intercepted for alternative interaction by e.g. GUI IDEs
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
		debugging_details.

	debugging_details :-
		(	spying_line_number_(_, _) ->
			findall(Entity-Line, spying_line_number_(Entity,Line), LineNumberSpyPoints),
			print_message(information, debugger, line_number_spy_points(LineNumberSpyPoints))
		;	print_message(information, debugger, no_line_number_spy_points_defined)
		),
		(	spying_predicate_(_, _) ->
			findall(Functor/Arity, spying_predicate_(Functor,Arity), PredicateSpyPoints),
			print_message(information, debugger, predicate_spy_points(PredicateSpyPoints))
		;	print_message(information, debugger, no_predicate_spy_points_defined)
		),
		(	spying_context_(_, _, _, _) ->
			findall((Sender,This,Self,Goal), spying_context_(Sender,This,Self,Goal), ContextSpyPoints),
			print_message(information, debugger, context_spy_points(ContextSpyPoints))
		;	print_message(information, debugger, no_context_spy_points_defined)
		),
		findall(Port, leashing_(Port), Ports),
		print_message(information, debugger, leashed_ports(Ports)).

	debugging(Entity) :-
		nonvar(Entity),
		!,
		(	current_object(Entity) ->
			object_property(Entity, debugging)
		;	current_protocol(Entity) ->
			protocol_property(Entity, debugging)
		;	current_category(Entity) ->
			category_property(Entity, debugging)
		;	fail
		).
	debugging(Entity) :-
		current_object(Entity),
		object_property(Entity, debugging).
	debugging(Entity) :-
		current_protocol(Entity),
		protocol_property(Entity, debugging).
	debugging(Entity) :-
		current_category(Entity),
		category_property(Entity, debugging).

	spy(SpyPoints) :-
		spy_aux(SpyPoints),
		print_message(information, debugger, all_spy_points_added),
		(	debugging_ ->
			true
		;	debug
		).

	spy_aux([]).
	spy_aux([SpyPoint| SpyPoints]) :-
		spy_list([SpyPoint| SpyPoints]).
	spy_aux(Entity-Line) :-
		spy_line_number(Entity-Line).
	spy_aux(Functor/Arity) :-
		spy_predicate(Functor/Arity).

	spy_list([]).
	spy_list([SpyPoint| SpyPoints]) :-
		spy_aux(SpyPoint),
		spy_list(SpyPoints).

	spy_line_number(Entity-Line) :-
		callable(Entity),
		integer(Line),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		(	spying_line_number_(Template, Line) ->
			true
		;	assertz(spying_line_number_(Template, Line))
		).

	spy_predicate(Functor/Arity) :-
		atom(Functor),
		integer(Arity),
		(	spying_predicate_(Functor, Arity) ->
			true
		;	assertz(spying_predicate_(Functor, Arity))
		).

	spying(Entity-Line) :-
		spying_line_number_(Entity, Line).
	spying(Functor/Arity) :-
		spying_predicate_(Functor, Arity).

	nospy(SpyPoints) :-
		nospy_aux(SpyPoints),
		print_message(information, debugger, matching_spy_points_removed).

	nospy_aux(SpyPoints) :-
		var(SpyPoints),
		!,
		nospy_line_number(_),
		nospy_predicate(_).
	nospy_aux([]).
	nospy_aux([SpyPoint| SpyPoints]) :-
		nospy_list([SpyPoint| SpyPoints]).
	nospy_aux(Entity-Line) :-
		nospy_line_number(Entity-Line).
	nospy_aux(Functor/Arity) :-
		nospy_predicate(Functor/Arity).

	nospy_list([]).
	nospy_list([SpyPoint| SpyPoints]) :-
		nospy_aux(SpyPoint),
		nospy_list(SpyPoints).

	nospy_line_number(Entity-Line) :-
		retractall(spying_line_number_(Entity, Line)).

	nospy_predicate(Functor/Arity) :-
		retractall(spying_predicate_(Functor, Arity)).

	spy(Sender, This, Self, Goal) :-
		asserta(spying_context_(Sender, This, Self, Goal)),
		print_message(information, debugger, context_spy_point_set),
		(	debugging_ ->
			true
		;	debug
		).

	spying(Sender, This, Self, Goal) :-
		spying_context_(Sender, This, Self, Goal).

	nospy(Sender, This, Self, Goal) :-
		retractall(spying_context_(Sender, This, Self, Goal)),
		print_message(comment, debugger, matching_context_spy_points_removed).

	nospyall :-
		retractall(spying_line_number_(_, _)),
		print_message(comment, debugger, all_line_number_spy_points_removed),
		retractall(spying_predicate_(_, _)),
		print_message(comment, debugger, all_predicate_spy_points_removed),
		retractall(spying_context_(_, _, _, _)),
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

	% initial default values
	leashing_(fact).
	leashing_(rule).
	leashing_(call).
	leashing_(exit).
	leashing_(redo).
	leashing_(fail).
	leashing_(exception).

	port_user_name(fact(_,_,_,_), fact).
	port_user_name(rule(_,_,_,_), rule).
	port_user_name(call, call).
	port_user_name(exit, exit).
	port_user_name(nd_exit, exit).
	port_user_name(redo, redo).
	port_user_name(fail, fail).
	port_user_name(exception, exception).

	valid_leash_value(Shorthand, Ports) :-
		atom(Shorthand),
		Shorthand \== [],
		!,
		leash_shorthand_ports(Shorthand, Ports).

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

	leash_shorthand_ports(none, []).
	leash_shorthand_ports(loose, [fact, rule, call]).
	leash_shorthand_ports(half, [fact, rule, call, redo]).
	leash_shorthand_ports(tight, [fact, rule, call, redo, fail, exception]).
	leash_shorthand_ports(full, [fact, rule, call, exit, redo, fail, exception]).

	leashing(Port) :-
		leashing_(Port).

	leashing(Port, PortUserName, N, Goal, ExCtx, Code) :-
		leashing_(PortUserName),
		(	spying_port_code(Port, Goal, ExCtx, Code) ->
			retractall(tracing_),
			assertz(tracing_)
		;	tracing_ ->
			Code = ' '
		;	retract(jump_to_invocation_number_(N)) ->
			assertz(tracing_),
			Code = ' '
		;	retract(zap_to_port_(PortUserName)) ->
			retractall(zap_to_port_(_)),
			assertz(tracing_),
			Code = ' '
		;	fail
		).

	spying_port_code(fact(Entity,_,_,Line), _, _, '#') :-
		spying_line_number_(Entity, Line),
		!.
	spying_port_code(rule(Entity,_,_,Line), _, _, '#') :-
		spying_line_number_(Entity, Line),
		!.
	spying_port_code(_, Goal, _, '+') :-
		functor(Goal, Functor, Arity),
		spying_predicate_(Functor, Arity),
		!.
	spying_port_code(_, Goal, ExCtx, '*') :-
		logtalk::execution_context(ExCtx, _, Sender, This, Self, _, _),
		spying_context_(Sender0, This0, Self0, Goal0),
		subsumes_term(sp(Sender0, This0, Self0, Goal0), sp(Sender, This, Self, Goal)),
		!.

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

	:- meta_predicate(debug_handler((::), (*))).

	debug_handler(fact(Entity,Fact,Clause,File,Line), ExCtx) :-
		invocation_number_(N),
		(	debugging_,
			(	\+ skipping_,
				\+ quasi_skipping_
			;	quasi_skipping_,
				spying_line_number_(Entity, Line)
			) ->
			port(fact(Entity,Clause,File,Line), N, Fact, _, _, ExCtx, Action),
			{Action}
		;	true
		).
	debug_handler(rule(Entity,Head,Clause,File,Line), ExCtx) :-
		invocation_number_(N),
		(	debugging_,
			(	\+ skipping_,
				\+ quasi_skipping_
			;	quasi_skipping_,
				spying_line_number_(Entity, Line)
			) ->
			port(rule(Entity,Clause,File,Line), N, Head, _, _, ExCtx, Action),
			{Action}
		;	true
		).
	debug_handler(top_goal(Goal, TGoal), ExCtx) :-
		reset_invocation_number(_),
		debug_handler(goal(Goal, TGoal), ExCtx).
	debug_handler(goal(Goal, TGoal), ExCtx) :-
		inc_invocation_number(N),
		(	debugging_,
			(	\+ skipping_,
				\+ quasi_skipping_
			;	quasi_skipping_,
				(	functor(Goal, Functor, Arity),
					spying_predicate_(Functor, Arity)
				;	logtalk::execution_context(ExCtx, _, Sender, This, Self, _, _),
					spying_context_(Sender0, This0, Self0, Goal0),
					subsumes_term(sp(Sender0, This0, Self0, Goal0), sp(Sender, This, Self, Goal))
				)
			) ->
			catch(box(N, Goal, TGoal, ExCtx), logtalk_debugger_retry, box(N, Goal, TGoal, ExCtx))
		;	{TGoal}
		).

	box(N, Goal, TGoal, ExCtx) :-
		port(call, N, Goal, TGoal, _, ExCtx, CAction),
		(	(CAction == ignore; CAction == unify) ->
			true
		;	{CAction},
			catch(call_goal(TGoal, Deterministic), Error, exception(N, Goal, TGoal, Error, ExCtx)),
			(	Deterministic == true ->
				!,
				port(exit, N, Goal, TGoal, _, ExCtx, EAction),
				{EAction}
			;	(	port(nd_exit, N, Goal, TGoal, _, ExCtx, EAction),
					{EAction}
				;	port(redo, N, Goal, TGoal, _, ExCtx, RAction),
					RAction == ignore
				)
			)
		;	retractall(skipping_),
			retractall(quasi_skipping_),
			port(fail, N, Goal, TGoal, _, ExCtx, _),
			fail
		),
		retractall(skipping_),
		retractall(quasi_skipping_).

	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == b; Dialect == qp; Dialect == swi; Dialect == yap))).

		call_goal(TGoal, Deterministic) :-
			{setup_call_cleanup(true, TGoal, Deterministic = true)}.

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == cx; Dialect == sicstus; Dialect == xsb))).

		call_goal(TGoal, Deterministic) :-
			{call_cleanup(TGoal, Deterministic = true)}.

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == ciao; Dialect == gnu))).

		call_goal(TGoal, Deterministic) :-
			{call_det(TGoal, Deterministic)}.

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
			{call(TGoal)}.

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
		port_user_name(Port, PortUserName),
		(	leashing(Port, PortUserName, N, Goal, ExCtx, _) ->
			repeat,
				% the do_port_option/7 call can fail but still change the value of Code
				% (e.g. when adding or removing a spy point)
				leashing(Port, PortUserName, N, Goal, ExCtx, Code),
				(	write_max_depth_(MaxDepth),
					MaxDepth > 0 ->
					print_message(information, debugger, leashing_port(Code, Port, N, Goal, MaxDepth))
				;	print_message(information, debugger, leashing_port(Code, Port, N, Goal))
				),
				catch(read_single_char(Option), _, fail),
				valid_port_option(Option, PortUserName, Code),
			do_port_option(Option, Port, N, Goal, TGoal, Error, ExCtx, Action),
			!
		;	(	tracing_ ->
				(	write_max_depth_(MaxDepth),
					MaxDepth > 0 ->
					print_message(information, debugger, tracing_port(' ', Port, N, Goal, MaxDepth))
				;	print_message(information, debugger, tracing_port(' ', Port, N, Goal))
				)
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
	valid_port_option(q, _, _) :- !.
	valid_port_option(j, _, _) :- !.
	valid_port_option(z, _, _) :- !.
	valid_port_option(i, call, _) :- !.
	valid_port_option(i, redo, _) :- !.
	valid_port_option(f, call, _) :- !.
	valid_port_option(f, fact, _) :- !.
	valid_port_option(f, rule, _) :- !.
	valid_port_option(f, redo, _) :- !.
	valid_port_option(r, fail, _) :- !.
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
	valid_port_option('.', fact, _) :- !.
	valid_port_option('.', rule, _) :- !.
	valid_port_option(h, _, _) :- !.
	valid_port_option((?), _, _) :- !.
	valid_port_option((=), _, _) :- !.
	valid_port_option((*), _, ' ') :- !.
	valid_port_option((/), _, (*)) :- !.
	valid_port_option((+), _, ' ') :- !.
	valid_port_option((-), _, (+)) :- !.
	valid_port_option((#), _, ' ') :- !.
	valid_port_option(('|'), _, (#)) :- !.
	valid_port_option(e, exception, _) :- !.
	valid_port_option((<), _, _) :- !.

	do_port_option('\r', _, _, _, _, _, _, true).
	do_port_option('\n', _, _, _, _, _, _, true).
	do_port_option(' ', _, _, _, _, _, _, true).
	do_port_option(c, _, _, _, _, _, _, true).

	do_port_option(l, _, _, _, _, _, _, true) :-
		retractall(tracing_).

	do_port_option(s, rule(_,_,_,_), _, _, _, _, _, true) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, call, _, _, _, _, _, true) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, redo, _, _, _, _, _, fail) :-
		!,
		retractall(skipping_),
		assertz(skipping_).
	do_port_option(s, _, _, _, _, _, _, true).

	do_port_option(q, call, _, _, _, _, _, true) :-
		!,
		retractall(quasi_skipping_),
		assertz(quasi_skipping_).
	do_port_option(q, redo, _, _, _, _, _, fail) :-
		!,
		retractall(quasi_skipping_),
		assertz(quasi_skipping_).
	do_port_option(q, _, _, _, _, _, _, true).

	do_port_option(j, _, _, _, _, _, _, true) :-
		ask_question(question, debugger, enter_invocation_number, integer, N),
		retractall(jump_to_invocation_number_(_)),
		assertz(jump_to_invocation_number_(N)),
		retractall(tracing_).

	do_port_option(z, _, _, _, _, _, _, true) :-
		ask_question(question, debugger, enter_port_name, valid_zap_port, ZapPort),
		retractall(zap_to_port_(_)),
		(	atom(ZapPort) ->
			assertz(zap_to_port_(ZapPort))
		;	ZapPort = -Port,
			(	valid_leash_port(OtherPort),
				OtherPort \== Port,
				assertz(zap_to_port_(OtherPort)),
				fail
			;	true
			)
		),
		retractall(tracing_).

	do_port_option(i, _, _, _, _, _, _, ignore).

	do_port_option(f, _, _, _, _, _, _, fail).

	do_port_option(r, _, _, _, _, _, _, _) :-
		throw(logtalk_debugger_retry).

	do_port_option(u, _, _, Goal, _, _, _, Result) :-
		ask_question(question, debugger, enter_goal, callable, Term),
		(	Goal = Term ->
			Result = unify
		;	Result = fail
		).

	do_port_option(t, _, _, _, _, _, _, _) :-
		(	tracing_ ->
			true
		;	assertz(tracing_)
		),
		fail.

	do_port_option(n, _, _, _, _, _, _, true) :-
		nodebug.

	do_port_option((=), _, _, _, _, _, _, _) :-
		debugging_details,
		fail.

	do_port_option((+), _, _, Goal, _, _, _, _) :-
		(	Goal = (_ :: Predicate) ->
			functor(Predicate, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		spy_predicate(Functor/Arity),
		print_message(information, debugger, predicate_spy_point_added),
		fail.

	do_port_option((-), _, _, Goal, _, _, _, _) :-
		(	Goal = (_ :: Predicate) ->
			functor(Predicate, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		nospy_predicate(Functor/Arity),
		print_message(information, debugger, predicate_spy_point_removed),
		fail.

	do_port_option((#), Port, _, _, _, _, _, _) :-
		(	Port = fact(Entity, _, _, Line) ->
			true
		;	Port = rule(Entity, _, _, Line)
		),
		spy_line_number(Entity-Line),
		print_message(information, debugger, line_number_spy_point_added),
		fail.

	do_port_option(('|'), Port, _, _, _, _, _, _) :-
		(	Port = fact(Entity, _, _, Line) ->
			true
		;	Port = rule(Entity, _, _, Line)
		),
		nospy_line_number(Entity-Line),
		print_message(information, debugger, line_number_spy_point_removed),
		fail.

	do_port_option((*), _, _, Goal, _, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(Template, Functor, Arity),
		ask_question(question, debugger, enter_context_spy_point(Template), '='((Sender,This,Self,Template)), (Sender,This,Self,Template)),
		spy(Sender, This, Self, Template),
		fail.

	do_port_option((/), _, _, Goal, _, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(Template, Functor, Arity),
		ask_question(question, debugger, enter_context_spy_point(Template), '='((Sender,This,Self,Template)), (Sender,This,Self,Template)),
		nospy(Sender, This, Self, Template),
		fail.

	do_port_option(!, Port, N, Goal, TGoal, Error, ExCtx, Action) :-
		do_port_option((@), Port, N, Goal, TGoal, Error, ExCtx, Action).

	do_port_option((@), _, _, _, _, _, _, _) :-
		ask_question(question, debugger, enter_query, callable, Goal),
		{once(Goal)},
		fail.

	do_port_option(a, _, _, _, _, _, _, _) :-
		retractall(skipping_),
		retractall(quasi_skipping_),
		throw(logtalk_debugger_aborted).

	do_port_option('Q', _, _, _, _, _, _, _) :-
		halt.

	do_port_option(p, _, _, Goal, _, _, _, _) :-
		print_message(information, debugger, print_current_goal(Goal)),
		fail.

	do_port_option(d, _, _, Goal, _, _, _, _) :-
		(	write_max_depth_(MaxDepth),
			MaxDepth > 0 ->
			print_message(information, debugger, display_current_goal(Goal,MaxDepth))
		;	print_message(information, debugger, display_current_goal(Goal))
		),
		fail.

	do_port_option(w, _, _, Goal, _, _, _, _) :-
		(	write_max_depth_(MaxDepth),
			MaxDepth > 0 ->
			print_message(information, debugger, write_current_goal(Goal,MaxDepth))
		;	print_message(information, debugger, write_current_goal(Goal))
		),
		fail.

	:- if((current_logtalk_flag(prolog_dialect,Dialect), Dialect \== b, Dialect \== cx, Dialect \== ji, Dialect \== lean)).

	do_port_option((<), _, _, _, _, _, _, _) :-
		ask_question(question, debugger, enter_write_max_depth, '=<'(0), N),
		retractall(write_max_depth_(_)),
		assertz(write_max_depth_(N)),
		fail.

	:- else.

	do_port_option((<), _, _, _, _, _, _, _) :-
		print_message(warning, debugger, max_depth_not_supported),
		fail.

	:- endif.

	do_port_option('$', _, _, Goal, TGoal0, _, _, _) :-
		(	Goal == ! ->
			% when the user goal is !/0, the compiled form in the debug
			% event is true/0 to avoid calling two cuts in a row; see
			% the compiler internal documentation for further details
			TGoal = !
		;	TGoal = TGoal0
		),
		(	write_max_depth_(MaxDepth),
			MaxDepth > 0 ->
			print_message(information, debugger, write_compiled_goal(TGoal,MaxDepth))
		;	print_message(information, debugger, write_compiled_goal(TGoal))
		),
		fail.

	do_port_option(x, _, _, _, _, _, ExCtx, _) :-
		logtalk::execution_context(ExCtx, Entity, Sender, This, Self, MetaCallCtx, Stack),
		print_message(information, debugger, execution_context(Entity,Sender,This,Self,MetaCallCtx,Stack)),
		fail.

	do_port_option('.', Port, _, Goal, _, _, _, _) :-
		(	Port = fact(Entity, Clause, File, Line) ->
			true
		;	Port = rule(Entity, Clause, File, Line)
		),
		(	current_object(Entity) ->
			true
		;	current_category(Entity) ->
			true
		;	current_protocol(Entity) ->
			true
		;	% assume a Prolog module, for which there isn't a portable
			% solution for retrieving source file information
			fail
		),
		(	Goal = (Other::Predicate) ->
			% clause for an entity multifile predicate
			functor(Predicate, Functor, Arity),
			print_message(information, debugger, file_context(File,Line,Entity,Other::Functor/Arity,Clause))
		;	Goal = ':'(Other,Predicate) ->
			% clause for a module multifile predicate
			functor(Predicate, Functor, Arity),
			print_message(information, debugger, file_context(File,Line,Entity,':'(Other,Functor/Arity),Clause))
		;	% clause for a local predicate
			functor(Goal, Functor, Arity),
			print_message(information, debugger, file_context(File,Line,Entity,Functor/Arity,Clause))
		),
		fail.

	do_port_option(e, _, _, _, _, Error, _, _) :-
		(	write_max_depth_(MaxDepth),
			MaxDepth > 0 ->
			print_message(information, debugger, write_exception_term(Error,MaxDepth))
		;	print_message(information, debugger, write_exception_term(Error))
		),
		fail.

	do_port_option(h, _, _, _, _, _, _, _) :-
		print_message(information, debugger, condensed_help),
		fail.

	do_port_option((?), _, _, _, _, _, _, _) :-
		print_message(information, debugger, extended_help),
		fail.

	:- if(predicate_property(break, built_in)).

	do_port_option(b, _, _, _, _, _, _, _) :-
		suspend(Tracing),
		{break},
		resume(Tracing),
		fail.

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

	:- else.

	do_port_option(b, _, _, _, _, _, _, _) :-
		print_message(warning, debugger, break_not_supported),
		fail.

	:- endif.

	valid_zap_port(ZapPort) :-
		callable(ZapPort),
		(	valid_leash_port(ZapPort)
		;	ZapPort = -Port,
			valid_leash_port(Port)
		).

	:- synchronized([
		inc_invocation_number/1, reset_invocation_number/1
	]).

	inc_invocation_number(New) :-
		(	retract(invocation_number_(Old)) ->
			New is Old + 1,
			asserta(invocation_number_(New))
		;	% something weird happen as the previous call should never fail
			reset_invocation_number(New)
		).

	reset_invocation_number(0) :-
		retractall(invocation_number_(_)),
		asserta(invocation_number_(0)),
		retractall(jump_to_invocation_number_(_)).

	:- if(current_logtalk_flag(prolog_dialect, cx)).

		read_single_char(Char) :-
			{get_single_char(Code)}, put_code(Code), char_code(Char, Code),
			(	Code =:= 10 ->
				true
			;	nl
			).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		read_single_char(Char) :-
			flush(user), tyi(Code), put_code(Code), nl, char_code(Char, Code).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		read_single_char(Char) :-
			get_key(Code), char_code(Char, Code), nl.

	:- elif(current_logtalk_flag(prolog_dialect, ji)).

		read_single_char(Char) :-
			get_code(Code),
			(	Code =:= -1 ->
				put_code(10), Char = '\n'
			;	put_code(Code), char_code(Char, Code), nl
			).

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
			{get_single_char(Code)}, put_code(Code), nl, char_code(Char, Code).

	:- else.

		% hack to workaround the lack of built-in support for non-buffered character input
		read_single_char(Char) :-
			flush_output, get_code(Code), char_code(Char, Code),
			(	Code =:= 10 ->
				true
			;	peek_code(10) ->
				get_code(_)
			;	true
			).

	:- endif.

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, swi)).
	% add dummy meta_predicate/1 directive to avoid cluttering the make/0 analysis report
	{:- meta_predicate(':'(user,'$debugger#0.call_goal#2'(*,*,*)))}.
:- endif.
