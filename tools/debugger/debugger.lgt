%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(debugger,
	implements(debuggerp)).

	:- info([
		version is 8:0:0,
		author is 'Paulo Moura',
		date is 2025-12-18,
		comment is 'Command-line debugger based on an extended procedure box model supporting execution tracing and spy points.'
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- private(debugging_/0).
	:- dynamic(debugging_/0).
	:- mode(debugging_, zero_or_one).
	:- info(debugging_/0, [
		comment is 'True iff debug is on.'
	]).

	:- private(tracing_/0).
	:- dynamic(tracing_/0).
	:- mode(tracing_, zero_or_one).
	:- info(tracing_/0, [
		comment is 'True iff tracing is on.'
	]).

	:- private(explicit_tracing_/0).
	:- dynamic(explicit_tracing_/0).
	:- mode(explicit_tracing_, zero_or_one).
	:- info(explicit_tracing_/0, [
		comment is 'True iff tracing is on due to a call to the trace/0 predicate.'
	]).

	:- private(skipping_/0).
	:- dynamic(skipping_/0).
	:- mode(skipping_, zero_or_one).
	:- info(skipping_/0, [
		comment is 'True iff skipping.'
	]).

	:- private(skipping_unleashed_/1).
	:- dynamic(skipping_unleashed_/1).
	:- mode(skipping_unleashed_(?integer), zero_or_one).
	:- info(skipping_unleashed_/1, [
		comment is 'True iff skipping (a goal with invocation number ``N``) but showing intermediate ports as unleashed.',
		argnames is ['N']
	]).

	:- private(quasi_skipping_/0).
	:- dynamic(quasi_skipping_/0).
	:- mode(quasi_skipping_, zero_or_one).
	:- info(quasi_skipping_/0, [
		comment is 'True iff quasi-skipping.'
	]).

	:- private(leaping_/1).
	:- dynamic(leaping_/1).
	:- mode(leaping_(?atom), zero_or_one).
	:- info(leaping_/1, [
		comment is 'True iff leaping in tracing or debugging mode.',
		argnames is ['Mode']
	]).

	:- private(leashing_/1).
	:- dynamic(leashing_/1).
	:- mode(leashing_(?atom), zero_or_more).
	:- info(leashing_/1, [
		comment is 'Table of currently leashed ports.',
		argnames is ['Port']
	]).

	:- private(invocation_number_/1).
	:- dynamic(invocation_number_/1).
	:- mode(invocation_number_(?integer), zero_or_one).
	:- info(invocation_number_/1, [
		comment is 'Current call stack invocation number.',
		argnames is ['N']
	]).

	:- private(jump_to_invocation_number_/1).
	:- dynamic(jump_to_invocation_number_/1).
	:- mode(jump_to_invocation_number_(?integer), zero_or_one).
	:- info(jump_to_invocation_number_/1, [
		comment is 'Invocation number to jump to.',
		argnames is ['N']
	]).

	:- private(zap_to_port_/1).
	:- dynamic(zap_to_port_/1).
	:- mode(zap_to_port_(?integer), zero_or_one).
	:- info(zap_to_port_/1, [
		comment is 'Port to zap to.',
		argnames is ['Port']
	]).

	:- private(write_max_depth_/1).
	:- dynamic(write_max_depth_/1).
	:- mode(write_max_depth_(?non_negative_integer), zero_or_one).
	:- info(write_max_depth_/1, [
		comment is 'Current term write maximum depth.',
		argnames is ['MaxDepth']
	]).

	:- private(log_point_/3).
	:- dynamic(log_point_/3).
	:- mode(log_point_(?object_identifier, ?integer, ?atom), zero_or_more).
	:- mode(log_point_(?category_identifier, ?integer, ?atom), zero_or_more).
	:- info(log_point_/3, [
		comment is 'Table of log points.',
		argnames is ['Entity', 'Line', 'Message']
	]).

	:- private(clause_breakpoint_/2).
	:- dynamic(clause_breakpoint_/2).
	:- mode(clause_breakpoint_(?object_identifier, ?integer), zero_or_more).
	:- mode(clause_breakpoint_(?category_identifier, ?integer), zero_or_more).
	:- info(clause_breakpoint_/2, [
		comment is 'Table of clause breakpoints.',
		argnames is ['Entity', 'Line']
	]).

	:- private(predicate_breakpoint_/3).
	:- dynamic(predicate_breakpoint_/3).
	:- mode(predicate_breakpoint_(?atom, ?integer, ?predicate_indicator), zero_or_more).
	:- mode(predicate_breakpoint_(?atom, ?integer, ?non_terminal_indicator), zero_or_more).
	:- info(predicate_breakpoint_/3, [
		comment is 'Table of predicate breakpoints.',
		argnames is ['Functor', 'Arity', 'Original']
	]).

	:- private(entity_predicate_breakpoint_/4).
	:- dynamic(entity_predicate_breakpoint_/4).
	:- mode(entity_predicate_breakpoint_(?callable, ?atom, ?integer, ?qualified_predicate_indicator), zero_or_more).
	:- mode(entity_predicate_breakpoint_(?callable, ?atom, ?integer, ?qualified_non_terminal_indicator), zero_or_more).
	:- info(entity_predicate_breakpoint_/4, [
		comment is 'Table of entity predicate breakpoints.',
		argnames is ['Entity', 'Functor', 'Arity', 'Original']
	]).

	:- private(context_breakpoint_/4).
	:- dynamic(context_breakpoint_/4).
	:- mode(context_breakpoint_(?object_identifier, ?object_identifier, ?object_identifier, ?callable), zero_or_more).
	:- info(context_breakpoint_/4, [
		comment is 'Table of context breakpoints.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- private(conditional_breakpoint_/3).
	:- dynamic(conditional_breakpoint_/3).
	:- mode(conditional_breakpoint_(?object_identifier, ?integer, ?callable), zero_or_more).
	:- mode(conditional_breakpoint_(?category_identifier, ?integer, ?callable), zero_or_more).
	:- info(conditional_breakpoint_/3, [
		comment is 'Table of conditional breakpoints.',
		argnames is ['Entity', 'Line', 'Condition']
	]).

	:- private(triggered_breakpoint_/4).
	:- dynamic(triggered_breakpoint_/4).
	:- mode(triggered_breakpoint_(?object_identifier, ?integer, ?object_identifier, ?integer), zero_or_more).
	:- mode(triggered_breakpoint_(?object_identifier, ?integer, ?category_identifier, ?integer), zero_or_more).
	:- mode(triggered_breakpoint_(?category_identifier, ?integer, ?object_identifier, ?integer), zero_or_more).
	:- mode(triggered_breakpoint_(?category_identifier, ?integer, ?category_identifier, ?integer), zero_or_more).
	:- info(triggered_breakpoint_/4, [
		comment is 'Table of defined triggered breakpoints.',
		argnames is ['Entity', 'Line', 'TriggerEntity', 'TriggerLine']
	]).

	:- private(triggered_breakpoint_enabled_/2).
	:- dynamic(triggered_breakpoint_enabled_/2).
	:- mode(triggered_breakpoint_enabled_(?object_identifier, ?integer), zero_or_more).
	:- mode(triggered_breakpoint_enabled_(?category_identifier, ?integer), zero_or_more).
	:- info(triggered_breakpoint_enabled_/2, [
		comment is 'Table of enabled triggered breakpoints.',
		argnames is ['Entity', 'Line']
	]).

	:- private(file_line_hit_count_/3).
	:- dynamic(file_line_hit_count_/3).
	:- mode(file_line_hit_count_(?atom, ?integer, ?integer), zero_or_one).
	:- info(file_line_hit_count_/3, [
		comment is 'Table of file and line hit counts (successful unifications with clause heads).',
		argnames is ['File', 'Line', 'Count']
	]).

	% we use the structured printing and question asking mechanisms to allow debugger
	% input and output to be intercepted for alternative interaction by e.g. GUI IDEs
	:- uses(logtalk, [
		print_message/3,
		ask_question/5
	]).

	:- initialization(reset_invocation_number(_)).

	reset :-
		nologall,
		nospyall,
		leash(full),
		nodebug,
		reset_invocation_number(_),
		retractall(file_line_hit_count_(_, _, _)),
		retractall(triggered_breakpoint_(_, _, _, _)),
		retractall(triggered_breakpoint_enabled_(_, _)).

	debug :-
		logtalk::activate_debug_handler(debugger),
		(	debugging_ ->
			print_message(comment, debugger, debugger_spying_on)
		;	assertz(debugging_),
			retractall(tracing_),
			reset_invocation_number(_),
			retractall(file_line_hit_count_(_, _, _)),
			print_message(comment, debugger, debugger_spying_switched_on)
		).

	nodebug :-
		(	(debugging_; leaping_(debugging)) ->
			retractall(debugging_),
			retractall(tracing_),
			retractall(leaping_(_)),
			print_message(comment, debugger, debugger_switched_off)
		;	print_message(comment, debugger, debugger_off)
		),
		logtalk::deactivate_debug_handler.

	trace :-
		logtalk::activate_debug_handler(debugger),
		retractall(explicit_tracing_),
		assertz(explicit_tracing_),
		(	tracing_ ->
			print_message(comment, debugger, debugger_tracing_on)
		;	assertz(tracing_),
			retractall(debugging_),
			assertz(debugging_),
			retractall(leaping_(_)),
			reset_invocation_number(_),
			retractall(file_line_hit_count_(_, _, _)),
			print_message(comment, debugger, debugger_tracing_switched_on)
		).

	notrace :-
		retractall(explicit_tracing_),
		(	(tracing_; leaping_(tracing)) ->
			retractall(tracing_),
			retractall(leaping_(_)),
			print_message(comment, debugger, debugger_tracing_switched_off)
		;	print_message(comment, debugger, debugger_tracing_off)
		).

	debugging :-
		(	debugging_ ->
			(	explicit_tracing_ ->
				print_message(information, debugger, debugger_tracing_on)
			;	print_message(information, debugger, debugger_spying_on)
			)
		;	print_message(information, debugger, debugger_off)
		),
		debugging_details.

	debugging_details :-
		(	log_point_(_, _, _) ->
			findall(Entity-Line, log_point_(Entity,Line,_), LogPoints),
			print_message(information, debugger, log_points(LogPoints))
		;	print_message(information, debugger, no_log_points_defined)
		),
		(	conditional_breakpoint_(_, _, _) ->
			findall(bp(Entity,Line,Condition), conditional_breakpoint_(Entity,Line,Condition), ConditionalPoints),
			print_message(information, debugger, conditional_breakpoints(ConditionalPoints))
		;	print_message(information, debugger, no_conditional_breakpoints_defined)
		),
		(	triggered_breakpoint_(_, _, _, _) ->
			findall(bp(Entity,Line,TriggerEntity-TriggerLine), triggered_breakpoint_(Entity,Line,TriggerEntity,TriggerLine), TriggeredPoints),
			print_message(information, debugger, triggered_breakpoints(TriggeredPoints))
		;	print_message(information, debugger, no_triggered_breakpoints_defined)
		),
		(	clause_breakpoint_(_, _) ->
			findall(Entity-Line, clause_breakpoint_(Entity,Line), ClauseBreakpoints),
			print_message(information, debugger, clause_breakpoints(ClauseBreakpoints))
		;	print_message(information, debugger, no_clause_breakpoints_defined)
		),
		(	predicate_breakpoint_(_, _, _) ->
			findall(Predicate, predicate_breakpoint_(_,_,Predicate), PredicateBreakpoints),
			print_message(information, debugger, predicate_breakpoints(PredicateBreakpoints))
		;	print_message(information, debugger, no_predicate_breakpoints_defined)
		),
		(	entity_predicate_breakpoint_(_, _, _, _) ->
			findall(QualifiedPredicate, entity_predicate_breakpoint_(_,_,_,QualifiedPredicate), QualifiedPredicates),
			print_message(information, debugger, entity_predicate_breakpoints(QualifiedPredicates))
		;	print_message(information, debugger, no_entity_predicate_breakpoints_defined)
		),
		(	context_breakpoint_(_, _, _, _) ->
			findall((Sender,This,Self,Goal), context_breakpoint_(Sender,This,Self,Goal), ContextBreakpoints),
			print_message(information, debugger, context_breakpoints(ContextBreakpoints))
		;	print_message(information, debugger, no_context_breakpoints_defined)
		),
		findall(Port, leashing_(Port), Ports),
		print_message(information, debugger, leashed_ports(Ports)),
		(	write_max_depth_(MaxDepth) ->
			print_message(information, debugger, max_depth(MaxDepth))
		;	print_message(information, debugger, max_depth(unset))
		).

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

	% predicate and clause breakpoint predicates

	spy(Spec) :-
		(	Spec = Entity-Line ->
			spy_clause(Entity-Line),
			print_message(information, debugger, clause_breakpoint_added)
		;	Spec = Name/Arity ->
			spy_predicate(Name, Arity, Name/Arity),
			print_message(information, debugger, predicate_breakpoint_added)
		;	Spec = Name//Arity ->
			ExtArity is Arity + 2,
			spy_predicate(Name, ExtArity, Name//Arity),
			print_message(information, debugger, predicate_breakpoint_added)
		;	Spec = Entity::Name/Arity ->
			spy_entity_predicate(Entity, Name, Arity),
			print_message(information, debugger, entity_predicate_breakpoint_added)
		;	Spec = Entity::Name//Arity ->
			spy_entity_non_terminal(Entity, Name, Arity),
			print_message(information, debugger, entity_predicate_breakpoint_added)
		;	spy_list(Spec),
			print_message(information, debugger, breakpoints_added)
		),
		(	debugging_ ->
			true
		;	debug
		).

	spy_list([]).
	spy_list([SpyPoint| SpyPoints]) :-
		spy_aux(SpyPoint),
		spy_list(SpyPoints).

	spy_aux(Entity-Line) :-
		spy_clause(Entity-Line).
	spy_aux(Functor/Arity) :-
		spy_predicate(Functor, Arity, Functor/Arity).
	spy_aux(Functor//Arity) :-
		ExtArity is Arity + 2,
		spy_predicate(Functor, ExtArity, Functor//Arity).

	spy_clause(Entity-Line) :-
		callable(Entity),
		integer(Line),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		(	clause_breakpoint_(Template, Line) ->
			true
		;	assertz(clause_breakpoint_(Template, Line))
		),
		retractall(log_point_(Template, Line, _)),
		retractall(conditional_breakpoint_(Template, Line, _)),
		retractall(triggered_breakpoint_(Template, Line, _, _)),
		retractall(triggered_breakpoint_enabled_(Template, Line)).

	spy_predicate(Functor, Arity, Original) :-
		atom(Functor),
		integer(Arity),
		(	predicate_breakpoint_(Functor, Arity, _) ->
			true
		;	assertz(predicate_breakpoint_(Functor, Arity, Original))
		).

	spy_entity_predicate(Entity, Functor, Arity) :-
		callable(Entity),
		functor(Entity, EntityFunctor, EntityArity),
		functor(Template, EntityFunctor, EntityArity),
		atom(Functor),
		integer(Arity),
		(	entity_predicate_breakpoint_(Template, Functor, Arity, _) ->
			true
		;	assertz(entity_predicate_breakpoint_(Template, Functor, Arity, Template::Functor/Arity))
		).

	spy_entity_non_terminal(Entity, Functor, Arity) :-
		callable(Entity),
		functor(Entity, EntityFunctor, EntityArity),
		functor(Template, EntityFunctor, EntityArity),
		atom(Functor),
		integer(Arity),
		ExtArity is Arity + 2,
		(	entity_predicate_breakpoint_(Template, Functor, ExtArity, _) ->
			true
		;	assertz(entity_predicate_breakpoint_(Template, Functor, ExtArity, Template::Functor//Arity))
		).

	spying(Entity-Line) :-
		clause_breakpoint_(Entity, Line).
	spying(Functor/Arity) :-
		predicate_breakpoint_(Functor, Arity, Functor/Arity).
	spying(Functor//Arity) :-
		predicate_breakpoint_(Functor, _, Functor//Arity).
	spying(Entity::Functor/Arity) :-
		entity_predicate_breakpoint_(Entity, Functor, Arity, Functor/Arity).
	spying(Entity::Functor//Arity) :-
		entity_predicate_breakpoint_(Entity, Functor, _, Functor//Arity).

	nospy(SpyPoints) :-
		nospy_aux(SpyPoints),
		print_message(information, debugger, matching_breakpoints_removed).

	nospy_aux(SpyPoints) :-
		var(SpyPoints),
		!,
		nospy_clause(_),
		nospy_predicate(_),
		nospy_non_terminal(_),
		nospy_entity_predicate(_),
		nospy_entity_non_terminal(_).
	nospy_aux([]).
	nospy_aux([SpyPoint| SpyPoints]) :-
		nospy_list([SpyPoint| SpyPoints]).
	nospy_aux(Entity-Line) :-
		nospy_clause(Entity-Line).
	nospy_aux(Functor/Arity) :-
		nospy_predicate(Functor/Arity).
	nospy_aux(Functor//Arity) :-
		nospy_non_terminal(Functor//Arity).
	nospy_aux(Entity::Functor/Arity) :-
		nospy_entity_predicate(Entity::Functor/Arity).
	nospy_aux(Entity::Functor//Arity) :-
		nospy_entity_non_terminal(Entity::Functor//Arity).

	nospy_list([]).
	nospy_list([SpyPoint| SpyPoints]) :-
		nospy_aux(SpyPoint),
		nospy_list(SpyPoints).

	nospy_clause(Entity-Line) :-
		retractall(clause_breakpoint_(Entity, Line)).

	nospy_predicate(Functor/Arity) :-
		retractall(predicate_breakpoint_(Functor, Arity, _)).

	nospy_non_terminal(Functor//Arity) :-
		retractall(predicate_breakpoint_(Functor, _, Functor//Arity)),
		(	integer(Arity) ->
			ExtArity is Arity + 2,
			retractall(predicate_breakpoint_(Functor, ExtArity, _))
		;	true
		).

	nospy_entity_predicate(Entity::Functor/Arity) :-
		retractall(entity_predicate_breakpoint_(Entity, Functor, Arity, _)).

	nospy_entity_non_terminal(Entity::Functor//Arity) :-
		retractall(entity_predicate_breakpoint_(Entity, Functor, _, Entity::Functor//Arity)),
		(	integer(Arity) ->
			ExtArity is Arity + 2,
			retractall(entity_predicate_breakpoint_(Entity, Functor, ExtArity, _))
		;	true
		).

	spy(Entity, Line, Condition) :-
		callable(Entity),
		integer(Line),
		nonvar(Condition),
		valid_conditional_spy_point_condition(Condition),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		retractall(conditional_breakpoint_(Template, Line, _)),
		retractall(triggered_breakpoint_(Template, Line, _, _)),
		retractall(triggered_breakpoint_enabled_(Template, Line)),
		retractall(clause_breakpoint_(Template, Line)),
		retractall(log_point_(Template, Line, _)),
		(	Condition = TriggerEntity-TriggerLine ->
			functor(TriggerEntity, TriggerFunctor, TriggerArity),
			functor(TriggerTemplate, TriggerFunctor, TriggerArity),
			assertz(triggered_breakpoint_(Template, Line, TriggerTemplate, TriggerLine)),
			print_message(information, debugger, triggered_breakpoint_added)
		;	assertz(conditional_breakpoint_(Template, Line, Condition)),
			print_message(information, debugger, conditional_breakpoint_added)
		),
		(	debugging_ ->
			true
		;	debug
		),
		(	object_property(Entity, file(File)) ->
			retractall(file_line_hit_count_(File, Line, _))
		;	category_property(Entity, file(File)) ->
			retractall(file_line_hit_count_(File, Line, _))
		;	% assume entity not yet loaded
			true
		).

	valid_conditional_spy_point_condition([_, _, _]>>Condition) :-
		!,
		callable(Condition).
	valid_conditional_spy_point_condition([_]>>Condition) :-
		!,
		callable(Condition).
	valid_conditional_spy_point_condition(Entity-Line) :-
		!,
		callable(Entity),
		integer(Line),
		once((
			conditional_breakpoint_(Entity, Line, _)
		;	clause_breakpoint_(Entity, Line)
		)).
	valid_conditional_spy_point_condition(>(Count)) :-
		!,
		integer(Count),
		Count >= 1.
	valid_conditional_spy_point_condition(>=(Count)) :-
		!,
		integer(Count),
		Count >= 1.
	valid_conditional_spy_point_condition(=:=(Count)) :-
		!,
		integer(Count),
		Count >= 1.
	valid_conditional_spy_point_condition(=<(Count)) :-
		!,
		integer(Count),
		Count >= 1.
	valid_conditional_spy_point_condition(<(Count)) :-
		!,
		integer(Count),
		Count >= 1.
	valid_conditional_spy_point_condition(mod(M)) :-
		!,
		integer(M),
		M >= 1.
	valid_conditional_spy_point_condition(Count) :-
		integer(Count),
		Count >= 1.

	spying(Entity, Line, Lambda) :-
		conditional_breakpoint_(Entity, Line, Lambda).

	nospy(Entity, Line, Lambda) :-
		retractall(conditional_breakpoint_(Entity, Line, Lambda)),
		print_message(comment, debugger, matching_conditional_spy_points_removed).

	spy(Sender, This, Self, Goal) :-
		asserta(context_breakpoint_(Sender, This, Self, Goal)),
		print_message(information, debugger, context_breakpoint_set),
		(	debugging_ ->
			true
		;	debug
		).

	spying(Sender, This, Self, Goal) :-
		context_breakpoint_(Sender, This, Self, Goal).

	nospy(Sender, This, Self, Goal) :-
		retractall(context_breakpoint_(Sender, This, Self, Goal)),
		print_message(comment, debugger, matching_context_breakpoints_removed).

	nospyall :-
		retractall(clause_breakpoint_(_, _)),
		retractall(conditional_breakpoint_(_, _, _)),
		retractall(triggered_breakpoint_(_, _, _, _)),
		retractall(triggered_breakpoint_enabled_(_, _)),
		retractall(predicate_breakpoint_(_, _, _)),
		retractall(entity_predicate_breakpoint_(_, _, _, _)),
		retractall(context_breakpoint_(_, _, _, _)),
		print_message(comment, debugger, all_breakpoints_removed).

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

	port_user_name(fact(_,_,_,_,_), fact).
	port_user_name(rule(_,_,_,_,_), rule).
	port_user_name(call,            call).
	port_user_name(exit,            exit).
	port_user_name(nd_exit,         exit).
	port_user_name(redo,            redo).
	port_user_name(fail,            fail).
	port_user_name(exception,       exception).

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

	% simplified version of the leashing_port/6 predicate just for checking
	% for a leashed port before entering the port interaction loop where the
	% check must be repeated due to some possible port commands
	leashing_port(Port, PortUserName, N, Goal, ExCtx) :-
		leashing_(PortUserName),
		(	conditional_port(Port, N, Goal) ->
			true
		;	triggered_port(Port) ->
			true
		;	spying_port_code(Port, Goal, ExCtx, _) ->
			true
		;	tracing_ ->
			true
		;	jump_to_invocation_number_(N) ->
			true
		;	zap_to_port_(PortUserName) ->
			true
		;	fail
		).

	leashing_port(Port, PortUserName, N, Goal, ExCtx, Code) :-
		leashing_(PortUserName),
		(	conditional_port(Port, N, Goal, Code) ->
			retractall(leaping_(_)),
			retractall(tracing_),
			assertz(tracing_)
		;	triggered_port(Port, Code) ->
			retractall(leaping_(_)),
			retractall(tracing_),
			assertz(tracing_)
		;	spying_port_code(Port, Goal, ExCtx, Code) ->
			retractall(leaping_(_)),
			retractall(tracing_),
			assertz(tracing_)
		;	tracing_ ->
			Code = ' '
		;	retract(jump_to_invocation_number_(N)) ->
			retractall(leaping_(_)),
			assertz(tracing_),
			Code = ' '
		;	retract(zap_to_port_(PortUserName)) ->
			retractall(zap_to_port_(_)),
			retractall(leaping_(_)),
			assertz(tracing_),
			Code = ' '
		;	fail
		).

	spying_port_code(fact(Entity,_,_,_,Line), _, _, '#') :-
		clause_breakpoint_(Entity, Line),
		(	triggered_breakpoint_(DependentEntity, DependentLine, Entity, Line) ->
			assertz(triggered_breakpoint_enabled_(DependentEntity, DependentLine))
		;	true
		),
		!.
	spying_port_code(rule(Entity,_,_,_,Line), _, _, '#') :-
		clause_breakpoint_(Entity, Line),
		(	triggered_breakpoint_(DependentEntity, DependentLine, Entity, Line) ->
			assertz(triggered_breakpoint_enabled_(DependentEntity, DependentLine))
		;	true
		),
		!.
	spying_port_code(_, Goal, _, '+') :-
		functor(Goal, Functor, Arity),
		predicate_breakpoint_(Functor, Arity, _),
		!.
	spying_port_code(_, Goal, ExCtx, '+') :-
		functor(Goal, Functor, Arity),
		logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
		entity_predicate_breakpoint_(Entity, Functor, Arity, _),
		!.
	spying_port_code(_, Goal, ExCtx, '*') :-
		logtalk::execution_context(ExCtx, _, Sender, This, Self, _, _),
		context_breakpoint_(Sender0, This0, Self0, Goal0),
		subsumes_term(sp(Sender0, This0, Self0, Goal0), sp(Sender, This, Self, Goal)),
		!.

	% log breakpoint predicates

	log(Entity, Line, Message) :-
		callable(Entity),
		integer(Line),
		functor(Entity, Functor, Arity),
		functor(Template, Functor, Arity),
		(	log_point_(Template, Line, Message) ->
			% log point already defined
			true
		;	log_point_(Template, Line, _) ->
			% assume updating log point message
			retractall(log_point_(Template, Line, _)),
			assertz(log_point_(Template, Line, Message))
		;	% new log point
			assertz(log_point_(Template, Line, Message))
		),
		retractall(clause_breakpoint_(Template, Line)),
		retractall(conditional_breakpoint_(Template, Line, _)),
		retractall(triggered_breakpoint_(Template, Line, _, _)),
		retractall(triggered_breakpoint_enabled_(Template, Line)),
		print_message(information, debugger, log_point_added),
		(	debugging_ ->
			true
		;	debug
		).

	logging(Entity, Line, Message) :-
		log_point_(Entity, Line, Message).

	nolog(Entity, Line, Message) :-
		retractall(log_point_(Entity, Line, Message)),
		print_message(information, debugger, matching_log_points_removed).

	nologall :-
		retractall(log_point_(_, _, _)),
		print_message(comment, debugger, all_log_points_removed).

	logging_port(fact(Entity, _, Clause, File, Line), N, Goal, ExCtx) :-
		log_point_(Entity, Line, Message),
		logging_message(Message, 'Fact', Entity, Clause, File, Line, N, Goal, ExCtx),
		(	triggered_breakpoint_(DependentEntity, DependentLine, Entity, Line) ->
			assertz(triggered_breakpoint_enabled_(DependentEntity, DependentLine))
		;	true
		).
	logging_port(rule(Entity, _, Clause, File, Line), N, Goal, ExCtx) :-
		log_point_(Entity, Line, Message),
		logging_message(Message, 'Rule', Entity, Clause, File, Line, N, Goal, ExCtx),
		(	triggered_breakpoint_(DependentEntity, DependentLine, Entity, Line) ->
			assertz(triggered_breakpoint_enabled_(DependentEntity, DependentLine))
		;	true
		).

	logging_message(Message, _Port, _Entity, _Clause, _File, _Line, N, Goal, _ExCtx) :-
		(	Message == ''
			% default port message
		;	sub_atom(Message, 0, 1, _, '%')
			% comment after default port message
		),
		!,
		(	write_max_depth_(MaxDepth),
			MaxDepth > 0 ->
			print_message(information, debugger, logging_port('@', Port, N, Goal, Message, MaxDepth))
		;	print_message(information, debugger, logging_port('@', Port, N, Goal, Message))
		).
	logging_message(Message, Port, Entity, Clause, File, Line, N, Goal, ExCtx) :-
		% default port message replacement
		atom_chars(Message, MessageChars),
		logging_message_tokens(MessageChars, Port, Entity, Clause, File, Line, N, Goal, ExCtx, Tokens),
		print_message(information, debugger, logging_port(Tokens)).

	logging_message_tokens([], _Port, _Entity, _Clause, _File, _Line, _N, _Goal, _ExCtx, []).
	logging_message_tokens(['$'| MessageChars0], Port, Entity, Clause, File, Line, N, Goal, ExCtx, [Token| Tokens]) :-
		logging_message_argument(MessageChars0, MessageChars, Port, Entity, Clause, File, Line, N, Goal, ExCtx, Token),
		!,
		logging_message_tokens(MessageChars, Port, Entity, Clause, File, Line, N, Goal, ExCtx, Tokens).
	logging_message_tokens([Char| MessageChars0], Port, Entity, Clause, File, Line, N, Goal, ExCtx, [Atom-[]| Tokens]) :-
		logging_message_chars(MessageChars0, Chars, MessageChars),
		atom_chars(Atom, [Char| Chars]),
		logging_message_tokens(MessageChars, Port, Entity, Clause, File, Line, N, Goal, ExCtx, Tokens).

	logging_message_argument(['P','O','R','T'| MessageChars], MessageChars, Port, _, _, _, _, _, _, _, '~w'-[Port]).
	logging_message_argument(['E','N','T','I','T','Y'| MessageChars], MessageChars, _, Entity, _, _, _, _, _, _, Token) :-
		logging_message_argument_token(Entity, Token).
	logging_message_argument(['C','L','A','U','S','E','_','N','U','M','B','E','R'| MessageChars], MessageChars, _, _, Clause, _, _, _, _, _, '~d'-[Clause]).
	logging_message_argument(['F','I','L','E'| MessageChars], MessageChars, _, _, _, File, _, _, _, _, '~q'-[File]).
	logging_message_argument(['L','I','N','E'| MessageChars], MessageChars, _, _, _, _, Line, _, _, _, '~d'-[Line]).
	logging_message_argument(['U','N','I','F','I','C','A','T','I','O','N','_','C','O','U','N','T'| MessageChars], MessageChars, _, _, _, File, Line, _, _, _, '~d'-[Count]) :-
		file_line_hit_count_(File, Line, Count).
	logging_message_argument(['I','N','V','O','C','A','T','I','O','N','_','N','U','M','B','E','R'| MessageChars], MessageChars, _, _, _, _, _, N, _, _, '~d'-[N]).
	logging_message_argument(['G','O','A','L'| MessageChars], MessageChars, _, _, _, _, _, _, Goal, _, Token) :-
		logging_message_argument_token(Goal, Token).
	logging_message_argument(['P','R','E','D','I','C','A','T','E'| MessageChars], MessageChars, _, _, _, _, _, _, Goal, _, '~q'-[Name/Arity]) :-
		functor(Goal, Name, Arity).
	logging_message_argument(['E','X','E','C','U','T','I','O','N','_','C','O','N','T','E','X','T'| MessageChars], MessageChars, _, _, _, _, _, _, _, ExCtx, Token) :-
		logging_message_argument_token(ExCtx, Token).
	logging_message_argument(['S','E','N','D','E','R'| MessageChars], MessageChars, _, _, _, _, _, _, _, ExCtx, Token) :-
		logtalk::execution_context(ExCtx, _, Sender, _, _, _, _),
		logging_message_argument_token(Sender, Token).
	logging_message_argument(['T','H','I','S'| MessageChars], MessageChars, _, _, _, _, _, _, _, ExCtx, Token) :-
		logtalk::execution_context(ExCtx, _, _, This, _, _, _),
		logging_message_argument_token(This, Token).
	logging_message_argument(['S','E','L','F'| MessageChars], MessageChars, _, _, _, _, _, _, _, ExCtx, Token) :-
		logtalk::execution_context(ExCtx, _, _, _, Self, _, _),
		logging_message_argument_token(Self, Token).
	logging_message_argument(['M','E','T','A','C','A','L','L','_','C','O','N','T','E','X','T'| MessageChars], MessageChars, _, _, _, _, _, _, _, ExCtx, Token) :-
		logtalk::execution_context(ExCtx, _, _, _, _, MetaCallContext, _),
		logging_message_argument_token(MetaCallContext, Token).
	logging_message_argument(['C','O','I','N','D','U','C','T','I','O','N','_','S','T','A','C','K'| MessageChars], MessageChars, _, _, _, _, _, _, _, ExCtx, Token) :-
		logtalk::execution_context(ExCtx, _, _, _, _, _, CoinductionStack),
		logging_message_argument_token(CoinductionStack, Token).
	:- if(current_logtalk_flag(threads, supported)).
		logging_message_argument(['T','H','R','E','A','D'| MessageChars], MessageChars, _, _, _, _, _, _, _, _, '~q'-[Thread]) :-
			thread_self(Thread).
	:- endif.

	logging_message_argument_token(Term, Token) :-
		(	write_max_depth_(MaxDepth),
			MaxDepth > 0 ->
			Token = term(Term,[quoted(true),numbervars(true),max_depth(MaxDepth)])
		;	Token = '~q'-[Term]
		).

	logging_message_chars([], [], []).
	logging_message_chars(['$'| Chars], [], ['$'| Chars]) :-
		!.
	logging_message_chars([Char| MessageChars0], [Char| Chars], MessageChars) :-
		logging_message_chars(MessageChars0, Chars, MessageChars).

	% conditional breakpoint predicates

	conditional_port(fact(Entity, _, _, File, Line), N, Goal) :-
		conditional_breakpoint_(Entity, Line, Condition),
		conditional_port_check(Condition, File, Line, N, Goal).
	conditional_port(rule(Entity, _, _, File, Line), N, Goal) :-
		conditional_breakpoint_(Entity, Line, Condition),
		conditional_port_check(Condition, File, Line, N, Goal).

	conditional_port(fact(Entity, _, _, File, Line), N, Goal, '?') :-
		conditional_breakpoint_(Entity, Line, Condition),
		conditional_port_check(Condition, File, Line, N, Goal),
		(	triggered_breakpoint_(DependentEntity, DependentLine, Entity, Line) ->
			assertz(triggered_breakpoint_enabled_(DependentEntity, DependentLine))
		;	true
		).
	conditional_port(rule(Entity, _, _, File, Line), N, Goal, '?') :-
		conditional_breakpoint_(Entity, Line, Condition),
		conditional_port_check(Condition, File, Line, N, Goal),
		(	triggered_breakpoint_(DependentEntity, DependentLine, Entity, Line) ->
			assertz(triggered_breakpoint_enabled_(DependentEntity, DependentLine))
		;	true
		).

	:- meta_predicate(conditional_port_check(*, *, *, *, *)).
	conditional_port_check([Count,N0,Goal0]>>Condition, File, Line, N, Goal) :-
		!,
		N = N0,
		Goal = Goal0,
		file_line_hit_count_(File, Line, Count),
		catch({Condition}, _, fail).
	conditional_port_check([Goal0]>>Condition, _, _, _, Goal) :-
		!,
		Goal = Goal0,
		catch({Condition}, _, fail).
	conditional_port_check(>(HitCount), File, Line, _, _) :-
		!,
		file_line_hit_count_(File, Line, Count),
		Count > HitCount.
	conditional_port_check(>=(HitCount), File, Line, _, _) :-
		!,
		file_line_hit_count_(File, Line, Count),
		Count >= HitCount.
	conditional_port_check(=:=(HitCount), File, Line, _, _) :-
		!,
		file_line_hit_count_(File, Line, Count),
		Count =:= HitCount.
	conditional_port_check(=<(HitCount), File, Line, _, _) :-
		!,
		file_line_hit_count_(File, Line, Count),
		Count =< HitCount.
	conditional_port_check(<(HitCount), File, Line, _, _) :-
		!,
		file_line_hit_count_(File, Line, Count),
		Count < HitCount.
	conditional_port_check(mod(M), File, Line, _, _) :-
		!,
		file_line_hit_count_(File, Line, Count),
		Count mod M =:= 0.
	conditional_port_check(HitCount, File, Line, _, _) :-
		file_line_hit_count_(File, Line, Count),
		Count >= HitCount.

	% triggered breakpoint predicates

	triggered_port(fact(Entity, _, _, _, Line)) :-
		triggered_breakpoint_enabled_(Entity, Line), !.
	triggered_port(rule(Entity, _, _, _, Line)) :-
		triggered_breakpoint_enabled_(Entity, Line), !.

	triggered_port(fact(Entity, _, _, _, Line), '^') :-
		retract(triggered_breakpoint_enabled_(Entity, Line)), !.
	triggered_port(rule(Entity, _, _, _, Line), '^') :-
		retract(triggered_breakpoint_enabled_(Entity, Line)), !.

	% debug handler

	:- multifile(logtalk::debug_handler/1).
	logtalk::debug_handler(debugger).

	:- multifile(logtalk::debug_handler/3).
	logtalk::debug_handler(debugger, Event, ExCtx) :-
		debug_handler(Event, ExCtx).

	:- meta_predicate(debug_handler((::), (*))).

	debug_handler(fact(Entity,Fact,Clause,File,Line), ExCtx) :-
		inc_file_line_hit_count(File, Line),
		(	debugging_,
			(	\+ skipping_,
				\+ quasi_skipping_
			;	quasi_skipping_,
				clause_breakpoint_(Entity, Line)
			) ->
			invocation_number_(N),
			port(fact(Entity,Fact,Clause,File,Line), N, Fact, _, _, ExCtx, Action),
			{Action}
		;	true
		).
	debug_handler(rule(Entity,Head,Clause,File,Line), ExCtx) :-
		inc_file_line_hit_count(File, Line),
		(	debugging_,
			(	\+ skipping_,
				\+ quasi_skipping_
			;	quasi_skipping_,
				clause_breakpoint_(Entity, Line)
			) ->
			invocation_number_(N),
			port(rule(Entity,Head,Clause,File,Line), N, Head, _, _, ExCtx, Action),
			{Action}
		;	true
		).
	debug_handler(top_goal(Goal, TGoal), ExCtx) :-
		reset_invocation_number(_),
		retractall(file_line_hit_count_(_, _, _)),
		retractall(tracing_),
		retractall(leaping_(_)),
		retractall(skipping_),
		retractall(quasi_skipping_),
		retractall(skipping_unleashed_(_)),
		retractall(zap_to_port_(_)),
		(	explicit_tracing_ ->
			assertz(tracing_)
		;	true
		),
		debug_handler(goal(Goal, TGoal), ExCtx).
	debug_handler(goal(Goal, TGoal), ExCtx) :-
		inc_invocation_number(N),
		(	debugging_,
			(	\+ skipping_,
				\+ quasi_skipping_
			;	quasi_skipping_,
				(	functor(Goal, Functor, Arity),
					predicate_breakpoint_(Functor, Arity, _)
				;	functor(Goal, Functor, Arity),
					logtalk::execution_context(ExCtx, Entity, _, _, _, _, _),
					entity_predicate_breakpoint_(Entity, Functor, Arity, _)
				;	logtalk::execution_context(ExCtx, _, Sender, This, Self, _, _),
					context_breakpoint_(Sender0, This0, Self0, Goal0),
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
			retractall(skipping_unleashed_(N)),
			port(fail, N, Goal, TGoal, _, ExCtx, _),
			fail
		),
		retractall(skipping_),
		retractall(quasi_skipping_),
		retractall(skipping_unleashed_(N)).

	:- meta_predicate(call_goal(*, *)).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(Dialect == b; Dialect == swi; Dialect == tau; Dialect == trealla; Dialect == yap)
	)).

		call_goal(TGoal, Deterministic) :-
			{setup_call_cleanup(true, TGoal, Deterministic = true)}.

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == sicstus; Dialect == xsb))).

		call_goal(TGoal, Deterministic) :-
			{call_cleanup(TGoal, Deterministic = true)}.

	:- elif((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == ciao; Dialect == gnu; Dialect == xvm))).

		call_goal(TGoal, Deterministic) :-
			{call_det(TGoal, Deterministic)}.

	:- elif(current_logtalk_flag(prolog_dialect, cx)).

		call_goal(TGoal, Deterministic) :-
			{call(TGoal), deterministic(Deterministic)}.

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

		call_goal(TGoal, Deterministic) :-
			{	sepia_kernel:get_cut(Before),
				call(TGoal),
				sepia_kernel:get_cut(After)
			},
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
		port_user_name(Port, PortUserName),
		(	logging_port(Port, N, Goal, ExCtx) ->
			Action = true
		;	leashing_port(Port, PortUserName, N, Goal, ExCtx),
			\+ skipping_unleashed_(_) ->
			repeat,
				% the do_port_option/7 call can fail but still change the value of Code
				% (e.g., when adding or removing a spy point)
				leashing_port(Port, PortUserName, N, Goal, ExCtx, Code),
				% allow tools such as the Logtalk for VSCode extension to intercept port messages
				print_message(silent, debugger, Port),
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
				Code = ' '
			;	spying_port_code(Port, Goal, ExCtx, Code)
			) ->
			(	write_max_depth_(MaxDepth),
				MaxDepth > 0 ->
				print_message(information, debugger, tracing_port(Code, Port, N, Goal, MaxDepth))
			;	print_message(information, debugger, tracing_port(Code, Port, N, Goal))
			),
			Action = true
		;	Action = true
		).

	port(_, _, _, _, _, _, true).

	valid_port_option('\r', _, _) :- !.
	valid_port_option('\n', _, _) :- !.
	valid_port_option(' ', _, _) :- !.
	valid_port_option(c, _, _) :- !.
	valid_port_option(l, _, _) :- !.
	valid_port_option(s, _, _) :- !.
	valid_port_option('S', _, _) :- !.
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
	valid_port_option('N', _, _) :- !.
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
	valid_port_option('E', _, _) :- !.
	valid_port_option((<), _, _) :- !.

	do_port_option('\r', _, _, _, _, _, _, true).
	do_port_option('\n', _, _, _, _, _, _, true).
	do_port_option(' ', _, _, _, _, _, _, true).
	do_port_option(c, _, _, _, _, _, _, true).

	do_port_option(l, _, _, _, _, _, _, true) :-
		(	tracing_ ->
			retractall(tracing_),
			retractall(leaping_(_)),
			assertz(leaping_(tracing))
		;	retractall(leaping_(_)),
			assertz(leaping_(debugging))
		).

	do_port_option(s, rule(_,_,_,_,_), _, _, _, _, _, true) :-
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

	do_port_option('S', rule(_,_,_,_,_), N, _, _, _, _, true) :-
		!,
		retractall(skipping_unleashed_(_)),
		assertz(skipping_unleashed_(N)).
	do_port_option('S', call, N, _, _, _, _, true) :-
		!,
		retractall(skipping_unleashed_(_)),
		assertz(skipping_unleashed_(N)).
	do_port_option('S', redo, N, _, _, _, _, true) :-
		!,
		retractall(skipping_unleashed_(_)),
		assertz(skipping_unleashed_(N)).
	do_port_option('S', _, _, _, _, _, _, true).

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
		discard_new_line,
		retractall(jump_to_invocation_number_(_)),
		assertz(jump_to_invocation_number_(N)),
		retractall(tracing_).

	do_port_option(z, _, _, _, _, _, _, true) :-
		ask_question(question, debugger, enter_port_name, valid_zap_port, ZapPort),
		discard_new_line,
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
		discard_new_line,
		(	Goal = Term ->
			Result = unify
		;	Result = fail
		).

	do_port_option(n, _, _, _, _, _, _, true) :-
		nodebug.

	do_port_option('N', _, _, _, _, _, _, true) :-
		notrace.

	do_port_option((=), _, _, _, _, _, _, _) :-
		debugging_details,
		fail.

	do_port_option((+), _, _, Goal, _, _, _, _) :-
		(	Goal = (_ :: Predicate) ->
			functor(Predicate, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		spy_predicate(Functor, Arity, Functor/Arity),
		print_message(information, debugger, predicate_breakpoint_added),
		fail.

	do_port_option((-), _, _, Goal, _, _, _, _) :-
		(	Goal = (_ :: Predicate) ->
			functor(Predicate, Functor, Arity)
		;	functor(Goal, Functor, Arity)
		),
		nospy_predicate(Functor/Arity),
		print_message(information, debugger, predicate_breakpoint_removed),
		fail.

	do_port_option((#), Port, _, _, _, _, _, _) :-
		(	Port = fact(Entity, _, _, _, Line) ->
			true
		;	Port = rule(Entity, _, _, _, Line)
		),
		spy_clause(Entity-Line),
		print_message(information, debugger, clause_breakpoint_added),
		fail.

	do_port_option(('|'), Port, _, _, _, _, _, _) :-
		(	Port = fact(Entity, _, _, _, Line) ->
			true
		;	Port = rule(Entity, _, _, _, Line)
		),
		nospy_clause(Entity-Line),
		print_message(information, debugger, clause_breakpoint_removed),
		fail.

	do_port_option((*), _, _, Goal, _, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(Template, Functor, Arity),
		ask_question(question, debugger, enter_context_breakpoint(Template), '='((Sender,This,Self,Template)), (Sender,This,Self,Template)),
		discard_new_line,
		spy(Sender, This, Self, Template),
		fail.

	do_port_option((/), _, _, Goal, _, _, _, _) :-
		functor(Goal, Functor, Arity),
		functor(Template, Functor, Arity),
		ask_question(question, debugger, enter_context_breakpoint(Template), '='((Sender,This,Self,Template)), (Sender,This,Self,Template)),
		discard_new_line,
		nospy(Sender, This, Self, Template),
		fail.

	do_port_option(!, Port, N, Goal, TGoal, Error, ExCtx, Action) :-
		do_port_option((@), Port, N, Goal, TGoal, Error, ExCtx, Action).

	do_port_option((@), _, _, _, _, _, _, _) :-
		ask_question(question, debugger, enter_query, callable, Goal),
		discard_new_line,
		{once(Goal)},
		fail.

	do_port_option(a, _, _, _, _, _, _, _) :-
		retractall(skipping_),
		retractall(quasi_skipping_),
		retractall(skipping_unleashed_(_)),
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

	:- if((current_logtalk_flag(prolog_dialect,Dialect), Dialect \== b, Dialect \== cx, Dialect \== ji)).

	do_port_option((<), _, _, _, _, _, _, _) :-
		ask_question(question, debugger, enter_write_max_depth, '=<'(0), N),
		discard_new_line,
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
		(	Port = fact(Entity, _, Clause, File, Line) ->
			true
		;	Port = rule(Entity, _, Clause, File, Line)
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

	do_port_option('E', _, _, _, _, _, _, _) :-
		ask_question(question, debugger, enter_exception_term, callable, Error),
		discard_new_line,
		throw(Error).

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
		inc_invocation_number/1, reset_invocation_number/1,
		inc_file_line_hit_count/2
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

	inc_file_line_hit_count(File, Line) :-
		(	retract(file_line_hit_count_(File, Line, Old)) ->
			New is Old + 1,
			asserta(file_line_hit_count_(File, Line, New))
		;	% something weird happen as the previous call should never fail
			asserta(file_line_hit_count_(File, Line, 1))
		).

	:- if(predicate_property(get_unbuffered_char(_), built_in)). % e.g. Trealla Prolog or XVM

		read_single_char(Char) :-
			{get_unbuffered_char(Char)}, put_char(Char),
			(	Char == '\n' ->
				true
			;	nl
			).

	:- elif(current_logtalk_flag(prolog_dialect, cx)).

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

	% called after a call to logtalk::ask_question/5 to discard
	% any new-line and thus avoiding an automatic creep when
	% returning to the port after the user answers the question
	discard_new_line :-
		(	peek_code(10) ->
			get_code(_)
		;	true
		).

	write_max_depth(Depth) :-
		write_max_depth_(Depth).

	set_write_max_depth(Depth) :-
		retractall(write_max_depth_(_)),
		assertz(write_max_depth_(Depth)).

:- end_object.


:- if(current_logtalk_flag(prolog_dialect, swi)).
	% add dummy meta_predicate/1 directive to avoid cluttering the make/0 analysis report
	{:- meta_predicate(':'(user,'$debugger#0.call_goal#2'(*,*,*)))}.
:- endif.
