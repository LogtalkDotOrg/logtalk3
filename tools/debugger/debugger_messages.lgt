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


:- category(debugger_messages).

	:- info([
		version is 4:0:0,
		author is 'Paulo Moura',
		date is 2025-12-18,
		comment is 'Logtalk ``debugger`` tool default message translations.'
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	% structured message printing settings;
	% the main reason to not write directly to an output stream is to allow
	% other tools such as IDEs to intercept and handle debugger output

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, debugger, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	message_prefix_stream(question,    '', user_output).
	message_prefix_stream(comment,     '', user_output).
	message_prefix_stream(information, '', user_output).
	message_prefix_stream(warning,     '', user_output).
	message_prefix_stream(error,       '', user_output).

	% structured question asking settings;
	% the main reason to not read directly from an input stream is to allow
	% other tools such as IDEs to intercept and handle debugger user input

	:- multifile(logtalk::question_prompt_stream/4).
	:- dynamic(logtalk::question_prompt_stream/4).

	logtalk::question_prompt_stream(question, debugger, '     > ', user_input).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, debugger) -->
		message_tokens(Message).

	% questions

	message_tokens(enter_query) -->
		['     Enter a query to be executed deterministically'-[], nl].

	message_tokens(enter_goal) -->
		['     Enter a goal to unify with the current goal'-[], nl].

	message_tokens(enter_context_breakpoint(GoalTemplate)) -->
		{ground_term_copy(GoalTemplate, GroundGoalTemplate)},
		['     Enter a context breakpoint term formatted as (Sender, This, Self, ~q)'-[GroundGoalTemplate], nl].

	message_tokens(enter_invocation_number) -->
		['     Enter an invocation number to jump to'-[], nl].

	message_tokens(enter_port_name) -->
		['     Enter a port name or a negated port name to zap to'-[], nl].

	message_tokens(enter_write_max_depth) -->
		['     Enter the maximum write depth for terms (0 to reset)'-[], nl].

	message_tokens(enter_exception_term) -->
		['     Enter the exception term to throw'-[], nl].

	% debugger status and switching

	message_tokens(debugger_spying_on) -->
		['   Debugger is on: pausing on breakpoints for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_spying_switched_on) -->
		['   Debugger switched on: pausing on breakpoints for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_tracing_on) -->
		['   Debugger is on: tracing everything for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_tracing_switched_on) -->
		['   Debugger switched on: tracing everything for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_off) -->
		['   Debugger is off.'-[], nl].

	message_tokens(debugger_switched_off) -->
		['   Debugger switched off.'-[], nl].

	message_tokens(debugger_tracing_off) -->
		['   Debugger tracing is off.'-[], nl].

	message_tokens(debugger_tracing_switched_off) -->
		['   Debugger tracing switched off.'-[], nl].

	message_tokens(max_depth(MaxDepth)) -->
		['     Maximum write depth for terms: ~w'-[MaxDepth], nl].

	% at port

	message_tokens(fact(_, _, _, _, _)) -->
		[].
	message_tokens(rule(_, _, _, _, _)) -->
		[].
	message_tokens(call) -->
		[].
	message_tokens(exit) -->
		[].
	message_tokens(nd_exit) -->
		[].
	message_tokens(redo) -->
		[].
	message_tokens(fail) -->
		[].
	message_tokens(exception) -->
		[].

	message_tokens(leashing_port(Code, Port, N, Goal, MaxDepth)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), ' ? '-[]].

	message_tokens(leashing_port(Code, Port, N, Goal)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q ? '-[Goal]].

	message_tokens(tracing_port(Code, Port, N, Goal, MaxDepth)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(tracing_port(Code, Port, N, Goal)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q'-[Goal], nl].

	% log points

	message_tokens(logging_port(Code, Port, N, Goal, Message, MaxDepth)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(false),numbervars(true),max_depth(MaxDepth)]), '~w'-[Message], nl].

	message_tokens(logging_port(Code, Port, N, Goal, Message)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q ~w'-[Goal, Message], nl].

	message_tokens(logging_port([])) -->
		[nl].
	message_tokens(logging_port([Token| Tokens])) -->
		[Token],
		message_tokens(logging_port(Tokens)).

	message_tokens(log_point_added) -->
		['     Log point added.'-[], nl].

	message_tokens(matching_log_points_removed) -->
		['     All matching log points removed.'-[], nl].

	message_tokens(all_log_points_removed) -->
		['     All log points removed.'-[], nl].

	message_tokens(log_points(LogPoints)) -->
		['     Defined log points (Entity-Line):'-[], nl],
		log_points(LogPoints).

	message_tokens(no_log_points_defined) -->
		['     No log points are defined.'-[], nl].

	% conditional breakpoints

	message_tokens(conditional_breakpoint_added) -->
		['     Conditional breakpoint added.'-[], nl].

	message_tokens(matching_conditional_spy_points_removed) -->
		['     All matching conditional breakpoints removed.'-[], nl].

	message_tokens(conditional_breakpoints(Breakpoints)) -->
		['     Defined conditional breakpoints: Entity-Line (Condition):'-[], nl],
		conditional_points(Breakpoints).

	message_tokens(no_conditional_breakpoints_defined) -->
		['     No conditional breakpoints are defined.'-[], nl].

	% triggered breakpoints

	message_tokens(triggered_breakpoint_added) -->
		['     Triggered breakpoint added.'-[], nl].

	message_tokens(matching_triggered_breakpoints_removed) -->
		['     All matching triggered breakpoints removed.'-[], nl].

	message_tokens(triggered_breakpoints(Breakpoints)) -->
		['     Defined triggered breakpoints: Entity-Line (TriggerEntity-TriggerLine):'-[], nl],
		conditional_points(Breakpoints).

	message_tokens(no_triggered_breakpoints_defined) -->
		['     No triggered breakpoints are defined.'-[], nl].

	% ading and removing breakpoints

	message_tokens(breakpoints_added) -->
		['     All specified breakpoints added.'-[], nl].

	message_tokens(matching_breakpoints_removed) -->
		['     All matching predicate and clause breakpoints removed.'-[], nl].

	% predicate breakpoints

	message_tokens(predicate_breakpoints(Breakpoints)) -->
		['     Defined predicate breakpoints:'-[], nl],
		breakpoints(Breakpoints).

	message_tokens(no_predicate_breakpoints_defined) -->
		['     No predicate breakpoints are defined.'-[], nl].

	message_tokens(predicate_breakpoint_added) -->
		['     Predicate breakpoint added.'-[], nl].

	message_tokens(predicate_breakpoint_removed) -->
		['     Predicate breakpoint removed.'-[], nl].

	% entity predicate breakpoints

	message_tokens(entity_predicate_breakpoints(Breakpoints)) -->
		['     Defined entity predicate breakpoints:'-[], nl],
		breakpoints(Breakpoints).

	message_tokens(no_entity_predicate_breakpoints_defined) -->
		['     No entity predicate breakpoints are defined.'-[], nl].

	message_tokens(entity_predicate_breakpoint_added) -->
		['     Entity predicate breakpoint added.'-[], nl].

	message_tokens(entity_predicate_breakpoint_removed) -->
		['     Entity predicate breakpoint removed.'-[], nl].

	% breakpoints

	message_tokens(all_breakpoints_removed) -->
		['     All breakpoints removed.'-[], nl].

	% clause breakpoints

	message_tokens(clause_breakpoints(Breakpoints)) -->
		['     Defined clause breakpoints: Entity-Line:'-[], nl],
		breakpoints(Breakpoints).

	message_tokens(no_clause_breakpoints_defined) -->
		['     No clause breakpoints are defined.'-[], nl].

	message_tokens(clause_breakpoint_added) -->
		['     Clause breakpoint added.'-[], nl].

	message_tokens(clause_breakpoint_removed) -->
		['     Clause breakpoint removed.'-[], nl].

	% context breakpoints

	message_tokens(context_breakpoints(Breakpoints)) -->
		['     Defined context breakpoints (Sender, This, Self, Goal):'-[], nl],
		context_breakpoints(Breakpoints).

	message_tokens(matching_context_breakpoints_removed) -->
		['     All matching context breakpoints removed.'-[], nl].

	message_tokens(context_breakpoint_set) -->
		['     Context breakpoint set.'-[], nl].

	message_tokens(no_context_breakpoints_defined) -->
		['     No context breakpoints are defined.'-[], nl].

	% execution context

	message_tokens(execution_context(Entity0, Sender0, This0, Self0, MetaCallCtx0, Stack0)) -->
		% in some cases, e.g. when dealing with multifile clauses for Prolog modules,
		% some of the execution context elements may not available
		{var(Entity0) -> Entity = n/a; Entity = Entity0},
		{var(Sender0) -> Sender = n/a; Sender = Sender0},
		{var(This0) -> This = n/a; This = This0},
		{var(Self0) -> Self = n/a; Self = Self0},
		{var(MetaCallCtx0) -> MetaCallCtx = n/a; MetaCallCtx = MetaCallCtx0},
		{var(Stack0) -> Stack = n/a; Stack = Stack0},
		[
			'     Entity:            ~q'-[Entity], nl,
			'     Sender:            ~q'-[Sender], nl,
			'     This:              ~q'-[This], nl,
			'     Self:              ~q'-[Self], nl,
			'     Meta-call context: ~q'-[MetaCallCtx], nl,
			'     Coinduction stack: ~q'-[Stack], nl
		].

	% file context

	message_tokens(file_context(File0, Line0, Entity, Predicate, Clause0)) -->
		{	ground_term_copy(Entity, GroundEntity),
			location_and_clause_number(File0, Line0, Clause0, File, Line, Clause)
		},
		[
			'     File:          ~w'-[File], nl,
			'     Line number:   ~w'-[Line], nl,
			'     Entity:        ~q'-[GroundEntity], nl,
			'     Predicate:     ~q'-[Predicate], nl,
			'     Clause number: ~w'-[Clause], nl
		].

	% goals

	message_tokens(print_current_goal(Goal)) -->
		['     Current goal: ~p'-[Goal], nl].

	message_tokens(display_current_goal(Goal,MaxDepth)) -->
		['     Current goal: '-[], term(Goal,[quoted(true),ignore_ops(true),max_depth(MaxDepth)]), nl].

	message_tokens(display_current_goal(Goal)) -->
		['     Current goal: ~k'-[Goal], nl].

	message_tokens(write_current_goal(Goal,MaxDepth)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['     Current goal: '-[], term(GroundGoal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(write_current_goal(Goal)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['     Current goal: ~q'-[GroundGoal], nl].

	message_tokens(write_compiled_goal(Goal,MaxDepth)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['     Compiled goal: '-[], term(GroundGoal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(write_compiled_goal(Goal)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['     Compiled goal: ~q'-[GroundGoal], nl].

	% exceptions

	message_tokens(write_exception_term(Error,MaxDepth)) -->
		{ground_term_copy(Error, GroundError)},
		['     Exception term: '-[], term(GroundError,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(write_exception_term(Error)) -->
		{ground_term_copy(Error, GroundError)},
		['     Exception term: ~q'-[GroundError], nl].

	% features

	message_tokens(break_not_supported) -->
		['     break/0 not supported by the backend Prolog compiler.'-[], nl].

	message_tokens(max_depth_not_supported) -->
		['     Limiting write term depth not supported by the backend Prolog compiler.'-[], nl].

	% help

	message_tokens(extended_help) -->
		[
			'     Available options are:'-[], nl,
			'       c - creep (go on; you may use also the space, return, or enter keys)'-[], nl,
			'       l - leap (continues execution until the next breakpoint is found)'-[], nl,
			'       s - skip (skips tracing for the current goal; valid at call, redo, and unification ports)'-[], nl,
			'       S - Skip (similar to skip but displaying all intermediate ports unleashed)'-[], nl,
			'       q - quasi-skip (skips tracing until returning to the current goal or reaching a breakpoint)'-[], nl,
			'       r - retry (retries the current goal but side-effects are not undone; valid at the fail port)'-[], nl,
			'       j - jump (reads invocation number and jumps to the next port with that number)'-[], nl,
			'       z - zap (reads port name and zaps to that port)'-[], nl,
			'               (reads negated port name and zaps to a port other than the negated port)'-[], nl,
			'       i - ignore (ignores goal, assumes that it succeeded; valid at call and redo ports)'-[], nl,
			'       f - fail (forces backtracking; may also be used to convert an exception into a failure)'-[], nl,
			'       u - unify (reads and unifies a term with the current goal; valid at the call port)'-[], nl,
			'       n - nodebug (turns off debugging)'-[], nl,
			'       N - notrace (turns off tracing)'-[], nl,
			'       ! - command (reads and executes a query)'-[], nl,
			'       @ - command (reads and executes a query)'-[], nl,
			'       b - break (suspends execution and starts new interpreter; type end_of_file to terminate)'-[], nl,
			'       a - abort (returns to top level interpreter)'-[], nl,
			'       Q - quit (quits Logtalk)'-[], nl,
			'       p - print (writes current goal calling the portray/1 hook predicate if available)'-[], nl,
			'       d - display (writes current goal without using operator notation)'-[], nl,
			'       w - write (writes current goal quoting atoms if necessary)'-[], nl,
			'       $ - outputs the compiled form of the current goal (for low-level debugging)'-[], nl,
			'       x - context (prints execution context)'-[], nl,
			'       . - file (prints file, entity, predicate, and line number data at an unification port)'-[], nl,
			'       e - exception (prints exception term thrown by current goal)'-[], nl,
			'       E - raise exception (reads and throws an exception term)'-[], nl,
			'       < - depth (sets the maximum write term depth; type 0 to reset)'-[], nl,
			'       = - debugging (prints debugging information)'-[], nl,
			'       * - add (adds a context breakpoint for the current goal)'-[], nl,
			'       / - remove (removes a context breakpoint for the current goal)'-[], nl,
			'       + - add (adds a predicate breakpoint for the current goal)'-[], nl,
			'       - - remove (removes a predicate breakpoint for the current goal)'-[], nl,
			'       # - add (adds a clause breakpoint for the current clause)'-[], nl,
			'       | - remove (removes a clause breakpoint for the current clause)'-[], nl,
			'       h - condensed help (prints this list of options)'-[], nl,
			'       ? - extended help'-[], nl
		].

	message_tokens(condensed_help) -->
		[
			'     Available options are:'-[], nl,
			'       c - creep            i - ignore     * - add context breakpoint'-[], nl,
			'       l - leap             f - fail       / - remove context breakpoint'-[], nl,
			'       s - skip             u - unify      + - add predicate breakpoint'-[], nl,
			'       S - Skip             n - nodebug    - - remove predicate breakpoint'-[], nl,
			'       q - quasi-skip       N - notrace    # - add clause breakpoint'-[], nl,
			'       r - retry            ! - command    | - remove clause breakpoint'-[], nl,
			'       j - jump             @ - command'-[], nl,
			'       z - zap              b - break'-[], nl,
			'       p - print            a - abort'-[], nl,
			'       d - display          Q - quit Logtalk'-[], nl,
			'       w - write            x - execution context'-[], nl,
			'       < - write depth      = - debugging information'-[], nl,
			'       $ - compiled goal    . - file information'-[], nl,
			'       e - exception'-[], nl,
			'       E - raise exception'-[], nl,
			'       h - condensed help   ? - extended help'-[], nl
		].

	% ports

	message_tokens(leashed_ports(Ports)) -->
		['     Leashed ports:'-[]],
		(	{Ports == []} ->
			[' (none)'-[]]
		;	leashed_ports(Ports)
		),
		[nl].

	leashed_ports([Port| Ports]) -->
		[' ~w'-[Port]],
		leashed_ports(Ports).
	leashed_ports([]) --> [].

	port_name(fact(_, _, _, _, _)) -->
		['  Fact: '-[]].
	port_name(rule(_, _, _, _, _)) -->
		['  Rule: '-[]].
	port_name(call) -->
		['  Call: '-[]].
	port_name(exit) -->
		['  Exit: '-[]].
	% Quintus Prolog doesn't provide a way to find if a call is deterministic
	:- if(current_logtalk_flag(prolog_dialect, quintus)).
		port_name(nd_exit) -->
			['  Exit: '-[]].
	:- else.
		port_name(nd_exit) -->
			[' *Exit: '-[]].
	:- endif.
	port_name(redo) -->
		['  Redo: '-[]].
	port_name(fail) -->
		['  Fail: '-[]].
	port_name(exception) -->
		['  Exception: '-[]].

	% invocation number

	invocation_number(N) -->
		['(~w) '-[N]].

	% auxiliary grammar rules

	log_points([Entity-Line| LogPoints]) -->
		{ground_term_copy(Entity, GroundEntity)},
		['        ~q'-[GroundEntity-Line], nl],
		log_points(LogPoints).
	log_points([]) -->
		[].

	conditional_points([bp(Entity,Line,Condition)| ConditionalPoints]) -->
		{ground_term_copy(Entity, GroundEntity)},
		['        ~q-~q (~q)'-[GroundEntity,Line,Condition], nl],
		conditional_points(ConditionalPoints).
	conditional_points([]) -->
		[].

	breakpoints([Breakpoint| Breakpoints]) -->
		breakpoint(Breakpoint),
		breakpoints(Breakpoints).
	breakpoints([]) -->
		[].

	breakpoint(Entity-Line) -->
		{ground_term_copy(Entity, GroundEntity)},
		['        ~q'-[GroundEntity-Line], nl].
	breakpoint(Functor/Arity) -->
		['        ~q'-[Functor/Arity], nl].
	breakpoint(Functor//Arity) -->
		['        ~q'-[Functor//Arity], nl].
	breakpoint(Entity::Predicate) -->
		{ground_term_copy(Entity, GroundEntity)},
		['        ~q'-[GroundEntity::Predicate], nl].

	context_breakpoints([Breakpoint| Breakpoints]) -->
		context_breakpoint(Breakpoint),
		context_breakpoints(Breakpoints).
	context_breakpoints([]) -->
		[].

	context_breakpoint((Sender,This,Self,Goal)) -->
		['        '-[]],
		(	{var(Sender)} ->
			['(_, '-[]]
		;	{ground_term_copy(Sender, GroundSender)},
			['(~q, '-[GroundSender]]
		),
		(	{var(This)} ->
			['_, '-[]]
		;	{ground_term_copy(This, GroundThis)},
			['~q, '-[GroundThis]]
		),
		(	{var(Self)} ->
			['_, '-[]]
		;	{ground_term_copy(Self, GroundSelf)},
			['~q, '-[GroundSelf]]
		),
		(	{var(Goal)} ->
			['_)'-[]]
		;	{ground_term_copy(Goal, GroundGoal)},
			['~q)'-[GroundGoal]]
		), [nl].

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

	location_and_clause_number(File0, Line0, Clause0, File, Line, Clause) :-
		(	File0 == nil ->
			File = 'n/a'
		;	File = File0
		),
		(	Line0 > 0 ->
			Line = Line0
		;	Line = 'n/a'
		),
		(	Clause0 =:= 0 ->
			Clause = 'n/a'
		;	Clause = Clause0
		).

:- end_category.
