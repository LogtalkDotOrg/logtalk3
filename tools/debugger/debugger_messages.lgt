%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Paulo Moura',
		date is 2015/09/12,
		comment is 'Logtalk debugger default message translations.'
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	% structured message printing settings;
	% the main reason to not write directly to an output stream is to allow
	% other tools such as IDEs to intercept and handle debugger output

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	% Quintus Prolog based prefixes (also used in SICStus Prolog):
	logtalk::message_prefix_stream(question,    debugger, '', user_output).
	logtalk::message_prefix_stream(comment,     debugger, '', user_output).
	logtalk::message_prefix_stream(information, debugger, '', user_output).
	logtalk::message_prefix_stream(warning,     debugger, '', user_output).
	logtalk::message_prefix_stream(error,       debugger, '', user_output).

	% structured question asking settings;
	% the main reason to not read directly from an input stream is to allow
	% other tools such as IDEs to intercept and handle debugger user input

	:- multifile(logtalk::question_prompt_stream/4).
	:- dynamic(logtalk::question_prompt_stream/4).

	logtalk::question_prompt_stream(question, debugger, '    > ', user_input).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	% questions

	logtalk::message_tokens(enter_query, debugger) -->
		['    Enter a query to be executed deterministically'-[], nl].

	logtalk::message_tokens(enter_goal, debugger) -->
		['    Enter a goal to unify with the current goal'-[], nl].

	logtalk::message_tokens(enter_context_spy_point(GoalTemplate), debugger) -->
		{ground_term_copy(GoalTemplate, GroundGoalTemplate)},
		['    Enter a context spy point term formatted as (Sender, This, Self, ~q)'-[GroundGoalTemplate], nl].

	logtalk::message_tokens(enter_invocation_number, debugger) -->
		['    Enter an invocation number to jump to'-[], nl].

	logtalk::message_tokens(enter_port_name, debugger) -->
		['    Enter a port name or a negated port name to zap to'-[], nl].

	logtalk::message_tokens(enter_write_max_depth, debugger) -->
		['    Enter the maximum write depth for terms (0 to reset)'-[], nl].

	% debugger status and switching

	logtalk::message_tokens(debugger_on_spying, debugger) -->
		['Debugger is on: showing spy points for all objects compiled in debug mode.'-[], nl].

	logtalk::message_tokens(debugger_switched_on_spying, debugger) -->
		['Debugger switched on: showing spy points for all objects compiled in debug mode.'-[], nl].

	logtalk::message_tokens(debugger_on_tracing, debugger) -->
		['Debugger is on: tracing everything for all objects compiled in debug mode.'-[], nl].

	logtalk::message_tokens(debugger_switched_on_tracing, debugger) -->
		['Debugger switched on: tracing everything for all objects compiled in debug mode.'-[], nl].

	logtalk::message_tokens(debugger_off, debugger) -->
		['Debugger is off.'-[], nl].

	logtalk::message_tokens(debugger_switched_off, debugger) -->
		['Debugger switched off.'-[], nl].

	% at port

	logtalk::message_tokens(leashing_port(Code, Port, N, Goal, MaxDepth), debugger) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), ' ? '-[]].

	logtalk::message_tokens(leashing_port(Code, Port, N, Goal), debugger) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q ? '-[Goal]].

	logtalk::message_tokens(tracing_port(Code, Port, N, Goal, MaxDepth), debugger) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	logtalk::message_tokens(tracing_port(Code, Port, N, Goal), debugger) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q'-[Goal], nl].

	% spy points

	logtalk::message_tokens(all_spy_points_added, debugger) -->
		['    All specified spy points added.'-[], nl].

	logtalk::message_tokens(matching_spy_points_removed, debugger) -->
		['    All matching line number and predicate spy points removed.'-[], nl].

	% predicate spy points

	logtalk::message_tokens(predicate_spy_points(SpyPoints), debugger) -->
		['    Defined predicate spy points (Functor/Arity):'-[], nl],
		spy_points(SpyPoints).

	logtalk::message_tokens(all_predicate_spy_points_removed, debugger) -->
		['    All predicate spy points removed.'-[], nl].

	logtalk::message_tokens(no_predicate_spy_points_defined, debugger) -->
		['    No predicate spy points are defined.'-[], nl].

	logtalk::message_tokens(predicate_spy_point_added, debugger) -->
		['    Predicate spy point added.'-[], nl].

	logtalk::message_tokens(predicate_spy_point_removed, debugger) -->
		['    Predicate spy point removed.'-[], nl].

	% line number spy points

	logtalk::message_tokens(line_number_spy_points(SpyPoints), debugger) -->
		['    Defined line number spy points (Entity-Line):'-[], nl],
		spy_points(SpyPoints).

	logtalk::message_tokens(all_line_number_spy_points_removed, debugger) -->
		['    All line number spy points removed.'-[], nl].

	logtalk::message_tokens(no_line_number_spy_points_defined, debugger) -->
		['    No line number spy points are defined.'-[], nl].

	logtalk::message_tokens(line_number_spy_point_added, debugger) -->
		['    Line number spy point added.'-[], nl].

	logtalk::message_tokens(line_number_spy_point_removed, debugger) -->
		['    Line number spy point removed.'-[], nl].

	% context spy points

	logtalk::message_tokens(context_spy_points(SpyPoints), debugger) -->
		['    Defined context spy points (Sender, This, Self, Goal):'-[], nl],
		context_spy_points(SpyPoints).

	logtalk::message_tokens(matching_context_spy_points_removed, debugger) -->
		['    All matching context spy points removed.'-[], nl].

	logtalk::message_tokens(all_context_spy_points_removed, debugger) -->
		['    All context spy points removed.'-[], nl].

	logtalk::message_tokens(context_spy_point_set, debugger) -->
		['    Context spy point set.'-[], nl].

	logtalk::message_tokens(no_context_spy_points_defined, debugger) -->
		['    No context spy points are defined.'-[], nl].

	% execution context

	logtalk::message_tokens(execution_context(Entity0, Sender0, This0, Self0, MetaCallCtx0, Stack0), debugger) -->
		% in some cases, e.g when dealing with multifile clauses for Prolog modules,
		% most of the execution context elements are not available
		{var(Entity0) -> Entity = n/a; Entity = Entity0},
		{var(Sender0) -> Sender = n/a; Sender = Sender0},
		{var(This0) -> This = n/a; This = This0},
		{var(Self0) -> Self = n/a; Self = Self0},
		{var(MetaCallCtx0) -> MetaCallCtx = n/a; MetaCallCtx = MetaCallCtx0},
		{var(Stack0) -> Stack = n/a; Stack = Stack0},
		[
			'    Entity:            ~q'-[Entity], nl,
			'    Sender:            ~q'-[Sender], nl,
			'    This:              ~q'-[This], nl,
			'    Self:              ~q'-[Self], nl,
			'    Meta-call context: ~q'-[MetaCallCtx], nl,
			'    Coinduction stack: ~q'-[Stack], nl
		].

	% file context

	logtalk::message_tokens(file_context(Basename, Directory, Entity, Predicate, Clause0, Line0), debugger) -->
		{atom_concat(Directory, Basename, File),
		 clause_and_line_numbers(Clause0, Line0, Clause, Line)},
		[
			'    File:              ~w'-[File], nl,
			'    Line number:       ~w'-[Line], nl,
			'    Entity:            ~q'-[Entity], nl,
			'    Predicate:         ~q'-[Predicate], nl,
			'    Clause number:     ~w'-[Clause], nl
		].

	% goals

	logtalk::message_tokens(print_current_goal(Goal), debugger) -->
		['    Current goal: ~p'-[Goal], nl].

	logtalk::message_tokens(display_current_goal(Goal,MaxDepth), debugger) -->
		['    Current goal: '-[], term(Goal,[quoted(true),ignore_ops(true),max_depth(MaxDepth)]), nl].

	logtalk::message_tokens(display_current_goal(Goal), debugger) -->
		['    Current goal: ~k'-[Goal], nl].

	logtalk::message_tokens(write_current_goal(Goal,MaxDepth), debugger) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Current goal: '-[], term(GroundGoal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	logtalk::message_tokens(write_current_goal(Goal), debugger) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Current goal: ~q'-[GroundGoal], nl].

	logtalk::message_tokens(write_compiled_goal(Goal,MaxDepth), debugger) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Compiled goal: '-[], term(GroundGoal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	logtalk::message_tokens(write_compiled_goal(Goal), debugger) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Compiled goal: ~q'-[GroundGoal], nl].

	% exceptions

	logtalk::message_tokens(write_exception_term(Error,MaxDepth), debugger) -->
		{ground_term_copy(Error, GroundError)},
		['    Exception term: '-[], term(GroundError,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	logtalk::message_tokens(write_exception_term(Error), debugger) -->
		{ground_term_copy(Error, GroundError)},
		['    Exception term: ~q'-[GroundError], nl].

	% features

	logtalk::message_tokens(break_not_supported, debugger) -->
		['    break/0 not supported by the back-end Prolog compiler.'-[], nl].

	logtalk::message_tokens(max_depth_not_supported, debugger) -->
		['    Limiting write term depth not supported by the back-end Prolog compiler.'-[], nl].

	% help

	logtalk::message_tokens(extended_help, debugger) -->
		[
			'     Available options are:'-[], nl,
			'       c - creep (go on; you may use also the spacebar, return, or enter keys)'-[], nl,
			'       l - leap (continues execution until the next spy point is found)'-[], nl,
			'       s - skip (skips debugging for the current goal; valid at call, redo, and unification ports)'-[], nl,
			'       q - quasi-skip (skips debugging until returning to the current goal or reaching a spy point)'-[], nl,
			'       r - retry (retries the current goal but side-effects are not undone; valid at the fail port)'-[], nl,
			'       j - jump (reads invocation number and jumps to the next port with that number)'-[], nl,
			'       z - zap (reads port name and zaps to that port)'-[], nl,
			'               (reads negated port name and zaps to a port other than the negated port)'-[], nl,
			'       i - ignore (ignores goal, assumes that it succeeded; valid at call and redo ports)'-[], nl,
			'       f - fail (forces backtracking; may also be used to convert an exception into a failure)'-[], nl,
			'       u - unify (reads and unifies a term with the current goal; valid at the call port)'-[], nl,
			'       n - nodebug (turns off debugging)'-[], nl,
			'       ! - command (reads and executes a query)'-[], nl,
			'       @ - command (reads and executes a query)'-[], nl,
			'       b - break (suspends execution and starts new interpreter; type end_of_file to terminate)'-[], nl,
			'       a - abort (returns to top level interpreter)'-[], nl,
			'       Q - quit (quits Logtalk)'-[], nl,
			'       p - print (writes current goal using print/1 if available)'-[], nl,
			'       d - display (writes current goal without using operator notation)'-[], nl,
			'       w - write (writes current goal quoting atoms if necessary)'-[], nl,
			'       $ - outputs the compiled form of the current goal (for low-level debugging)'-[], nl,
			'       x - context (prints execution context)'-[], nl,
			'       . - file (prints file, entity, predicate, and line number data at an unification port)'-[], nl,
			'       e - exception (prints exception term thrown by current goal)'-[], nl,
			'       < - depth (sets the maximum write term depth; type 0 to reset)'-[], nl,
			'       = - debugging (prints debugging information)'-[], nl,
			'       * - add (adds a context spy point for the current goal)'-[], nl,
			'       / - remove (removes a context spy point for the current goal)'-[], nl,
			'       + - add (adds a predicate spy point for the current goal)'-[], nl,
			'       - - remove (removes a predicate spy point for the current goal)'-[], nl,
			'       # - add (adds a line number spy point for the current clause)'-[], nl,
			'       | - remove (removes a line number spy point for the current clause)'-[], nl,
			'       h - condensed help (prints this list of options)'-[], nl,
			'       ? - extended help'-[], nl
		].

	logtalk::message_tokens(condensed_help, debugger) -->
		[
			'     Available options are:'-[], nl,
			'       c - creep            i - ignore     * - add context spy point'-[], nl,
			'       l - leap             f - fail       / - remove context spy point'-[], nl,
			'       s - skip             u - unify      + - add predicate spy point'-[], nl,
			'       q - quasi-skip       n - nodebug    - - remove predicate spy point'-[], nl,
			'       r - retry            ! - command    # - add line number spy point'-[], nl,
			'       j - jump             @ - command    | - remove line number spy point'-[], nl,
			'       z - zap              b - break'-[], nl,
			'       p - print            a - abort'-[], nl,
			'       d - display          Q - quit Logtalk'-[], nl,
			'       w - write            x - execution context'-[], nl,
			'       e - exception        = - debugging information'-[], nl,
			'       $ - compiled goal    . - file information'-[], nl,
			'       < - write depth'-[], nl,
			'       h - condensed help   ? - extended help'-[], nl
		].

	% ports

	logtalk::message_tokens(leashed_ports(Ports), debugger) -->
		['    Leashed ports:'-[]],
		(	{Ports == []} ->
			[' (none)'-[]]
		;	leashed_ports(Ports)
		),
		[nl].

	leashed_ports([Port| Ports]) -->
		 [' ~w'-[Port]],
		leashed_ports(Ports).
	leashed_ports([]) --> [].

	port_name(fact(_,_,_)) -->
		['  Fact: '-[]].
	port_name(rule(_,_,_)) -->
		['  Rule: '-[]].
	port_name(call) -->
		['  Call: '-[]].
	port_name(exit) -->
		['  Exit: '-[]].
	% Lean Prolog and Quintus Prolog don't provide a way to find if a call is deterministic
	:- if((current_logtalk_flag(prolog_dialect, Dialect), (Dialect == lean; Dialect == quintus))).
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

	spy_points([SpyPoint| SpyPoints]) -->
		spy_point(SpyPoint),
		spy_points(SpyPoints).
	spy_points([]) --> [].

	spy_point(Entity-Line) -->
		{ground_term_copy(Entity, GroundEntity)},
		['        ~q'-[GroundEntity-Line], nl].
	spy_point(Functor/Arity) -->
		['        ~q'-[Functor/Arity], nl].

	context_spy_points([SpyPoint| SpyPoints]) -->
		context_spy_point(SpyPoint),
		context_spy_points(SpyPoints).
	context_spy_points([]) --> [].

	context_spy_point((Sender,This,Self,Goal)) -->
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

	clause_and_line_numbers(Clause0, Line0, Clause, Line) :-
		 (	Line0 > 0 ->
		 	Line = Line0
		 ;	Line = 'n/a'
		 ),
		 (	Clause0 =:= 0 ->
		 	Clause = 'n/a'
		 ;	Clause = Clause0
		 ).

:- end_category.
