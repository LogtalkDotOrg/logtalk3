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
		version is 1.3,
		author is 'Paulo Moura',
		date is 2015/10/22,
		comment is 'Logtalk debugger default message translations.'
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

	logtalk::question_prompt_stream(question, debugger, '    > ', user_input).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, debugger) -->
		message_tokens(Message).

	% questions

	message_tokens(enter_query) -->
		['    Enter a query to be executed deterministically'-[], nl].

	message_tokens(enter_goal) -->
		['    Enter a goal to unify with the current goal'-[], nl].

	message_tokens(enter_context_spy_point(GoalTemplate)) -->
		{ground_term_copy(GoalTemplate, GroundGoalTemplate)},
		['    Enter a context spy point term formatted as (Sender, This, Self, ~q)'-[GroundGoalTemplate], nl].

	message_tokens(enter_invocation_number) -->
		['    Enter an invocation number to jump to'-[], nl].

	message_tokens(enter_port_name) -->
		['    Enter a port name or a negated port name to zap to'-[], nl].

	message_tokens(enter_write_max_depth) -->
		['    Enter the maximum write depth for terms (0 to reset)'-[], nl].

	% debugger status and switching

	message_tokens(debugger_on_spying) -->
		['Debugger is on: showing spy points for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_switched_on_spying) -->
		['Debugger switched on: showing spy points for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_on_tracing) -->
		['Debugger is on: tracing everything for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_switched_on_tracing) -->
		['Debugger switched on: tracing everything for all objects compiled in debug mode.'-[], nl].

	message_tokens(debugger_off) -->
		['Debugger is off.'-[], nl].

	message_tokens(debugger_switched_off) -->
		['Debugger switched off.'-[], nl].

	% at port

	message_tokens(leashing_port(Code, Port, N, Goal, MaxDepth)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), ' ? '-[]].

	message_tokens(leashing_port(Code, Port, N, Goal)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q ? '-[Goal]].

	message_tokens(tracing_port(Code, Port, N, Goal, MaxDepth)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), [term(Goal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(tracing_port(Code, Port, N, Goal)) -->
		['~w'-[Code]], port_name(Port), invocation_number(N), ['~q'-[Goal], nl].

	% spy points

	message_tokens(all_spy_points_added) -->
		['    All specified spy points added.'-[], nl].

	message_tokens(matching_spy_points_removed) -->
		['    All matching line number and predicate spy points removed.'-[], nl].

	% predicate spy points

	message_tokens(predicate_spy_points(SpyPoints)) -->
		['    Defined predicate spy points (Functor/Arity):'-[], nl],
		spy_points(SpyPoints).

	message_tokens(all_predicate_spy_points_removed) -->
		['    All predicate spy points removed.'-[], nl].

	message_tokens(no_predicate_spy_points_defined) -->
		['    No predicate spy points are defined.'-[], nl].

	message_tokens(predicate_spy_point_added) -->
		['    Predicate spy point added.'-[], nl].

	message_tokens(predicate_spy_point_removed) -->
		['    Predicate spy point removed.'-[], nl].

	% line number spy points

	message_tokens(line_number_spy_points(SpyPoints)) -->
		['    Defined line number spy points (Entity-Line):'-[], nl],
		spy_points(SpyPoints).

	message_tokens(all_line_number_spy_points_removed) -->
		['    All line number spy points removed.'-[], nl].

	message_tokens(no_line_number_spy_points_defined) -->
		['    No line number spy points are defined.'-[], nl].

	message_tokens(line_number_spy_point_added) -->
		['    Line number spy point added.'-[], nl].

	message_tokens(line_number_spy_point_removed) -->
		['    Line number spy point removed.'-[], nl].

	% context spy points

	message_tokens(context_spy_points(SpyPoints)) -->
		['    Defined context spy points (Sender, This, Self, Goal):'-[], nl],
		context_spy_points(SpyPoints).

	message_tokens(matching_context_spy_points_removed) -->
		['    All matching context spy points removed.'-[], nl].

	message_tokens(all_context_spy_points_removed) -->
		['    All context spy points removed.'-[], nl].

	message_tokens(context_spy_point_set) -->
		['    Context spy point set.'-[], nl].

	message_tokens(no_context_spy_points_defined) -->
		['    No context spy points are defined.'-[], nl].

	% execution context

	message_tokens(execution_context(Entity0, Sender0, This0, Self0, MetaCallCtx0, Stack0)) -->
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

	message_tokens(file_context(Basename, Directory, Entity, Predicate, Clause0, Line0)) -->
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

	message_tokens(print_current_goal(Goal)) -->
		['    Current goal: ~p'-[Goal], nl].

	message_tokens(display_current_goal(Goal,MaxDepth)) -->
		['    Current goal: '-[], term(Goal,[quoted(true),ignore_ops(true),max_depth(MaxDepth)]), nl].

	message_tokens(display_current_goal(Goal)) -->
		['    Current goal: ~k'-[Goal], nl].

	message_tokens(write_current_goal(Goal,MaxDepth)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Current goal: '-[], term(GroundGoal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(write_current_goal(Goal)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Current goal: ~q'-[GroundGoal], nl].

	message_tokens(write_compiled_goal(Goal,MaxDepth)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Compiled goal: '-[], term(GroundGoal,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(write_compiled_goal(Goal)) -->
		{ground_term_copy(Goal, GroundGoal)},
		['    Compiled goal: ~q'-[GroundGoal], nl].

	% exceptions

	message_tokens(write_exception_term(Error,MaxDepth)) -->
		{ground_term_copy(Error, GroundError)},
		['    Exception term: '-[], term(GroundError,[quoted(true),numbervars(true),max_depth(MaxDepth)]), nl].

	message_tokens(write_exception_term(Error)) -->
		{ground_term_copy(Error, GroundError)},
		['    Exception term: ~q'-[GroundError], nl].

	% features

	message_tokens(break_not_supported) -->
		['    break/0 not supported by the back-end Prolog compiler.'-[], nl].

	message_tokens(max_depth_not_supported) -->
		['    Limiting write term depth not supported by the back-end Prolog compiler.'-[], nl].

	% help

	message_tokens(extended_help) -->
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

	message_tokens(condensed_help) -->
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

	message_tokens(leashed_ports(Ports)) -->
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
