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


:- protocol(debuggerp).

	:- info([
		version is 3:5:0,
		author is 'Paulo Moura',
		date is 2025-09-03,
		comment is 'Debugger protocol.',
		remarks is [
			'Debugger help' - 'Type the character ``h`` (condensed help) or the character ``?`` (extended help) at a leashed port.',
			'Predicate breakpoint' - 'Specified as a ground term ``Functor/Arity``.',
			'Non-terminal breakpoint' - 'Specified as a ground term ``Functor//Arity``.',
			'Entity predicate breakpoint' - 'Specified as a term ``Entity::Functor/Arity``. ``Entity`` must be an object or category and may not be ground if parametric.',
			'Entity non-terminal breakpoint' - 'Specified as a term ``Entity::Functor//Arity``. ``Entity`` must be an object or category and may not be ground if parametric.',
			'Clause breakpoint' - 'Specified as an ``Entity-Line`` term with both ``Entity`` and ``Line`` bound. ``Line`` must be the first source file line of an entity clause.',
			'Conditional breakpoint' - 'Specified as an ``Entity-Line`` term with both ``Entity`` and ``Line`` bound and a condition. ``Line`` must be the first source file line of an entity clause.',
			'Hit count breakpoint' - 'Specified as an ``Entity-Line`` term with both ``Entity`` and ``Line`` bound and an unification count expression as a condition. ``Line`` must be the first source file line of an entity clause.',
			'Triggered breakpoint' - 'Specified as an ``Entity-Line`` term with both ``Entity`` and ``Line`` bound and another breakpoint as a condition. ``Line`` must be the first source file line of an entity clause.',
			'Context breakpoint' - 'Specified as a ``(Sender, This, Self, Goal)`` tuple.',
			'Log point' - 'Specified as an ``(Entity, Line, Message)`` tuple.',
			'Leash port shorthands' - '``none`` - ``[]``, ``loose`` - ``[fact,rule,call]``, ``half`` - ``[fact,rule,call,redo]``, ``tight`` - ``[fact,rule,call,redo,fail,exception]``, and ``full`` - ``[fact,rule,call,exit,redo,fail,exception]``.'
		],
		see_also is [debugger]
	]).

	% avoid a catch-22...
	:- set_logtalk_flag(debug, off).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Resets all debugging settings (including breakpoints, log points, and leashed ports) and turns off debugging.',
		see_also is [nospyall/0]
	]).

	:- public(debug/0).
	:- mode(debug, one).
	:- info(debug/0, [
		comment is 'Starts debugging for all defined breakpoints.'
	]).

	:- public(nodebug/0).
	:- mode(nodebug, one).
	:- info(nodebug/0, [
		comment is 'Stops debugging for all defined breakpoints. Also turns off tracing. Does not remove defined breakpoints.',
		see_also is [reset/0]
	]).

	:- public(debugging/0).
	:- mode(debugging, one).
	:- info(debugging/0, [
		comment is 'Reports current debugging settings, including breakpoints and log points.'
	]).

	:- public(debugging/1).
	:- mode(debugging(?entity_identifier), zero_or_more).
	:- info(debugging/1, [
		comment is 'Enumerates, by backtracking, all entities compiled in debug mode.',
		argnames is ['Entity']
	]).

	:- public(trace/0).
	:- mode(trace, one).
	:- info(trace/0, [
		comment is 'Starts tracing all calls compiled in debug mode.'
	]).

	:- public(notrace/0).
	:- mode(notrace, one).
	:- info(notrace/0, [
		comment is 'Stops tracing of calls compiled in debug mode. Debugger will still stop at defined breakpoints.'
	]).

	:- public(leash/1).
	:- mode(leash(+atom), one).
	:- mode(leash(+list(atom)), one).
	:- info(leash/1, [
		comment is 'Sets the debugger leash ports using an abbreviation (``none``, ``loose``, ``half``, ``tight``, or ``full``) or a list of ports (valid ports are ``fact``, ``rule``, ``call``, ``exit``, ``redo``, ``fail``, and ``exception``).',
		argnames is ['Ports']
	]).

	:- public(leashing/1).
	:- mode(leashing(?atom), zero_or_more).
	:- info(leashing/1, [
		comment is 'Enumerates, by backtracking, all leashed ports (valid ports are ``fact``, ``rule``, ``call``, ``exit``, ``redo``, ``fail``, and ``exception``).',
		argnames is ['Port']
	]).

	:- public((spy)/1).
	:- mode(spy(@spy_point), zero_or_one).
	:- mode(spy(@list(spy_point)), zero_or_one).
	:- info((spy)/1, [
		comment is 'Sets a predicate or clause breakpoint (removing any existing log point or breakpoint defined for the same location, or a list of breakpoints. Fails if a breakpoint is invalid.',
		argnames is ['Breakpoint']
	]).

	:- public(spying/1).
	:- mode(spying(?spy_point), zero_or_more).
	:- info(spying/1, [
		comment is 'Enumerates, by backtracking, all defined predicate and clause breakpoints.',
		argnames is ['Breakpoint']
	]).

	:- public((nospy)/1).
	:- mode(nospy(@var), one).
	:- mode(nospy(@spy_point), one).
	:- mode(nospy(@list(spy_point)), one).
	:- info((nospy)/1, [
		comment is 'Removes all matching predicate and clause breakpoints.',
		argnames is ['Breakpoint']
	]).

	:- public((spy)/3).
	:- mode(spy(+atom, +integer, @callable), zero_or_one).
	:- info((spy)/3, [
		comment is 'Sets a conditional or triggered breakpoint (removing any existing log point or breakpoint defined for the same location). The condition can be a unification count expression, a lambda expression, or another breakpoint. Fails if the breakpoint is invalid.',
		argnames is ['Entity', 'Line', 'Condition'],
		remarks is [
			'Unification count expression conditions' - '``>(Count)``, ``>=(Count)``, ``=:=(Count)``, ``=<(Count)``, ``<(Count)``, ``mod(M)``, and ``Count``.',
			'Lambda expression conditions' - '``[Count,N,Goal]>>Condition`` and ``[Goal]>>Condition`` where ``Count`` is the unification count, ``N`` is the invocation number, and ``Goal`` is the goal that unified with the clause head; ``Condition`` is called in the context of ``user``.',
			'Triggered breakpoint conditions' - '``Entity-Line``.'
		]
	]).

	:- public(spying/3).
	:- mode(spying(?atom, ?integer, ?callable), zero_or_more).
	:- info(spying/3, [
		comment is 'Enumerates, by backtracking, all conditional and triggered breakpoints.',
		argnames is ['Entity', 'Line', 'Condition']
	]).

	:- public((nospy)/3).
	:- mode(nospy(@term, @term, @term), one).
	:- info((nospy)/3, [
		comment is 'Removes all matching conditional and triggered breakpoints.',
		argnames is ['Entity', 'Line', 'Condition']
	]).

	:- public((spy)/4).
	:- mode(spy(@term, @term, @term, @term), one).
	:- info((spy)/4, [
		comment is 'Sets a context breakpoint.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public(spying/4).
	:- mode(spying(?term, ?term, ?term, ?term), zero_or_more).
	:- info(spying/4, [
		comment is 'Enumerates, by backtracking, all defined context breakpoints.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public((nospy)/4).
	:- mode(nospy(@term, @term, @term, @term), one).
	:- info((nospy)/4, [
		comment is 'Removes all matching context breakpoints.',
		argnames is ['Sender', 'This', 'Self', 'Goal']
	]).

	:- public(nospyall/0).
	:- mode(nospyall, one).
	:- info(nospyall/0, [
		comment is 'Removes all breakpoints and log points.',
		see_also is [reset/0]
	]).

	:- public(log/3).
	:- mode(log(@object_identifier, +integer, +atom), zero_or_one).
	:- mode(log(@category_identifier, +integer, +atom), zero_or_one).
	:- info(log/3, [
		comment is 'Sets a log point (removing any existing breakpoint defined for the same location). Fails if the log point is invalid.',
		argnames is ['Entity', 'Line', 'Message']
	]).

	:- public(logging/3).
	:- mode(logging(?object_identifier, ?integer, ?atom), zero_or_more).
	:- mode(logging(?category_identifier, ?integer, ?atom), zero_or_more).
	:- info(logging/3, [
		comment is 'Enumerates, by backtracking, all defined log points.',
		argnames is ['Entity', 'Line', 'Message']
	]).

	:- public(nolog/3).
	:- mode(nolog(@var_or(object_identifier), @var_or(integer), @var_or(atom)), one).
	:- mode(nolog(@var_or(category_identifier), @var_or(integer), @var_or(atom)), one).
	:- info(nolog/3, [
		comment is 'Removes all matching log points.',
		argnames is ['Entity', 'Line', 'Message']
	]).

	:- public(nologall/0).
	:- mode(nologall, one).
	:- info(nologall/0, [
		comment is 'Removes all log points.',
		see_also is [reset/0]
	]).

	:- public(write_max_depth/1).
	:- mode(write_max_depth(?non_negative_integer), zero_or_one).
	:- info(write_max_depth/1, [
		comment is 'Current term write maximum depth. When not defined, the backend default is used.',
		argnames is ['MaxDepth']
	]).

	:- public(set_write_max_depth/1).
	:- mode(set_write_max_depth(+non_negative_integer), one).
	:- info(set_write_max_depth/1, [
		comment is 'Sets the default term maximum write depth. For most backends, a value of zero means that the whole term is written.',
		argnames is ['MaxDepth']
	]).

:- end_protocol.
