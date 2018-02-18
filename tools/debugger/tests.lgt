%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0.3,
		author is 'Paulo Moura',
		date is 2018/02/18,
		comment is 'Unit tests for the "debugger" tool.'
	]).

	:- uses(debugger, [
		reset/0,
		debug/0, nodebug/0,
		debugging/0, debugging/1,
		trace/0, notrace/0,
		(spy)/1, spying/1, (nospy)/1,
		(spy)/4, spying/4, (nospy)/4,
		nospyall/0,
		leash/1, leashing/1
	]).

	test(debugger_reset_0_01, deterministic) :-
		reset.

	% no spy points should be defined after calling reset/0
	test(debugger_reset_0_02, deterministic) :-
		spy(foo/1),
		spy(logtalk-13),
		spy(_, _, _, _),
		reset,
		\+ spying(_),
		\+ spying(_, _, _, _).

	test(debugger_debug_0_01, deterministic) :-
		debug.

	% calling debug/0 must not change the existing spy points
	test(debugger_debug_0_02, deterministic) :-
		reset,
		spy(foo/1),
		spy(logtalk-13),
		spy(user, logtalk, _, _),
		debug,
		spying(Name/Arity), Name == foo, Arity == 1,
		spying(Entity-Line), Entity == logtalk, Line == 13,
		spying(Sender, This, _, _), Sender == user, This = logtalk.

	test(debugger_nodebug_0_01, deterministic) :-
		nodebug.

	% calling nodebug/0 must not change the existing spy points
	test(debugger_nodebug_0_02, deterministic) :-
		reset,
		spy(foo/1),
		spy(logtalk-13),
		spy(user, logtalk, _, _),
		nodebug,
		spying(Name/Arity), Name == foo, Arity == 1,
		spying(Entity-Line), Entity == logtalk, Line == 13,
		spying(Sender, This, _, _), Sender == user, This = logtalk.

	% debugging/0 must be deterministic when there are no spy points to report
	test(debugger_debugging_0_01, deterministic) :-
		reset,
		debugging.

	% debugging/0 must be deterministic when there are spy points to report
	test(debugger_debugging_0_02, deterministic) :-
		reset,
		spy(foo/1),
		spy(logtalk-13),
		spy(user, logtalk, _, _),
		debugging.

	test(debugger_debugging_1_01, deterministic) :-
		\+ debugging(_).

	test(debugger_debugging_1_02, true) :-
		create_object(Object, [], [set_logtalk_flag(debug,on)], []),
		debugging(Entity),
		Object == Entity.

	test(debugger_trace_0_01, deterministic) :-
		trace.

	% calling trace/0 must not change the existing spy points
	test(debugger_trace_0_02, deterministic) :-
		reset,
		spy(foo/1),
		spy(logtalk-13),
		spy(user, logtalk, _, _),
		trace,
		spying(Name/Arity), Name == foo, Arity == 1,
		spying(Entity-Line), Entity == logtalk, Line == 13,
		spying(Sender, This, _, _), Sender == user, This = logtalk.

	test(debugger_notrace_0_01, deterministic) :-
		notrace.

	% calling trace/0 must not change the existing spy points
	test(debugger_notrace_0_02, deterministic) :-
		reset,
		spy(foo/1),
		spy(logtalk-13),
		spy(user, logtalk, _, _),
		notrace,
		spying(Name/Arity), Name == foo, Arity == 1,
		spying(Entity-Line), Entity == logtalk, Line == 13,
		spying(Sender, This, _, _), Sender == user, This = logtalk.

	test(debugger_nospyall_0_01, deterministic) :-
		nospyall.

	% calling nospyall/0 must delete all existing spy points
	test(debugger_nospyall_0_02, deterministic) :-
		reset,
		spy(foo/1),
		spy(logtalk-13),
		spy(user, logtalk, _, _),
		nospyall,
		\+ spying(_),
		\+ spying(_, _, _, _).

	test(debugger_leash_1_01, deterministic) :-
		leash(none),
		\+ leashing(_).

	test(debugger_leash_1_02, deterministic) :-
		leash(loose),
		setof(Port, leashing(Port), Ports),
		Ports == [call, fact, rule].

	test(debugger_leash_1_03, deterministic) :-
		leash(half),
		setof(Port, leashing(Port), Ports),
		Ports == [call, fact, redo, rule].

	test(debugger_leash_1_04, deterministic) :-
		leash(tight),
		setof(Port, leashing(Port), Ports),
		Ports == [call, exception, fact, fail, redo, rule].

	test(debugger_leash_1_05, deterministic) :-
		leash(full),
		setof(Port, leashing(Port), Ports),
		Ports == [call, exception, exit, fact, fail, redo, rule].

	test(debugger_leash_1_06, deterministic) :-
		set::powerset([fact,rule,call,exit,redo,fail,exception], PowerSet),
		forall(
			(	list::member(Set, PowerSet),
				sort(Set, SetSorted)
			),
			(	leash(Set),
				findall(Port, leashing(Port), Ports),
				sort(Ports, PortsSorted),
				PortsSorted == SetSorted
			)
		).

	test(debugger_spy_1_01, deterministic) :-
		spy(logtalk-13).

	% setting a line number spy point already set must still succeed deterministically
	test(debugger_spy_1_02, deterministic) :-
		reset,
		spy(logtalk-13),
		spy(logtalk-13).

	test(debugger_spy_1_03, deterministic) :-
		spy(foo/3).

	% setting a predicate spy point already set must still succeed deterministically
	test(debugger_spy_1_04, deterministic) :-
		reset,
		spy(foo/3),
		spy(foo/3).

	test(debugger_spy_1_05, deterministic) :-
		spy([]).

	% all spy point in a list must be set
	test(debugger_spy_1_06, deterministic) :-
		reset,
		spy([logtalk-27, bar/5]),
		setof(SpyPoint, spying(SpyPoint), SpyPoints),
		SpyPoints == [logtalk-27, bar/5].

	test(debugger_nospy_1_01, deterministic) :-
		reset,
		nospy(_).

	test(debugger_nospy_1_02, deterministic) :-
		reset,
		spy(logtalk-13),
		nospy(logtalk-13),
		\+ spying(_).

	test(debugger_nospy_1_03, deterministic) :-
		reset,
		spy(foo/3),
		nospy(foo/3),
		\+ spying(_).

	test(debugger_nospy_1_04, deterministic) :-
		reset,
		spy(logtalk-13),
		spy(foo/3),
		nospy(_),
		\+ spying(_).

	test(debugger_nospy_4_01, deterministic) :-
		reset,
		nospy(_, _, _, _).

	test(debugger_nospy_4_02, deterministic) :-
		reset,
		spy(user, logtalk, _, _),
		nospy(user, logtalk, _, _),
		\+ spying(_, _, _, _).

	test(debugger_nospy_4_03, deterministic) :-
		reset,
		spy(user, logtalk, _, _),
		nospy(_, _, _, _),
		\+ spying(_, _, _, _).

	% supress all messages from the "debugger" component
	% to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, debugger, _Tokens).

:- end_object.
