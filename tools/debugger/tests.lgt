%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2016/10/14,
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
		leash/1
	]).

	:- uses(lgtunit, [
		deterministic/1
	]).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	test(debugger_reset_0_01) :-
		deterministic(reset).

	test(debugger_debug_0_01) :-
		deterministic(debug).

	test(debugger_nodebug_0_01) :-
		deterministic(nodebug).

	test(debugger_debugging_0_01) :-
		deterministic(debugging).

	test(debugger_debugging_1_01) :-
		create_object(Object, [], [set_logtalk_flag(debug,on)], []),
		deterministic(debugging(Object)).

	test(debugger_trace_0_01) :-
		deterministic(trace).

	test(debugger_notrace_0_01) :-
		deterministic(notrace).

	test(debugger_nospyall_0_01) :-
		deterministic(nospyall).

	test(debugger_leash_1_01) :-
		deterministic(leash(none)).

	test(debugger_leash_1_02) :-
		deterministic(leash(loose)).

	test(debugger_leash_1_03) :-
		deterministic(leash(half)).

	test(debugger_leash_1_04) :-
		deterministic(leash(tight)).

	test(debugger_leash_1_05) :-
		deterministic(leash(full)).

	test(debugger_leash_1_06) :-
		set::powerset([fact,rule,call,exit,redo,fail,exception], PowerSet),
		forall(
			list::member(Set, PowerSet),
			deterministic(leash(Set))
		).

	test(debugger_spy_1_01) :-
		deterministic(spy(logtalk-13)).

	test(debugger_spy_1_02) :-
		deterministic(spy(foo/3)).

	test(debugger_spy_1_03) :-
		deterministic(spy([])).

	test(debugger_spy_1_04) :-
		deterministic(spy([logtalk-27, bar/5])).

	test(debugger_nospy_1_01) :-
		deterministic(nospy(logtalk-13)).

	test(debugger_nospy_1_02) :-
		deterministic(nospy(foo/3)).

	test(debugger_nospy_1_03) :-
		deterministic(nospy(_)).

	test(debugger_spy_4_01) :-
		deterministic(nospy(user, logtalk, _, _)).

	test(debugger_nospy_4_01) :-
		deterministic(nospy(user, logtalk, _, _)).

	test(debugger_nospy_4_02) :-
		deterministic(nospy(_, _, _, _)).

	% supress all messages from the "dead_code_scanner"
	% component to not pollute the unit tests output

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, debugger, _Tokens).

:- end_object.
