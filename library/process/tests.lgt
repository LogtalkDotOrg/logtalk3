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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2026-11-05,
		comment is 'Unit tests for the "process" library.'
	]).

	:- uses(process, [
		create/3, wait/2, wait/3, kill/1, kill/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		member/2
	]).

	cover(process).

	:- if(os::operating_system_type(windows)).

		% Windows tests

		test(process_create_3_01, true(ground(Pid)), [cleanup(catch(kill(Pid), _, true))]) :-
			create('cmd.exe', ['/c', 'echo', 'hello'], [process(Pid)]).

		test(process_create_3_02, true, [cleanup(catch(close(Out), _, true))]) :-
			create('cmd.exe', ['/c', 'echo', 'hello'], [stdout(Out)]).

		test(process_create_3_03, true, [cleanup(catch(close(Out), _, true))]) :-
			create('cmd.exe', ['/c', 'echo', 'hello'], [stdout(Out), type(text)]).

		test(process_create_3_04, true, [cleanup(catch(close(Out), _, true))]) :-
			create('cmd.exe', ['/c', 'echo', '42'], [stdout(Out), type(binary)]).

		test(process_wait_2_01, true((Status == exit(0) ; Status == 0))) :-
			create('cmd.exe', ['/c', 'echo', 'hello'], [process(Pid)]),
			wait(Pid, Status).

		test(process_wait_2_02, true((Status \== exit(0), Status \== 0))) :-
			create('cmd.exe', ['/c', 'dir', 'non_existant'], [process(Pid)]),
			wait(Pid, Status).

		test(process_kill_1_01, true) :-
			create('cmd.exe', ['/c', 'ping', '-n', '10', 'localhost'], [process(Pid)]),
			kill(Pid).

		test(process_kill_2_01, true) :-
			create('cmd.exe', ['/c', 'ping', '-n', '10', 'localhost'], [process(Pid)]),
			kill(Pid, sigkill).

		test(process_kill_2_02, true) :-
			create('cmd.exe', ['/c', 'ping', '-n', '10', 'localhost'], [process(Pid)]),
			kill(Pid, 9).

	:- else.

		% POSIX tests

		test(process_create_3_01, true(ground(Pid)), [cleanup(catch(kill(Pid), _, true))]) :-
			create('/bin/echo', ['hello'], [process(Pid)]).

		test(process_create_3_02, true, [cleanup(catch(close(Out), _, true))]) :-
			create('/bin/echo', ['hello'], [stdout(Out)]).

		test(process_create_3_03, true, [cleanup(catch(close(Out), _, true))]) :-
			create('/bin/echo', ['hello'], [stdout(Out), type(text)]).

		test(process_create_3_04, true, [cleanup(catch(close(Out), _, true))]) :-
			create('/bin/echo', ['42'], [stdout(Out), type(binary)]).

		test(process_wait_2_01, true((Status == exit(0) ; Status == 0))) :-
			create('/bin/echo', ['hello'], [process(Pid)]),
			wait(Pid, Status).

		test(process_wait_2_02, true((Status \== exit(0), Status \== 0))) :-
			create('/bin/ls', ['non_existant'], [process(Pid)]),
			wait(Pid, Status).

		test(process_kill_1_01, true) :-
			create('/bin/sleep', ['10'], [process(Pid)]),
			kill(Pid).

		test(process_kill_2_01, true) :-
			create('/bin/sleep', ['10'], [process(Pid)]),
			kill(Pid, sigkill).

		test(process_kill_2_02, true) :-
			create('/bin/sleep', ['10'], [process(Pid)]),
			kill(Pid, 9).

	:- endif.

	test(process_wait_3_exit_codes, all(check_exit_code(Code)), [
		condition(\+ current_logtalk_flag(prolog_dialect, gnu)),
		note('GNU shell-command argument round-trips require the deferred serializer repair.')
	]) :-
		member(Code, [0, 1, 10, 20, 30]).

	test(process_wait_3_exit_127, true, [condition(\+ current_logtalk_flag(prolog_dialect, gnu))]) :-
		check_exit_code(127).

	test(process_wait_3_unknown_status, true) :-
		process << normalize_status(unrecognized(status), Status),
		assertion(Status == unknown(unrecognized(status))).

	test(process_wait_3_conflicting_status, false) :-
		process << normalize_status(exit(0), unknown(exit(0))).

	test(process_wait_3_killed, true, [condition(\+ os::operating_system_type(windows)), cleanup(reap_child(Pid))]) :-
		create('/bin/sleep', ['10'], [process(Pid)]),
		kill(Pid, sigkill),
		wait(Pid, RawStatus, ExitStatus),
		expected_killed_status(RawStatus, Expected),
		assertion(ExitStatus == Expected).

	test(process_create_stdout_contents, true(Char == a), [cleanup((catch(close(Out), _, true), reap_child(Pid)))]) :-
		(	os::operating_system_type(windows) ->
			create('cmd.exe', ['/c', 'echo', 'a'], [stdout(Out), process(Pid)])
		;	create('/bin/echo', ['hello.'], [stdout(Out), process(Pid)])
		),
		get_char(Out, Char),
		wait(Pid, _).

	% auxiliary predicates

	reap_child(Pid) :-
		(	nonvar(Pid) ->
			catch(ignore(wait(Pid, _)), _, true)
		;	true
		).

	check_exit_code(Code) :-
		number_codes(Code, Codes),
		atom_codes(CodeAtom, Codes),
		atom_concat('exit ', CodeAtom, Command),
		(	os::operating_system_type(windows) ->
			create('cmd.exe', ['/d', '/c', Command], [process(Pid)])
		;	create('/bin/sh', ['-c', Command], [process(Pid)])
		),
		wait(Pid, RawStatus, ExitStatus),
		expected_raw_status(Code, ExpectedRawStatus),
		assertion(RawStatus == ExpectedRawStatus),
		expected_exit_status(Code, RawStatus, Expected),
		assertion(ExitStatus == Expected).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		expected_raw_status(Code, RawStatus) :-
			(	os::operating_system_type(windows) ->
				RawStatus = Code
			;	RawStatus is Code * 256
			).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		expected_raw_status(Code, Code).

	:- else.

		expected_raw_status(Code, exit(Code)).

	:- endif.

	:- if(current_logtalk_flag(prolog_dialect, gnu)).

		expected_exit_status(Code, RawStatus, ExitStatus) :-
			(	os::operating_system_type(windows) ->
				ExitStatus = exit(Code)
			;	Code =:= 0 ->
				ExitStatus = exit(0)
			;	ExitStatus = unknown(RawStatus)
			).

		expected_killed_status(RawStatus, unknown(RawStatus)).

	:- else.

		expected_exit_status(Code, _, exit(Code)).

		expected_killed_status(_, killed(9)).

	:- endif.

:- end_object.
