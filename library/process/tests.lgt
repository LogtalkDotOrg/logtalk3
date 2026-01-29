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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-01-29,
		comment is 'Unit tests for the "process" library.'
	]).

	:- uses(process, [
		create/3, wait/2, kill/1, kill/2
	]).

	cover(process).

	:- if(os::operating_system_type(windows)).

		% Windows tests

		test(process_create_3_01, true(ground(Pid)), [cleanup(catch(kill(Pid), _, true))]) :-
			create(path('cmd.exe'), ['/c', 'echo', 'hello'], [process(Pid)]).

		test(process_create_3_02, true, [cleanup(catch(close(Out), _, true))]) :-
			create(path('cmd.exe'), ['/c', 'echo', 'hello'], [stdout(Out)]).

		test(process_wait_2_01, true((Status == exit(0) ; Status == 0))) :-
			create(path('cmd.exe'), ['/c', 'echo', 'hello'], [process(Pid)]),
			wait(Pid, Status).

		test(process_kill_1_01, true) :-
			create(path('cmd.exe'), ['/c', 'ping', '-n', '10', 'localhost'], [process(Pid)]),
			kill(Pid).

		test(process_kill_2_01, true) :-
			create(path('cmd.exe'), ['/c', 'ping', '-n', '10', 'localhost'], [process(Pid)]),
			kill(Pid, sigkill).

		test(process_kill_2_02, true) :-
			create(path('cmd.exe'), ['/c', 'ping', '-n', '10', 'localhost'], [process(Pid)]),
			kill(Pid, 9).

	:- else.

		% POSIX tests

		test(process_create_3_01, true(ground(Pid)), [cleanup(catch(kill(Pid), _, true))]) :-
			create('/bin/echo', ['hello'], [process(Pid)]).

		test(process_create_3_02, true, [cleanup(catch(close(Out), _, true))]) :-
			create('/bin/echo', ['hello'], [stdout(Out)]).

		test(process_wait_2_01, true((Status == exit(0) ; Status == 0))) :-
			create('/bin/echo', ['hello'], [process(Pid)]),
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

:- end_object.
