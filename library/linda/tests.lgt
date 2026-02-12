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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-12,
		comment is 'Unit tests for the "linda" library.'
	]).

	:- threaded.

	:- uses(os, [
		sleep/1
	]).

	cover(linda).

	% ==========================================================================
	% Test setup and cleanup
	% ==========================================================================

	% Server port for tests (will be dynamically assigned)
	:- private(test_host_port_/1).
	:- dynamic(test_host_port_/1).

	setup :-
		% Start server in background thread
		threaded_ignore(start_test_server).

	cleanup :-
		% Clean up any remaining connections;
		catch(linda::shutdown_server, _, true),
		% Currently we need a client connection after the shutdown request to ensure that it's processed
		test_host_port_(Address),
		linda::linda_client(Address),
		catch(linda::close_client, _, true),
		retractall(test_host_port_(_)).

	start_test_server :-
		linda::linda([Address-assertz(test_host_port_(Address))]).

	wait_for_server :-
		(	test_host_port_(_) ->
			true
		;	sleep(0.1),
			wait_for_server
		).

	% ==========================================================================
	% Basic connection tests
	% ==========================================================================

	test(linda_client_connect_01, true) :-
		wait_for_server,
		test_host_port_(Address),
		linda::linda_client(Address).

	test(linda_client_connect_02, error(linda_error(already_connected))) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::linda_client(Address).

	test(linda_close_client_01, true) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::close_client.

	% ==========================================================================
	% Tuple out/in tests
	% ==========================================================================

	test(linda_out_01, true) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(test_tuple(1, a)).

	test(linda_in_noblock_01, true(X == 1)) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(value(1)),
		linda::in_noblock(value(X)).

	test(linda_in_noblock_02, false) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::in_noblock(nonexistent_tuple(_)).

	test(linda_rd_noblock_01, true(X == hello)) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(greeting(hello)),
		linda::rd_noblock(greeting(X)).

	test(linda_rd_noblock_02, true(X == hello)) :-
		% rd should not remove the tuple
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(persistent(hello)),
		linda::rd_noblock(persistent(X)),
		linda::rd_noblock(persistent(X)).

	test(linda_rd_noblock_03, fail) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::rd_noblock(missing_tuple(_)).

	% ==========================================================================
	% Blocking in/1 tests
	% ==========================================================================

	test(linda_in_01, true(X == 42)) :-
		% Basic in/1 test - remove a tuple that exists
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(in_test_01(42)),
		linda::in(in_test_01(X)).

	test(linda_in_02, fail) :-
		% Verify in/1 removes the tuple
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(in_test_02(data)),
		linda::in(in_test_02(data)),
		linda::rd_noblock(in_test_02(data)).

	test(linda_in_03, true([X, Y] == [alice, bob])) :-
		% Pattern matching with variables
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(in_test_03(alice, 30)),
		linda::out(in_test_03(bob, 25)),
		linda::in(in_test_03(X, 30)),
		linda::in(in_test_03(Y, 25)).

	% ==========================================================================
	% Blocking rd/1 tests
	% ==========================================================================

	test(linda_rd_01, true(X == hello)) :-
		% Basic rd/1 test - read a tuple that exists
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(rd_test_01(hello)),
		linda::rd(rd_test_01(X)).

	test(linda_rd_02, true(X == world)) :-
		% Verify rd/1 does not remove the tuple
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(rd_test_02(world)),
		linda::rd(rd_test_02(X)),
		linda::rd(rd_test_02(X)).

	test(linda_rd_03, true([A, B] == [foo, bar])) :-
		% Pattern matching with variables
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(rd_test_03(foo, 1)),
		linda::out(rd_test_03(bar, 2)),
		linda::rd(rd_test_03(A, 1)),
		linda::rd(rd_test_03(B, 2)).

	% ==========================================================================
	% Multiple tuple tests
	% ==========================================================================

	test(linda_multiple_tuples_01, true) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(item(1)),
		linda::out(item(2)),
		linda::out(item(3)),
		linda::in_noblock(item(1)),
		linda::in_noblock(item(2)),
		linda::in_noblock(item(3)).

	test(linda_pattern_matching_01, true(X == b)) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(pair(a, 1)),
		linda::out(pair(b, 2)),
		linda::in_noblock(pair(X, 2)).

	% ==========================================================================
	% in_list/2 and rd_list/2 tests
	% ==========================================================================

	test(linda_in_list_01, true(Tuple == task(1, a))) :-
		% Test in_list/2 removes first matching tuple from list
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(task(1, a)),
		linda::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_in_list_02, true(Tuple == task(2, b))) :-
		% Test in_list/2 matches second pattern in list
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(task(2, b)),
		linda::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_in_list_03, true(Tuple == done)) :-
		% Test in_list/2 matches last pattern in list
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(done),
		linda::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_rd_list_01, true(Tuple == status(ready))) :-
		% Test rd_list/2 reads first matching tuple from list
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(status(ready)),
		linda::rd_list([status(ready), status(waiting), status(done)], Tuple).

	test(linda_rd_list_02, true(Tuple == status(ready))) :-
		% Test rd_list/2 does not remove tuple
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(status(ready)),
		linda::rd_list([status(ready), status(waiting)], Tuple),
		linda::rd_list([status(ready), status(waiting)], Tuple).

	test(linda_rd_list_03, true(Tuple == msg(hello, world))) :-
		% Test rd_list/2 with pattern unification
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(msg(hello, world)),
		linda::rd_list([msg(_, world), msg(bye, world)], Tuple).

	% ==========================================================================
	% findall_rd_noblock tests
	% ==========================================================================

	test(linda_findall_rd_noblock_01, true(List == [1, 2, 3])) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(num(1)),
		linda::out(num(2)),
		linda::out(num(3)),
		linda::findall_rd_noblock(N, num(N), List).

	test(linda_findall_rd_noblock_02, true(List == [])) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::findall_rd_noblock(N, missing(N), List).

	test(linda_findall_rd_noblock_03, true(List == [a, b, c])) :-
		% Pattern matching with complex terms
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(pair(1, a)),
		linda::out(pair(2, b)),
		linda::out(pair(3, c)),
		linda::findall_in_noblock(X, pair(_, X), List).

	% ==========================================================================
	% findall_in_noblock tests
	% ==========================================================================

	test(linda_findall_in_noblock_01, true(List == [10, 20, 30])) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(val(10)),
		linda::out(val(20)),
		linda::out(val(30)),
		linda::findall_in_noblock(N, val(N), List).

	test(linda_findall_in_noblock_02, false) :-
		% Verify tuples were removed
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(temp(1)),
		linda::findall_in_noblock(N, temp(N), _),
		linda::rd_noblock(temp(1)).

	test(linda_findall_in_noblock_03, true(List == [])) :-
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::findall_in_noblock(N, nonexistent(N), List).

	test(linda_findall_in_noblock_04, true(List == [a, b, c])) :-
		% Pattern matching with complex terms
		wait_for_server,
		test_host_port_(Address),
		catch(linda::close_client, _, true),
		linda::linda_client(Address),
		linda::out(data(1, a)),
		linda::out(data(2, b)),
		linda::out(data(3, c)),
		linda::findall_in_noblock(X, data(_, X), List).

	% ==========================================================================
	% Timeout tests
	% ==========================================================================

	test(linda_timeout_01, true(Old == off)) :-
		linda::linda_timeout(Old, 1:0).

	test(linda_timeout_02, true(Old == 1:0)) :-
		linda::linda_timeout(_, 1:0),
		linda::linda_timeout(Old, off).

	% ==========================================================================
	% Error handling tests
	% ==========================================================================

	test(linda_not_connected_out_01, error(linda_error(not_connected))) :-
		catch(linda::close_client, _, true),
		linda::out(test).

	test(linda_not_connected_in_01, error(linda_error(not_connected))) :-
		catch(linda::close_client, _, true),
		linda::in(test).

	test(linda_not_connected_rd_01, error(linda_error(not_connected))) :-
		catch(linda::close_client, _, true),
		linda::rd(test).

:- end_object.
