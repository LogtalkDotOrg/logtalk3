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
		version is 1:5:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Unit tests for the "linda" library.'
	]).

	:- uses(os, [
		sleep/1
	]).

	cover(linda_server).
	cover(linda_client).
	cover(linda).

	% ==========================================================================
	% Test setup and cleanup
	% ==========================================================================

	% Server port for tests (will be dynamically assigned)
	:- private(test_server_address_/1).
	:- dynamic(test_server_address_/1).

	setup :-
		server::linda([Address-assertz(test_server_address_(Address))]),
		wait_for_server,
		% Connect to the server for client tests
		test_server_address_(Address),
		client::linda_client(Address),
		wait_for_client.

	cleanup :-
		% Clean up any remaining connections;
		catch(
			ignore((
				test_server_address_(Address),
				client::linda_client(Address),
				client::shutdown_server(Address),
				client::close_client(Address)
			)),
			_,
			true
		),
		retractall(test_server_address_(_)).

	wait_for_server :-
		(	test_server_address_(_) ->
			true
		;	sleep(0.1),
			wait_for_server
		).

	wait_for_client :-
		(	catch(server<<client_engine_(_, _), _, fail) ->
			true
		;	sleep(0.1),
			wait_for_client
		).

	% ==========================================================================
	% Tuple out/in tests
	% ==========================================================================

	test(linda_out_01, true) :-
		client::out(test_tuple(1, a)).

	test(linda_in_noblock_01, true(X == 1)) :-
		client::out(value(1)),
		client::in_noblock(value(X)).

	test(linda_in_noblock_02, false) :-
		client::in_noblock(nonexistent_tuple(_)).

	test(linda_rd_noblock_01, true(X == hello)) :-
		client::out(greeting(hello)),
		client::rd_noblock(greeting(X)).

	test(linda_rd_noblock_02, true(X == hello)) :-
		% rd should not remove the tuple
		client::out(persistent(hello)),
		client::rd_noblock(persistent(X)),
		client::rd_noblock(persistent(X)).

	test(linda_rd_noblock_03, fail) :-
		client::rd_noblock(missing_tuple(_)).

	% ==========================================================================
	% Blocking in/1 tests
	% ==========================================================================

	test(linda_in_01, true(X == 42)) :-
		% Basic in/1 test - remove a tuple that exists
		client::out(in_test_01(42)),
		client::in(in_test_01(X)).

	test(linda_in_02, fail) :-
		% Verify in/1 removes the tuple
		client::out(in_test_02(data)),
		client::in(in_test_02(data)),
		client::rd_noblock(in_test_02(data)).

	test(linda_in_03, true([X, Y] == [alice, bob])) :-
		% Pattern matching with variables
		client::out(in_test_03(alice, 30)),
		client::out(in_test_03(bob, 25)),
		client::in(in_test_03(X, 30)),
		client::in(in_test_03(Y, 25)).

	% ==========================================================================
	% Blocking rd/1 tests
	% ==========================================================================

	test(linda_rd_01, true(X == hello)) :-
		% Basic rd/1 test - read a tuple that exists
		client::out(rd_test_01(hello)),
		client::rd(rd_test_01(X)).

	test(linda_rd_02, true(X == world)) :-
		% Verify rd/1 does not remove the tuple
		client::out(rd_test_02(world)),
		client::rd(rd_test_02(X)),
		client::rd(rd_test_02(X)).

	test(linda_rd_03, true([A, B] == [foo, bar])) :-
		% Pattern matching with variables
		client::out(rd_test_03(foo, 1)),
		client::out(rd_test_03(bar, 2)),
		client::rd(rd_test_03(A, 1)),
		client::rd(rd_test_03(B, 2)).

	% ==========================================================================
	% Multiple tuple tests
	% ==========================================================================

	test(linda_multiple_tuples_01, true) :-
		client::out(item(1)),
		client::out(item(2)),
		client::out(item(3)),
		client::in_noblock(item(1)),
		client::in_noblock(item(2)),
		client::in_noblock(item(3)).

	test(linda_pattern_matching_01, true(X == b)) :-
		client::out(pair1(a, 1)),
		client::out(pair1(b, 2)),
		client::in_noblock(pair1(X, 2)).

	test(linda_pattern_matching_02, true(X == 2)) :-
		client::out(n(1)),
		client::out(n(2)),
		client::in_noblock(n(_)),
		client::in_noblock(n(X)).

	% ==========================================================================
	% in_list/2 and rd_list/2 tests
	% ==========================================================================

	test(linda_in_list_01, true(Tuple == task(1, a))) :-
		% Test in_list/2 removes first matching tuple from list
		client::out(task(1, a)),
		client::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_in_list_02, true(Tuple == task(2, b))) :-
		% Test in_list/2 matches second pattern in list
		client::out(task(2, b)),
		client::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_in_list_03, true(Tuple == done)) :-
		% Test in_list/2 matches last pattern in list
		client::out(done),
		client::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_rd_list_01, true(Tuple == status(ready))) :-
		% Test rd_list/2 reads first matching tuple from list
		client::out(status(ready)),
		client::rd_list([status(ready), status(waiting), status(done)], Tuple).

	test(linda_rd_list_02, true(Tuple == status(ready))) :-
		% Test rd_list/2 does not remove tuple
		client::out(status(ready)),
		client::rd_list([status(ready), status(waiting)], Tuple),
		client::rd_list([status(ready), status(waiting)], Tuple).

	test(linda_rd_list_03, true(Tuple == msg(hello, world))) :-
		% Test rd_list/2 with pattern unification
		client::out(msg(hello, world)),
		client::rd_list([msg(_, world), msg(bye, world)], Tuple).

	% ==========================================================================
	% findall_rd_noblock tests
	% ==========================================================================

	test(linda_findall_rd_noblock_01, true(List == [1, 2, 3])) :-
		client::out(num(1)),
		client::out(num(2)),
		client::out(num(3)),
		client::findall_rd_noblock(N, num(N), List).

	test(linda_findall_rd_noblock_02, true(List == [])) :-
		client::findall_rd_noblock(N, missing(N), List).

	test(linda_findall_rd_noblock_03, true(List == [a, b, c])) :-
		% Pattern matching with complex terms
		client::out(pair2(1, a)),
		client::out(pair2(2, b)),
		client::out(pair2(3, c)),
		client::findall_in_noblock(X, pair2(_, X), List).

	% ==========================================================================
	% findall_in_noblock tests
	% ==========================================================================

	test(linda_findall_in_noblock_01, true(List == [10, 20, 30])) :-
		client::out(val(10)),
		client::out(val(20)),
		client::out(val(30)),
		client::findall_in_noblock(N, val(N), List).

	test(linda_findall_in_noblock_02, false) :-
		% Verify tuples were removed
		client::out(temp(1)),
		client::findall_in_noblock(N, temp(N), _),
		client::rd_noblock(temp(1)).

	test(linda_findall_in_noblock_03, true(List == [])) :-
		client::findall_in_noblock(N, nonexistent(N), List).

	test(linda_findall_in_noblock_04, true(List == [a, b, c])) :-
		% Pattern matching with complex terms
		client::out(data(1, a)),
		client::out(data(2, b)),
		client::out(data(3, c)),
		client::findall_in_noblock(X, data(_, X), List).

	% ==========================================================================
	% Alias tests
	% ==========================================================================

	test(linda_server_alias_01, true(X == 1)) :-
		client::out(blackboard, a(1)),
		client::in(blackboard, a(X)).

	test(linda_server_alias_02, true(X == 1)) :-
		test_server_address_(Address),
		catch(client::close_client(Address), _, true),
		client::linda_client(Address, [alias(data)]),
		client::out(data, b(1)),
		client::in(data, b(X)).

	test(linda_server_alias_03, error(linda_error(not_connected(unknown)))) :-
		client::out(unknown, c(1)).

	% ==========================================================================
	% Timeout tests
	% ==========================================================================

	test(linda_timeout_01, true(Old == off)) :-
		client::linda_timeout(Old, 1:0).

	test(linda_timeout_02, true(Old == 1:0)) :-
		client::linda_timeout(_, 1:0),
		client::linda_timeout(Old, off).

	% ==========================================================================
	% Error handling tests
	% ==========================================================================

	test(linda_not_connected_out_01, error(linda_error(not_connected(blackboard)))) :-
		catch(client::close_client(blackboard), _, true),
		client::out(test).

	test(linda_not_connected_in_01, error(linda_error(not_connected(blackboard)))) :-
		catch(client::close_client(blackboard), _, true),
		client::in(test).

	test(linda_not_connected_rd_01, error(linda_error(not_connected(blackboard)))) :-
		catch(client::close_client(blackboard), _, true),
		client::rd(test).

	% ==========================================================================
	% Other connection tests
	% ==========================================================================

	test(linda_client_connect_01, error(linda_error(already_connected))) :-
		test_server_address_(Address),
		client::linda_client(Address).

	test(linda_close_client_01, true) :-
		test_server_address_(Address),
		catch(client::close_client(Address), _, true),
		client::linda_client(Address),
		client::close_client(Address).

	% ==========================================================================
	% Shutdown tests
	% ==========================================================================

	test(linda_shutdown_server_01, true(ConnectionRefused == yes)) :-
		test_server_address_(Address),
		catch(client::close_client(Address), _, true),
		client::linda_client(Address),
		client::shutdown_server(Address),
		Address = Host:Port,
		catch(
			(	socket::client_open(Host, Port, Input, Output, [type(text)]),
				socket::close(Input, Output),
				ConnectionRefused = no
			),
			_,
			ConnectionRefused = yes
		),
		client::close_client(Address).

:- end_object.
