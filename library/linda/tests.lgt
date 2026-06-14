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
		version is 1:6:0,
		author is 'Paulo Moura',
		date is 2026-06-13,
		comment is 'Unit tests for the "linda" library.'
	]).

	:- threaded.

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
		client1::linda_client(Address),
		wait_for_client.

	cleanup :-
		(	test_server_address_(Address) ->
			catch(client2::close_client(Address), _, true),
			catch(client1::shutdown_server(Address), _, true),
			catch(client1::close_client(Address), _, true)
		;	true
		),
		retractall(test_server_address_(_)).

	wait_for_server :-
		wait_for_server(100).

	wait_for_server(_Attempts) :-
		test_server_address_(_),
		!.
	wait_for_server(0) :-
		!,
		throw(linda_error(server_start_timeout)).
	wait_for_server(Attempts) :-
		sleep(0.1),
		RemainingAttempts is Attempts - 1,
		wait_for_server(RemainingAttempts).

	wait_for_client :-
		wait_for_client(100).

	wait_for_client(_Attempts) :-
		catch(server<<client_engine_(_, _), _, fail),
		!.
	wait_for_client(0) :-
		!,
		throw(linda_error(client_start_timeout)).
	wait_for_client(Attempts) :-
		sleep(0.1),
		RemainingAttempts is Attempts - 1,
		wait_for_client(RemainingAttempts).

	wait_for_no_client_connections :-
		wait_for_no_client_connections(100).

	wait_for_no_client_connections(_Attempts) :-
		\+ server<<client_connection_(_, _, _),
		!.
	wait_for_no_client_connections(0) :-
		!,
		throw(linda_error(wait_for_no_client_connections_timeout)).
	wait_for_no_client_connections(Attempts) :-
		sleep(0.1),
		RemainingAttempts is Attempts - 1,
		wait_for_no_client_connections(RemainingAttempts).

	wait_for_max_client_connections(Maximum) :-
		wait_for_max_client_connections(Maximum, 100).

	wait_for_max_client_connections(Maximum, _Attempts) :-
		findall(ClientId, server<<client_connection_(ClientId, _, _), ClientIds),
		length(ClientIds, Count),
		Count =< Maximum,
		!.
	wait_for_max_client_connections(Maximum, 0) :-
		!,
		throw(linda_error(wait_for_max_client_connections_timeout(Maximum))).
	wait_for_max_client_connections(Maximum, Attempts) :-
		sleep(0.1),
		RemainingAttempts is Attempts - 1,
		wait_for_max_client_connections(Maximum, RemainingAttempts).

	reset_timeout :-
		client1::linda_timeout(_, off).

	reset_timeout_and_reconnect_client1 :-
		reset_timeout,
		(	test_server_address_(Address) ->
			catch(client1::close_client(Address), _, true),
			catch(client1::linda_client(Address), _, true),
			wait_for_client
		;	true
		).

	reset_timeout_and_close_client2 :-
		reset_timeout,
		(	test_server_address_(Address) ->
			catch(client2::close_client(Address), _, true)
		;	true
		).

	reconnect_client :-
		(	test_server_address_(Address) ->
			catch(client1::close_client(Address), _, true),
			catch(client1::linda_client(Address), _, true),
			wait_for_client
		;	true
		).

	close_client2 :-
		(	test_server_address_(Address) ->
			catch(client2::close_client(Address), _, true),
			wait_for_max_client_connections(1)
		;	true
		).

	delayed_out(Tuple) :-
		sleep(0.1),
		client2::out(Tuple).

	% ==========================================================================
	% Tuple out/in tests
	% ==========================================================================

	test(linda_out_01, true) :-
		client1::out(test_tuple(1, a)).

	test(linda_in_noblock_01, true(X == 1)) :-
		client1::out(value(1)),
		client1::in_noblock(value(X)).

	test(linda_in_noblock_02, false) :-
		client1::in_noblock(nonexistent_tuple(_)).

	test(linda_rd_noblock_01, true(X == hello)) :-
		client1::out(greeting(hello)),
		client1::rd_noblock(greeting(X)).

	test(linda_rd_noblock_02, true(X == hello)) :-
		% rd should not remove the tuple
		client1::out(persistent(hello)),
		client1::rd_noblock(persistent(X)),
		client1::rd_noblock(persistent(X)).

	test(linda_rd_noblock_03, fail) :-
		client1::rd_noblock(missing_tuple(_)).

	% ==========================================================================
	% Blocking in/1 tests
	% ==========================================================================

	test(linda_in_01, true(X == 42)) :-
		% Basic in/1 test - remove a tuple that exists
		client1::out(in_test_01(42)),
		client1::in(in_test_01(X)).

	test(linda_in_02, fail) :-
		% Verify in/1 removes the tuple
		client1::out(in_test_02(data)),
		client1::in(in_test_02(data)),
		client1::rd_noblock(in_test_02(data)).

	test(linda_in_03, true([X, Y] == [alice, bob])) :-
		% Pattern matching with variables
		client1::out(in_test_03(alice, 30)),
		client1::out(in_test_03(bob, 25)),
		client1::in(in_test_03(X, 30)),
		client1::in(in_test_03(Y, 25)).

	% ==========================================================================
	% Blocking rd/1 tests
	% ==========================================================================

	test(linda_rd_01, true(X == hello)) :-
		% Basic rd/1 test - read a tuple that exists
		client1::out(rd_test_01(hello)),
		client1::rd(rd_test_01(X)).

	test(linda_rd_02, true(X == world)) :-
		% Verify rd/1 does not remove the tuple
		client1::out(rd_test_02(world)),
		client1::rd(rd_test_02(X)),
		client1::rd(rd_test_02(X)).

	test(linda_rd_03, true([A, B] == [foo, bar])) :-
		% Pattern matching with variables
		client1::out(rd_test_03(foo, 1)),
		client1::out(rd_test_03(bar, 2)),
		client1::rd(rd_test_03(A, 1)),
		client1::rd(rd_test_03(B, 2)).

	% ==========================================================================
	% Multiple tuple tests
	% ==========================================================================

	test(linda_multiple_tuples_01, true) :-
		client1::out(item(1)),
		client1::out(item(2)),
		client1::out(item(3)),
		client1::in_noblock(item(1)),
		client1::in_noblock(item(2)),
		client1::in_noblock(item(3)).

	test(linda_pattern_matching_01, true(X == b)) :-
		client1::out(pair1(a, 1)),
		client1::out(pair1(b, 2)),
		client1::in_noblock(pair1(X, 2)).

	test(linda_pattern_matching_02, true(X == 2)) :-
		client1::out(n(1)),
		client1::out(n(2)),
		client1::in_noblock(n(_)),
		client1::in_noblock(n(X)).

	% ==========================================================================
	% in_list/2 and rd_list/2 tests
	% ==========================================================================

	test(linda_in_list_01, true(Tuple == task(1, a))) :-
		% Test in_list/2 removes first matching tuple from list
		client1::out(task(1, a)),
		client1::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_in_list_02, true(Tuple == task(2, b))) :-
		% Test in_list/2 matches second pattern in list
		client1::out(task(2, b)),
		client1::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_in_list_03, true(Tuple == done)) :-
		% Test in_list/2 matches last pattern in list
		client1::out(done),
		client1::in_list([task(1, X), task(2, X), done], Tuple).

	test(linda_rd_list_01, true(Tuple == status(ready))) :-
		% Test rd_list/2 reads first matching tuple from list
		client1::out(status(ready)),
		client1::rd_list([status(ready), status(waiting), status(done)], Tuple).

	test(linda_rd_list_02, true(Tuple == status(ready))) :-
		% Test rd_list/2 does not remove tuple
		client1::out(status(ready)),
		client1::rd_list([status(ready), status(waiting)], Tuple),
		client1::rd_list([status(ready), status(waiting)], Tuple).

	test(linda_rd_list_03, true(Tuple == msg(hello, world))) :-
		% Test rd_list/2 with pattern unification
		client1::out(msg(hello, world)),
		client1::rd_list([msg(_, world), msg(bye, world)], Tuple).

	% ==========================================================================
	% findall_rd_noblock tests
	% ==========================================================================

	test(linda_findall_rd_noblock_01, true(List == [1, 2, 3])) :-
		client1::out(num(1)),
		client1::out(num(2)),
		client1::out(num(3)),
		client1::findall_rd_noblock(N, num(N), List).

	test(linda_findall_rd_noblock_02, true(List == [])) :-
		client1::findall_rd_noblock(N, missing(N), List).

	test(linda_findall_rd_noblock_03, true(List == [a, b, c])) :-
		% Pattern matching with complex terms
		client1::out(pair2(1, a)),
		client1::out(pair2(2, b)),
		client1::out(pair2(3, c)),
		client1::findall_in_noblock(X, pair2(_, X), List).

	% ==========================================================================
	% findall_in_noblock tests
	% ==========================================================================

	test(linda_findall_in_noblock_01, true(List == [10, 20, 30])) :-
		client1::out(val(10)),
		client1::out(val(20)),
		client1::out(val(30)),
		client1::findall_in_noblock(N, val(N), List).

	test(linda_findall_in_noblock_02, false) :-
		% Verify tuples were removed
		client1::out(temp(1)),
		client1::findall_in_noblock(N, temp(N), _),
		client1::rd_noblock(temp(1)).

	test(linda_findall_in_noblock_03, true(List == [])) :-
		client1::findall_in_noblock(N, nonexistent(N), List).

	test(linda_findall_in_noblock_04, true(List == [a, b, c])) :-
		% Pattern matching with complex terms
		client1::out(data(1, a)),
		client1::out(data(2, b)),
		client1::out(data(3, c)),
		client1::findall_in_noblock(X, data(_, X), List).

	% ==========================================================================
	% Alias tests
	% ==========================================================================

	test(linda_server_alias_01, true(X == 1)) :-
		client1::out(blackboard, a(1)),
		client1::in(blackboard, a(X)).

	test(linda_server_alias_02, true(X == 1), [cleanup(close_client2)]) :-
		test_server_address_(Address),
		catch(client2::close_client(Address), _, true),
		client2::linda_client(Address, [alias(data)]),
		client2::out(data, b(1)),
		client2::in(data, b(X)).

	test(linda_server_alias_03, error(linda_error(not_connected(unknown)))) :-
		client1::out(unknown, c(1)).

	% ==========================================================================
	% Timeout tests
	% ==========================================================================

	test(linda_timeout_01, true(Old == off)) :-
		client1::linda_timeout(Old, 1:0).

	test(linda_timeout_02, true(Old == 1:0)) :-
		client1::linda_timeout(_, 1:0),
		client1::linda_timeout(Old, off).

	% tests are called from an initialization/1 directive, which can not be
	% interrupted by call_with_timeout/3 in SWI-Prolog and Trealla Prolog
	test(linda_timeout_03, error(linda_error(timeout)), [condition(current_logtalk_flag(prolog_dialect,xvm)), cleanup(reset_timeout_and_reconnect_client1)]) :-
		client1::linda_timeout(_, 0:100),
		client1::in(timeout_missing_tuple_03).

	test(linda_timeout_04, true, [cleanup(reset_timeout_and_close_client2)]) :-
		test_server_address_(Address),
		catch(client2::close_client(Address), _, true),
		client2::linda_client(Address),
		client1::linda_timeout(_, 1:0),
		threaded_once(delayed_out(timeout_available_tuple_04), Tag),
		client1::in(timeout_available_tuple_04),
		threaded_exit(delayed_out(timeout_available_tuple_04), Tag).

	test(linda_timeout_05, true(Old == 0:100), [cleanup(reset_timeout)]) :-
		client1::linda_timeout(_, 0:100),
		client1::linda_timeout(Old, off).

	% ==========================================================================
	% Blocking waiter wakeup tests
	% ==========================================================================

	test(linda_blocking_rd_04, true(Tuple == wake_rd_tuple), [cleanup(close_client2)]) :-
		test_server_address_(Address),
		catch(client2::close_client(Address), _, true),
		client2::linda_client(Address),
		threaded_once(delayed_out(wake_rd_tuple), Tag),
		client1::rd(wake_rd_tuple),
		Tuple = wake_rd_tuple,
		threaded_exit(delayed_out(wake_rd_tuple), Tag).

	test(linda_blocking_rd_list_04, true(Tuple == wake_rd_list_tuple), [cleanup(close_client2)]) :-
		test_server_address_(Address),
		catch(client2::close_client(Address), _, true),
		client2::linda_client(Address),
		threaded_once(delayed_out(wake_rd_list_tuple), Tag),
		client1::rd_list([ignored_tuple, wake_rd_list_tuple], Tuple),
		threaded_exit(delayed_out(wake_rd_list_tuple), Tag).

	% ==========================================================================
	% Error handling tests
	% ==========================================================================

	test(linda_not_connected_out_01, error(linda_error(not_connected(blackboard)))) :-
		catch(client1::close_client(blackboard), _, true),
		client1::out(test).

	test(linda_not_connected_in_01, error(linda_error(not_connected(blackboard)))) :-
		catch(client1::close_client(blackboard), _, true),
		client1::in(test).

	test(linda_not_connected_rd_01, error(linda_error(not_connected(blackboard)))) :-
		catch(client1::close_client(blackboard), _, true),
		client1::rd(test).

	% ==========================================================================
	% Other connection tests
	% ==========================================================================

	test(linda_client_connect_01, error(linda_error(already_connected)), [setup(reconnect_client)]) :-
		test_server_address_(Address),
		client1::linda_client(Address).

	test(linda_close_client_01, true) :-
		test_server_address_(Address),
		catch(client1::close_client(Address), _, true),
		client1::linda_client(Address),
		client1::close_client(Address).

	% ==========================================================================
	% Shutdown tests
	% ==========================================================================

	test(linda_shutdown_server_01, true(ConnectionRefused == yes)) :-
		test_server_address_(Address),
		catch(client1::close_client(Address), _, true),
		client1::linda_client(Address),
		client1::shutdown_server(Address),
		client1::close_client(Address),
		wait_for_no_client_connections,
		Address = _Host:Port,
		catch(
			(	socket::client_open('127.0.0.1', Port, Input, Output, [type(text)]),
				socket::close(Input, Output),
				ConnectionRefused = no
			),
			_,
			ConnectionRefused = yes
		),
		client1::close_client(Address).

:- end_object.
