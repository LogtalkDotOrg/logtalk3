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


% Test pool object - imports the amqp_pool category
:- object(test_pool,
	imports(amqp_pool)).
:- end_object.


:- object(tests_pool,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-11,
		comment is 'Unit tests for the "amqp_pool" category.'
	]).

	cover(amqp_pool).

	% Clean up after each test
	cleanup :-
		catch(test_pool::destroy, _, true).

	% ==========================================================================
	% Pool Initialization Tests
	% ==========================================================================

	test(pool_initialize_default_01, true, [cleanup(cleanup)]) :-
		test_pool::initialize([host(localhost), port(5672)]).

	test(pool_initialize_with_sizes_01, true, [cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(2),
			max_size(5)
		]).

	test(pool_initialize_invalid_sizes_01, error(pool_error(invalid_config(_))), [cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(10),
			max_size(5)
		]).

	% ==========================================================================
	% Pool Stats Tests
	% ==========================================================================

	test(pool_stats_uninitialized_01, true(Stats == stats(0, 0, 0, 0, 0))) :-
		% Stats on uninitialized pool should return zeros
		catch(test_pool::destroy, _, true),
		test_pool::stats(Stats).

	test(pool_stats_initialized_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(2),
			max_size(5)
		]),
		test_pool::stats(stats(Available, InUse, Total, MinSize, MaxSize)),
		^^assertion(available, Available >= 0),
		^^assertion(in_use, InUse == 0),
		^^assertion(total, Total >= 0),
		^^assertion(min_size, MinSize == 2),
		^^assertion(max_size, MaxSize == 5).

	% ==========================================================================
	% Pool Acquire/Release Tests
	% ==========================================================================

	test(pool_acquire_not_initialized_01, error(pool_error(not_initialized)), [cleanup(cleanup)]) :-
		catch(test_pool::destroy, _, true),
		test_pool::acquire(_).

	test(pool_acquire_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::acquire(Connection),
		amqp::connection_alive(Connection),
		test_pool::release(Connection).

	test(pool_acquire_multiple_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::acquire(Conn1),
		test_pool::acquire(Conn2),
		test_pool::acquire(Conn3),
		% All should be in use
		test_pool::stats(stats(_, InUse, _, _, _)),
		^^assertion(in_use_after_acquire, InUse == 3),
		% Release all
		test_pool::release(Conn1),
		test_pool::release(Conn2),
		test_pool::release(Conn3),
		% All should be available now
		test_pool::stats(stats(Available, InUse2, _, _, _)),
		^^assertion(available_after_release, Available == 3),
		^^assertion(in_use_after_release, InUse2 == 0).

	test(pool_acquire_exhausted_01, error(pool_error(exhausted)), [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(2)
		]),
		test_pool::acquire(_Conn1),
		test_pool::acquire(_Conn2),
		% Pool is at max, this should fail
		test_pool::acquire(_Conn3).

	test(pool_release_reuse_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(2)
		]),
		test_pool::acquire(Conn1),
		test_pool::release(Conn1),
		% Should get the same connection back (or a pooled one)
		test_pool::acquire(Conn2),
		amqp::connection_alive(Conn2),
		test_pool::release(Conn2).

	% ==========================================================================
	% Pool with_connection Tests
	% ==========================================================================

	test(pool_with_connection_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::with_connection(amqp::connection_alive).

	test(pool_with_connection_channel_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::with_connection(test_channel_operations).

	test(pool_with_connection_release_on_failure_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		% Goal that fails - connection should still be released
		(	test_pool::with_connection(fail_goal) ->
			true
		;	true
		),
		test_pool::stats(stats(Available, _, _, _, _)),
		^^assertion(available_after_failure, Available > 0).

	test(pool_with_connection_release_on_exception_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		% Goal that throws - connection should still be released
		catch(
			test_pool::with_connection(throw_goal),
			_,
			true
		),
		test_pool::stats(stats(Available, _, _, _, _)),
		^^assertion(available_after_exception, Available > 0).

	% ==========================================================================
	% Pool Resize Tests
	% ==========================================================================

	test(pool_resize_not_initialized_01, error(pool_error(not_initialized)), [cleanup(cleanup)]) :-
		catch(test_pool::destroy, _, true),
		test_pool::resize(2, 10).

	test(pool_resize_invalid_01, error(pool_error(invalid_config(_))), [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::resize(10, 5).

	test(pool_resize_01, true, [condition(amqp_server_available), cleanup(cleanup)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::resize(2, 10),
		test_pool::stats(stats(_, _, _, MinSize, MaxSize)),
		^^assertion(min_size, MinSize == 2),
		^^assertion(max_size, MaxSize == 10).

	% ==========================================================================
	% Pool Destroy Tests
	% ==========================================================================

	test(pool_destroy_01, true, [condition(amqp_server_available)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(2),
			max_size(5)
		]),
		test_pool::acquire(Conn),
		test_pool::release(Conn),
		test_pool::destroy,
		% After destroy, stats should be zeros
		test_pool::stats(stats(Available, InUse, Total, MinSize, MaxSize)),
		^^assertion(available, Available == 0),
		^^assertion(in_use, InUse == 0),
		^^assertion(total, Total == 0),
		^^assertion(min_size, MinSize == 0),
		^^assertion(max_size, MaxSize == 0).

	test(pool_destroy_with_in_use_01, true, [condition(amqp_server_available)]) :-
		test_pool::initialize([
			host(localhost),
			port(5672),
			min_size(1),
			max_size(5)
		]),
		test_pool::acquire(_Conn),
		% Destroy while connection is in use
		test_pool::destroy,
		test_pool::stats(stats(Available, InUse, Total, MinSize, MaxSize)),
		^^assertion(available, Available == 0),
		^^assertion(in_use, InUse == 0),
		^^assertion(total, Total == 0),
		^^assertion(min_size, MinSize == 0),
		^^assertion(max_size, MaxSize == 0).

	% ==========================================================================
	% Helper Predicates
	% ==========================================================================

	test_channel_operations(Connection) :-
		amqp::channel_open(Connection, 1, Channel),
		amqp::channel_close(Channel).

	fail_goal(_) :-
		fail.

	throw_goal(_) :-
		throw(test_exception).

	% Auxiliary predicate to check if AMQP server is available
	amqp_server_available :-
		catch(
			(	socket::client_open(localhost, 5672, Input, Output, []),
				socket::close(Input, Output)
			),
			_,
			fail
		).

:- end_object.
