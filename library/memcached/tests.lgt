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
		date is 2026-02-09,
		comment is 'Unit tests for the "memcached" library.'
	]).

	:- uses(list, [
		length/2, valid/1 as is_list/1
	]).

	cover(memcached).

	condition(memcached_server_available).

	% ==========================================================================
	% Connection Tests (require running Memcached server on localhost:11211)
	% ==========================================================================

	test(memcached_connect_disconnect_01, true) :-
		memcached::connect(localhost, 11211, Connection),
		memcached::disconnect(Connection).

	test(memcached_connect_default_port_01, true) :-
		memcached::connect(localhost, Connection),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Version Tests
	% ==========================================================================

	test(memcached_version_01, true(atom(Version))) :-
		memcached::connect(localhost, Connection),
		memcached::version(Connection, Version),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Storage and Retrieval Tests (set/get)
	% ==========================================================================

	test(memcached_set_get_01, true(Value == 'Hello, World!')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_01', 'Hello, World!', 0, 60),
		memcached::get(Connection, 'test_key_01', Value),
		memcached::disconnect(Connection).

	test(memcached_set_get_simple_01, true(Value == 'simple_value')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_02', 'simple_value'),
		memcached::get(Connection, 'test_key_02', Value),
		memcached::disconnect(Connection).

	test(memcached_set_get_flags_01, true(Flags == 42)) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_flags', 'flagged_value', 42, 60),
		memcached::get(Connection, 'test_key_flags', _, Flags),
		memcached::disconnect(Connection).

	test(memcached_set_overwrite_01, true(Value == 'new_value')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_overwrite', 'old_value'),
		memcached::set(Connection, 'test_key_overwrite', 'new_value'),
		memcached::get(Connection, 'test_key_overwrite', Value),
		memcached::disconnect(Connection).

	test(memcached_set_empty_value_01, true(Value == '')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_empty', ''),
		memcached::get(Connection, 'test_key_empty', Value),
		memcached::disconnect(Connection).

	test(memcached_get_miss_01, fail) :-
		memcached::connect(localhost, Connection),
		memcached::delete(Connection, 'nonexistent_key_xyz'),
		memcached::get(Connection, 'nonexistent_key_xyz', _),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Add Tests
	% ==========================================================================

	test(memcached_add_01, true(Value == 'added_value')) :-
		memcached::connect(localhost, Connection),
		% Ensure key doesn't exist
		(	memcached::delete(Connection, 'test_key_add') -> true ; true ),
		memcached::add(Connection, 'test_key_add', 'added_value', 0, 60),
		memcached::get(Connection, 'test_key_add', Value),
		memcached::disconnect(Connection).

	test(memcached_add_exists_01, error(memcached_error(not_stored))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_add_exists', 'existing'),
		memcached::add(Connection, 'test_key_add_exists', 'new_value', 0, 60),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Replace Tests
	% ==========================================================================

	test(memcached_replace_01, true(Value == 'replaced_value')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_replace', 'original'),
		memcached::replace(Connection, 'test_key_replace', 'replaced_value', 0, 60),
		memcached::get(Connection, 'test_key_replace', Value),
		memcached::disconnect(Connection).

	test(memcached_replace_missing_01, error(memcached_error(not_stored))) :-
		memcached::connect(localhost, Connection),
		(	memcached::delete(Connection, 'test_key_replace_miss') -> true ; true ),
		memcached::replace(Connection, 'test_key_replace_miss', 'value', 0, 60),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Append/Prepend Tests
	% ==========================================================================

	test(memcached_append_01, true(Value == 'hello_world')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_append', 'hello'),
		memcached::append(Connection, 'test_key_append', '_world'),
		memcached::get(Connection, 'test_key_append', Value),
		memcached::disconnect(Connection).

	test(memcached_prepend_01, true(Value == 'hello_world')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_prepend', '_world'),
		memcached::prepend(Connection, 'test_key_prepend', 'hello'),
		memcached::get(Connection, 'test_key_prepend', Value),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Gets/CAS Tests
	% ==========================================================================

	test(memcached_gets_01, true((Value == 'cas_value', integer(CasUnique)))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_gets', 'cas_value'),
		memcached::gets(Connection, 'test_key_gets', Value, CasUnique),
		memcached::disconnect(Connection).

	test(memcached_gets_with_flags_01, true((Value == 'cas_value', Flags == 7, integer(CasUnique)))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_gets_f', 'cas_value', 7, 60),
		memcached::gets(Connection, 'test_key_gets_f', Value, Flags, CasUnique),
		memcached::disconnect(Connection).

	test(memcached_cas_01, true(Value == 'updated_value')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_cas', 'original_value'),
		memcached::gets(Connection, 'test_key_cas', _, CasUnique),
		memcached::cas(Connection, 'test_key_cas', 'updated_value', 0, 60, CasUnique),
		memcached::get(Connection, 'test_key_cas', Value),
		memcached::disconnect(Connection).

	test(memcached_cas_conflict_01, error(memcached_error(exists))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_cas_conflict', 'original'),
		memcached::gets(Connection, 'test_key_cas_conflict', _, CasUnique),
		% Modify the value to invalidate the CAS token
		memcached::set(Connection, 'test_key_cas_conflict', 'modified_by_other'),
		% This should fail with EXISTS
		memcached::cas(Connection, 'test_key_cas_conflict', 'my_update', 0, 60, CasUnique),
		memcached::disconnect(Connection).

	test(memcached_cas_not_found_01, error(memcached_error(not_found))) :-
		memcached::connect(localhost, Connection),
		% Ensure key doesn't exist
		(	memcached::delete(Connection, 'test_key_cas_missing') -> true ; true ),
		% CAS on a non-existent key should fail with NOT_FOUND
		memcached::cas(Connection, 'test_key_cas_missing', 'value', 0, 60, 12345),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Delete Tests
	% ==========================================================================

	test(memcached_delete_01, true) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_delete', 'to_delete'),
		memcached::delete(Connection, 'test_key_delete'),
		memcached::disconnect(Connection).

	test(memcached_delete_verify_01, fail) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_delete_v', 'to_delete'),
		memcached::delete(Connection, 'test_key_delete_v'),
		memcached::get(Connection, 'test_key_delete_v', _),
		memcached::disconnect(Connection).

	test(memcached_delete_missing_01, fail) :-
		memcached::connect(localhost, Connection),
		(	memcached::delete(Connection, 'nonexistent_key_del') -> true ; true ),
		memcached::delete(Connection, 'nonexistent_key_del'),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Increment/Decrement Tests
	% ==========================================================================

	test(memcached_incr_01, true(NewValue == 15)) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_incr', '10'),
		memcached::incr(Connection, 'test_key_incr', 5, NewValue),
		memcached::disconnect(Connection).

	test(memcached_decr_01, true(NewValue == 5)) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_decr', '10'),
		memcached::decr(Connection, 'test_key_decr', 5, NewValue),
		memcached::disconnect(Connection).

	test(memcached_decr_underflow_01, true(NewValue == 0)) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_decr_uf', '5'),
		memcached::decr(Connection, 'test_key_decr_uf', 10, NewValue),
		memcached::disconnect(Connection).

	test(memcached_incr_missing_01, fail) :-
		memcached::connect(localhost, Connection),
		(	memcached::delete(Connection, 'nonexistent_key_incr') -> true ; true ),
		memcached::incr(Connection, 'nonexistent_key_incr', 1, _),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Touch Tests
	% ==========================================================================

	test(memcached_touch_01, true) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_touch', 'touch_value', 0, 60),
		memcached::touch(Connection, 'test_key_touch', 120),
		memcached::disconnect(Connection).

	test(memcached_touch_missing_01, fail) :-
		memcached::connect(localhost, Connection),
		(	memcached::delete(Connection, 'nonexistent_key_touch') -> true ; true ),
		memcached::touch(Connection, 'nonexistent_key_touch', 60),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Get-And-Touch Tests
	% ==========================================================================

	test(memcached_gat_01, true(Value == 'gat_value')) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_gat', 'gat_value', 0, 60),
		memcached::gat(Connection, 'test_key_gat', 120, Value),
		memcached::disconnect(Connection).

	test(memcached_gats_01, true((Value == 'gats_value', integer(CasUnique)))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'test_key_gats', 'gats_value', 0, 60),
		memcached::gats(Connection, 'test_key_gats', 120, Value, CasUnique),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Multi-Get Tests
	% ==========================================================================

	test(memcached_mget_01, true(length(Items, 3))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'mget_key_1', 'value_1'),
		memcached::set(Connection, 'mget_key_2', 'value_2'),
		memcached::set(Connection, 'mget_key_3', 'value_3'),
		memcached::mget(Connection, ['mget_key_1', 'mget_key_2', 'mget_key_3'], Items),
		memcached::disconnect(Connection).

	test(memcached_mget_partial_01, true(length(Items, 2))) :-
		memcached::connect(localhost, Connection),
		memcached::set(Connection, 'mget_key_a', 'value_a'),
		memcached::set(Connection, 'mget_key_b', 'value_b'),
		(	memcached::delete(Connection, 'mget_key_missing') -> true ; true ),
		memcached::mget(Connection, ['mget_key_a', 'mget_key_missing', 'mget_key_b'], Items),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Flush All Tests
	% ==========================================================================

	test(memcached_flush_all_01, true) :-
		memcached::connect(localhost, Connection),
		memcached::flush_all(Connection),
		memcached::disconnect(Connection).

	test(memcached_flush_all_delay_01, true) :-
		memcached::connect(localhost, Connection),
		memcached::flush_all(Connection, 1),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Stats Tests
	% ==========================================================================

	test(memcached_stats_01, true(Stats \== [])) :-
		memcached::connect(localhost, Connection),
		memcached::stats(Connection, Stats),
		memcached::disconnect(Connection).

	test(memcached_stats_items_01, true(is_list(Stats))) :-
		memcached::connect(localhost, Connection),
		memcached::stats(Connection, items, Stats),
		memcached::disconnect(Connection).

	% ==========================================================================
	% Connection Error Tests
	% ==========================================================================

	test(memcached_connect_refused_01, error(memcached_error(connection_failed))) :-
		% Try connecting to a port that should not have Memcached running
		memcached::connect(localhost, 19999, _).

	% auxiliary predicates

	memcached_server_available :-
		catch(
			(	socket::client_open(localhost, 11211, Input, Output, []),
				socket::close(Input, Output)
			),
			_,
			fail
		).

:- end_object.
