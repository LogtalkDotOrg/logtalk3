%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:3:0,
		author is 'Sean Charles. Adapted to Logtalk by Paulo Moura',
		date is 2020-05-27,
		comment is 'Unit tests for the "redis" library.'
	]).

	:- uses(redis, [
		connect/1, connect/3,
		send/3,
		disconnect/1
	]).

	:- uses(list, [
		length/2
	]).

	:- if(current_logtalk_flag(prolog_dialect, ciao)).
		:- use_module(system, [pause/1 as sleep/1]).
	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(system, [sleep/1]).
	:- elif(current_logtalk_flag(prolog_dialect, qp)).
		:- uses(user, [thread_sleep/1 as sleep/1]).
	:- else.
		% ECLiPSe, GNU Prolog, SWI-Prolog, and XSB provide sleep/1
		% as a built-in predicate but we list it here for clarity
		:- uses(user, [sleep/1]).
	:- endif.

	% only run the tests if there's a Redis server running on localhost
	condition :-
		catch(
			(connect(Connection), disconnect(Connection)),
			_,
			fail
		).

	%% CONNECTION...

	test(default_connection_and_echo) :-
		connect(Connection),
		send(Connection, echo('GNU Prolog rocks!'), bulk(Output)),
		disconnect(Connection),
		Output == 'GNU Prolog rocks!'.

	test(explicit_connection_and_echo) :-
		connect(localhost, 6379, Connection),
		send(Connection, echo('GNU Prolog rocks!'), bulk(Output)),
		disconnect(Connection),
		Output == 'GNU Prolog rocks!'.

	test(ping_the_server) :-
		connect(Connection),
		send(Connection, ping, status(Output)),
		disconnect(Connection),
		Output == 'PONG'.

	%% SERVER...

	test(set_and_get_client_name) :-
		connect(Connection),
		send(Connection, client(setname, 'Objitsu'), status(Set)),
		send(Connection, client(getname), bulk(Get)),
		disconnect(Connection),
		Set == 'OK',
		Get == 'Objitsu'.

	test(set_and_get_timeout) :-
		connect(Connection),
		send(Connection, config(set, timeout, 86400), status(Set)),
		send(Connection, config(get, timeout), [bulk(Key), bulk(Val)]),
		disconnect(Connection),
		Set == 'OK',
		Key == 'timeout',
		Val == '86400'.

	test(flushall_flushdb_and_dbsize) :-
		connect(Connection),
		send(Connection, flushall, status(OK1)),
		send(Connection, set(test_key_1, 'Hello'), status(OK2)),
		send(Connection, get(test_key_1), bulk(Val)),
		send(Connection, dbsize, number(Size1)),
		send(Connection, flushdb, status(OK3)),
		send(Connection, dbsize, number(Size2)),
		disconnect(Connection),
		OK1 == 'OK',
		OK2 == 'OK',
		OK3 == 'OK',
		Val == 'Hello',
		Size1 == 1,
		Size2 == 0.

	%% KEYS...

	test(key_creation_exists_set_get_and_deletion) :-
		connect(Connection),
		send(Connection, flushall, status(OK1)),
		send(Connection, exists(test_key_1), number(Exists0)),
		send(Connection, set(test_key_1, 'Hello'), status(OK2)),
		send(Connection, exists(test_key_1), number(Exists1)),
		send(Connection, del(test_key_1), number(Del1)),
		send(Connection, exists(test_key_1), number(Exists2)),
		disconnect(Connection),
		OK1 == 'OK',
		OK2 == 'OK',
		Exists0 == 0,
		Exists1 == 1,
		Del1 == 1,
		Exists2 == 0.

	test(key_expiry_with_set_ttl_expire_and_exists) :-
		connect(Connection),
		send(Connection, flushall, status(OK1)),
		send(Connection, set(test_key_1, 'Hello'), status(OK2)),
		send(Connection, ttl(test_key_1), number(Minus1)),
		send(Connection, expire(test_key_1, 1), number(Set1)),
		sleep(2),
		send(Connection, exists(test_key_1), number(Exists0)),
		disconnect(Connection),
		OK1 == 'OK',
		OK2 == 'OK',
		Set1 == 1,
		Minus1 == -1,
		Exists0 == 0.

	%% HASHES...

	test(create_hash_with_data) :-
		connect(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, exists(test_hash), number(0)),
		send(Connection, hset(test_hash, name, 'Emacs The Viking'), number(1)),
		send(Connection, hset(test_hash, age, 48), number(1)),
		send(Connection, hset(test_hash, status, 'Thinking'), number(1)),
		send(Connection, exists(test_hash), number(1)),
		disconnect(Connection).

	test(previously_created_keys_exist) :-
		connect(Connection),
		send(Connection, hlen(test_hash), number(3)),
		send(Connection, hexists(test_hash, name), number(1)),
		send(Connection, hexists(test_hash, age), number(1)),
		send(Connection, hexists(test_hash, status), number(1)),
		disconnect(Connection).

	test(values_of_previously_created_keys) :-
		connect(Connection),
		send(Connection, hget(test_hash, name), bulk('Emacs The Viking')),
		send(Connection, hget(test_hash, age), bulk('48')),
		send(Connection, hget(test_hash, status), bulk('Thinking')),
		disconnect(Connection).

	test(integer_increment_of_hash_value) :-
		connect(Connection),
		send(Connection, hincrby(test_hash, age, -20), number(28)),
		send(Connection, hincrby(test_hash, age, 20), number(48)),
		disconnect(Connection).

	test(float_increment_of_hash_value) :-
		connect(Connection),
		send(Connection, hincrbyfloat(test_hash, age, -0.5), bulk('47.5')),
		send(Connection, hincrbyfloat(test_hash, age, 1.5), bulk('49')),
		disconnect(Connection).

	test(setting_multiple_keys_at_once) :-
		connect(Connection),
		send(Connection, hmset(test_hash,
			new_field_1, 'Hello',
			new_field_2, 'World',
			new_field_3, 42), status('OK')),
		disconnect(Connection).

	test(getting_multiple_keys_previously_set) :-
		connect(Connection),
		send(Connection, hmget(test_hash, new_field_1, new_field_2, new_field_3),
			[bulk('Hello'), bulk('World'), bulk('42')]),
		disconnect(Connection).

	test(getting_all_hash_keys_at_once) :-
		connect(Connection),
		send(Connection, hgetall(test_hash), X),
		length(X, 12),
		disconnect(Connection).

	test(deleting_some_existing_fields) :-
		connect(Connection),
		send(Connection, hdel(test_hash, name), number(1)),
		send(Connection, hdel(test_hash, age), number(1)),
		send(Connection, hdel(test_hash, status), number(1)),
		send(Connection, hdel(test_hash, unknown), number(0)),
		send(Connection, hlen(test_hash), number(3)),
		disconnect(Connection).

	%% LISTS...

	test(create_a_list_with_a_single_value) :-
		connect(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, lpush(test_list, 42), number(1)),
		send(Connection, llen(test_list), number(1)),
		disconnect(Connection).

	test(pop_only_entry_from_a_list) :-
		connect(Connection),
		send(Connection, lpop(test_list), bulk('42')),
		send(Connection, llen(test_list), number(0)),
		disconnect(Connection).

	test(create_a_list_with_multiple_values_lpush_1) :-
		connect(Connection),
		send(Connection, lpush(test_list_1, 'Hello', world, 42), number(3)),
		send(Connection, llen(test_list_1), number(3)),
		disconnect(Connection).

	test(lrange_on_existing_list_with_lpush_1) :-
		connect(Connection),
		send(Connection, lrange(test_list_1, 0, -1), [bulk('42'), bulk('world'), bulk('Hello')]),
		disconnect(Connection).

	test(create_a_list_with_multiple_values_lpush_2) :-
		connect(Connection),
		send(Connection, rpush(test_list_2, 'Hello', world, 42), number(3)),
		send(Connection, llen(test_list_2), number(3)),
		disconnect(Connection).

	test(lrange_on_existing_list_with_lpush_2) :-
		connect(Connection),
		send(Connection, lrange(test_list_2, 0, -1), [bulk('Hello'), bulk('world'), bulk('42')]),
		disconnect(Connection).

	test(get_length_of_existing_list) :-
		connect(Connection),
		send(Connection, llen(test_list_1), number(3)),
		disconnect(Connection).

	test(get_values_by_lindex_position) :-
		connect(Connection),
		send(Connection, lindex(test_list_1,1), bulk('world')),
		send(Connection, lindex(test_list_1,2), bulk('Hello')),
		send(Connection, lindex(test_list_1,0), bulk('42')),
		disconnect(Connection).

	test(add_to_list_with_linset_command) :-
		connect(Connection),
		send(Connection, linsert(test_list_1, before, 42, 'FRIST'), number(4)),
		send(Connection, linsert(test_list_1, after, world, 'custard creams rock'), number(5)),
		send(Connection, lindex(test_list_1, 3), bulk('custard creams rock')),
		send(Connection, lindex(test_list_1, -1), bulk('Hello')),
		send(Connection, lindex(test_list_1, -3), bulk('world')),
		send(Connection, lindex(test_list_1, 0), bulk('FRIST')),
		disconnect(Connection).

	test(popping_with_lpop_and_rpop) :-
		connect(Connection),
		send(Connection, lpop(test_list_1), bulk('FRIST')),
		send(Connection, rpop(test_list_1), bulk('Hello')),
		send(Connection, lpop(test_list_1), bulk('42')),
		send(Connection, rpop(test_list_1), bulk('custard creams rock')),
		send(Connection, lpop(test_list_1), bulk('world')),
		disconnect(Connection).

	%% STRINGS...

	test(basic_string_get_and_set) :-
		connect(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, set(test_string, 'Hello World'), status('OK')),
		send(Connection, get(test_string), bulk('Hello World')),
		disconnect(Connection).

	test(extended_set_and_get_with_expiry) :-
		connect(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, set(test_string, 'Miller time!', ex, 1), status('OK')),
		sleep(2),
		send(Connection, get(test_string), nil),
		disconnect(Connection).

	test(append_to_an_existing_string) :-
		connect(Connection),
		send(Connection, set(test_string, 'GNU Prolog'), status('OK')),
		send(Connection, append(test_string, ' is Cool'), number(18)),
		send(Connection, strlen(test_string), number(18)),
		disconnect(Connection).

	test(counting_bits_in_a_string) :-
		connect(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, set('bitbucket(!)', 'U'), status('OK')),
		send(Connection, bitcount('bitbucket(!)'), number(4)),
		disconnect(Connection).

:- end_object.
