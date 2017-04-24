%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		author is 'Sean Charles. Adapted to Logtalk by Paulo Moura',
		date is 2017/04/24,
		comment is 'Unit tests for the "redis" example.'
	]).

	:- uses(redis, [
		connect/1, connect/3,
		send/3,
		disconnect/1
	]).

	:- if(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(system, [sleep/1]).
	:- endif.

	%% CONNECTION...
	
	test(default_connection_and_echo) :-
		connect(C),
		send(C, echo('GNU Prolog rocks!'), bulk(Output)),
		disconnect(C),
		Output == 'GNU Prolog rocks!'.

	test(explicit_connection_and_echo) :-
		connect(C, localhost, 6379),
		send(C, echo('GNU Prolog rocks!'), bulk(Output)),
		disconnect(C),
		Output == 'GNU Prolog rocks!'.

	test(ping_the_server) :-
		connect(C),
		send(C, ping, status(Output)),
		disconnect(C),
		Output == 'PONG'.

	%% SERVER...

	test(set_and_get_client_name) :-
		connect(C),
		send(C, client(setname, 'Objitsu'), status(Set)),
		send(C, client(getname), bulk(Get)),
		disconnect(C),
		Set == 'OK',
		Get == 'Objitsu'.
	
	test(set_and_get_timeout) :-
		connect(C),
		send(C, config(set, timeout, 86400), status(Set)),
		send(C, config(get, timeout), [bulk(Key), bulk(Val)]),
		disconnect(C),
		Set == 'OK',
		Key == 'timeout',
		Val == '86400'.

	test(flushall_flushdb_and_dbsize) :-
		connect(C),
		send(C, flushall, status(OK1)),
		send(C, set(test_key_1, 'Hello'), status(OK2)),
		send(C, get(test_key_1), bulk(Val)),
		send(C, dbsize, number(Size1)),
		send(C, flushdb, status(OK3)),
		send(C, dbsize, number(Size2)),
		disconnect(C),
		OK1 == 'OK',
		OK2 == 'OK',
		OK3 == 'OK',
		Val == 'Hello',
		Size1 == 1,
		Size2 == 0.

	%% KEYS...

	test(key_creation_exists_set_get_and_deletion) :-
		connect(C),
		send(C, flushall, status(OK1)),
		send(C, exists(test_key_1), number(Exists0)),
		send(C, set(test_key_1, 'Hello'), status(OK2)),
		send(C, exists(test_key_1), number(Exists1)),
		send(C, del(test_key_1), number(Del1)),
		send(C, exists(test_key_1), number(Exists2)),
		disconnect(C),
		OK1 == 'OK',
		OK2 == 'OK',
		Exists0 == 0,
		Exists1 == 1,
		Del1 == 1,
		Exists2 == 0.
	
	test(key_expiry_with_set_ttl_expire_and_exists) :-
		connect(C),
		send(C, flushall, status(OK1)),
		send(C, set(test_key_1, 'Hello'), status(OK2)),
		send(C, ttl(test_key_1), number(Minus1)),
		send(C, expire(test_key_1, 1), number(Set1)),
		sleep(2),
		send(C, exists(test_key_1), number(Exists0)),
		disconnect(C),
		OK1 == 'OK',
		OK2 == 'OK',
		Set1 == 1,
		Minus1 == -1,
		Exists0 == 0.

	%% HASHES...

	test(create_hash_with_data) :-
		connect(C),
		send(C, flushall, status('OK')),
		send(C, exists(test_hash), number(0)),
		send(C, hset(test_hash, name, 'Emacs The Viking'), number(1)),
		send(C, hset(test_hash, age, 48), number(1)),
		send(C, hset(test_hash, status, 'Thinking'), number(1)),
		send(C, exists(test_hash), number(1)),
		disconnect(C).

	test(previously_created_keys_exist) :-
		connect(C),
		send(C, hlen(test_hash), number(3)),
		send(C, hexists(test_hash, name), number(1)),
		send(C, hexists(test_hash, age), number(1)),
		send(C, hexists(test_hash, status), number(1)),
		disconnect(C).

	test(values_of_previously_created_keys) :-
		connect(C),
		send(C, hget(test_hash, name), bulk('Emacs The Viking')),
		send(C, hget(test_hash, age), bulk('48')),
		send(C, hget(test_hash, status), bulk('Thinking')),
		disconnect(C).

	test(integer_increment_of_hash_value) :-
		connect(C),
		send(C, hincrby(test_hash, age, -20), number(28)),
		send(C, hincrby(test_hash, age, 20), number(48)),
		disconnect(C).

	test(float_increment_of_hash_value) :-
		connect(C),
		send(C, hincrbyfloat(test_hash, age, -0.5), bulk('47.5')),
		send(C, hincrbyfloat(test_hash, age, 1.5), bulk('49')),
		disconnect(C).

	test(setting_multiple_keys_at_once) :-
		connect(C),
		send(C, hmset(test_hash,
			new_field_1, 'Hello',
			new_field_2, 'World',
			new_field_3, 42), status('OK')),	
		disconnect(C).

	test(getting_multiple_keys_previously_set) :-
		connect(C),
		send(C, hmget(test_hash, new_field_1, new_field_2, new_field_3),
			[bulk('Hello'), bulk('World'), bulk('42')]),	
		disconnect(C).

	test(getting_all_hash_keys_at_once) :-
		connect(C),
		send(C, hgetall(test_hash), X),
		length(X, 12),
		disconnect(C).

	test(deleting_some_existing_fields) :-
		connect(C),
		send(C, hdel(test_hash, name), number(1)),
		send(C, hdel(test_hash, age), number(1)),
		send(C, hdel(test_hash, status), number(1)),
		send(C, hdel(test_hash, unknown), number(0)),
		send(C, hlen(test_hash), number(3)),
		disconnect(C).

	%% LISTS...

	test(create_a_list_with_a_single_value) :-
		connect(C),
		send(C, flushall, status('OK')),
		send(C, lpush(test_list, 42), number(1)),
		send(C, llen(test_list), number(1)),
		disconnect(C).

	test(pop_only_entry_from_a_list) :-
		connect(C),
		send(C, lpop(test_list), bulk('42')),
		send(C, llen(test_list), number(0)),
		disconnect(C).

	test(create_a_list_with_multiple_values_lpush_1) :-
		connect(C),
		send(C, lpush(test_list_1, 'Hello', world, 42), number(3)),
		send(C, llen(test_list_1), number(3)),
		disconnect(C).

	test(lrange_on_existing_list_with_lpush_1) :-
		connect(C),
		send(C, lrange(test_list_1, 0, -1), [bulk('42'), bulk('world'), bulk('Hello')]),
		disconnect(C).

	test(create_a_list_with_multiple_values_lpush_2) :-
		connect(C),
		send(C, rpush(test_list_2, 'Hello', world, 42), number(3)),
		send(C, llen(test_list_2), number(3)),
		disconnect(C).

	test(lrange_on_existing_list_with_lpush_2) :-
		connect(C),
		send(C, lrange(test_list_2, 0, -1), [bulk('Hello'), bulk('world'), bulk('42')]),
		disconnect(C).

	test(get_length_of_existing_list) :-
		connect(C),
		send(C, llen(test_list_1), number(3)),
		disconnect(C).

	test(get_values_by_lindex_position) :-
		connect(C),
		send(C, lindex(test_list_1,1), bulk('world')),
		send(C, lindex(test_list_1,2), bulk('Hello')),
		send(C, lindex(test_list_1,0), bulk('42')),
		disconnect(C).

	test(add_to_list_with_linset_command) :-
		connect(C),
		send(C, linsert(test_list_1, before, 42, 'FRIST'), number(4)),
		send(C, linsert(test_list_1, after, world, 'custard creams rock'), number(5)),
		send(C, lindex(test_list_1, 3), bulk('custard creams rock')),
		send(C, lindex(test_list_1, -1), bulk('Hello')),
		send(C, lindex(test_list_1, -3), bulk('world')),
		send(C, lindex(test_list_1, 0), bulk('FRIST')),
		disconnect(C).

	test(popping_with_lpop_and_rpop) :-
		connect(C),
		send(C, lpop(test_list_1), bulk('FRIST')),
		send(C, rpop(test_list_1), bulk('Hello')),
		send(C, lpop(test_list_1), bulk('42')),
		send(C, rpop(test_list_1), bulk('custard creams rock')),
		send(C, lpop(test_list_1), bulk('world')),
		disconnect(C).

	%% STRINGS...

	test(basic_string_get_and_set) :-
		connect(C),
		send(C, flushall, status('OK')),
		send(C, set(test_string, 'Hello World'), status('OK')),
		send(C, get(test_string), bulk('Hello World')),
		disconnect(C).

	test(extended_set_and_get_with_expiry) :-
		connect(C),
		send(C, flushall, status('OK')),
		send(C, set(test_string, 'Miller time!', ex, 1), status('OK')),
		sleep(2),
		send(C, get(test_string), nil),
		disconnect(C).

	test(append_to_an_existing_string) :-
		connect(C),
		send(C, set(test_string, 'GNU Prolog'), status('OK')),
		send(C, append(test_string, ' is Cool'), number(18)),
		send(C, strlen(test_string), number(18)),
		disconnect(C).

	test(counting_bits_in_a_string) :-
		connect(C),
		send(C, flushall, status('OK')),
		send(C, set('bitbucket(!)', 'U'), status('OK')),
		send(C, bitcount('bitbucket(!)'), number(4)),
		disconnect(C).

:- end_object.
