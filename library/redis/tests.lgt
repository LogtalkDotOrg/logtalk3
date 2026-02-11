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
		version is 0:6:1,
		author is 'Sean Charles. Adapted to Logtalk by Paulo Moura',
		date is 2026-02-11,
		comment is 'Unit tests for the "redis" library.'
	]).

	:- uses(redis, [
		connect/1, connect/3,
		send/3,
		disconnect/1,
		get/3, set/4, append/4, getrange/5, setrange/5, strlen/3,
		mget/3, mset/3, incr/3, decr/3, incrby/4, decrby/4,
		del/3, exists/3, keys/3, ttl/3, expire/4, persist/3, rename/4, type/3,
		hset/5, hget/4, hgetall/3, hdel/4, hexists/4, hkeys/3, hvals/3, hlen/3,
		lpush/4, rpush/4, lpop/3, rpop/3, lrange/5, llen/3, lrem/5, ltrim/5,
		sadd/4, srem/4, smembers/3, sismember/4, scard/3,
		zadd/5, zrem/4, zrange/5, zrank/4, zcard/3, zscore/4
	]).

	:- uses(list, [
		length/2
	]).

	:- uses(os, [
		environment_variable/2
	]).

	:- uses(term_io, [
		read_from_atom/2
	]).

	:- if(current_logtalk_flag(prolog_dialect, ciao)).
		:- use_module(system, [pause/1 as sleep/1]).
	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).
		:- use_module(system, [sleep/1]).
	:- elif(current_logtalk_flag(prolog_dialect, qp)).
		:- uses(user, [thread_sleep/1 as sleep/1]).
	:- else.
		% ECLiPSe, GNU Prolog, SWI-Prolog, Trealla Prolog, XSB, and XVM provide
		% sleep/1 as a built-in predicate but we list it here for clarity
		:- uses(user, [sleep/1]).
	:- endif.

	cover(redis).

	% only run the tests if there's a Redis server running
	condition :-
		server_host_port(Host, Port),
		catch((connect(Host, Port, Connection), disconnect(Connection)), _, fail).

	%% CONNECTION...

	test(default_connection_and_echo, true(Output == 'Logtalk rocks!')) :-
		server_connection(Connection),
		send(Connection, echo('Logtalk rocks!'), bulk(Output)),
		disconnect(Connection).

	test(explicit_connection_and_echo, true(Output == 'Logtalk rocks!')) :-
		server_host_port(Host, Port),
		connect(Host, Port, Connection),
		send(Connection, echo('Logtalk rocks!'), bulk(Output)),
		disconnect(Connection).

	test(ping_the_server, true(Output == 'PONG')) :-
		server_connection(Connection),
		send(Connection, ping, status(Output)),
		disconnect(Connection).

	%% SERVER...

	test(set_and_get_client_name, true(Set-Get == 'OK'-'Objitsu')) :-
		server_connection(Connection),
		send(Connection, client(setname, 'Objitsu'), status(Set)),
		send(Connection, client(getname), bulk(Get)),
		disconnect(Connection).

	test(set_and_get_timeout, true(v(Set,Key,Val) == v('OK','timeout','86400'))) :-
		server_connection(Connection),
		send(Connection, config(set, timeout, 86400), status(Set)),
		send(Connection, config(get, timeout), [bulk(Key), bulk(Val)]),
		disconnect(Connection).

	test(flushall_flushdb_and_dbsize, true(v(OK1,OK2,OK3,Val,Size1,Size2) == v('OK','OK','OK','Hello',1,0))) :-
		server_connection(Connection),
		send(Connection, flushall, status(OK1)),
		send(Connection, set(test_key_1, 'Hello'), status(OK2)),
		send(Connection, get(test_key_1), bulk(Val)),
		send(Connection, dbsize, number(Size1)),
		send(Connection, flushdb, status(OK3)),
		send(Connection, dbsize, number(Size2)),
		disconnect(Connection).

	%% KEYS...

	test(key_creation_exists_set_get_and_deletion, true(v(OK1,OK2,Exists0,Exists1,Del1,Exists2) == v('OK','OK',0,1,1,0))) :-
		server_connection(Connection),
		send(Connection, flushall, status(OK1)),
		send(Connection, exists(test_key_1), number(Exists0)),
		send(Connection, set(test_key_1, 'Hello'), status(OK2)),
		send(Connection, exists(test_key_1), number(Exists1)),
		send(Connection, del(test_key_1), number(Del1)),
		send(Connection, exists(test_key_1), number(Exists2)),
		disconnect(Connection).

	test(key_expiry_with_set_ttl_expire_and_exists, true(v(OK1,OK2,Set1,Minus1,Exists0) == v('OK','OK',1,-1,0))) :-
		server_connection(Connection),
		send(Connection, flushall, status(OK1)),
		send(Connection, set(test_key_1, 'Hello'), status(OK2)),
		send(Connection, ttl(test_key_1), number(Minus1)),
		send(Connection, expire(test_key_1, 1), number(Set1)),
		sleep(2),
		send(Connection, exists(test_key_1), number(Exists0)),
		disconnect(Connection).

	%% HASHES...

	test(create_hash_with_data, true) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, exists(test_hash), number(0)),
		send(Connection, hset(test_hash, name, 'Emacs The Viking'), number(1)),
		send(Connection, hset(test_hash, age, 48), number(1)),
		send(Connection, hset(test_hash, status, 'Thinking'), number(1)),
		send(Connection, exists(test_hash), number(1)),
		disconnect(Connection).

	test(previously_created_keys_exist, true) :-
		server_connection(Connection),
		send(Connection, hlen(test_hash), number(3)),
		send(Connection, hexists(test_hash, name), number(1)),
		send(Connection, hexists(test_hash, age), number(1)),
		send(Connection, hexists(test_hash, status), number(1)),
		disconnect(Connection).

	test(values_of_previously_created_keys, true) :-
		server_connection(Connection),
		send(Connection, hget(test_hash, name), bulk('Emacs The Viking')),
		send(Connection, hget(test_hash, age), bulk('48')),
		send(Connection, hget(test_hash, status), bulk('Thinking')),
		disconnect(Connection).

	test(integer_increment_of_hash_value, true) :-
		server_connection(Connection),
		send(Connection, hincrby(test_hash, age, -20), number(28)),
		send(Connection, hincrby(test_hash, age, 20), number(48)),
		disconnect(Connection).

	test(float_increment_of_hash_value, true) :-
		server_connection(Connection),
		send(Connection, hincrbyfloat(test_hash, age, -0.5), bulk('47.5')),
		send(Connection, hincrbyfloat(test_hash, age, 1.5), bulk('49')),
		disconnect(Connection).

	test(setting_multiple_keys_at_once, true) :-
		server_connection(Connection),
		send(Connection, hmset(test_hash,
			new_field_1, 'Hello',
			new_field_2, 'World',
			new_field_3, 42), status('OK')
		),
		disconnect(Connection).

	test(getting_multiple_keys_previously_set, true) :-
		server_connection(Connection),
		send(Connection, hmget(test_hash, new_field_1, new_field_2, new_field_3),
			[bulk('Hello'), bulk('World'), bulk('42')]),
		disconnect(Connection).

	test(getting_all_hash_keys_at_once, true(length(X, 12))) :-
		server_connection(Connection),
		send(Connection, hgetall(test_hash), X),
		disconnect(Connection).

	test(deleting_some_existing_fields, true) :-
		server_connection(Connection),
		send(Connection, hdel(test_hash, name), number(1)),
		send(Connection, hdel(test_hash, age), number(1)),
		send(Connection, hdel(test_hash, status), number(1)),
		send(Connection, hdel(test_hash, unknown), number(0)),
		send(Connection, hlen(test_hash), number(3)),
		disconnect(Connection).

	%% LISTS...

	test(create_a_list_with_a_single_value, true) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, lpush(test_list, 42), number(1)),
		send(Connection, llen(test_list), number(1)),
		disconnect(Connection).

	test(pop_only_entry_from_a_list, true) :-
		server_connection(Connection),
		send(Connection, lpop(test_list), bulk('42')),
		send(Connection, llen(test_list), number(0)),
		disconnect(Connection).

	test(create_a_list_with_multiple_values_lpush_1, true) :-
		server_connection(Connection),
		send(Connection, lpush(test_list_1, 'Hello', world, 42), number(3)),
		send(Connection, llen(test_list_1), number(3)),
		disconnect(Connection).

	test(lrange_on_existing_list_with_lpush_1, true) :-
		server_connection(Connection),
		send(Connection, lrange(test_list_1, 0, -1), [bulk('42'), bulk('world'), bulk('Hello')]),
		disconnect(Connection).

	test(create_a_list_with_multiple_values_lpush_2, true) :-
		server_connection(Connection),
		send(Connection, rpush(test_list_2, 'Hello', world, 42), number(3)),
		send(Connection, llen(test_list_2), number(3)),
		disconnect(Connection).

	test(lrange_on_existing_list_with_lpush_2, true) :-
		server_connection(Connection),
		send(Connection, lrange(test_list_2, 0, -1), [bulk('Hello'), bulk('world'), bulk('42')]),
		disconnect(Connection).

	test(get_length_of_existing_list, true) :-
		server_connection(Connection),
		send(Connection, llen(test_list_1), number(3)),
		disconnect(Connection).

	test(get_values_by_lindex_position, true) :-
		server_connection(Connection),
		send(Connection, lindex(test_list_1,1), bulk('world')),
		send(Connection, lindex(test_list_1,2), bulk('Hello')),
		send(Connection, lindex(test_list_1,0), bulk('42')),
		disconnect(Connection).

	test(add_to_list_with_linset_command, true) :-
		server_connection(Connection),
		send(Connection, linsert(test_list_1, before, 42, 'FRIST'), number(4)),
		send(Connection, linsert(test_list_1, after, world, 'custard creams rock'), number(5)),
		send(Connection, lindex(test_list_1, 3), bulk('custard creams rock')),
		send(Connection, lindex(test_list_1, -1), bulk('Hello')),
		send(Connection, lindex(test_list_1, -3), bulk('world')),
		send(Connection, lindex(test_list_1, 0), bulk('FRIST')),
		disconnect(Connection).

	test(popping_with_lpop_and_rpop, true) :-
		server_connection(Connection),
		send(Connection, lpop(test_list_1), bulk('FRIST')),
		send(Connection, rpop(test_list_1), bulk('Hello')),
		send(Connection, lpop(test_list_1), bulk('42')),
		send(Connection, rpop(test_list_1), bulk('custard creams rock')),
		send(Connection, lpop(test_list_1), bulk('world')),
		disconnect(Connection).

	%% STRINGS...

	test(basic_string_get_and_set, true) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, set(test_string, 'Hello World'), status('OK')),
		send(Connection, get(test_string), bulk('Hello World')),
		disconnect(Connection).

	test(extended_set_and_get_with_expiry, true) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, set(test_string, 'Miller time!', ex, 1), status('OK')),
		sleep(2),
		send(Connection, get(test_string), nil),
		disconnect(Connection).

	test(append_to_an_existing_string, true) :-
		server_connection(Connection),
		send(Connection, set(test_string, 'Logtalk'), status('OK')),
		send(Connection, append(test_string, ' is Cool'), number(15)),
		send(Connection, strlen(test_string), number(15)),
		disconnect(Connection).

	test(counting_bits_in_a_string, true) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		send(Connection, set('bitbucket(!)', 'U'), status('OK')),
		send(Connection, bitcount('bitbucket(!)'), number(4)),
		disconnect(Connection).

	%% STRING WRAPPER PREDICATES...

	test(wrapper_get_set, true(Key == 'Test Value')) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		set(Connection, wrapper_test_key, 'Test Value', 'OK'),
		get(Connection, wrapper_test_key, Key),
		disconnect(Connection).

	test(wrapper_getrange, true(Range1-Range2 == 'Hello'-'World')) :-
		server_connection(Connection),
		set(Connection, range_key, 'Hello World', 'OK'),
		getrange(Connection, range_key, 0, 4, Range1),
		getrange(Connection, range_key, 6, 10, Range2),
		disconnect(Connection).

	test(wrapper_setrange, true(Range == 'Hello Redis')) :-
		server_connection(Connection),
		set(Connection, setrange_key, 'Hello World', 'OK'),
		setrange(Connection, setrange_key, 6, 'Redis', 11),
		get(Connection, setrange_key, Range),
		disconnect(Connection).

	test(wrapper_mget_mset, true(Values == [bulk(value1),bulk(value2),bulk(value3)])) :-
		server_connection(Connection),
		mset(Connection, [key1, value1, key2, value2, key3, value3], 'OK'),
		mget(Connection, [key1,key2,key3], Values),
		disconnect(Connection).

	test(wrapper_incr_decr, true(v(Incr1,Incr2,Decr1,Decr2) == v(11,12,11,10))) :-
		server_connection(Connection),
		set(Connection, counter, '10', 'OK'),
		incr(Connection, counter, Incr1),
		incr(Connection, counter, Incr2),
		decr(Connection, counter, Decr1),
		decr(Connection, counter, Decr2),
		disconnect(Connection).

	test(wrapper_incrby_decrby, true(Inc-Dec == 150-120)) :-
		server_connection(Connection),
		set(Connection, counter2, '100', 'OK'),
		incrby(Connection, counter2, 50, Inc),
		decrby(Connection, counter2, 30, Dec),
		disconnect(Connection).

	%% KEY WRAPPER PREDICATES...

	test(wrapper_del_exists, true) :-
		server_connection(Connection),
		set(Connection, del_test_key, 'value', 'OK'),
		exists(Connection, del_test_key, 1),
		del(Connection, del_test_key, 1),
		exists(Connection, del_test_key, 0),
		disconnect(Connection).

	test(wrapper_keys_pattern, true(Length >= 2)) :-
		server_connection(Connection),
		set(Connection, test_key_a, 'a', 'OK'),
		set(Connection, test_key_b, 'b', 'OK'),
		set(Connection, other_key, 'c', 'OK'),
		keys(Connection, 'test_key_*', Keys),
		length(Keys, Length),
		disconnect(Connection).

	test(wrapper_expire_ttl_persist, true((0 < TTL, TTL =< 100))) :-
		server_connection(Connection),
		set(Connection, ttl_test_key, 'value', 'OK'),
		expire(Connection, ttl_test_key, 100, 1),
		ttl(Connection, ttl_test_key, TTL),
		persist(Connection, ttl_test_key, 1),
		ttl(Connection, ttl_test_key, -1),
		disconnect(Connection).

	test(wrapper_rename, true(Exists1-Exists2 == 0-1)) :-
		server_connection(Connection),
		set(Connection, old_name, 'value', 'OK'),
		rename(Connection, old_name, new_name, 'OK'),
		exists(Connection, old_name, Exists1),
		exists(Connection, new_name, Exists2),
		del(Connection, new_name, _),
		disconnect(Connection).

	test(wrapper_type, true(Type == zset)) :-
		server_connection(Connection),
		set(Connection, string_key, 'value', 'OK'),
		type(Connection, string_key, string),
		lpush(Connection, list_key, item1, _),
		type(Connection, list_key, list),
		sadd(Connection, set_key, member1, _),
		type(Connection, set_key, set),
		hset(Connection, hash_key, field, value, _),
		type(Connection, hash_key, hash),
		zadd(Connection, zset_key, 1.0, member, _),
		type(Connection, zset_key, Type),
		disconnect(Connection).

	%% HASH WRAPPER PREDICATES...

	test(wrapper_hset_hget_hdel, true(HGet == value1)) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		hset(Connection, wrapper_hash, field1, value1, 1),
		hget(Connection, wrapper_hash, field1, HGet),
		hdel(Connection, wrapper_hash, field1, 1),
		disconnect(Connection).

	test(wrapper_hexists, true(HExists1-HExists2 == 1-0)) :-
		server_connection(Connection),
		hset(Connection, exists_hash, field, value, 1),
		hexists(Connection, exists_hash, field, HExists1),
		hexists(Connection, exists_hash, nonexistent, HExists2),
		disconnect(Connection).

	test(wrapper_hkeys_hvals_hlen, true(LKeys-LVals == 3-3)) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		hset(Connection, multi_hash, f1, v1, 1),
		hset(Connection, multi_hash, f2, v2, 1),
		hset(Connection, multi_hash, f3, v3, 1),
		hlen(Connection, multi_hash, 3),
		hkeys(Connection, multi_hash, Keys),
		length(Keys, LKeys),
		hvals(Connection, multi_hash, Vals),
		length(Vals, LVals),
		disconnect(Connection).

	test(wrapper_hgetall, true(length(All, 4))) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		hset(Connection, getall_hash, name, 'John', 1),
		hset(Connection, getall_hash, age, '30', 1),
		hgetall(Connection, getall_hash, All),
		disconnect(Connection).

	%% LIST WRAPPER PREDICATES...

	test(wrapper_lpush_rpush_lpop_rpop, true(v(LPop1,RPop,LPop2) == v(item0,item2,item1))) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		lpush(Connection, wrapper_list, item1, 1),
		rpush(Connection, wrapper_list, item2, 2),
		lpush(Connection, wrapper_list, item0, 3),
		lpop(Connection, wrapper_list, LPop1),
		rpop(Connection, wrapper_list, RPop),
		lpop(Connection, wrapper_list, LPop2),
		disconnect(Connection).

	test(wrapper_llen_lrange, true(LLen-LRange == 4-[bulk(a),bulk(b)])) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		rpush(Connection, range_list, a, 1),
		rpush(Connection, range_list, b, 2),
		rpush(Connection, range_list, c, 3),
		rpush(Connection, range_list, d, 4),
		llen(Connection, range_list, LLen),
		lrange(Connection, range_list, 0, 1, LRange),
		disconnect(Connection).

	test(wrapper_lrem, true(LRem-LLen == 2-2)) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		rpush(Connection, rem_list, a, _),
		rpush(Connection, rem_list, b, _),
		rpush(Connection, rem_list, a, _),
		rpush(Connection, rem_list, a, _),
		lrem(Connection, rem_list, 2, a, LRem),
		llen(Connection, rem_list, LLen),
		disconnect(Connection).

	test(wrapper_ltrim, true(v(LTrim,LLen,LRange) == v('OK',2,[bulk(b),bulk(c)]))) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		rpush(Connection, trim_list, a, _),
		rpush(Connection, trim_list, b, _),
		rpush(Connection, trim_list, c, _),
		rpush(Connection, trim_list, d, _),
		ltrim(Connection, trim_list, 1, 2, LTrim),
		llen(Connection, trim_list, LLen),
		lrange(Connection, trim_list, 0, -1, LRange),
		disconnect(Connection).

	%% SET WRAPPER PREDICATES...

	test(wrapper_sadd_srem_smembers, true(Length-Length2 == 3-2)) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		sadd(Connection, wrapper_set, member1, 1),
		sadd(Connection, wrapper_set, member2, 1),
		sadd(Connection, wrapper_set, member3, 1),
		sadd(Connection, wrapper_set, member1, 0),
		smembers(Connection, wrapper_set, Members),
		length(Members, Length),
		srem(Connection, wrapper_set, member2, 1),
		smembers(Connection, wrapper_set, Members2),
		length(Members2, Length2),
		disconnect(Connection).

	test(wrapper_sismember_scard, true(v(SisMember1,SisMember2,SCard) == v(1,0,2))) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		sadd(Connection, test_set, a, 1),
		sadd(Connection, test_set, b, 1),
		sismember(Connection, test_set, a, SisMember1),
		sismember(Connection, test_set, c, SisMember2),
		scard(Connection, test_set, SCard),
		disconnect(Connection).

	%% SORTED SET WRAPPER PREDICATES...

	test(wrapper_zadd_zrem_zcard, true(v(ZCard1,ZRem,ZCard2) == v(3,1,2))) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		zadd(Connection, wrapper_zset, 1.0, member1, 1),
		zadd(Connection, wrapper_zset, 2.0, member2, 1),
		zadd(Connection, wrapper_zset, 3.0, member3, 1),
		zcard(Connection, wrapper_zset, ZCard1),
		zrem(Connection, wrapper_zset, member2, ZRem),
		zcard(Connection, wrapper_zset, ZCard2),
		disconnect(Connection).

	test(wrapper_zrange, true(ZRange1-ZRange2 == [bulk(a),bulk(b)]-[bulk(a),bulk(b),bulk(c)])) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		zadd(Connection, range_zset, 1.0, a, _),
		zadd(Connection, range_zset, 2.0, b, _),
		zadd(Connection, range_zset, 3.0, c, _),
		zrange(Connection, range_zset, 0, 1, ZRange1),
		zrange(Connection, range_zset, 0, -1, ZRange2),
		disconnect(Connection).

	test(wrapper_zrank_zscore, true(v(ZRank1,ZRank2,ZRank3) == v(0,1,2))) :-
		server_connection(Connection),
		send(Connection, flushall, status('OK')),
		zadd(Connection, score_zset, 10.5, member1, _),
		zadd(Connection, score_zset, 20.3, member2, _),
		zadd(Connection, score_zset, 15.7, member3, _),
		zrank(Connection, score_zset, member1, ZRank1),
		zrank(Connection, score_zset, member3, ZRank2),
		zrank(Connection, score_zset, member2, ZRank3),
		zscore(Connection, score_zset, member1, ZScore1),
		^^assertion(sub_atom(ZScore1, 0, _, _, '10.5')),
		zscore(Connection, score_zset, member2, ZScore2),
		^^assertion(sub_atom(ZScore2, 0, _, _, '20.3')),
		disconnect(Connection).

	% auxiliary predicates

	server_connection(Connection) :-
		environment_variable('REDIS_HOST', Host),
		environment_variable('REDIS_PORT', PortAtom),
		read_from_atom(PortAtom, Port),
		integer(Port),
		!,
		connect(Host, Port, Connection).
	server_connection(Connection) :-
		connect(Connection).

	server_host_port(Host, Port) :-
		environment_variable('REDIS_HOST', Host),
		environment_variable('REDIS_PORT', PortAtom),
		read_from_atom(PortAtom, Port),
		integer(Port),
		!.
	server_host_port(localhost, 6379).

:- end_object.
