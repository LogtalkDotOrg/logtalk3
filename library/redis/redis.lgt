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


:- object(redis).

	:- info([
		version is 0:7:0,
		author is 'Paulo Moura',
		date is 2026-02-10,
		comment is 'Redis client library with support for strings, keys, hashes, lists, sets, and sorted sets. Inspired by Sean Charles GNU Prolog Redis client.',
		remarks is [
			'Command representation' - 'Use the Redis command name as the functor of a compound term where the arguments are the command arguments.',
			'Valid arguments' - 'Atoms, integers, and floats. Always use atoms instead of double-quoted "strings". This helps portability by not depending on the value of the ``double_quotes`` flag.',
			'Wrapper predicates' - 'The library provides convenient wrapper predicates for common Redis operations. For operations without a wrapper, use the generic ``send/3`` predicate.'
		]
	]).

	:- public(connect/1).
	:- mode(connect(--ground), one).
	:- info(connect/1, [
		comment is 'Connect to a Redis server running on localhost using the default 6379 port.',
		argnames is ['Connection']
	]).

	:- public(connect/3).
	:- mode(connect(+atom, +integer, --ground), one).
	:- info(connect/3, [
		comment is 'Connect to a Redis server running on the given host and port.',
		argnames is ['Host', 'Port', 'Connection']
	]).

	:- public(disconnect/1).
	:- mode(disconnect(++ground), one).
	:- info(disconnect/1, [
		comment is 'Disconnect from a Redis server.',
		argnames is ['Connection']
	]).

	:- public(send/3).
	:- mode(send(++ground, ++callable, --callable), one).
	:- info(send/3, [
		comment is 'Sends a request to the a Redis server and returns its reply.',
		argnames is ['Connection', 'Request', 'Reply']
	]).

	:- public(console/1).
	:- mode(console(++callable), one).
	:- info(console/1, [
		comment is 'Sends a request to a Redis server running on localhost at the default 6379 port and prints the reply.',
		argnames is ['Request']
	]).

	% String operations

	:- public(get/3).
	:- mode(get(+ground, +atom, -atom), one).
	:- info(get/3, [
		comment is 'Gets the value of a key.',
		argnames is ['Connection', 'Key', 'Value']
	]).

	:- public(set/4).
	:- mode(set(+ground, +atom, +ground, -atom), one).
	:- info(set/4, [
		comment is 'Sets the value of a key.',
		argnames is ['Connection', 'Key', 'Value', 'Status']
	]).

	:- public(append/4).
	:- mode(append(+ground, +atom, +ground, -integer), one).
	:- info(append/4, [
		comment is 'Appends a value to a key. Returns the length of the string after the append.',
		argnames is ['Connection', 'Key', 'Value', 'Length']
	]).

	:- public(getrange/5).
	:- mode(getrange(+ground, +atom, +integer, +integer, -atom), one).
	:- info(getrange/5, [
		comment is 'Gets a substring of the string stored at a key.',
		argnames is ['Connection', 'Key', 'Start', 'End', 'Substring']
	]).

	:- public(setrange/5).
	:- mode(setrange(+ground, +atom, +integer, +ground, -integer), one).
	:- info(setrange/5, [
		comment is 'Overwrites part of a string at a key starting at the specified offset. Returns the length of the string after modification.',
		argnames is ['Connection', 'Key', 'Offset', 'Value', 'Length']
	]).

	:- public(strlen/3).
	:- mode(strlen(+ground, +atom, -integer), one).
	:- info(strlen/3, [
		comment is 'Gets the length of the value stored at a key.',
		argnames is ['Connection', 'Key', 'Length']
	]).

	:- public(mget/3).
	:- mode(mget(+ground, +list(atom), -list), one).
	:- info(mget/3, [
		comment is 'Gets the values of multiple keys. Returns a list of values.',
		argnames is ['Connection', 'Keys', 'Values']
	]).

	:- public(mset/3).
	:- mode(mset(+ground, +list, -atom), one).
	:- info(mset/3, [
		comment is 'Sets multiple key-value pairs. Pairs should be provided as a flat list [Key1, Value1, Key2, Value2, ...].',
		argnames is ['Connection', 'Pairs', 'Status']
	]).

	:- public(incr/3).
	:- mode(incr(+ground, +atom, -integer), one).
	:- info(incr/3, [
		comment is 'Increments the integer value of a key by one. Returns the value after increment.',
		argnames is ['Connection', 'Key', 'Value']
	]).

	:- public(decr/3).
	:- mode(decr(+ground, +atom, -integer), one).
	:- info(decr/3, [
		comment is 'Decrements the integer value of a key by one. Returns the value after decrement.',
		argnames is ['Connection', 'Key', 'Value']
	]).

	:- public(incrby/4).
	:- mode(incrby(+ground, +atom, +integer, -integer), one).
	:- info(incrby/4, [
		comment is 'Increments the integer value of a key by the specified amount. Returns the value after increment.',
		argnames is ['Connection', 'Key', 'Increment', 'Value']
	]).

	:- public(decrby/4).
	:- mode(decrby(+ground, +atom, +integer, -integer), one).
	:- info(decrby/4, [
		comment is 'Decrements the integer value of a key by the specified amount. Returns the value after decrement.',
		argnames is ['Connection', 'Key', 'Decrement', 'Value']
	]).

	% Key operations

	:- public(del/3).
	:- mode(del(+ground, +atom, -integer), one).
	:- info(del/3, [
		comment is 'Deletes a key. Returns the number of keys removed.',
		argnames is ['Connection', 'Key', 'Count']
	]).

	:- public(exists/3).
	:- mode(exists(+ground, +atom, -integer), one).
	:- info(exists/3, [
		comment is 'Checks if a key exists. Returns 1 if the key exists, 0 otherwise.',
		argnames is ['Connection', 'Key', 'Exists']
	]).

	:- public(keys/3).
	:- mode(keys(+ground, +atom, -list), one).
	:- info(keys/3, [
		comment is 'Finds all keys matching a pattern. Returns a list of keys.',
		argnames is ['Connection', 'Pattern', 'Keys']
	]).

	:- public(ttl/3).
	:- mode(ttl(+ground, +atom, -integer), one).
	:- info(ttl/3, [
		comment is 'Gets the time to live for a key in seconds. Returns -1 if the key has no expiry, -2 if the key does not exist.',
		argnames is ['Connection', 'Key', 'Seconds']
	]).

	:- public(expire/4).
	:- mode(expire(+ground, +atom, +integer, -integer), one).
	:- info(expire/4, [
		comment is 'Sets a timeout on a key in seconds. Returns 1 if the timeout was set, 0 if the key does not exist.',
		argnames is ['Connection', 'Key', 'Seconds', 'Result']
	]).

	:- public(persist/3).
	:- mode(persist(+ground, +atom, -integer), one).
	:- info(persist/3, [
		comment is 'Removes the expiration from a key. Returns 1 if the timeout was removed, 0 if the key does not exist or has no timeout.',
		argnames is ['Connection', 'Key', 'Result']
	]).

	:- public(rename/4).
	:- mode(rename(+ground, +atom, +atom, -atom), one).
	:- info(rename/4, [
		comment is 'Renames a key. Returns status OK or error if the key does not exist.',
		argnames is ['Connection', 'OldKey', 'NewKey', 'Status']
	]).

	:- public(type/3).
	:- mode(type(+ground, +atom, -atom), one).
	:- info(type/3, [
		comment is 'Gets the type of the value stored at a key. Returns one of: string, list, set, zset, hash, stream, none.',
		argnames is ['Connection', 'Key', 'Type']
	]).

	% Hash operations

	:- public(hset/5).
	:- mode(hset(+ground, +atom, +atom, +ground, -integer), one).
	:- info(hset/5, [
		comment is 'Sets a field in a hash. Returns 1 if a new field was created, 0 if the field was updated.',
		argnames is ['Connection', 'Key', 'Field', 'Value', 'Result']
	]).

	:- public(hget/4).
	:- mode(hget(+ground, +atom, +atom, -ground), one).
	:- info(hget/4, [
		comment is 'Gets the value of a field in a hash.',
		argnames is ['Connection', 'Key', 'Field', 'Value']
	]).

	:- public(hgetall/3).
	:- mode(hgetall(+ground, +atom, -list), one).
	:- info(hgetall/3, [
		comment is 'Gets all fields and values in a hash. Returns a flat list of alternating fields and values.',
		argnames is ['Connection', 'Key', 'FieldsValues']
	]).

	:- public(hdel/4).
	:- mode(hdel(+ground, +atom, +atom, -integer), one).
	:- info(hdel/4, [
		comment is 'Deletes a field from a hash. Returns the number of fields removed.',
		argnames is ['Connection', 'Key', 'Field', 'Count']
	]).

	:- public(hexists/4).
	:- mode(hexists(+ground, +atom, +atom, -integer), one).
	:- info(hexists/4, [
		comment is 'Checks if a field exists in a hash. Returns 1 if the field exists, 0 otherwise.',
		argnames is ['Connection', 'Key', 'Field', 'Exists']
	]).

	:- public(hkeys/3).
	:- mode(hkeys(+ground, +atom, -list), one).
	:- info(hkeys/3, [
		comment is 'Gets all field names in a hash.',
		argnames is ['Connection', 'Key', 'Fields']
	]).

	:- public(hvals/3).
	:- mode(hvals(+ground, +atom, -list), one).
	:- info(hvals/3, [
		comment is 'Gets all values in a hash.',
		argnames is ['Connection', 'Key', 'Values']
	]).

	:- public(hlen/3).
	:- mode(hlen(+ground, +atom, -integer), one).
	:- info(hlen/3, [
		comment is 'Gets the number of fields in a hash.',
		argnames is ['Connection', 'Key', 'Count']
	]).

	% List operations

	:- public(lpush/4).
	:- mode(lpush(+ground, +atom, +ground, -integer), one).
	:- info(lpush/4, [
		comment is 'Prepends a value to a list. Returns the length of the list after the push.',
		argnames is ['Connection', 'Key', 'Value', 'Length']
	]).

	:- public(rpush/4).
	:- mode(rpush(+ground, +atom, +ground, -integer), one).
	:- info(rpush/4, [
		comment is 'Appends a value to a list. Returns the length of the list after the push.',
		argnames is ['Connection', 'Key', 'Value', 'Length']
	]).

	:- public(lpop/3).
	:- mode(lpop(+ground, +atom, -ground), one).
	:- info(lpop/3, [
		comment is 'Removes and returns the first element of a list.',
		argnames is ['Connection', 'Key', 'Value']
	]).

	:- public(rpop/3).
	:- mode(rpop(+ground, +atom, -ground), one).
	:- info(rpop/3, [
		comment is 'Removes and returns the last element of a list.',
		argnames is ['Connection', 'Key', 'Value']
	]).

	:- public(lrange/5).
	:- mode(lrange(+ground, +atom, +integer, +integer, -list), one).
	:- info(lrange/5, [
		comment is 'Gets a range of elements from a list. Indices are zero-based.',
		argnames is ['Connection', 'Key', 'Start', 'Stop', 'Elements']
	]).

	:- public(llen/3).
	:- mode(llen(+ground, +atom, -integer), one).
	:- info(llen/3, [
		comment is 'Gets the length of a list.',
		argnames is ['Connection', 'Key', 'Length']
	]).

	:- public(lrem/5).
	:- mode(lrem(+ground, +atom, +integer, +ground, -integer), one).
	:- info(lrem/5, [
		comment is 'Removes elements from a list. Count > 0: remove from head, Count < 0: remove from tail, Count = 0: remove all. Returns the number of removed elements.',
		argnames is ['Connection', 'Key', 'Count', 'Value', 'Removed']
	]).

	:- public(ltrim/5).
	:- mode(ltrim(+ground, +atom, +integer, +integer, -atom), one).
	:- info(ltrim/5, [
		comment is 'Trims a list to the specified range.',
		argnames is ['Connection', 'Key', 'Start', 'Stop', 'Status']
	]).

	% Set operations

	:- public(sadd/4).
	:- mode(sadd(+ground, +atom, +ground, -integer), one).
	:- info(sadd/4, [
		comment is 'Adds a member to a set. Returns the number of elements added.',
		argnames is ['Connection', 'Key', 'Member', 'Count']
	]).

	:- public(srem/4).
	:- mode(srem(+ground, +atom, +ground, -integer), one).
	:- info(srem/4, [
		comment is 'Removes a member from a set. Returns the number of elements removed.',
		argnames is ['Connection', 'Key', 'Member', 'Count']
	]).

	:- public(smembers/3).
	:- mode(smembers(+ground, +atom, -list), one).
	:- info(smembers/3, [
		comment is 'Gets all members in a set.',
		argnames is ['Connection', 'Key', 'Members']
	]).

	:- public(sismember/4).
	:- mode(sismember(+ground, +atom, +ground, -integer), one).
	:- info(sismember/4, [
		comment is 'Checks if a value is a member of a set. Returns 1 if the member exists, 0 otherwise.',
		argnames is ['Connection', 'Key', 'Member', 'IsMember']
	]).

	:- public(scard/3).
	:- mode(scard(+ground, +atom, -integer), one).
	:- info(scard/3, [
		comment is 'Gets the number of members in a set.',
		argnames is ['Connection', 'Key', 'Count']
	]).

	% Sorted set operations

	:- public(zadd/5).
	:- mode(zadd(+ground, +atom, +number, +ground, -integer), one).
	:- info(zadd/5, [
		comment is 'Adds a member with a score to a sorted set. Returns the number of elements added.',
		argnames is ['Connection', 'Key', 'Score', 'Member', 'Count']
	]).

	:- public(zrem/4).
	:- mode(zrem(+ground, +atom, +ground, -integer), one).
	:- info(zrem/4, [
		comment is 'Removes a member from a sorted set. Returns the number of elements removed.',
		argnames is ['Connection', 'Key', 'Member', 'Count']
	]).

	:- public(zrange/5).
	:- mode(zrange(+ground, +atom, +integer, +integer, -list), one).
	:- info(zrange/5, [
		comment is 'Gets a range of members from a sorted set, ordered from lowest to highest score.',
		argnames is ['Connection', 'Key', 'Start', 'Stop', 'Members']
	]).

	:- public(zrank/4).
	:- mode(zrank(+ground, +atom, +ground, -integer), one).
	:- info(zrank/4, [
		comment is 'Gets the rank (index) of a member in a sorted set, ordered from lowest to highest score. Returns nil if the member does not exist.',
		argnames is ['Connection', 'Key', 'Member', 'Rank']
	]).

	:- public(zcard/3).
	:- mode(zcard(+ground, +atom, -integer), one).
	:- info(zcard/3, [
		comment is 'Gets the number of members in a sorted set.',
		argnames is ['Connection', 'Key', 'Count']
	]).

	:- public(zscore/4).
	:- mode(zscore(+ground, +atom, +ground, -ground), one).
	:- info(zscore/4, [
		comment is 'Gets the score of a member in a sorted set.',
		argnames is ['Connection', 'Key', 'Member', 'Score']
	]).

	:- uses(logtalk, [print_message/3]).
	:- uses(list, [length/2]).
	:- uses(socket, [client_open/4, close/2]).

	% public predicates

	connect(redis(Input, Output, _Socket)) :-
		context(Context),
		catch(
			client_open(localhost, 6379, Input, Output),
			Error,
			throw(error(Error, Context))
		).

	connect(Host, Port, redis(Input, Output, _Socket)) :-
		context(Context),
		catch(
			client_open(Host, Port, Input, Output),
			Error,
			throw(error(Error, Context))
		).

	disconnect(redis(Input, Output, _Socket)) :-
		context(Context),
		catch(
			close(Input, Output),
			Error,
			throw(error(Error, Context))
		).

	send(Connection, Request, Reply) :-
		context(Context),
		catch(
			send_request(Connection, Request, Reply),
			Error,
			throw(error(Error, Context))
		).

	console(Request) :-
		context(Context),
		catch(
			console_request(Request),
			Error,
			throw(error(Error, Context))
		).

	% String operations wrappers

	get(Connection, Key, Value) :-
		send(Connection, get(Key), bulk(Value)).

	set(Connection, Key, Value, Status) :-
		send(Connection, set(Key, Value), status(Status)).

	append(Connection, Key, Value, Length) :-
		send(Connection, append(Key, Value), number(Length)).

	getrange(Connection, Key, Start, End, Substring) :-
		send(Connection, getrange(Key, Start, End), bulk(Substring)).

	setrange(Connection, Key, Offset, Value, Length) :-
		send(Connection, setrange(Key, Offset, Value), number(Length)).

	strlen(Connection, Key, Length) :-
		send(Connection, strlen(Key), number(Length)).

	mget(Connection, Keys, Values) :-
		Request =.. [mget| Keys],
		send(Connection, Request, Values).

	mset(Connection, Pairs, Status) :-
		Request =.. [mset| Pairs],
		send(Connection, Request, status(Status)).

	incr(Connection, Key, Value) :-
		send(Connection, incr(Key), number(Value)).

	decr(Connection, Key, Value) :-
		send(Connection, decr(Key), number(Value)).

	incrby(Connection, Key, Increment, Value) :-
		send(Connection, incrby(Key, Increment), number(Value)).

	decrby(Connection, Key, Decrement, Value) :-
		send(Connection, decrby(Key, Decrement), number(Value)).

	% Key operations wrappers

	del(Connection, Key, Count) :-
		send(Connection, del(Key), number(Count)).

	exists(Connection, Key, Exists) :-
		send(Connection, exists(Key), number(Exists)).

	keys(Connection, Pattern, Keys) :-
		send(Connection, keys(Pattern), Keys).

	ttl(Connection, Key, Seconds) :-
		send(Connection, ttl(Key), number(Seconds)).

	expire(Connection, Key, Seconds, Result) :-
		send(Connection, expire(Key, Seconds), number(Result)).

	persist(Connection, Key, Result) :-
		send(Connection, persist(Key), number(Result)).

	rename(Connection, OldKey, NewKey, Status) :-
		send(Connection, rename(OldKey, NewKey), status(Status)).

	type(Connection, Key, Type) :-
		send(Connection, type(Key), status(Type)).

	% Hash operations wrappers

	hset(Connection, Key, Field, Value, Result) :-
		send(Connection, hset(Key, Field, Value), number(Result)).

	hget(Connection, Key, Field, Value) :-
		send(Connection, hget(Key, Field), bulk(Value)).

	hgetall(Connection, Key, FieldsValues) :-
		send(Connection, hgetall(Key), FieldsValues).

	hdel(Connection, Key, Field, Count) :-
		send(Connection, hdel(Key, Field), number(Count)).

	hexists(Connection, Key, Field, Exists) :-
		send(Connection, hexists(Key, Field), number(Exists)).

	hkeys(Connection, Key, Fields) :-
		send(Connection, hkeys(Key), Fields).

	hvals(Connection, Key, Values) :-
		send(Connection, hvals(Key), Values).

	hlen(Connection, Key, Count) :-
		send(Connection, hlen(Key), number(Count)).

	% List operations wrappers

	lpush(Connection, Key, Value, Length) :-
		send(Connection, lpush(Key, Value), number(Length)).

	rpush(Connection, Key, Value, Length) :-
		send(Connection, rpush(Key, Value), number(Length)).

	lpop(Connection, Key, Value) :-
		send(Connection, lpop(Key), bulk(Value)).

	rpop(Connection, Key, Value) :-
		send(Connection, rpop(Key), bulk(Value)).

	lrange(Connection, Key, Start, Stop, Elements) :-
		send(Connection, lrange(Key, Start, Stop), Elements).

	llen(Connection, Key, Length) :-
		send(Connection, llen(Key), number(Length)).

	lrem(Connection, Key, Count, Value, Removed) :-
		send(Connection, lrem(Key, Count, Value), number(Removed)).

	ltrim(Connection, Key, Start, Stop, Status) :-
		send(Connection, ltrim(Key, Start, Stop), status(Status)).

	% Set operations wrappers

	sadd(Connection, Key, Member, Count) :-
		send(Connection, sadd(Key, Member), number(Count)).

	srem(Connection, Key, Member, Count) :-
		send(Connection, srem(Key, Member), number(Count)).

	smembers(Connection, Key, Members) :-
		send(Connection, smembers(Key), Members).

	sismember(Connection, Key, Member, IsMember) :-
		send(Connection, sismember(Key, Member), number(IsMember)).

	scard(Connection, Key, Count) :-
		send(Connection, scard(Key), number(Count)).

	% Sorted set operations wrappers

	zadd(Connection, Key, Score, Member, Count) :-
		send(Connection, zadd(Key, Score, Member), number(Count)).

	zrem(Connection, Key, Member, Count) :-
		send(Connection, zrem(Key, Member), number(Count)).

	zrange(Connection, Key, Start, Stop, Members) :-
		send(Connection, zrange(Key, Start, Stop), Members).

	zrank(Connection, Key, Member, Rank) :-
		send(Connection, zrank(Key, Member), Reply),
		(	Reply = number(Rank) ->
			true
		;	Reply = nil,
			Rank = nil
		).

	zcard(Connection, Key, Count) :-
		send(Connection, zcard(Key), number(Count)).

	zscore(Connection, Key, Member, Score) :-
		send(Connection, zscore(Key, Member), bulk(Score)).

	% other auxiliary predicates

	console_request(Request) :-
		connect(Connection),
		send_request(Connection, Request, Reply),
		disconnect(Connection),
		print_reply(Reply).

	send_request(redis(Input, Output, _), Request, Reply) :-
		parse_request(Request, Bytes),
		send_request(Bytes, Output),
		parse_reply(Input, Reply).

	parse_request(Request, Bytes) :-
		Request =.. Arguments,
		phrase(parse_request_command(Arguments), Bytes).

	parse_request_command(Arguments) -->
		{	length(Arguments, Length),
			number_codes(Length, Codes)
		},
		codes([42| Codes]), [13, 10],
		parse_request_arguments(Arguments).

	parse_request_arguments([]) -->
		[].
	parse_request_arguments([Argument| Arguments]) -->
		parse_request_argument(Argument),
		parse_request_arguments(Arguments).

	parse_request_argument(Argument) -->
		{	parse_argument(Argument, Bytes),
			length(Bytes, Length),
			number_codes(Length, Codes)
		},
		codes([36| Codes]), codes([13, 10| Bytes]), [13, 10].

	parse_argument(Head, Codes) :-
		atom(Head),
		!,
		atom_codes(Head, Codes).
	parse_argument(Head, Codes) :-
		number(Head),
		!,
		number_codes(Head, Codes).
	parse_argument(Codes, Codes).

	send_request([], Output) :-
		flush_output(Output).
	send_request([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		send_request(Bytes, Output).

	parse_reply(Input, Reply) :-
		get_byte(Input, Byte),
		char_code(Key, Byte),
		parse_reply(Key, Input, Reply).

	parse_reply((-), Input, _) :-
		phrase(parse_line(Input), Codes),
		atom_codes(Error, Codes),
		throw(redis_error(Error)).
	parse_reply((+), Input, status(Status)) :-
		parse_status(Input, Status).
	parse_reply((:), Input, number(Number)) :-
		parse_number(Input, Number).
	parse_reply(($), Input, Bulk) :-
		parse_bulk(Input, Bulk).
	parse_reply((*), Input, MBulk) :-
		parse_mbulk(Input, MBulk).

	parse_status(Input, Status) :-
		phrase(parse_line(Input), Codes), !,
		atom_codes(Status, Codes).

	parse_number(Input, Number) :-
		phrase(parse_line(Input), Codes), !,
		number_codes(Number, Codes).

	parse_bulk(Input, Bulk) :-
		parse_number(Input, Length),
		phrase(parse_string(Length, Input), Codes), !,
		(	Codes == [nil] ->
			Bulk = nil
		;	atom_codes(String, Codes),
			Bulk = bulk(String)
		).

	parse_string(-1, _) -->
		[nil].
	parse_string(0, Input) -->
		{get_byte(Input, 13), get_byte(Input, 10)},
		[].
	parse_string(N, Input) -->
		{	N > 0,
			get_byte(Input, Byte),
			M is N - 1
		},
		[Byte],
		parse_string(M, Input).

	parse_mbulk(Input, MBulk) :-
		parse_number(Input, Length),
		phrase(parse_mbulk(Length, Input), MBulk0), !,
		(	MBulk0 == [nil] ->
			MBulk = nil
		;	MBulk = MBulk0
		).

	parse_mbulk(-1, _) -->
		[nil].
	parse_mbulk(0, _) -->
		[].
	parse_mbulk(N, Input) -->
		{	N > 0,
			get_byte(Input, Byte),
			char_code(Key, Byte),
			parse_reply(Key, Input, Line),
			N1 is N-1
		},
		[Line],
		parse_mbulk(N1, Input).

	parse_line(Input) -->
		{get_byte(Input, Byte)},
		(	{Byte =:= 13} ->
			{get_byte(Input, _)},
			[]
		;	[Byte],
			parse_line(Input)
		).

	codes([]) -->
		[].
	codes([Code| Codes]) -->
		[Code], codes(Codes).

	print_reply([]).
	print_reply([Head| Tail]) :-
		print_reply(Head),
		print_reply(Tail).
	print_reply(bulk(String)) :-
		print_message(information, redis, reply('STRING', String)).
	print_reply(number(Number)) :-
		print_message(information, redis, reply('NUMBER', Number)).
	print_reply(status(Status)) :-
		print_message(information, redis, reply('STATUS', Status)).
	print_reply(nil) :-
		print_message(information, redis, nil).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, redis) -->
		message_tokens(Message).

	message_tokens(reply(Key, Value)) -->
		['~w: ~w'-[Key, Value], nl].
	message_tokens(nil) -->
		['NIL'-[], nl].

:- end_object.
