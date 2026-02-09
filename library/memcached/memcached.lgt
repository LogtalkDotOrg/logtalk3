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


:- object(memcached).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-09,
		comment is 'Portable Memcached client implementing the text (ASCII) protocol. Uses the sockets library for TCP communication.',
		remarks is [
			'Supported backends' - 'ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog, and Trealla Prolog (same as the sockets library).',
			'Protocol version' - 'Implements the Memcached text (ASCII) protocol as documented in the official protocol.txt specification.',
			'Connection' - 'The default Memcached port is 11211. Connections are represented as opaque handles that should be passed to all other predicates.',
			'Keys' - 'Keys are atoms up to 250 characters. They must not include control characters or whitespace.',
			'Flags' - 'Flags are 32-bit unsigned integers stored alongside the data, opaque to the server.',
			'Expiration' - 'Expiration time in seconds. 0 means never expire. Values over 30 days (2592000) are treated as Unix timestamps.',
			'Storage commands' - 'Supports set, add, replace, append, prepend, and cas (check-and-set).',
			'Retrieval commands' - 'Supports get, gets (with CAS token), gat, and gats (get-and-touch).',
			'Other commands' - 'Supports delete, incr/decr, touch, flush_all, version, stats, and quit.'
		]
	]).

	% ==========================================================================
	% Public API - Connection Management
	% ==========================================================================

	:- public(connect/3).
	:- mode(connect(+atom, +integer, --compound), one_or_error).
	:- info(connect/3, [
		comment is 'Connects to a Memcached server at the given host and port. Returns a connection handle for subsequent operations.',
		argnames is ['Host', 'Port', 'Connection'],
		exceptions is [
			'Connection refused or network error' - memcached_error(connection_failed)
		]
	]).

	:- public(connect/2).
	:- mode(connect(+atom, --compound), one_or_error).
	:- info(connect/2, [
		comment is 'Connects to a Memcached server at the given host on the default port (11211). Returns a connection handle for subsequent operations.',
		argnames is ['Host', 'Connection'],
		exceptions is [
			'Connection refused or network error' - memcached_error(connection_failed)
		]
	]).

	:- public(disconnect/1).
	:- mode(disconnect(+compound), one).
	:- info(disconnect/1, [
		comment is 'Disconnects from the Memcached server. Sends a quit command before closing.',
		argnames is ['Connection']
	]).

	% ==========================================================================
	% Public API - Storage Commands
	% ==========================================================================

	:- public(set/5).
	:- mode(set(+compound, +atom, +atom, +integer, +integer), one_or_error).
	:- info(set/5, [
		comment is 'Stores the data unconditionally. Overwrites any existing data for the key.',
		argnames is ['Connection', 'Key', 'Value', 'Flags', 'ExpTime'],
		exceptions is [
			'Storage failed' - memcached_error(not_stored),
			'Network error' - memcached_error('Error')
		]
	]).

	:- public(set/3).
	:- mode(set(+compound, +atom, +atom), one_or_error).
	:- info(set/3, [
		comment is 'Stores the data unconditionally with default flags (0) and no expiration (0).',
		argnames is ['Connection', 'Key', 'Value'],
		exceptions is [
			'Storage failed' - memcached_error(not_stored),
			'Network error' - memcached_error('Error')
		]
	]).

	:- public(add/5).
	:- mode(add(+compound, +atom, +atom, +integer, +integer), one_or_error).
	:- info(add/5, [
		comment is 'Stores the data only if the key does not already exist.',
		argnames is ['Connection', 'Key', 'Value', 'Flags', 'ExpTime'],
		exceptions is [
			'Key already exists' - memcached_error(not_stored),
			'Network error' - memcached_error('Error')
		]
	]).

	:- public(replace/5).
	:- mode(replace(+compound, +atom, +atom, +integer, +integer), one_or_error).
	:- info(replace/5, [
		comment is 'Stores the data only if the key already exists.',
		argnames is ['Connection', 'Key', 'Value', 'Flags', 'ExpTime'],
		exceptions is [
			'Key does not exist' - memcached_error(not_stored),
			'Network error' - memcached_error('Error')
		]
	]).

	:- public(append/3).
	:- mode(append(+compound, +atom, +atom), one_or_error).
	:- info(append/3, [
		comment is 'Appends the data to the end of an existing item''s data.',
		argnames is ['Connection', 'Key', 'Value'],
		exceptions is [
			'Key does not exist' - memcached_error(not_stored),
			'Network error' - memcached_error('Error')
		]
	]).

	:- public(prepend/3).
	:- mode(prepend(+compound, +atom, +atom), one_or_error).
	:- info(prepend/3, [
		comment is 'Prepends the data to the beginning of an existing item''s data.',
		argnames is ['Connection', 'Key', 'Value'],
		exceptions is [
			'Key does not exist' - memcached_error(not_stored),
			'Network error' - memcached_error('Error')
		]
	]).

	:- public(cas/6).
	:- mode(cas(+compound, +atom, +atom, +integer, +integer, +integer), one_or_error).
	:- info(cas/6, [
		comment is 'Stores the data only if no one else has updated it since the given CAS unique value was obtained (via gets/3).',
		argnames is ['Connection', 'Key', 'Value', 'Flags', 'ExpTime', 'CasUnique'],
		exceptions is [
			'CAS value mismatch (item modified by another client)' - memcached_error(exists),
			'Key does not exist' - memcached_error(not_found),
			'Network error' - memcached_error('Error')
		]
	]).

	% ==========================================================================
	% Public API - Retrieval Commands
	% ==========================================================================

	:- public(get/3).
	:- mode(get(+compound, +atom, -atom), zero_or_one_or_error).
	:- info(get/3, [
		comment is 'Retrieves the value associated with the key. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'Value']
	]).

	:- public(get/4).
	:- mode(get(+compound, +atom, -atom, -integer), zero_or_one_or_error).
	:- info(get/4, [
		comment is 'Retrieves the value and flags associated with the key. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'Value', 'Flags']
	]).

	:- public(gets/4).
	:- mode(gets(+compound, +atom, -atom, -integer), zero_or_one_or_error).
	:- info(gets/4, [
		comment is 'Retrieves the value and CAS unique token for the key. Fails if the key is not found. The CAS value is used with the cas/6 predicate.',
		argnames is ['Connection', 'Key', 'Value', 'CasUnique']
	]).

	:- public(gets/5).
	:- mode(gets(+compound, +atom, -atom, -integer, -integer), zero_or_one_or_error).
	:- info(gets/5, [
		comment is 'Retrieves the value, flags, and CAS unique token for the key. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'Value', 'Flags', 'CasUnique']
	]).

	:- public(mget/3).
	:- mode(mget(+compound, +list(atom), -list(compound)), one_or_error).
	:- info(mget/3, [
		comment is 'Retrieves multiple keys at once. Returns a list of ``item(Key, Value, Flags)`` terms for found keys.',
		argnames is ['Connection', 'Keys', 'Items']
	]).

	% ==========================================================================
	% Public API - Deletion
	% ==========================================================================

	:- public(delete/2).
	:- mode(delete(+compound, +atom), zero_or_one_or_error).
	:- info(delete/2, [
		comment is 'Deletes the item with the given key. Fails if the key is not found.',
		argnames is ['Connection', 'Key']
	]).

	% ==========================================================================
	% Public API - Increment/Decrement
	% ==========================================================================

	:- public(incr/4).
	:- mode(incr(+compound, +atom, +integer, -integer), zero_or_one_or_error).
	:- info(incr/4, [
		comment is 'Increments the numeric value of the given key by the specified amount. Returns the new value. The item must already exist and contain a decimal representation of a 64-bit unsigned integer. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'Amount', 'NewValue']
	]).

	:- public(decr/4).
	:- mode(decr(+compound, +atom, +integer, -integer), zero_or_one_or_error).
	:- info(decr/4, [
		comment is 'Decrements the numeric value of the given key by the specified amount. Returns the new value. Underflow is caught: decrementing below 0 yields 0. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'Amount', 'NewValue']
	]).

	% ==========================================================================
	% Public API - Touch
	% ==========================================================================

	:- public(touch/3).
	:- mode(touch(+compound, +atom, +integer), zero_or_one_or_error).
	:- info(touch/3, [
		comment is 'Updates the expiration time of the given key without fetching the data. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'ExpTime']
	]).

	:- public(gat/4).
	:- mode(gat(+compound, +atom, +integer, -atom), zero_or_one_or_error).
	:- info(gat/4, [
		comment is 'Gets the value of the key and updates its expiration time. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'ExpTime', 'Value']
	]).

	:- public(gats/5).
	:- mode(gats(+compound, +atom, +integer, -atom, -integer), zero_or_one_or_error).
	:- info(gats/5, [
		comment is 'Gets the value and CAS unique token of the key and updates its expiration time. Fails if the key is not found.',
		argnames is ['Connection', 'Key', 'ExpTime', 'Value', 'CasUnique']
	]).

	% ==========================================================================
	% Public API - Server Commands
	% ==========================================================================

	:- public(flush_all/1).
	:- mode(flush_all(+compound), one_or_error).
	:- info(flush_all/1, [
		comment is 'Invalidates all existing items immediately.',
		argnames is ['Connection']
	]).

	:- public(flush_all/2).
	:- mode(flush_all(+compound, +integer), one_or_error).
	:- info(flush_all/2, [
		comment is 'Invalidates all existing items after the specified number of seconds.',
		argnames is ['Connection', 'Delay']
	]).

	:- public(version/2).
	:- mode(version(+compound, -atom), one_or_error).
	:- info(version/2, [
		comment is 'Returns the version string of the Memcached server.',
		argnames is ['Connection', 'Version']
	]).

	:- public(stats/2).
	:- mode(stats(+compound, -list(compound)), one_or_error).
	:- info(stats/2, [
		comment is 'Returns general-purpose statistics as a list of ``stat(Name, Value)`` terms.',
		argnames is ['Connection', 'Stats']
	]).

	:- public(stats/3).
	:- mode(stats(+compound, +atom, -list(compound)), one_or_error).
	:- info(stats/3, [
		comment is 'Returns statistics for the given argument (e.g. ``items``, ``slabs``, ``sizes``) as a list of ``stat(Name, Value)`` terms.',
		argnames is ['Connection', 'Argument', 'Stats']
	]).

	:- uses(list, [
		length/2, member/2, reverse/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	% ==========================================================================
	% Implementation - Connection Management
	% ==========================================================================

	connect(Host, Port, Connection) :-
		context(Context),
		catch(
			socket::client_open(Host, Port, Input, Output, []),
			_,
			throw(error(memcached_error(connection_failed), Context))
		),
		Connection = connection(Input, Output, Host, Port).

	connect(Host, Connection) :-
		connect(Host, 11211, Connection).

	disconnect(Connection) :-
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atom_codes('quit\r\n', QuitCommand),
		catch(
			(	send_command(Output, QuitCommand),
				socket::close(Input, Output)
			),
			_,
			(	catch(socket::close(Input, Output), _, true)
			)
		).

	% ==========================================================================
	% Implementation - Storage Commands
	% ==========================================================================

	set(Connection, Key, Value, Flags, ExpTime) :-
		storage_command(Connection, set, Key, Value, Flags, ExpTime).

	set(Connection, Key, Value) :-
		set(Connection, Key, Value, 0, 0).

	add(Connection, Key, Value, Flags, ExpTime) :-
		storage_command(Connection, add, Key, Value, Flags, ExpTime).

	replace(Connection, Key, Value, Flags, ExpTime) :-
		storage_command(Connection, replace, Key, Value, Flags, ExpTime).

	append(Connection, Key, Value) :-
		% append and prepend ignore flags and exptime
		storage_command(Connection, append, Key, Value, 0, 0).

	prepend(Connection, Key, Value) :-
		storage_command(Connection, prepend, Key, Value, 0, 0).

	cas(Connection, Key, Value, Flags, ExpTime, CasUnique) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atom_length(Value, Bytes),
		% Build: cas <key> <flags> <exptime> <bytes> <cas_unique>\r\n <data>\r\n
		atomic_list_concat(['cas ', Key, ' ', Flags, ' ', ExpTime, ' ', Bytes, ' ', CasUnique, '\r\n', Value, '\r\n'], FullCommandAtom),
		atom_codes(FullCommandAtom, FullCommand),
		catch(
			(	send_command(Output, FullCommand),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		handle_storage_response(Response, Context).

	% Generic storage command (set, add, replace, append, prepend)
	% Format: <command> <key> <flags> <exptime> <bytes>\r\n<data>\r\n
	storage_command(Connection, Command, Key, Value, Flags, ExpTime) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atom_length(Value, Bytes),
		% Build: <command> <key> <flags> <exptime> <bytes>\r\n <data>\r\n
		atomic_list_concat([Command, ' ', Key, ' ', Flags, ' ', ExpTime, ' ', Bytes, '\r\n', Value, '\r\n'], FullCommandAtom),
		atom_codes(FullCommandAtom, FullCommand),
		catch(
			(	send_command(Output, FullCommand),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		handle_storage_response(Response, Context).

	handle_storage_response(Response, _) :-
		atom_codes('STORED', StoredCodes),
		starts_with(Response, StoredCodes),
		!.
	handle_storage_response(Response, Context) :-
		atom_codes('NOT_STORED', NotStoredCodes),
		starts_with(Response, NotStoredCodes),
		!,
		throw(error(memcached_error(not_stored), Context)).
	handle_storage_response(Response, Context) :-
		atom_codes('EXISTS', ExistsCodes),
		starts_with(Response, ExistsCodes),
		!,
		throw(error(memcached_error(exists), Context)).
	handle_storage_response(Response, Context) :-
		atom_codes('NOT_FOUND', NotFoundCodes),
		starts_with(Response, NotFoundCodes),
		!,
		throw(error(memcached_error(not_found), Context)).
	handle_storage_response(Response, Context) :-
		atom_codes(Message, Response),
		throw(error(memcached_error(server_error(Message)), Context)).

	% ==========================================================================
	% Implementation - Retrieval Commands
	% ==========================================================================

	get(Connection, Key, Value) :-
		get(Connection, Key, Value, _).

	get(Connection, Key, Value, Flags) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['get ', Key, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_retrieval_response(Input, Items)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		member(item(Key, Value, Flags), Items).

	gets(Connection, Key, Value, CasUnique) :-
		gets(Connection, Key, Value, _, CasUnique).

	gets(Connection, Key, Value, Flags, CasUnique) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['gets ', Key, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_retrieval_response(Input, Items)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		member(item(Key, Value, Flags, CasUnique), Items).

	mget(Connection, Keys, Items) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(Keys, ' ', KeysAtom),
		atomic_list_concat(['get ', KeysAtom, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_retrieval_response(Input, Items)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		).



	% ==========================================================================
	% Implementation - Deletion
	% ==========================================================================

	delete(Connection, Key) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['delete ', Key, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		atom_codes('DELETED', DeletedCodes),
		starts_with(Response, DeletedCodes).

	% ==========================================================================
	% Implementation - Increment/Decrement
	% ==========================================================================

	incr(Connection, Key, Amount, NewValue) :-
		incr_decr_command(Connection, incr, Key, Amount, NewValue).

	decr(Connection, Key, Amount, NewValue) :-
		incr_decr_command(Connection, decr, Key, Amount, NewValue).

	incr_decr_command(Connection, Command, Key, Amount, NewValue) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat([Command, ' ', Key, ' ', Amount, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		atom_codes('NOT_FOUND', NotFoundCodes),
		(	starts_with(Response, NotFoundCodes) ->
			fail
		;	strip_crlf(Response, ValueCodes),
			number_codes(NewValue, ValueCodes)
		).

	% ==========================================================================
	% Implementation - Touch
	% ==========================================================================

	touch(Connection, Key, ExpTime) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['touch ', Key, ' ', ExpTime, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		atom_codes('TOUCHED', TouchedCodes),
		starts_with(Response, TouchedCodes).

	gat(Connection, Key, ExpTime, Value) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['gat ', ExpTime, ' ', Key, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_retrieval_response(Input, Items)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		member(item(Key, Value, _), Items).

	gats(Connection, Key, ExpTime, Value, CasUnique) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['gats ', ExpTime, ' ', Key, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_retrieval_response(Input, Items)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		member(item(Key, Value, _, CasUnique), Items).

	% ==========================================================================
	% Implementation - Server Commands
	% ==========================================================================

	flush_all(Connection) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atom_codes('flush_all\r\n', FlushCommand),
		catch(
			(	send_command(Output, FlushCommand),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		atom_codes('OK', OkCodes),
		(	starts_with(Response, OkCodes) ->
			true
		;	atom_codes(Message, Response),
			throw(error(memcached_error(server_error(Message)), Context))
		).

	flush_all(Connection, Delay) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['flush_all ', Delay, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		atom_codes('OK', OkCodes),
		(	starts_with(Response, OkCodes) ->
			true
		;	atom_codes(Message, Response),
			throw(error(memcached_error(server_error(Message)), Context))
		).

	version(Connection, Version) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atom_codes('version\r\n', VersionCommand),
		catch(
			(	send_command(Output, VersionCommand),
				read_line(Input, Response)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		),
		atom_codes('VERSION ', VersionPrefix),
		(	starts_with(Response, VersionPrefix) ->
			length(VersionPrefix, PrefixLen),
			length(Response, ResponseLen),
			SkipLen is ResponseLen - PrefixLen,
			length(VersionCodes, SkipLen),
			list::append(VersionPrefix, VersionCodes, Response),
			strip_crlf(VersionCodes, StrippedCodes),
			atom_codes(Version, StrippedCodes)
		;	atom_codes(Message, Response),
			throw(error(memcached_error(server_error(Message)), Context))
		).

	stats(Connection, Stats) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atom_codes('stats\r\n', StatsCommand),
		catch(
			(	send_command(Output, StatsCommand),
				read_stats_response(Input, Stats)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		).

	stats(Connection, Argument, Stats) :-
		context(Context),
		arg(1, Connection, Input),
		arg(2, Connection, Output),
		atomic_list_concat(['stats ', Argument, '\r\n'], CommandLineAtom),
		atom_codes(CommandLineAtom, CommandLine),
		catch(
			(	send_command(Output, CommandLine),
				read_stats_response(Input, Stats)
			),
			Error,
			throw(error(memcached_error(Error), Context))
		).

	% ==========================================================================
	% Implementation - Low-Level I/O
	% ==========================================================================

	% Send command bytes to the server
	send_command(Stream, Codes) :-
		write_codes(Stream, Codes),
		flush_output(Stream).

	write_codes(_, []).
	write_codes(Stream, [Code| Codes]) :-
		put_byte(Stream, Code),
		write_codes(Stream, Codes).

	% Read a line terminated by \r\n from the stream
	read_line(Stream, Line) :-
		read_line_acc(Stream, [], Line).

	read_line_acc(Stream, Acc, Line) :-
		get_byte(Stream, Byte),
		(	Byte == -1 ->
			reverse(Acc, Line)
		;	Byte == 10 ->  % LF - end of line
			reverse(Acc, Line)
		;	Byte == 13 ->  % CR - expect LF
			get_byte(Stream, Next),
			(	Next == 10 ->
				reverse(Acc, Line)
			;	read_line_acc(Stream, [Next, 13| Acc], Line)
			)
		;	read_line_acc(Stream, [Byte| Acc], Line)
		).

	% Read exactly N bytes from the stream
	read_bytes(_, 0, []) :-
		!.
	read_bytes(Stream, N, [Byte| Rest]) :-
		N > 0,
		get_byte(Stream, Byte),
		N1 is N - 1,
		read_bytes(Stream, N1, Rest).

	% ==========================================================================
	% Implementation - Response Parsing
	% ==========================================================================

	% Parse retrieval responses: VALUE lines followed by END
	read_retrieval_response(Stream, Items) :-
		read_retrieval_response_acc(Stream, [], Items).

	read_retrieval_response_acc(Stream, Acc, Items) :-
		read_line(Stream, Line),
		atom_codes('END', EndCodes),
		(	starts_with(Line, EndCodes) ->
			reverse(Acc, Items)
		;	atom_codes('VALUE ', ValuePrefix),
			starts_with(Line, ValuePrefix) ->
			% Parse: VALUE <key> <flags> <bytes> [<cas_unique>]
			length(ValuePrefix, PrefixLen),
			length(Line, LineLen),
			RestLen is LineLen - PrefixLen,
			length(RestCodes, RestLen),
			list::append(ValuePrefix, RestCodes, Line),
			parse_value_header(RestCodes, Key, Flags, Bytes, MaybeCas),
			% Read the data block (Bytes bytes + \r\n)
			read_bytes(Stream, Bytes, DataCodes),
			% Consume trailing \r\n
			get_byte(Stream, _CR),
			get_byte(Stream, _LF),
			atom_codes(Value, DataCodes),
			(	MaybeCas == none ->
				Item = item(Key, Value, Flags)
			;	Item = item(Key, Value, Flags, MaybeCas)
			),
			read_retrieval_response_acc(Stream, [Item| Acc], Items)
		;	% Unexpected line, skip it
			read_retrieval_response_acc(Stream, Acc, Items)
		).

	% Parse "key flags bytes [cas_unique]" from VALUE response header
	parse_value_header(Codes, Key, Flags, Bytes, MaybeCas) :-
		split_spaces(Codes, Parts),
		(	Parts = [KeyCodes, FlagsCodes, BytesCodes, CasCodes] ->
			atom_codes(Key, KeyCodes),
			number_codes(Flags, FlagsCodes),
			number_codes(Bytes, BytesCodes),
			number_codes(MaybeCas, CasCodes)
		;	Parts = [KeyCodes, FlagsCodes, BytesCodes] ->
			atom_codes(Key, KeyCodes),
			number_codes(Flags, FlagsCodes),
			number_codes(Bytes, BytesCodes),
			MaybeCas = none
		;	fail
		).

	% Parse stats responses: STAT lines followed by END
	read_stats_response(Stream, Stats) :-
		read_stats_response_acc(Stream, [], Stats).

	read_stats_response_acc(Stream, Acc, Stats) :-
		read_line(Stream, Line),
		atom_codes('END', EndCodes),
		(	starts_with(Line, EndCodes) ->
			reverse(Acc, Stats)
		;	atom_codes('STAT ', StatPrefix),
			starts_with(Line, StatPrefix) ->
			% Parse: STAT <name> <value>
			length(StatPrefix, PrefixLen),
			length(Line, LineLen),
			RestLen is LineLen - PrefixLen,
			length(RestCodes, RestLen),
			list::append(StatPrefix, RestCodes, Line),
			parse_stat_line(RestCodes, Name, Value),
			read_stats_response_acc(Stream, [stat(Name, Value)| Acc], Stats)
		;	% Unexpected line, skip it
			read_stats_response_acc(Stream, Acc, Stats)
		).

	parse_stat_line(Codes, Name, Value) :-
		% Split at first space
		list::append(NameCodes, [32| ValueCodes], Codes),
		!,
		atom_codes(Name, NameCodes),
		strip_crlf(ValueCodes, StrippedCodes),
		atom_codes(Value, StrippedCodes).

	% ==========================================================================
	% Auxiliary Predicates
	% ==========================================================================

	% Check if a codes list starts with a given prefix
	starts_with(List, Prefix) :-
		list::append(Prefix, _, List).

	% Strip trailing \r\n from a codes list
	strip_crlf(Codes, Stripped) :-
		(	list::append(Core, [13, 10], Codes) ->
			Stripped = Core
		;	list::append(Core, [10], Codes) ->
			Stripped = Core
		;	list::append(Core, [13], Codes) ->
			Stripped = Core
		;	Stripped = Codes
		).

	% Split codes at spaces
	split_spaces(Codes, Parts) :-
		split_spaces_acc(Codes, [], [], Parts).

	split_spaces_acc([], [], Acc, Parts) :-
		!,
		reverse(Acc, Parts).
	split_spaces_acc([], Current, Acc, Parts) :-
		!,
		reverse(Current, Part),
		reverse([Part| Acc], Parts).
	split_spaces_acc([32| Rest], Current, Acc, Parts) :-
		!,
		(	Current == [] ->
			split_spaces_acc(Rest, [], Acc, Parts)
		;	reverse(Current, Part),
			split_spaces_acc(Rest, [], [Part| Acc], Parts)
		).
	split_spaces_acc([Code| Rest], Current, Acc, Parts) :-
		split_spaces_acc(Rest, [Code| Current], Acc, Parts).

:- end_object.
