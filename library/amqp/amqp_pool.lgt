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


:- category(amqp_pool).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-11,
		comment is 'AMQP connection pool category. Import this category into an object to create a named connection pool with automatic connection management.'
	]).

	:- uses(list, [
		length/2, member/2
	]).

	% ==========================================================================
	% Public API
	% ==========================================================================

	:- public(initialize/1).
	:- mode(initialize(+list), one_or_error).

	:- info(initialize/1, [
		comment is 'Initializes the connection pool with the given configuration options. Must be called before using other pool predicates.',
		argnames is ['Options'],
		remarks is [
			'Option host(Host)' - 'AMQP server hostname. Default is localhost.',
			'Option port(Port)' - 'AMQP server port. Default is 5672.',
			'Option min_size(N)' - 'Minimum number of connections to maintain. Default is 1.',
			'Option max_size(N)' - 'Maximum number of connections allowed. Default is 10.',
			'Option connection_options(Options)' - 'Options passed to amqp::connect/4. Default is [].'
		]
	]).

	:- public(destroy/0).
	:- mode(destroy, one).
	:- info(destroy/0, [
		comment is 'Destroys the pool, closing all connections and clearing state.'
	]).

	:- public(acquire/1).
	:- mode(acquire(--compound), one_or_error).
	:- info(acquire/1, [
		comment is 'Acquires a connection from the pool. Returns an available connection or creates a new one if the pool is not at maximum capacity.',
		argnames is ['Connection'],
		exceptions is [
			'Pool not initialized' - pool_error(not_initialized),
			'Pool exhausted (at max capacity)' - pool_error(exhausted)
		]
	]).

	:- public(release/1).
	:- mode(release(+compound), one).
	:- info(release/1, [
		comment is 'Releases a connection back to the pool, making it available for reuse.',
		argnames is ['Connection']
	]).

	:- public(with_connection/1).
	:- mode(with_connection(+callable), zero_or_more).
	:- meta_predicate(with_connection(1)).
	:- info(with_connection/1, [
		comment is 'Acquires a connection, calls Goal with the connection as argument, and releases the connection. The connection is released even if Goal fails or throws an exception.',
		argnames is ['Goal']
	]).

	:- public(stats/1).
	:- mode(stats(-compound), one).
	:- info(stats/1, [
		comment is 'Returns pool statistics as a compound term stats(Available, InUse, Total, MinSize, MaxSize).',
		argnames is ['Stats']
	]).

	:- public(resize/2).
	:- mode(resize(+integer, +integer), one_or_error).
	:- info(resize/2, [
		comment is 'Resizes the pool by setting new minimum and maximum sizes.',
		argnames is ['MinSize', 'MaxSize']
	]).

	% ==========================================================================
	% Private State
	% ==========================================================================

	:- private(pool_config/5).
	:- dynamic(pool_config/5).
	:- mode(pool_config(?atom, ?integer, ?integer, ?integer, ?list), zero_or_one).
	:- info(pool_config/5, [
		comment is 'Stores the pool configuration parameters.',
		argnames is ['Host', 'Port', 'MinSize', 'MaxSize', 'ConnectionOptions']
	]).

	:- private(available/1).
	:- dynamic(available/1).
	:- mode(available(?compound), zero_or_more).
	:- info(available/1, [
		comment is 'Tracks connections that are available for use.',
		argnames is ['Connection']
	]).

	:- private(in_use/2).
	:- dynamic(in_use/2).
	:- mode(in_use(?compound, ?compound), zero_or_more).
	:- info(in_use/2, [
		comment is 'Tracks connections currently in use along with their acquisition timestamp.',
		argnames is ['Connection', 'AcquireTimestamp']
	]).

	% ==========================================================================
	% Implementation
	% ==========================================================================

	initialize(Options) :-
		% Clear any existing state
		::retractall(pool_config(_, _, _, _, _)),
		::retractall(available(_)),
		::retractall(in_use(_, _)),
		% Extract configuration
		option(host(Host), Options, localhost),
		option(port(Port), Options, 5672),
		option(min_size(MinSize), Options, 1),
		option(max_size(MaxSize), Options, 10),
		option(connection_options(ConnOptions), Options, []),
		% Validate configuration
		(	MinSize > MaxSize ->
			throw(error(pool_error(invalid_config('min_size > max_size')), _))
		;	true
		),
		% Store configuration
		::assertz(pool_config(Host, Port, MinSize, MaxSize, ConnOptions)),
		% Pre-populate pool with minimum connections
		create_initial_connections(Host, Port, ConnOptions, MinSize).

	destroy :-
		% Close all available connections
		forall(
			::retract(available(Connection)),
			catch(amqp::close(Connection), _, true)
		),
		% Close all in-use connections
		forall(
			::retract(in_use(Connection, _)),
			catch(amqp::close(Connection), _, true)
		),
		% Clear configuration
		::retractall(pool_config(_, _, _, _, _)).

	acquire(Connection) :-
		% Check pool is initialized
		(	::pool_config(Host, Port, _, MaxSize, ConnOptions) ->
			true
		;	throw(error(pool_error(not_initialized), _))
		),
		% Try to get an available connection
		(	::retract(available(Connection)) ->
			% Verify connection is still alive
			(	amqp::connection_alive(Connection) ->
				% Mark as in-use
				current_timestamp(Timestamp),
				::assertz(in_use(Connection, Timestamp))
			;	% Connection died, try again
				acquire(Connection)
			)
		;	% No available connection, try to create new one
			total_connections(Total),
			(	Total < MaxSize ->
				% Create new connection
				amqp::connect(Host, Port, Connection, ConnOptions),
				current_timestamp(Timestamp),
				::assertz(in_use(Connection, Timestamp))
			;	% Pool exhausted
				throw(error(pool_error(exhausted), _))
			)
		).

	release(Connection) :-
		(	::retract(in_use(Connection, _)) ->
			% Check if connection is still alive
			(	amqp::connection_alive(Connection) ->
				::assertz(available(Connection))
			;	% Connection died, don't return to pool
				true
			)
		;	% Connection not tracked, ignore
			true
		).

	with_connection(Goal) :-
		::acquire(Connection),
		(	catch(call(Goal, Connection), Error, (::release(Connection), throw(Error))) ->
			::release(Connection)
		;	::release(Connection),
			fail
		).

	stats(stats(Available, InUse, Total, MinSize, MaxSize)) :-
		(	::pool_config(_, _, MinSize, MaxSize, _) ->
			findall(C, ::available(C), AvailableList),
			length(AvailableList, Available),
			findall(C, ::in_use(C, _), InUseList),
			length(InUseList, InUse),
			Total is Available + InUse
		;	Available = 0,
			InUse = 0,
			Total = 0,
			MinSize = 0,
			MaxSize = 0
		).

	resize(MinSize, MaxSize) :-
		(	::pool_config(Host, Port, _, _, ConnOptions) ->
			true
		;	throw(error(pool_error(not_initialized), _))
		),
		(	MinSize > MaxSize ->
			throw(error(pool_error(invalid_config('min_size > max_size')), _))
		;	true
		),
		::retract(pool_config(Host, Port, _, _, ConnOptions)),
		::assertz(pool_config(Host, Port, MinSize, MaxSize, ConnOptions)),
		% Ensure minimum connections exist
		total_connections(Total),
		(	Total < MinSize ->
			ConnectionsNeeded is MinSize - Total,
			create_connections(Host, Port, ConnOptions, ConnectionsNeeded)
		;	true
		).

	% ==========================================================================
	% Private Helpers
	% ==========================================================================

	:- private(create_initial_connections/4).

	create_initial_connections(_, _, _, 0) :-
		!.
	create_initial_connections(Host, Port, Options, N) :-
		N > 0,
		catch(
			(	amqp::connect(Host, Port, Connection, Options),
				::assertz(available(Connection))
			),
			_,
			true  % Ignore connection failures during initialization
		),
		N1 is N - 1,
		create_initial_connections(Host, Port, Options, N1).

	:- private(create_connections/4).

	create_connections(_, _, _, 0) :-
		!.
	create_connections(Host, Port, Options, N) :-
		N > 0,
		amqp::connect(Host, Port, Connection, Options),
		::assertz(available(Connection)),
		N1 is N - 1,
		create_connections(Host, Port, Options, N1).

	:- private(total_connections/1).

	total_connections(Total) :-
		findall(C, ::available(C), AvailableList),
		findall(C, ::in_use(C, _), InUseList),
		length(AvailableList, Available),
		length(InUseList, InUse),
		Total is Available + InUse.

	:- private(current_timestamp/1).

	current_timestamp(Timestamp) :-
		os::date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		Timestamp = timestamp(Year, Month, Day, Hours, Minutes, Seconds).

	:- private(option/3).

	option(Option, Options, _) :-
		member(Option, Options),
		!.
	option(Option, _, Default) :-
		Option =.. [_, Default].

:- end_category.
