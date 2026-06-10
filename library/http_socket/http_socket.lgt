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


:- object(http_socket,
	implements(http_socket_protocol),
	imports([options, http_message_helpers, http_text_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-10,
		comment is 'Sockets-backed HTTP transport predicates built on top of the http_client_core and http_server libraries.',
		remarks is [
			'Supported backends' - 'Availability depends on the supported backends of the sockets library.',
			'Client side' - 'The open_connection/4 and close_connection/1 predicates manage reusable client connections. The open_connection_pool/4 and close_connection_pool/1 predicates manage reusable connection pools. The exchange/3 and exchange_connection/3 predicates operate on an open connection or a connection pool. The exchange/4 and exchange_connection/4 predicates open a client socket, perform one or more HTTP exchanges, and close the connection.',
			'Server side' - 'The open_listener/4 and close_listener/1 predicates manage listener lifecycle. The serve_once/3 predicate serves one accepted connection. The serve_websocket_once/5 predicate serves one WebSocket opening handshake and returns an upgraded connection handle that remains open for use with the http_websocket library or later message-processing layers. The serve_listener/4 predicate provides bounded sequential serving. The serve_listener/5 predicate adds shutdown policy, worker-per-connection orchestration, and bounded worker-pool serving. The serve_until_shutdown/4 and request_shutdown/1 predicates provide externally controlled open-ended serving loops.',
			'Option precedence' - 'When the same listener, serving, or pool-management option is given multiple times, the first occurrence is used.',
			'Connection stream access' - 'The connection_streams/3 predicate exposes the binary input and output streams carried by both reusable client connection handles and upgraded WebSocket connection handles.'
		]
	]).

	:- private(connection_pool_seed_/1).
	:- dynamic(connection_pool_seed_/1).
	:- mode(connection_pool_seed_(?positive_integer), zero_or_one).
	:- info(connection_pool_seed_/1, [
		comment is 'Last allocated connection pool identifier.',
		argnames is ['PoolId']
	]).

	:- private(connection_pool_config_/6).
	:- dynamic(connection_pool_config_/6).
	:- mode(connection_pool_config_(?positive_integer, ?atom, ?integer, ?non_negative_integer, ?positive_integer, ?list), zero_or_more).
	:- info(connection_pool_config_/6, [
		comment is 'Stored connection pool configuration indexed by pool identifier.',
		argnames is ['PoolId', 'Host', 'Port', 'MinSize', 'MaxSize', 'ConnectionOptions']
	]).

	:- private(connection_pool_available_/2).
	:- dynamic(connection_pool_available_/2).
	:- mode(connection_pool_available_(?positive_integer, ?compound), zero_or_more).
	:- info(connection_pool_available_/2, [
		comment is 'Reusable idle client connections indexed by pool identifier.',
		argnames is ['PoolId', 'Connection']
	]).

	:- private(connection_pool_in_use_/2).
	:- dynamic(connection_pool_in_use_/2).
	:- mode(connection_pool_in_use_(?positive_integer, ?compound), zero_or_more).
	:- info(connection_pool_in_use_/2, [
		comment is 'Checked-out client connections indexed by pool identifier.',
		argnames is ['PoolId', 'Connection']
	]).

	:- private(listener_shutdown_seed_/1).
	:- dynamic(listener_shutdown_seed_/1).
	:- mode(listener_shutdown_seed_(?positive_integer), zero_or_one).
	:- info(listener_shutdown_seed_/1, [
		comment is 'Last allocated shutdown run identifier for open-ended listener loops.',
		argnames is ['RunId']
	]).

	:- private(listener_shutdown_control_/3).
	:- dynamic(listener_shutdown_control_/3).
	:- mode(listener_shutdown_control_(?nonvar, ?compound, ?positive_integer), zero_or_more).
	:- info(listener_shutdown_control_/3, [
		comment is 'Registered shutdown control terms and their associated listeners.',
		argnames is ['Control', 'Listener', 'RunId']
	]).

	:- private(listener_shutdown_requested_/2).
	:- dynamic(listener_shutdown_requested_/2).
	:- mode(listener_shutdown_requested_(?nonvar, ?positive_integer), zero_or_more).
	:- info(listener_shutdown_requested_/2, [
		comment is 'Recorded shutdown requests indexed by control term and run identifier.',
		argnames is ['Control', 'RunId']
	]).

	:- private(listener_active_worker_/3).
	:- dynamic(listener_active_worker_/3).
	:- mode(listener_active_worker_(?nonvar, ?positive_integer, ?compound), zero_or_more).
	:- info(listener_active_worker_/3, [
		comment is 'Active listener worker records indexed by control term and run identifier.',
		argnames is ['Control', 'RunId', 'Worker']
	]).

	:- private(listener_endpoint_/3).
	:- dynamic(listener_endpoint_/3).
	:- mode(listener_endpoint_(?compound, ?atom, ?integer), zero_or_more).
	:- info(listener_endpoint_/3, [
		comment is 'Stored listener endpoints indexed by listener handle.',
		argnames is ['Listener', 'Host', 'Port']
	]).

	:- synchronized([
		allocate_connection_pool_id/1,
		register_connection_pool/7,
		acquire_pool_connection/2,
		release_pool_connection/3,
		discard_pool_connection/2,
		close_connection_pool_state/2,
		connection_pool_id_outcome/2,
		allocate_shutdown_run_id/1,
		register_listener_endpoint/3,
		unregister_listener_endpoint/1,
		register_shutdown_control/3,
		cleanup_shutdown_control/2,
		force_shutdown_control/2,
		register_active_worker/3,
		unregister_active_worker/3,
		current_active_workers/3
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.

	:- endif.

	:- uses(list, [
		length/2, member/2, reverse/2, valid/1 as proper_list/1
	]).

	:- meta_predicate(call_with_catch_cleanup(0, 0)).
	:- meta_predicate(call_with_shutdown_policy(*, *, 0)).
	:- if(current_logtalk_flag(threads, supported)).
		:- meta_predicate(spawn_connection_worker(*, *, *, *)).
		:- meta_predicate(spawn_notifying_connection_worker(*, *, *, *, *)).
		:- meta_predicate(collect_finished_connection_worker(*, 0, *, *)).
		:- meta_predicate(collect_finished_worker(*, 0, *, *, *, *)).
		:- meta_predicate(wait_for_workers(*, *)).
	:- endif.

	open_listener(Host, Port, Listener, Options) :-
		socket::server_open(Host, Port, Listener, Options),
		register_listener_endpoint(Listener, Host, Port).

	close_listener(Listener) :-
		unregister_listener_endpoint(Listener),
		socket::server_close(Listener).

	open_connection(Host, Port, http_connection(NormalizedHost, Port, Input, Output), Options) :-
		normalize_connection_endpoint(Host, Port, NormalizedHost),
		socket::client_open(Host, Port, Input, Output, Options).

	close_connection(Connection) :-
		connection_streams(Connection, Input, Output),
		socket::close(Input, Output).

	open_connection_pool(Host, Port, Pool, Options) :-
		parse_connection_pool_options(Options, MinSize, MaxSize, ConnectionOptions),
		normalize_connection_endpoint(Host, Port, NormalizedHost),
		open_initial_pool_connections(MinSize, Host, Port, ConnectionOptions, [], ReversedConnections),
		allocate_connection_pool_id(PoolId),
		register_connection_pool(PoolId, NormalizedHost, Port, MinSize, MaxSize, ConnectionOptions, ReversedConnections),
		Pool = http_connection_pool(NormalizedHost, Port, PoolId).

	close_connection_pool(Pool) :-
		pool_id(Pool, PoolId),
		close_connection_pool_state(PoolId, Outcome),
		close_connection_pool_outcome(Outcome, Pool).

	connection_pool_stats(Pool, Stats) :-
		pool_id(Pool, PoolId),
		connection_pool_id_outcome(PoolId, Outcome),
		connection_pool_stats_outcome(Outcome, Pool, Stats).

	exchange(http_connection_pool(Host, Port, PoolId), Request, Response) :-
		!,
		acquire_managed_connection(http_connection_pool(Host, Port, PoolId), PoolId, Connection),
		catch(
			exchange(Connection, Request, Response),
			Error,
			(	discard_managed_connection(PoolId, Connection),
				throw(Error)
			)
		),
		recycle_managed_connection(PoolId, Connection, Request, Response).
	exchange(Connection, Request, Response) :-
		connection_streams(Connection, Input, Output),
		http_client_core::exchange(Input, Output, Request, Response).

	exchange_connection(http_connection_pool(Host, Port, PoolId), Requests, Responses) :-
		!,
		pool_exchange_connection(PoolId, http_connection_pool(Host, Port, PoolId), Requests, Responses).
	exchange_connection(Connection, Requests, Responses) :-
		connection_streams(Connection, Input, Output),
		http_client_core::exchange_connection(Input, Output, Requests, Responses).

	exchange(Host, Port, Request, Response) :-
		one_shot_request(Request, OneShotRequest),
		socket::client_open(Host, Port, Input, Output),
		call_with_catch_cleanup(
			http_client_core::exchange(Input, Output, OneShotRequest, Response),
			socket::close(Input, Output)
		).

	exchange_connection(_Host, _Port, [], []) :-
		!.
	exchange_connection(Host, Port, Requests0, Responses) :-
		socket::client_open(Host, Port, Input, Output),
		call_with_catch_cleanup(
			(	one_shot_request_sequence(Requests0, Requests),
				http_client_core::exchange_connection(Input, Output, Requests, Responses)
			),
			socket::close(Input, Output)
		).

	serve_once(ServerSocket, Handler, ClientInfo) :-
		socket::server_accept(ServerSocket, Input, Output, ClientInfo),
		serve_accepted_connection(Input, Output, Handler).

	serve_websocket_once(ServerSocket, Handler, Connection, Response, ClientInfo) :-
		socket::server_accept(ServerSocket, Input, Output, ClientInfo),
		catch(
			serve_accepted_websocket(Input, Output, Handler, Connection, Response, ClientInfo),
			Error,
			(	catch(socket::close(Input, Output), _, true),
				throw(Error)
			)
		).

	serve_listener(Listener, Handler, Count, ClientInfos) :-
		serve_listener(Listener, Handler, Count, ClientInfos, []).

	serve_listener(Listener, Handler, Count, ClientInfos, Options) :-
		validate_listener_count(Count),
		parse_listener_options(Options, Shutdown, Workers),
		call_with_shutdown_policy(
			Shutdown,
			Listener,
			serve_listener_with_workers(Workers, Listener, Handler, Count, ClientInfos)
		).

	serve_until_shutdown(Listener, Handler, Control, Options) :-
		serve_until_shutdown_impl(Listener, Handler, Control, Options).

	request_shutdown(Control) :-
		request_shutdown_impl(Control).

	serve_listener_serial(_Listener, _Handler, 0, []) :-
		!.
	serve_listener_serial(Listener, Handler, Count, [ClientInfo| ClientInfos]) :-
		serve_once(Listener, Handler, ClientInfo),
		NextCount is Count - 1,
		serve_listener_serial(Listener, Handler, NextCount, ClientInfos).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		serve_accepted_connection(Input, Output, Handler) :-
			adopt_connection_streams(Input, Output),
			call_with_catch_cleanup(
				http_server::serve_connection(Input, Output, Handler),
				socket::close(Input, Output)
			).

	:- else.

		serve_accepted_connection(Input, Output, Handler) :-
			call_with_catch_cleanup(
				http_server::serve_connection(Input, Output, Handler),
				socket::close(Input, Output)
			).

	:- endif.

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

		serve_accepted_websocket(Input, Output, Handler, Connection, Response, ClientInfo) :-
			adopt_connection_streams(Input, Output),
			http_server::serve_websocket(Input, Output, Handler, Outcome),
			serve_websocket_outcome(Outcome, Input, Output, Connection, Response, ClientInfo).

	:- else.

		serve_accepted_websocket(Input, Output, Handler, Connection, Response, ClientInfo) :-
			http_server::serve_websocket(Input, Output, Handler, Outcome),
			serve_websocket_outcome(Outcome, Input, Output, Connection, Response, ClientInfo).

	:- endif.

	serve_websocket_outcome(accepted(_Request, Response), Input, Output, http_websocket_connection(ClientInfo, Input, Output), Response, ClientInfo).
	serve_websocket_outcome(rejected(Response), _Input, _Output, _Connection, _RejectedResponse, _ClientInfo) :-
		domain_error(http_socket_websocket_response, Response).
	serve_websocket_outcome(end_of_file, _Input, _Output, _Connection, _Response, _ClientInfo) :-
		existence_error(http_socket_websocket_request, end_of_file).

	connection_streams(http_connection(_Host, _Port, Input, Output), Input, Output) :-
		adopt_connection_streams(Input, Output),
		!.
	connection_streams(http_websocket_connection(_ClientInfo, Input, Output), Input, Output) :-
		adopt_connection_streams(Input, Output),
		!.
	connection_streams(Connection, _Input, _Output) :-
		(	var(Connection) ->
			instantiation_error
		;	domain_error(http_socket_connection, Connection)
		).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

	adopt_connection_streams(Input, Output) :-
		{adopt_stream(Input)},
		(	Input == Output ->
			true
		;	{adopt_stream(Output)}
		).

	:- else.

	adopt_connection_streams(_Input, _Output).

	:- endif.

	normalize_connection_endpoint(Host, Port, NormalizedHost) :-
		http_core::request(get, authority(Host, Port), http(1, 1), [], empty, [], _),
		atom_codes(Host, HostCodes),
		lowercase_ascii_codes(HostCodes, NormalizedHostCodes),
		atom_codes(NormalizedHost, NormalizedHostCodes).

	pool_id(http_connection_pool(_Host, _Port, PoolId), PoolId) :-
		!.
	pool_id(Pool, _PoolId) :-
		(	var(Pool) ->
			instantiation_error
		;	domain_error(http_socket_connection_pool, Pool)
		).

	validate_listener_count(Count) :-
		(	var(Count) ->
			instantiation_error
		;	integer(Count),
			Count >= 0 ->
			true
		;	domain_error(non_negative_integer, Count)
		).

	parse_listener_options(Options, Shutdown, Workers) :-
		^^check_options(Options),
		check_listener_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(shutdown(Shutdown), MergedOptions),
		^^option(workers(Workers), MergedOptions).

	parse_open_listener_options(Options, Workers) :-
		^^check_options(Options),
		check_open_listener_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(workers(Workers), MergedOptions).

	parse_connection_pool_options(Options, MinSize, MaxSize, ConnectionOptions) :-
		^^check_options(Options),
		check_connection_pool_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(min_size(MinSize), MergedOptions),
		^^option(max_size(MaxSize), MergedOptions),
		^^option(connection_options(ConnectionOptions), MergedOptions),
		(	MinSize =< MaxSize ->
			true
		;	domain_error(http_socket_connection_pool_options, [min_size(MinSize), max_size(MaxSize)])
		).

	check_listener_options([]).
	check_listener_options([Option| Options]) :-
		check_listener_option(Option),
		check_listener_options(Options).

	check_listener_option(shutdown(_)) :-
		!.
	check_listener_option(workers(_)) :-
		!.
	check_listener_option(Option) :-
		domain_error(http_socket_listener_option, Option).

	check_open_listener_options([]).
	check_open_listener_options([Option| Options]) :-
		check_open_listener_option(Option),
		check_open_listener_options(Options).

	check_open_listener_option(workers(_)) :-
		!.
	check_open_listener_option(Option) :-
		domain_error(http_socket_open_listener_option, Option).

	check_connection_pool_options([]).
	check_connection_pool_options([Option| Options]) :-
		check_connection_pool_option(Option),
		check_connection_pool_options(Options).

	check_connection_pool_option(min_size(_)) :-
		!.
	check_connection_pool_option(max_size(_)) :-
		!.
	check_connection_pool_option(connection_options(_)) :-
		!.
	check_connection_pool_option(Option) :-
		domain_error(http_socket_connection_pool_option, Option).

	valid_option(shutdown(Shutdown)) :-
		once((Shutdown == keep_open; Shutdown == close)).
	valid_option(workers(Worker)) :-
		valid_workers_option(Worker).
	valid_option(min_size(MinSize)) :-
		integer(MinSize),
		MinSize >= 0.
	valid_option(max_size(MaxSize)) :-
		integer(MaxSize),
		MaxSize > 0.
	valid_option(connection_options(ConnectionOptions)) :-
		socket::valid_options(ConnectionOptions).

	default_option(shutdown(keep_open)).
	default_option(workers(serial)).
	default_option(min_size(0)).
	default_option(max_size(10)).
	default_option(connection_options([])).

	valid_workers_option(serial).
	valid_workers_option(per_connection).
	valid_workers_option(pool(Size)) :-
		integer(Size),
		Size > 0.
	valid_workers_option(pool(Size, rolling)) :-
		integer(Size),
		Size > 0.

	call_with_catch_cleanup(Goal, Cleanup) :-
		catch(
			(	call(Goal) ->
				call(Cleanup)
			;	call(Cleanup),
				fail
			),
			Error,
			(	catch(call(Cleanup), _, true),
				throw(Error)
			)
		).

	call_with_shutdown_policy(close, Listener, Goal) :-
		call_with_catch_cleanup(Goal, close_listener(Listener)).
	call_with_shutdown_policy(keep_open, _Listener, Goal) :-
		call(Goal).

	open_initial_pool_connections(0, _Host, _Port, _ConnectionOptions, Connections, Connections) :-
		!.
	open_initial_pool_connections(Count, Host, Port, ConnectionOptions, Connections0, Connections) :-
		open_connection(Host, Port, Connection, ConnectionOptions),
		NextCount is Count - 1,
		catch(
			open_initial_pool_connections(NextCount, Host, Port, ConnectionOptions, [Connection| Connections0], Connections),
			Error,
			(	close_connection_list([Connection| Connections0]),
				throw(Error)
			)
		).

	close_connection_pool_outcome(closed(Connections), _Pool) :-
		close_connection_list(Connections).
	close_connection_pool_outcome(in_use, Pool) :-
		permission_error(close, http_socket_connection_pool, Pool).
	close_connection_pool_outcome(missing, Pool) :-
		existence_error(http_socket_connection_pool, Pool).

	connection_pool_stats_outcome(stats(Available, InUse, Total, MinSize, MaxSize), _Pool, stats(Available, InUse, Total, MinSize, MaxSize)).
	connection_pool_stats_outcome(missing, Pool, _Stats) :-
		existence_error(http_socket_connection_pool, Pool).

	acquire_managed_connection(Pool, PoolId, Connection) :-
		acquire_pool_connection(PoolId, Outcome),
		acquire_pool_connectionoutcome(Outcome, Pool, Connection).

	acquire_pool_connectionoutcome(connection(Connection), _Pool, Connection).
	acquire_pool_connectionoutcome(exhausted, _Pool, _Connection) :-
		resource_error(http_socket_connection_pool).
	acquire_pool_connectionoutcome(missing, Pool, _Connection) :-
		existence_error(http_socket_connection_pool, Pool).

	recycle_managed_connection(PoolId, Connection, Request, Response) :-
		(	^^connection_persistent(Request, Response) ->
			release_pool_connection(PoolId, Connection, Action),
			release_pool_connectionaction(Action, Connection)
		;	discard_managed_connection(PoolId, Connection)
		).

	discard_managed_connection(PoolId, Connection) :-
		discard_pool_connection(PoolId, Connection),
		catch(close_connection(Connection), _, true).

	release_pool_connectionaction(reused, _Connection).
	release_pool_connectionaction(closed, Connection) :-
		catch(close_connection(Connection), _, true).

	pool_exchange_connection(PoolId, Pool, Requests, Responses) :-
		(	Requests == [] ->
			connection_pool_id_outcome(PoolId, Outcome),
			connection_pool_stats_outcome(Outcome, Pool, _Stats),
			Responses = []
		;	acquire_managed_connection(Pool, PoolId, Connection),
			catch(
				exchange_connection(Connection, Requests, Responses),
				Error,
				(	discard_managed_connection(PoolId, Connection),
					throw(Error)
				)
			),
			recycle_managed_connection_sequence(PoolId, Connection, Requests, Responses)
		).

	recycle_managed_connection_sequence(PoolId, Connection, Requests, Responses) :-
		last_request_response(Requests, Responses, Request, Response),
		recycle_managed_connection(PoolId, Connection, Request, Response).

	last_request_response([Request], [Response], Request, Response) :-
		!.
	last_request_response([_Request| Requests], [_Response| Responses], Request, Response) :-
		last_request_response(Requests, Responses, Request, Response).

	one_shot_request(Request0, Request) :-
		(	http_core::is_request(Request0) ->
			ensure_close_connection_request(Request0, Request)
		;	Request = Request0
		).

	one_shot_request_sequence(Requests0, Requests) :-
		(	proper_list(Requests0),
			Requests0 \== [] ->
			one_shot_request_sequence_(Requests0, Requests)
		;	Requests = Requests0
		).

	one_shot_request_sequence_([Request], [OneShotRequest]) :-
		!,
		one_shot_request(Request, OneShotRequest).
	one_shot_request_sequence_([Request| Requests], [Request| ReversedRequests]) :-
		one_shot_request_sequence_(Requests, ReversedRequests).

	ensure_close_connection_request(request(Method, Target, Version, Headers0, Body, Properties0), Request) :-
		normalize_close_connection_headers(Headers0, Headers),
		normalize_close_connection_properties(Properties0, Properties),
		Request = request(Method, Target, Version, Headers, Body, Properties).

	normalize_close_connection_headers([], []).
	normalize_close_connection_headers([connection-Tokens0| Headers0], [connection-Tokens| Headers]) :-
		!,
		force_close_connection_tokens(Tokens0, Tokens),
		normalize_close_connection_headers(Headers0, Headers).
	normalize_close_connection_headers([Header| Headers0], [Header| Headers]) :-
		normalize_close_connection_headers(Headers0, Headers).

	normalize_close_connection_properties([], [connection([close])]).
	normalize_close_connection_properties([connection(Tokens0)| Properties0], [connection(Tokens)| Properties]) :-
		!,
		force_close_connection_tokens(Tokens0, Tokens),
		normalize_close_connection_properties(Properties0, Properties).
	normalize_close_connection_properties([Property| Properties0], [Property| Properties]) :-
		normalize_close_connection_properties(Properties0, Properties).

	force_close_connection_tokens(Tokens0, [close| Tokens]) :-
		remove_persistence_connection_tokens(Tokens0, Tokens).

	remove_persistence_connection_tokens([], []).
	remove_persistence_connection_tokens([close| Tokens0], Tokens) :-
		!,
		remove_persistence_connection_tokens(Tokens0, Tokens).
	remove_persistence_connection_tokens(['keep-alive'| Tokens0], Tokens) :-
		!,
		remove_persistence_connection_tokens(Tokens0, Tokens).
	remove_persistence_connection_tokens([Token| Tokens0], [Token| Tokens]) :-
		remove_persistence_connection_tokens(Tokens0, Tokens).

	lowercase_ascii_codes(Codes, LowercaseCodes) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes).

	close_connection_list([]).
	close_connection_list([Connection| Connections]) :-
		catch(close_connection(Connection), _, true),
		close_connection_list(Connections).

	serve_listener_with_workers(serial, Listener, Handler, Count, ClientInfos) :-
		!,
		serve_listener_serial(Listener, Handler, Count, ClientInfos).
	serve_listener_with_workers(Workers, Listener, Handler, Count, ClientInfos) :-
		serve_listener_with_workers_impl(Workers, Listener, Handler, Count, ClientInfos).

	:- if(current_logtalk_flag(threads, supported)).

	serve_until_shutdown_impl(Listener, Handler, Control, Options) :-
		parse_open_listener_options(Options, Workers),
		register_shutdown_control(Control, Listener, RunId),
		call_with_catch_cleanup(
			serve_until_shutdown_with_workers(Workers, Listener, Handler, Control, RunId),
			(	catch(close_listener(Listener), _, true),
				cleanup_shutdown_control(Control, RunId)
			)
		).

	request_shutdown_impl(Control) :-
		(	var(Control) ->
			instantiation_error
		;	listener_shutdown_control_(Control, _Listener, RunId) ->
			force_shutdown_control(Control, RunId)
		;	existence_error(http_socket_shutdown_control, Control)
		).

	serve_listener_with_workers_impl(per_connection, Listener, Handler, Count, ClientInfos) :-
		catch(
			serve_listener_parallel(Listener, Handler, Count, ClientInfos, [], ReversedWorkers),
			Error,
			(	wait_for_reversed_workers(ReversedWorkers),
				throw(Error)
			)
		),
		wait_for_reversed_workers(ReversedWorkers).

	serve_listener_with_workers_impl(pool(Size), Listener, Handler, Count, ClientInfos) :-
		serve_listener_pool_batch(Listener, Handler, Size, Count, [], ReversedClientInfos),
		reverse(ReversedClientInfos, ClientInfos).

	serve_listener_with_workers_impl(pool(Size, rolling), Listener, Handler, Count, ClientInfos) :-
		allocate_shutdown_run_id(RunId),
		catch(
			serve_listener_pool_rolling(Listener, Handler, RunId, Size, Count, [], ReversedClientInfos, [], ReversedWorkers),
			Error,
			(	wait_for_reversed_workers(ReversedWorkers),
				throw(Error)
			)
		),
		wait_for_reversed_workers(ReversedWorkers),
		reverse(ReversedClientInfos, ClientInfos).

	serve_until_shutdown_with_workers(serial, Listener, Handler, Control, RunId) :-
		serve_until_shutdown_serial(Listener, Handler, Control, RunId).

	serve_until_shutdown_with_workers(per_connection, Listener, Handler, Control, RunId) :-
		serve_until_shutdown_parallel(Listener, Handler, Control, RunId).

	serve_until_shutdown_with_workers(pool(Size), Listener, Handler, Control, RunId) :-
		serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size).

	serve_until_shutdown_with_workers(pool(Size, rolling), Listener, Handler, Control, RunId) :-
		serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size).

	serve_listener_parallel(_Listener, _Handler, 0, [], Workers, Workers) :-
		!.
	serve_listener_parallel(Listener, Handler, Count, [ClientInfo| ClientInfos], Workers0, Workers) :-
		socket::server_accept(Listener, Input, Output, ClientInfo),
		spawn_connection_worker(Input, Output, Handler, Worker),
		NextCount is Count - 1,
		serve_listener_parallel(Listener, Handler, NextCount, ClientInfos, [Worker| Workers0], Workers).

	serve_listener_pool_batch(_Listener, _Handler, _Size, 0, ClientInfos, ClientInfos) :-
		!.
	serve_listener_pool_batch(Listener, Handler, Size, Count, ClientInfos0, ClientInfos) :-
		pool_batch_counts(Size, Count, BatchCount, RemainingCount),
		accept_worker_batch(Listener, Handler, BatchCount, ClientInfos0, ClientInfos1, [], ReversedWorkers),
		wait_for_reversed_workers(ReversedWorkers),
		serve_listener_pool_batch(Listener, Handler, Size, RemainingCount, ClientInfos1, ClientInfos).

	serve_listener_pool_rolling(_Listener, _Handler, _RunId, _Size, 0, ClientInfos, ClientInfos, Workers, Workers) :-
		!.
	serve_listener_pool_rolling(Listener, Handler, RunId, Size, Count, ClientInfos0, ClientInfos, Workers0, Workers) :-
		collect_finished_connection_workers(Workers0, no_error, Workers1, Error),
		(	Error == no_error ->
			true
		;	throw(Error)
		),
		length(Workers1, ActiveWorkers),
		(	ActiveWorkers < Size ->
			socket::server_accept(Listener, Input, Output, ClientInfo),
			spawn_notifying_connection_worker(listener_pool_worker_finished(RunId), Input, Output, Handler, Worker),
			NextCount is Count - 1,
			serve_listener_pool_rolling(Listener, Handler, RunId, Size, NextCount, [ClientInfo| ClientInfos0], ClientInfos, [Worker| Workers1], Workers)
		;	threaded_wait(listener_pool_worker_finished(RunId)),
			serve_listener_pool_rolling(Listener, Handler, RunId, Size, Count, ClientInfos0, ClientInfos, Workers1, Workers)
		).

	accept_worker_batch(_Listener, _Handler, 0, ClientInfos, ClientInfos, Workers, Workers) :-
		!.
	accept_worker_batch(Listener, Handler, BatchCount, ClientInfos0, ClientInfos, Workers0, Workers) :-
		socket::server_accept(Listener, Input, Output, ClientInfo),
		spawn_connection_worker(Input, Output, Handler, Worker),
		NextBatchCount is BatchCount - 1,
		accept_worker_batch(Listener, Handler, NextBatchCount, [ClientInfo| ClientInfos0], ClientInfos, [Worker| Workers0], Workers).

	pool_batch_counts(Size, Count, BatchCount, RemainingCount) :-
		(	Count =< Size ->
			BatchCount = Count,
			RemainingCount = 0
		;	BatchCount = Size,
			RemainingCount is Count - Size
		).

	spawn_connection_worker(Input, Output, Handler, worker(Tag, Goal)) :-
		Goal = serve_accepted_connection(Input, Output, Handler),
		catch(
			threaded_once(Goal, Tag),
			Error,
			(	catch(socket::close(Input, Output), _, true),
				throw(Error)
			)
		).

	spawn_notifying_connection_worker(Notification, Input, Output, Handler, worker(Tag, Goal)) :-
		Goal = call_with_catch_cleanup(
			serve_accepted_connection(Input, Output, Handler),
			threaded_notify(Notification)
		),
		catch(
			threaded_once(Goal, Tag),
			Error,
			(	catch(socket::close(Input, Output), _, true),
				throw(Error)
			)
		).

	wait_for_reversed_workers(ReversedWorkers) :-
		normalize_reversed_list(ReversedWorkers, Workers),
		wait_for_workers(Workers, no_error).

	wait_for_workers([], no_error).
	wait_for_workers([], Error) :-
		throw(Error).
	wait_for_workers([worker(Tag, Goal)| Workers], Error0) :-
		catch(threaded_exit(Goal, Tag), WorkerError, true),
		remember_worker_error(Error0, WorkerError, Error),
		wait_for_workers(Workers, Error).

	remember_worker_error(no_error, WorkerError, error(WorkerError)) :-
		nonvar(WorkerError),
		!.
	remember_worker_error(Error, _WorkerError, Error).

	collect_finished_connection_workers([], Error, [], Error).
	collect_finished_connection_workers([worker(Tag, Goal)| Workers0], Error0, Workers, Error) :-
		(	collect_finished_connection_worker(Tag, Goal, Error0, Error1) ->
			collect_finished_connection_workers(Workers0, Error1, Workers, Error)
		;	Workers = [worker(Tag, Goal)| Workers1],
			collect_finished_connection_workers(Workers0, Error0, Workers1, Error)
		).

	collect_finished_connection_worker(Tag, Goal, Error0, Error) :-
		catch(threaded_peek(Goal, Tag), _, fail),
		catch(threaded_exit(Goal, Tag), WorkerError, true),
		remember_worker_error(Error0, WorkerError, Error).

	:- endif.

	allocate_connection_pool_id(PoolId) :-
		(	retract(connection_pool_seed_(CurrentPoolId)) ->
			PoolId is CurrentPoolId + 1
		;	PoolId = 1
		),
		assertz(connection_pool_seed_(PoolId)).

	register_connection_pool(PoolId, Host, Port, MinSize, MaxSize, ConnectionOptions, ReversedConnections) :-
		assertz(connection_pool_config_(PoolId, Host, Port, MinSize, MaxSize, ConnectionOptions)),
		register_pool_available_connections_(ReversedConnections, PoolId).

	register_listener_endpoint(Listener, Host, Port) :-
		retractall(listener_endpoint_(Listener, _, _)),
		assertz(listener_endpoint_(Listener, Host, Port)).

	unregister_listener_endpoint(Listener) :-
		retractall(listener_endpoint_(Listener, _, _)).

	register_pool_available_connections_([], _PoolId).
	register_pool_available_connections_([Connection| Connections], PoolId) :-
		assertz(connection_pool_available_(PoolId, Connection)),
		register_pool_available_connections_(Connections, PoolId).

	acquire_pool_connection(PoolId, connection(Connection)) :-
		retract(connection_pool_available_(PoolId, Connection)),
		!,
		assertz(connection_pool_in_use_(PoolId, Connection)).
	acquire_pool_connection(PoolId, connection(Connection)) :-
		connection_pool_config_(PoolId, Host, Port, _MinSize, MaxSize, ConnectionOptions),
		connection_pool_connection_counts_(PoolId, Available, InUse),
		Total is Available + InUse,
		Total < MaxSize,
		!,
		open_connection(Host, Port, Connection, ConnectionOptions),
		assertz(connection_pool_in_use_(PoolId, Connection)).
	acquire_pool_connection(PoolId, exhausted) :-
		connection_pool_config_(PoolId, _Host, _Port, _MinSize, _MaxSize, _ConnectionOptions),
		!.
	acquire_pool_connection(_PoolId, missing).

	release_pool_connection(PoolId, Connection, reused) :-
		retract(connection_pool_in_use_(PoolId, Connection)),
		connection_pool_config_(PoolId, _Host, _Port, _MinSize, _MaxSize, _ConnectionOptions),
		!,
		assertz(connection_pool_available_(PoolId, Connection)).
	release_pool_connection(PoolId, Connection, closed) :-
		retractall(connection_pool_in_use_(PoolId, Connection)).

	discard_pool_connection(PoolId, Connection) :-
		retractall(connection_pool_in_use_(PoolId, Connection)),
		retractall(connection_pool_available_(PoolId, Connection)).

	close_connection_pool_state(PoolId, closed(Connections)) :-
		connection_pool_config_(PoolId, _Host, _Port, _MinSize, _MaxSize, _ConnectionOptions),
		\+ connection_pool_in_use_(PoolId, _Connection),
		!,
		retractall(connection_pool_config_(PoolId, _, _, _, _, _)),
		findall(Connection, retract(connection_pool_available_(PoolId, Connection)), Connections).
	close_connection_pool_state(PoolId, in_use) :-
		connection_pool_config_(PoolId, _Host, _Port, _MinSize, _MaxSize, _ConnectionOptions),
		!.
	close_connection_pool_state(_PoolId, missing).

	connection_pool_id_outcome(PoolId, stats(Available, InUse, Total, MinSize, MaxSize)) :-
		connection_pool_config_(PoolId, _Host, _Port, MinSize, MaxSize, _ConnectionOptions),
		!,
		connection_pool_connection_counts_(PoolId, Available, InUse),
		Total is Available + InUse.
	connection_pool_id_outcome(_PoolId, missing).

	connection_pool_connection_counts_(PoolId, Available, InUse) :-
		findall(Connection, connection_pool_available_(PoolId, Connection), AvailableConnections),
		length(AvailableConnections, Available),
		findall(Connection, connection_pool_in_use_(PoolId, Connection), InUseConnections),
		length(InUseConnections, InUse).

	:- if(current_logtalk_flag(threads, supported)).

	register_shutdown_control(Control, Listener, RunId) :-
		(	var(Control) ->
			instantiation_error
		;	\+ listener_shutdown_control_(Control, _, _),
			allocate_shutdown_run_id(RunId),
			assertz(listener_shutdown_control_(Control, Listener, RunId)) ->
			true
		;	permission_error(reuse, http_socket_shutdown_control, Control)
		).

	allocate_shutdown_run_id(RunId) :-
		(	retract(listener_shutdown_seed_(CurrentRunId)) ->
			RunId is CurrentRunId + 1
		;	RunId = 1
		),
		assertz(listener_shutdown_seed_(RunId)).

	cleanup_shutdown_control(Control, RunId) :-
		retractall(listener_active_worker_(Control, RunId, _)),
		retractall(listener_shutdown_requested_(Control, RunId)),
		retractall(listener_shutdown_control_(Control, _, RunId)).

	force_shutdown_control(Control, RunId) :-
		(	listener_shutdown_requested_(Control, RunId) ->
			true
		;	assertz(listener_shutdown_requested_(Control, RunId))
		),
		(	listener_shutdown_control_(Control, Listener, RunId) ->
			signal_shutdown_listener(Listener)
		;	true
		).

	signal_shutdown_listener(Listener) :-
		(	listener_endpoint_(Listener, Host0, Port) ->
			shutdown_signal_host(Host0, Host),
			catch(
				(	socket::client_open(Host, Port, Input, Output, []),
					socket::close(Input, Output)
				),
				_,
				true
			)
		;	true
		).

	shutdown_signal_host('0.0.0.0', '127.0.0.1') :-
		!.
	shutdown_signal_host('::', '::1') :-
		!.
	shutdown_signal_host(Host, Host).

	shutdown_requested(Control, RunId) :-
		listener_shutdown_requested_(Control, RunId).

	serve_until_shutdown_serial(Listener, Handler, Control, RunId) :-
		(	shutdown_requested(Control, RunId) ->
			true
		;	catch(
				serve_once(Listener, Handler, _ClientInfo),
				Error,
				(	shutdown_requested(Control, RunId) ->
					true
				;	force_shutdown_control(Control, RunId),
					throw(Error)
				)
			) ->
			serve_until_shutdown_serial(Listener, Handler, Control, RunId)
		;	force_shutdown_control(Control, RunId),
			throw(unexpted_failure(server_once/3))
		).

	serve_until_shutdown_parallel(Listener, Handler, Control, RunId) :-
		check_finished_workers(Control, RunId),
		(	shutdown_requested(Control, RunId) ->
			wait_for_active_workers(Control, RunId)
		;	try_server_accept(Listener, Control, RunId, Input, Output) ->
			spawn_open_connection_worker(Control, RunId, Input, Output, Handler),
			serve_until_shutdown_parallel(Listener, Handler, Control, RunId)
		;	wait_for_active_workers(Control, RunId)
		).

	serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size) :-
		check_finished_workers(Control, RunId),
		(	shutdown_requested(Control, RunId) ->
			wait_for_active_workers(Control, RunId)
		;	active_worker_count(Control, RunId, Count),
			(	Count < Size ->
				(	try_server_accept(Listener, Control, RunId, Input, Output) ->
					spawn_open_connection_worker(Control, RunId, Input, Output, Handler),
					serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size)
				;	wait_for_active_workers(Control, RunId)
				)
			;	wait_for_one_active_worker(Control, RunId),
				serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size)
			)
		).

	try_server_accept(Listener, Control, RunId, Input, Output) :-
		catch(
			socket::server_accept(Listener, Input, Output, _ClientInfo),
			Error,
			handle_accept_error(Control, RunId, Error)
		).

	handle_accept_error(Control, RunId, _Error) :-
		shutdown_requested(Control, RunId),
		!,
		fail.
	handle_accept_error(Control, RunId, Error) :-
		force_shutdown_control(Control, RunId),
		throw(Error).

	spawn_open_connection_worker(Control, RunId, Input, Output, Handler) :-
		Goal = call_with_catch_cleanup(
			catch(
				serve_accepted_connection(Input, Output, Handler),
				Error,
				(	force_shutdown_control(Control, RunId),
					throw(Error)
				)
			),
			threaded_notify(listener_worker_finished(Control, RunId))
		),
		catch(
			threaded_once(Goal, Tag),
			Error,
			(	force_shutdown_control(Control, RunId),
				catch(socket::close(Input, Output), _, true),
				throw(Error)
			)
		),
		register_active_worker(Control, RunId, worker(Tag, Goal)).

	register_active_worker(Control, RunId, Worker) :-
		assertz(listener_active_worker_(Control, RunId, Worker)).

	unregister_active_worker(Control, RunId, Worker) :-
		retractall(listener_active_worker_(Control, RunId, Worker)).

	current_active_workers(Control, RunId, Workers) :-
		findall(Worker, listener_active_worker_(Control, RunId, Worker), Workers).

	active_worker_count(Control, RunId, Count) :-
		current_active_workers(Control, RunId, Workers),
		length(Workers, Count).

	check_finished_workers(Control, RunId) :-
		collect_finished_workers(Control, RunId, no_error, Error),
		(	Error == no_error ->
			true
		;	throw(Error)
		).

	wait_for_active_workers(Control, RunId) :-
		active_worker_count(Control, RunId, Count),
		(	Count =:= 0 ->
			true
		;	wait_for_one_active_worker(Control, RunId),
			wait_for_active_workers(Control, RunId)
		).

	wait_for_one_active_worker(Control, RunId) :-
		threaded_wait(listener_worker_finished(Control, RunId)),
		collect_finished_workers(Control, RunId, no_error, Error),
		(	Error == no_error ->
			true
		;	throw(Error)
		).

	collect_finished_workers(Control, RunId, Error0, Error) :-
		current_active_workers(Control, RunId, Workers),
		collect_finished_workers(Workers, Control, RunId, Error0, Error).

	collect_finished_workers([], _Control, _RunId, Error, Error).
	collect_finished_workers([worker(Tag, Goal)| Workers], Control, RunId, Error0, Error) :-
		collect_finished_worker(Tag, Goal, Control, RunId, Error0, Error1),
		collect_finished_workers(Workers, Control, RunId, Error1, Error).

	collect_finished_worker(Tag, Goal, Control, RunId, Error0, Error) :-
		(	catch(threaded_peek(Goal, Tag), _, fail) ->
			unregister_active_worker(Control, RunId, worker(Tag, Goal)),
			catch(threaded_exit(Goal, Tag), WorkerError, true),
			remember_worker_error(Error0, WorkerError, Error)
		;	Error = Error0
		).

	normalize_reversed_list(Reversed, List) :-
		(	var(Reversed) ->
			List = []
		;	reverse(Reversed, List)
		).

	:- else.

	serve_until_shutdown_impl(_Listener, _Handler, _Control, _Options) :-
		throw(error(resource_error(threads), http_socket::serve_until_shutdown(_, _, _, _))).

	request_shutdown_impl(_Control) :-
		throw(error(resource_error(threads), http_socket::request_shutdown(_))).

	serve_listener_with_workers_impl(per_connection, _Listener, _Handler, _Count, _ClientInfos) :-
		throw(error(resource_error(threads), http_socket::serve_listener(_, _, _, _, [workers(per_connection)]) )).
	serve_listener_with_workers_impl(pool(_Size), _Listener, _Handler, _Count, _ClientInfos) :-
		throw(error(resource_error(threads), http_socket::serve_listener(_, _, _, _, [workers(pool(_))]) )).
	serve_listener_with_workers_impl(pool(_Size, rolling), _Listener, _Handler, _Count, _ClientInfos) :-
		throw(error(resource_error(threads), http_socket::serve_listener(_, _, _, _, [workers(pool(_, rolling))]) )).

	:- endif.

:- end_object.
