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


:- object(http_process_transport,
	implements(http_transport_protocol),
	imports([options, http_message_helpers, http_text_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Process-backed HTTP transport predicates using the process library and helper processes.'
	]).

	:- public(temporary_tls_credentials/3).
	:- mode(temporary_tls_credentials(+atom, -atom, -atom), one_or_error).
	:- info(temporary_tls_credentials/3, [
		comment is 'Creates or reuses temporary TLS certificate and key files under the system temporary directory using the given file name prefix.',
		argnames is ['Prefix', 'CertificateFile', 'KeyFile']
	]).

	:- public(temporary_tls_credentials_files/3).
	:- mode(temporary_tls_credentials_files(+atom, -atom, -atom), one_or_error).
	:- info(temporary_tls_credentials_files/3, [
		comment is 'Computes the temporary TLS certificate and key file paths used for a given file name prefix without creating the files.',
		argnames is ['Prefix', 'CertificateFile', 'KeyFile']
	]).

	:- private(process_connection_state_/3).
	:- dynamic(process_connection_state_/3).
	:- mode(process_connection_state_(?compound, ?ground, ?stream), zero_or_more).
	:- info(process_connection_state_/3, [
		comment is 'Registered helper-process state for open reusable client connections.',
		argnames is ['Connection', 'Process', 'Error']
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

	:- private(listener_seed_/1).
	:- dynamic(listener_seed_/1).
	:- mode(listener_seed_(?positive_integer), zero_or_one).
	:- info(listener_seed_/1, [
		comment is 'Last allocated listener identifier.',
		argnames is ['ListenerId']
	]).

	:- private(process_listener_state_/5).
	:- dynamic(process_listener_state_/5).
	:- mode(process_listener_state_(?positive_integer, ?atom, ?ground, ?compound, ?stream), zero_or_more).
	:- info(process_listener_state_/5, [
		comment is 'Registered process state for open listeners.',
		argnames is ['ListenerId', 'Executable', 'Process', 'RelayListener', 'Error']
	]).

	:- private(listener_temporary_tls_credentials_/3).
	:- dynamic(listener_temporary_tls_credentials_/3).
	:- mode(listener_temporary_tls_credentials_(?positive_integer, ?atom, ?atom), zero_or_one).
	:- info(listener_temporary_tls_credentials_/3, [
		comment is 'Tracked temporary TLS credential files owned by open listeners.',
		argnames is ['ListenerId', 'CertificateFile', 'KeyFile']
	]).

	:- private(listener_handle_alias_/2).
	:- dynamic(listener_handle_alias_/2).
	:- mode(listener_handle_alias_(?term, ?positive_integer), zero_or_more).
	:- info(listener_handle_alias_/2, [
		comment is 'Maps public listener handles to registered process listener identifiers.',
		argnames is ['Listener', 'ListenerId']
	]).

	:- private(listener_event_/2).
	:- dynamic(listener_event_/2).
	:- mode(listener_event_(?positive_integer, ?compound), zero_or_more).
	:- info(listener_event_/2, [
		comment is 'Queued listener events parsed from the listener helper diagnostics stream.',
		argnames is ['ListenerId', 'Event']
	]).

	:- private(process_listener_shutdown_requested_/1).
	:- dynamic(process_listener_shutdown_requested_/1).
	:- mode(process_listener_shutdown_requested_(?positive_integer), zero_or_one).
	:- info(process_listener_shutdown_requested_/1, [
		comment is 'Flag recording that a synthetic listener shutdown wakeup is pending.',
		argnames is ['ListenerId']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- private(listener_shutdown_seed_/1).
		:- dynamic(listener_shutdown_seed_/1).
		:- mode(listener_shutdown_seed_(?positive_integer), zero_or_one).
		:- info(listener_shutdown_seed_/1, [
			comment is 'Last allocated shutdown-run identifier.',
			argnames is ['RunId']
		]).

		:- private(listener_shutdown_control_/3).
		:- dynamic(listener_shutdown_control_/3).
		:- mode(listener_shutdown_control_(?nonvar, ?compound, ?positive_integer), zero_or_more).
		:- info(listener_shutdown_control_/3, [
			comment is 'Maps public shutdown control terms to listeners and active serving runs.',
			argnames is ['Control', 'Listener', 'RunId']
		]).

		:- private(listener_shutdown_requested_/2).
		:- dynamic(listener_shutdown_requested_/2).
		:- mode(listener_shutdown_requested_(?nonvar, ?positive_integer), zero_or_one).
		:- info(listener_shutdown_requested_/2, [
			comment is 'Recorded shutdown requests for serve_until_shutdown/4 runs.',
			argnames is ['Control', 'RunId']
		]).

		:- private(listener_active_worker_/3).
		:- dynamic(listener_active_worker_/3).
		:- mode(listener_active_worker_(?nonvar, ?positive_integer, ?compound), zero_or_more).
		:- info(listener_active_worker_/3, [
			comment is 'Tracked active connection workers for a serve_until_shutdown/4 run.',
			argnames is ['Control', 'RunId', 'Worker']
		]).

	:- endif.

	:- synchronized([
		register_process_connection/3, take_process_connection/2, allocate_connection_pool_id/1,
		register_connection_pool/7, acquire_pool_connection/2, release_pool_connection/3,
		discard_pool_connection/2, close_connection_pool_state/2, connection_pool_id_outcome/2,
		allocate_listener_id/1, register_process_listener/5, request_process_listener_shutdown/2,
		clear_process_listener_shutdown_request/1, register_listener_temporary_tls_credentials/2,
		take_listener_temporary_tls_credentials/2, take_process_listener/2, register_listener_event/2,
		take_listener_event/2
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- synchronized([
			register_shutdown_control/3,
			cleanup_shutdown_control/2,
			force_shutdown_control/2,
			register_active_worker/3,
			unregister_active_worker/3
		]).
	:- endif.

	:- uses(list, [
		append/3, length/2, member/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(logtalk, [
		print_message/3
	]).

	:- uses(os, [
		delete_file/1, file_exists/1, operating_system_type/1, path_concat/3, pid/1, resolve_command_path/2,
		temporary_directory/1, shell/1
	]).

	:- uses(reader, [
		line_to_codes/2 as reader_line_to_codes/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3, setup_call_cleanup/3
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.

		:- meta_predicate(spawn_connection_worker(*, *, *, *)).
		:- meta_predicate(spawn_notifying_connection_worker(*, *, *, *, *)).
		:- meta_predicate(collect_finished_connection_worker(*, 0, *, *)).
		:- meta_predicate(collect_finished_worker(*, 0, *, *, *, *)).
		:- meta_predicate(wait_for_workers(*, *)).
	:- endif.

	:- initialization(verify_commands_availability).

	supported_request_scheme(http).
	supported_request_scheme(https).

	supported_websocket_scheme(ws).
	supported_websocket_scheme(wss).

	open_listener(Host, Port, Listener, Options) :-
		context(Context),
		validate_listener_port(Port, RequestedPort),
		normalize_listener_host(Host, NormalizedHost),
		parse_listener_socket_options(Options, Backlog, Transport, ListenerExecutable, TemporaryTLSCredentialsPrefix, CertificateFile0, KeyFile0),
		resolve_listener_tls_credentials(Transport, TemporaryTLSCredentialsPrefix, CertificateFile0, KeyFile0, CertificateFile, KeyFile, TemporaryTLSCredentials),
		resolve_listener_executable(ListenerExecutable, ResolvedHelperExecutable, ListenerExecutableKind),
		select_listener_port(ListenerExecutableKind, NormalizedHost, RequestedPort, SelectedPort),
		open_loopback_listener(binary, Backlog, RelayPort, RelayListener),
		atom_number(SelectedPortAtom, SelectedPort),
		atom_number(RelayPortAtom, RelayPort),
		listener_process_arguments(ListenerExecutableKind, ResolvedHelperExecutable, NormalizedHost, SelectedPortAtom, Backlog, Transport, CertificateFile, KeyFile, RelayPortAtom, Arguments),
		catch(
			process::create(ResolvedHelperExecutable, Arguments, [stderr(Error), process(Process)]),
			CreateError,
			(	close_server_socket(RelayListener),
				cleanup_temporary_tls_credentials(TemporaryTLSCredentials),
				throw(CreateError)
			)
		),
		catch(
			startup_listener_response(ListenerExecutableKind, Error, Context, BoundPort),
			StartupError,
			(	close_server_socket(RelayListener),
				cleanup_listener_process(Process, Error),
				cleanup_temporary_tls_credentials(TemporaryTLSCredentials),
				throw(StartupError)
			)
		),
		Port = BoundPort,
		allocate_listener_id(ListenerId),
		register_process_listener(ListenerId, ListenerExecutableKind, Process, relay(RelayListener, RelayPort), Error),
		register_listener_temporary_tls_credentials(ListenerId, TemporaryTLSCredentials),
		register_listener_handle_alias(RelayListener, ListenerId),
		Listener = RelayListener.

	close_listener(Listener) :-
		listener_id(Listener, ListenerId),
		terminate_process_listener_transport(ListenerId),
		finalize_process_listener(ListenerId, FinalizeOutcome),
		setup_call_cleanup(
			true,
			finalize_process_listener_outcome(FinalizeOutcome, Listener),
			finalize_listener_temporary_tls_credentials(ListenerId)
		).

	temporary_tls_credentials(Prefix, CertificateFile, KeyFile) :-
		validate_temporary_tls_credentials_prefix(Prefix),
		temporary_tls_credentials_files(Prefix, CertificateFile, KeyFile),
		(	file_exists(CertificateFile),
			file_exists(KeyFile) ->
			true
		;	cleanup_temporary_tls_credentials(credentials(CertificateFile, KeyFile)),
			resolve_command_path(openssl, Path),
			process::create(
				Path,
				['req', '-x509', '-newkey', 'rsa:2048', '-nodes', '-keyout', KeyFile, '-out', CertificateFile, '-subj', '/CN=127.0.0.1', '-days', '1'],
				[stdout(Output), stderr(Error), process(Process)]
			),
			process::wait(Process, Status),
			close_process_stream(Output),
			close_process_stream(Error),
			(	once((Status == 0; Status == exit(0))) ->
				true
			;	cleanup_temporary_tls_credentials(credentials(CertificateFile, KeyFile)),
				throw(error(resource_error(http_process_transport_temporary_tls_credentials), http_process_transport::temporary_tls_credentials(Prefix, _, _)))
			)
		).

	open_connection(Host, Port, Connection, Options) :-
		context(Context),
		parse_connection_options(Options, Type, Transport, Executable, ServerNameOption, OpensslArguments),
		normalize_connection_endpoint(Host, Port, NormalizedHost),
		build_connection_arguments(Transport, Host, Port, ServerNameOption, OpensslArguments, Arguments),
		resolve_command_path(Executable, ExecutablePath),
		open_process_connection(Transport, ExecutablePath, Arguments, Type, NormalizedHost, Port, Context, Connection).

	close_connection(http_websocket_connection(_ClientInfo, Input, Output)) :-
		!,
		connection_streams(http_websocket_connection(_ClientInfo, Input, Output), AdoptedInput, AdoptedOutput),
		close_stream_pair(AdoptedInput, AdoptedOutput).
	close_connection(Connection) :-
		take_process_connection(Connection, Outcome),
		close_process_connection_outcome(Outcome, Connection).

	connection_streams(Connection, Input, Output) :-
		nonvar(Connection),
		process_connection_state_(Connection, _Process, _Error),
		Connection = http_connection(_Host, _Port, Input, Output),
		adopt_connection_streams(Input, Output),
		!.
	connection_streams(http_websocket_connection(_ClientInfo, Input, Output), Input, Output) :-
		adopt_connection_streams(Input, Output),
		!.
	connection_streams(Connection, _Input, _Output) :-
		(	var(Connection) ->
			instantiation_error
		;	domain_error(http_socket_transport_connection, Connection)
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

	exchange_sequence(http_connection_pool(Host, Port, PoolId), Requests, Responses) :-
		!,
		pool_exchange_sequence(PoolId, http_connection_pool(Host, Port, PoolId), Requests, Responses).
	exchange_sequence(Connection, Requests, Responses) :-
		connection_streams(Connection, Input, Output),
		http_client_core::exchange_sequence(Input, Output, Requests, Responses).

	exchange(Host, Port, Request, Response) :-
		exchange(Host, Port, Request, Response, []).

	exchange(Host, Port, Request, Response, Options) :-
		one_shot_request(Request, OneShotRequest),
		setup_call_cleanup(
			open_connection(Host, Port, Connection, [type(binary)| Options]),
			exchange(Connection, OneShotRequest, Response),
			close_connection(Connection)
		).

	exchange_sequence(_Host, _Port, [], []) :-
		!.
	exchange_sequence(Host, Port, Requests0, Responses) :-
		setup_call_cleanup(
			open_connection(Host, Port, Connection, [type(binary)]),
			(	one_shot_request_sequence(Requests0, Requests),
				exchange_sequence(Connection, Requests, Responses)
			),
			close_connection(Connection)
		).

	serve_once(Listener, Handler, ClientInfo) :-
		accept_process_listener_connection(Listener, Input, Output, ClientInfo),
		serve_accepted_connection(Input, Output, Handler).

	serve_websocket_once(Listener, Handler, Connection, Response, ClientInfo) :-
		accept_process_listener_connection(Listener, Input, Output, ClientInfo),
		catch(
			serve_accepted_websocket(Input, Output, Handler, Connection, Response, ClientInfo),
			Error,
			(	catch(close_stream_pair(Input, Output), _, true),
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
		serve_until_shutdown(Listener, Handler, Control, Options, true).

	request_shutdown(Control) :-
		request_shutdown_impl(Control).

	serve_listener_serial(_Listener, _Handler, 0, []) :-
		!.
	serve_listener_serial(Listener, Handler, Count, ClientInfos) :-
		try_accept_bounded_listener_connection(Listener, Input, Output, ClientInfo, Outcome),
		(	Outcome == shutdown ->
			ClientInfos = []
		;	serve_accepted_connection(Input, Output, Handler),
			NextCount is Count - 1,
			ClientInfos = [ClientInfo| RemainingClientInfos],
			serve_listener_serial(Listener, Handler, NextCount, RemainingClientInfos)
		).

	serve_accepted_connection(Input, Output, Handler) :-
		setup_call_cleanup(
			adopt_connection_streams(Input, Output),
			http_server_core::serve_connection(Input, Output, Handler),
			close_stream_pair(Input, Output)
		).

	serve_accepted_websocket(Input, Output, Handler, Connection, Response, ClientInfo) :-
		adopt_connection_streams(Input, Output),
		http_server_core::serve_websocket(Input, Output, Handler, Outcome),
		serve_websocket_outcome(Outcome, Input, Output, Connection, Response, ClientInfo).

	try_accept_bounded_listener_connection(Listener, Input, Output, ClientInfo, Outcome) :-
		catch(
			(	accept_process_listener_connection(Listener, Input, Output, ClientInfo),
				Outcome = accepted
			),
			listener_shutdown(Listener),
			Outcome = shutdown
		).

	serve_websocket_outcome(accepted(_Request, Response), Input, Output, http_websocket_connection(ClientInfo, Input, Output), Response, ClientInfo).
	serve_websocket_outcome(rejected(Response), _Input, _Output, _Connection, _RejectedResponse, _ClientInfo) :-
		domain_error(http_socket_transport_websocket_response, Response).
	serve_websocket_outcome(end_of_file, _Input, _Output, _Connection, _Response, _ClientInfo) :-
		existence_error(http_socket_transport_websocket_request, end_of_file).

	open_process_connection(Transport, Executable, Arguments, Type, NormalizedHost, Port, Context, Connection) :-
		% the type/1 option is only effective for the SICStus Prolog and SWI-Prolog backends
		process::create(Executable, Arguments, [stdin(Output), stdout(Input), stderr(Error), process(Process), type(Type)]),
		Connection = http_connection(NormalizedHost, Port, Input, Output),
		catch(
			% for other backends, we need to set the streams type after creating the process
			setup_process_connection_streams(Type, Input, Output),
			SetupError,
			(	cleanup_process_connection(Connection, Process, Error),
				throw(SetupError)
			)
		),
		catch(
			wait_for_connection_startup(Transport, Error, Context),
			StartupError,
			(	cleanup_process_connection(Connection, Process, Error),
				throw(StartupError)
			)
		),
		register_process_connection(Connection, Process, Error).

	wait_for_connection_startup(tcp, Error, Context) :-
		wait_for_ncat_connection_startup(Error, Context).
	wait_for_connection_startup(tls, _Error, _Context).

	wait_for_ncat_connection_startup(Error, Context) :-
		line_to_codes(Error, Codes),
		codes_line_atom(Codes, Line),
		(	Line == end_of_file ->
			throw(error(resource_error(http_process_transport_connection), Context))
		;	ncat_connection_ready_line(Line) ->
			true
		;	ncat_connection_ignorable_line(Line) ->
			wait_for_ncat_connection_startup(Error, Context)
		;	throw(error(socket_error(Line), Context))
		).

	ncat_connection_ready_line(Line) :-
		atom(Line),
		atom_length('Ncat: Connected to ', PrefixLength),
		sub_atom(Line, 0, PrefixLength, _, 'Ncat: Connected to ').

	ncat_connection_ignorable_line(Line) :-
		atom(Line),
		atom_length('Ncat: Version ', PrefixLength),
		sub_atom(Line, 0, PrefixLength, _, 'Ncat: Version ').

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	setup_process_connection_streams(binary, Input, Output) :-
		{set_stream_property(Input, encoding, octet)},
		{set_stream_property(Output, encoding, octet)}.
	setup_process_connection_streams(text, _Input, _Output).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

	setup_process_connection_streams(Type, Input, Output) :-
		{set_stream_type(Input, Type)},
		{set_stream_type(Output, Type)}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	setup_process_connection_streams(_Type, _Input, _Output).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

	setup_process_connection_streams(Type, Input, Output) :-
		{set_stream(Input, type(Type))},
		{set_stream(Output, type(Type))}.

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

	setup_process_connection_streams(Type, Input, Output) :-
		{set_stream(Input, type(Type))},
		{set_stream(Output, type(Type))}.

	:- elif(current_logtalk_flag(prolog_dialect, xvm)).

	setup_process_connection_streams(Type, Input, Output) :-
		{set_stream_type(Input, Type)},
		{set_stream_type(Output, Type)}.

	:- endif.

	register_process_connection(Connection, Process, Error) :-
		assertz(process_connection_state_(Connection, Process, Error)).

	take_process_connection(Connection, connection(Process, Error)) :-
		retract(process_connection_state_(Connection, Process, Error)),
		!.
	take_process_connection(_Connection, missing).

	close_process_connection_outcome(connection(Process, Error), Connection) :-
		cleanup_process_connection(Connection, Process, Error).
	close_process_connection_outcome(missing, Connection) :-
		(	var(Connection) ->
			instantiation_error
		;	domain_error(http_socket_transport_connection, Connection)
		).

	cleanup_process_connection(http_connection(_Host, _Port, Input, Output), Process, Error) :-
		close_process_stream(Output),
		close_process_stream(Input),
		close_process_stream(Error),
		catch(process::kill(Process, sigterm), _, true),
		catch(process::wait(Process, _Status), _, true).

	cleanup_listener_process(Process, Error) :-
		close_process_stream(Error),
		catch(process::kill(Process, sigterm), _, true),
		catch(process::wait(Process, _Status), _, true).

	close_process_stream(Stream) :-
		catch(close(Stream), _, true).

	open_loopback_listener(Type, Backlog, Port, Listener) :-
		local_loopback_host(Host),
		socket::server_open(Host, Port, Listener, [backlog(Backlog), type(Type)]).

	close_server_socket(ServerSocket) :-
		catch(socket::server_close(ServerSocket), _, true).

	local_loopback_host('127.0.0.1').

	validate_listener_port(Port, RequestedPort) :-
		(	var(Port) ->
			RequestedPort = 0
		;	integer(Port),
			Port >= 0 ->
			RequestedPort = Port
		;	domain_error(non_negative_integer, Port)
		).

	normalize_listener_host(Host, NormalizedHost) :-
		(	atom(Host) ->
			atom_codes(Host, HostCodes),
			lowercase_ascii_codes(HostCodes, NormalizedHostCodes),
			atom_codes(NormalizedHost, NormalizedHostCodes)
		;	type_error(atom, Host)
		).

	resolve_listener_executable(Executable, ResolvedExecutable, ListenerExecutableKind) :-
		resolve_command_path(Executable, ResolvedExecutable),
		listener_executable_kind(ResolvedExecutable, ListenerExecutableKind),
		!.
	resolve_listener_executable(Executable, _ResolvedExecutable, _ListenerExecutableKind) :-
		domain_error(http_process_transport_listener_helper_executable, Executable).

	listener_executable_kind(Executable, ncat) :-
		sub_atom(Executable, _, _, _, 'ncat'),
		!.
	listener_executable_kind(Executable, socat) :-
		sub_atom(Executable, _, _, _, 'socat'),
		!.

	select_listener_port(ncat, Host, 0, SelectedPort) :-
		reserve_listener_port(Host, SelectedPort),
		!.
	select_listener_port(_ListenerExecutableKind, _Host, RequestedPort, RequestedPort).

	reserve_listener_port(Host, Port) :-
		socket::server_open(Host, Port, ProbeListener, [backlog(1), type(binary)]),
		close_server_socket(ProbeListener).

	listener_process_arguments(socat, _ResolvedExecutable, Host, Port, Backlog, Transport, CertificateFile, KeyFile, RelayPort, ['-d', '-d', ListenAddress, RelayAddress]) :-
		listener_listen_address(socat, Transport, Host, Port, Backlog, CertificateFile, KeyFile, ListenAddress),
		listener_relay_address(socat, RelayPort, RelayAddress).
	listener_process_arguments(ncat, _ResolvedExecutable, Host, Port, _Backlog, Transport, CertificateFile, KeyFile, RelayPort, Arguments) :-
		listener_listen_address(ncat, Transport, Host, Port, _Backlog, CertificateFile, KeyFile, ListenArguments),
		listener_relay_address(ncat, RelayPort, RelayCommand),
		append(ListenArguments, ['--sh-exec', RelayCommand], Arguments).

	listener_listen_address(socat, tcp, Host, Port, Backlog, _CertificateFile, _KeyFile, ListenAddress) :-
		!,
		listener_bind_family_suffix(Host, FamilySuffix),
		atomic_list_concat(['TCP-LISTEN:', Port, ',bind=', Host, FamilySuffix, ',reuseaddr,fork,backlog=', Backlog], ListenAddress).
	listener_listen_address(socat, tls, Host, Port, Backlog, CertificateFile, KeyFile, ListenAddress) :-
		!,
		listener_bind_family_suffix(Host, FamilySuffix),
		atomic_list_concat(['OPENSSL-LISTEN:', Port, ',bind=', Host, FamilySuffix, ',reuseaddr,fork,backlog=', Backlog, ',cert=', CertificateFile, ',key=', KeyFile, ',verify=0'], ListenAddress).
	listener_listen_address(ncat, tcp, Host, Port, _Backlog, _CertificateFile, _KeyFile, ['-v', '-k', '-l', FamilyOption, Host, Port]) :-
		!,
		listener_bind_family_option(Host, FamilyOption).
	listener_listen_address(ncat, tls, Host, Port, _Backlog, CertificateFile, KeyFile, ['-v', '-k', '-l', FamilyOption, Host, Port, '--ssl', '--ssl-cert', CertificateFile, '--ssl-key', KeyFile]) :-
		listener_bind_family_option(Host, FamilyOption).

	listener_bind_family_suffix(Host, ',pf=ip6') :-
		sub_atom(Host, _, _, _, ':'),
		!.
	listener_bind_family_suffix(_Host, '').

	listener_bind_family_option(Host, '-6') :-
		sub_atom(Host, _, _, _, ':'),
		!.
	listener_bind_family_option(_Host, '-4').

	listener_relay_address(socat, RelayPort, RelayAddress) :-
		local_loopback_host(LoopbackHost),
		atomic_list_concat(['TCP:', LoopbackHost, ':', RelayPort], RelayAddress).
	listener_relay_address(ncat, RelayPort, RelayCommand) :-
		local_loopback_host(LoopbackHost),
		listener_bind_family_option(LoopbackHost, FamilyOption),
		atomic_list_concat([ncat, '-n', FamilyOption, LoopbackHost, RelayPort], ' ', RelayCommand).

	client_connect_arguments(Host, Port, ['-v', '-n', FamilyOption, Host, PortAtom]) :-
		atom_number(PortAtom, Port),
		listener_bind_family_option(Host, FamilyOption).

	startup_listener_response(ListenerExecutableKind, Error, Context, BoundPort) :-
		line_to_codes(Error, Codes),
		codes_line_atom(Codes, Line),
		startup_listener_outcome(ListenerExecutableKind, Line, Context, BoundPort, Outcome),
		startup_listener_response_outcome(Outcome, ListenerExecutableKind, Error, Context, BoundPort).

	startup_listener_response_outcome(ready(BoundPort), _ListenerExecutableKind, _Error, _Context, BoundPort).
	startup_listener_response_outcome(continue, ListenerExecutableKind, Error, Context, BoundPort) :-
		startup_listener_response(ListenerExecutableKind, Error, Context, BoundPort).
	startup_listener_response_outcome(error(ListenerError), _ListenerExecutableKind, _Error, _Context, _BoundPort) :-
		throw(ListenerError).

	startup_listener_outcome(socat, Line, _Context, BoundPort, ready(BoundPort)) :-
		atom(Line),
		sub_atom(Line, _, _, _, 'listening on'),
		startup_listener_port(Line, BoundPort),
		!.
	startup_listener_outcome(ncat, Line, _Context, BoundPort, ready(BoundPort)) :-
		atom(Line),
		sub_atom(Line, _, _, _, 'Listening on '),
		startup_listener_port(Line, BoundPort),
		!.
	startup_listener_outcome(ncat, Line, _Context, _BoundPort, continue) :-
		atom(Line),
		sub_atom(Line, _, _, _, 'Version '),
		!.
	startup_listener_outcome(_ListenerExecutableKind, end_of_file, Context, _BoundPort, error(error(resource_error(http_process_transport_listener), Context))).
	startup_listener_outcome(_ListenerExecutableKind, Line, Context, _BoundPort, error(error(socket_error(Line), Context))).

	startup_listener_port(Line, BoundPort) :-
		atom_codes(Line, Codes),
		reverse(Codes, ReversedCodes),
		reversed_port_codes(ReversedCodes, [], PortCodes),
		number_codes(BoundPort, PortCodes).

	reversed_port_codes([Code| Codes], PortCodes0, PortCodes) :-
		decimal_digit_code(Code),
		!,
		reversed_port_codes(Codes, [Code| PortCodes0], PortCodes).
	reversed_port_codes([0':| _Codes], PortCodes, PortCodes) :-
		PortCodes \== [],
		!.
	reversed_port_codes([_Code| Codes], [], PortCodes) :-
		reversed_port_codes(Codes, [], PortCodes).

	decimal_digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	codes_line_atom(end_of_file, end_of_file) :-
		!.
	codes_line_atom(Codes, Line) :-
		atom_codes(Line, Codes).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).

	adopt_listener_process_stream(Stream) :-
		{adopt_stream(Stream)}.

	:- else.

	adopt_listener_process_stream(_Stream).

	:- endif.

	listener_process_error_event(_ListenerExecutableKind, end_of_file, end_of_file).
	listener_process_error_event(socat, Line, accept(PeerAddress)) :-
		listener_accept_line_peer_address(Line, PeerAddress).
	listener_process_error_event(ncat, Line, accept(PeerAddress)) :-
		ncat_accept_line_peer_address(Line, PeerAddress).

	ncat_accept_line_peer_address(Line, PeerAddress) :-
		atom(Line),
		AcceptLine = 'Ncat: Connection from ',
		atom_length(AcceptLine, AcceptLineLength),
		sub_atom(Line, 0, AcceptLineLength, _, AcceptLine),
		sub_atom(Line, AcceptLineLength, _, 0, EndpointWithSuffix),
		strip_trailing_period(EndpointWithSuffix, EndpointDescription),
		listener_endpoint_description_peer_address(EndpointDescription, PeerAddress).

	strip_trailing_period(Atom, StrippedAtom) :-
		atom_concat(StrippedAtom, '.', Atom),
		!.
	strip_trailing_period(Atom, Atom).

	accept_process_listener_connection(Listener, Input, Output, ClientInfo) :-
		listener_id(Listener, ListenerId),
		( 	listener_relay_socket(ListenerId, RelayListener) ->
			socket::server_accept(RelayListener, Input, Output, _RelayClientInfo, [type(binary)]),
			accept_listener_outcome(ListenerId, Listener, Input, Output, ClientInfo)
		;	existence_error(http_socket_transport_listener, Listener)
		),
		!.

	accept_listener_outcome(ListenerId, Listener, Input, Output, ClientInfo) :-
		( 	process_listener_shutdown_requested_(ListenerId) ->
			clear_process_listener_shutdown_request(ListenerId),
			clear_process_listener_shutdown_event(ListenerId),
			close_stream_pair(Input, Output),
			throw(listener_shutdown(Listener))
		;	accept_listener_metadata(ListenerId, Listener, Input, Output, ClientInfo)
		).

	accept_listener_metadata(ListenerId, Listener, Input, Output, ClientInfo) :-
		take_listener_event(ListenerId, Event),
		accept_listener_event(Event, ListenerId, Listener, Input, Output, ClientInfo).

	accept_listener_event(end_of_file, _ListenerId, Listener, Input, Output, _ClientInfo) :-
		close_stream_pair(Input, Output),
		existence_error(http_socket_transport_listener, Listener).
	accept_listener_event(accept(PeerAddress), _ListenerId, _Listener, _Input, _Output, client(PeerAddress)) :-
		!.
	accept_listener_event(shutdown, ListenerId, Listener, Input, Output, _ClientInfo) :-
		clear_process_listener_shutdown_request(ListenerId),
		close_stream_pair(Input, Output),
		throw(listener_shutdown(Listener)).

	listener_accept_line_peer_address(Line, PeerAddress) :-
		atom(Line),
		AcceptLine = ' accepting connection from ',
		atom_length(AcceptLine, AcceptLineLength),
		sub_atom(Line, Before, _, _, AcceptLine),
		Start is Before + AcceptLineLength,
		sub_atom(Line, Start, _, 0, Suffix),
		sub_atom(Suffix, EndpointLength, _, _, ' on '),
		sub_atom(Suffix, 0, EndpointLength, _, EndpointDescription),
		listener_endpoint_description_peer_address(EndpointDescription, PeerAddress).

	listener_endpoint_description_peer_address(EndpointDescription, PeerAddress) :-
		atom_codes(EndpointDescription, EndpointCodes),
		reverse(EndpointCodes, ReversedEndpointCodes),
		reversed_prefix_until_space(ReversedEndpointCodes, ReversedTokenCodes),
		reverse(ReversedTokenCodes, TokenCodes),
		listener_endpoint_token_peer_address_codes(TokenCodes, PeerAddressCodes),
		atom_codes(PeerAddress, PeerAddressCodes),
		PeerAddress \== ''.

	reversed_prefix_until_space([], []).
	reversed_prefix_until_space([0' | _Codes], []) :-
		!.
	reversed_prefix_until_space([Code| Codes], [Code| PrefixCodes]) :-
		reversed_prefix_until_space(Codes, PrefixCodes).

	listener_endpoint_token_peer_address_codes(TokenCodes, PeerAddressCodes) :-
		reverse(TokenCodes, ReversedTokenCodes),
		listener_reversed_peer_address_codes(ReversedTokenCodes, ReversedPeerAddressCodes),
		reverse(ReversedPeerAddressCodes, PeerAddressCodes0),
		strip_bracket_codes(PeerAddressCodes0, PeerAddressCodes).

	listener_reversed_peer_address_codes([Code| Codes], PeerAddressCodes) :-
		decimal_digit_code(Code),
		!,
		listener_reversed_peer_address_codes(Codes, PeerAddressCodes).
	listener_reversed_peer_address_codes([0':| Codes], Codes) :-
		!.
	listener_reversed_peer_address_codes(Codes, Codes).

	strip_bracket_codes([0'[| Codes0], Codes) :-
		append(Codes, [0']], Codes0),
		!.
	strip_bracket_codes(Codes, Codes).

	listener_id(http_process_listener(_Transport, _Host, _Port, ListenerId), ListenerId) :-
		!.
	listener_id(Listener, ListenerId) :-
		listener_handle_alias_(Listener, ListenerId),
		!.
	listener_id(Listener, _ListenerId) :-
		(	var(Listener) ->
			instantiation_error
		;	domain_error(http_socket_transport_listener, Listener)
		).

	listener_executable(ListenerId, ListenerExecutableKind) :-
		process_listener_state_(ListenerId, ListenerExecutableKind, _Process, _RelayListener, _Error),
		!.
	listener_executable(_ListenerId, _ListenerExecutableKind) :-
		fail.

	listener_relay_socket(ListenerId, RelayListener) :-
		process_listener_state_(ListenerId, _ListenerExecutableKind, _Process, relay(RelayListener, _RelayPort), _Error),
		!.
	listener_relay_socket(_ListenerId, _RelayListener) :-
		fail.

	listener_relay_port(ListenerId, RelayPort) :-
		process_listener_state_(ListenerId, _ListenerExecutableKind, _Process, relay(_RelayListener, RelayPort), _Error),
		!.
	listener_relay_port(_ListenerId, _RelayPort) :-
		fail.

	listener_error_stream(ListenerId, Error) :-
		process_listener_state_(ListenerId, _ListenerExecutableKind, _Process, _RelayListener, Error),
		!.
	listener_error_stream(_ListenerId, _Error) :-
		fail.

	listener_process_event(ListenerExecutableKind, Error, Event) :-
		line_to_codes(Error, Codes),
		codes_line_atom(Codes, Line),
		( 	listener_process_error_event(ListenerExecutableKind, Line, Event) ->
			true
		; 	listener_process_event(ListenerExecutableKind, Error, Event)
		).

	allocate_listener_id(ListenerId) :-
		(	retract(listener_seed_(CurrentListenerId)) ->
			ListenerId is CurrentListenerId + 1
		;	ListenerId = 1
		),
		assertz(listener_seed_(ListenerId)).

	register_process_listener(ListenerId, ListenerExecutableKind, Process, RelayListener, Error) :-
		assertz(process_listener_state_(ListenerId, ListenerExecutableKind, Process, RelayListener, Error)).

	register_listener_temporary_tls_credentials(_ListenerId, none) :-
		!.
	register_listener_temporary_tls_credentials(ListenerId, credentials(CertificateFile, KeyFile)) :-
		assertz(listener_temporary_tls_credentials_(ListenerId, CertificateFile, KeyFile)).

	register_listener_handle_alias(Listener, ListenerId) :-
		assertz(listener_handle_alias_(Listener, ListenerId)).

	:- if(current_logtalk_flag(threads, supported)).

	register_listener_event(ListenerId, Event) :-
		assertz(listener_event_(ListenerId, Event)),
		threaded_notify(listener_event(ListenerId)).

	take_listener_event(ListenerId, Event) :-
		( 	retract(listener_event_(ListenerId, Event)) ->
			true
		; 	listener_executable(ListenerId, ListenerExecutableKind),
			listener_error_stream(ListenerId, Error),
			adopt_listener_process_stream(Error),
			listener_process_event(ListenerExecutableKind, Error, Event)
		).

	:- else.

	register_listener_event(ListenerId, Event) :-
		assertz(listener_event_(ListenerId, Event)).

	take_listener_event(ListenerId, Event) :-
		( 	retract(listener_event_(ListenerId, Event)) ->
			true
		; 	listener_executable(ListenerId, ListenerExecutableKind),
			listener_error_stream(ListenerId, Error),
			adopt_listener_process_stream(Error),
			listener_process_event(ListenerExecutableKind, Error, Event)
		).

	:- endif.

	request_process_listener_shutdown(ListenerId, requested) :-
		process_listener_state_(ListenerId, _ListenerExecutableKind, _Process, _RelayListener, _Error),
		\+ process_listener_shutdown_requested_(ListenerId),
		assertz(process_listener_shutdown_requested_(ListenerId)),
		!.
	request_process_listener_shutdown(ListenerId, already_requested) :-
		process_listener_state_(ListenerId, _ListenerExecutableKind, _Process, _RelayListener, _Error),
		process_listener_shutdown_requested_(ListenerId),
		!.
	request_process_listener_shutdown(_ListenerId, missing).

	clear_process_listener_shutdown_request(ListenerId) :-
		retractall(process_listener_shutdown_requested_(ListenerId)).

	clear_process_listener_shutdown_event(ListenerId) :-
		retractall(listener_event_(ListenerId, shutdown)).

	request_process_listener_shutdown_outcome(requested, ListenerId, _Listener) :-
		catch(signal_process_listener_accept_shutdown(ListenerId), _, true).
	request_process_listener_shutdown_outcome(already_requested, _ListenerId, _Listener).
	request_process_listener_shutdown_outcome(missing, _ListenerId, Listener) :-
		existence_error(http_socket_transport_listener, Listener).

	signal_process_listener_accept_shutdown(ListenerId) :-
		register_listener_event(ListenerId, shutdown),
		( 	listener_relay_port(ListenerId, RelayPort) ->
			catch(signal_process_listener_shutdown(RelayPort), _, true)
		;	true
		).

	terminate_process_listener_transport(ListenerId) :-
		process_listener_state_(ListenerId, _ListenerExecutableKind, Process, _RelayListener, _Error),
		catch(process::kill(Process, sigterm), _, true).

	signal_process_listener_shutdown(RelayPort) :-
		local_loopback_host(LoopbackHost),
		socket::client_open(LoopbackHost, RelayPort, Input, Output, [type(binary)]),
		close_stream_pair(Input, Output).

	take_process_listener(ListenerId, listener(ListenerExecutableKind, Process, RelayListener, Error)) :-
		retract(process_listener_state_(ListenerId, ListenerExecutableKind, Process, relay(RelayListener, _RelayPort), Error)),
		retractall(listener_handle_alias_(_Listener, ListenerId)),
		retractall(process_listener_shutdown_requested_(ListenerId)),
		retractall(listener_event_(ListenerId, _)),
		!.
	take_process_listener(_ListenerId, missing).

	take_listener_temporary_tls_credentials(ListenerId, credentials(CertificateFile, KeyFile)) :-
		retract(listener_temporary_tls_credentials_(ListenerId, CertificateFile, KeyFile)),
		!.
	take_listener_temporary_tls_credentials(_ListenerId, none).

	finalize_process_listener(ListenerId, Outcome) :-
		take_process_listener(ListenerId, Outcome).

	finalize_process_listener_outcome(listener(_ListenerExecutableKind, Process, RelayListener, Error), _Listener) :-
		close_server_socket(RelayListener),
		close_process_stream(Error),
		catch(process::wait(Process, _Status), _, true).
	finalize_process_listener_outcome(missing, Listener) :-
		existence_error(http_socket_transport_listener, Listener).

	request_listener_shutdown(Listener) :-
		(	listener_shutdown_id(Listener, ListenerId) ->
			request_process_listener_shutdown(ListenerId, Outcome),
			request_process_listener_shutdown_outcome(Outcome, ListenerId, Listener)
		;	true
		).

	listener_shutdown_id(http_process_listener(_Transport, _Host, _Port, ListenerId), ListenerId) :-
		!.
	listener_shutdown_id(Listener, ListenerId) :-
		listener_handle_alias_(Listener, ListenerId).

	finalize_shutdown_listener(Listener) :-
		listener_id(Listener, ListenerId),
		terminate_process_listener_transport(ListenerId),
		finalize_process_listener(ListenerId, Outcome),
		setup_call_cleanup(
			true,
			(	Outcome == missing ->
				true
			;	finalize_process_listener_outcome(Outcome, Listener)
			),
			finalize_listener_temporary_tls_credentials(ListenerId)
		).

	parse_connection_options(Options, Type, Transport, Executable, ServerName, OpensslArguments) :-
		^^check_options(Options),
		check_connection_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(type(Type), MergedOptions),
		resolve_connection_transport(Options, Transport),
		resolve_connection_executable(Transport, Executable, Options, MergedOptions),
		^^option(server_name(ServerName), MergedOptions),
		^^option(openssl_arguments(OpensslArguments), MergedOptions).

	resolve_connection_transport(Options, Transport) :-
		(	member(connection_transport(Transport0), Options) ->
			Transport = Transport0
		;	Transport = tcp
		).

	resolve_connection_executable(tcp, ncat, _Options, _MergedOptions).
	resolve_connection_executable(tls, Executable, Options, MergedOptions) :-
		(	member(openssl_executable(Executable0), Options) ->
			Executable = Executable0
		;	^^option(openssl_executable(Executable), MergedOptions)
		).

	build_connection_arguments(tcp, Host, Port, _ServerNameOption, _OpensslArguments, Arguments) :-
		client_connect_arguments(Host, Port, Arguments).
	build_connection_arguments(tls, Host, Port, ServerNameOption, OpensslArguments, Arguments) :-
		build_openssl_arguments(Host, Port, ServerNameOption, OpensslArguments, Arguments).

	build_openssl_arguments(Host, Port, ServerNameOption, OpensslArguments, Arguments) :-
		resolve_server_name(ServerNameOption, Host, ServerName),
		openssl_connect_argument(Host, Port, ConnectArgument),
		BaseArguments0 = ['s_client', '-quiet', '-no_ign_eof', '-connect', ConnectArgument],
		add_server_name_argument(ServerName, BaseArguments0, BaseArguments),
		append(BaseArguments, OpensslArguments, Arguments).

	parse_listener_socket_options(Options, Backlog, Transport, ListenerExecutable, TemporaryTLSCredentialsPrefix, CertificateFile, KeyFile) :-
		^^check_options(Options),
		check_listener_socket_options(Options),
		^^merge_options(Options, MergedOptions),
		^^option(backlog(Backlog), MergedOptions),
		^^option(listener_transport(Transport), MergedOptions),
		^^option(listener_helper_executable(ListenerExecutable), MergedOptions),
		^^option(temporary_tls_credentials(TemporaryTLSCredentialsPrefix), MergedOptions, temporary_tls_credentials(none)),
		^^option(tls_certificate_file(CertificateFile), MergedOptions, tls_certificate_file(none)),
		^^option(tls_key_file(KeyFile), MergedOptions, tls_key_file(none)),
		validate_listener_transport_options(Transport, TemporaryTLSCredentialsPrefix, CertificateFile, KeyFile).

	validate_listener_transport_options(tcp, TemporaryTLSCredentialsPrefix, _CertificateFile, _KeyFile) :-
		(	TemporaryTLSCredentialsPrefix == none ->
			true
		;	domain_error(http_process_transport_listener_tcp_options, [listener_transport(tcp), temporary_tls_credentials(TemporaryTLSCredentialsPrefix)])
		).
	validate_listener_transport_options(tls, TemporaryTLSCredentialsPrefix, CertificateFile, KeyFile) :-
		(	TemporaryTLSCredentialsPrefix == none ->
			(	(CertificateFile == none ; KeyFile == none) ->
				domain_error(http_process_transport_listener_tls_options, [tls_certificate_file(CertificateFile), tls_key_file(KeyFile)])
			;	true
			)
		;	(	CertificateFile == none,
				KeyFile == none ->
				true
			;	domain_error(http_process_transport_listener_tls_options, [temporary_tls_credentials(TemporaryTLSCredentialsPrefix), tls_certificate_file(CertificateFile), tls_key_file(KeyFile)])
			)
		).

	resolve_listener_tls_credentials(tcp, _TemporaryTLSCredentialsPrefix, CertificateFile, KeyFile, CertificateFile, KeyFile, none).
	resolve_listener_tls_credentials(tls, none, CertificateFile, KeyFile, CertificateFile, KeyFile, none) :-
		!.
	resolve_listener_tls_credentials(tls, TemporaryTLSCredentialsPrefix, _CertificateFile, _KeyFile, CertificateFile, KeyFile, credentials(CertificateFile, KeyFile)) :-
		temporary_tls_credentials(TemporaryTLSCredentialsPrefix, CertificateFile, KeyFile).

	resolve_server_name(default, Host, ServerName) :-
		(	sub_atom(Host, _, _, _, ':') ->
			ServerName = none
		;	ServerName = Host
		),
		!.
	resolve_server_name(ServerName, _Host, ServerName).

	add_server_name_argument(none, Arguments, Arguments) :-
		!.
	add_server_name_argument(ServerName, Arguments0, Arguments) :-
		append(Arguments0, ['-servername', ServerName], Arguments).

	openssl_connect_argument(Host, Port, ConnectArgument) :-
		(	sub_atom(Host, _, _, _, ':'),
			\+ sub_atom(Host, 0, 1, _, '[') ->
			atomic_list_concat(['[', Host, ']'], WrappedHost)
		;	WrappedHost = Host
		),
		atomic_list_concat([WrappedHost, Port], ':', ConnectArgument).

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
		;	domain_error(http_process_transport_connection_pool_options, [min_size(MinSize), max_size(MaxSize)])
		).

	check_connection_options([]).
	check_connection_options([Option| Options]) :-
		check_connection_option(Option),
		check_connection_options(Options).

	check_connection_option(Option) :-
		connection_compatibility_option(Option),
		!.
	check_connection_option(Option) :-
		domain_error(http_process_transport_connection_option, Option).

	check_listener_socket_options([]).
	check_listener_socket_options([Option| Options]) :-
		check_listener_socket_option(Option),
		check_listener_socket_options(Options).

	check_listener_socket_option(Option) :-
		listener_socket_compatibility_option(Option),
		!.
	check_listener_socket_option(Option) :-
		domain_error(http_process_transport_listener_option, Option).

	check_listener_options([]).
	check_listener_options([Option| Options]) :-
		check_listener_option(Option),
		check_listener_options(Options).

	check_listener_option(shutdown(_)) :-
		!.
	check_listener_option(workers(_)) :-
		!.
	check_listener_option(Option) :-
		domain_error(http_socket_transport_listener_option, Option).

	check_open_listener_options([]).
	check_open_listener_options([Option| Options]) :-
		check_open_listener_option(Option),
		check_open_listener_options(Options).

	check_open_listener_option(workers(_)) :-
		!.
	check_open_listener_option(Option) :-
		domain_error(http_socket_transport_open_listener_option, Option).

	check_connection_pool_options([]).
	check_connection_pool_options([Option| Options]) :-
		check_connection_pool_option(Option),
		check_connection_pool_options(Options).

	check_connection_pool_option(min_size(_)) :-
		!.
	check_connection_pool_option(max_size(_)) :-
		!.
	check_connection_pool_option(connection_options(ConnectionOptions)) :-
		check_connection_options(ConnectionOptions),
		!.
	check_connection_pool_option(Option) :-
		domain_error(http_process_transport_connection_pool_option, Option).

	valid_option(type(Type)) :-
		valid_stream_type_option(Type).
	valid_option(connection_transport(Transport)) :-
		once((Transport == tcp; Transport == tls)).
	valid_option(backlog(Backlog)) :-
		integer(Backlog),
		Backlog > 0.
	valid_option(openssl_executable(Executable)) :-
		atom(Executable).
	valid_option(server_name(ServerName)) :-
		atom(ServerName).
	valid_option(listener_transport(Transport)) :-
		once((Transport == tcp; Transport == tls)).
	valid_option(listener_helper_executable(Executable)) :-
		once((Executable == ncat; Executable == socat)).
	valid_option(temporary_tls_credentials(Prefix)) :-
		atom(Prefix).
	valid_option(tls_certificate_file(File)) :-
		atom(File).
	valid_option(tls_key_file(File)) :-
		atom(File).
	valid_option(shutdown(Shutdown)) :-
		once((Shutdown == keep_open; Shutdown == close)).
	valid_option(workers(Workers)) :-
		ground(Workers),
		valid_workers_option(Workers).
	valid_option(openssl_arguments(Arguments)) :-
		proper_list(Arguments),
		valid_atom_list(Arguments).
	valid_option(min_size(MinSize)) :-
		integer(MinSize),
		MinSize >= 0.
	valid_option(max_size(MaxSize)) :-
		integer(MaxSize),
		MaxSize > 0.
	valid_option(connection_options(ConnectionOptions)) :-
		proper_list(ConnectionOptions),
		valid_connection_options(ConnectionOptions).

	default_option(type(binary)).
	default_option(connection_transport(tcp)).
	default_option(openssl_executable(openssl)).
	default_option(server_name(default)).
	default_option(backlog(5)).
	default_option(listener_transport(tcp)).
	default_option(listener_helper_executable(ncat)).
	default_option(shutdown(keep_open)).
	default_option(workers(serial)).
	default_option(openssl_arguments([])).
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

	valid_connection_options([]).
	valid_connection_options([Option| Options]) :-
		valid_connection_option(Option),
		valid_connection_options(Options).

	valid_connection_option(Option) :-
		valid_connection_compatibility_option(Option).

	connection_compatibility_option(type(_)).
	connection_compatibility_option(connection_transport(_)).
	connection_compatibility_option(openssl_executable(_)).
	connection_compatibility_option(server_name(_)).
	connection_compatibility_option(openssl_arguments(_)).

	listener_socket_compatibility_option(backlog(_)).
	listener_socket_compatibility_option(type(_)).
	listener_socket_compatibility_option(listener_transport(_)).
	listener_socket_compatibility_option(listener_helper_executable(_)).
	listener_socket_compatibility_option(temporary_tls_credentials(_)).
	listener_socket_compatibility_option(tls_certificate_file(_)).
	listener_socket_compatibility_option(tls_key_file(_)).

	finalize_listener_temporary_tls_credentials(ListenerId) :-
		take_listener_temporary_tls_credentials(ListenerId, Credentials),
		cleanup_temporary_tls_credentials(Credentials).

	cleanup_temporary_tls_credentials(none).
	cleanup_temporary_tls_credentials(credentials(CertificateFile, KeyFile)) :-
		catch(delete_file(CertificateFile), _, true),
		catch(delete_file(KeyFile), _, true).

	temporary_tls_credentials_files(Prefix, CertificateFile, KeyFile) :-
		validate_temporary_tls_credentials_prefix(Prefix),
		temporary_directory(TemporaryDirectory),
		pid(PID),
		atomic_list_concat([Prefix, PID, '_cert.pem'], CertificateName),
		atomic_list_concat([Prefix, PID, '_key.pem'], KeyName),
		path_concat(TemporaryDirectory, CertificateName, CertificateFile),
		path_concat(TemporaryDirectory, KeyName, KeyFile).

	validate_temporary_tls_credentials_prefix(Prefix) :-
		(	var(Prefix) ->
			instantiation_error
		;	atom(Prefix) ->
			true
		;	type_error(atom, Prefix)
		).

	valid_connection_compatibility_option(type(Type)) :-
		valid_stream_type_option(Type).
	valid_connection_compatibility_option(connection_transport(Transport)) :-
		once((Transport == tcp; Transport == tls)).
	valid_connection_compatibility_option(Option) :-
		valid_process_connection_option(Option).

	valid_process_connection_option(openssl_executable(Executable)) :-
		atom(Executable).
	valid_process_connection_option(server_name(ServerName)) :-
		atom(ServerName).
	valid_process_connection_option(openssl_arguments(Arguments)) :-
		proper_list(Arguments),
		valid_atom_list(Arguments).

	valid_stream_type_option(Type) :-
		once((Type == binary; Type == text)).

	valid_atom_list([]).
	valid_atom_list([Argument| Arguments]) :-
		atom(Argument),
		valid_atom_list(Arguments).

	:- meta_predicate(call_with_shutdown_policy(*, *, 0)).

	call_with_shutdown_policy(close, Listener, Goal) :-
		setup_call_cleanup(true, once(Goal), close_listener(Listener)).
	call_with_shutdown_policy(keep_open, _Listener, Goal) :-
		call(Goal).

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
		;	domain_error(http_socket_transport_connection_pool, Pool)
		).

	validate_listener_count(Count) :-
		(	var(Count) ->
			instantiation_error
		;	integer(Count),
			Count >= 0 ->
			true
		;	domain_error(non_negative_integer, Count)
		).

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
		permission_error(close, http_socket_transport_connection_pool, Pool).
	close_connection_pool_outcome(missing, Pool) :-
		existence_error(http_socket_transport_connection_pool, Pool).

	connection_pool_stats_outcome(stats(Available, InUse, Total, MinSize, MaxSize), _Pool, stats(Available, InUse, Total, MinSize, MaxSize)).
	connection_pool_stats_outcome(missing, Pool, _Stats) :-
		existence_error(http_socket_transport_connection_pool, Pool).

	acquire_managed_connection(Pool, PoolId, Connection) :-
		acquire_pool_connection(PoolId, Outcome),
		acquire_pool_connection_outcome(Outcome, Pool, Connection).

	acquire_pool_connection_outcome(connection(Connection), _Pool, Connection).
	acquire_pool_connection_outcome(exhausted, _Pool, _Connection) :-
		resource_error(http_socket_transport_connection_pool).
	acquire_pool_connection_outcome(missing, Pool, _Connection) :-
		existence_error(http_socket_transport_connection_pool, Pool).

	recycle_managed_connection(PoolId, Connection, Request, Response) :-
		(	^^connection_persistent(Request, Response) ->
			release_pool_connection(PoolId, Connection, Action),
			release_pool_connection_action(Action, Connection)
		;	discard_managed_connection(PoolId, Connection)
		).

	discard_managed_connection(PoolId, Connection) :-
		discard_pool_connection(PoolId, Connection),
		catch(close_connection(Connection), _, true).

	release_pool_connection_action(reused, _Connection).
	release_pool_connection_action(closed, Connection) :-
		catch(close_connection(Connection), _, true).

	pool_exchange_sequence(PoolId, Pool, Requests, Responses) :-
		(	Requests == [] ->
			connection_pool_id_outcome(PoolId, Outcome),
			connection_pool_stats_outcome(Outcome, Pool, _Stats),
			Responses = []
		;	acquire_managed_connection(Pool, PoolId, Connection),
			catch(
				exchange_sequence(Connection, Requests, Responses),
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
	one_shot_request_sequence_([Request| Requests], [Request| OneShotRequests]) :-
		one_shot_request_sequence_(Requests, OneShotRequests).

	ensure_close_connection_request(request(Method, Target, Version, Headers, Body, Properties0), Request) :-
		(	member(connection(_), Properties0) ->
			Properties = Properties0
		;	member(connection-_, Headers) ->
			Properties = Properties0
		;	Properties = [connection([close])| Properties0]
		),
		Request = request(Method, Target, Version, Headers, Body, Properties).

	lowercase_ascii_codes(Codes, LowercaseCodes) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes).

	close_connection_list([]).
	close_connection_list([Connection| Connections]) :-
		catch(close_connection(Connection), _, true),
		close_connection_list(Connections).

	close_stream_pair(Input, Output) :-
		(	Input == Output ->
			close(Input)
		;	close(Input),
			close(Output)
		).

	serve_listener_with_workers(serial, Listener, Handler, Count, ClientInfos) :-
		!,
		serve_listener_serial(Listener, Handler, Count, ClientInfos).
	serve_listener_with_workers(Workers, Listener, Handler, Count, ClientInfos) :-
		serve_listener_with_workers_impl(Workers, Listener, Handler, Count, ClientInfos).

	:- if(current_logtalk_flag(threads, supported)).

	:- meta_predicate(serve_until_shutdown(*, *, *, *, 0)).

	serve_until_shutdown(Listener, Handler, Control, Options, Ready) :-
		parse_open_listener_options(Options, Workers),
		register_shutdown_control(Control, Listener, RunId),
		(	catch(
				call(Ready),
				Error,
				(	catch(finalize_shutdown_listener(Listener), _, true),
					cleanup_shutdown_control(Control, RunId),
					throw(Error)
				)
			) ->
			setup_call_cleanup(
				true,
				serve_until_shutdown_with_workers(Workers, Listener, Handler, Control, RunId),
				(	finalize_shutdown_listener(Listener),
					cleanup_shutdown_control(Control, RunId)
				)
			)
		;	catch(finalize_shutdown_listener(Listener), _, true),
			cleanup_shutdown_control(Control, RunId),
			fail
		).

	request_shutdown_impl(Control) :-
		(	var(Control) ->
			instantiation_error
		;	listener_shutdown_control_(Control, _Listener, RunId) ->
			force_shutdown_control(Control, RunId)
		;	existence_error(http_socket_transport_shutdown_control, Control)
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
	serve_listener_parallel(Listener, Handler, Count, ClientInfos, Workers0, Workers) :-
		try_accept_bounded_listener_connection(Listener, Input, Output, ClientInfo, Outcome),
		(	Outcome == shutdown ->
			ClientInfos = [],
			Workers = Workers0
		;	spawn_connection_worker(Input, Output, Handler, Worker),
			NextCount is Count - 1,
			ClientInfos = [ClientInfo| RemainingClientInfos],
			serve_listener_parallel(Listener, Handler, NextCount, RemainingClientInfos, [Worker| Workers0], Workers)
		).

	serve_listener_pool_batch(_Listener, _Handler, _Size, 0, ClientInfos, ClientInfos) :-
		!.
	serve_listener_pool_batch(Listener, Handler, Size, Count, ClientInfos0, ClientInfos) :-
		pool_batch_counts(Size, Count, BatchCount, RemainingCount),
		accept_worker_batch(Listener, Handler, BatchCount, ClientInfos0, ClientInfos1, [], ReversedWorkers, Status),
		wait_for_reversed_workers(ReversedWorkers),
		(	Status == shutdown ->
			ClientInfos = ClientInfos1
		;	serve_listener_pool_batch(Listener, Handler, Size, RemainingCount, ClientInfos1, ClientInfos)
		).

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
			try_accept_bounded_listener_connection(Listener, Input, Output, ClientInfo, Outcome),
			(	Outcome == shutdown ->
				ClientInfos = ClientInfos0,
				Workers = Workers1
			;	spawn_notifying_connection_worker(listener_pool_worker_finished(RunId), Input, Output, Handler, Worker),
				NextCount is Count - 1,
				serve_listener_pool_rolling(Listener, Handler, RunId, Size, NextCount, [ClientInfo| ClientInfos0], ClientInfos, [Worker| Workers1], Workers)
			)
		;	threaded_wait(listener_pool_worker_finished(RunId)),
			serve_listener_pool_rolling(Listener, Handler, RunId, Size, Count, ClientInfos0, ClientInfos, Workers1, Workers)
		).

	accept_worker_batch(_Listener, _Handler, 0, ClientInfos, ClientInfos, Workers, Workers, completed) :-
		!.
	accept_worker_batch(Listener, Handler, BatchCount, ClientInfos0, ClientInfos, Workers0, Workers, Status) :-
		try_accept_bounded_listener_connection(Listener, Input, Output, ClientInfo, Outcome),
		(	Outcome == shutdown ->
			ClientInfos = ClientInfos0,
			Workers = Workers0,
			Status = shutdown
		;	spawn_connection_worker(Input, Output, Handler, Worker),
			NextBatchCount is BatchCount - 1,
			accept_worker_batch(Listener, Handler, NextBatchCount, [ClientInfo| ClientInfos0], ClientInfos, [Worker| Workers0], Workers, Status)
		).

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
			(	catch(close_stream_pair(Input, Output), _, true),
				throw(Error)
			)
		).

	spawn_notifying_connection_worker(Notification, Input, Output, Handler, worker(Tag, Goal)) :-
		Goal = setup_call_cleanup(
			true,
			serve_accepted_connection(Input, Output, Handler),
			threaded_notify(Notification)
		),
		catch(
			threaded_once(Goal, Tag),
			Error,
			(	catch(close_stream_pair(Input, Output), _, true),
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

	register_shutdown_control(Control, Listener, RunId) :-
		(	var(Control) ->
			instantiation_error
		;	\+ listener_shutdown_control_(Control, _, _),
			allocate_shutdown_run_id(RunId),
			assertz(listener_shutdown_control_(Control, Listener, RunId)) ->
			true
		;	permission_error(reuse, http_socket_transport_shutdown_control, Control)
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
			catch(request_listener_shutdown(Listener), _, true)
		;	true
		).

	shutdown_requested(Control, RunId) :-
		listener_shutdown_requested_(Control, RunId).

	serve_until_shutdown_serial(Listener, Handler, Control, RunId) :-
		(	shutdown_requested(Control, RunId) ->
			drain_shutdown_accept(Listener, Control, RunId)
		;	try_process_listener_accept(Listener, Control, RunId, Input, Output) ->
			(	shutdown_requested(Control, RunId) ->
				catch(close_stream_pair(Input, Output), _, true)
			;	catch(
					serve_accepted_connection(Input, Output, Handler),
					Error,
					(	force_shutdown_control(Control, RunId),
						throw(Error)
					)
				),
				serve_until_shutdown_serial(Listener, Handler, Control, RunId)
			)
		;	true
		).

	serve_until_shutdown_parallel(Listener, Handler, Control, RunId) :-
		check_finished_workers(Control, RunId),
		(	shutdown_requested(Control, RunId) ->
			drain_shutdown_accept(Listener, Control, RunId),
			wait_for_active_workers(Control, RunId)
		;	try_process_listener_accept(Listener, Control, RunId, Input, Output) ->
			(	shutdown_requested(Control, RunId) ->
				catch(close_stream_pair(Input, Output), _, true),
				wait_for_active_workers(Control, RunId)
			;	spawn_open_connection_worker(Control, RunId, Input, Output, Handler),
				serve_until_shutdown_parallel(Listener, Handler, Control, RunId)
			)
		;	wait_for_active_workers(Control, RunId)
		).

	serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size) :-
		check_finished_workers(Control, RunId),
		(	shutdown_requested(Control, RunId) ->
			drain_shutdown_accept(Listener, Control, RunId),
			wait_for_active_workers(Control, RunId)
		;	active_worker_count(Control, RunId, Count),
			(	Count < Size ->
				(	try_process_listener_accept(Listener, Control, RunId, Input, Output) ->
					(	shutdown_requested(Control, RunId) ->
						catch(close_stream_pair(Input, Output), _, true),
						wait_for_active_workers(Control, RunId)
					;	spawn_open_connection_worker(Control, RunId, Input, Output, Handler),
						serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size)
					)
				;	wait_for_active_workers(Control, RunId)
				)
			;	wait_for_one_active_worker(Control, RunId),
				serve_until_shutdown_pool(Listener, Handler, Control, RunId, Size)
			)
		).

	try_process_listener_accept(Listener, Control, RunId, Input, Output) :-
		catch(
			accept_process_listener_connection(Listener, Input, Output, _ClientInfo),
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

	drain_shutdown_accept(Listener, Control, RunId) :-
		( 	try_process_listener_accept(Listener, Control, RunId, Input, Output) ->
			catch(close_stream_pair(Input, Output), _, true)
		; 	true
		).

	spawn_open_connection_worker(Control, RunId, Input, Output, Handler) :-
		Goal = setup_call_cleanup(
			catch(
				true,
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
				catch(close_stream_pair(Input, Output), _, true),
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

	serve_until_shutdown(_Listener, _Handler, _Control, _Options, _Ready) :-
		throw(error(resource_error(threads), http_process_transport::serve_until_shutdown(_, _, _, _, _))).

	request_shutdown_impl(_Control) :-
		throw(error(resource_error(threads), http_process_transport::request_shutdown(_))).

	serve_listener_with_workers_impl(per_connection, _Listener, _Handler, _Count, _ClientInfos) :-
		throw(error(resource_error(threads), http_process_transport::serve_listener(_, _, _, _, [workers(per_connection)]))).
	serve_listener_with_workers_impl(pool(_Size), _Listener, _Handler, _Count, _ClientInfos) :-
		throw(error(resource_error(threads), http_process_transport::serve_listener(_, _, _, _, [workers(pool(_))]))).
	serve_listener_with_workers_impl(pool(_Size, rolling), _Listener, _Handler, _Count, _ClientInfos) :-
		throw(error(resource_error(threads), http_process_transport::serve_listener(_, _, _, _, [workers(pool(_, rolling))]))).

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

	:- if(predicate_property(atom_number(_, _), built_in)).

	atom_number(Atom, Number) :-
		{atom_number(Atom, Number)}.

	:- else.

	atom_number(Atom, Number) :-
		number_codes(Number, Codes),
		atom_codes(Atom, Codes).

	:- endif.

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	% workaround ECLiPSe broken implementation of the at_end_of_stream/1 standard predicate
	line_to_codes(Stream, Codes) :-
		ignore(peek_code(Stream, _)),
		reader_line_to_codes(Stream, Codes).

	:- else.

	line_to_codes(Stream, Codes) :-
		reader_line_to_codes(Stream, Codes).

	:- endif.

	verify_commands_availability :-
		operating_system_type(OS),
		verify_commands_availability(OS).

	verify_commands_availability(unix) :-
		(	shell('type openssl >/dev/null 2>&1') ->
			true
		;	print_message(warning, http_process_transport, @'Missing openssl command!')
		),
		(	shell('type ncat >/dev/null 2>&1') ->
			true
		;	print_message(warning, http_process_transport, @'Missing ncat command!')
		).
	verify_commands_availability(windows) :-
		(	shell('where /q openssl.exe') ->
			true
		;	print_message(warning, http_process_transport, @'Missing openssl command!')
		),
		(	shell('where /q ncat.exe') ->
			true
		;	print_message(warning, http_process_transport, @'Missing ncat command!')
		).
	verify_commands_availability(unknown) :-
		verify_commands_availability(unix).

:- end_object.
