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


:- object(http_websocket_server_service(_HTTPTransport_),
	extends(http_websocket_service(_HTTPTransport_, server, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-13,
		comment is 'Server-side convenience for callback-driven WebSocket sessions with atom text representation, including registry-backed broadcast helpers.',
		parnames is ['HTTPTransport']
	]).

	:- public(serve_once/6).
	:- mode(serve_once(+compound, +object_identifier, +object_identifier, -compound, -compound, -compound), one_or_error).
	:- info(serve_once/6, [
		comment is 'Accepts one incoming socket connection on the given listener created by the selected transport parameterization, serves one WebSocket opening handshake using the given HTTP handler, then runs one callback-driven WebSocket session using the given session handler until the close handshake completes or the peer closes the stream.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Response', 'State', 'ClientInfo'],
		exceptions is [
			'The listener is invalid for the selected transport' - domain_error(http_socket_transport_listener, 'Listener'),
			'The WebSocket opening request does not exist' - existence_error(http_socket_transport_websocket_request, end_of_file),
			'The WebSocket opening response is invalid' - domain_error(http_socket_transport_websocket_response, 'Response'),
			'``SessionHandler`` is a variable' - instantiation_error,
			'``SessionHandler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'SessionHandler'),
			'The upgraded connection handle is invalid' - domain_error(http_socket_transport_connection, 'Connection'),
			'The delegated session loop raises a WebSocket session error' - domain_error(http_websocket_session_sequence, 'Frame'),
			'``SessionHandler`` returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply')
		]
	]).

	:- public(serve_once/7).
	:- mode(serve_once(+compound, +object_identifier, +object_identifier, -compound, -compound, -compound, +list), one_or_error).
	:- info(serve_once/7, [
		comment is 'Accepts one incoming socket connection on the given listener created by the selected transport parameterization, serves one WebSocket opening handshake using the given HTTP handler, then runs one callback-driven WebSocket session using the given session handler and the given session-loop options.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Response', 'State', 'ClientInfo', 'Options'],
		exceptions is [
			'The listener is invalid for the selected transport' - domain_error(http_socket_transport_listener, 'Listener'),
			'The WebSocket opening request does not exist' - existence_error(http_socket_transport_websocket_request, end_of_file),
			'The WebSocket opening response is invalid' - domain_error(http_socket_transport_websocket_response, 'Response'),
			'``SessionHandler`` is a variable' - instantiation_error,
			'``SessionHandler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'SessionHandler'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid WebSocket service loop option' - domain_error(http_websocket_service_option, 'Option'),
			'Timed session-loop options are not available on this backend' - existence_error(http_websocket_service, timing),
			'The upgraded connection handle is invalid' - domain_error(http_socket_transport_connection, 'Connection'),
			'The delegated session loop raises a WebSocket session error' - domain_error(http_websocket_session_sequence, 'Frame'),
			'``SessionHandler`` returns an invalid reply' - domain_error(http_websocket_service_handler_reply, 'Reply')
		],
		remarks is [
			'Option ``auto_pong(on)``' - 'Automatically writes pong replies while still forwarding ping messages to the session handler.',
			'Option ``auto_pong(off)``' - 'Disables automatic pong replies. This is the default.',
			'Option ``keepalive_interval(Seconds)``' - 'Schedules empty ping messages when the peer stays silent for the given positive number of seconds. This option requires backend thread support.',
			'Option ``idle_timeout(Seconds)``' - 'Closes the session with ``status(1001, idle_timeout)`` after the given positive number of seconds without an inbound message. This option requires backend thread support.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in the session loop. Use a non-negative integer.'
		]
	]).

	:- public(serve_until_shutdown/5).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +object_identifier, +compound, +nonvar), one_or_error).
	:- info(serve_until_shutdown/5, [
		comment is 'Accepts WebSocket opening handshakes on the given listener created by the selected transport parameterization until request_shutdown/1 is called for the specified control term, runs one callback-driven session loop per accepted upgraded connection, registers active sessions in the given registry for queued broadcasts, and closes the listener before returning.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Registry', 'Control'],
		exceptions is [
			'Registry-backed serving is not available on this backend' - existence_error(http_websocket_server_service, registry),
			'``Registry`` is not an open WebSocket service registry handle' - domain_error(http_websocket_service_registry, 'Registry'),
			'``Registry`` refers to a closed WebSocket service registry handle' - existence_error(http_websocket_service_registry, session_registry('RegistryId')),
			'``Control`` is a variable' - instantiation_error,
			'``Control`` is already registered for another open-ended WebSocket server loop' - permission_error(reuse, http_websocket_service_shutdown_control, 'Control'),
			'The WebSocket opening response is invalid' - domain_error(http_socket_transport_websocket_response, 'Response'),
			'``SessionHandler`` is a variable' - instantiation_error,
			'``SessionHandler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'SessionHandler'),
			'The delegated session loop raises a WebSocket session error' - domain_error(http_websocket_session_sequence, 'Frame'),
			'``SessionHandler`` returns an invalid registry action' - domain_error(http_websocket_service_handler_action, 'Action')
		],
		remarks is [
			'Thread support' - 'This helper requires backend thread support so that multiple sessions can stay active concurrently.',
			'Handler actions' - 'Plain normalized messages are written back to the originating session. ``broadcast(Message)`` queues ``Message`` for all registered sessions and ``broadcast_others(Message)`` queues it for all registered sessions except the originating one.'
		]
	]).

	:- public(serve_until_shutdown/6).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +object_identifier, +compound, +nonvar, +list), one_or_error).
	:- info(serve_until_shutdown/6, [
		comment is 'Accepts WebSocket opening handshakes on the given listener created by the selected transport parameterization until request_shutdown/1 is called for the specified control term, runs one registry-backed callback session per accepted connection, applies the given session-loop options to every active session, and closes the listener before returning.',
		argnames is ['Listener', 'HandshakeHandler', 'SessionHandler', 'Registry', 'Control', 'Options'],
		exceptions is [
			'Registry-backed serving is not available on this backend' - existence_error(http_websocket_server_service, registry),
			'``Registry`` is not an open WebSocket service registry handle' - domain_error(http_websocket_service_registry, 'Registry'),
			'``Registry`` refers to a closed WebSocket service registry handle' - existence_error(http_websocket_service_registry, session_registry('RegistryId')),
			'``Control`` is a variable' - instantiation_error,
			'``Control`` is already registered for another open-ended WebSocket server loop' - permission_error(reuse, http_websocket_service_shutdown_control, 'Control'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid WebSocket service loop option' - domain_error(http_websocket_service_option, 'Option'),
			'Timed session-loop options are not available on this backend' - existence_error(http_websocket_service, timing),
			'The WebSocket opening response is invalid' - domain_error(http_socket_transport_websocket_response, 'Response'),
			'``SessionHandler`` is a variable' - instantiation_error,
			'``SessionHandler`` is not a valid WebSocket service handler' - domain_error(http_websocket_service_handler, 'SessionHandler'),
			'The delegated session loop raises a WebSocket session error' - domain_error(http_websocket_session_sequence, 'Frame'),
			'``SessionHandler`` returns an invalid registry action' - domain_error(http_websocket_service_handler_action, 'Action')
		],
		remarks is [
			'Option ``auto_pong(on|off)``' - 'Controls automatic pong replies in each active session loop.',
			'Option ``keepalive_interval(Seconds)``' - 'Schedules empty ping messages when a peer stays silent for the given positive number of seconds.',
			'Option ``idle_timeout(Seconds)``' - 'Closes a session with ``status(1001, idle_timeout)`` after the given positive number of seconds without an inbound message.',
			'Option ``max_payload_length(Bytes)``' - 'Rejects inbound frames whose declared payload length is greater than ``Bytes`` before allocating payload storage. Oversized frames are treated as ``1009`` close errors in each session loop. Use a non-negative integer.'
		]
	]).

	:- public(request_shutdown/1).
	:- mode(request_shutdown(+nonvar), one_or_error).
	:- info(request_shutdown/1, [
		comment is 'Requests shutdown of a registry-backed server loop started with serve_until_shutdown/5-6 for the specified control term and wakes any blocked accept call so the loop can terminate portably.',
		argnames is ['Control'],
		exceptions is [
			'Registry-backed serving is not available on this backend' - existence_error(http_websocket_server_service, registry),
			'``Control`` is a variable' - instantiation_error,
			'``Control`` is not registered for an open-ended WebSocket server loop' - existence_error(http_websocket_service_shutdown_control, 'Control')
		]
	]).

	:- if(current_logtalk_flag(threads, supported)).
		:- threaded.

		:- private(shutdown_seed_/1).
		:- dynamic(shutdown_seed_/1).
		:- mode(shutdown_seed_(?positive_integer), zero_or_one).
		:- info(shutdown_seed_/1, [
			comment is 'Last allocated shutdown run identifier for open-ended WebSocket server loops.',
			argnames is ['RunId']
		]).

		:- private(shutdown_control_/3).
		:- dynamic(shutdown_control_/3).
		:- mode(shutdown_control_(?nonvar, ?compound, ?positive_integer), zero_or_more).
		:- info(shutdown_control_/3, [
			comment is 'Registered shutdown control terms and their associated listeners.',
			argnames is ['Control', 'Listener', 'RunId']
		]).

		:- private(shutdown_requested_/2).
		:- dynamic(shutdown_requested_/2).
		:- mode(shutdown_requested_(?nonvar, ?positive_integer), zero_or_more).
		:- info(shutdown_requested_/2, [
			comment is 'Recorded shutdown requests indexed by control term and run identifier.',
			argnames is ['Control', 'RunId']
		]).

		:- private(active_worker_/3).
		:- dynamic(active_worker_/3).
		:- mode(active_worker_(?nonvar, ?positive_integer, ?compound), zero_or_more).
		:- info(active_worker_/3, [
			comment is 'Active WebSocket session worker records indexed by control term and run identifier.',
			argnames is ['Control', 'RunId', 'Worker']
		]).

		:- synchronized([
			allocate_shutdown_run_id/1,
			register_shutdown_control/3,
			cleanup_shutdown_control/2,
			force_shutdown_control/2,
			register_active_worker/3,
			unregister_active_worker/3,
			current_active_workers/3
		]).

		:- meta_predicate(wait_for_worker(*, 0, *, *, *, *)).
		:- meta_predicate(collect_finished_worker(*, 0, *, *, *, *)).
	:- endif.

	:- uses(_HTTPTransport_, [
		close_connection/1, close_listener/1, request_listener_shutdown/1, serve_websocket_once/5
	]).

	serve_once(Listener, HandshakeHandler, SessionHandler, Response, State, ClientInfo) :-
		serve_once(Listener, HandshakeHandler, SessionHandler, Response, State, ClientInfo, []).

	serve_once(Listener, HandshakeHandler, SessionHandler, Response, State, ClientInfo, Options) :-
		serve_websocket_once(Listener, HandshakeHandler, Connection, Response, ClientInfo),
		^^run_session(Connection, SessionHandler, State, Options).

	:- if(current_logtalk_flag(threads, supported)).

	:- uses(user, [
		setup_call_cleanup/3
	]).

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control) :-
		serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, []).

	serve_until_shutdown(Listener, HandshakeHandler, SessionHandler, Registry, Control, Options) :-
		validate_session_registry(Registry),
		setup_call_cleanup(
			register_shutdown_control(Control, Listener, RunId),
			serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options),
			(	catch(close_listener(Listener), _, true),
				cleanup_shutdown_control(Control, RunId)
			)
		).

	request_shutdown(Control) :-
		(	var(Control) ->
			instantiation_error
		;	shutdown_control_(Control, _Listener, RunId) ->
			force_shutdown_control(Control, RunId)
		;	existence_error(http_websocket_service_shutdown_control, Control)
		).

	validate_session_registry(Registry) :-
		http_websocket_service_registry::session_count(Registry, _Count).

	register_shutdown_control(Control, Listener, RunId) :-
		(	var(Control) ->
			instantiation_error
		;	\+ shutdown_control_(Control, _, _),
			allocate_shutdown_run_id(RunId),
			assertz(shutdown_control_(Control, Listener, RunId)) ->
			true
		;	permission_error(reuse, http_websocket_service_shutdown_control, Control)
		).

	allocate_shutdown_run_id(RunId) :-
		(	retract(shutdown_seed_(CurrentRunId)) ->
			RunId is CurrentRunId + 1
		;	RunId = 1
		),
		assertz(shutdown_seed_(RunId)).

	cleanup_shutdown_control(Control, RunId) :-
		retractall(active_worker_(Control, RunId, _Worker)),
		retractall(shutdown_requested_(Control, RunId)),
		retractall(shutdown_control_(Control, _Listener, RunId)).

	force_shutdown_control(Control, RunId) :-
		(	shutdown_requested_(Control, RunId) ->
			true
		;	assertz(shutdown_requested_(Control, RunId))
		),
		(	shutdown_control_(Control, Listener, RunId) ->
			catch(request_listener_shutdown(Listener), _, true)
		;	true
		).

	shutdown_requested(Control, RunId) :-
		shutdown_requested_(Control, RunId).

	serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options) :-
		check_finished_workers(Control, RunId),
		(	shutdown_requested(Control, RunId) ->
			wait_for_active_workers(Control, RunId)
		;	accept_session_connection(Listener, HandshakeHandler, Control, RunId, Outcome),
			(	Outcome = accepted(Connection) ->
				spawn_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options),
				serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options)
			;	Outcome == recoverable ->
				serve_until_shutdown_loop(Listener, HandshakeHandler, SessionHandler, Registry, Control, RunId, Options)
			;	wait_for_active_workers(Control, RunId)
			)
		).

	accept_session_connection(Listener, HandshakeHandler, Control, RunId, Outcome) :-
		catch(serve_websocket_once(Listener, HandshakeHandler, Connection, _Response, _ClientInfo), Error, true),
		(	var(Error) ->
			Outcome = accepted(Connection)
		;	shutdown_requested(Control, RunId) ->
			Outcome = shutdown
		;	recoverable_accept_error(Error) ->
			Outcome = recoverable
		;	force_shutdown_control(Control, RunId),
			throw(Error)
		).

	recoverable_accept_error(error(domain_error(http_socket_transport_websocket_response, _Response), _Context)).
	recoverable_accept_error(error(existence_error(http_socket_transport_websocket_request, end_of_file), _Context)).

	spawn_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options) :-
		Goal = serve_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options),
		catch(
			threaded_once(Goal, Tag),
			Error,
			(	force_shutdown_control(Control, RunId),
				catch(close_connection(Connection), _, true),
				throw(Error)
			)
		),
		register_active_worker(Control, RunId, worker(Tag, Goal)).

	serve_session_worker(Control, RunId, Connection, Registry, SessionHandler, Options) :-
		setup_call_cleanup(
			true,
			catch(
				serve_registered_session(Connection, Registry, SessionHandler, Options),
				Error,
				(	force_shutdown_control(Control, RunId),
					throw(Error)
				)
			),
			threaded_notify(websocket_service_worker_finished(Control, RunId))
		).

	serve_registered_session(Connection, Registry, SessionHandler, Options) :-
		http_websocket_service_registry::register(Registry, Session),
		setup_call_cleanup(
			^^initial_state(State0),
			setup_call_cleanup(
				true,
				^^run_session_connection(Connection, handler_registry(Registry, Session, SessionHandler), State0, _State, Options),
				http_websocket_service_registry::unregister(Registry, Session)
			),
			catch(close_connection(Connection), _, true)
		).

	register_active_worker(Control, RunId, Worker) :-
		assertz(active_worker_(Control, RunId, Worker)).

	unregister_active_worker(Control, RunId, Worker) :-
		retractall(active_worker_(Control, RunId, Worker)).

	current_active_workers(Control, RunId, Workers) :-
		findall(Worker, active_worker_(Control, RunId, Worker), Workers).

	check_finished_workers(Control, RunId) :-
		collect_finished_workers(Control, RunId, no_error, Error),
		(	Error == no_error ->
			true
		;	throw(Error)
		).

	wait_for_active_workers(Control, RunId) :-
		current_active_workers(Control, RunId, Workers),
		wait_for_workers(Workers, Control, RunId, no_error, Error),
		(	Error == no_error ->
			true
		;	throw(Error)
		).

	wait_for_workers([], _Control, _RunId, Error, Error).
	wait_for_workers([worker(Tag, Goal)| Workers], Control, RunId, Error0, Error) :-
		wait_for_worker(Tag, Goal, Control, RunId, Error0, Error1),
		wait_for_workers(Workers, Control, RunId, Error1, Error).

	wait_for_worker(Tag, Goal, Control, RunId, Error0, Error) :-
		unregister_active_worker(Control, RunId, worker(Tag, Goal)),
		catch(threaded_exit(Goal, Tag), WorkerError, true),
		remember_worker_error(Error0, WorkerError, Error).

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

	remember_worker_error(no_error, WorkerError, error(WorkerError)) :-
		nonvar(WorkerError),
		!.
	remember_worker_error(Error, _WorkerError, Error).

	:- else.

	serve_until_shutdown(_Listener, _HandshakeHandler, _SessionHandler, _Registry, _Control) :-
		existence_error(http_websocket_server_service, registry).

	serve_until_shutdown(_Listener, _HandshakeHandler, _SessionHandler, _Registry, _Control, _Options) :-
		existence_error(http_websocket_server_service, registry).

	request_shutdown(_Control) :-
		existence_error(http_websocket_server_service, registry).

	:- endif.

:- end_object.


:- object(http_websocket_server_service,
	extends(http_websocket_service(http_socket_transport, server, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'By default, the server-side convenience for callback-driven WebSocket sessions with atom text representation, including registry-backed broadcast helpers, uses the ``http_socket_transport`` library.'
	]).

:- end_object.
