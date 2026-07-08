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


:- protocol(http_socket_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Sockets-backed HTTP transport predicates.'
	]).

	:- public(open_listener/4).
	:- mode(open_listener(+atom, ?integer, --compound, +list), one_or_error).
	:- info(open_listener/4, [
		comment is 'Opens a TCP listener on the given host and port using the given socket options. If Port is a variable, it is unified with the bound port number.',
		argnames is ['Host', 'Port', 'Listener', 'Options'],
		exceptions is [
			'``Host`` is not a valid listener host atom' - type_error(atom, 'Host'),
			'``Port`` is not a non-negative integer or a variable' - domain_error(non_negative_integer, 'Port'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid listener option' - domain_error(http_socket_process_listener_option, 'Option'),
			'``Options`` contains an invalid listener helper executable' - domain_error(http_socket_process_listener_helper_executable, 'Executable'),
			'``Options`` contains incompatible TCP listener options' - domain_error(http_socket_process_listener_tcp_options, [listener_transport(tcp), temporary_tls_credentials('TemporaryTLSCredentialsPrefix')]),
			'``Options`` contains incompatible TLS listener options' - domain_error(http_socket_process_listener_tls_options, [tls_certificate_file('CertificateFile'), tls_key_file('KeyFile')]),
			'Temporary TLS credentials cannot be created' - resource_error(http_socket_process_temporary_tls_credentials),
			'The process-backed listener helper cannot be started' - resource_error(http_socket_process_listener)
		]
	]).

	:- public(close_listener/1).
	:- mode(close_listener(+compound), one_or_error).
	:- info(close_listener/1, [
		comment is 'Closes a listener previously opened with open_listener/4.',
		argnames is ['Listener'],
		exceptions is [
			'``Listener`` is a variable' - instantiation_error,
			'``Listener`` is not an open listener handle for the transport implementation' - domain_error(http_socket_listener, 'Listener'),
			'``Listener`` no longer exists for the transport implementation' - existence_error(http_socket_listener, 'Listener')
		]
	]).

	:- public(request_listener_shutdown/1).
	:- mode(request_listener_shutdown(+compound), one_or_error).
	:- info(request_listener_shutdown/1, [
		comment is 'Best-effort wakeup request for a listener accept loop opened with ``open_listener/4``. This predicate does not close the listener but causes a blocked accept call to return portably when possible.',
		argnames is ['Listener'],
		exceptions is [
			'``Listener`` is a variable' - instantiation_error,
			'``Listener`` is not an open listener handle for the transport implementation' - domain_error(http_socket_listener, 'Listener'),
			'``Listener`` no longer exists for the transport implementation' - existence_error(http_socket_listener, 'Listener')
		]
	]).

	:- public(open_connection/4).
	:- mode(open_connection(+atom, +integer, --compound, +list), one_or_error).
	:- info(open_connection/4, [
		comment is 'Opens a reusable client connection to the given host and port using the given socket options.',
		argnames is ['Host', 'Port', 'Connection', 'Options'],
		exceptions is [
			'``Host`` and ``Port`` are not a valid HTTP authority target' - domain_error(http_target, authority('Host', 'Port')),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid connection option' - domain_error(http_socket_process_connection_option, 'Option'),
			'The process-backed client helper cannot be started' - resource_error(http_socket_process_connection)
		]
	]).

	:- public(close_connection/1).
	:- mode(close_connection(+compound), one_or_error).
	:- info(close_connection/1, [
		comment is 'Closes a reusable client connection previously opened with open_connection/4.',
		argnames is ['Connection'],
		exceptions is [
			'``Connection`` is a variable' - instantiation_error,
			'``Connection`` is not an open reusable client connection handle' - domain_error(http_socket_connection, 'Connection')
		]
	]).

	:- public(connection_streams/3).
	:- mode(connection_streams(+compound, --stream, --stream), one_or_error).
	:- info(connection_streams/3, [
		comment is 'Returns the binary input and output streams carried by a reusable client connection handle or by an upgraded WebSocket connection handle returned by ``serve_websocket_once/5``.',
		argnames is ['Connection', 'Input', 'Output'],
		exceptions is [
			'``Connection`` is a variable' - instantiation_error,
			'``Connection`` is not an open reusable client or upgraded WebSocket connection handle' - domain_error(http_socket_connection, 'Connection')
		]
	]).

	:- public(open_connection_pool/4).
	:- mode(open_connection_pool(+atom, +integer, --compound, +list), one_or_error).
	:- info(open_connection_pool/4, [
		comment is 'Opens a managed reusable connection pool for the given host and port.',
		argnames is ['Host', 'Port', 'Pool', 'Options'],
		exceptions is [
			'``Host`` and ``Port`` are not a valid HTTP authority target' - domain_error(http_target, authority('Host', 'Port')),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid connection-pool option' - domain_error(http_socket_connection_pool_option, 'Option'),
			'``Options`` contains an invalid process-backed connection-pool option' - domain_error(http_socket_process_connection_pool_option, 'Option'),
			'``Options`` contains incompatible connection-pool bounds' - domain_error(http_socket_connection_pool_options, [min_size('MinSize'), max_size('MaxSize')]),
			'``Options`` contains incompatible process-backed connection-pool bounds' - domain_error(http_socket_process_connection_pool_options, [min_size('MinSize'), max_size('MaxSize')]),
			'No managed connection is available in the connection pool' - resource_error(http_socket_connection_pool),
			'The process-backed client helper cannot be started' - resource_error(http_socket_process_connection)
		],
		remarks is [
			'Option ``min_size(N)``' - 'Pre-open N reusable client connections when creating the pool. The default is ``0``.',
			'Option ``max_size(N)``' - 'Allow at most N managed client connections in the pool. The default is ``10``.',
			'Option ``connection_options(Options)``' - 'Socket options passed to ``open_connection/4`` when creating pooled connections. The default is ``[]``.'
		]
	]).

	:- public(close_connection_pool/1).
	:- mode(close_connection_pool(+compound), one_or_error).
	:- info(close_connection_pool/1, [
		comment is 'Closes a managed reusable connection pool and all currently available pooled connections. Throws if pooled exchanges are still in progress.',
		argnames is ['Pool'],
		exceptions is [
			'``Pool`` is a variable' - instantiation_error,
			'``Pool`` is not a valid managed connection-pool handle' - domain_error(http_socket_connection_pool, 'Pool'),
			'``Pool`` no longer exists' - existence_error(http_socket_connection_pool, 'Pool'),
			'``Pool`` cannot be closed while pooled exchanges remain active' - permission_error(close, http_socket_connection_pool, 'Pool')
		]
	]).

	:- public(connection_pool_stats/2).
	:- mode(connection_pool_stats(+compound, --compound), one_or_error).
	:- info(connection_pool_stats/2, [
		comment is 'Returns managed pool statistics as stats(Available, InUse, Total, MinSize, MaxSize).',
		argnames is ['Pool', 'Stats'],
		exceptions is [
			'``Pool`` is a variable' - instantiation_error,
			'``Pool`` is not a valid managed connection-pool handle' - domain_error(http_socket_connection_pool, 'Pool'),
			'``Pool`` no longer exists' - existence_error(http_socket_connection_pool, 'Pool')
		]
	]).

	:- public(exchange/3).
	:- mode(exchange(+compound, +compound, --compound), one_or_error).
	:- info(exchange/3, [
		comment is 'Performs a single HTTP exchange on an open reusable client connection or by temporarily acquiring a pooled reusable client connection.',
		argnames is ['ConnectionOrPool', 'Request', 'Response'],
		exceptions is [
			'``ConnectionOrPool`` is a variable' - instantiation_error,
			'``ConnectionOrPool`` is neither an open connection nor a managed connection-pool handle' - domain_error(http_socket_connection, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not a valid managed connection-pool handle' - domain_error(http_socket_connection_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` no longer exists as a managed connection pool' - existence_error(http_socket_connection_pool, 'ConnectionOrPool'),
			'No managed connection is available in the connection pool' - resource_error(http_socket_connection_pool),
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'The response stream cannot be parsed as a normalized HTTP response' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(exchange_sequence/3).
	:- mode(exchange_sequence(+compound, ++list(compound), --list(compound)), one_or_error).
	:- info(exchange_sequence/3, [
		comment is 'Performs a sequence of HTTP exchanges on an open reusable client connection or by temporarily acquiring a pooled reusable client connection while HTTP persistence rules allow it.',
		argnames is ['ConnectionOrPool', 'Requests', 'Responses'],
		exceptions is [
			'``ConnectionOrPool`` is a variable' - instantiation_error,
			'``ConnectionOrPool`` is neither an open connection nor a managed connection-pool handle' - domain_error(http_socket_connection, 'ConnectionOrPool'),
			'``ConnectionOrPool`` is not a valid managed connection-pool handle' - domain_error(http_socket_connection_pool, 'ConnectionOrPool'),
			'``ConnectionOrPool`` no longer exists as a managed connection pool' - existence_error(http_socket_connection_pool, 'ConnectionOrPool'),
			'No managed connection is available in the connection pool' - resource_error(http_socket_connection_pool),
			'``Requests`` is not a list of normalized HTTP request terms' - domain_error(http_request_sequence, 'Requests'),
			'The response stream cannot be parsed as a normalized HTTP response' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(exchange/4).
	:- mode(exchange(+atom, +integer, +compound, --compound), one_or_error).
	:- info(exchange/4, [
		comment is 'Opens a client socket connection to the given host and port, performs a single HTTP exchange, and closes the connection. When the request does not already specify connection handling, ``Connection: close`` is added automatically.',
		argnames is ['Host', 'Port', 'Request', 'Response'],
		exceptions is [
			'``Host`` and ``Port`` are not a valid HTTP authority target' - domain_error(http_target, authority('Host', 'Port')),
			'The process-backed client helper cannot be started' - resource_error(http_socket_process_connection),
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'The response stream cannot be parsed as a normalized HTTP response' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(exchange/5).
	:- mode(exchange(+atom, +integer, +compound, --compound, +list), one_or_error).
	:- info(exchange/5, [
		comment is 'Opens a client socket connection to the given host and port using the given connection options, performs a single HTTP exchange, and closes the connection. When the request does not already specify connection handling, ``Connection: close`` is added automatically.',
		argnames is ['Host', 'Port', 'Request', 'Response', 'Options'],
		exceptions is [
			'``Host`` and ``Port`` are not a valid HTTP authority target' - domain_error(http_target, authority('Host', 'Port')),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid connection option' - domain_error(http_socket_process_connection_option, 'Option'),
			'The process-backed client helper cannot be started' - resource_error(http_socket_process_connection),
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request'),
			'The response stream cannot be parsed as a normalized HTTP response' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(exchange_sequence/4).
	:- mode(exchange_sequence(+atom, +integer, ++list(compound), --list(compound)), one_or_error).
	:- info(exchange_sequence/4, [
		comment is 'Opens a client socket connection to the given host and port, performs a sequence of HTTP exchanges on that connection, and closes the connection. When the last request does not already specify connection handling, ``Connection: close`` is added automatically to that final request.',
		argnames is ['Host', 'Port', 'Requests', 'Responses'],
		exceptions is [
			'``Host`` and ``Port`` are not a valid HTTP authority target' - domain_error(http_target, authority('Host', 'Port')),
			'The process-backed client helper cannot be started' - resource_error(http_socket_process_connection),
			'``Requests`` is not a list of normalized HTTP request terms' - domain_error(http_request_sequence, 'Requests'),
			'The response stream cannot be parsed as a normalized HTTP response' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(serve_once/3).
	:- mode(serve_once(+compound, +object_identifier, --compound), one_or_error).
	:- info(serve_once/3, [
		comment is 'Accepts one incoming socket connection on the given server socket, serves that connection using the http_server library, closes the streams, and returns client information.',
		argnames is ['ServerSocket', 'Handler', 'ClientInfo'],
		exceptions is [
			'``Handler`` does not conform to the HTTP handler protocol' - domain_error(http_handler_protocol, 'Handler'),
			'The delegated HTTP server rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(serve_websocket_once/5).
	:- mode(serve_websocket_once(+compound, +object_identifier, --compound, --compound, --compound), one_or_error).
	:- info(serve_websocket_once/5, [
		comment is 'Accepts one incoming socket connection on the given listener, serves exactly one WebSocket opening handshake using the given handler, and on success returns an upgraded connection handle that remains open together with the handshake response and client information. Rejected or malformed handshakes are written to the stream, the accepted streams are closed, and the predicate throws.',
		argnames is ['Listener', 'Handler', 'Connection', 'Response', 'ClientInfo'],
		exceptions is [
			'``Handler`` does not conform to the HTTP handler protocol' - domain_error(http_handler_protocol, 'Handler'),
			'The WebSocket opening request does not exist' - existence_error(http_socket_websocket_request, end_of_file),
			'The WebSocket opening response is invalid' - domain_error(http_socket_websocket_response, 'Response'),
			'The delegated HTTP server rejects the response stream' - domain_error(http_response_stream, 'Error')
		]
	]).

	:- public(serve_listener/4).
	:- mode(serve_listener(+compound, +object_identifier, +integer, --list(compound)), one_or_error).
	:- info(serve_listener/4, [
		comment is 'Accepts and serves Count incoming connections on the given listener, returning the accepted client information terms in order. A ``request_listener_shutdown/1`` call for the listener may interrupt the bounded loop before Count connections are accepted.',
		argnames is ['Listener', 'Handler', 'Count', 'ClientInfos'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Handler`` does not conform to the HTTP handler protocol' - domain_error(http_handler_protocol, 'Handler'),
			'The delegated HTTP server rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'Thread workers are not available on this backend' - resource_error(threads)
		]
	]).

	:- public(serve_listener/5).
	:- mode(serve_listener(+compound, +object_identifier, +integer, --list(compound), +list), one_or_error).
	:- info(serve_listener/5, [
		comment is 'Accepts and serves ``Count`` incoming connections on the given listener using the specified shutdown and worker options. A ``request_listener_shutdown/1`` call for the listener may interrupt the bounded loop before ``Count`` connections are accepted.',
		argnames is ['Listener', 'Handler', 'Count', 'ClientInfos', 'Options'],
		exceptions is [
			'``Count`` is a variable' - instantiation_error,
			'``Count`` is not a non-negative integer' - domain_error(non_negative_integer, 'Count'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option'),
			'``Options`` contains an invalid listener-serving option' - domain_error(http_socket_listener_option, 'Option'),
			'``Handler`` does not conform to the HTTP handler protocol' - domain_error(http_handler_protocol, 'Handler'),
			'The delegated HTTP server rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'Thread workers are not available on this backend' - resource_error(threads)
		],
		remarks is [
			'Option ``shutdown(keep_open)``' - 'Leave the listener open after serving the requested number of connections. This is the default.',
			'Option ``shutdown(close)``' - 'Close the listener when serving completes or aborts.',
			'Option ``workers(serial)``' - 'Serve accepted connections sequentially in the caller thread. This is the default.',
			'Option ``workers(per_connection)``' - 'Spawn one worker thread per accepted connection and wait for all workers before returning. Requires backend thread support.',
			'Option ``workers(pool(Size))``' - 'Serve accepted connections in batches of up to ``Size`` worker threads, waiting for each batch to finish before accepting the next batch. Requires backend thread support.',
			'Option ``workers(pool(Size, rolling))``' - 'Serve accepted connections using at most ``Size`` concurrent worker threads, accepting the next connection as soon as a worker finishes. Requires backend thread support.'
		]
	]).

	:- public(serve_until_shutdown/4).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +nonvar, +list), one_or_error).
	:- info(serve_until_shutdown/4, [
		comment is 'Accepts and serves incoming connections on the given listener until ``request_shutdown/1`` is called for the specified control term, then closes the listener before returning.',
		argnames is ['Listener', 'Handler', 'Control', 'Options'],
		exceptions is [
			'``Control`` is a variable' - instantiation_error,
			'``Control`` is already registered for another open-ended listener loop' - permission_error(reuse, http_socket_shutdown_control, 'Control'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid listener-serving option' - domain_error(http_socket_listener_option, 'Option'),
			'``Handler`` does not conform to the HTTP handler protocol' - domain_error(http_handler_protocol, 'Handler'),
			'The delegated HTTP server rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'Thread workers are not available on this backend' - resource_error(threads)
		],
		remarks is [
			'Control term' - 'The control term must be non-variable and should be fresh for each open-ended serving loop.',
			'Option ``workers(serial)``' - 'Serve accepted connections sequentially in the caller thread. This is the default.',
			'Option ``workers(per_connection)``' - 'Spawn one worker thread per accepted connection and wait for all workers when shutdown is requested. Requires backend thread support.',
			'Option ``workers(pool(Size))``' - 'Serve accepted connections using at most Size concurrent worker threads, waiting for worker completion notifications before accepting additional connections. Requires backend thread support.',
			'Option ``workers(pool(Size, rolling))``' - 'Alias for ``workers(pool(Size))``. Requires backend thread support.',
			'Shutdown behavior' - 'Calling request_shutdown/1 wakes the serving loop, stops accepting new connections, and waits for active workers to finish before the listener is closed and the serving loop returns.'
		]
	]).

	:- public(serve_until_shutdown/5).
	:- meta_predicate(serve_until_shutdown(*, *, *, *, 0)).
	:- mode(serve_until_shutdown(+compound, +object_identifier, +nonvar, +list, +callable), one_or_error).
	:- info(serve_until_shutdown/5, [
		comment is 'Accepts and serves incoming connections on the given listener until ``request_shutdown/1`` is called for the specified control term, calling Ready after the shutdown control is registered and before the serving loop starts accepting connections.',
		argnames is ['Listener', 'Handler', 'Control', 'Options', 'Ready'],
		exceptions is [
			'``Control`` is a variable' - instantiation_error,
			'``Control`` is already registered for another open-ended listener loop' - permission_error(reuse, http_socket_shutdown_control, 'Control'),
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'``Options`` contains an invalid listener-serving option' - domain_error(http_socket_listener_option, 'Option'),
			'``Handler`` does not conform to the HTTP handler protocol' - domain_error(http_handler_protocol, 'Handler'),
			'The delegated HTTP server rejects the response stream' - domain_error(http_response_stream, 'Error'),
			'Thread workers are not available on this backend' - resource_error(threads)
		],
		remarks is [
			'Control term' - 'The control term must be non-variable and should be fresh for each open-ended serving loop.',
			'Ready goal' - 'Called once after the shutdown control is registered and before the serving loop starts accepting connections.',
			'Options' - 'Supports the same worker options as ``serve_until_shutdown/4``.'
		]
	]).

	:- public(request_shutdown/1).
	:- mode(request_shutdown(+nonvar), one_or_error).
	:- info(request_shutdown/1, [
		comment is 'Requests shutdown of an open-ended serving loop started with ``serve_until_shutdown/4`` or ``serve_until_shutdown/5`` for the specified control term and wakes any blocked accept call so the loop can terminate portably.',
		argnames is ['Control'],
		exceptions is [
			'``Control`` is a variable' - instantiation_error,
			'``Control`` is not registered for an open-ended listener loop' - existence_error(http_socket_shutdown_control, 'Control'),
			'Thread-backed shutdown is not available on this backend' - resource_error(threads)
		]
	]).

	:- public(supported_request_scheme/1).
	:- mode(supported_request_scheme(+nonvar), one_or_more).
	:- info(supported_request_scheme/1, [
		comment is 'Table of supported request schemas.',
		argnames is ['Scheme']
	]).

	:- public(supported_websocket_scheme/1).
	:- mode(supported_websocket_scheme(+nonvar), one_or_more).
	:- info(supported_websocket_scheme/1, [
		comment is 'Table of supported websocket schemas.',
		argnames is ['Scheme']
	]).

:- end_protocol.
