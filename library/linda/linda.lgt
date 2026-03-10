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


:- object(linda).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-03-10,
		comment is 'Linda tuple-space implementation for process communication. Provides a server that acts as a shared blackboard where clients can write (``out/1-2``), read (``rd/1-2``), and remove (``in/1-2``) tuples. Uses threaded engines for the server implementation and the sockets library for network communication.',
		remarks is [
			'Supported backends' - 'SWI-Prolog, Trealla Prolog, and XVM (requires both multi-threading and sockets support).',
			'Linda operations' - 'The basic operations are ``out/1-2`` (write tuple), ``in/1-2`` (remove tuple, blocking), ``rd/1-2`` (read tuple, blocking), ``in_noblock/1-2`` (remove tuple, non-blocking), and ``rd_noblock/1-2`` (read tuple, non-blocking).',
			'Tuple matching' - 'Tuples are matched using unification.',
			'Blocking behavior' - 'The ``in/1-2`` and ``rd/1-2`` predicates block until a matching tuple is available. The ``in_noblock/1-2`` and ``rd_noblock/1-2`` predicates fail immediately if no matching tuple is found.',
			'Multiple clients' - 'Multiple clients can connect to the same server. A tuple removed by the ``in/1-2`` or ``in_noblock/1-2`` predicates is only removed for one client.',
			'Multiple servers' - 'A client can connect to multiple servers. The first server it connects to, becomes the default server.',
			'API compatibility' - 'The API is inspired by the SICStus Prolog Linda library.',
			'Network communication' - 'Uses TCP sockets for client-server communication, allowing processes to run on different machines.'
		]
	]).

	:- threaded.

	% ==========================================================================
	% Server predicates
	% ==========================================================================

	:- public(linda/0).
	:- mode(linda, one).
	:- info(linda/0, [
		comment is 'Starts a Linda server on an automatically assigned port. The server address (``Host:Port``) is written to the current output stream. The predicate succeeds when all clients have disconnected after a shutdown request.'
	]).

	:- public(linda/1).
	:- meta_predicate(linda(::)).
	:- mode(linda(+list), one).
	:- info(linda/1, [
		comment is 'Starts a Linda server with the given options. The predicate succeeds when all clients have disconnected after a shutdown request.',
		argnames is ['Options'],
		remarks is [
			'Option ``port(Port)``' - 'Use ``Port`` as the server port. Must be an integer and an available port.',
			'Option ``Address-Goal``' - '``Address`` is unified with ``Host:Port`` and ``Goal`` is called when the server starts. Useful for saving the address or starting clients.',
			'Option ``accept_hook(Client,Stream,Goal)``' - 'When a client connects, ``Client`` is unified with the client address, ``Stream`` with the connection stream, and ``Goal`` is called. If ``Goal`` fails, the connection is rejected.'
		]
	]).

	% ==========================================================================
	% Client predicates
	% ==========================================================================

	:- public(linda_client/1).
	:- mode(linda_client(+compound), one_or_error).
	:- info(linda_client/1, [
		comment is 'Connects to a Linda server at the given address (``Host:Port``).',
		argnames is ['Address'],
		exceptions is [
			'Already connected' - linda_error(already_connected),
			'Connection failed' - linda_error(connection_failed('Error'))
		]
	]).

	:- public(close_client/1).
	:- mode(close_client(+compound), one).
	:- info(close_client/1, [
		comment is 'Closes the connection to the given Linda server.',
		argnames is ['Address']
	]).

	:- public(close_client/0).
	:- mode(close_client, one).
	:- info(close_client/0, [
		comment is 'Closes the connection to the default Linda server.'
	]).

	:- public(shutdown_server/1).
	:- mode(shutdown_server(+compound), one_or_error).
	:- info(shutdown_server/1, [
		comment is 'Sends a shutdown signal to the given server. The server stops accepting new connections but continues serving existing clients until they all disconnect. Call ``close_client/1`` after this predicate.',
		argnames is ['Address'],
		exceptions is [
			'Not connected' - linda_error(not_connected)
		]
	]).

	:- public(shutdown_server/0).
	:- mode(shutdown_server, one_or_error).
	:- info(shutdown_server/0, [
		comment is 'Sends a shutdown signal to the default server. The server stops accepting new connections but continues serving existing clients until they all disconnect. Call ``close_client/0`` after this predicate.',
		exceptions is [
			'Not connected' - linda_error(not_connected)
		]
	]).

	:- public(linda_timeout/2).
	:- mode(linda_timeout(?compound, +compound), one).
	:- info(linda_timeout/2, [
		comment is 'Gets or sets the client timeout. ``OldTime`` is unified with the current timeout and the timeout is set to ``NewTime``. The timeout value is either ``off`` (no timeout, wait forever) or ``Seconds:Milliseconds``.',
		argnames is ['OldTime', 'NewTime']
	]).

	% ==========================================================================
	% Tuple-space operations (client-side)
	% ==========================================================================

	:- public(out/2).
	:- mode(out(++compound, +term), one).
	:- info(out/2, [
		comment is 'Places the tuple ``Tuple`` in the tuple-space of the given server.',
		argnames is ['Address', 'Tuple']
	]).

	:- public(out/1).
	:- mode(out(+term), one).
	:- info(out/1, [
		comment is 'Places the tuple ``Tuple`` in the tuple-space of the default server.',
		argnames is ['Tuple']
	]).

	:- public(in/2).
	:- mode(in(++compound, ?term), one).
	:- info(in/2, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the given server. Blocks if no matching tuple is available.',
		argnames is ['Address', 'Tuple']
	]).

	:- public(in/1).
	:- mode(in(?term), one).
	:- info(in/1, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the default server. Blocks if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(in_noblock/2).
	:- mode(in_noblock(++compound, ?term), zero_or_one).
	:- info(in_noblock/2, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the default server. Fails if no matching tuple is available.',
		argnames is ['Address', 'Tuple']
	]).

	:- public(in_noblock/1).
	:- mode(in_noblock(?term), zero_or_one).
	:- info(in_noblock/1, [
		comment is 'Removes a tuple matching ``Tuple`` from the tuple-space of the default server. Fails if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(in_list/3).
	:- mode(in_list(++compound, +list, ?term), one).
	:- info(in_list/3, [
		comment is 'Removes a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the given server. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['Address', 'TupleList', 'Tuple']
	]).

	:- public(in_list/2).
	:- mode(in_list(+list, ?term), one).
	:- info(in_list/2, [
		comment is 'Removes a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the default server. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['TupleList', 'Tuple']
	]).

	:- public(rd/2).
	:- mode(rd(++compound, ?term), one).
	:- info(rd/2, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the given server without removing it. Blocks if no matching tuple is available.',
		argnames is ['Address', 'Tuple']
	]).

	:- public(rd/1).
	:- mode(rd(?term), one).
	:- info(rd/1, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the default server without removing it. Blocks if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(rd_noblock/2).
	:- mode(rd_noblock(++compound, ?term), zero_or_one).
	:- info(rd_noblock/2, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the given server without removing it. Fails if no matching tuple is available.',
		argnames is ['Address', 'Tuple']
	]).

	:- public(rd_noblock/1).
	:- mode(rd_noblock(?term), zero_or_one).
	:- info(rd_noblock/1, [
		comment is 'Reads a tuple matching ``Tuple`` from the tuple-space of the default server without removing it. Fails if no matching tuple is available.',
		argnames is ['Tuple']
	]).

	:- public(rd_list/3).
	:- mode(rd_list(++compound, +list, ?term), one).
	:- info(rd_list/3, [
		comment is 'Reads a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the given server without removing it. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['Address', 'TupleList', 'Tuple']
	]).

	:- public(rd_list/2).
	:- mode(rd_list(+list, ?term), one).
	:- info(rd_list/2, [
		comment is 'Reads a tuple matching one of the patterns in ``TupleList`` from the tuple-space of the default server without removing it. ``Tuple`` is unified with the matched tuple. Blocks if no matching tuple is available.',
		argnames is ['TupleList', 'Tuple']
	]).

	:- public(findall_rd_noblock/4).
	:- mode(findall_rd_noblock(++compound, ?term, +term, ?list), one).
	:- info(findall_rd_noblock/4, [
		comment is 'Returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic.',
		argnames is ['Address', 'Template', 'Tuple', 'List']
	]).

	:- public(findall_rd_noblock/3).
	:- mode(findall_rd_noblock(?term, +term, ?list), one).
	:- info(findall_rd_noblock/3, [
		comment is 'Returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	:- public(findall_in_noblock/4).
	:- mode(findall_in_noblock(++compound, ?term, +term, ?list), one).
	:- info(findall_in_noblock/4, [
		comment is 'Removes and returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic - all matching tuples are removed in one synchronized operation.',
		argnames is ['Address', 'Template', 'Tuple', 'List']
	]).

	:- public(findall_in_noblock/3).
	:- mode(findall_in_noblock(?term, +term, ?list), one).
	:- info(findall_in_noblock/3, [
		comment is 'Removes and returns a list of all instances of ``Template`` for tuples matching ``Tuple`` in the tuple-space. The operation is atomic - all matching tuples are removed in one synchronized operation.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	% ==========================================================================
	% Private server state using dynamic predicates
	% ==========================================================================

	:- private(server_socket_/1).
	:- dynamic(server_socket_/1).
	:- mode(server_socket_(?term), zero_or_one).
	:- info(server_socket_/1, [
		comment is 'Stores the server socket descriptor.',
		argnames is ['ServerSocket']
	]).

	:- private(client_connection_/3).
	:- dynamic(client_connection_/3).
	:- mode(client_connection_(?term, ?term, ?term), zero_or_more).
	:- info(client_connection_/3, [
		comment is 'Stores active client connections. Each client has an ID, input stream, and output stream.',
		argnames is ['ClientId', 'InputStream', 'OutputStream']
	]).

	:- private(accept_hook_/1).
	:- dynamic(accept_hook_/1).
	:- mode(accept_hook_(?callable), zero_or_one).
	:- info(accept_hook_/1, [
		comment is 'Stores the optional accept hook goal to call when a client connects.',
		argnames is ['Hook']
	]).

	:- private(server_running_/1).
	:- dynamic(server_running_/1).
	:- mode(server_running_(?compound), zero_or_one).
	:- info(server_running_/1, [
		comment is 'Flag indicating the server is running.',
		argnames is ['Address']
	]).

	:- private(server_shutdown_/0).
	:- dynamic(server_shutdown_/0).
	:- mode(server_shutdown_, zero_or_one).
	:- info(server_shutdown_/0, [
		comment is 'Flag indicating the server has received a shutdown request.'
	]).

	:- private(tuple_/1).
	:- dynamic(tuple_/1).
	:- mode(tuple_(?term), zero_or_more).
	:- info(tuple_/1, [
		comment is 'Stores tuples in the Linda tuple space.',
		argnames is ['Tuple']
	]).

	:- private(waiting_/3).
	:- dynamic(waiting_/3).
	:- mode(waiting_(?term, ?term, ?term), zero_or_more).
	:- info(waiting_/3, [
		comment is 'Stores blocked clients waiting for tuples. Records the client ID, request pattern, and output stream.',
		argnames is ['ClientId', 'Request', 'OutputStream']
	]).

	:- private(client_engine_/2).
	:- dynamic(client_engine_/2).
	:- mode(client_engine_(?term, ?atom), zero_or_more).
	:- info(client_engine_/2, [
		comment is 'Maps client IDs to their corresponding threaded engine names.',
		argnames is ['ClientId', 'EngineName']
	]).

	% ==========================================================================
	% Private client state
	% ==========================================================================

	:- private(client_connection_input_/2).
	:- dynamic(client_connection_input_/2).
	:- mode(client_connection_input_(?compound, ?term), zero_or_more).
	:- info(client_connection_input_/2, [
		comment is 'Stores the input stream for the client connection to the server.',
		argnames is ['Address', 'InputStream']
	]).

	:- private(client_connection_output_/2).
	:- dynamic(client_connection_output_/2).
	:- mode(client_connection_output_(?compound, ?term), zero_or_more).
	:- info(client_connection_output_/2, [
		comment is 'Stores the output stream for the client connection to the server.',
		argnames is ['Address', 'OutputStream']
	]).

	:- private(client_timeout_/1).
	:- dynamic(client_timeout_/1).
	:- mode(client_timeout_(?compound), zero_or_one).
	:- info(client_timeout_/1, [
		comment is 'Stores the timeout value for blocking client operations. Value is either ``off`` or ``Seconds:Milliseconds``.',
		argnames is ['Timeout']
	]).

	% Synchronized predicates for tuple space operations

	:- private(ts_out/1).
	:- synchronized(ts_out/1).
	:- mode(ts_out(+term), one).
	:- info(ts_out/1, [
		comment is 'Synchronized predicate to add a tuple to the tuple space and wake waiting clients.',
		argnames is ['Tuple']
	]).

	:- private(ts_in/4).
	:- synchronized(ts_in/4).
	:- mode(ts_in(+term, +term, +term, -compound), one).
	:- info(ts_in/4, [
		comment is 'Synchronized predicate to remove a matching tuple or register a waiting client.',
		argnames is ['Tuple', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_in_noblock/2).
	:- synchronized(ts_in_noblock/2).
	:- mode(ts_in_noblock(+term, -compound), zero_or_one).
	:- info(ts_in_noblock/2, [
		comment is 'Synchronized predicate to try removing a matching tuple without blocking.',
		argnames is ['Tuple', 'Found']
	]).

	:- private(ts_in_list/4).
	:- synchronized(ts_in_list/4).
	:- mode(ts_in_list(+list, +term, +term, -compound), one).
	:- info(ts_in_list/4, [
		comment is 'Synchronized predicate to remove a tuple matching one of multiple patterns or register a waiting client.',
		argnames is ['TupleList', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_rd/4).
	:- synchronized(ts_rd/4).
	:- mode(ts_rd(+term, +term, +term, -compound), one).
	:- info(ts_rd/4, [
		comment is 'Synchronized predicate to read a matching tuple or register a waiting client.',
		argnames is ['Tuple', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_rd_noblock/2).
	:- synchronized(ts_rd_noblock/2).
	:- mode(ts_rd_noblock(+term, -compound), zero_or_one).
	:- info(ts_rd_noblock/2, [
		comment is 'Synchronized predicate to try reading a matching tuple without blocking.',
		argnames is ['Tuple', 'Found']
	]).

	:- private(ts_rd_list/4).
	:- synchronized(ts_rd_list/4).
	:- mode(ts_rd_list(+list, +term, +term, -compound), one).
	:- info(ts_rd_list/4, [
		comment is 'Synchronized predicate to read a tuple matching one of multiple patterns or register a waiting client.',
		argnames is ['TupleList', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_findall_rd_noblock/3).
	:- synchronized(ts_findall_rd_noblock/3).
	:- mode(ts_findall_rd_noblock(+term, +term, -list), zero_or_more).
	:- info(ts_findall_rd_noblock/3, [
		comment is 'Synchronized predicate to collect all tuples matching a pattern.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	:- private(ts_findall_in_noblock/3).
	:- synchronized(ts_findall_in_noblock/3).
	:- mode(ts_findall_in_noblock(+term, +term, -list), zero_or_more).
	:- info(ts_findall_in_noblock/3, [
		comment is 'Synchronized predicate to collect and remove all tuples matching a pattern.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	% ==========================================================================
	% Dependencies
	% ==========================================================================

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(logtalk, [
		print_message/3,
		print_message(debug, linda, Message) as dbg(Message)
	]).

	:- uses(os, [
		cpu_time/1, sleep/1
	]).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(debug, linda, '>>> ', Stream) :-
		current_output(Stream).

	% ==========================================================================
	% Server implementation
	% ==========================================================================

	linda :-
		linda([(Address)-true]),
		sleep(1),
		server_running_(Address),
		print_message(information, linda, 'Server started at ~w~n'+[Address]).

	linda(Options) :-
		threaded_ignore(linda_(Options)).

	:- meta_predicate(linda_(::)).

	linda_(Options) :-
		context(Context),
		% Start socket server
		ignore(memberchk(port(Port), Options)),
		socket::server_open(Port, ServerSocket, [type(text)]),
		assertz(server_socket_(ServerSocket)),
		socket::current_host(Host),
		assertz(server_running_(Host:Port)),
		% Print server address
		dbg(Host-Port),
		% Process options
		process_server_options(Options, Host:Port),
		% Spawn client connection accept loop
		threaded_engine_create(_, accept_loop(ServerSocket), linda_server_engine),
		% Wait for it to complete
		catch(
			threaded_engine_next(linda_server_engine, _),
			Error,
			(	cleanup_server(ServerSocket),
				threaded_engine_destroy(linda_server_engine),
				throw(error(Error, Context))
			)
		),
		% Cleanup
		cleanup_server(ServerSocket),
		threaded_engine_destroy(linda_server_engine).

	cleanup_server(ServerSocket) :-
		dbg(@'Cleaning up server'),
		% Destroy all client engines
		forall(
			retract(client_engine_(_, EngineName)),
			catch(threaded_engine_destroy(EngineName), _, true)
		),
		retractall(server_socket_(_)),
		retractall(server_running_(_)),
		retractall(server_shutdown_),
		retractall(tuple_(_)),
		retractall(waiting_(_ ,_, _)),
		retractall(client_connection_(_, _, _)),
		retractall(accept_hook_(_)),
		catch(socket::server_close(ServerSocket), _, true).

	:- meta_predicate(process_server_options(::, *)).

	process_server_options([], _) :-
		!.
	process_server_options([Option| Options], Address) :-
		process_server_option(Option, Address),
		process_server_options(Options, Address).

	:- meta_predicate(process_server_option(::, *)).

	process_server_option(port(Port), Port) :-
		!,
		integer(Port).
	process_server_option(Address-Goal, Address) :-
		!,
		call(Goal).
	process_server_option(accept_hook(Client, Stream, Goal), _) :-
		!,
		assertz(accept_hook_(accept_hook(Client, Stream, Goal))).
	process_server_option(_, _).

	% Accept loop runs in its own thread, blocks on server_accept
	accept_loop(ServerSocket) :-
		(   server_shutdown_ ->
			% Stop accepting new connections
			dbg(@'Accept loop stopping due to shutdown'),
			threaded_engine_yield(done)
		;   catch(
				(   socket::server_accept(ServerSocket, Input, Output, ClientInfo, [type(text)]),
					dbg('Accepted new connection'-ClientInfo),
					(   accept_hook_(accept_hook(ClientAddr, Stream, Goal)) ->
						ClientInfo = client(ClientAddr),
						Stream = Input,
						(   call(Goal) ->
							create_client_engine(ClientInfo, Input, Output)
						;   socket::close(Input, Output)
						)
					;   create_client_engine(ClientInfo, Input, Output)
					)
				),
				Error,
				dbg('Accept error'-Error)
			),
			accept_loop(ServerSocket)
		).

	create_client_engine(ClientId, Input, Output) :-
		% Register client connection
		assertz(client_connection_(ClientId, Input, Output)),
		% Create engine that reads directly from its socket
		threaded_engine_create(_, client_engine_loop(ClientId, Input, Output, EngineName), EngineName),
		assertz(client_engine_(ClientId, EngineName)),
		dbg('Client connected'::['ClientId'-ClientId, 'EngineName'-EngineName]).

	% Each client engine reads directly from its socket (blocking read)
	% The loop must be robust - failures in handle_request must not terminate the engine
	client_engine_loop(ClientId, Input, Output, EngineName) :-
		adopt_stream(Input),
		adopt_stream(Output),
		(   catch(read_term(Input, Request, []), ReadError, (Request = read_error(ReadError))) ->
			dbg('Engine loop'::['EngineName'-EngineName, 'Request'-Request]),
			(   Request = read_error(Error) ->
				% Read error occurred
				dbg('Engine loop'-['EngineName'-EngineName, 'Read error'-Error]),
				remove_client(ClientId, Input, Output)
			;   Request == exit ->
				% Client disconnected
				dbg('Client disconnected'-ClientId),
				remove_client(ClientId, Input, Output)
			;   Request == end_of_file ->
				% Client disconnected
				dbg('Client disconnected'-ClientId),
				remove_client(ClientId, Input, Output)
			;   % Handle the request - catch any failures or errors
				dbg('Engine loop'::['EngineName'-EngineName, 'Handling request'-Request]),
				(   catch(
						handle_request(Request, ClientId, Output),
						HandleError,
						dbg('Engine loop'::['EngineName'-EngineName, 'Handle error'-HandleError])
					) ->
					true
				;   dbg('Engine loop'::['EngineName'-EngineName, 'Handle failure for request'-Request])
				),
				% Always continue reading regardless of handle_request outcome
				client_engine_loop(ClientId, Input, Output, EngineName)
			)
		;   % read_term failed without exception
			dbg('Engine loop'::['EngineName'-EngineName, 'Read action'-'failed']),
			remove_client(ClientId, Input, Output)
		).

	remove_client(ClientId, Input, Output) :-
		dbg('Removing client'-ClientId),
		retractall(client_connection_(ClientId, _, _)),
		% Retract and destroy the engine in a separate thread to avoid self-destruction
		(   retract(client_engine_(ClientId, EngineName)) ->
			threaded_ignore(threaded_engine_destroy(EngineName))
		;   true
		),
		retractall(waiting_(ClientId, _, _)),
		catch(socket::close(Input, Output), _, true).

	% ==========================================================================
	% Request handlers
	% All tuple space operations use synchronized predicates
	% ==========================================================================

	handle_request(out(Tuple), _ClientId, Output) :-
		!,
		ts_out(Tuple),
		write_canonical(Output, ok),
		write(Output, '.\n'),
		flush_output(Output).

	handle_request(in(Tuple), ClientId, Output) :-
		!,
		ts_in(Tuple, ClientId, Output, Found),
		(   Found = yes(Result) ->
			write_canonical(Output, result(Result)),
			write(Output, '.\n'),
			flush_output(Output)
		;   true  % Response will be sent when tuple becomes available
		).

	handle_request(in_noblock(Tuple), _ClientId, Output) :-
		!,
		ts_in_noblock(Tuple, Found),
		(   Found = yes(Result) ->
			write_canonical(Output, result(Result))
		;   write_canonical(Output, fail)
		),
		write(Output, '.\n'),
		flush_output(Output).

	handle_request(in_list(TupleList), ClientId, Output) :-
		!,
		ts_in_list(TupleList, ClientId, Output, Found),
		(   Found = yes(Result) ->
			write_canonical(Output, result(Result)),
			write(Output, '.\n'),
			flush_output(Output)
		;   true
		).

	handle_request(rd(Tuple), ClientId, Output) :-
		!,
		ts_rd(Tuple, ClientId, Output, Found),
		(   Found = yes(Result) ->
			write_canonical(Output, result(Result)),
			write(Output, '.\n'),
			flush_output(Output)
		;   true
		).

	handle_request(rd_noblock(Tuple), _ClientId, Output) :-
		!,
		ts_rd_noblock(Tuple, Found),
		(   Found = yes(Result) ->
			write_canonical(Output, result(Result))
		;   write_canonical(Output, fail)
		),
		write(Output, '.\n'),
		flush_output(Output).

	handle_request(rd_list(TupleList), ClientId, Output) :-
		!,
		ts_rd_list(TupleList, ClientId, Output, Found),
		(   Found = yes(Result) ->
			write_canonical(Output, result(Result)),
			write(Output, '.\n'),
			flush_output(Output)
		;   true
		).

	handle_request(findall_rd_noblock(Template, Tuple), _ClientId, Output) :-
		!,
		ts_findall_rd_noblock(Template, Tuple, List),
		write_canonical(Output, result(List)),
		write(Output, '.\n'),
		flush_output(Output).

	handle_request(findall_in_noblock(Template, Tuple), _ClientId, Output) :-
		!,
		ts_findall_in_noblock(Template, Tuple, List),
		write_canonical(Output, result(List)),
		write(Output, '.\n'),
		flush_output(Output).

	handle_request(shutdown, ClientId, Output) :-
		!,
		assertz(server_shutdown_),
		% Send response first
		write_canonical(Output, ok),
		write(Output, '.\n'),
		flush_output(Output),
		% Write end_of_file term to all client input streams to terminate engines
		forall(
			(client_connection_(OtherClientId, Input, _), OtherClientId \== ClientId),
			(write_canonical(Input, exit), write(Input, '.\n'), flush_output(Input))
		).

	handle_request(_, _ClientId, Output) :-
		write_canonical(Output, error(unknown_request)),
		write(Output, '.\n'),
		flush_output(Output).

	% ==========================================================================
	% Synchronized tuple space operations
	% These predicates are declared synchronized so they execute atomically
	% ==========================================================================

	ts_out(Tuple) :-
		assertz(tuple_(Tuple)),
		% Check if any blocked clients are waiting for this tuple
		wake_waiting_clients(Tuple).

	ts_in(Tuple, ClientId, Output, Found) :-
		(   retract(tuple_(Match)), copy_term(Tuple, TupleCopy), Match = TupleCopy ->
			Found = yes(Match)
		;   % No matching tuple, client will block
			assertz(waiting_(ClientId, in(Tuple), Output)),
			Found = no
		).

	ts_in_noblock(Tuple, Found) :-
		(   retract(tuple_(Match)), copy_term(Tuple, TupleCopy), Match = TupleCopy ->
			Found = yes(Match)
		;   Found = no
		).

	ts_in_list(TupleList, ClientId, Output, Found) :-
		(   find_matching_tuple(TupleList, Match), retract(tuple_(Match)) ->
			Found = yes(Match)
		;   assertz(waiting_(ClientId, in_list(TupleList), Output)),
			Found = no
		).

	ts_rd(Tuple, ClientId, Output, Found) :-
		(   tuple_(Match), copy_term(Tuple, TupleCopy), Match = TupleCopy ->
			Found = yes(Match)
		;   assertz(waiting_(ClientId, rd(Tuple), Output)),
			Found = no
		).

	ts_rd_noblock(Tuple, Found) :-
		(   tuple_(Match), copy_term(Tuple, TupleCopy), Match = TupleCopy ->
			Found = yes(Match)
		;   Found = no
		).

	ts_rd_list(TupleList, ClientId, Output, Found) :-
		(   find_matching_tuple_rd(TupleList, Match) ->
			Found = yes(Match)
		;   assertz(waiting_(ClientId, rd_list(TupleList), Output)),
			Found = no
		).

	% Template and Tuple share variables by design - Template contains variables
	% that get instantiated when Tuple unifies with stored tuples
	ts_findall_rd_noblock(Template, Tuple, List) :-
		findall(Template, tuple_(Tuple), List).

	% Collect and remove all matching tuples atomically
	ts_findall_in_noblock(Template, Tuple, List) :-
		findall(Template, (tuple_(Tuple), once(retract(tuple_(Tuple)))), List).

	% ==========================================================================
	% Auxiliary predicates
	% ==========================================================================

	find_matching_tuple([Pattern| Patterns], Match) :-
		(   tuple_(Match), copy_term(Pattern, PatternCopy), Match = PatternCopy ->
			true
		;   find_matching_tuple(Patterns, Match)
		).

	find_matching_tuple_rd([Pattern| Patterns], Match) :-
		(   tuple_(Match), copy_term(Pattern, PatternCopy), Match = PatternCopy ->
			true
		;   find_matching_tuple_rd(Patterns, Match)
		).

	% Wake waiting clients when a new tuple is added.
	% For in/1 and in_list/1: only ONE client gets the tuple (and tuple is consumed)
	% For rd/1 and rd_list/1: ALL matching clients get notified (tuple remains)
	% Called from synchronized ts_out/1 predicate
	wake_waiting_clients(Tuple) :-
		% First, try to wake ONE in/1 or in_list/1 waiter (they consume the tuple)
		(   wake_one_in_waiter(Tuple) ->
			% Tuple was consumed by an in/1 waiter, retract it
			ignore(retract(tuple_(Tuple)))
		;   % No in/1 waiter took it, tuple remains - wake all rd/1 waiters
			wake_all_rd_waiters(Tuple)
		).

	% Try to wake one in/1 or in_list/1 waiter. Succeeds if a waiter was woken.
	wake_one_in_waiter(Tuple) :-
		% Find the first matching in/1 or in_list/1 waiter
		(   waiting_(ClientId, in(Pattern), Output),
			copy_term(Pattern, PatternCopy),
			Tuple = PatternCopy ->
			Request = in(Pattern)
		;   waiting_(ClientId, in_list(Patterns), Output),
			member(Pattern, Patterns),
			copy_term(Pattern, PatternCopy),
			Tuple = PatternCopy ->
			Request = in_list(Patterns)
		;   fail
		),
		% Wake this one waiter
		retract(waiting_(ClientId, Request, Output)),
		catch(
			(   write_canonical(Output, result(Tuple)),
				write(Output, '.\n'),
				flush_output(Output)
			),
			_,
			true
		).

	% Wake all rd/1 and rd_list/1 waiters matching the tuple
	wake_all_rd_waiters(Tuple) :-
		forall(
			(   waiting_(ClientId, Request, Output),
				is_rd_request(Request, Tuple)
			),
			(   retract(waiting_(ClientId, Request, Output)) ->
				catch(
					(   write_canonical(Output, result(Tuple)),
						write(Output, '.\n'),
						flush_output(Output)
					),
					_,
					true
				)
			;   true
			)
		).

	% Check if a request is a rd/1 or rd_list/1 that matches the tuple
	is_rd_request(rd(Pattern), Tuple) :-
		copy_term(Pattern, PatternCopy),
		Tuple = PatternCopy.
	is_rd_request(rd_list(Patterns), Tuple) :-
		member(Pattern, Patterns),
		copy_term(Pattern, PatternCopy),
		Tuple = PatternCopy,
		!.

	% ==========================================================================
	% Client implementation
	% ==========================================================================

	linda_client(Host:Port) :-
		context(Context),
		(	client_connection_input_(Host:Port, _) ->
			throw(error(linda_error(already_connected), Context))
		;	catch(
				(	socket::client_open(Host, Port, Input, Output, [type(text)]),
					assertz(client_connection_input_(Host:Port, Input)),
					assertz(client_connection_output_(Host:Port, Output))
				),
				Error,
				throw(error(linda_error(connection_failed(Error)), Context))
			)
		).

	close_client(Address) :-
		(	retract(client_connection_input_(Address, Input)),
			retract(client_connection_output_(Address, Output)) ->
			write_canonical(Output, exit),
			write(Output, '.\n'),
			flush_output(Output),
			catch(socket::close(Input, Output), _, true)
		;	true
		).

	close_client :-
		close_client(_).

	shutdown_server(Address) :-
		context(Context),
		assertz(server_shutdown_),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, shutdown),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			read_term(Input, Response, []),
			(	Response == ok ->
				true
			;	throw(error(linda_error(shutdown_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	shutdown_server :-
		shutdown_server(_).

	linda_timeout(OldTime, NewTime) :-
		(	retract(client_timeout_(OldTime)) ->
			true
		;	OldTime = off
		),
		assertz(client_timeout_(NewTime)).

	% ==========================================================================
	% Client tuple-space operations
	% ==========================================================================

	out(Address, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, out(Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			read_term(Input, Response, []),
			(	Response == ok ->
				true
			;	throw(error(linda_error(out_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	out(Tuple) :-
		out(_, Tuple).

	in(Address, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, in(Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(in_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	in(Tuple) :-
		in(_, Tuple).

	in_noblock(Address, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, in_noblock(Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			read_term(Input, Response, []),
			(	Response = result(Match) ->
				Tuple = Match
			;	Response == fail ->
				fail
			;	throw(error(linda_error(in_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	in_noblock(Tuple) :-
		in_noblock(_, Tuple).

	in_list(Address, TupleList, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, in_list(TupleList)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(in_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	in_list(TupleList, Tuple) :-
		in_list(_, TupleList, Tuple).

	rd(Address, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, rd(Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(rd_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	rd(Tuple) :-
		rd(_, Tuple).

	rd_noblock(Address, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, rd_noblock(Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			read_term(Input, Response, []),
			(	Response = result(Match) ->
				Tuple = Match
			;	Response == fail ->
				fail
			;	throw(error(linda_error(rd_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	rd_noblock(Tuple) :-
		rd_noblock(_, Tuple).

	rd_list(Address, TupleList, Tuple) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, rd_list(TupleList)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			wait_for_result(Input, Result, Context),
			(	Result = result(Match) ->
				Tuple = Match
			;	throw(error(linda_error(rd_failed(Result)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	rd_list(TupleList, Tuple) :-
		rd_list(_, TupleList, Tuple).

	findall_rd_noblock(Address, Template, Tuple, Bag) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, findall_rd_noblock(Template, Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			read_term(Input, Response, []),
			(	Response = result(ResultBag) ->
				Bag = ResultBag
			;	Response == fail ->
				fail
			;	throw(error(linda_error(findall_rd_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	findall_rd_noblock(Template, Tuple, Bag) :-
		findall_rd_noblock(_, Template, Tuple, Bag).

	findall_in_noblock(Address, Template, Tuple, Bag) :-
		context(Context),
		(	client_connection_output_(Address, Output) ->
			write_canonical(Output, findall_in_noblock(Template, Tuple)),
			write(Output, '.\n'),
			flush_output(Output),
			client_connection_input_(Address, Input),
			read_term(Input, Response, []),
			(	Response = result(ResultBag) ->
				Bag = ResultBag
			;	Response == fail ->
				fail
			;	throw(error(linda_error(findall_in_noblock_failed(Response)), Context))
			)
		;	throw(error(linda_error(not_connected), Context))
		).

	findall_in_noblock(Template, Tuple, Bag) :-
		findall_in_noblock(_, Template, Tuple, Bag).

	% Check if stream has data ready (backend dependent)
	stream_ready(Stream) :-
		catch(peek_char(Stream, _), _, fail).

	% Auxiliary predicate to wait for result with optional timeout
	wait_for_result(Input, Result, Context) :-
		(	client_timeout_(Timeout), Timeout \== off ->
			Timeout = Seconds:Milliseconds,
			TotalSeconds is Seconds + Milliseconds / 1000,
			wait_for_result_timeout(Input, Result, TotalSeconds, Context)
		;	% No timeout, block until result
			read_term(Input, Result, [])
		).

	wait_for_result_timeout(Input, Result, TimeoutSeconds, Context) :-
		cpu_time(StartTime),
		wait_for_result_loop(Input, Result, StartTime, TimeoutSeconds, Context).

	wait_for_result_loop(Input, Result, StartTime, TimeoutSeconds, Context) :-
		(	stream_ready(Input) ->
			read_term(Input, Result, [])
		;	cpu_time(CurrentTime),
			Elapsed is CurrentTime - StartTime,
			(	Elapsed > TimeoutSeconds ->
				throw(error(linda_error(timeout), Context))
			;	% Small sleep to avoid busy waiting
				sleep(0.01),
				wait_for_result_loop(Input, Result, StartTime, TimeoutSeconds, Context)
			)
		).

	:- if(current_logtalk_flag(prolog_dialect, xvm)).
		% streams are thread-owned in XVM
		adopt_stream(Stream) :-
			{adopt_stream(Stream)}.
	:- else.
		adopt_stream(_).
	:- endif.

:- end_object.
