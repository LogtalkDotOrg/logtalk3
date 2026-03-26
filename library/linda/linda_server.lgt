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


:- category(linda_server).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-03-26,
		comment is 'Linda server predicates and tuple-space state. Import into a threaded object together with the linda_client category.'
	]).

	% ==========================================================================
	% Server predicates
	% ==========================================================================

	:- public(linda/1).
	:- meta_predicate(linda(::)).
	:- mode(linda(+list), one).
	:- info(linda/1, [
		comment is 'Starts a Linda server with the given options. The predicate succeeds when all clients have disconnected after a shutdown request.',
		argnames is ['Options'],
		remarks is [
			'Option ``port(Port)``' - 'Use ``Port`` as the server port. Must be an integer and an available port.',
			'Option ``Address-Goal``' - '``Address`` is unified with ``Host:Port`` and ``Goal`` is called when the server starts. Useful for saving the address or starting clients.',
			'Option ``accept_hook(Client,Input,Output,Goal)``' - 'When a client connects, ``Client`` is unified with the client address, ``Input`` and ``Output`` with the connection streams, and ``Goal`` is called. If ``Goal`` fails, the connection is rejected.'
		]
	]).

	:- public(linda/0).
	:- mode(linda, one).
	:- info(linda/0, [
		comment is 'Starts a Linda server on an automatically assigned port using default options. The server address (``Host:Port``) is written to the current output stream. The predicate succeeds when all clients have disconnected after a shutdown request.'
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
	:- mode(server_running_(?address), zero_or_one).
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

	% Synchronized predicates for tuple space operations

	:- private(ts_out/1).
	:- mode(ts_out(+term), one).
	:- info(ts_out/1, [
		comment is 'Synchronized predicate to add a tuple to the tuple space and wake waiting clients.',
		argnames is ['Tuple']
	]).

	:- private(ts_in/4).
	:- mode(ts_in(+term, +term, +term, -compound), one).
	:- info(ts_in/4, [
		comment is 'Synchronized predicate to remove a matching tuple or register a waiting client.',
		argnames is ['Tuple', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_in_noblock/2).
	:- mode(ts_in_noblock(+term, -compound), zero_or_one).
	:- info(ts_in_noblock/2, [
		comment is 'Synchronized predicate to try removing a matching tuple without blocking.',
		argnames is ['Tuple', 'Found']
	]).

	:- private(ts_in_list/4).
	:- mode(ts_in_list(+list, +term, +term, -compound), one).
	:- info(ts_in_list/4, [
		comment is 'Synchronized predicate to remove a tuple matching one of multiple patterns or register a waiting client.',
		argnames is ['TupleList', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_rd/4).
	:- mode(ts_rd(+term, +term, +term, -compound), one).
	:- info(ts_rd/4, [
		comment is 'Synchronized predicate to read a matching tuple or register a waiting client.',
		argnames is ['Tuple', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_rd_noblock/2).
	:- mode(ts_rd_noblock(+term, -compound), zero_or_one).
	:- info(ts_rd_noblock/2, [
		comment is 'Synchronized predicate to try reading a matching tuple without blocking.',
		argnames is ['Tuple', 'Found']
	]).

	:- private(ts_rd_list/4).
	:- mode(ts_rd_list(+list, +term, +term, -compound), one).
	:- info(ts_rd_list/4, [
		comment is 'Synchronized predicate to read a tuple matching one of multiple patterns or register a waiting client.',
		argnames is ['TupleList', 'ClientId', 'OutputStream', 'Found']
	]).

	:- private(ts_findall_rd_noblock/3).
	:- mode(ts_findall_rd_noblock(+term, +term, -list), zero_or_more).
	:- info(ts_findall_rd_noblock/3, [
		comment is 'Synchronized predicate to collect all tuples matching a pattern.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	:- private(ts_findall_in_noblock/3).
	:- mode(ts_findall_in_noblock(+term, +term, -list), zero_or_more).
	:- info(ts_findall_in_noblock/3, [
		comment is 'Synchronized predicate to collect and remove all tuples matching a pattern.',
		argnames is ['Template', 'Tuple', 'List']
	]).

	:- synchronized([
		ts_out/1,
		ts_in/4,
		ts_in_noblock/2,
		ts_in_list/4,
		ts_rd/4,
		ts_rd_noblock/2,
		ts_rd_list/4,
		ts_findall_rd_noblock/3,
		ts_findall_in_noblock/3
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
		sleep/1
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
		retractall(server_shutdown_),
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
		wait_for_clients_to_disconnect,
		% Cleanup
		cleanup_server(ServerSocket),
		threaded_engine_destroy(linda_server_engine).

	wait_for_clients_to_disconnect :-
		(	client_connection_(_, _, _) ->
			sleep(0.01),
			wait_for_clients_to_disconnect
		;	true
		).

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
	process_server_option(accept_hook(Client, Input, Output, Goal), _) :-
		!,
		assertz(accept_hook_(accept_hook(Client, Input, Output, Goal))).
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
					(   accept_hook_(accept_hook(ClientAddr, Input, Output, Goal)) ->
						ClientInfo = client(ClientAddr),
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
		write(Output, 'ok.\n'),
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
			write_canonical(Output, result(Result)),
			write(Output, '.\n')
		;   write(Output, 'fail.\n')
		),
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
			write_canonical(Output, result(Result)),
			write(Output, '.\n')
		;   write(Output, 'fail.\n')
		),
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
		(	retract(server_socket_(ServerSocket)) ->
			catch(socket::server_close(ServerSocket), Error, dbg('Server shutdown error'-Error))
		;	true
		),
		% Send response first
		write(Output, 'ok.\n'),
		flush_output(Output),
		% Write exit term to all client input streams to terminate engines
		forall(
			(client_connection_(OtherClientId, Input, _), OtherClientId \== ClientId),
			(write(Input, 'exit.\n'), flush_output(Input))
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
		(   retract(tuple_(Tuple)) ->
			Found = yes(Tuple)
		;   % No matching tuple, client will block
			assertz(waiting_(ClientId, in(Tuple), Output)),
			Found = no
		).

	ts_in_noblock(Tuple, Found) :-
		(   retract(tuple_(Tuple)) ->
			Found = yes(Tuple)
		;   Found = no
		).

	ts_in_list(TupleList, ClientId, Output, Found) :-
		(   find_matching_tuple(TupleList, Match), retract(tuple_(Match)) ->
			Found = yes(Match)
		;   assertz(waiting_(ClientId, in_list(TupleList), Output)),
			Found = no
		).

	ts_rd(Tuple, ClientId, Output, Found) :-
		(   tuple_(Tuple) ->
			Found = yes(Tuple)
		;   assertz(waiting_(ClientId, rd(Tuple), Output)),
			Found = no
		).

	ts_rd_noblock(Tuple, Found) :-
		(   tuple_(Tuple) ->
			Found = yes(Tuple)
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
		(   tuple_(Match), \+ Match \= Pattern ->
			true
		;   find_matching_tuple(Patterns, Match)
		).

	find_matching_tuple_rd([Pattern| Patterns], Match) :-
		(   tuple_(Match), \+ Match \= Pattern ->
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
			\+ Tuple \= Pattern ->
			Request = in(Pattern)
		;   waiting_(ClientId, in_list(Patterns), Output),
			member(Pattern, Patterns),
			\+ Tuple \= Pattern ->
			Request = in_list(Patterns)
		;   fail
		),
		% Wake this one waiter
		retract(waiting_(ClientId, Request, Output)),
		write_out(Output, result(Tuple)).

	% Wake all rd/1 and rd_list/1 waiters matching the tuple
	wake_all_rd_waiters(Tuple) :-
		forall(
			(   waiting_(ClientId, Request, Output),
				is_rd_request(Request, Tuple)
			),
			(   retract(waiting_(ClientId, Request, Output)) ->
				write_out(Output, result(Tuple))
			;   true
			)
		).

	% Check if a request is a rd/1 or rd_list/1 that matches the tuple
	is_rd_request(rd(Pattern), Tuple) :-
		\+ Tuple \= Pattern.
	is_rd_request(rd_list(Patterns), Tuple) :-
		member(Pattern, Patterns),
		\+ Tuple \= Pattern,
		!.

	:- if(current_logtalk_flag(prolog_dialect, xvm)).
		% streams are thread-owned in XVM

		write_out(Output, Term) :-
			adopt_stream(Output),
			write_canonical(Output, Term),
			write(Output, '.\n'),
			flush_output(Output).

		read_in(Input, Term) :-
			adopt_stream(Input),
			read_term(Input, Term, []).

		adopt_stream(Stream) :-
			{adopt_stream(Stream)}.
	:- else.

		write_out(Output, Term) :-
			write_canonical(Output, Term),
			write(Output, '.\n'),
			flush_output(Output).

		read_in(Input, Term) :-
			read_term(Input, Term, []).

		adopt_stream(_).

	:- endif.

:- end_category.
