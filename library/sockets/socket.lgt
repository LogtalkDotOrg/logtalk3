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


:- object(socket).

	:- info([
		version is 0:5:0,
		author is 'Paulo Moura',
		date is 2026-02-07,
		comment is 'Portable abstraction over TCP sockets. Provides a high-level API for client and server socket operations that works with selected backend Prolog systems.',
		remarks is [
			'Supported backends' - 'ECLiPSe, GNU Prolog, SICStus Prolog, and SWI-Prolog.',
			'Design rationale' - 'Some backends (notably SICStus Prolog) do not provide low-level socket creation predicates that can be separated from binding or connecting. This library therefore provides a higher-level API with ``client_open/5`` and ``server_open/3`` that abstracts over these differences.',
			'Stream handling' - 'Predicates ``client_open/5`` and ``server_accept/5`` return separate input and output streams opened in binary mode. For backends where the same stream is used for bidirectional communication, the same stream handle is returned in both arguments. Use standard stream predicates (``put_byte/2``, ``get_byte/2``, ``read/2``, ``write/2``, etc.) to communicate.',
			'Options' - 'The ``Options`` argument is currently reserved for future use and should be passed as an empty list ``[]``.'
		]
	]).

	% Client predicates

	:- public(client_open/5).
	:- mode(client_open(+atom, +integer, --stream, --stream, +list), one_or_error).
	:- info(client_open/5, [
		comment is 'Opens a client connection to the specified host and port. Returns separate input and output streams for bidirectional communication. For backends where the same stream is used for bidirectional communication, the same stream handle is returned in both arguments. The streams are opened in binary mode.',
		argnames is ['Host', 'Port', 'InputStream', 'OutputStream', 'Options'],
		exceptions is [
			'Connection refused or host not found' - 'socket_error(Error)'
		]
	]).

	% Server predicates

	:- public(server_open/3).
	:- mode(server_open(?integer, --compound, +list), one_or_error).
	:- info(server_open/3, [
		comment is 'Opens a server socket bound to the specified port. If ``Port`` is a variable, binds to an available port and unifies ``Port`` with the port number. Returns a ``ServerSocket`` handle to use with ``server_accept/4``. The default backlog (queue length) for pending connections is 5.',
		argnames is ['Port', 'ServerSocket', 'Options'],
		exceptions is [
			'Port already in use' - 'socket_error(Error)'
		]
	]).

	:- public(server_accept/5).
	:- mode(server_accept(+compound, --stream, --stream, --compound, +list), one_or_error).
	:- info(server_accept/5, [
		comment is 'Accepts an incoming connection on the server socket, blocking until a client connects. Returns separate input and output streams for bidirectional communication and client information as ``client(Host, Port)`` or ``client(Address)`` depending on backend. For backends where the same stream is used for bidirectional communication, the same stream handle is returned in both arguments. The streams are opened in binary mode.',
		argnames is ['ServerSocket', 'InputStream', 'OutputStream', 'ClientInfo', 'Options'],
		exceptions is [
			'Invalid server socket' - 'socket_error(Error)'
		]
	]).

	:- public(server_close/1).
	:- mode(server_close(+compound), one_or_error).
	:- info(server_close/1, [
		comment is 'Closes a server socket.',
		argnames is ['ServerSocket']
	]).

	% Common predicates

	:- public(close/2).
	:- mode(close(+stream, +stream), one_or_error).
	:- info(close/2, [
		comment is 'Closes a client or accepted connection by closing both the input and output streams. If the same stream is used for both, it is closed only once.',
		argnames is ['InputStream', 'OutputStream']
	]).

	:- public(current_host/1).
	:- mode(current_host(-atom), one_or_error).
	:- info(current_host/1, [
		comment is 'Returns the hostname of the current machine.',
		argnames is ['Host']
	]).

	% Backend Prolog compiler dependent implementations

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

	% ECLiPSe: socket/2 for creation, then bind, listen, etc.

	client_open(Host, Port, Stream, Stream, _Options) :-
		context(Context),
		catch(
			(	{socket(internet, stream, Socket)},
				{connect(Socket, Host/Port)},
				set_stream_property(Socket, encoding, octet),
				Stream = Socket
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_open(Port, server_socket(Socket, Port), _Options) :-
		context(Context),
		catch(
			(	{socket(internet, stream, Socket)},
				{bind(Socket, _Host/Port)},
				{listen(Socket, 5)}
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_accept(server_socket(Socket, _), ClientSocket, ClientSocket, client(Host, Port), _Options) :-
		context(Context),
		catch(
			(	{accept(Socket, Host/Port, ClientSocket)},
				set_stream_property(ClientSocket, encoding, octet)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_close(server_socket(Socket, _)) :-
		context(Context),
		catch(
			close(Socket, Socket),
			Error,
			throw(error(socket_error(Error), Context))
		).

	close(Input, Output) :-
		context(Context),
		catch(
			(	Input == Output ->
				close(Input)
			;	close(Input),
				close(Output)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	current_host(Host) :-
		context(Context),
		catch(
			(	get_flag(hostname, HostString),
				atom_string(Host, HostString)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

	% GNU Prolog: socket/2, socket_connect/4, socket_bind/2, socket_listen/2, socket_accept/4

	client_open(Host, Port, Input, Output, _Options) :-
		context(Context),
		catch(
			(	socket('AF_INET', Socket),
				socket_connect(Socket, 'AF_INET'(Host, Port), Input, Output),
				set_stream_type(Input, binary),
				set_stream_type(Output, binary)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_open(Port, server_socket(Socket, Port), _Options) :-
		context(Context),
		catch(
			(	socket('AF_INET', Socket),
				socket_bind(Socket, 'AF_INET'('', Port)),
				socket_listen(Socket, 5)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_accept(server_socket(Socket, _), Input, Output, client(Host, Port), _Options) :-
		context(Context),
		catch(
			(	socket_accept(Socket, 'AF_INET'(Host, Port), Input, Output),
				set_stream_type(Input, binary),
				set_stream_type(Output, binary)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_close(server_socket(Socket, _)) :-
		context(Context),
		catch(
			socket_close(Socket),
			Error,
			throw(error(socket_error(Error), Context))
		).

	close(Input, Output) :-
		context(Context),
		catch(
			(	Input == Output ->
				close(Input)
			;	close(Input),
				close(Output)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	current_host(Host) :-
		context(Context),
		catch(
			host_name(Host),
			Error,
			throw(error(socket_error(Error), Context))
		).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	% SICStus Prolog: higher-level API with socket_client_open/3 and socket_server_open/[2,3]

	client_open(Host, Port, Stream, Stream, _Options) :-
		context(Context),
		catch(
			sockets:socket_client_open(inet(Host, Port), Stream, [type(binary)]),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_open(Port, server_socket(ServerSocket, PortInt), _Options) :-
		context(Context),
		catch(
			(	% Use internal variable for socket_server_open
				(	var(Port) ->
					sockets:socket_server_open(Port0, ServerSocket, [reuseaddr(true)])
				;	Port0 = Port,
					sockets:socket_server_open(Port0, ServerSocket, [reuseaddr(true)])
				),
				% SICStus may return the port as an atom, convert to integer
				(	atom(Port0) ->
					{atom_codes(Port0, Codes), number_codes(PortInt, Codes)}
				;	PortInt = Port0
				),
				% Unify Port with the integer value if it was a variable
				(	var(Port) ->
					Port = PortInt
				;	true
				)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_accept(server_socket(ServerSocket, _), Stream, Stream, client(Client), _Options) :-
		context(Context),
		catch(
			sockets:socket_server_accept(ServerSocket, Client, Stream, [type(binary)]),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_close(server_socket(ServerSocket, _)) :-
		context(Context),
		catch(
			sockets:socket_server_close(ServerSocket),
			Error,
			throw(error(socket_error(Error), Context))
		).

	close(Input, Output) :-
		context(Context),
		catch(
			(	Input == Output ->
				close(Input)
			;	close(Input),
				close(Output)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	current_host(Host) :-
		context(Context),
		catch(
			sockets:current_host(Host),
			Error,
			throw(error(socket_error(Error), Context))
		).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

	% SWI-Prolog: tcp_socket/1, tcp_connect/2, tcp_bind/2, tcp_listen/2, tcp_accept/3

	client_open(Host, Port, Input, Output, _Options) :-
		context(Context),
		catch(
			(	socket:tcp_socket(Socket),
				socket:tcp_connect(Socket, Host:Port, StreamPair),
				stream_pair(StreamPair, Input, Output),
				set_stream(Input, type(binary)),
				set_stream(Output, type(binary))
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_open(Port, server_socket(Socket, Port), _Options) :-
		context(Context),
		catch(
			(	socket:tcp_socket(Socket),
				socket:tcp_setopt(Socket, reuseaddr),
				socket:tcp_bind(Socket, Port),
				socket:tcp_listen(Socket, 5)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_accept(server_socket(Socket, _), Input, Output, client(Host, Port), _Options) :-
		context(Context),
		catch(
			(	socket:tcp_accept(Socket, ClientSocket, Peer),
				socket:tcp_open_socket(ClientSocket, Input, Output),
				set_stream(Input, type(binary)),
				set_stream(Output, type(binary)),
				peer_to_host_port(Peer, Host, Port)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	peer_to_host_port(ip(A, B, C, D):Port, Host, Port) :-
		!,
		format(atom(Host), '~w.~w.~w.~w', [A, B, C, D]).
	peer_to_host_port(Host:Port, Host, Port) :-
		atom(Host),
		!.
	peer_to_host_port(_, unknown, 0).

	server_close(server_socket(Socket, _)) :-
		context(Context),
		catch(
			socket:tcp_close_socket(Socket),
			Error,
			throw(error(socket_error(Error), Context))
		).

	close(Input, Output) :-
		context(Context),
		catch(
			(	Input == Output ->
				close(Input)
			;	close(Input),
				close(Output)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	current_host(Host) :-
		context(Context),
		catch(
			socket:gethostname(Host),
			Error,
			throw(error(socket_error(Error), Context))
		).

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

	% Trealla Prolog: higher-level API with socket_client_open/3 and socket_server_open/[2,3]

	client_open(Host, Port, Stream, Stream, _Options) :-
		context(Context),
		catch(
			sockets:socket_client_open(inet(Host, Port), Stream, [type(binary)]),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_open(Port, server_socket(ServerSocket, PortInt), _Options) :-
		context(Context),
		catch(
			(	% Use internal variable for socket_server_open
				(	var(Port) ->
					sockets:socket_server_open(Port0, ServerSocket, [reuseaddr(true)])
				;	Port0 = Port,
					sockets:socket_server_open(Port0, ServerSocket, [reuseaddr(true)])
				),
				% SICStus may return the port as an atom, convert to integer
				(	atom(Port0) ->
					{atom_codes(Port0, Codes), number_codes(PortInt, Codes)}
				;	PortInt = Port0
				),
				% Unify Port with the integer value if it was a variable
				(	var(Port) ->
					Port = PortInt
				;	true
				)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_accept(server_socket(ServerSocket, _), Stream, Stream, client(Client), _Options) :-
		context(Context),
		catch(
			sockets:socket_server_accept(ServerSocket, Client, Stream, [type(binary)]),
			Error,
			throw(error(socket_error(Error), Context))
		).

	server_close(server_socket(ServerSocket, _)) :-
		context(Context),
		catch(
			sockets:socket_server_close(ServerSocket),
			Error,
			throw(error(socket_error(Error), Context))
		).

	close(Input, Output) :-
		context(Context),
		catch(
			(	Input == Output ->
				close(Input)
			;	close(Input),
				close(Output)
			),
			Error,
			throw(error(socket_error(Error), Context))
		).

	current_host(Host) :-
		context(Context),
		catch(
			sockets:current_host(Host),
			Error,
			throw(error(socket_error(Error), Context))
		).

	:- endif.

:- end_object.
