%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:4:0,
		author is 'Paulo Moura',
		date is 2020-05-27,
		comment is 'Redis client. Inspired by Sean Charles GNU Prolog Redis client.',
		remarks is [
			'Command representation' - 'Use the Redis command name as the functor of a compound term where the arguments are the command arguments.',
			'Valid arguments' - 'Atoms, integers, and floats. Always use atoms instead of double-quoted "strings". This helps portability by not depending on the value of the ``double_quotes`` flag.'
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
		comment is 'Sends a request to the a Redis server and returns its reply',
		argnames is ['Connection', 'Request', 'Reply']
	]).

	:- public(console/1).
	:- mode(console(++callable), one).
	:- info(console/1, [
		comment is 'Sends a request to a Redis server running on localhost at the default 6379 port and prints the reply.',
		argnames is ['Request']
	]).

	:- uses(logtalk, [print_message/3]).
	:- uses(list, [length/2]).

	% public predicates

	connect(Connection) :-
		context(Context),
		catch(
			connect_to_server(localhost, 6379, Connection),
			Error,
			throw(error(Error, Context))
		).

	connect(Host, Port, Connection) :-
		context(Context),
		catch(
			connect_to_server(Host, Port, Connection),
			Error,
			throw(error(Error, Context))
		).

	disconnect(Connection) :-
		context(Context),
		catch(
			disconnect_from_server(Connection),
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

	% backend Prolog compiler dependent auxiliary predicates
	% (there is not standard sockets Prolog library)

	:- if(current_logtalk_flag(prolog_dialect, ciao)).

	connect_to_server(Host, Port, redis(Stream, Stream, _)) :-
		sockets:connect_to_socket(Host, Port, Stream).

	disconnect_from_server(redis(Stream, _, _)) :-
		sockets:socket_shutdown(Stream, read_write).

	:- elif(current_logtalk_flag(prolog_dialect, eclipse)).

	connect_to_server(Host, Port, redis(Stream, Stream, _)) :-
		socket(internet, stream, Stream),
		connect(Stream, Host/Port),
		set_stream_property(Stream, encoding, octet).

	disconnect_from_server(redis(Stream, _, _)) :-
		close(Stream).

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

	connect_to_server(Host, Port, redis(Input, Output, Socket)) :-
		socket('AF_INET', Socket),
		socket_connect(Socket, 'AF_INET'(Host, Port), Input, Output),
		set_stream_type(Input, binary),
		set_stream_type(Output, binary).

	disconnect_from_server(redis(_, _, Socket)) :-
		socket_close(Socket).

	:- elif(current_logtalk_flag(prolog_dialect, qp)).

	connect_to_server(Host0, Port, redis(Input, Output, Socket)) :-
		(	Host0 == localhost ->
			Host = '127.0.0.1'
		;	Host = Host0
		),
		tcp_client(Port, Host, Socket),
		open_socket_stream(Socket, read, Input),
		open_socket_stream(Socket, write, Output).

	disconnect_from_server(redis(_, _, Socket)) :-
		tcp_close(Socket).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	connect_to_server(Host, Port, redis(Stream, Stream, _)) :-
		sockets:socket_client_open(inet(Host,Port), Stream, [type(binary), eof_action(eof)]).

	disconnect_from_server(redis(Stream, _, _)) :-
		close(Stream).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

	connect_to_server(Host, Port, redis(Input, Output, Socket)) :-
		socket:tcp_socket(Socket),
		socket:tcp_connect(Socket, Host:Port, Stream),
		stream_pair(Stream, Input, Output),
		set_stream(Stream, type(binary)).

	disconnect_from_server(redis(_, _, Socket)) :-
		socket:tcp_close_socket(Socket).

	:- elif(current_logtalk_flag(prolog_dialect, xsb)).

	connect_to_server(Host, Port, redis(Socket, Socket, Socket)) :-
		{socket(Socket, _),
		 socket_connect(Socket, Port, Host, _)}.

	disconnect_from_server(redis(_, _, Socket)) :-
		{socket_close(Socket, _)}.

	put_byte(Socket, Byte) :-
		{socket_put(Socket, Byte, _)}.

	get_byte(Socket, Byte) :-
		{socket_get0(Socket, Byte0, _), Byte0 = Byte}.

	flush_output(_).

	:- endif.

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
		[42| Codes], [13, 10],
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
		[36| Codes], [13, 10| Bytes], [13, 10].

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
