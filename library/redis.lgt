%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 0.1,
		author is 'Paulo Moura',
		date is 2017/04/24,
		comment is 'Redis client. Inspired by Sean Charles GNU Prolog Redis client.',
		remarks is [
			'Command representation' - 'Use the Redis command name as the functor of a compound term where the arguments are the command arguments.',
			'Valid arguments' - 'Atoms, integers, and floats. Always use atoms instead of double-quoted "strings". This helps portability by not depending on the value of the double_quotes flag.'
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

	:- uses(list, [length/2]).

	connect(Connection) :-
		catch(
			connect_to_server(Connection, localhost, 6379),
			Error,
			error_handler(Error, connect(Connection))
		).

	connect(Connection, Host, Port) :-
		catch(
			connect_to_server(Connection, Host, Port),
			Error,
			error_handler(Error, connect(Connection, Host, Port))
		).

	disconnect(Connection) :-
		catch(
			disconnect_from_server(Connection),
			Error,
			error_handler(Error, disconnect(Connection))
		).

	send(Connection, Request, Reply) :-
		catch(
			send_request(Connection, Request, Reply),
			Error,
			error_handler(Error, send(Connection, Request, Reply))
		).

	:- if(current_logtalk_flag(prolog_dialect, gnu)).

	connect_to_server(redis(Input, Output, Socket), Host, Port) :-
		socket('AF_INET', Socket),
		socket_connect(Socket, 'AF_INET'(Host, Port), Input, Output),
		set_stream_type(Input, binary),
		set_stream_type(Output, binary).

	disconnect_from_server(redis(_, _, Socket)) :-
		socket_close(Socket).

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

	connect_to_server(redis(Stream, Stream, _), Host, Port) :-
		sockets:socket_client_open(inet(Host,Port), Stream, [type(binary), eof_action(eof)]).

	disconnect_from_server(redis(Stream, _, _)) :-
		close(Stream).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

	connect_to_server(redis(Input, Output, Socket), Host, Port) :-
		socket:tcp_socket(Socket),
		socket:tcp_connect(Socket, Host:Port, Stream),
		stream_pair(Stream, Input, Output),
		set_stream(Stream, type(binary)).

	disconnect_from_server(redis(_, _, Socket)) :-
		socket:tcp_close_socket(Socket).

	:- endif.

	send_request(redis(Input, Output, _), Request, Reply) :-
		parse_request(Request, Bytes),
		send_request(Bytes, Output),
		parse_reply(Input, Reply).

	error_handler(Error, Message) :-
		self(Self),
		sender(Sender),
		throw(error(Error, logtalk(Self::Message, Sender))).

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
		phrase(parse_mbulk(Length, Input), Out), !,
		(	Out == [nil] ->
			MBulk = nil
		;	MBulk = Out
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

:- end_object.
