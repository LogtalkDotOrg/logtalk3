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


:- object(smtp_process_transport).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-10,
		comment is 'Process-backed TLS transport support for the SMTP client.'
	]).

	:- public(open/8).
	:- mode(open(+atom, +integer, +atom, +atom, +atom, +atom, +list, --compound), one_or_error).
	:- info(open/8, [
		comment is 'Opens an implicit TLS or STARTTLS SMTP connection using OpenSSL.',
		argnames is ['Host', 'Port', 'Security', 'Helo', 'Executable', 'ServerName', 'Options', 'Connection']
	]).

	:- public(streams/3).
	:- mode(streams(+compound, --stream, --stream), one_or_error).
	:- info(streams/3, [
		comment is 'Returns the input and output streams for an open process connection.',
		argnames is ['Connection', 'Input', 'Output']
	]).

	:- public(failure_details/3).
	:- mode(failure_details(+compound, --term, --atom), one).
	:- info(failure_details/3, [
		comment is 'Returns the process exit status and standard error output after an OpenSSL connection failure.',
		argnames is ['Connection', 'Status', 'Diagnostic']
	]).

	:- public(close/1).
	:- mode(close(+compound), one).
	:- info(close/1, [
		comment is 'Closes the streams and terminates the OpenSSL process.',
		argnames is ['Connection']
	]).

	:- uses(list, [
		append/3, member/2
	]).

	:- uses(os, [
		resolve_command_path/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	open(Host, Port, Security, Helo, Executable, ServerName, Options, Connection) :-
		resolve_command_path(Executable, ExecutablePath),
		connection_arguments(Host, Port, Security, Helo, ServerName, Options, Arguments),
		process::create(ExecutablePath, Arguments, [stdin(Output), stdout(Input), stderr(Error), process(Process), type(binary)]),
		Connection = smtp_process_connection(Input, Output, Error, Process),
		catch(
			setup_streams(Input, Output, Error),
			SetupError,
			(	close(Connection),
				throw(SetupError)
			)
		).

	streams(smtp_process_connection(Input, Output, _Error, _Process), Input, Output) :-
		catch(stream_property(Input, _), _, fail),
		catch(stream_property(Output, _), _, fail),
		!.
	streams(Connection, _Input, _Output) :-
		domain_error(smtp_process_connection, Connection).

	failure_details(smtp_process_connection(_Input, _Output, Error, Process), Status, Diagnostic) :-
		read_error_stream(Error, Bytes),
		wait_process_status(Process, Status),
		atom_codes(Diagnostic, Bytes).

	close(smtp_process_connection(Input, Output, Error, Process)) :-
		close_stream(Output),
		close_stream(Input),
		close_stream(Error),
		kill_process(Process),
		wait_process(Process).

	connection_arguments(Host, Port, Security, Helo, ServerName, Options, Arguments) :-
		connect_argument(Host, Port, ConnectArgument),
		Base = ['s_client', '-quiet', '-no_ign_eof', '-nocommands', '-connect', ConnectArgument],
		add_server_name(ServerName, Base, Arguments0),
		add_verification(Host, ServerName, Options, Arguments0, Arguments1),
		add_ca_file(Options, Arguments1, Arguments2),
		add_starttls(Security, Helo, Arguments2, Arguments3),
		option_arguments(Options, ExtraArguments),
		append(Arguments3, ExtraArguments, Arguments).

	connect_argument(Host, Port, ConnectArgument) :-
		number_codes(Port, PortCodes),
		atom_codes(PortAtom, PortCodes),
		(	sub_atom(Host, _, _, _, ':') ->
			atomic_list_concat(['[', Host, ']:', PortAtom], ConnectArgument)
		;	atomic_list_concat([Host, ':', PortAtom], ConnectArgument)
		).

	add_server_name(none, Arguments, Arguments) :-
		!.
	add_server_name(default, Arguments, Arguments) :-
		!.
	add_server_name(ServerName, Arguments0, Arguments) :-
		append(Arguments0, ['-servername', ServerName], Arguments).

	add_verification(_Host, _ServerName, Options, Arguments, Arguments) :-
		member(verify_peer(false), Options),
		!.
	add_verification(Host, ServerName, _Options, Arguments0, Arguments) :-
		verification_name(Host, ServerName, Name),
		(	ip_address(Name) ->
			append(Arguments0, ['-verify_return_error', '-verify_ip', Name], Arguments)
		;	append(Arguments0, ['-verify_return_error', '-verify_hostname', Name], Arguments)
		).

	verification_name(Host, default, Host) :-
		!.
	verification_name(Host, none, Host) :-
		!.
	verification_name(_Host, ServerName, ServerName).

	ip_address(Host) :-
		sub_atom(Host, _, _, _, ':'),
		!.
	ip_address(Host) :-
		atom_codes(Host, [Code| Codes]),
		ip_address_codes([Code| Codes]).

	ip_address_codes([]).
	ip_address_codes([Code| Codes]) :-
		once((Code == 0'.; Code >= 0'0, Code =< 0'9)),
		ip_address_codes(Codes).

	add_ca_file(Options, Arguments0, Arguments) :-
		(	member(ca_file(File), Options) ->
			append(Arguments0, ['-CAfile', File], Arguments)
		;	Arguments = Arguments0
		).

	add_starttls(starttls, Helo, Arguments0, Arguments) :-
		!,
		append(Arguments0, ['-starttls', smtp, '-name', Helo], Arguments).
	add_starttls(tls, _Helo, Arguments, Arguments).

	option_arguments(Options, Arguments) :-
		(	member(openssl_arguments(Arguments0), Options) ->
			Arguments = Arguments0
		;	Arguments = []
		).

	:- if(current_logtalk_flag(prolog_dialect, eclipse)).

		setup_streams(Input, Output, Error) :-
			{set_stream_property(Input, encoding, octet)},
			{set_stream_property(Output, encoding, octet)},
			{set_stream_property(Error, encoding, octet)}.

	:- elif(current_logtalk_flag(prolog_dialect, gnu)).

		setup_streams(Input, Output, Error) :-
			{set_stream_type(Input, binary)},
			{set_stream_type(Output, binary)},
			{set_stream_type(Error, binary)}.

	:- elif(current_logtalk_flag(prolog_dialect, sicstus)).

		setup_streams(_Input, _Output, _Error).

	:- elif(current_logtalk_flag(prolog_dialect, swi)).

		setup_streams(Input, Output, Error) :-
			{set_stream(Input, type(binary))},
			{set_stream(Output, type(binary))},
			{set_stream(Error, type(binary))}.

	:- elif(current_logtalk_flag(prolog_dialect, trealla)).

		setup_streams(Input, Output, Error) :-
			{set_stream(Input, type(binary))},
			{set_stream(Output, type(binary))},
			{set_stream(Error, type(binary))}.

	:- elif(current_logtalk_flag(prolog_dialect, xvm)).

		setup_streams(Input, Output, Error) :-
			{set_stream_type(Input, binary)},
			{set_stream_type(Output, binary)},
			{set_stream_type(Error, binary)}.

	:- endif.

	close_stream(Stream) :-
		catch(close(Stream), _, true),
		!.
	close_stream(_Stream).

	kill_process(Process) :-
		catch(process::kill(Process, sigterm), _, true),
		!.
	kill_process(_Process).

	wait_process(Process) :-
		catch(process::wait(Process, _Status), _, true),
		!.
	wait_process(_Process).

	read_error_stream(Error, Bytes) :-
		catch(reader::stream_to_bytes(Error, Bytes), _, fail),
		!.
	read_error_stream(_Error, []).

	wait_process_status(Process, Status) :-
		catch(process::wait(Process, Status), _, fail),
		!.
	wait_process_status(_Process, unknown).

:- end_object.
