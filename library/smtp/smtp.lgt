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


:- object(smtp,
	implements(smtp_protocol),
	imports(options)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-09-10,
		comment is 'Portable SMTP/ESMTP client.'
	]).

	:- uses(list, [
		append/3, member/2, reverse/2, valid/1 as proper_list/1
	]).

	:- uses(reader, [
		line_to_bytes/2
	]).

	:- uses(type, [
		valid/2, check/3
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	send(Host, Port, Message, Result, Options) :-
		context(Context),
		^^check_options(Options),
		check_one_shot_options(Options),
		^^merge_options(Options, MergedOptions),
		check(atom, Host, Context),
		check(positive_integer, Port, Context),
		validate_message(Message, Context),
		connect_(Host, Port, Connection, MergedOptions, Context),
		catch(
			send_(Connection, Message, Result, MergedOptions, Context),
			Error,
			(	disconnect_(Connection),
				throw(Error)
			)
		),
		disconnect_(Connection).

	connect(Host, Port, Connection, Options) :-
		context(Context),
		^^check_options(Options),
		check_connection_options(Options),
		^^merge_options(Options, MergedOptions),
		check(atom, Host, Context),
		check(positive_integer, Port, Context),
		connect_(Host, Port, Connection, MergedOptions, Context).

	disconnect(Connection) :-
		(	valid_connection(Connection, _Input, _Output) ->
			disconnect_(Connection)
		;	domain_error(smtp_connection, Connection)
		).

	connection_alive(smtp_connection(Input, _Output, _Host, _Port, _Features, socket)) :-
		catch(stream_property(Input, _), _, fail).
	connection_alive(smtp_connection(_Input, _Output, _Host, _Port, _Features, process(ProcessConnection))) :-
		catch(smtp_process_transport::streams(ProcessConnection, _ProcessInput, _ProcessOutput), _, fail).

	send(Connection, Message, Result, Options) :-
		context(Context),
		^^check_options(Options),
		check_transaction_options(Options),
		^^merge_options(Options, MergedOptions),
		validate_message(Message, Context),
		send_(Connection, Message, Result, MergedOptions, Context).

	connect_(Host, Port, Connection, Options, Context) :-
		^^option(security(Security), Options),
		(	Security == plain ->
			connect_plain(Host, Port, Connection, Options, Context)
		;	connect_secure(Host, Port, Security, Connection, Options, Context)
		).

	connect_plain(Host, Port, Connection, Options, Context) :-
		catch(
			socket::client_open(Host, Port, Input, Output, [type(binary)]),
			_,
			throw(error(smtp_error(connection_failed), Context))
		),
		catch(
			negotiate_session(Input, Output, Host, Options, Features, Context),
			Error,
			(	catch(socket::close(Input, Output), _, true),
				throw(Error)
			)
		),
		Connection = smtp_connection(Input, Output, Host, Port, Features, socket).

	connect_secure(Host, Port, Security, Connection, Options, Context) :-
		helo_name(Host, Options, Helo),
		^^option(openssl_executable(Executable), Options),
		^^option(server_name(ServerName), Options),
		catch(
			smtp_process_transport::open(Host, Port, Security, Helo, Executable, ServerName, Options, ProcessConnection),
			_,
			throw(error(smtp_error(connection_failed), Context))
		),
		smtp_process_transport::streams(ProcessConnection, Input, Output),
		catch(
			negotiate_secure_session(Security, Input, Output, Helo, Options, Features, Context),
			Error,
			(	smtp_process_transport::close(ProcessConnection),
				throw(Error)
			)
		),
		Connection = smtp_connection(Input, Output, Host, Port, Features, process(ProcessConnection)).

	negotiate_secure_session(tls, Input, Output, Helo, Options, Features, Context) :-
		read_response(Input, Greeting, Context),
		expect_code(Greeting, [220], greeting_failed, Context),
		negotiate_ehlo(Input, Output, Helo, Options, tls, Features, Context).
	negotiate_secure_session(starttls, Input, Output, Helo, Options, Features, Context) :-
		negotiate_ehlo(Input, Output, Helo, Options, starttls, Features, Context).

	negotiate_session(Input, Output, Host, Options, Features, Context) :-
		read_response(Input, Greeting, Context),
		expect_code(Greeting, [220], greeting_failed, Context),
		helo_name(Host, Options, Helo),
		negotiate_ehlo(Input, Output, Helo, Options, plain, Features, Context).

	helo_name(Host, Options, Helo) :-
		(	^^option(helo(Helo0), Options) ->
			Helo = Helo0
		;	catch(socket::current_host(Helo), _, Helo = Host)
		).

	negotiate_ehlo(Input, Output, Helo, Options, Security, Features, Context) :-
		send_atom_command(Output, 'EHLO', Helo),
		read_response(Input, EhloResponse, Context),
		(	EhloResponse = smtp_response(250, Features0) ->
			Features = Features0
		;	EhloResponse = smtp_response(Code, _),
			member(Code, [500, 502, 504]) ->
			send_atom_command(Output, 'HELO', Helo),
			read_response(Input, HeloResponse, Context),
			expect_code(HeloResponse, [250], protocol_error, Context),
			Features = []
		;	throw(error(smtp_error(protocol_error(EhloResponse)), Context))
		),
		authenticate_if_requested(Input, Output, Security, Features, Options, Context).

	authenticate_if_requested(Input, Output, Security, Features, Options, Context) :-
		(	^^option(auth(User-Password), Options) ->
			^^option(allow_insecure_auth(Allow), Options),
			(	Security == plain, Allow \== true ->
				throw(error(permission_error(authenticate, insecure_smtp_connection, plain), Context))
			;	select_authentication_mechanism(Features, Mechanism, Context),
				authenticate(Mechanism, Input, Output, User, Password, Context)
			)
		; true
		).

	select_authentication_mechanism(Features, plain, _Context) :-
		authentication_mechanism(Features, 'PLAIN'),
		!.
	select_authentication_mechanism(Features, login, _Context) :-
		authentication_mechanism(Features, 'LOGIN'),
		!.
	select_authentication_mechanism(_Features, _Mechanism, Context) :-
		throw(error(smtp_error(authentication_not_supported), Context)).

	authentication_mechanism([Feature| _], Mechanism) :-
		atom_codes(Feature, Codes),
		uppercase_ascii_codes(Codes, UppercaseCodes),
		authentication_feature_codes(UppercaseCodes, Mechanisms),
		member(Mechanism, Mechanisms),
		!.
	authentication_mechanism([_| Features], Mechanism) :-
		authentication_mechanism(Features, Mechanism).

	authentication_feature_codes([0'A,0'U,0'T,0'H,Separator| Codes], Mechanisms) :-
		member(Separator, [0' , 0'=]),
		!,
		authentication_mechanism_codes(Codes, Mechanisms).

	authentication_mechanism_codes([], []).
	authentication_mechanism_codes(Codes, [Mechanism| Mechanisms]) :-
		drop_spaces(Codes, NonSpaceCodes),
		take_word(NonSpaceCodes, WordCodes, Rest),
		WordCodes = [_| _],
		atom_codes(Mechanism, WordCodes),
		authentication_mechanism_codes(Rest, Mechanisms).

	drop_spaces([0' | Codes], Rest) :-
		!,
		drop_spaces(Codes, Rest).
	drop_spaces(Codes, Codes).

	take_word([], [], []).
	take_word([0' | Codes], [], Codes) :-
		!.
	take_word([Code| Codes], [Code| Word], Rest) :-
		take_word(Codes, Word, Rest).

	uppercase_ascii_codes([], []).
	uppercase_ascii_codes([Code| Codes], [UppercaseCode| UppercaseCodes]) :-
		(	Code >= 0'a, Code =< 0'z ->
			UppercaseCode is Code - 32
		;	UppercaseCode = Code
		),
		uppercase_ascii_codes(Codes, UppercaseCodes).

	authenticate(plain, Input, Output, User, Password, Context) :-
		plain_credentials(User, Password, Encoded),
		send_atom_command(Output, 'AUTH PLAIN', Encoded),
		read_response(Input, Response, Context),
		(	Response = smtp_response(235, _) ->
			true
		;	Response = smtp_response(334, _) ->
			send_command_atom(Output, Encoded),
			read_response(Input, FinalResponse, Context),
			expect_authentication_success(FinalResponse, Context)
		;	throw_authentication_error(Response, Context)
		).
	authenticate(login, Input, Output, User, Password, Context) :-
		send_command(Output, [0'A,0'U,0'T,0'H,0' ,0'L,0'O,0'G,0'I,0'N]),
		read_response(Input, UserChallenge, Context),
		expect_authentication_challenge(UserChallenge, Context),
		atom_codes(User, UserCodes),
		base64::generate(atom(EncodedUser), UserCodes),
		send_command_atom(Output, EncodedUser),
		read_response(Input, PasswordChallenge, Context),
		expect_authentication_challenge(PasswordChallenge, Context),
		atom_codes(Password, PasswordCodes),
		base64::generate(atom(EncodedPassword), PasswordCodes),
		send_command_atom(Output, EncodedPassword),
		read_response(Input, Response, Context),
		expect_authentication_success(Response, Context).

	plain_credentials(User, Password, Encoded) :-
		atom_codes(User, UserCodes),
		atom_codes(Password, PasswordCodes),
		append([0| UserCodes], [0| PasswordCodes], Bytes),
		base64::generate(atom(Encoded), Bytes).

	expect_authentication_challenge(smtp_response(334, _), _Context) :-
		!.
	expect_authentication_challenge(Response, Context) :-
		throw_authentication_error(Response, Context).

	expect_authentication_success(smtp_response(235, _), _Context) :-
		!.
	expect_authentication_success(Response, Context) :-
		throw_authentication_error(Response, Context).

	throw_authentication_error(smtp_response(Code, Lines), Context) :-
		throw(error(smtp_error(auth_failed(Code, Lines)), Context)).

	send_(Connection, smtp_message(From, Recipients0, Headers, Body), Result, Options, Context) :-
		(	valid_connection(Connection, Input, Output) ->
			true
		;	domain_error(smtp_connection, Connection)
		),
		normalize_recipients(Recipients0, Recipients),
		send_envelope_command(Output, 'MAIL FROM:<', From),
		read_response(Input, MailResponse, Context),
		expect_code(MailResponse, [250], sender_rejected, Context),
		send_recipients(Recipients, Input, Output, [], Accepted0, [], Rejected0, Context),
		reverse(Accepted0, Accepted),
		reverse(Rejected0, Rejected),
		^^option(require_all_recipients(RequireAll), Options),
		(	Accepted == [] ->
			reset_transaction(Input, Output, Context),
			Result = smtp_result(not_sent, [], Rejected)
		;	RequireAll == true, Rejected \== [] ->
			reset_transaction(Input, Output, Context),
			Result = smtp_result(not_sent, Accepted, Rejected)
		;	send_command(Output, [0'D,0'A,0'T,0'A]),
			read_response(Input, DataResponse, Context),
			expect_code(DataResponse, [354], send_failed, Context),
			write_message(Output, From, Recipients, Headers, Body, Options),
			read_response(Input, FinalResponse, Context),
			expect_code(FinalResponse, [250], send_failed, Context),
			Result = smtp_result(FinalResponse, Accepted, Rejected)
		).

	send_recipients([], _Input, _Output, Accepted, Accepted, Rejected, Rejected, _Context).
	send_recipients([Recipient| Recipients], Input, Output, Accepted0, Accepted, Rejected0, Rejected, Context) :-
		send_envelope_command(Output, 'RCPT TO:<', Recipient),
		read_response(Input, Response, Context),
		Response = smtp_response(Code, _),
		(	member(Code, [250, 251, 252]) ->
			send_recipients(Recipients, Input, Output, [Recipient| Accepted0], Accepted, Rejected0, Rejected, Context)
		;	send_recipients(Recipients, Input, Output, Accepted0, Accepted, [Recipient-Response| Rejected0], Rejected, Context)
		).

	reset_transaction(Input, Output, Context) :-
		send_command(Output, [0'R,0'S,0'E,0'T]),
		read_response(Input, Response, Context),
		expect_code(Response, [250], protocol_error, Context).

	write_message(Output, From, Recipients, Headers, Body, Options) :-
		write_header(Output, 'From', From),
		atomic_list_concat(Recipients, ', ', To),
		write_header(Output, 'To', To),
		write_headers(Headers, Output),
		write_option_headers(Options, Output),
		write_crlf(Output),
		body_codes(Body, BodyCodes),
		write_body(BodyCodes, Output),
		write_bytes([0'.,0'\r,0'\n], Output),
		flush_output(Output).

	write_headers([], _Output).
	write_headers([Name-Value| Headers], Output) :-
		write_header(Output, Name, Value),
		write_headers(Headers, Output).

	write_option_headers([], _Output).
	write_option_headers([header(Name, Value)| Options], Output) :-
		!,
		write_header(Output, Name, Value),
		write_option_headers(Options, Output).
	write_option_headers([_| Options], Output) :-
		write_option_headers(Options, Output).

	write_header(Output, Name, Value) :-
		atom_codes(Name, NameCodes),
		atom_codes(Value, ValueCodes),
		write_bytes(NameCodes, Output),
		write_bytes([0':,0' ], Output),
		write_bytes(ValueCodes, Output),
		write_crlf(Output).

	write_body([], _Output).
	write_body([Code| Codes], Output) :-
		take_body_line([Code| Codes], Line, Rest),
		(	Line = [0'.| _] ->
			put_byte(Output, 0'.)
		;	true
		),
		write_bytes(Line, Output),
		write_crlf(Output),
		write_body(Rest, Output).

	take_body_line([], [], []).
	take_body_line([0'\r,0'\n| Codes], [], Codes) :-
		!.
	take_body_line([0'\r| Codes], [], Codes) :-
		!.
	take_body_line([0'\n| Codes], [], Codes) :-
		!.
	take_body_line([Code| Codes], [Code| Line], Rest) :-
		take_body_line(Codes, Line, Rest).

	send_atom_command(Output, Command, Argument) :-
		atom_codes(Command, CommandCodes),
		atom_codes(Argument, ArgumentCodes),
		append(CommandCodes, [0' | ArgumentCodes], Codes),
		send_command(Output, Codes).

	send_command_atom(Output, Atom) :-
		atom_codes(Atom, Codes),
		send_command(Output, Codes).

	send_envelope_command(Output, Prefix, Mailbox) :-
		atom_codes(Prefix, PrefixCodes),
		atom_codes(Mailbox, MailboxCodes),
		append(PrefixCodes, MailboxCodes, Codes0),
		append(Codes0, [0'>], Codes),
		send_command(Output, Codes).

	send_command(Output, Codes) :-
		write_bytes(Codes, Output),
		write_crlf(Output),
		flush_output(Output).

	write_crlf(Output) :-
		put_byte(Output, 0'\r),
		put_byte(Output, 0'\n).

	write_bytes([], _Output).
	write_bytes([Byte| Bytes], Output) :-
		put_byte(Output, Byte),
		write_bytes(Bytes, Output).

	read_response(Input, smtp_response(Code, Lines), Context) :-
		line_to_bytes(Input, Line),
		parse_response_line(Line, Code, Separator, Text, Context),
		read_response_lines(Separator, Input, Code, [Text], ReversedLines, Context),
		reverse(ReversedLines, Lines).

	read_response_lines(0' , _Input, _Code, Lines, Lines, _Context).
	read_response_lines(0'-, Input, Code, Lines0, Lines, Context) :-
		line_to_bytes(Input, Line),
		parse_response_line(Line, NextCode, Separator, Text, Context),
		(	NextCode =:= Code ->
			read_response_lines(Separator, Input, Code, [Text| Lines0], Lines, Context)
		;	throw(error(smtp_error(protocol_error(Line)), Context))
		).

	parse_response_line(end_of_file, _Code, _Separator, _Text, Context) :-
		throw(error(smtp_error(connection_closed), Context)).
	parse_response_line([Digit1, Digit2, Digit3, Separator| TextCodes], Code, Separator, Text, _Context) :-
		digit_code(Digit1), digit_code(Digit2), digit_code(Digit3),
		member(Separator, [0' , 0'-]),
		!,
		Code is (Digit1 - 0'0) * 100 + (Digit2 - 0'0) * 10 + Digit3 - 0'0,
		atom_codes(Text, TextCodes).
	parse_response_line(Line, _Code, _Separator, _Text, Context) :-
		throw(error(smtp_error(protocol_error(Line)), Context)).

	digit_code(Code) :-
		Code >= 0'0,
		Code =< 0'9.

	expect_code(Response, Codes, Error, Context) :-
		Response = smtp_response(Code, _),
		(	member(Code, Codes) ->
			true
		;	throw_smtp_response_error(Error, Response, Context)
		).

	throw_smtp_response_error(greeting_failed, smtp_response(Code, Lines), Context) :-
		throw(error(smtp_error(greeting_failed(Code, Lines)), Context)).
	throw_smtp_response_error(sender_rejected, smtp_response(Code, Lines), Context) :-
		throw(error(smtp_error(sender_rejected(Code, Lines)), Context)).
	throw_smtp_response_error(send_failed, smtp_response(Code, Lines), Context) :-
		throw(error(smtp_error(send_failed(Code, Lines)), Context)).
	throw_smtp_response_error(protocol_error, Response, Context) :-
		throw(error(smtp_error(protocol_error(Response)), Context)).

	disconnect_(smtp_connection(Input, Output, _Host, _Port, _Features, socket)) :-
		catch(
			(	send_command(Output, [0'Q,0'U,0'I,0'T]),
				read_response(Input, _Response, _Context)
			),
			_,
			true
		),
		catch(socket::close(Input, Output), _, true).
	disconnect_(smtp_connection(Input, Output, _Host, _Port, _Features, process(ProcessConnection))) :-
		catch(
			(	send_command(Output, [0'Q,0'U,0'I,0'T]),
				read_response(Input, _Response, _Context)
			),
			_,
			true
		),
		smtp_process_transport::close(ProcessConnection).

	valid_connection(smtp_connection(Input, Output, _Host, _Port, _Features, socket), Input, Output) :-
		catch(stream_property(Input, _), _, fail),
		catch(stream_property(Output, _), _, fail).
	valid_connection(smtp_connection(Input, Output, _Host, _Port, _Features, process(ProcessConnection)), Input, Output) :-
		catch(smtp_process_transport::streams(ProcessConnection, Input, Output), _, fail).

	validate_message(smtp_message(From, Recipients, Headers, Body), Context) :-
		!,
		validate_mailbox(From, Context),
		normalize_recipients(Recipients, RecipientList),
		validate_recipients(RecipientList, Context),
		validate_headers(Headers, Context),
		body_codes(Body, BodyCodes),
		validate_body_codes(BodyCodes, Context).
	validate_message(Message, _Context) :-
		domain_error(smtp_message, Message).

	normalize_recipients([], _Normalized) :-
		domain_error(smtp_recipients, []).
	normalize_recipients(Recipient, [Recipient]) :-
		atom(Recipient),
		!.
	normalize_recipients(Recipients, Recipients) :-
		proper_list(Recipients),
		Recipients = [_| _],
		!.
	normalize_recipients(Recipients, _Normalized) :-
		domain_error(smtp_recipients, Recipients).

	validate_recipients([], _Context).
	validate_recipients([Recipient| Recipients], Context) :-
		validate_mailbox(Recipient, Context),
		validate_recipients(Recipients, Context).

	validate_mailbox(Mailbox, Context) :-
		check(atom, Mailbox, Context),
		atom_codes(Mailbox, Codes),
		(	Codes = [_| _], valid_mailbox_codes(Codes) ->
			true
		;	domain_error(smtp_mailbox, Mailbox)
		).

	valid_mailbox_codes([]).
	valid_mailbox_codes([Code| Codes]) :-
		Code >= 33,
		Code =< 126,
		Code =\= 0'<,
		Code =\= 0'>,
		valid_mailbox_codes(Codes).

	validate_headers(Headers, Context) :-
		(	proper_list(Headers) ->
			validate_header_list(Headers, Context)
		;	type_error(list, Headers)
		).

	validate_header_list([], _Context).
	validate_header_list([Name-Value| Headers], Context) :-
		!,
		check(atom, Name, Context),
		check(atom, Value, Context),
		atom_codes(Name, NameCodes),
		atom_codes(Value, ValueCodes),
		(	NameCodes = [_| _],
			valid_header_name_codes(NameCodes),
			valid_header_value_codes(ValueCodes) ->
			validate_header_list(Headers, Context)
		;	domain_error(smtp_header, Name-Value)
		).
	validate_header_list([Header| _], _Context) :-
		domain_error(smtp_header, Header).

	valid_header_name_codes([]).
	valid_header_name_codes([Code| Codes]) :-
		Code >= 33,
		Code =< 126,
		Code =\= 0':,
		valid_header_name_codes(Codes).

	valid_header_value_codes([]).
	valid_header_value_codes([Code| Codes]) :-
		once((Code == 0'\t; Code >= 32, Code =< 126)),
		valid_header_value_codes(Codes).

	body_codes(Body, Codes) :-
		atom(Body),
		!,
		atom_codes(Body, Codes).
	body_codes(Body, Body) :-
		proper_list(Body),
		!.
	body_codes(Body, _Codes) :-
		domain_error(smtp_body, Body).

	validate_body_codes([], _Context).
	validate_body_codes([Code| Codes], Context) :-
		(	integer(Code), Code >= 0, Code =< 127 ->
			validate_body_codes(Codes, Context)
		;	domain_error(smtp_body_code, Code)
		).

	check_one_shot_options([]).
	check_one_shot_options([Option| Options]) :-
		(	connection_option(Option)
		;	transaction_option(Option)
		),
		!,
		check_one_shot_options(Options).
	check_one_shot_options([Option| _]) :-
		domain_error(smtp_send_option, Option).

	check_connection_options([]).
	check_connection_options([Option| Options]) :-
		connection_option(Option),
		!,
		check_connection_options(Options).
	check_connection_options([Option| _]) :-
		domain_error(smtp_connection_option, Option).

	check_transaction_options([]).
	check_transaction_options([Option| Options]) :-
		transaction_option(Option),
		!,
		check_transaction_options(Options).
	check_transaction_options([Option| _]) :-
		domain_error(smtp_transaction_option, Option).

	connection_option(security(_)).
	connection_option(helo(_)).
	connection_option(auth(_)).
	connection_option(allow_insecure_auth(_)).
	connection_option(openssl_executable(_)).
	connection_option(server_name(_)).
	connection_option(verify_peer(_)).
	connection_option(ca_file(_)).
	connection_option(openssl_arguments(_)).

	transaction_option(require_all_recipients(_)).
	transaction_option(header(_, _)).

	valid_option(security(Security)) :-
		once((Security == plain; Security == tls; Security == starttls)).
	valid_option(helo(Name)) :-
		atom(Name).
	valid_option(auth(User-Password)) :-
		atom(User),
		atom(Password).
	valid_option(allow_insecure_auth(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(require_all_recipients(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(header(Name, Value)) :-
		atom(Name),
		atom(Value),
		atom_codes(Name, NameCodes),
		atom_codes(Value, ValueCodes),
		NameCodes = [_| _],
		valid_header_name_codes(NameCodes),
		valid_header_value_codes(ValueCodes).
	valid_option(openssl_executable(Executable)) :-
		atom(Executable).
	valid_option(server_name(ServerName)) :-
		atom(ServerName).
	valid_option(verify_peer(Boolean)) :-
		once((Boolean == true; Boolean == false)).
	valid_option(ca_file(File)) :-
		atom(File).
	valid_option(openssl_arguments(Arguments)) :-
		valid(list(atom), Arguments).

	default_option(security(plain)).
	default_option(allow_insecure_auth(false)).
	default_option(require_all_recipients(false)).
	default_option(openssl_executable(openssl)).
	default_option(server_name(default)).
	default_option(verify_peer(true)).
	default_option(openssl_arguments([])).

:- end_object.
