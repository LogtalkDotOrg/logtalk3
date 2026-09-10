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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-10,
		comment is 'Unit tests for the "smtp" library.'
	]).

	cover(smtp).

	test(smtp_default_options_01, true(Defaults == [security(plain), allow_insecure_auth(false), require_all_recipients(false), openssl_executable(openssl), server_name(default), verify_peer(true), openssl_arguments([])])) :-
		smtp::default_options(Defaults).

	test(smtp_invalid_option_01, ball(error(domain_error(option, bogus(value)), _))) :-
		smtp::send(localhost, 25, smtp_message('a@example.com', 'b@example.com', [], ''), _Result, [bogus(value)]).

	test(smtp_connection_option_partition_01, ball(error(domain_error(smtp_connection_option, require_all_recipients(true)), _))) :-
		smtp::connect(localhost, 25, _Connection, [require_all_recipients(true)]).

	test(smtp_valid_options_01, true) :-
		smtp::valid_option(security(tls)),
		smtp::valid_option(helo(localhost)),
		smtp::valid_option(auth(user-password)),
		smtp::valid_option(allow_insecure_auth(true)),
		smtp::valid_option(require_all_recipients(true)),
		smtp::valid_option(header('X-Test', value)),
		smtp::valid_option(openssl_executable(openssl)),
		smtp::valid_option(server_name('smtp.example.com')),
		smtp::valid_option(verify_peer(false)),
		smtp::valid_option(ca_file('/tmp/ca.pem')),
		smtp::valid_option(openssl_arguments(['-brief'])).

	test(smtp_invalid_recipients_01, ball(error(domain_error(smtp_recipients, []), _))) :-
		smtp::send(localhost, 25, smtp_message('a@example.com', [], [], ''), _Result, []).

	test(smtp_invalid_recipients_02, ball(error(domain_error(smtp_recipients, recipients(term)), _))) :-
		smtp::send(localhost, 25, smtp_message('a@example.com', recipients(term), [], ''), _Result, []).

	test(smtp_invalid_body_01, ball(error(domain_error(smtp_body, body(term)), _))) :-
		smtp::send(localhost, 25, smtp_message('a@example.com', 'b@example.com', [], body(term)), _Result, []).

	test(smtp_invalid_body_code_01, ball(error(domain_error(smtp_body_code, 128), _))) :-
		smtp::send(localhost, 25, smtp_message('a@example.com', ['b@example.com'], [], [128]), _Result, []).

	test(smtp_invalid_header_01, ball(error(domain_error(smtp_header, malformed), _))) :-
		smtp::send(localhost, 25, smtp_message('a@example.com', 'b@example.com', [malformed], ''), _Result, []).

	test(smtp_invalid_message_01, ball(error(domain_error(smtp_message, malformed), _))) :-
		smtp::send(localhost, 25, malformed, _Result, []).

	test(smtp_connection_options_01, ball(error(type_error(atom, 42), _))) :-
		smtp::send(42, 25, smtp_message('a@example.com', 'b@example.com', [], ''), _Result, [
			security(tls), openssl_executable(openssl), server_name(none), verify_peer(false),
			ca_file('/tmp/ca.pem'), openssl_arguments(['-brief'])
		]).

	test(smtp_transaction_option_partition_01, ball(error(domain_error(smtp_transaction_option, helo(localhost)), _))) :-
		smtp::send(not_a_connection, smtp_message('a@example.com', 'b@example.com', [], ''), _Result, [helo(localhost)]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(smtp_send_plain_01, true((Result == smtp_result(smtp_response(250, ['queued']), ['bob@example.com'], []), Transcript == transcript('EHLO client.test', 'MAIL FROM:<alice@example.com>', 'RCPT TO:<bob@example.com>', 'DATA', ['From: alice@example.com', 'To: bob@example.com', 'Subject: Test', '', 'Hello', 'CR', '..leading dot'], 'QUIT')))) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_smtp_server(Listener, Transcript), Tag),
			smtp::send(
				'127.0.0.1', Port,
				smtp_message('alice@example.com', 'bob@example.com', ['Subject'-'Test'], 'Hello\r\nCR\r.leading dot'),
				Result,
				[helo('client.test')]
			),
			threaded_exit(mock_smtp_server(Listener, Transcript), Tag),
			socket::server_close(Listener).

		test(smtp_auth_plain_01, true(Auth == 'AUTH PLAIN AHVzZXIAc2VjcmV0')) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_auth_server(Listener, Auth), Tag),
			smtp::connect('127.0.0.1', Port, Connection, [
				helo('client.test'), auth(user-secret), allow_insecure_auth(true)
			]),
			smtp::disconnect(Connection),
			threaded_exit(mock_auth_server(Listener, Auth), Tag),
			socket::server_close(Listener).

		test(smtp_helo_fallback_01, true((Ehlo == 'EHLO client.test', Helo == 'HELO client.test'))) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_helo_server(Listener, Ehlo, Helo), Tag),
			smtp::connect('127.0.0.1', Port, Connection, [helo('client.test')]),
			smtp::disconnect(Connection),
			threaded_exit(mock_helo_server(Listener, Ehlo, Helo), Tag),
			socket::server_close(Listener).

		test(smtp_connection_reuse_01, true((Result == smtp_result(smtp_response(250, ['queued']), ['bob@example.com'], []), Alive == true, Closed == true, Data == ['From: alice@example.com', 'To: bob@example.com', 'X-Test: value', '']))) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_reuse_server(Listener, Data), Tag),
			smtp::connect('127.0.0.1', Port, Connection, [helo('client.test')]),
			( smtp::connection_alive(Connection) -> Alive = true; Alive = false ),
			smtp::send(Connection, smtp_message('alice@example.com', ['bob@example.com'], [], []), Result, [header('X-Test', value)]),
			smtp::disconnect(Connection),
			( smtp::connection_alive(Connection) -> Closed = false; Closed = true ),
			threaded_exit(mock_reuse_server(Listener, Data), Tag),
			socket::server_close(Listener).

		test(smtp_require_all_recipients_01, true(Result == smtp_result(not_sent, ['good@example.com'], ['bad@example.com'-smtp_response(550, ['rejected'])]))) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_rejection_server(Listener), Tag),
			smtp::send(
				'127.0.0.1', Port,
				smtp_message('alice@example.com', ['good@example.com', 'bad@example.com'], [], ''),
				Result,
				[helo('client.test'), require_all_recipients(true)]
			),
			threaded_exit(mock_rejection_server(Listener), Tag),
			socket::server_close(Listener).

		test(smtp_auth_login_01, true(Transcript == auth_login('AUTH LOGIN', 'dXNlcg==', c2VjcmV0))) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_auth_login_server(Listener, Transcript), Tag),
			smtp::connect('127.0.0.1', Port, Connection, [
				helo('client.test'), auth(user-secret), allow_insecure_auth(true)
			]),
			smtp::disconnect(Connection),
			threaded_exit(mock_auth_login_server(Listener, Transcript), Tag),
			socket::server_close(Listener).

		test(smtp_auth_plain_challenge_01, true((Initial == 'AUTH PLAIN AHVzZXIAc2VjcmV0', Response == 'AHVzZXIAc2VjcmV0'))) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(mock_auth_plain_challenge_server(Listener, Initial, Response), Tag),
			smtp::connect('127.0.0.1', Port, Connection, [
				helo('client.test'), auth(user-secret), allow_insecure_auth(true)
			]),
			smtp::disconnect(Connection),
			threaded_exit(mock_auth_plain_challenge_server(Listener, Initial, Response), Tag),
			socket::server_close(Listener).

		test(smtp_auth_unsupported_01, true) :-
			run_connect_error(mock_auth_unsupported_server, [auth(user-secret), allow_insecure_auth(true)], Error),
			Error = error(smtp_error(authentication_not_supported), _).

		test(smtp_auth_failed_01, true) :-
			run_connect_error(mock_auth_failed_server, [auth(user-secret), allow_insecure_auth(true)], Error),
			Error = error(smtp_error(auth_failed(535, ['denied'])), _).

		test(smtp_auth_login_challenge_failed_01, true) :-
			run_connect_error(mock_auth_login_challenge_failed_server, [auth(user-secret), allow_insecure_auth(true)], Error),
			Error = error(smtp_error(auth_failed(535, ['denied'])), _).

		test(smtp_auth_login_response_failed_01, true) :-
			run_connect_error(mock_auth_login_response_failed_server, [auth(user-secret), allow_insecure_auth(true)], Error),
			Error = error(smtp_error(auth_failed(535, ['denied'])), _).

		test(smtp_malformed_response_01, true) :-
			run_connect_error(mock_malformed_response_server, [], Error),
			atom_codes(malformed, Line),
			Error = error(smtp_error(protocol_error(Line)), _).

		test(smtp_closed_connection_01, true) :-
			run_connect_error(mock_closed_connection_server, [], Error),
			Error = error(smtp_error(connection_closed), _).

		test(smtp_greeting_failed_01, true) :-
			run_connect_error(mock_greeting_failed_server, [], Error),
			Error = error(smtp_error(greeting_failed(554, ['unavailable'])), _).

		test(smtp_sender_rejected_01, true) :-
			run_send_error(mock_sender_rejected_server, Error),
			Error = error(smtp_error(sender_rejected(550, ['rejected'])), _).

		test(smtp_data_rejected_01, true) :-
			run_send_error(mock_data_rejected_server, Error),
			Error = error(smtp_error(send_failed(554, ['rejected'])), _).

		test(smtp_helo_rejected_01, true) :-
			run_connect_error(mock_helo_rejected_server, [], Error),
			Error = error(smtp_error(protocol_error(smtp_response(550, ['rejected']))), _).

		run_connect_error(Server, Options, Error) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(call(Server, Listener), Tag),
			catch(smtp::connect('127.0.0.1', Port, _Connection, [helo('client.test')| Options]), Error, true),
			threaded_exit(call(Server, Listener), Tag),
			socket::server_close(Listener).

		run_send_error(Server, Error) :-
			socket::server_open('127.0.0.1', Port, Listener, [type(binary)]),
			threaded_once(call(Server, Listener), Tag),
			catch(smtp::send('127.0.0.1', Port, smtp_message('a@example.com', 'b@example.com', [], ''), _Result, [helo('client.test')]), Error, true),
			threaded_exit(call(Server, Listener), Tag),
			socket::server_close(Listener).

		mock_smtp_server(Listener, transcript(Ehlo, Mail, Recipient, DataCommand, Data, Quit)) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, Ehlo),
			write_line(Output, '250-mock.example'),
			write_line(Output, '250 SIZE 1000000'),
			read_line(Input, Mail),
			write_line(Output, '250 sender accepted'),
			read_line(Input, Recipient),
			write_line(Output, '250 recipient accepted'),
			read_line(Input, DataCommand),
			write_line(Output, '354 send data'),
			read_data(Input, Data),
			write_line(Output, '250 queued'),
			read_line(Input, Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_auth_server(Listener, Auth) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, _Ehlo),
			write_line(Output, '250-mock.example'),
			write_line(Output, '250-AUTH PLAIN LOGIN'),
			write_line(Output, '250 SIZE 1000000'),
			read_line(Input, Auth),
			write_line(Output, '235 authenticated'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_helo_server(Listener, Ehlo, Helo) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, Ehlo),
			write_line(Output, '502 EHLO not supported'),
			read_line(Input, Helo),
			write_line(Output, '250 mock.example'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_reuse_server(Listener, Data) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, _Ehlo),
			write_line(Output, '250 mock.example'),
			read_line(Input, _Mail),
			write_line(Output, '250 sender accepted'),
			read_line(Input, _Recipient),
			write_line(Output, '250 recipient accepted'),
			read_line(Input, _DataCommand),
			write_line(Output, '354 send data'),
			read_data(Input, Data),
			write_line(Output, '250 queued'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_rejection_server(Listener) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, _Ehlo),
			write_line(Output, '250 mock.example'),
			read_line(Input, _Mail),
			write_line(Output, '250 sender accepted'),
			read_line(Input, _GoodRecipient),
			write_line(Output, '250 recipient accepted'),
			read_line(Input, _BadRecipient),
			write_line(Output, '550 rejected'),
			read_line(Input, 'RSET'),
			write_line(Output, '250 reset'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_auth_login_server(Listener, auth_login(Command, User, Password)) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, _Ehlo),
			write_line(Output, '250-mock.example'),
			write_line(Output, '250 AUTH  LOGIN'),
			read_line(Input, Command),
			write_line(Output, '334 VXNlcm5hbWU6'),
			read_line(Input, User),
			write_line(Output, '334 UGFzc3dvcmQ6'),
			read_line(Input, Password),
			write_line(Output, '235 authenticated'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_auth_plain_challenge_server(Listener, Initial, Response) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, _Ehlo),
			write_line(Output, '250-mock.example'),
			write_line(Output, '250 AUTH PLAIN'),
			read_line(Input, Initial),
			write_line(Output, '334 continue'),
			read_line(Input, Response),
			write_line(Output, '235 authenticated'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_auth_unsupported_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '250 SIZE 1000000'),
			socket::close(Input, Output).

		mock_auth_failed_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '250 AUTH PLAIN'),
			read_line(Input, _Auth),
			write_line(Output, '535 denied'),
			socket::close(Input, Output).

		mock_auth_login_challenge_failed_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '250 AUTH LOGIN'),
			read_line(Input, _Auth),
			write_line(Output, '535 denied'),
			socket::close(Input, Output).

		mock_auth_login_response_failed_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '250 AUTH LOGIN'),
			read_line(Input, _Auth),
			write_line(Output, '334 VXNlcm5hbWU6'),
			read_line(Input, _User),
			write_line(Output, '334 UGFzc3dvcmQ6'),
			read_line(Input, _Password),
			write_line(Output, '535 denied'),
			socket::close(Input, Output).

		mock_malformed_response_server(Listener) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, malformed),
			socket::close(Input, Output).

		mock_closed_connection_server(Listener) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			socket::close(Input, Output).

		mock_greeting_failed_server(Listener) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '554 unavailable'),
			socket::close(Input, Output).

		mock_sender_rejected_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '250 mock.example'),
			read_line(Input, _Mail),
			write_line(Output, '550 rejected'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_data_rejected_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '250 mock.example'),
			read_line(Input, _Mail),
			write_line(Output, '250 sender accepted'),
			read_line(Input, _Recipient),
			write_line(Output, '250 recipient accepted'),
			read_line(Input, _Data),
			write_line(Output, '554 rejected'),
			read_line(Input, _Quit),
			write_line(Output, '221 bye'),
			socket::close(Input, Output).

		mock_helo_rejected_server(Listener) :-
			open_mock_session(Listener, Input, Output),
			write_line(Output, '502 EHLO not supported'),
			read_line(Input, _Helo),
			write_line(Output, '550 rejected'),
			socket::close(Input, Output).

		open_mock_session(Listener, Input, Output) :-
			socket::server_accept(Listener, Input, Output, _ClientInfo, [type(binary)]),
			write_line(Output, '220 mock.example ESMTP'),
			read_line(Input, _Ehlo).

		read_data(Input, Lines) :-
			read_line(Input, Line),
			(	Line == '.' ->
				Lines = []
			;	Lines = [Line| Rest],
				read_data(Input, Rest)
			).

		read_line(Input, Line) :-
			reader::line_to_bytes(Input, Codes),
			atom_codes(Line, Codes).

		write_line(Output, Line) :-
			atom_codes(Line, Codes),
			write_bytes(Output, Codes),
			put_byte(Output, 0'\r),
			put_byte(Output, 0'\n),
			flush_output(Output).

		write_bytes(_Output, []).
		write_bytes(Output, [Byte| Bytes]) :-
			put_byte(Output, Byte),
			write_bytes(Output, Bytes).

	:- endif.

:- end_object.
