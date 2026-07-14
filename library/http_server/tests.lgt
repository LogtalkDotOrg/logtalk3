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
		date is 2026-07-10,
		comment is 'Unit tests for the "http_server" library.'
	]).

	:- uses(http_core, [
		body/2, status/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(http_server).

	test(http_server_open_close_3_01, deterministic) :-
		http_server::open(Port, Server, []),
		integer(Port),
		Port > 0,
		http_server::server_property(Server, scheme(http)),
		http_server::server_property(Server, transport(http_socket_transport)),
		http_server::close(Server).

	test(http_server_open_3_02, error(consistency_error(http_server_options, scheme(https), transport(http_socket_transport)))) :-
		http_server::open(_Port, _Server, [scheme(https), transport(http_socket_transport)]).

	test(http_server_open_3_03, error(consistency_error(http_server_options, scheme(http), listener_transport(tls)))) :-
		http_server::open(_Port, _Server, [listener_transport(tls)]).

	test(http_server_open_close_3_04, deterministic, [condition(tls_listener_available)]) :-
		Prefix = 'logtalk_http_server_test_',
		http_process_transport::temporary_tls_credentials_files(Prefix, CertificateFile, KeyFile),
		delete_file_if_exists(CertificateFile),
		delete_file_if_exists(KeyFile),
		http_server::open(Port, Server, [scheme(https), temporary_tls_credentials(Prefix)]),
		(	catch(
				(	integer(Port),
					Port > 0,
					http_server::server_property(Server, scheme(https)),
					http_server::server_property(Server, transport(http_process_transport)),
					os::file_exists(CertificateFile),
					os::file_exists(KeyFile)
				),
				Error,
				(	catch(http_server::close(Server), _, true),
					throw(Error)
				)
			) ->
			http_server::close(Server)
		; 	catch(http_server::close(Server), _, true),
			fail
		),
		\+ os::file_exists(CertificateFile),
		\+ os::file_exists(KeyFile).

	test(http_server_open_close_4_01, deterministic) :-
		http_server::open('127.0.0.1', Port, Server, [transport(default), backlog(2), type(binary), listener_options([]), serve_options([]), control(test_control)]),
		integer(Port),
		Port > 0,
		http_server::server_property(Server, host('127.0.0.1')),
		http_server::server_property(Server, port(Port)),
		http_server::server_property(Server, listener(_)),
		http_server::close(Server).

	test(http_server_open_4_02, error(domain_error(option, workers(foo)))) :-
		http_server::open('127.0.0.1', _Port, _Server, [workers(foo)]).

	test(http_server_open_4_03, error(domain_error(option, shutdown(wait)))) :-
		http_server::open('127.0.0.1', _Port, _Server, [shutdown(wait)]).

	test(http_server_serve_5_02, error(domain_error(http_server_serve_option, scheme(http)))) :-
		http_server::open(_Port, Server, []),
		( 	catch(
				http_server::serve(Server, target_http_server_handler, 1, _ClientInfos, [scheme(http)]),
				Error,
				(catch(http_server::close(Server), _, true), throw(Error))
			) ->
			http_server::close(Server)
		; 	catch(http_server::close(Server), _, true),
			fail
		).

	test(http_server_serve_websocket_once_5_01, error(domain_error(http_server, invalid_server))) :-
		http_server::serve_websocket_once(invalid_server, target_http_server_handler, _Connection, _Response, _ClientInfo).

	test(http_server_serve_4_02, error(domain_error(option, scheme(ws)))) :-
		http_server::serve(0, target_http_server_handler, 0, [workers(per_connection), scheme(ws)]).

	test(http_server_serve_5_03, error(domain_error(option, scheme(ws)))) :-
		http_server::serve(0, target_http_server_handler, 0, _ClientInfos, [workers(pool(1)), scheme(ws)]).

	test(http_server_serve_6_01, error(domain_error(option, scheme(ws)))) :-
		http_server::serve('127.0.0.1', 0, target_http_server_handler, 0, _ClientInfos, [workers(pool(2, rolling)), scheme(ws)]).

	test(http_server_serve_6_02, deterministic) :-
		http_server::serve('127.0.0.1', 0, target_http_server_handler, 0, [], [transport(probe_http_server_transport)]).

	test(http_server_serve_until_shutdown_5_01, error(domain_error(option, scheme(ws)))) :-
		http_server::serve_until_shutdown('127.0.0.1', 0, target_http_server_handler, test_control, [scheme(ws)]).

	test(http_server_serve_until_shutdown_6_01, error(domain_error(option, scheme(ws)))) :-
		http_server::serve_until_shutdown('127.0.0.1', 0, target_http_server_handler, test_control, [scheme(ws)], true).

	test(http_server_open_close_4_04, deterministic, [condition(tls_listener_available)]) :-
		Prefix = 'logtalk_http_server_explicit_tls_test_',
		http_process_transport::temporary_tls_credentials(Prefix, CertificateFile, KeyFile),
		( 	catch(
				http_server::open(Port, Server, [scheme(https), listener_helper_executable(ncat), tls_certificate_file(CertificateFile), tls_key_file(KeyFile)]),
				Error,
				(delete_file_if_exists(CertificateFile), delete_file_if_exists(KeyFile), throw(Error))
			) ->
			integer(Port),
			Port > 0,
			http_server::close(Server)
		; 	delete_file_if_exists(CertificateFile),
			delete_file_if_exists(KeyFile),
			fail
		),
		delete_file_if_exists(CertificateFile),
		delete_file_if_exists(KeyFile).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_server_serve_4_01, deterministic) :-
			RequestPath = '/serve-four-open',
			http_server::open(Port, Server, []),
			local_http_url(Port, RequestPath, URL),
			threaded_once(http_client::get(URL, Response, []), Tag),
			( 	catch(
					http_server::serve(Server, target_http_server_handler, 1, [ClientInfo]),
					Error,
					(catch(http_server::close(Server), _, true), throw(Error))
				) ->
				true
			; 	catch(http_server::close(Server), _, true),
				fail
			),
			threaded_exit(http_client::get(URL, Response, []), Tag),
			compound(ClientInfo),
			status(Response, status(200, 'OK')),
			body(Response, content('text/plain', text(RequestPath))).

		test(http_server_serve_5_01, deterministic) :-
			RequestPath = '/serve-five-open',
			http_server::open(Port, Server, []),
			local_http_url(Port, RequestPath, URL),
			threaded_once(http_client::get(URL, Response, []), Tag),
			( 	catch(
					http_server::serve(Server, target_http_server_handler, 1, [ClientInfo], [workers(serial), shutdown(close), serve_options([])]),
					Error,
					(catch(http_server::close(Server), _, true), throw(Error))
				) ->
				true
			; 	catch(http_server::close(Server), _, true),
				fail
			),
			threaded_exit(http_client::get(URL, Response, []), Tag),
			compound(ClientInfo),
			status(Response, status(200, 'OK')),
			body(Response, content('text/plain', text(RequestPath))).

		test(http_server_serve_once_3_01, deterministic) :-
			RequestPath = '/ping',
			http_server::open(Port, Server, []),
			local_http_url(Port, RequestPath, URL),
			threaded_once(http_client::get(URL, Response, []), Tag),
			(	catch(
					http_server::serve_once(Server, target_http_server_handler, ClientInfo),
					Error,
					(catch(http_server::close(Server), _, true), throw(Error))
				) ->
				http_server::close(Server)
			;	catch(http_server::close(Server), _, true),
				fail
			),
			threaded_exit(http_client::get(URL, Response, []), Tag),
			compound(ClientInfo),
			status(Response, status(200, 'OK')),
			body(Response, content('text/plain', text(RequestPath))).

		test(http_server_start_stop_4_01, deterministic) :-
			http_server::start(Port, target_http_server_handler, Server, []),
			http_server::server_property(Server, port(Port)),
			local_http_url(Port, '/ready', URL),
			(	catch(
					http_client::get(URL, Response, []),
					Error,
					(catch(http_server::stop(Server), _, true), throw(Error))
				) ->
				http_server::stop(Server)
			;	catch(http_server::stop(Server), _, true),
				fail
			),
			status(Response, status(200, 'OK')),
			body(Response, content('text/plain', text('/ready'))).

		test(http_server_start_stop_5_01, deterministic) :-
			http_server::start('127.0.0.1', Port, target_http_server_handler, Server, [control(start_stop_test_control)]),
			http_server::server_property(Server, port(Port)),
			http_server::stop(Server).

	:- endif.

	% auxiliary predicates

	local_http_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

	tls_listener_available :-
		executable_available(ncat),
		executable_available(openssl).

	delete_file_if_exists(File) :-
		catch(os::delete_file(File), _, true).

	executable_available(Executable) :-
		os::resolve_command_path(Executable, _).

:- end_object.
