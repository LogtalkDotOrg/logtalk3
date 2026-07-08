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
		date is 2026-07-08,
		comment is 'Unit tests for the "http_multipart_form" example.'
	]).

	:- uses(http_core, [
		body/2, request/7, status/2
	]).

	:- uses(user, [
		atomic_list_concat/3
	]).

	cover(multipart_form_http_handler).
	cover(multipart_form_server).
	cover(multipart_form_client).
	cover(http_multipart_form_demo).

	test(http_multipart_form_handler_01, deterministic) :-
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		multipart_form_http_handler::handle(Request, Response),
		logtalk_version(LogtalkVersion),
		backend_name(BackendName),
		backend_version(BackendVersion),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, '<form')),
		once(sub_atom(HTML, _, _, _, 'multipart/form-data')),
		once(sub_atom(HTML, _, _, _, 'name="name"')),
		once(sub_atom(HTML, _, _, _, 'name="email"')),
		once(sub_atom(HTML, _, _, _, LogtalkVersion)),
		once(sub_atom(HTML, _, _, _, BackendName)),
		once(sub_atom(HTML, _, _, _, BackendVersion)).

	test(http_multipart_form_handler_02, deterministic) :-
		http_multipart::form_data_body([
			field(name, 'Ada Lovelace', []),
			field(email, 'ada@example.com', [])
		], Body),
		request(post, origin('/submit'), http(1, 1), [], Body, [], Request),
		multipart_form_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'Ada Lovelace')),
		once(sub_atom(HTML, _, _, _, 'ada@example.com')).

	test(http_multipart_form_handler_03, deterministic) :-
		http_multipart::form_data_body([
			field(name, 'Ada Lovelace', [])
		], Body),
		request(post, origin('/submit'), http(1, 1), [], Body, [], Request),
		multipart_form_http_handler::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'Missing multipart form fields')).

	test(http_multipart_form_handler_04, deterministic) :-
		request(post, origin('/submit'), http(1, 1), [], empty, [], Request),
		multipart_form_http_handler::handle(Request, Response),
		status(Response, status(400, 'Bad Request')).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_multipart_form_client_01, deterministic) :-
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(multipart_form_server::serve_listener(Listener, 2), Tag),
			catch(
				multipart_form_client::run(Port, 'Ada Lovelace', 'ada@example.com', result(FormResponse, SubmitResponse)),
				Error,
				( 	cleanup_server_thread(Listener, Tag),
					throw(Error)
				)
			),
			http_socket_transport::request_listener_shutdown(Listener),
			threaded_exit(multipart_form_server::serve_listener(Listener, 2), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			status(FormResponse, status(200, 'OK')),
			body(FormResponse, content('text/html', text(FormHTML))),
			once(sub_atom(FormHTML, _, _, _, 'Contact form')),
			status(SubmitResponse, status(200, 'OK')),
			body(SubmitResponse, content('text/html', text(SubmitHTML))),
			once(sub_atom(SubmitHTML, _, _, _, 'Ada Lovelace')),
			once(sub_atom(SubmitHTML, _, _, _, 'ada@example.com')).

		test(http_multipart_form_demo_01, deterministic) :-
			http_multipart_form_demo::run(result(FormResponse, SubmitResponse)),
			status(FormResponse, status(200, 'OK')),
			status(SubmitResponse, status(200, 'OK')).

		cleanup_server_thread(Listener, Tag) :-
			http_socket_transport::request_listener_shutdown(Listener),
			catch(threaded_exit(multipart_form_server::serve_listener(Listener, 2), Tag), _, true),
			catch(http_socket_transport::close_listener(Listener), _, true).

	:- endif.

	logtalk_version(Version) :-
		current_logtalk_flag(version_data, logtalk(Major, Minor, Patch, Status)),
		atomic_list_concat([Major, Minor, Patch], '.', Prefix),
		( 	Status == stable ->
			Version = Prefix
		; 	atomic_list_concat([Prefix, Status], '-', Version)
		).

	backend_name(Name) :-
		current_logtalk_flag(prolog_dialect, Backend),
		backend(Backend, Name).

	backend_version(Version) :-
		current_logtalk_flag(prolog_version, v(Major, Minor, Patch)),
		atomic_list_concat([Major, Minor, Patch], '.', Version).

	backend(logtalk, 'Logtalk').
	backend(b, 'B-Prolog').
	backend(ciao, 'Ciao Prolog').
	backend(cx, 'CxProlog').
	backend(eclipse, 'ECLiPSe').
	backend(gnu, 'GNU Prolog').
	backend(ji, 'JIProlog').
	backend(quintus, 'Quintus Prolog').
	backend(sicstus, 'SICStus Prolog').
	backend(swi, 'SWI-Prolog').
	backend(tau, 'Tau Prolog').
	backend(trealla, 'Trealla Prolog').
	backend(xsb, 'XSB').
	backend(xvm, 'XVM').
	backend(yap, 'YAP').

:- end_object.
