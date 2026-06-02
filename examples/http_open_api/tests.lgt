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
		date is 2026-05-23,
		comment is 'Unit tests for the "http_open_api" example.'
	]).

	:- uses(http_core, [body/2, request/7, status/2]).

	cover(greetings_api(_)).
	cover(greetings_http_handler(_)).
	cover(greetings_server).
	cover(open_ended_greetings_server).
	cover(greetings_client).
	cover(managed_greetings_client).
	cover(http_open_api_demo).

	test(http_open_api_document_01, deterministic) :-
		open_api::document(greetings_api(8080), Document),
		open_api::validate_document(Document),
		open_api::operation(greetings_api(8080), create_greeting, operation(create_greeting, post, '/greetings', 'Create greeting', _Parameters, _RequestBody, _Responses, _Properties)).

	test(http_open_api_document_02, deterministic) :-
		open_api::operation(greetings_api(8080), get_greeting, operation(get_greeting, get, '/greetings/{name}', 'Get greeting', _Parameters, _RequestBody, _Responses, _Properties)).

	test(http_open_api_handler_01, deterministic) :-
		request(post, origin('/greetings'), http(1, 1), [host-host('example.com')], content('application/json', json({name-'Ada'})), [], Request),
		greetings_http_handler(8080)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({message-'Hello, Ada!'}))),
		open_api::validate_response(greetings_api(8080), create_greeting, Response).

	test(http_open_api_handler_02, deterministic) :-
		request(get, origin('/greetings/Ada'), http(1, 1), [host-host('example.com')], empty, [], Request),
		open_api::validate_request(greetings_api(8080), get_greeting, Request),
		greetings_http_handler(8080)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({message-'Hello, Ada!'}))),
		open_api::validate_response(greetings_api(8080), get_greeting, Response).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_open_api_open_ended_server_01, deterministic) :-
			Control = http_open_api_open_ended_server_01,
			open_ended_greetings_server::start(Port, Control, Tag),
			greetings_client::run(Port, 'Ada', result(Document, CreateResponse, LookupResponse)),
			open_ended_greetings_server::stop(Control, Tag),
			open_api::validate_document(Document),
			status(CreateResponse, status(200, 'OK')),
			body(CreateResponse, content('application/json', json({message-'Hello, Ada!'}))),
			status(LookupResponse, status(200, 'OK')),
			body(LookupResponse, content('application/json', json({message-'Hello, Ada!'}))).

		test(http_open_api_managed_client_01, deterministic) :-
			managed_greetings_client::run('Ada', result(Document, CreateResponse, LookupResponse)),
			open_api::validate_document(Document),
			status(CreateResponse, status(200, 'OK')),
			body(CreateResponse, content('application/json', json({message-'Hello, Ada!'}))),
			status(LookupResponse, status(200, 'OK')),
			body(LookupResponse, content('application/json', json({message-'Hello, Ada!'}))).

		test(http_open_api_demo_01, deterministic) :-
			http_open_api_demo::run(result(Document, CreateResponse, LookupResponse)),
			open_api::validate_document(Document),
			status(CreateResponse, status(200, 'OK')),
			body(CreateResponse, content('application/json', json({message-'Hello, Ada!'}))),
			status(LookupResponse, status(200, 'OK')),
			body(LookupResponse, content('application/json', json({message-'Hello, Ada!'}))).

	:- endif.

:- end_object.
