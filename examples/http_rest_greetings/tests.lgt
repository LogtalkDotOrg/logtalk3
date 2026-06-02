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
		date is 2026-06-02,
		comment is 'Unit tests for the "http_rest" example.'
	]).

	:- uses(http_core, [
		body/2, header/3, request/7, status/2
	]).

	cover(greetings_rest_api(_)).
	cover(greetings_rest_server).
	cover(greetings_rest_client).
	cover(http_rest_greetings_demo).

	test(http_rest_document_01, deterministic) :-
		request(get, origin('/openapi.json'), http(1, 1), [], empty, [], Request),
		greetings_rest_api(8080)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json(Document))),
		open_api::validate_document(Document),
		open_api::operation(greetings_rest_api(8080), create_greeting, operation(create_greeting, post, '/greetings', _Summary, _Parameters, _RequestBody, _Responses, _Properties)).

	test(http_rest_handler_01, deterministic) :-
		request(post, origin('/greetings'), http(1, 1), [], content('application/json', json({name-'Ada'})), [], Request),
		open_api::validate_request(greetings_rest_api(8080), create_greeting, Request),
		greetings_rest_api(8080)::handle(Request, Response),
		status(Response, status(201, 'Created')),
		header(Response, location, '/greetings/Ada'),
		body(Response, content('application/json', json({message-'Hello, Ada!'}))),
		open_api::validate_response(greetings_rest_api(8080), create_greeting, Response).

	test(http_rest_handler_02, deterministic) :-
		request(post, origin('/greetings'), http(1, 1), [], content('text/plain', text('Ada')), [], Request),
		greetings_rest_api(8080)::handle(Request, Response),
		status(Response, status(415, 'Unsupported Media Type')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:unsupported-request-media-type', title-'Unsupported Media Type', detail-'Request media type is not supported by the OpenAPI contract.', status-415}))),
		open_api::validate_response(greetings_rest_api(8080), create_greeting, Response).

	test(http_rest_handler_03, deterministic) :-
		request(post, origin('/greetings'), http(1, 1), [], content('application/json', json({nickname-'Ada'})), [], Request),
		greetings_rest_api(8080)::handle(Request, Response),
		status(Response, status(422, 'Unprocessable Content')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:invalid-request-body', title-'Unprocessable Content', detail-'Request body does not conform to the OpenAPI contract.', status-422}))),
		open_api::validate_response(greetings_rest_api(8080), create_greeting, Response).

	test(http_rest_handler_04, deterministic) :-
		request(get, origin('/greetings/Ada'), http(1, 1), [], empty, [], Request),
		open_api::validate_request(greetings_rest_api(8080), get_greeting, Request),
		greetings_rest_api(8080)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({message-'Hello, Ada!'}))),
		open_api::validate_response(greetings_rest_api(8080), get_greeting, Response).

	test(http_rest_handler_05, deterministic) :-
		request(delete, origin('/greetings/Ada'), http(1, 1), [], empty, [], Request),
		greetings_rest_api(8080)::handle(Request, Response),
		status(Response, status(204, 'No Content')),
		body(Response, empty),
		open_api::validate_response(greetings_rest_api(8080), delete_greeting, Response).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_rest_demo_01, deterministic) :-
			http_rest_greetings_demo::run(result(Document, CreateResponse, LookupResponse, DeleteResponse)),
			open_api::validate_document(Document),
			status(CreateResponse, status(201, 'Created')),
			header(CreateResponse, location, '/greetings/Ada'),
			body(CreateResponse, content('application/json', json({message-'Hello, Ada!'}))),
			status(LookupResponse, status(200, 'OK')),
			body(LookupResponse, content('application/json', json({message-'Hello, Ada!'}))),
			status(DeleteResponse, status(204, 'No Content')),
			body(DeleteResponse, empty).

	:- endif.

:- end_object.
