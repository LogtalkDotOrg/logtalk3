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


:- object(greetings_rest_api(_Port_),
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'REST API used by the HTTP REST example.',
		parnames is ['Port']
	]).

	:- protected(endpoint/5).
	:- info(endpoint/5, [
		comment is 'Declares the REST endpoints used by the example API.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(open_api_info/1).
	:- info(open_api_info/1, [
		comment is 'Returns the top-level OpenAPI info descriptor for the example API.',
		argnames is ['Info']
	]).

	:- protected(open_api_servers/1).
	:- info(open_api_servers/1, [
		comment is 'Returns the published server descriptor for the example API.',
		argnames is ['Servers']
	]).

	:- protected(open_api_schema/2).
	:- info(open_api_schema/2, [
		comment is 'Returns reusable OpenAPI schemas referenced by the example API operations.',
		argnames is ['Name', 'Schema']
	]).

	:- protected(open_api_validate_request/1).
	:- info(open_api_validate_request/1, [
		comment is 'Enables automatic request validation for selected example endpoints.',
		argnames is ['Id']
	]).

	:- protected(open_api_validate_response/1).
	:- info(open_api_validate_response/1, [
		comment is 'Enables automatic response validation for selected example endpoints.',
		argnames is ['Id']
	]).

	:- protected(create_greeting/2).
	:- info(create_greeting/2, [
		comment is 'Creates a greeting from a JSON request body.',
		argnames is ['Request', 'Result']
	]).

	:- protected(get_greeting/2).
	:- info(get_greeting/2, [
		comment is 'Returns a greeting using the path parameter value.',
		argnames is ['Request', 'Result']
	]).

	:- protected(delete_greeting/2).
	:- info(delete_greeting/2, [
		comment is 'Demonstrates the no-content result variant.',
		argnames is ['Request', 'Result']
	]).

	open_api_info(info('Greetings REST API', '1.0.0', 'Tiny REST API used by the HTTP REST example.', [description('Shows endpoint descriptors, decoded body helpers, automatic OpenAPI derivation, and optional contract validation.')])).

	open_api_servers([server(URL, 'Local demo server')]) :-
		base_url(URL).

	open_api_schema(greeting_request, {
		type-object,
		properties-{
			name-{type-string, minLength-1}
		},
		required-[name],
		additionalProperties- @false
	}).

	open_api_schema(greeting_response, {
		type-object,
		properties-{
			message-{type-string, minLength-1}
		},
		required-[message],
		additionalProperties- @false
	}).

	open_api_schema(problem_details, {
		type-object,
		properties-{
			type-{type-string},
			title-{type-string},
			detail-{type-string},
			status-{type-integer}
		},
		required-[type, title, detail, status],
		additionalProperties- @false
	}).

	handle(Request, Response) :-
		http_core::method(Request, get),
		http_core::target(Request, origin('/openapi.json')),
		!,
		self(Self),
		open_api::document(Self, Document),
		::json_response(Request, 200, Document, Response).
	handle(Request, Response) :-
		^^handle(Request, Response).

	endpoint(create_greeting, post, '/greetings', create_greeting, [
		summary('Create greeting'),
		description('Creates a greeting from the supplied JSON request body.'),
		tags([greetings]),
		request_body(request_body('Greeting input', true, [media('application/json', schema_ref(greeting_request))])),
		responses([
			response(201, 'Greeting created', [media('application/json', schema_ref(greeting_response))]),
			response(400, 'Invalid request', [media('application/problem+json', schema_ref(problem_details))]),
			response(415, 'Unsupported media type', [media('application/problem+json', schema_ref(problem_details))]),
			response(422, 'Invalid request body', [media('application/problem+json', schema_ref(problem_details))])
		]),
		produces(['application/json'])
	]).

	endpoint(get_greeting, get, '/greetings/{name}', get_greeting, [
		summary('Get greeting'),
		description('Returns a greeting for the supplied path parameter value.'),
		tags([greetings]),
		responses([
			response(200, 'Greeting generated', [media('application/json', schema_ref(greeting_response))])
		]),
		produces(['application/json'])
	]).

	endpoint(delete_greeting, delete, '/greetings/{name}', delete_greeting, [
		summary('Delete greeting'),
		description('Demonstrates a ``204 No Content`` REST endpoint.'),
		tags([greetings]),
		responses([
			response(204, 'Greeting deleted', [])
		])
	]).

	open_api_validate_request(create_greeting).

	open_api_validate_response(create_greeting).
	open_api_validate_response(get_greeting).
	open_api_validate_response(delete_greeting).

	create_greeting(Request, created(Location, {message-Message})) :-
		::json_body(Request, {name-Name}),
		greeting_text(Name, Message),
		greeting_path(Name, Location).

	get_greeting(Request, ok({message-Message})) :-
		::path_parameter(Request, name, Name),
		greeting_text(Name, Message).

	delete_greeting(Request, no_content) :-
		::path_parameter(Request, name, _Name).

	greeting_text(Name, Message) :-
		atom_concat('Hello, ', Name, Prefix),
		atom_concat(Prefix, '!', Message).

	greeting_path(Name, Path) :-
		atom_concat('/greetings/', Name, Path).

	base_url(URL) :-
		number_codes(_Port_, PortCodes),
		atom_codes(Port, PortCodes),
		atom_concat('http://127.0.0.1:', Port, URL).

:- end_object.


:- object(greetings_rest_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Small local HTTP server used by the HTTP REST example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener and serves the requested number of accepted connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	serve(Port, Count) :-
		http_server::serve('127.0.0.1', Port, greetings_rest_api(Port), Count, _ClientInfos, []).

:- end_object.


:- object(greetings_rest_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'HTTP client used by the HTTP REST example.'
	]).

	:- public(fetch_document/2).
	:- mode(fetch_document(+integer, -compound), one_or_error).
	:- info(fetch_document/2, [
		comment is 'Fetches the published OpenAPI document and validates it.',
		argnames is ['Port', 'Document']
	]).

	:- public(create_greeting/3).
	:- mode(create_greeting(+integer, +atom, --compound), one_or_error).
	:- info(create_greeting/3, [
		comment is 'Calls the POST greeting endpoint and validates the returned response.',
		argnames is ['Port', 'Name', 'Response']
	]).

	:- public(get_greeting/3).
	:- mode(get_greeting(+integer, +atom, --compound), one_or_error).
	:- info(get_greeting/3, [
		comment is 'Calls the GET greeting endpoint and validates the returned response.',
		argnames is ['Port', 'Name', 'Response']
	]).

	:- public(delete_greeting/3).
	:- mode(delete_greeting(+integer, +atom, --compound), one_or_error).
	:- info(delete_greeting/3, [
		comment is 'Calls the DELETE greeting endpoint and validates the returned response.',
		argnames is ['Port', 'Name', 'Response']
	]).

	:- public(run/3).
	:- mode(run(+integer, +atom, -compound), one_or_error).
	:- info(run/3, [
		comment is 'Fetches the OpenAPI document and then exercises the POST, GET, and DELETE greeting endpoints.',
		argnames is ['Port', 'Name', 'Result']
	]).

	fetch_document(Port, Document) :-
		open_api_url(Port, URL),
		http_client::get(URL, Response, []),
		http_core::status(Response, status(200, 'OK')),
		http_core::body(Response, content('application/json', json(Document))),
		open_api::validate_document(Document).

	create_greeting(Port, Name, Response) :-
		operation_url(Port, create_greeting, post, URL),
		RequestBody = content('application/json', json({name-Name})),
		http_client::post(URL, RequestBody, Response, []),
		open_api::validate_response(greetings_rest_api(Port), create_greeting, Response).

	get_greeting(Port, Name, Response) :-
		path_operation_url(Port, get_greeting, get, Name, URL),
		http_client::get(URL, Response, []),
		open_api::validate_response(greetings_rest_api(Port), get_greeting, Response).

	delete_greeting(Port, Name, Response) :-
		path_operation_url(Port, delete_greeting, delete, Name, URL),
		http_client::delete(URL, Response, []),
		open_api::validate_response(greetings_rest_api(Port), delete_greeting, Response).

	run(Port, Name, result(Document, CreateResponse, LookupResponse, DeleteResponse)) :-
		fetch_document(Port, Document),
		open_api::document(greetings_rest_api(Port), Document),
		create_greeting(Port, Name, CreateResponse),
		get_greeting(Port, Name, LookupResponse),
		delete_greeting(Port, Name, DeleteResponse).

	open_api_url(Port, URL) :-
		greetings_rest_api(Port)::servers([server(BaseURL, _Description)]),
		atom_concat(BaseURL, '/openapi.json', URL).

	operation_url(Port, OperationId, Method, URL) :-
		open_api::operation(greetings_rest_api(Port), OperationId, operation(OperationId, Method, Path, _Summary, _Parameters, _RequestBody, _Responses, _Properties)),
		greetings_rest_api(Port)::servers([server(BaseURL, _Description)]),
		atom_concat(BaseURL, Path, URL).

	path_operation_url(Port, OperationId, Method, Name, URL) :-
		open_api::operation(greetings_rest_api(Port), OperationId, operation(OperationId, Method, '/greetings/{name}', _Summary, _Parameters, _RequestBody, _Responses, _Properties)),
		greetings_rest_api(Port)::servers([server(BaseURL, _Description)]),
		atom_concat('/greetings/', Name, Path),
		atom_concat(BaseURL, Path, URL).

:- end_object.


:- object(http_rest_greetings_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-12,
		comment is 'Self-contained demo object for the HTTP REST example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns the fetched document plus the POST, GET, and DELETE responses when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		run(Result) :-
			http_server::start(Port, greetings_rest_api(Port), Server, []),
			catch(
				greetings_rest_client::run(Port, 'Ada', Result),
				Error,
				( cleanup_demo(Server),
					throw(Error)
				)
			),
			cleanup_demo(Server).

		cleanup_demo(Server) :-
			catch(http_server::stop(Server), _, true).

		print_result(result(_Document, CreateResponse, LookupResponse, DeleteResponse)) :-
			http_core::body(CreateResponse, content('application/json', json({message-CreateMessage}))),
			http_core::body(LookupResponse, content('application/json', json({message-LookupMessage}))),
			http_core::status(DeleteResponse, status(204, 'No Content')),
			write('Created greeting: '),
			write(CreateMessage),
			nl,
			write('Fetched greeting: '),
			write(LookupMessage),
			nl,
			write('Deleted greeting and received 204 No Content.'),
			nl.

	:- else.

		run :-
			write('This demo needs backend thread support. Run greetings_rest_server::serve/2 and the greetings_rest_client predicates in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_rest_greetings_demo::run/1)).

	:- endif.

:- end_object.
