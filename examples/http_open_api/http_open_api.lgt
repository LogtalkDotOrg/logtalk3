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


% The provider object is the single source of truth for the example API.
% Both the server and the client refer back to this object when they need
% the contract, operation descriptors, or reusable schemas.
:- object(greetings_api(_Port_),
	implements(open_api_provider_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'OpenAPI provider for the HTTP and OpenAPI example.',
		parnames is (['Port'])
	]).

	api_info(info('Greetings API', '1.0.0', 'Tiny HTTP API used by the HTTP and OpenAPI example.', [description('Publishes a small greeting contract and validates the corresponding request and response terms.')])).

	servers([
		server(URL, 'Local demo server')
	]) :-
		base_url(URL).

	% The example intentionally exposes only two operations so the generated
	% document stays small: one body-based POST and one path-based GET.
	operations([
		operation(
			create_greeting,
			post,
			'/greetings',
			'Create greeting',
			[],
			request_body('Greeting input', true, [media('application/json', schema_ref(greeting_request))]),
			[
				response(200, 'Greeting generated', [media('application/json', schema_ref(greeting_response))]),
				response(400, 'Invalid request', [media('application/json', schema_ref(api_error))])
			],
			[
				description('Returns a greeting for the supplied name.'),
				tags([greetings])
			]
		),
		operation(
			get_greeting,
			get,
			'/greetings/{name}',
			'Get greeting',
			[
				parameter(name, path, 'Name to greet', true, schema_ref(greeting_name))
			],
			none,
			[
				response(200, 'Greeting generated', [media('application/json', schema_ref(greeting_response))]),
				response(400, 'Invalid request', [media('application/json', schema_ref(api_error))])
			],
			[
				description('Returns a greeting for the supplied name from the URL path.'),
				tags([greetings])
			]
		)
	]).

	% A dedicated path-parameter schema keeps the GET operation readable and
	% mirrors how larger APIs usually factor out reusable parameter schemas.
	schema(greeting_name, {
		type-string,
		minLength-1
	}).

	% The POST operation accepts a tiny JSON payload.
	schema(greeting_request, {
		type-object,
		properties-{
			name-{type-string, minLength-1}
		},
		required-[name],
		additionalProperties- @false
	}).

	% Both operations return the same response shape so the example can show
	% one schema being reused by multiple operation descriptors.
	schema(greeting_response, {
		type-object,
		properties-{
			message-{type-string, minLength-1}
		},
		required-[message],
		additionalProperties- @false
	}).

	% Error responses are also described in the contract even though the demo
	% only exercises the successful path.
	schema(api_error, {
		type-object,
		properties-{
			code-{type-string, enum-[invalid_request, not_found]},
			message-{type-string, minLength-1}
		},
		required-[code, message],
		additionalProperties- @false
	}).

	% The published server URL depends on the port chosen when the listener is
	% opened, so the object parameter is used to compute the final base URL.
	base_url(URL) :-
		number_codes(_Port_, PortCodes),
		atom_codes(PortAtom, PortCodes),
		atom_concat('http://127.0.0.1:', PortAtom, URL).

:- end_object.


% The handler performs three roles: publishes the generated OpenAPI
% document, validates incoming requests against the selected operation, and
% validates the response before sending it back.
:- object(greetings_http_handler(_Port_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'HTTP handler for the HTTP and OpenAPI example.'
	]).

	% Route dispatch is written explicitly so the example stays easy to read
	% without introducing an extra router abstraction.
	handle(Request, Response) :-
		http_core::method(Request, get),
		http_core::target(Request, origin('/openapi.json')),
		!,
		reply_with_document(Request, Response).
	handle(Request, Response) :-
		http_core::method(Request, get),
		request_greeting_name(Request, _Name),
		!,
		reply_with_path_greeting(Request, Response).
	handle(Request, Response) :-
		http_core::method(Request, post),
		http_core::target(Request, origin('/greetings')),
		!,
		reply_with_greeting(Request, Response).
	handle(Request, Response) :-
		http_core::version(Request, Version),
		json_response(Version, status(404, 'Not Found'), {code-not_found, message-'Unknown route'}, Response).

	% The OpenAPI document is derived on demand from the provider object and
	% then returned as ordinary JSON content.
	reply_with_document(Request, Response) :-
		http_core::version(Request, Version),
		provider(Provider),
		open_api::document(Provider, Document),
		json_response(Version, status(200, 'OK'), Document, Response).

	% For the POST operation the handler first validates the request contract,
	% then extracts the body field used to build the greeting text.
	reply_with_greeting(Request, Response) :-
		http_core::version(Request, Version),
		provider(Provider),
		open_api::validate_request(Provider, create_greeting, Request, Errors),
		( 	Errors == [] ->
			request_body_name(Request, Name),
			greeting_text(Name, Message),
			json_response(Version, status(200, 'OK'), {message-Message}, Candidate)
		; 	json_response(Version, status(400, 'Bad Request'), {code-invalid_request, message-'Request does not conform to the OpenAPI contract'}, Candidate)
		),
		open_api::validate_response(Provider, create_greeting, Candidate),
		Response = Candidate.

	% The GET path-parameter operation follows the same contract-validation
	% pattern. The only difference is how the input value is extracted.
	reply_with_path_greeting(Request, Response) :-
		http_core::version(Request, Version),
		provider(Provider),
		open_api::validate_request(Provider, get_greeting, Request, Errors),
		( 	Errors == [] ->
			request_greeting_name(Request, Name),
			greeting_text(Name, Message),
			json_response(Version, status(200, 'OK'), {message-Message}, Candidate)
		; 	json_response(Version, status(400, 'Bad Request'), {code-invalid_request, message-'Request does not conform to the OpenAPI contract'}, Candidate)
		),
		open_api::validate_response(Provider, get_greeting, Candidate),
		Response = Candidate.

	% Keeping provider selection in one helper avoids repeating the parametric
	% object construction across the handler predicates.
	provider(greetings_api(_Port_)).

	% POST input comes from a JSON body.
	request_body_name(Request, Name) :-
		http_core::body(Request, content('application/json', json({name-Name}))),
		!.

	% GET input comes from the request target. OpenAPI validation confirms the
	% path matches the template, but the example still extracts the actual
	% segment explicitly so the handler can use it.
	request_greeting_name(Request, Name) :-
		http_core::target(Request, Target),
		target_greeting_name(Target, Name).

	target_greeting_name(origin(Path), Name) :-
		greeting_path_name(Path, Name).
	target_greeting_name(origin(Path, _Query), Name) :-
		greeting_path_name(Path, Name).

	% This is enough for the example because the only templated path begins
	% with /greetings/. Larger applications would usually centralize this kind
	% of parsing in a router.
	greeting_path_name(Path, Name) :-
		atom_concat('/greetings/', Name, Path),
		Name \== ''.

	% Both operations share the same greeting formatting logic.
	greeting_text(Name, Message) :-
		atom_concat('Hello, ', Name, Prefix),
		atom_concat(Prefix, '!', Message).

	% All responses are JSON so the response builder can stay small.
	json_response(Version, Status, Body, Response) :-
		http_core::response(Version, Status, [], content('application/json', json(Body)), [], Response).

:- end_object.


% This object is the simplest standalone server entry point for the example.
% It serves a bounded number of accepted connections and then closes the
% listener, which makes it convenient for short demos and tests where the
% number of expected connections is known in advance. The open-ended server
% alternative defined below trades that simplicity for an explicit stop step.
:- object(greetings_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Small local HTTP server used by the HTTP and OpenAPI example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	% Count is the number of accepted connections handled before shutdown.
	% Each accepted connection is served using the HTTP server connection loop,
	% so a keep-alive client may still send multiple requests on one accepted
	% socket connection.
	serve(Port, Count) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			http_socket::serve_listener(Listener, greetings_http_handler(Port), Count, _ClientInfos, [shutdown(close)]),
			Error,
			(	catch(http_socket::close_listener(Listener), _, true),
				throw(Error)
			)
		).

:- end_object.


	% This alternative server uses the open-ended listener loop. Unlike the
	% bounded greetings_server object, it does not stop after a fixed number of
	% accepted connections. Instead it stays up until an explicit call to stop/1.
	% That makes it a better fit for interactive sessions or longer-running
	% examples where the total number of requests is not known beforehand.
	:- object(open_ended_greetings_server).

		:- info([
			version is 0:1:0,
			author is 'Paulo Moura',
			date is 2026-05-21,
			comment is 'Open-ended local HTTP server used by the HTTP and OpenAPI example.'
		]).

		:- public(serve/2).
		:- mode(serve(?integer, +nonvar), one_or_error).
		:- info(serve/2, [
			comment is 'Opens a local listener and serves accepted connections until stop/1 is called for the given control term.',
			argnames is ['Port', 'Control']
		]).

		:- public(stop/1).
		:- mode(stop(+nonvar), one_or_error).
		:- info(stop/1, [
			comment is 'Requests shutdown of an open-ended server loop previously started with serve/2.',
			argnames is ['Control']
		]).

		:- if(current_logtalk_flag(threads, supported)).
			:- public(start/3).
			:- mode(start(?integer, +nonvar, -integer), one_or_error).
			:- info(start/3, [
				comment is 'Starts the open-ended server in a worker thread, waits until the listener is ready, and returns the worker tag.',
				argnames is ['Port', 'Control', 'Tag']
			]).

			:- public(stop/2).
			:- mode(stop(+nonvar, +integer), one_or_error).
			:- info(stop/2, [
				comment is 'Requests shutdown of an open-ended server loop and waits for the worker thread to finish.',
				argnames is ['Control', 'Tag']
			]).

			:- threaded.
		:- endif.

		% The control term is the handle used later by stop/1. The server also
		% emits a readiness notification so an in-process client can wait until the
		% listener port is known before sending requests.
		serve(Port, Control) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			catch(
				(	notify_server_ready(Control, Port),
					http_socket::serve_until_shutdown(Listener, greetings_http_handler(Port), Control, [])
				),
				Error,
				(	catch(http_socket::close_listener(Listener), _, true),
					throw(Error)
				)
			).

		% Shutdown is explicit for this server alternative instead of count-based.
		stop(Control) :-
			http_socket::request_shutdown(Control).

		:- if(current_logtalk_flag(threads, supported)).

			% These helpers keep readiness notifications and worker joins inside the
			% same object, which is important because the thread notification queue is
			% object-scoped.
			start(Port, Control, Tag) :-
				threaded_once(serve(Port, Control), Tag),
				threaded_wait(open_ended_greetings_server_ready(Control, Port)).

			stop(Control, Tag) :-
				stop(Control),
				once(threaded_exit(serve(_Port, Control), Tag)).

			notify_server_ready(Control, Port) :-
				threaded_notify(open_ended_greetings_server_ready(Control, Port)).

		:- else.

			notify_server_ready(_Control, _Port).

		:- endif.

	:- end_object.


% The client object mirrors how a separate consumer would use the API: it
% fetches the published OpenAPI document, builds request URLs from the
	% operation descriptors, and validates the received responses. Compared with
	% managed_greetings_client below, this object only speaks HTTP and assumes
	% that some server lifecycle is managed elsewhere. That makes it the more
	% reusable choice when talking to either the bounded server or an already
	% running open-ended server.
:- object(greetings_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'HTTP client used by the HTTP and OpenAPI example.'
	]).

	:- public(fetch_document/2).
	:- mode(fetch_document(+integer, -compound), one_or_error).
	:- info(fetch_document/2, [
		comment is 'Fetches the published OpenAPI document from the server and validates it.',
		argnames is ['Port', 'Document']
	]).

	:- public(create_greeting/3).
	:- mode(create_greeting(+integer, +atom, --compound), one_or_error).
	:- info(create_greeting/3, [
		comment is 'Builds a request body, calls the operation declared by the provider object, and validates the returned response.',
		argnames is ['Port', 'Name', 'Response']
	]).

	:- public(get_greeting/3).
	:- mode(get_greeting(+integer, +atom, --compound), one_or_error).
	:- info(get_greeting/3, [
		comment is 'Calls the path-parameter operation declared by the provider object and validates the returned response.',
		argnames is ['Port', 'Name', 'Response']
	]).

	:- public(run/3).
	:- mode(run(+integer, +atom, -compound), one_or_error).
	:- info(run/3, [
		comment is 'Fetches the OpenAPI document and then exercises both greeting operations.',
		argnames is ['Port', 'Name', 'Result']
	]).

	% The client fetches the server-published contract first so the example
	% shows that the document is a normal HTTP resource.
	fetch_document(Port, Document) :-
		open_api_url(Port, URL),
		http_client::get(URL, Response, []),
		http_core::status(Response, status(200, 'OK')),
		http_core::body(Response, content('application/json', json(Document))),
		open_api::validate_document(Document).

	% The POST call demonstrates a body-based request built directly from the
	% OpenAPI operation URL.
	create_greeting(Port, Name, Response) :-
		operation_url(Port, create_greeting, post, URL),
		RequestBody = content('application/json', json({name-Name})),
		http_client::post(URL, RequestBody, Response, []),
		open_api::validate_response(greetings_api(Port), create_greeting, Response).

	% The GET call demonstrates the path-parameter variant of the same API.
	get_greeting(Port, Name, Response) :-
		path_operation_url(Port, get_greeting, get, Name, URL),
		http_client::get(URL, Response, []),
		open_api::validate_response(greetings_api(Port), get_greeting, Response).

	% Running the full client flow returns the fetched document plus both
	% operation responses so tests and the demo can inspect the whole result.
	run(Port, Name, result(Document, CreateResponse, LookupResponse)) :-
		fetch_document(Port, Document),
		open_api::document(greetings_api(Port), Document),
		create_greeting(Port, Name, CreateResponse),
		get_greeting(Port, Name, LookupResponse).

	open_api_url(Port, URL) :-
		greetings_api(Port)::servers([server(BaseURL, _Description)]),
		atom_concat(BaseURL, '/openapi.json', URL).

	% Using open_api::operation/3 keeps the client aligned with the contract
	% instead of duplicating paths in multiple places.
	operation_url(Port, OperationId, Method, URL) :-
		open_api::operation(greetings_api(Port), OperationId, operation(OperationId, Method, Path, _Summary, _Parameters, _RequestBody, _Responses, _Properties)),
		greetings_api(Port)::servers([server(BaseURL, _Description)]),
		atom_concat(BaseURL, Path, URL).

	% The path-parameter helper still consults the operation descriptor first,
	% then fills the template with the concrete example value.
	path_operation_url(Port, OperationId, Method, Name, URL) :-
		open_api::operation(greetings_api(Port), OperationId, operation(OperationId, Method, '/greetings/{name}', _Summary, _Parameters, _RequestBody, _Responses, _Properties)),
		greetings_api(Port)::servers([server(BaseURL, _Description)]),
		atom_concat('/greetings/', Name, Path),
		atom_concat(BaseURL, Path, URL).

:- end_object.


% This alternative client is intentionally less generic than greetings_client.
% It is a convenience wrapper for single-process demos and tests: it starts
% the open-ended server alternative in a worker thread, waits for the server
% to announce the chosen port, runs the same HTTP workflow as greetings_client,
% and then explicitly stops the server. In short, greetings_client is better
% when server lifecycle already exists; managed_greetings_client is better
% when the client is also responsible for starting and stopping that server.
:- object(managed_greetings_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Managed client that starts and stops the open-ended server alternative for the HTTP and OpenAPI example.'
	]).

	:- public(run/2).
	:- mode(run(+atom, -compound), one_or_error).
	:- info(run/2, [
		comment is 'Starts the open-ended server on an ephemeral port, runs the full client workflow, stops the server, and returns the results.',
		argnames is ['Name', 'Result']
	]).

	:- public(run/3).
	:- mode(run(?integer, +atom, -compound), one_or_error).
	:- info(run/3, [
		comment is 'Starts the open-ended server on the given or selected port, runs the full client workflow, stops the server, and returns the results.',
		argnames is ['Port', 'Name', 'Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run(Name, Result) :-
			run(_Port, Name, Result).

		% The managed client waits for an explicit readiness notification before
		% sending requests so it can reuse the simpler greetings_client object for
		% the actual HTTP work.
		run(Port, Name, Result) :-
			Control = managed_greetings_client_server(Name),
			open_ended_greetings_server::start(Port, Control, Tag),
			catch(
				greetings_client::run(Port, Name, Result),
				Error,
				(	cleanup_server(Control, Port, Tag),
					throw(Error)
				)
			),
			open_ended_greetings_server::stop(Control, Tag).

		cleanup_server(Control, Port, Tag) :-
			catch(open_ended_greetings_server::stop(Control, Tag), _, true),
			catch(once(threaded_exit(open_ended_greetings_server::serve(Port, Control), Tag)), _, true).

	:- else.

		run(_Name, _Result) :-
			throw(error(resource_error(threads), managed_greetings_client::run/2)).

		run(_Port, _Name, _Result) :-
			throw(error(resource_error(threads), managed_greetings_client::run/3)).

	:- endif.

:- end_object.


% The demo object keeps the example self-contained when backend threads are
% available: one thread runs the bounded server while the main thread acts
% as the client. This keeps the existing bounded-server story visible even
% after adding the open-ended server and managed client alternatives.
:- object(http_open_api_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'Self-contained demo object for the HTTP and OpenAPI example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns the fetched document plus both greeting responses when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		% The demo opens an ephemeral port, starts the server in a worker thread,
		% runs the client workflow, then joins the worker before returning.
		run(Result) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_demo_requests(Listener, Port), Tag),
			catch(
				greetings_client::run(Port, 'Ada', Result),
				Error,
				(	cleanup_demo(Listener, Port, Tag),
					throw(Error)
				)
			),
			once(threaded_exit(serve_demo_requests(Listener, Port), Tag)),
			catch(http_socket::close_listener(Listener), _, true).

		% The demo needs three accepted connections because the client performs
		% three one-shot HTTP calls: fetch the document, POST a greeting, and GET
		% the path-parameter greeting.
		serve_demo_requests(Listener, Port) :-
			http_socket::serve_listener(Listener, greetings_http_handler(Port), 3, _ClientInfos, [shutdown(close)]).

		% Cleanup closes the listener first so the server thread cannot keep
		% waiting for more accepted connections after an exception.
		cleanup_demo(Listener, Port, Tag) :-
			catch(http_socket::close_listener(Listener), _, true),
			catch(once(threaded_exit(serve_demo_requests(Listener, Port), Tag)), _, true).

		% Printing both responses makes the difference between the POST and GET
		% operations visible when running the demo interactively.
		print_result(result(_Document, CreateResponse, LookupResponse)) :-
			http_core::body(CreateResponse, content('application/json', json({message-CreateMessage}))),
			http_core::body(LookupResponse, content('application/json', json({message-LookupMessage}))),
			write('Fetched the OpenAPI document and created: '),
			write(CreateMessage),
			nl,
			write('Then fetched the path-parameter greeting: '),
			write(LookupMessage),
			nl.

	:- else.

		% Without backend threads the source code is still useful, but the fully
		% automatic demo cannot run in a single session.
		run :-
			write('This demo needs backend thread support. Run greetings_server::serve/2 and the greetings_client predicates in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_open_api_demo::run/1)).

	:- endif.

:- end_object.
