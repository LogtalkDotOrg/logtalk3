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


:- object(sample_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Sample router object used by the http_router tests.'
	]).

	:- protected(list_users/2).
	:- info(list_users/2, [
		comment is 'Route handler used by the sample router object for the ``/users`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_user/2).
	:- info(show_user/2, [
		comment is 'Route handler used by the sample router object for the ``/users/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(status_head/2).
	:- info(status_head/2, [
		comment is 'Route handler used by the sample router object for explicit ``HEAD /status`` requests.',
		argnames is ['Request', 'Response']
	]).

	:- protected(status_get/2).
	:- info(status_get/2, [
		comment is 'Route handler used by the sample router object for ``GET /status`` requests.',
		argnames is ['Request', 'Response']
	]).

	route(list_users, get, '/users', list_users).
	route(show_user, get, '/users/{id}', show_user).
	route(status_head, head, '/status', status_head).
	route(status_get, get, '/status', status_get).

	list_users(Request, Response) :-
		http::property(Request, route(list_users)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(users)), [], Response).

	show_user(Request, Response) :-
		http::property(Request, route(show_user)),
		http::property(Request, path_params([id-'42'])),
		http::method(Request, Method),
		http::version(Request, Version),
		show_user_body(Method, Body),
		http::response(Version, status(200, 'OK'), [], Body, [], Response).

	show_user_body(head, content('text/plain', text(head_user))).
	show_user_body(get, content('text/plain', text(user))).

	status_head(Request, Response) :-
		http::property(Request, route(status_head)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(head_status)), [], Response).

	status_get(Request, Response) :-
		http::property(Request, route(status_get)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(get_status)), [], Response).

:- end_object.


:- object(advanced_path_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise typed placeholders and wildcard path segments.'
	]).

	:- protected(show_report/2).
	:- info(show_report/2, [
		comment is 'Route handler used by the advanced path router object for the ``/reports/{year:integer}/*`` path.',
		argnames is ['Request', 'Response']
	]).

	route(show_report, get, '/reports/{year:integer}/*', show_report).

	show_report(Request, Response) :-
		http::property(Request, route(show_report)),
		http::property(Request, path_params([year-2026])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(report_2026)), [], Response).

:- end_object.


:- object(custom_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise customizable 404 and 405 hooks.'
	]).

	:- protected(show_items/2).
	:- info(show_items/2, [
		comment is 'Route handler used by the custom router object for the ``/items`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(route_not_found_response/2).
	:- info(route_not_found_response/2, [
		comment is 'Custom ``404`` response hook used by the router tests.',
		argnames is ['Request', 'Response']
	]).

	:- protected(route_method_not_allowed_response/3).
	:- info(route_method_not_allowed_response/3, [
		comment is 'Custom ``405`` response hook used by the router tests.',
		argnames is ['Request', 'AllowedMethods', 'Response']
	]).

	route(show_items, get, '/items', show_items).

	show_items(Request, Response) :-
		http::property(Request, route(show_items)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(items)), [], Response).

	route_not_found_response(Request, Response) :-
		http::version(Request, Version),
		http::response(Version, status(404, 'Not Found'), [x_router-custom], content('text/plain', text(custom_not_found)), [], Response).

	route_method_not_allowed_response(Request, AllowedMethods, Response) :-
		AllowedMethods == [get, head, options],
		http::property(Request, matched_path(true)),
		http::property(Request, effective_methods([get, head, options])),
		http::version(Request, Version),
		http::response(Version, status(405, 'Method Not Allowed'), [x_router-custom, allow-'GET, HEAD, OPTIONS'], content('text/plain', text(custom_method_not_allowed)), [], Response).

:- end_object.


:- object(automatic_options_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Router object used by the http_router tests to exercise automatic ``OPTIONS`` customization and annotated synthetic request context.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares route metadata used by the automatic ``OPTIONS`` router test object.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(route_automatic_options_response/3).
	:- info(route_automatic_options_response/3, [
		comment is 'Custom automatic ``OPTIONS`` response hook used by the router tests.',
		argnames is ['Request', 'EffectiveMethods', 'Response']
	]).

	:- protected(add_router_stage/3).
	:- info(add_router_stage/3, [
		comment is 'Response middleware handler that tags whether a response originated from the automatic ``OPTIONS`` path.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- protected(show_page/2).
	:- info(show_page/2, [
		comment is 'Route handler used by the automatic ``OPTIONS`` router object for the ``/options/pages/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	response_middleware(add_router_stage, add_router_stage).

	route(show_page, get, '/options/pages/{id}', show_page).

	route_metadata(show_page, [summary('Show page options'), tags([options, pages])]).

	route_automatic_options_response(Request, EffectiveMethods, Response) :-
		EffectiveMethods == [get, head, options],
		http::property(Request, automatic_options(true)),
		http::property(Request, effective_methods(EffectiveMethods)),
		http::property(Request, route(show_page)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, summary('Show page options')),
		http::property(Request, tags([options, pages])),
		http::version(Request, Version),
		http::response(Version, status(204, 'No Content'), [x_router-custom, allow-'GET, HEAD, OPTIONS'], empty, [], Response).

	add_router_stage(Request, response(Version, Status, Headers0, Body, Properties), Response) :-
		( 	http::property(Request, automatic_options(true)) ->
			Stage = automatic
		; 	Stage = routed
		),
		http::response(Version, Status, [x_router_stage-Stage| Headers0], Body, Properties, Response).

	show_page(Request, Response) :-
		http::property(Request, route(show_page)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, summary('Show page options')),
		http::property(Request, tags([options, pages])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(automatic_options_page)), [], Response).

:- end_object.


:- object(multi_route_automatic_options_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Router object used by the http_router tests to exercise synthetic ``OPTIONS`` metadata when multiple same-path routes match.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares route metadata used by the multi-route automatic ``OPTIONS`` router test object.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(route_automatic_options_response/3).
	:- info(route_automatic_options_response/3, [
		comment is 'Custom automatic ``OPTIONS`` response hook used by the multi-route router tests.',
		argnames is ['Request', 'EffectiveMethods', 'Response']
	]).

	:- protected(show_item/2).
	:- info(show_item/2, [
		comment is 'Route handler used by the multi-route automatic ``OPTIONS`` router object for ``GET /options/items/{id}`` requests.',
		argnames is ['Request', 'Response']
	]).

	:- protected(update_item/2).
	:- info(update_item/2, [
		comment is 'Route handler used by the multi-route automatic ``OPTIONS`` router object for ``POST /options/items/{id}`` requests.',
		argnames is ['Request', 'Response']
	]).

	route(show_item, get, '/options/items/{id}', show_item).
	route(update_item, post, '/options/items/{id}', update_item).

	route_metadata(show_item, [summary('Show option item'), tags([items]), deprecated(false)]).
	route_metadata(update_item, [summary('Update option item'), tags([items]), deprecated(false)]).

	route_automatic_options_response(Request, EffectiveMethods, Response) :-
		EffectiveMethods == [get, head, post, options],
		http::property(Request, automatic_options(true)),
		http::property(Request, effective_methods(EffectiveMethods)),
		\+ http::property(Request, route(_)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, tags([items])),
		http::property(Request, deprecated(false)),
		\+ http::property(Request, summary(_)),
		http::version(Request, Version),
		http::response(Version, status(204, 'No Content'), [x_router-multi, allow-'GET, HEAD, POST, OPTIONS'], empty, [], Response).

	show_item(Request, Response) :-
		http::property(Request, route(show_item)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, summary('Show option item')),
		http::property(Request, tags([items])),
		http::property(Request, deprecated(false)),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(multi_options_show_item)), [], Response).

	update_item(Request, Response) :-
		http::property(Request, route(update_item)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, summary('Update option item')),
		http::property(Request, tags([items])),
		http::property(Request, deprecated(false)),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(multi_options_update_item)), [], Response).

:- end_object.


:- object(middleware_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise middleware chaining.'
	]).

	:- protected(rewrite_legacy_items/2).
	:- info(rewrite_legacy_items/2, [
		comment is 'Middleware handler that rewrites the legacy items path to the canonical items path.',
		argnames is ['Request', 'Action']
	]).

	:- protected(tag_request/2).
	:- info(tag_request/2, [
		comment is 'Middleware handler that annotates requests before routing.',
		argnames is ['Request', 'Action']
	]).

	:- protected(maintenance_mode/2).
	:- info(maintenance_mode/2, [
		comment is 'Middleware handler that short-circuits maintenance requests with a service unavailable response.',
		argnames is ['Request', 'Action']
	]).

	:- protected(show_item/2).
	:- info(show_item/2, [
		comment is 'Route handler used by the middleware router object for the ``/items/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	middleware(maintenance_mode, maintenance_mode).
	middleware(rewrite_legacy_items, rewrite_legacy_items).
	middleware(tag_request, tag_request).

	route(show_item, get, '/items/{id}', show_item).

	maintenance_mode(Request, respond(Response)) :-
		http::target(Request, origin('/maintenance')),
		!,
		http::version(Request, Version),
		http::response(Version, status(503, 'Service Unavailable'), [], content('text/plain', text(maintenance)), [], Response).
	maintenance_mode(Request, continue(Request)).

	rewrite_legacy_items(request(Method, origin('/legacy-items/42'), Version, Headers, Body, Properties), continue(Request)) :-
		!,
		http::request(Method, origin('/items/42'), Version, Headers, Body, Properties, Request).
	rewrite_legacy_items(Request, continue(Request)).

	tag_request(request(Method, Target, Version, Headers, Body, Properties0), continue(Request)) :-
		Properties = [middleware(tagged)| Properties0],
		http::request(Method, Target, Version, Headers, Body, Properties, Request).

	show_item(Request, Response) :-
		http::property(Request, route(show_item)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, middleware(tagged)),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(middleware_item)), [], Response).

:- end_object.


:- object(negotiation_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise content negotiation.'
	]).

	:- protected(show_document/2).
	:- info(show_document/2, [
		comment is 'Route handler used by the negotiation router object for the ``/documents/42`` path.',
		argnames is ['Request', 'Response']
	]).

	route(show_document, get, '/documents/42', show_document).

	route_produces(show_document, ['application/json', 'text/plain']).

	show_document(Request, Response) :-
		http::property(Request, route(show_document)),
		http::property(Request, path_params([])),
		http::property(Request, response_media_type(MediaType)),
		http::version(Request, Version),
		document_body(MediaType, Body),
		http::response(Version, status(200, 'OK'), [], Body, [], Response).

	document_body('application/json', content('application/json', json({id-'42', format-json}))).
	document_body('text/plain', content('text/plain', text(document_text))).

:- end_object.


:- object(route_metadata_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise route metadata annotations.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares route-specific metadata used by the route metadata router object.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(add_route_summary/3).
	:- info(add_route_summary/3, [
		comment is 'Response middleware handler that exposes route summary metadata in a response header.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- protected(show_page/2).
	:- info(show_page/2, [
		comment is 'Route handler used by the route metadata router object for the ``/metadata/pages/42`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_document/2).
	:- info(show_document/2, [
		comment is 'Route handler used by the route metadata router object for the ``/metadata/documents/42`` path.',
		argnames is ['Request', 'Response']
	]).

	response_middleware(add_route_summary, add_route_summary).

	route(show_page, get, '/metadata/pages/42', show_page).
	route(show_document, get, '/metadata/documents/42', show_document).

	route_metadata(show_page, [summary('Show page'), description('Display a page using route metadata.'), tags([pages, public]), deprecated(false)]).
	route_metadata(show_document, [summary('Show document'), tags([documents, api])]).

	route_produces(show_document, ['application/json', 'text/plain']).

	add_route_summary(Request, response(Version, Status, Headers0, Body, Properties), Response) :-
		http::property(Request, summary(Summary)),
		http::response(Version, Status, [x_route_summary-Summary| Headers0], Body, Properties, Response).

	show_page(Request, Response) :-
		http::property(Request, route(show_page)),
		http::property(Request, path_params([])),
		http::property(Request, summary('Show page')),
		http::property(Request, description('Display a page using route metadata.')),
		http::property(Request, tags([pages, public])),
		http::property(Request, deprecated(false)),
		\+ http::property(Request, summary(stale)),
		\+ http::property(Request, tags([legacy])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(route_metadata_page)), [], Response).

	show_document(Request, Response) :-
		http::property(Request, route(show_document)),
		http::property(Request, path_params([])),
		http::property(Request, summary('Show document')),
		http::property(Request, tags([documents, api])),
		http::property(Request, response_media_type('application/json')),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('application/json', json({kind-route_metadata, format-json})), [], Response).

:- end_object.


:- object(open_api_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise automatic OpenAPI provider derivation.'
	]).

	:- protected(open_api_info/1).
	:- info(open_api_info/1, [
		comment is 'OpenAPI info descriptor hook used by the OpenAPI router test object.',
		argnames is ['Info']
	]).

	:- protected(open_api_servers/1).
	:- info(open_api_servers/1, [
		comment is 'OpenAPI server descriptor hook used by the OpenAPI router test object.',
		argnames is ['Servers']
	]).

	:- protected(open_api_schema/2).
	:- info(open_api_schema/2, [
		comment is 'OpenAPI reusable schema hook used by the OpenAPI router test object.',
		argnames is ['Name', 'Schema']
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares route metadata used for automatic OpenAPI provider derivation tests.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(show_item/2).
	:- info(show_item/2, [
		comment is 'Route handler used by the OpenAPI router object for the ``/open-api/items/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(create_document/2).
	:- info(create_document/2, [
		comment is 'Route handler used by the OpenAPI router object for the ``POST /open-api/documents`` path.',
		argnames is ['Request', 'Response']
	]).

	open_api_info(info('Router OpenAPI API', '1.0.0', 'OpenAPI document derived from http_router descriptors.', [])).

	open_api_servers([
		server('https://api.example.com/router', 'Production server')
	]).

	route(show_item, get, '/open-api/items/{id}', show_item).
	route(create_document, post, '/open-api/documents', create_document).

	route_produces(show_item, ['application/json']).

	route_metadata(show_item, [
		summary('Show item'),
		description('Returns an item by id.'),
		tags([items]),
		parameters([
			parameter(id, path, 'Item identifier', true, schema_ref(item_id))
		]),
		responses([
			response(200, 'Requested item', [media('application/json', schema_ref(item))]),
			response(default, 'Error response', [media('application/json', schema_ref(api_error))])
		])
	]).

	route_metadata(create_document, [
		summary('Create document'),
		description('Creates a document from the submitted payload.'),
		tags([documents]),
		request_body(request_body('Document payload', true, [media('application/json', schema_ref(document_input))])),
		responses([
			response(201, 'Created document', [media('application/json', schema_ref(document))]),
			response(default, 'Error response', [media('application/json', schema_ref(api_error))])
		])
	]).

	open_api_schema(item_id, {
		type-string,
		minLength-1
	}).

	open_api_schema(item, {
		type-object,
		properties-{
			id-{type-string},
			name-{type-string}
		},
		required-[id, name],
		additionalProperties- @false
	}).

	open_api_schema(document_input, {
		type-object,
		properties-{
			title-{type-string},
			content-{type-string}
		},
		required-[title],
		additionalProperties- @false
	}).

	open_api_schema(document, {
		type-object,
		properties-{
			id-{type-string},
			title-{type-string},
			content-{type-string}
		},
		required-[id, title],
		additionalProperties- @false
	}).

	open_api_schema(api_error, {
		type-object,
		properties-{
			code-{type-string},
			message-{type-string}
		},
		required-[code, message],
		additionalProperties- @false
	}).

	show_item(Request, Response) :-
		http::property(Request, route(show_item)),
		http::property(Request, path_params([id-'42'])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('application/json', json({id-'42', name-'Widget'})), [], Response).

	create_document(Request, Response) :-
		http::property(Request, route(create_document)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(201, 'Created'), [], content('application/json', json({id-'100', title-'Guide'})), [], Response).

:- end_object.


:- object(inferred_open_api_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise OpenAPI request and response inference from route handlers.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares only the minimal route metadata needed by the OpenAPI inference tests.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(show_message/2).
	:- info(show_message/2, [
		comment is 'Route handler used by the inferred OpenAPI router object for the ``GET /inferred/messages/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(create_message/2).
	:- info(create_message/2, [
		comment is 'Route handler used by the inferred OpenAPI router object for the ``POST /inferred/messages`` path.',
		argnames is ['Request', 'Response']
	]).

	route(show_message, get, '/inferred/messages/{id}', show_message).
	route(create_message, post, '/inferred/messages', create_message).

	route_produces(show_message, ['application/json', 'text/plain']).
	route_produces(create_message, ['application/json']).

	route_metadata(show_message, [
		summary('Show inferred message'),
		tags([inferred])
	]).

	route_metadata(create_message, [
		summary('Create inferred message'),
		tags([inferred])
	]).

	show_message(Request, Response) :-
		http::property(Request, route(show_message)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, response_media_type(MediaType)),
		http::version(Request, Version),
		inferred_message_body(MediaType, Body),
		http::response(Version, status(200, 'OK'), [], Body, [], Response).

	inferred_message_body('application/json', content('application/json', json({id-'42', title-'Guide'}))).
	inferred_message_body('text/plain', content('text/plain', text('Guide'))).

	create_message(Request, Response) :-
		http::property(Request, route(create_message)),
		http::property(Request, path_params([])),
		http::body(Request, content('application/json', json({title-'Guide', published-true}))),
		http::version(Request, Version),
		http::response(Version, status(201, 'Created'), [], content('application/json', json({id-'100', title-'Guide', published-true})), [], Response).

:- end_object.


:- object(example_open_api_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise example-driven OpenAPI inference.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares minimal route metadata for the example-driven OpenAPI inference tests.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(route_open_api_request_body_example/2).
	:- info(route_open_api_request_body_example/2, [
		comment is 'Provides request-body examples for OpenAPI inference tests.',
		argnames is ['Id', 'Body']
	]).

	:- protected(route_open_api_response_example/2).
	:- info(route_open_api_response_example/2, [
		comment is 'Provides response examples for OpenAPI inference tests.',
		argnames is ['Id', 'Response']
	]).

	:- protected(update_message/2).
	:- info(update_message/2, [
		comment is 'Route handler used by the example-driven OpenAPI router object for the ``PUT /examples/messages/{id:integer}`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_nullable_message/2).
	:- info(show_nullable_message/2, [
		comment is 'Route handler used by the example-driven OpenAPI router object for the ``GET /examples/messages/nullable`` path.',
		argnames is ['Request', 'Response']
	]).

	route(show_nullable_message, get, '/examples/messages/nullable', show_nullable_message).
	route(update_message, put, '/examples/messages/{id:integer}', update_message).

	route_metadata(show_nullable_message, [
		summary('Show example nullable message'),
		tags([examples])
	]).

	route_metadata(update_message, [
		summary('Update example message'),
		tags([examples])
	]).

	route_open_api_request_body_example(update_message, empty).
	route_open_api_request_body_example(update_message, content('application/json', json({title-'Guide'}))).

	route_open_api_response_example(show_nullable_message, response(http(1, 1), status(200, 'OK'), [], content('application/json', json({id-'42', subtitle-null})), [])).
	route_open_api_response_example(update_message, response(http(1, 1), status(200, 'OK'), [], content('application/json', json({id-42, title-'Guide'})), [])).
	route_open_api_response_example(update_message, response(http(1, 1), status(400, 'Bad Request'), [], content('application/json', json({error-'invalid'})), [])).

	show_nullable_message(Request, Response) :-
		http::property(Request, route(show_nullable_message)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('application/json', json({id-'42', subtitle-null})), [], Response).

	update_message(Request, Response) :-
		http::property(Request, route(update_message)),
		http::property(Request, path_params([id-42])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('application/json', json({id-42, title-'Guide'})), [], Response).

:- end_object.


:- object(response_middleware_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise response post-processing middleware.'
	]).

	:- protected(short_circuit/2).
	:- info(short_circuit/2, [
		comment is 'Middleware handler that short-circuits blocked requests with a service unavailable response.',
		argnames is ['Request', 'Action']
	]).

	:- protected(add_router_stage/3).
	:- info(add_router_stage/3, [
		comment is 'Response middleware handler that adds a post-processing marker header.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- protected(add_response_kind/3).
	:- info(add_response_kind/3, [
		comment is 'Response middleware handler that tags responses according to whether routing occurred before the response was produced.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- protected(show_page/2).
	:- info(show_page/2, [
		comment is 'Route handler used by the response middleware router object for the ``/pages/42`` path.',
		argnames is ['Request', 'Response']
	]).

	middleware(short_circuit, short_circuit).
	response_middleware(add_router_stage, add_router_stage).
	response_middleware(add_response_kind, add_response_kind).

	route(show_page, get, '/pages/42', show_page).

	short_circuit(Request, respond(Response)) :-
		http::target(Request, origin('/blocked')),
		!,
		http::version(Request, Version),
		http::response(Version, status(503, 'Service Unavailable'), [], content('text/plain', text(blocked)), [], Response).
	short_circuit(Request, continue(Request)).

	add_router_stage(_Request, response(Version, Status, Headers0, Body, Properties), Response) :-
		http::response(Version, Status, [x_router_stage-after| Headers0], Body, Properties, Response).

	add_response_kind(Request, response(Version, Status, Headers0, Body, Properties), Response) :-
		( 	http::property(Request, route(show_page)) ->
			Kind = routed
		; 	Kind = intercepted
		),
		http::response(Version, Status, [x_response_kind-Kind| Headers0], Body, Properties, Response).

	show_page(Request, Response) :-
		http::property(Request, route(show_page)),
		http::property(Request, path_params([])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(page)), [], Response).

:- end_object.


:- object(custom_negotiation_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Router object used by the http_router tests to exercise customizable 406 responses for content negotiation failures.'
	]).

	:- protected(show_document/2).
	:- info(show_document/2, [
		comment is 'Route handler used by the custom negotiation router object for the ``/documents/42`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(route_not_acceptable_response/3).
	:- info(route_not_acceptable_response/3, [
		comment is 'Custom ``406`` response hook used by the router tests.',
		argnames is ['Request', 'ProducedMediaTypes', 'Response']
	]).

	route(show_document, get, '/documents/42', show_document).

	route_produces(show_document, ['application/json', 'text/plain']).

	show_document(Request, Response) :-
		http::property(Request, route(show_document)),
		http::property(Request, path_params([])),
		http::property(Request, response_media_type(MediaType)),
		http::version(Request, Version),
		document_body(MediaType, Body),
		http::response(Version, status(200, 'OK'), [], Body, [], Response).

	route_not_acceptable_response(Request, ProducedMediaTypes, Response) :-
		ProducedMediaTypes == ['application/json', 'text/plain'],
		http::version(Request, Version),
		http::response(Version, status(406, 'Not Acceptable'), [x_router-custom], content('text/plain', text(custom_not_acceptable)), [], Response).

	document_body('application/json', content('application/json', json({id-'42', format-json}))).
	document_body('text/plain', content('text/plain', text(document_text))).

:- end_object.
