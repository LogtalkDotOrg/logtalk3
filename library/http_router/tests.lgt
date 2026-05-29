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
		comment is 'Unit tests for the "http_router" library.'
	]).

	:- uses(http, [body/2, header/3, property/2, status/2]).

	cover(http_router).

	test(http_router_handle_2_01, deterministic) :-
		Request = request(get, origin('/users'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(users))).

	test(http_router_handle_2_02, deterministic) :-
		Request = request(get, origin('/users/42'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(user))).

	test(http_router_handle_2_03, deterministic) :-
		Request = request(head, origin('/users/42'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(head_user))).

	test(http_router_handle_2_04, deterministic) :-
		Request = request(head, origin('/status'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(head_status))).

	test(http_router_handle_2_05, deterministic) :-
		Request = request(post, origin('/users/42'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(405, 'Method Not Allowed')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		body(Response, content('text/plain', text('Method Not Allowed'))).

	test(http_router_handle_2_06, deterministic) :-
		Request = request(get, origin('/missing'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_router_handle_2_07, deterministic) :-
		Request = request(
			get,
			absolute([scheme(http), authority('example.com'), path('/users/42'), query(''), fragment('')]),
			http(1, 1),
			[],
			empty,
			[]
		),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(user))).

	test(http_router_handle_2_08, deterministic) :-
		Request = request(post, origin('/status'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(405, 'Method Not Allowed')),
		header(Response, allow, 'HEAD, GET, OPTIONS'),
		body(Response, content('text/plain', text('Method Not Allowed'))).

	test(http_router_handle_2_09, deterministic) :-
		Request = request(options, origin('/users/42'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		body(Response, empty).

	test(http_router_handle_2_10, deterministic) :-
		Request = request(options, origin('/status'), http(1, 1), [], empty, []),
		sample_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, allow, 'HEAD, GET, OPTIONS'),
		body(Response, empty).

	test(http_router_handle_2_10a, deterministic) :-
		Request = request(options, origin('/metadata/pages/42'), http(1, 1), [], empty, []),
		route_metadata_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		header(Response, x_route_summary, 'Show page'),
		body(Response, empty).

	test(http_router_handle_2_10b, deterministic) :-
		Request = request(options, origin('/options/pages/42'), http(1, 1), [], empty, []),
		automatic_options_http_router::handle(Request, Response),
		status(Response, status(204, 'No Content')),
		header(Response, x_router, custom),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		header(Response, x_router_stage, automatic),
		body(Response, empty).

	test(http_router_handle_2_10c, deterministic) :-
		Request = request(options, origin('/options/items/42'), http(1, 1), [], empty, []),
		multi_route_automatic_options_http_router::handle(Request, Response),
		status(Response, status(204, 'No Content')),
		header(Response, x_router, multi),
		header(Response, allow, 'GET, HEAD, POST, OPTIONS'),
		body(Response, empty).

	test(http_router_handle_2_11, deterministic) :-
		Request = request(get, origin('/missing'), http(1, 1), [], empty, []),
		custom_http_router::handle(Request, Response),
		status(Response, status(404, 'Not Found')),
		header(Response, x_router, custom),
		body(Response, content('text/plain', text(custom_not_found))).

	test(http_router_handle_2_12, deterministic) :-
		Request = request(post, origin('/items'), http(1, 1), [], empty, []),
		custom_http_router::handle(Request, Response),
		status(Response, status(405, 'Method Not Allowed')),
		header(Response, x_router, custom),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		body(Response, content('text/plain', text(custom_method_not_allowed))).

	test(http_router_handle_2_13, deterministic) :-
		Request = request(get, origin('/legacy-items/42'), http(1, 1), [], empty, []),
		middleware_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(middleware_item))).

	test(http_router_handle_2_14, deterministic) :-
		Request = request(get, origin('/maintenance'), http(1, 1), [], empty, []),
		middleware_http_router::handle(Request, Response),
		status(Response, status(503, 'Service Unavailable')),
		body(Response, content('text/plain', text(maintenance))).

	test(http_router_handle_2_15, deterministic) :-
		Request = request(get, origin('/documents/42'), http(1, 1), [], empty, []),
		negotiation_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-'42', format-json}))).

	test(http_router_handle_2_16, deterministic) :-
		Request = request(get, origin('/documents/42'), http(1, 1), [accept-'text/plain'], empty, []),
		negotiation_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(document_text))).

	test(http_router_handle_2_17, deterministic) :-
		Request = request(get, origin('/documents/42'), http(1, 1), [accept-'application/*'], empty, []),
		negotiation_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-'42', format-json}))).

	test(http_router_handle_2_18, deterministic) :-
		Request = request(get, origin('/documents/42'), http(1, 1), [accept-'text/plain; q=0.3, application/json; q=0.9'], empty, []),
		negotiation_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-'42', format-json}))).

	test(http_router_handle_2_19, deterministic) :-
		Request = request(get, origin('/documents/42'), http(1, 1), [accept-'image/png'], empty, []),
		negotiation_http_router::handle(Request, Response),
		status(Response, status(406, 'Not Acceptable')),
		body(Response, content('text/plain', text('Not Acceptable'))).

	test(http_router_handle_2_20, deterministic) :-
		Request = request(get, origin('/documents/42'), http(1, 1), [accept-'image/png'], empty, []),
		custom_negotiation_http_router::handle(Request, Response),
		status(Response, status(406, 'Not Acceptable')),
		header(Response, x_router, custom),
		body(Response, content('text/plain', text(custom_not_acceptable))).

	test(http_router_handle_2_21, deterministic) :-
		Request = request(get, origin('/pages/42'), http(1, 1), [], empty, []),
		response_middleware_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, x_router_stage, after),
		header(Response, x_response_kind, routed),
		body(Response, content('text/plain', text(page))).

	test(http_router_handle_2_22, deterministic) :-
		Request = request(get, origin('/blocked'), http(1, 1), [], empty, []),
		response_middleware_http_router::handle(Request, Response),
		status(Response, status(503, 'Service Unavailable')),
		header(Response, x_router_stage, after),
		header(Response, x_response_kind, intercepted),
		body(Response, content('text/plain', text(blocked))).

	test(http_router_handle_2_23, deterministic) :-
		Request = request(get, origin('/metadata/pages/42'), http(1, 1), [], empty, [summary(stale), tags([legacy])]),
		route_metadata_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, x_route_summary, 'Show page'),
		body(Response, content('text/plain', text(route_metadata_page))).

	test(http_router_handle_2_24, deterministic) :-
		Request = request(get, origin('/metadata/documents/42'), http(1, 1), [accept-'application/json'], empty, []),
		route_metadata_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, x_route_summary, 'Show document'),
		body(Response, content('application/json', json({kind-route_metadata, format-json}))).

	test(http_router_handle_2_25, deterministic) :-
		Request = request(get, origin('/reports/2026/summary'), http(1, 1), [], empty, []),
		advanced_path_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(report_2026))).

	test(http_router_handle_2_26, deterministic) :-
		Request = request(get, origin('/reports/today/summary'), http(1, 1), [], empty, []),
		advanced_path_http_router::handle(Request, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('text/plain', text('Not Found'))).

	test(http_router_handle_2_27, deterministic) :-
		Request = request(get, origin('/scrubbed'), http(1, 1), [], empty, [route(stale), path_params([id-'legacy']), open_api_probe(true), automatic_options(true), effective_methods([get, options]), response_media_type('application/vnd.stale+json')]),
		property_scrubbing_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text(scrubbed))).

	test(http_router_handle_2_28, deterministic) :-
		Request = request(get, origin('/bad-request/default'), http(1, 1), [], empty, []),
		parameter_validation_http_router::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('text/plain', text('Bad Request'))).

	test(http_router_handle_2_29, deterministic) :-
		Request = request(get, origin('/bad-request/custom'), http(1, 1), [], empty, []),
		parameter_validation_http_router::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		header(Response, x_router, custom),
		body(Response, content('text/plain', text(custom_bad_request))).

	test(http_router_handle_2_30, error(domain_error(http_router_test, invalid_handler_error))) :-
		Request = request(get, origin('/bad-request/error'), http(1, 1), [], empty, []),
		parameter_validation_http_router::handle(Request, _Response).

	test(http_router_open_api_3_01, deterministic(Operation == operation(
			show_item,
			get,
			'/open-api/items/{id}',
			'Show item',
			[parameter(id, path, 'Item identifier', true, schema_ref(item_id))],
			none,
			[
				response(200, 'Requested item', [media('application/json', schema_ref(item))]),
				response(default, 'Error response', [media('application/json', schema_ref(api_error))])
			],
			[description('Returns an item by id.'), tags([items])]
	))) :-
		open_api::operation(open_api_http_router, show_item, Operation).

	test(http_router_open_api_2_01, deterministic((
		ground(Document),
		open_api::validate_document(Document),
		json_field(Document, info, Info),
		json_field(Info, title, 'Router OpenAPI API'),
		json_field(Document, servers, [Server]),
		json_field(Server, url, 'https://api.example.com/router'),
		json_field(Document, paths, Paths),
		json_field(Paths, '/open-api/items/{id}', ItemPath),
		json_field(ItemPath, get, ShowItemOperation),
		json_field(ShowItemOperation, summary, 'Show item'),
		json_field(ShowItemOperation, parameters, [ItemParameter]),
		json_field(ItemParameter, name, id),
		json_field(Paths, '/open-api/documents', DocumentsPath),
		json_field(DocumentsPath, post, CreateDocumentOperation),
		json_field(CreateDocumentOperation, requestBody, RequestBody),
		json_field(RequestBody, required, @true),
		json_field(CreateDocumentOperation, responses, Responses),
		json_field(Responses, '201', CreatedResponse),
		json_field(CreatedResponse, content, ResponseContent),
		json_field(ResponseContent, 'application/json', _),
		json_field(Document, components, Components),
		json_field(Components, schemas, Schemas),
		json_field(Schemas, item, _),
		json_field(Schemas, document, _)
	))) :-
		open_api::document(open_api_http_router, Document).

	test(http_router_open_api_inference_3_01, deterministic(Operation == operation(
			show_message,
			get,
			'/inferred/messages/{id}',
			'Show inferred message',
			[parameter(id, path, 'Path parameter.', true, {type-string})],
			none,
			[
				response(200, 'Successful response', [
					media('application/json', {
						type-object,
						properties-{
							id-{type-string},
							title-{type-string}
						},
						required-[id, title],
						additionalProperties- @false
					}),
					media('text/plain', {type-string})
				])
			],
			[tags([inferred])]
	))) :-
		open_api::operation(inferred_open_api_http_router, show_message, Operation).

	test(http_router_open_api_inference_3_02, deterministic(Operation == operation(
			create_message,
			post,
			'/inferred/messages',
			'Create inferred message',
			[],
			request_body('Inferred request body', true, [
				media('application/json', {
					type-object,
					properties-{
						title-{type-string},
						published-{type-boolean}
					},
					required-[title, published],
					additionalProperties- @false
				})
			]),
			[
				response(201, 'Created', [
					media('application/json', {
						type-object,
						properties-{
							id-{type-string},
							title-{type-string},
							published-{type-boolean}
						},
						required-[id, title, published],
						additionalProperties- @false
					})
				])
			],
			[tags([inferred])]
	))) :-
		open_api::operation(inferred_open_api_http_router, create_message, Operation).

	test(http_router_open_api_inference_2_01, deterministic((
		ground(Document),
		open_api::validate_document(Document),
		json_field(Document, paths, Paths),
		json_field(Paths, '/inferred/messages/{id}', ItemPath),
		json_field(ItemPath, get, ShowMessageOperation),
		json_field(ShowMessageOperation, responses, ShowResponses),
		json_field(ShowResponses, '200', ShowResponse),
		json_field(ShowResponse, content, ShowContent),
		json_field(ShowContent, 'application/json', _),
		json_field(ShowContent, 'text/plain', _),
		json_field(Paths, '/inferred/messages', MessagesPath),
		json_field(MessagesPath, post, CreateMessageOperation),
		json_field(CreateMessageOperation, requestBody, RequestBody),
		json_field(RequestBody, content, RequestBodyContent),
		json_field(RequestBodyContent, 'application/json', RequestBodyMedia),
		json_field(RequestBodyMedia, schema, RequestSchema),
		json_field(RequestSchema, required, [title, published])
	))) :-
		open_api::document(inferred_open_api_http_router, Document).

	test(http_router_open_api_inference_3_03, deterministic(Operation == operation(
			update_message,
			put,
			'/examples/messages/{id}',
			'Update example message',
			[parameter(id, path, 'Path parameter.', true, {type-integer})],
			request_body('Inferred request body', false, [
				media('application/json', {
					type-object,
					properties-{
						title-{type-string}
					},
					required-[title],
					additionalProperties- @false
				})
			]),
			[
				response(200, 'Successful response', [
					media('application/json', {
						type-object,
						properties-{
							id-{type-integer},
							title-{type-string}
						},
						required-[id, title],
						additionalProperties- @false
					})
				]),
				response(400, 'Bad Request', [
					media('application/json', {
						type-object,
						properties-{
							error-{type-string}
						},
						required-[error],
						additionalProperties- @false
					})
				])
			],
			[tags([examples])]
	))) :-
		open_api::operation(example_open_api_http_router, update_message, Operation).

	test(http_router_open_api_inference_3_04, deterministic(Operation == operation(
			show_nullable_message,
			get,
			'/examples/messages/nullable',
			'Show example nullable message',
			[],
			none,
			[
				response(200, 'Successful response', [
					media('application/json', {
						type-object,
						properties-{
							id-{type-string},
							subtitle-{}
						},
						required-[id, subtitle],
						additionalProperties- @false
					})
				])
			],
			[tags([examples])]
	))) :-
		open_api::operation(example_open_api_http_router, show_nullable_message, Operation).

	json_field({Pairs}, Key, Value) :-
		json_pair(Pairs, Key, Value).

	json_pair((Pair, Rest), Key, Value) :-
		!,
		(	pair_key_value(Pair, Key, Value) ->
			true
		;	json_pair(Rest, Key, Value)
		).
	json_pair(Pair, Key, Value) :-
		pair_key_value(Pair, Key, Value).

	pair_key_value(Key-Value, Key, Value) :-
		!.
	pair_key_value(Key=Value, Key, Value) :-
		!.
	pair_key_value(':'(Key, Value), Key, Value).

:- end_object.
