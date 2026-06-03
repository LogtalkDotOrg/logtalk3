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
		date is 2026-06-03,
		comment is 'Unit tests for the "rest" library.'
	]).

	:- uses(http_core, [
		body/2, header/3, status/2
	]).

	cover(rest).

	test(rest_handle_2_01, deterministic) :-
		Request = request(get, origin('/items/42'), http(1, 1), [], empty, []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-'42', name-'Widget'}))).

	test(rest_handle_2_02, deterministic) :-
		Request = request(post, origin('/items'), http(1, 1), [], content('application/json', json({title-'Guide'})), []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(201, 'Created')),
		header(Response, location, '/items/100'),
		body(Response, content('application/json', json({id-'100', title-'Guide'}))).

	test(rest_handle_2_03, deterministic) :-
		Request = request(get, origin('/search', 'name=Ada'), http(1, 1), [], empty, [query_pairs([name-'Ada'])]),
		sample_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({query-'Ada'}))).

	test(rest_handle_2_04, deterministic) :-
		Request = request(get, origin('/missing'), http(1, 1), [], empty, []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:not-found', title-'Not Found', detail-'Item not found', status-404}))).

	test(rest_handle_2_05, deterministic) :-
		Request = request(delete, origin('/items/42'), http(1, 1), [], empty, []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(204, 'No Content')),
		body(Response, empty).

	test(rest_handle_2_06, deterministic) :-
		Request = request(post, origin('/items/conflict'), http(1, 1), [], empty, []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(409, 'Conflict')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:conflict', title-'Conflict', detail-'Item already exists', status-409}))).

	test(rest_handle_2_07, deterministic) :-
		Request = request(get, origin('/broken'), http(1, 1), [], empty, []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(500, 'Internal Server Error')),
		body(Response, content('application/problem+json', json({type-'about:blank', title-'Internal Server Error', detail-'REST action failed.', status-500}))).

	test(rest_helper_3_01, deterministic) :-
		Request = request(post, origin('/helpers'), http(1, 1), [accept-'text/plain'], content('application/json', json({name-'Ada'})), []),
		sample_rest_application::request_header(Request, accept, 'text/plain'),
		sample_rest_application::request_body(Request, content('application/json', json({name-'Ada'}))).

	test(rest_helper_2_01, deterministic) :-
		Request = request(post, origin('/helpers/form'), http(1, 1), [], content('application/x-www-form-urlencoded', form([name-'Ada', city-'Porto'])), []),
		sample_rest_application::form_body(Request, [name-'Ada', city-'Porto']).

	test(rest_helper_2_02, deterministic) :-
		Request = request(post, origin('/helpers/text'), http(1, 1), [], content('text/plain', text('hello')), []),
		sample_rest_application::text_body(Request, 'hello').

	test(rest_helper_2_03, deterministic) :-
		Request = request(post, origin('/helpers/binary'), http(1, 1), [], content('application/octet-stream', binary([1, 2, 3])), []),
		sample_rest_application::binary_body(Request, [1, 2, 3]).

	test(rest_helper_2_04, deterministic(Name == 'Ada')) :-
		Request = request(get, origin('/search', 'name=Ada&name=Grace'), http(1, 1), [], empty, [query_pairs([name-'Ada', name-'Grace'])]),
		sample_rest_application::query_parameter(Request, name, Name).

	test(rest_json_body_shape_2_01, deterministic(JSONObject == {name-'Ada'})) :-
		Request = request(post, origin('/helpers/object'), http(1, 1), [], content('application/json', json({name-'Ada'})), []),
		sample_rest_application::json_object_body(Request, JSONObject).

	test(rest_json_body_shape_2_02, deterministic(JSONArray == ['Ada', 'Grace'])) :-
		Request = request(post, origin('/helpers/array'), http(1, 1), [], content('application/json', json(['Ada', 'Grace'])), []),
		sample_rest_application::json_array_body(Request, JSONArray).

	test(rest_json_body_shape_handle_2_01, deterministic) :-
		Request = request(post, origin('/json/object'), http(1, 1), [], content('application/json', json({name-'Ada'})), []),
		json_body_shapes_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({name-'Ada'}))).

	test(rest_json_body_shape_handle_2_02, deterministic) :-
		Request = request(post, origin('/json/array'), http(1, 1), [], content('application/json', json([1, 2, 3])), []),
		json_body_shapes_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json([1, 2, 3]))).

	test(rest_json_body_shape_handle_2_03, deterministic) :-
		Request = request(post, origin('/json/object'), http(1, 1), [], content('application/json', json([name-'Ada'])), []),
		json_body_shapes_rest_application::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:invalid-request-body', title-'Bad Request', detail-'Expected JSON object request body.', status-400}))).

	test(rest_json_body_shape_handle_2_04, deterministic) :-
		Request = request(post, origin('/json/array'), http(1, 1), [], content('application/json', json({name-'Ada'})), []),
		json_body_shapes_rest_application::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:invalid-request-body', title-'Bad Request', detail-'Expected JSON array request body.', status-400}))).

	test(rest_handle_2_08, deterministic) :-
		Request = request(get, origin('/variants/wrapped'), http(1, 1), [], empty, []),
		result_variants_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({wrapped- @true}))).

	test(rest_handle_2_09, deterministic) :-
		Request = request(get, origin('/variants/accepted'), http(1, 1), [], empty, []),
		result_variants_rest_application::handle(Request, Response),
		status(Response, status(202, 'Accepted')),
		body(Response, content('application/json', json({accepted- @true}))).

	test(rest_handle_2_10, deterministic) :-
		Request = request(get, origin('/variants/unauthorized'), http(1, 1), [], empty, []),
		result_variants_rest_application::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		header(Response, x_auth_required, yes),
		body(Response, content('application/json', json({error-unauthorized}))).

	test(rest_handle_2_11, error(domain_error(rest_result, unexpected_result))) :-
		Request = request(get, origin('/variants/invalid'), http(1, 1), [], empty, []),
		result_variants_rest_application::handle(Request, _Response).

	test(rest_handle_2_12, deterministic) :-
		Request = request(get, origin('/variants/unprocessable'), http(1, 1), [], empty, []),
		result_variants_rest_application::handle(Request, Response),
		status(Response, status(422, 'Unprocessable Content')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:unprocessable', title-'Unprocessable Content', detail-'Payload rejected', status-422}))).

	test(rest_handle_2_13, deterministic) :-
		Request = request(get, origin('/variants/error'), http(1, 1), [], empty, []),
		result_variants_rest_application::handle(Request, Response),
		status(Response, status(500, 'Internal Server Error')),
		body(Response, content('application/problem+json', json({type-'about:blank', title-'Internal Server Error', detail-'Unhandled REST action error.', status-500}))).

	test(rest_handle_2_14, deterministic) :-
		Request = request(get, origin('/vendor/items/42'), http(1, 1), [accept-'application/vnd.example+json'], empty, []),
		vendor_media_type_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/vnd.example+json', json({id-'42', format-vendor}))).

	test(rest_handle_2_15, deterministic) :-
		Request = request(post, origin('/vendor/items'), http(1, 1), [accept-'application/vnd.example+json'], content('application/json', json({title-'Guide'})), []),
		vendor_media_type_rest_application::handle(Request, Response),
		status(Response, status(201, 'Created')),
		header(Response, location, '/vendor/items/100'),
		body(Response, content('application/vnd.example+json', json({id-'100', title-'Guide'}))).

	test(rest_handle_2_16, deterministic) :-
		Request = request(get, origin('/vendor/items/accepted'), http(1, 1), [accept-'application/vnd.example+json'], empty, []),
		vendor_media_type_rest_application::handle(Request, Response),
		status(Response, status(202, 'Accepted')),
		body(Response, content('application/vnd.example+json', json({accepted- @true}))).

	test(rest_handle_2_17, deterministic) :-
		Request = request(get, origin('/vendor/items/missing'), http(1, 1), [accept-'application/vnd.example+json'], empty, []),
		vendor_media_type_rest_application::handle(Request, Response),
		status(Response, status(404, 'Not Found')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:vendor-missing', title-'Not Found', detail-'Vendor item not found', status-404}))).

	test(rest_handle_2_18, error(domain_error(rest_endpoint_id, duplicate(duplicate_item)))) :-
		Request = request(get, origin('/duplicates/two'), http(1, 1), [], empty, []),
		duplicate_endpoint_id_rest_application::handle(Request, _Response).

	test(rest_handle_2_19, deterministic) :-
		Request = request(post, origin('/items'), http(1, 1), [], empty, []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:invalid-request-body', title-'Bad Request', detail-'Expected JSON request body.', status-400}))).

	test(rest_handle_2_20, deterministic) :-
		Request = request(post, origin('/items'), http(1, 1), [], content('text/plain', text('Guide')), []),
		sample_rest_application::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:invalid-request-body', title-'Bad Request', detail-'Expected JSON request body.', status-400}))).

	test(rest_handle_2_21, deterministic) :-
		Request = request(get, origin('/implicit/items/42'), http(1, 1), [], empty, [response_media_type('application/vnd.stale+json')]),
		sample_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-'42', format-default}))).

	test(rest_handle_2_22, deterministic) :-
		Request = request(get, origin('/mixed/items/42'), http(1, 1), [accept-'text/plain'], empty, []),
		mixed_media_type_rest_application::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({id-'42', format-mixed}))).

	test(rest_route_metadata_2_01, deterministic) :-
		sample_rest_application::route_metadata_descriptor(show_item, Metadata),
		Metadata == [summary('Show item'), tags([items])].

	test(rest_route_metadata_2_02, error(domain_error(rest_endpoint_id, duplicate(duplicate_item)))) :-
		duplicate_endpoint_id_rest_application::route_metadata_descriptor(duplicate_item, _Metadata).

	test(rest_route_produces_2_01, deterministic) :-
		sample_rest_application::route_produces_descriptor(show_item, MediaTypes),
		MediaTypes == ['application/json'].

	test(rest_route_produces_2_02, error(domain_error(rest_endpoint_id, duplicate(duplicate_item)))) :-
		duplicate_endpoint_id_rest_application::route_produces_descriptor(duplicate_item, _MediaTypes).

	test(rest_route_4_01, error(domain_error(rest_endpoint_id, duplicate(duplicate_item)))) :-
		duplicate_endpoint_id_rest_application::route_descriptor(_Id, _Method, _Path, _Handler).

	test(rest_open_api_duplicate_endpoint_id_2_01, error(domain_error(rest_endpoint_id, duplicate(duplicate_item)))) :-
		open_api::operation(duplicate_endpoint_id_rest_application, duplicate_item, _Operation).

	test(rest_json_response_4_01, deterministic) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, []),
		sample_rest_application::json_response(Request, 403, {error-forbidden}, Response403),
		status(Response403, status(403, 'Forbidden')),
		sample_rest_application::json_response(Request, 405, {error-method_not_allowed}, Response405),
		status(Response405, status(405, 'Method Not Allowed')),
		sample_rest_application::json_response(Request, 406, {error-not_acceptable}, Response406),
		status(Response406, status(406, 'Not Acceptable')).

	test(rest_json_response_4_02, error(instantiation_error)) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, []),
		sample_rest_application::json_response(Request, _Status, {error-missing_status}, _Response).

	test(rest_json_response_4_03, error(instantiation_error)) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, []),
		sample_rest_application::json_response(Request, status(_Code, 'Accepted'), {error-partial_status}, _Response).

	test(rest_json_response_4_04, error(domain_error(http_status, status(99, 'Invalid')))) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, []),
		sample_rest_application::json_response(Request, status(99, 'Invalid'), {error-invalid_status_code}, _Response).

	test(rest_json_response_4_05, error(domain_error(http_status, status(200, invalid_reason(detail))))) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, []),
		sample_rest_application::json_response(Request, status(200, invalid_reason(detail)), {error-invalid_status_reason}, _Response).

	test(rest_json_response_4_06, deterministic) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, [response_media_type('text/json')]),
		sample_rest_application::json_response(Request, 200, {ok- @true}, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/json', json({ok- @true}))).

	test(rest_json_response_5_01, deterministic) :-
		Request = request(get, origin('/helpers/statuses'), http(1, 1), [], empty, [response_media_type('text/plain')]),
		sample_rest_application::json_response(Request, 202, [x_test-yes], {accepted- @true}, Response),
		status(Response, status(202, 'Accepted')),
		header(Response, x_test, yes),
		body(Response, content('application/json', json({accepted- @true}))).

	test(rest_open_api_validation_2_01, deterministic) :-
		Request = request(post, origin('/contract/items'), http(1, 1), [], content('application/json', json({name-'Guide'})), []),
		validated_rest_application::handle(Request, Response),
		status(Response, status(422, 'Unprocessable Content')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:invalid-request-body', title-'Unprocessable Content', detail-'Request body does not conform to the OpenAPI contract.', status-422}))).

	test(rest_open_api_validation_2_02, deterministic) :-
		Request = request(post, origin('/contract/items'), http(1, 1), [], content('application/json', json({title-'Guide'})), []),
		validated_rest_application::handle(Request, Response),
		status(Response, status(201, 'Created')),
		header(Response, location, '/contract/items/100'),
		body(Response, content('application/json', json({id-'100', title-'Guide'}))).

	test(rest_open_api_validation_2_03, deterministic) :-
		Request = request(get, origin('/contract/items/42'), http(1, 1), [], empty, []),
		validated_rest_application::handle(Request, Response),
		status(Response, status(500, 'Internal Server Error')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:invalid-response', title-'Invalid Response', detail-'Response does not conform to the OpenAPI contract.', status-500}))).

	test(rest_open_api_validation_2_04, deterministic) :-
		Request = request(post, origin('/validation-exception/request'), http(1, 1), [], content('application/json', json({name-'Guide'})), []),
		invalid_request_validation_rest_application::handle(Request, Response),
		status(Response, status(500, 'Internal Server Error')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:request-validation-error', title-'OpenAPI Validation Error', detail-'OpenAPI request contract validation failed.', status-500}))).

	test(rest_open_api_validation_2_05, deterministic) :-
		Request = request(get, origin('/validation-exception/response/42'), http(1, 1), [], empty, []),
		invalid_response_validation_rest_application::handle(Request, Response),
		status(Response, status(500, 'Internal Server Error')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:response-validation-error', title-'OpenAPI Validation Error', detail-'OpenAPI response contract validation failed.', status-500}))).

	test(rest_open_api_validation_2_06, deterministic) :-
		Request = request(post, origin('/contract/items'), http(1, 1), [], content('application/json', json({name-'Guide'})), [open_api_probe(true)]),
		validated_rest_application::handle(Request, Response),
		status(Response, status(422, 'Unprocessable Content')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:invalid-request-body', title-'Unprocessable Content', detail-'Request body does not conform to the OpenAPI contract.', status-422}))).

	test(rest_open_api_validation_2_07, deterministic) :-
		Request = request(post, origin('/contract/items'), http(1, 1), [], content('text/plain', text('Guide')), []),
		validated_rest_application::handle(Request, Response),
		status(Response, status(415, 'Unsupported Media Type')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:unsupported-request-media-type', title-'Unsupported Media Type', detail-'Request media type is not supported by the OpenAPI contract.', status-415}))).

	test(rest_open_api_validation_2_08, deterministic) :-
		Request = request(post, origin('/contract/items'), http(1, 1), [], empty, []),
		validated_rest_application::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:invalid-request', title-'Invalid Request', detail-'Request does not conform to the OpenAPI contract.', status-400}))).

	test(rest_open_api_validation_2_09, deterministic) :-
		Request = request(get, origin('/contract/items/42'), http(1, 1), [], content('application/json', json({title-'Guide'})), []),
		validated_rest_application::handle(Request, Response),
		status(Response, status(400, 'Bad Request')),
		body(Response, content('application/problem+json', json({type-'urn:logtalk:open-api:invalid-request', title-'Invalid Request', detail-'Request does not conform to the OpenAPI contract.', status-400}))).

	test(rest_open_api_3_01, deterministic) :-
		open_api::operation(open_api_rest_application, show_item, Operation),
		Operation = operation(
			show_item,
			get,
			'/items/{id}',
			'Show item',
			[parameter(id, path, 'Path parameter.', true, {type-string})],
			none,
			[
				response(200, 'Successful response', [
					media('application/json', {
						type-object,
						properties-{
							id-{type-string},
							name-{type-string}
						},
						required-[id, name],
						additionalProperties- @false
					})
				])
			],
			[description('Returns an item by id.'), tags([items])]
		).

	test(rest_open_api_3_02, deterministic) :-
		open_api::operation(validated_rest_application, create_contract_item, Operation),
		Operation = operation(
			create_contract_item,
			post,
			'/contract/items',
			'Create contract item',
			[],
			request_body('Contract item payload', true, [
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
				response(201, 'Created', [
					media('application/json', {
						type-object,
						properties-{
							id-{type-string},
							title-{type-string}
						},
						required-[id, title],
						additionalProperties- @false
					})
				])
			],
			[tags([contract])]
		).

:- end_object.
