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


:- object(sample_rest_application,
	implements(http_handler_protocol),
	imports(rest)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'Sample REST application object used by the rest library tests.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the REST endpoints exercised by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- public(route_metadata_descriptor/2).
	:- mode(route_metadata_descriptor(+atom, -list(compound)), zero_or_one).
	:- info(route_metadata_descriptor/2, [
		comment is 'Calls the imported route metadata hook so the rest library tests can verify metadata lookup for uniquely identified endpoints.',
		argnames is ['Id', 'Metadata']
	]).

	:- public(route_produces_descriptor/2).
	:- mode(route_produces_descriptor(+atom, -list(atom)), zero_or_one).
	:- info(route_produces_descriptor/2, [
		comment is 'Calls the imported route produces hook so the rest library tests can verify produces lookup for uniquely identified endpoints.',
		argnames is ['Id', 'MediaTypes']
	]).

	:- protected(show_item/2).
	:- mode(show_item(+compound, -term), one_or_error).
	:- info(show_item/2, [
		comment is 'Action used by the sample REST application for the ``GET /items/{id}`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(create_item/2).
	:- mode(create_item(+compound, -term), one_or_error).
	:- info(create_item/2, [
		comment is 'Action used by the sample REST application for the ``POST /items`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(search_items/2).
	:- mode(search_items(+compound, -term), one_or_error).
	:- info(search_items/2, [
		comment is 'Action used by the sample REST application for the ``GET /search`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(missing_item/2).
	:- mode(missing_item(+compound, -term), one_or_error).
	:- info(missing_item/2, [
		comment is 'Action used by the sample REST application for the ``GET /missing`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(delete_item/2).
	:- mode(delete_item(+compound, -term), one_or_error).
	:- info(delete_item/2, [
		comment is 'Action used by the sample REST application for the ``DELETE /items/{id}`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(conflict_item/2).
	:- mode(conflict_item(+compound, -term), one_or_error).
	:- info(conflict_item/2, [
		comment is 'Action used by the sample REST application for the ``POST /items/conflict`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(broken_item/2).
	:- mode(broken_item(+compound, -term), zero_or_one).
	:- info(broken_item/2, [
		comment is 'Action used by the sample REST application for the ``GET /broken`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(show_implicit_item/2).
	:- mode(show_implicit_item(+compound, -term), one_or_error).
	:- info(show_implicit_item/2, [
		comment is 'Action used by the sample REST application for the ``GET /implicit/items/42`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(show_item, get, '/items/{id}', show_item, [
		summary('Show item'),
		tags([items]),
		produces(['application/json'])
	]).

	endpoint(create_item, post, '/items', create_item, [
		summary('Create item'),
		tags([items]),
		produces(['application/json'])
	]).

	endpoint(search_items, get, '/search', search_items, [
		summary('Search items'),
		tags([items]),
		produces(['application/json'])
	]).

	endpoint(missing_item, get, '/missing', missing_item, [
		summary('Missing item'),
		produces(['application/problem+json'])
	]).

	endpoint(delete_item, delete, '/items/{id}', delete_item, []).

	endpoint(conflict_item, post, '/items/conflict', conflict_item, [
		summary('Conflict item'),
		produces(['application/problem+json'])
	]).

	endpoint(broken_item, get, '/broken', broken_item, [
		summary('Broken item'),
		produces(['application/problem+json'])
	]).

	endpoint(show_implicit_item, get, '/implicit/items/42', show_implicit_item, []).

	route_metadata_descriptor(Id, Metadata) :-
		::route_metadata(Id, Metadata).

	route_produces_descriptor(Id, MediaTypes) :-
		::route_produces(Id, MediaTypes).

	show_item(Request, ok({id-Id, name-'Widget'})) :-
		::path_parameter(Request, id, Id).

	create_item(Request, created('/items/100', {id-'100', title-Title})) :-
		::json_body(Request, {title-Title}).

	search_items(Request, ok({query-Name})) :-
		::query_parameter(Request, name, Name).

	missing_item(_Request, problem(404, 'urn:logtalk:not-found', 'Not Found', 'Item not found')).

	delete_item(Request, no_content) :-
		::path_parameter(Request, id, _Id).

	conflict_item(_Request, _Result) :-
		throw(problem(409, 'urn:logtalk:conflict', 'Conflict', 'Item already exists')).

	broken_item(_Request, _Result) :-
		fail.

	show_implicit_item(_Request, ok({id-'42', format-default})).

:- end_object.


	:- object(json_body_shapes_rest_application,
		implements(http_handler_protocol),
		imports(rest)).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-03,
			comment is 'REST application object used by the rest library tests to exercise JSON object and array body helpers.'
		]).

		:- protected(endpoint/5).
		:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
		:- info(endpoint/5, [
			comment is 'Declares the REST endpoints used by the JSON body helper tests.',
			argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
		]).

		:- protected(echo_object/2).
		:- mode(echo_object(+compound, -compound), one_or_error).
		:- info(echo_object/2, [
			comment is 'Action used by the JSON body helper REST application for the ``POST /json/object`` endpoint.',
			argnames is ['Request', 'Result']
		]).

		:- protected(echo_array/2).
		:- mode(echo_array(+compound, -compound), one_or_error).
		:- info(echo_array/2, [
			comment is 'Action used by the JSON body helper REST application for the ``POST /json/array`` endpoint.',
			argnames is ['Request', 'Result']
		]).

		endpoint(echo_object, post, '/json/object', echo_object, [produces(['application/json'])]).
		endpoint(echo_array, post, '/json/array', echo_array, [produces(['application/json'])]).

		echo_object(Request, ok(JSONObject)) :-
			::json_object_body(Request, JSONObject).

		echo_array(Request, ok(JSONArray)) :-
			::json_array_body(Request, JSONArray).

	:- end_object.


:- object(result_variants_rest_application,
	implements(http_handler_protocol),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise additional result and error normalization branches.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the REST endpoints used by the result variants tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(wrapped_response/2).
	:- mode(wrapped_response(+compound, -term), one_or_error).
	:- info(wrapped_response/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/wrapped`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(accepted_json/2).
	:- mode(accepted_json(+compound, -term), one_or_error).
	:- info(accepted_json/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/accepted`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(header_json/2).
	:- mode(header_json(+compound, -term), one_or_error).
	:- info(header_json/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/unauthorized`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(invalid_result/2).
	:- mode(invalid_result(+compound, -term), one_or_error).
	:- info(invalid_result/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/invalid`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(context_problem_error/2).
	:- mode(context_problem_error(+compound, -term), one_or_error).
	:- info(context_problem_error/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/unprocessable`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(generic_error/2).
	:- mode(generic_error(+compound, -term), one_or_error).
	:- info(generic_error/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/error`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(thrown_ok_result/2).
	:- mode(thrown_ok_result(+compound, -term), one_or_error).
	:- info(thrown_ok_result/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/thrown/ok`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(thrown_created_result/2).
	:- mode(thrown_created_result(+compound, -term), one_or_error).
	:- info(thrown_created_result/2, [
		comment is 'Action used by the result variants REST application for the ``POST /variants/thrown/created`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(thrown_no_content_result/2).
	:- mode(thrown_no_content_result(+compound, -term), one_or_error).
	:- info(thrown_no_content_result/2, [
		comment is 'Action used by the result variants REST application for the ``DELETE /variants/thrown/no-content`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(thrown_header_json_result/2).
	:- mode(thrown_header_json_result(+compound, -term), one_or_error).
	:- info(thrown_header_json_result/2, [
		comment is 'Action used by the result variants REST application for the ``GET /variants/thrown/headers`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(wrapped_response, get, '/variants/wrapped', wrapped_response, [produces(['application/json'])]).
	endpoint(accepted_json, get, '/variants/accepted', accepted_json, [produces(['application/json'])]).
	endpoint(header_json, get, '/variants/unauthorized', header_json, [produces(['application/json'])]).
	endpoint(invalid_result, get, '/variants/invalid', invalid_result, []).
	endpoint(context_problem_error, get, '/variants/unprocessable', context_problem_error, [produces(['application/problem+json'])]).
	endpoint(generic_error, get, '/variants/error', generic_error, [produces(['application/problem+json'])]).
	endpoint(thrown_ok_result, get, '/variants/thrown/ok', thrown_ok_result, [produces(['application/json'])]).
	endpoint(thrown_created_result, post, '/variants/thrown/created', thrown_created_result, [produces(['application/json'])]).
	endpoint(thrown_no_content_result, delete, '/variants/thrown/no-content', thrown_no_content_result, []).
	endpoint(thrown_header_json_result, get, '/variants/thrown/headers', thrown_header_json_result, [produces(['application/json'])]).

	wrapped_response(Request, response(Response)) :-
		::json_response(Request, status(200, 'OK'), {wrapped- @true}, Response).

	accepted_json(_Request, json(202, {accepted- @true})).

	header_json(_Request, json(401, [x_auth_required-yes], {error-unauthorized})).

	thrown_ok_result(_Request, _Result) :-
		throw(ok({thrown- @true})).

	thrown_created_result(_Request, _Result) :-
		throw(created('/variants/thrown/created/100', {id-'100', created- @true})).

	thrown_no_content_result(_Request, _Result) :-
		throw(no_content).

	thrown_header_json_result(_Request, _Result) :-
		throw(json(202, [x_thrown-yes], {accepted- @true})).

	invalid_result(_Request, unexpected_result).

	context_problem_error(_Request, _Result) :-
		throw(error(problem(422, 'urn:logtalk:unprocessable', 'Unprocessable Content', 'Payload rejected'), context(result_variants_rest_application, context_problem_error/2))).

	generic_error(_Request, _Result) :-
		throw(error(unexpected_failure, context(result_variants_rest_application, generic_error/2))).

:- end_object.


	:- object(action_error_hooks_rest_application,
		implements(http_handler_protocol),
		imports(rest)).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-03,
			comment is 'REST application object used by the rest library tests to exercise action failure and unsupported exception response hooks.'
		]).

		:- protected(endpoint/5).
		:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
		:- info(endpoint/5, [
			comment is 'Declares the REST endpoints used by the action hook tests.',
			argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
		]).

		:- protected(rest_action_failure_response/3).
		:- mode(rest_action_failure_response(+compound, +atom, -compound), zero_or_one).
		:- info(rest_action_failure_response/3, [
			comment is 'Customizes the response returned when the failing action hook test endpoint fails without throwing an exception.',
			argnames is ['Request', 'Id', 'Response']
		]).

		:- protected(rest_action_error_response/4).
		:- mode(rest_action_error_response(+compound, +atom, +term, -compound), zero_or_one).
		:- info(rest_action_error_response/4, [
			comment is 'Customizes the response returned when the erroring action hook test endpoint throws an unsupported exception.',
			argnames is ['Request', 'Id', 'Error', 'Response']
		]).

		:- protected(failing_item/2).
		:- mode(failing_item(+compound, -term), zero_or_one).
		:- info(failing_item/2, [
			comment is 'Action used by the action hook REST application for the ``GET /hooks/failure`` endpoint.',
			argnames is ['Request', 'Result']
		]).

		:- protected(erroring_item/2).
		:- mode(erroring_item(+compound, -term), one_or_error).
		:- info(erroring_item/2, [
			comment is 'Action used by the action hook REST application for the ``GET /hooks/error`` endpoint.',
			argnames is ['Request', 'Result']
		]).

		endpoint(failing_item, get, '/hooks/failure', failing_item, []).
		endpoint(erroring_item, get, '/hooks/error', erroring_item, []).

		rest_action_failure_response(Request, failing_item, Response) :-
			::problem_response(Request, 409, 'urn:logtalk:action-failed', 'Conflict', 'Custom REST action failure.', Response).

		rest_action_error_response(Request, erroring_item, error(unexpected_failure, _Context), Response) :-
			::problem_response(Request, 422, 'urn:logtalk:action-error', 'Unprocessable Content', 'Custom REST action error.', Response).

		failing_item(_Request, _Result) :-
			fail.

		erroring_item(_Request, _Result) :-
			throw(error(unexpected_failure, context(action_error_hooks_rest_application, erroring_item/2))).

	:- end_object.


:- object(invalid_request_validation_rest_application,
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise request validation exception handling.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the invalid request validation endpoint used by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(open_api_validate_request/1).
	:- mode(open_api_validate_request(?atom), zero_or_more).
	:- info(open_api_validate_request/1, [
		comment is 'Enables automatic OpenAPI request contract validation for the invalid request validation fixture endpoint.',
		argnames is ['Id']
	]).

	:- protected(create_item/2).
	:- mode(create_item(+compound, -term), one_or_error).
	:- info(create_item/2, [
		comment is 'Action used by the invalid request validation REST application for the ``POST /validation-exception/request`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(create_item, post, '/validation-exception/request', create_item, [
		summary('Broken request contract'),
		request_body(request_body('Broken request contract', true, [
			media('application/json', schema_ref(missing_request_schema))
		])),
		produces(['application/json'])
	]).

	open_api_validate_request(create_item).

	create_item(_Request, ok({ok- @true})).

:- end_object.


:- object(invalid_response_validation_rest_application,
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise response validation exception handling.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the invalid response validation endpoint used by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(open_api_validate_response/1).
	:- mode(open_api_validate_response(?atom), zero_or_more).
	:- info(open_api_validate_response/1, [
		comment is 'Enables automatic OpenAPI response contract validation for the invalid response validation fixture endpoint.',
		argnames is ['Id']
	]).

	:- protected(show_item/2).
	:- mode(show_item(+compound, -term), one_or_error).
	:- info(show_item/2, [
		comment is 'Action used by the invalid response validation REST application for the ``GET /validation-exception/response/{id}`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(show_item, get, '/validation-exception/response/{id}', show_item, [
		summary('Broken response contract'),
		responses([
			response(200, 'Broken response contract', [
				media('application/json', schema_ref(missing_response_schema))
			])
		]),
		produces(['application/json'])
	]).

	open_api_validate_response(show_item).

	show_item(Request, ok({id-Id})) :-
		::path_parameter(Request, id, Id).

:- end_object.


:- object(open_api_rest_application,
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise OpenAPI derivation through endpoint descriptors.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the REST endpoint used by the OpenAPI rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(show_item/2).
	:- mode(show_item(+compound, -term), one_or_error).
	:- info(show_item/2, [
		comment is 'Action used by the OpenAPI REST application for the ``GET /items/{id}`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(show_item, get, '/items/{id}', show_item, [
		summary('Show item'),
		description('Returns an item by id.'),
		tags([items]),
		produces(['application/json'])
	]).

	show_item(Request, ok({id-Id, name-'Widget'})) :-
		::path_parameter(Request, id, Id).

:- end_object.


:- object(validated_rest_application,
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise automatic OpenAPI request and response contract validation hooks.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the validated REST endpoints used by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(open_api_validate_request/1).
	:- mode(open_api_validate_request(?atom), zero_or_more).
	:- info(open_api_validate_request/1, [
		comment is 'Enables automatic OpenAPI request contract validation for selected endpoints.',
		argnames is ['Id']
	]).

	:- protected(open_api_validate_response/1).
	:- mode(open_api_validate_response(?atom), zero_or_more).
	:- info(open_api_validate_response/1, [
		comment is 'Enables automatic OpenAPI response contract validation for selected endpoints.',
		argnames is ['Id']
	]).

	:- protected(create_contract_item/2).
	:- mode(create_contract_item(+compound, -term), one_or_error).
	:- info(create_contract_item/2, [
		comment is 'Action used by the validated REST application for the ``POST /contract/items`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(show_contract_item/2).
	:- mode(show_contract_item(+compound, -term), one_or_error).
	:- info(show_contract_item/2, [
		comment is 'Action used by the validated REST application for the ``GET /contract/items/{id}`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(create_contract_item, post, '/contract/items', create_contract_item, [
		summary('Create contract item'),
		tags([contract]),
		request_body(request_body('Contract item payload', true, [
			media('application/json', {
				type-object,
				properties-{
					title-{type-string}
				},
				required-[title],
				additionalProperties- @false
			})
		])),
		responses([
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
		]),
		produces(['application/json'])
	]).

	endpoint(show_contract_item, get, '/contract/items/{id}', show_contract_item, [
		summary('Show contract item'),
		tags([contract]),
		responses([
			response(200, 'Successful response', [
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
		]),
		produces(['application/json'])
	]).

	open_api_validate_request(create_contract_item).
	open_api_validate_request(show_contract_item).

	open_api_validate_response(show_contract_item).

	create_contract_item(Request, created('/contract/items/100', {id-'100', title-Title})) :-
		::json_body(Request, {title-Title}).

	show_contract_item(Request, ok({id-Id})) :-
		::path_parameter(Request, id, Id).

:- end_object.


:- object(vendor_media_type_rest_application,
	implements(http_handler_protocol),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise negotiated vendor JSON media types.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the vendor media type REST endpoints used by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(show_vendor_item/2).
	:- mode(show_vendor_item(+compound, -term), one_or_error).
	:- info(show_vendor_item/2, [
		comment is 'Action used by the vendor media type REST application for the ``GET /vendor/items/42`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(create_vendor_item/2).
	:- mode(create_vendor_item(+compound, -term), one_or_error).
	:- info(create_vendor_item/2, [
		comment is 'Action used by the vendor media type REST application for the ``POST /vendor/items`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(accepted_vendor_item/2).
	:- mode(accepted_vendor_item(+compound, -term), one_or_error).
	:- info(accepted_vendor_item/2, [
		comment is 'Action used by the vendor media type REST application for the ``GET /vendor/items/accepted`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(missing_vendor_item/2).
	:- mode(missing_vendor_item(+compound, -term), one_or_error).
	:- info(missing_vendor_item/2, [
		comment is 'Action used by the vendor media type REST application for the ``GET /vendor/items/missing`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(show_vendor_item, get, '/vendor/items/42', show_vendor_item, [produces(['application/vnd.example+json'])]).
	endpoint(create_vendor_item, post, '/vendor/items', create_vendor_item, [produces(['application/vnd.example+json'])]).
	endpoint(accepted_vendor_item, get, '/vendor/items/accepted', accepted_vendor_item, [produces(['application/vnd.example+json'])]).
	endpoint(missing_vendor_item, get, '/vendor/items/missing', missing_vendor_item, [produces(['application/vnd.example+json'])]).

	show_vendor_item(_Request, ok({id-'42', format-vendor})).

	create_vendor_item(Request, created('/vendor/items/100', {id-'100', title-Title})) :-
		::json_body(Request, {title-Title}).

	accepted_vendor_item(_Request, json(202, {accepted- @true})).

	missing_vendor_item(_Request, problem(404, 'urn:logtalk:vendor-missing', 'Not Found', 'Vendor item not found')).

:- end_object.


:- object(mixed_media_type_rest_application,
	implements(http_handler_protocol),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise fallback from negotiated non-JSON media types when using JSON response helpers.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the mixed-media endpoint used by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(show_mixed_item/2).
	:- mode(show_mixed_item(+compound, -term), one_or_error).
	:- info(show_mixed_item/2, [
		comment is 'Action used by the mixed-media REST application for the ``GET /mixed/items/42`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(show_mixed_item, get, '/mixed/items/42', show_mixed_item, [produces(['application/json', 'text/plain'])]).

	show_mixed_item(_Request, ok({id-'42', format-mixed})).

:- end_object.


:- object(duplicate_endpoint_id_rest_application,
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-03,
		comment is 'REST application object used by the rest library tests to exercise duplicate endpoint identifier failures.'
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Declares the duplicate endpoint identifiers used by the rest library tests.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- public(route_descriptor/4).
	:- mode(route_descriptor(?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(route_descriptor/4, [
		comment is 'Calls the imported route descriptor predicate so the rest library tests can exercise duplicate endpoint identifier failures on the earliest routing path.',
		argnames is ['Id', 'Method', 'Path', 'Handler']
	]).

	:- public(route_metadata_descriptor/2).
	:- mode(route_metadata_descriptor(+atom, -list(compound)), zero_or_one).
	:- info(route_metadata_descriptor/2, [
		comment is 'Calls the imported route metadata hook so the rest library tests can exercise duplicate endpoint identifier failures during metadata lookup.',
		argnames is ['Id', 'Metadata']
	]).

	:- public(route_produces_descriptor/2).
	:- mode(route_produces_descriptor(+atom, -list(atom)), zero_or_one).
	:- info(route_produces_descriptor/2, [
		comment is 'Calls the imported route produces hook so the rest library tests can exercise duplicate endpoint identifier failures during produces lookup.',
		argnames is ['Id', 'MediaTypes']
	]).

	:- protected(first_duplicate_item/2).
	:- mode(first_duplicate_item(+compound, -term), one_or_error).
	:- info(first_duplicate_item/2, [
		comment is 'Action used by the duplicate endpoint id REST application for the ``GET /duplicates/one`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	:- protected(second_duplicate_item/2).
	:- mode(second_duplicate_item(+compound, -term), one_or_error).
	:- info(second_duplicate_item/2, [
		comment is 'Action used by the duplicate endpoint id REST application for the ``GET /duplicates/two`` endpoint.',
		argnames is ['Request', 'Result']
	]).

	endpoint(duplicate_item, get, '/duplicates/one', first_duplicate_item, []).
	endpoint(duplicate_item, get, '/duplicates/two', second_duplicate_item, []).

	route_descriptor(Id, Method, Path, Handler) :-
		::route(Id, Method, Path, Handler).

	route_metadata_descriptor(Id, Metadata) :-
		::route_metadata(Id, Metadata).

	route_produces_descriptor(Id, MediaTypes) :-
		::route_produces(Id, MediaTypes).

	first_duplicate_item(_Request, ok({which-first})).

	second_duplicate_item(_Request, ok({which-second})).

:- end_object.


	:- object(rest_digest_auth_test_verifier,
		implements(http_digest_verifier_protocol)).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-03,
			comment is 'Local Digest verifier used by the rest library tests to exercise router companion integration.'
		]).

		:- uses(user, [
			atomic_list_concat/3
		]).

		ha1(sha256, Realm, Username, HA1) :-
			credential_password(Realm, Username, Password),
			credential_material_codes(Username, Realm, Password, Codes),
			sha256::hash(Codes, HA1).

		credential_password('test-realm', 'Mufasa', 'Circle Of Life').

		credential_material_codes(Username, Realm, Password, Codes) :-
			atomic_list_concat([Username, Realm, Password], ':', Material),
			atom_codes(Material, Codes).

	:- end_object.


	:- object(digest_auth_rest_application,
		implements(http_handler_protocol),
		imports([
			rest,
			http_router_digest_auth(
				rest_digest_auth_test_verifier,
				[realm('test-realm'), algorithm(sha256), accepted_algorithms([sha256]), qops([auth]), nonce_secret('secret'), current_time(1700000000)],
				[nextnonce('next-nonce')]
			)
		])).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-06-03,
			comment is 'REST application object used by the rest library tests to exercise Digest-auth router companion integration.'
		]).

		:- protected(endpoint/5).
		:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
		:- info(endpoint/5, [
			comment is 'Declares the REST endpoints used by the Digest-auth integration tests.',
			argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
		]).

		:- public(route_metadata_descriptor/2).
		:- mode(route_metadata_descriptor(+atom, -list(compound)), zero_or_one).
		:- info(route_metadata_descriptor/2, [
			comment is 'Calls the imported route metadata hook so the rest library tests can verify Digest-auth metadata passthrough.',
			argnames is ['Id', 'Metadata']
		]).

		:- protected(authorize_routed_request/2).
		:- mode(authorize_routed_request(+compound, -compound), one_or_error).
		:- info(authorize_routed_request/2, [
			comment is 'Route-authorization hook that delegates Digest-auth checks to the imported router helper category.',
			argnames is ['Request', 'Action']
		]).

		:- protected(response_middleware/2).
		:- mode(response_middleware(?atom, ?atom), zero_or_more).
		:- info(response_middleware/2, [
			comment is 'Response middleware hook that registers Digest ``Authentication-Info`` decoration for successfully verified requests.',
			argnames is ['Id', 'Handler']
		]).

		:- protected(show_secret/2).
		:- mode(show_secret(+compound, -compound), one_or_error).
		:- info(show_secret/2, [
			comment is 'Action used by the Digest-auth REST application for the ``GET /secret`` endpoint.',
			argnames is ['Request', 'Result']
		]).

		endpoint(show_secret, get, '/secret', show_secret, [digest_auth([]), produces(['application/json'])]).

		route_metadata_descriptor(Id, Metadata) :-
			::route_metadata(Id, Metadata).

		authorize_routed_request(Request, Action) :-
			^^authorize_digest_auth_request(Request, Action).

		response_middleware(digest_authentication_info, add_digest_authentication_info).

		show_secret(Request, ok({user-Username})) :-
			http_core::property(Request, digest_username(Username)).

	:- end_object.
