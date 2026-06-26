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


:- category(rest,
	extends(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-26,
		comment is 'REST authoring layer built on top of the HTTP router category.'
	]).

	:- public(path_parameter/3).
	:- mode(path_parameter(+compound, ?atom, ?term), zero_or_one).
	:- info(path_parameter/3, [
		comment is 'Returns a named path parameter extracted by the router from the current request using deterministic lookup.',
		argnames is ['Request', 'Name', 'Value']
	]).

	:- public(query_parameter/3).
	:- mode(query_parameter(+compound, ?atom, ?term), zero_or_one).
	:- info(query_parameter/3, [
		comment is 'Returns the first named query parameter from the current request when query pairs are available.',
		argnames is ['Request', 'Name', 'Value']
	]).

	:- public(request_header/3).
	:- mode(request_header(+compound, ?atom, ?term), zero_or_more).
	:- info(request_header/3, [
		comment is 'Returns a normalized request header value.',
		argnames is ['Request', 'Name', 'Value']
	]).

	:- public(request_body/2).
	:- mode(request_body(+compound, -compound), zero_or_one).
	:- info(request_body/2, [
		comment is 'Returns the normalized request body term.',
		argnames is ['Request', 'Body']
	]).

	:- public(json_body/2).
	:- mode(json_body(+compound, -term), one_or_error).
	:- info(json_body/2, [
		comment is 'Returns the decoded JSON payload from a normalized request body and reports a client error when the request body is missing or not decoded as JSON.',
		argnames is ['Request', 'JSON'],
		exceptions is [
			'The normalized request body is missing or is not decoded as JSON' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON request body.')
		]
	]).

	:- public(json_object_body/2).
	:- mode(json_object_body(+compound, -term), one_or_error).
	:- info(json_object_body/2, [
		comment is 'Returns the decoded JSON payload when it is a JSON object term and reports a client error when the request body is missing, not decoded as JSON, or not an object.',
		argnames is ['Request', 'JSONObject'],
		exceptions is [
			'The normalized request body is missing or is not decoded as JSON' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON request body.'),
			'The decoded JSON payload is not a JSON object term' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON object request body.')
		]
	]).

	:- public(json_array_body/2).
	:- mode(json_array_body(+compound, -list), one_or_error).
	:- info(json_array_body/2, [
		comment is 'Returns the decoded JSON payload when it is a JSON array term and reports a client error when the request body is missing, not decoded as JSON, or not an array.',
		argnames is ['Request', 'JSONArray'],
		exceptions is [
			'The normalized request body is missing or is not decoded as JSON' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON request body.'),
			'The decoded JSON payload is not a JSON array term' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON array request body.')
		]
	]).

	:- public(form_body/2).
	:- mode(form_body(+compound, -list(compound)), one_or_error).
	:- info(form_body/2, [
		comment is 'Returns the decoded form payload from a normalized request body and reports a client error when the request body is missing or not decoded as form data.',
		argnames is ['Request', 'Pairs'],
		exceptions is [
			'The normalized request body is missing or is not decoded as form data' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected form request body.')
		]
	]).

	:- public(text_body/2).
	:- mode(text_body(+compound, -atom), one_or_error).
	:- info(text_body/2, [
		comment is 'Returns the decoded text payload from a normalized request body and reports a client error when the request body is missing or not decoded as text.',
		argnames is ['Request', 'Text'],
		exceptions is [
			'The normalized request body is missing or is not decoded as text' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected text request body.')
		]
	]).

	:- public(binary_body/2).
	:- mode(binary_body(+compound, -list(integer)), one_or_error).
	:- info(binary_body/2, [
		comment is 'Returns the decoded binary payload from a normalized request body and reports a client error when the request body is missing or not decoded as binary.',
		argnames is ['Request', 'Bytes'],
		exceptions is [
			'The normalized request body is missing or is not decoded as binary' - problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected binary request body.')
		]
	]).

	:- public(json_response/4).
	:- mode(json_response(+compound, ++term, ++term, -compound), one_or_error).
	:- info(json_response/4, [
		comment is 'Builds a JSON response for the given request and status descriptor, honoring any negotiated response media type annotation.',
		argnames is ['Request', 'Status', 'JSON', 'Response'],
		exceptions is [
			'``Status`` is a variable or a ``status(Code, Reason)`` term with a variable argument' - instantiation_error,
			'``Status`` is an integer without a built-in reason phrase supported by the rest library' - domain_error(http_status_code, 'Status'),
			'``Status`` is neither an integer nor a valid normalized HTTP status term' - domain_error(http_status, 'Status')
		]
	]).

	:- public(json_response/5).
	:- mode(json_response(+compound, ++term, +list(compound), ++term, -compound), one_or_error).
	:- info(json_response/5, [
		comment is 'Builds a JSON response for the given request and status descriptor, honoring any negotiated response media type annotation and adding the provided normalized headers.',
		argnames is ['Request', 'Status', 'Headers', 'JSON', 'Response'],
		exceptions is [
			'``Status`` is a variable or a ``status(Code, Reason)`` term with a variable argument' - instantiation_error,
			'``Status`` is an integer without a built-in reason phrase supported by the rest library' - domain_error(http_status_code, 'Status'),
			'``Status`` is neither an integer nor a valid normalized HTTP status term' - domain_error(http_status, 'Status'),
			'``Headers`` is not a valid normalized header list' - domain_error(http_headers, 'Headers')
		]
	]).

	:- public(created_response/4).
	:- mode(created_response(+compound, ++term, ++term, -compound), one_or_error).
	:- info(created_response/4, [
		comment is 'Builds a ``201 Created`` JSON response with a ``Location`` header, honoring any negotiated response media type annotation.',
		argnames is ['Request', 'Location', 'JSON', 'Response'],
		exceptions is [
			'``Location`` is not a valid normalized header value' - domain_error(http_header_value, 'Location')
		]
	]).

	:- public(no_content_response/2).
	:- mode(no_content_response(+compound, -compound), one_or_error).
	:- info(no_content_response/2, [
		comment is 'Builds a ``204 No Content`` response.',
		argnames is ['Request', 'Response'],
		exceptions is [
			'``Request`` is not a valid normalized HTTP request term' - domain_error(http_request, 'Request')
		]
	]).

	:- public(problem_response/6).
	:- mode(problem_response(+compound, ++term, ++atom, ++atom, ++atom, -compound), one_or_error).
	:- info(problem_response/6, [
		comment is 'Builds an ``application/problem+json`` response with the standard ``type``, ``title``, ``detail``, and numeric ``status`` fields.',
		argnames is ['Request', 'Status', 'Type', 'Title', 'Detail', 'Response'],
		exceptions is [
			'``Status`` is a variable or a ``status(Code, Reason)`` term with a variable argument' - instantiation_error,
			'``Status`` is an integer without a built-in reason phrase supported by the rest library' - domain_error(http_status_code, 'Status'),
			'``Status`` is neither an integer nor a valid normalized HTTP status term' - domain_error(http_status, 'Status')
		]
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Hook predicate that enumerates REST endpoint descriptors. ``Id`` is the endpoint identifier and must be unique within the importing object; when OpenAPI derivation is used it also becomes the operation identifier. ``Method`` is the lowercase HTTP method, ``Path`` the path template, ``Action`` the name of a declared local predicate with arity 2, and ``Options`` a list of route metadata terms plus the optional ``produces(MediaTypes)`` descriptor.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(open_api_validate_request/1).
	:- mode(open_api_validate_request(?atom), zero_or_more).
	:- info(open_api_validate_request/1, [
		comment is 'Optional hook predicate that enables automatic OpenAPI request contract validation for the given endpoint identifier before its action predicate is called.',
		argnames is ['Id']
	]).

	:- protected(open_api_request_validation_error_response/4).
	:- mode(open_api_request_validation_error_response(+compound, +atom, +list, -compound), zero_or_one).
	:- info(open_api_request_validation_error_response/4, [
		comment is 'Optional hook predicate that customizes the response returned when OpenAPI request contract validation fails. The default implementation preserves the distinction between generic ``400`` request failures, ``415 Unsupported Media Type``, and ``422 Unprocessable Content`` for invalid request bodies. The arguments are the current request, the endpoint identifier, the validation errors, and the response to send.',
		argnames is ['Request', 'Id', 'Errors', 'Response']
	]).

	:- protected(open_api_validate_response/1).
	:- mode(open_api_validate_response(?atom), zero_or_more).
	:- info(open_api_validate_response/1, [
		comment is 'Optional hook predicate that enables automatic OpenAPI response contract validation for the given endpoint identifier after its action result is normalized into an HTTP response.',
		argnames is ['Id']
	]).

	:- protected(open_api_response_validation_error_response/5).
	:- mode(open_api_response_validation_error_response(+compound, +atom, +compound, +list, -compound), zero_or_one).
	:- info(open_api_response_validation_error_response/5, [
		comment is 'Optional hook predicate that customizes the response returned when OpenAPI response contract validation fails. The arguments are the current request, the endpoint identifier, the candidate response, the validation errors, and the response to send.',
		argnames is ['Request', 'Id', 'Response0', 'Errors', 'Response']
	]).

	:- protected(rest_action_failure_response/3).
	:- mode(rest_action_failure_response(+compound, +atom, -compound), zero_or_one).
	:- info(rest_action_failure_response/3, [
		comment is 'Optional hook predicate that customizes the response returned when the matched endpoint action fails without throwing an exception. The arguments are the current request, the endpoint identifier, and the response to send.',
		argnames is ['Request', 'Id', 'Response']
	]).

	:- protected(rest_action_error_response/4).
	:- mode(rest_action_error_response(+compound, +atom, +term, -compound), zero_or_one).
	:- info(rest_action_error_response/4, [
		comment is 'Optional hook predicate that customizes the response returned when the matched endpoint action throws an unsupported exception. The arguments are the current request, the endpoint identifier, the thrown term, and the response to send.',
		argnames is ['Request', 'Id', 'Error', 'Response']
	]).

	:- protected(dispatch_rest_endpoint/2).
	:- mode(dispatch_rest_endpoint(+compound, -compound), one_or_error).
	:- info(dispatch_rest_endpoint/2, [
		comment is 'Runs the action associated with the matched endpoint and normalizes any supported returned or thrown result term into an HTTP response.',
		argnames is ['Request', 'Response'],
		exceptions is [
			'The routed endpoint identifier is missing or unknown' - existence_error(rest_endpoint_id, 'Id'),
			'The routed endpoint identifier is duplicated' - domain_error(rest_endpoint_id, duplicate('Id')),
			'The endpoint action produces an invalid REST result term' - domain_error(rest_result, 'Result')
		]
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	route(Id, Method, Path, dispatch_rest_endpoint) :-
		validate_unique_endpoint_ids,
		::endpoint(Id, Method, Path, _Action, _Options).

	route_metadata(Id, Metadata) :-
		endpoint_descriptor(Id, _Method, _Path, _Action, Options),
		metadata_options(Options, Metadata),
		Metadata \== [].

	route_produces(Id, MediaTypes) :-
		endpoint_descriptor(Id, _Method, _Path, _Action, Options),
		memberchk(produces(MediaTypes), Options).

	path_parameter(Request, Name, Value) :-
		http_core::property(Request, path_params(Pairs)),
		memberchk(Name-Value, Pairs).

	query_parameter(Request, Name, Value) :-
		http_core::property(Request, query_pairs(Pairs)),
		memberchk(Name-Value, Pairs).

	request_header(Request, Name, Value) :-
		http_core::header(Request, Name, Value).

	request_body(Request, Body) :-
		http_core::body(Request, Body).

	json_body(Request, JSON) :-
		request_body_payload(Request, json, JSON).

	json_object_body(Request, JSONObject) :-
		json_body(Request, JSON),
		json_body_shape(object, JSON, JSONObject).

	json_array_body(Request, JSONArray) :-
		json_body(Request, JSON),
		json_body_shape(array, JSON, JSONArray).

	form_body(Request, Pairs) :-
		request_body_payload(Request, form, Pairs).

	text_body(Request, Text) :-
		request_body_payload(Request, text, Text).

	binary_body(Request, Bytes) :-
		request_body_payload(Request, binary, Bytes).

	request_body_payload(Request, PayloadType, Payload) :-
		http_core::body(Request, Body),
		request_body_payload_(PayloadType, Body, Payload).

	request_body_payload_(json, content(_MediaType, json(JSON)), JSON) :-
		!.
	request_body_payload_(form, content(_MediaType, form(Pairs)), Pairs) :-
		!.
	request_body_payload_(text, content(_MediaType, text(Text)), Text) :-
		!.
	request_body_payload_(binary, content(_MediaType, binary(Bytes)), Bytes) :-
		!.
	request_body_payload_(PayloadType, _Body, _Payload) :-
		request_body_payload_error(PayloadType).

	json_body_shape(object, JSON, JSON) :-
		nonvar(JSON),
		^^json_object_pairs(JSON, _Pairs),
		!.
	json_body_shape(array, JSON, JSON) :-
		json_array(JSON),
		!.
	json_body_shape(Shape, _JSON, _Payload) :-
		json_body_shape_error(Shape).

	json_array(JSON) :-
		nonvar(JSON),
		json_array_(JSON).

	json_array_([]).
	json_array_([_Value| Values]) :-
		json_array_(Values).

	json_body_shape_error(object) :-
		throw(problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON object request body.')).
	json_body_shape_error(array) :-
		throw(problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON array request body.')).

	request_body_payload_error(json) :-
		throw(problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected JSON request body.')).
	request_body_payload_error(form) :-
		throw(problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected form request body.')).
	request_body_payload_error(text) :-
		throw(problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected text request body.')).
	request_body_payload_error(binary) :-
		throw(problem(400, 'urn:logtalk:invalid-request-body', 'Bad Request', 'Expected binary request body.')).

	json_response(Request, StatusSpec, JSON, Response) :-
		json_response(Request, StatusSpec, [], JSON, Response).

	json_response(Request, StatusSpec, Headers, JSON, Response) :-
		http_core::version(Request, Version),
		normalize_status(StatusSpec, Status),
		success_response_media_type(Request, MediaType),
		http_core::response(Version, Status, Headers, content(MediaType, json(JSON)), [], Response).

	created_response(Request, Location, JSON, Response) :-
		json_response(Request, 201, [location-Location], JSON, Response).

	no_content_response(Request, Response) :-
		http_core::version(Request, Version),
		normalize_status(204, Status),
		http_core::response(Version, Status, [], empty, [], Response).

	problem_response(Request, StatusSpec, Type, Title, Detail, Response) :-
		normalize_status(StatusSpec, Status),
		Status = status(Code, _Reason),
		Problem = {type-Type, title-Title, detail-Detail, status-Code},
		http_core::version(Request, Version),
		http_core::response(Version, Status, [], content('application/problem+json', json(Problem)), [], Response).

	success_response_media_type(Request, MediaType) :-
		(	http_core::property(Request, response_media_type(MediaType0)),
			json_compatible_media_type(MediaType0) ->
			MediaType = MediaType0
		;	MediaType = 'application/json'
		).

	json_compatible_media_type('application/json').
	json_compatible_media_type('text/json').
	json_compatible_media_type(MediaType) :-
		atom(MediaType),
		sub_atom(MediaType, _, _, 0, '+json').

	dispatch_rest_endpoint(Request, Response) :-
		http_core::property(Request, route(Id)),
		(	http_core::property(Request, open_api_probe(true)) ->
			run_rest_endpoint(Id, Request, Response)
		;	validate_open_api_request(Id, Request, ValidationResponse),
			(	var(ValidationResponse) ->
				run_rest_endpoint(Id, Request, CandidateResponse),
				validate_open_api_response(Id, Request, CandidateResponse, Response)
			;	Response = ValidationResponse
			)
		).

	run_rest_endpoint(Id, Request, Response) :-
		endpoint_descriptor(Id, _Method, _Path, Action, _Options),
		run_rest_action(Id, Action, Request, Result, Response0),
		(	var(Response0) ->
			normalize_rest_result(Request, Result, Response)
		;	Response = Response0
		).

	endpoint_descriptor(Id, Method, Path, Action, Options) :-
		validate_unique_endpoint_ids,
		findall(
			Method0-Path0-Action0-Options0,
			::endpoint(Id, Method0, Path0, Action0, Options0),
			Matches
		),
		endpoint_descriptor_match(Id, Matches, Method, Path, Action, Options).

	validate_unique_endpoint_ids :-
		findall(Id, ::endpoint(Id, _Method, _Path, _Action, _Options), Ids),
		(	duplicate_endpoint_id(Ids, DuplicateId) ->
			domain_error(rest_endpoint_id, duplicate(DuplicateId))
		;	true
		).

	duplicate_endpoint_id([Id| Ids], DuplicateId) :-
		(	member(Id, Ids) ->
			DuplicateId = Id
		;	duplicate_endpoint_id(Ids, DuplicateId)
		).
	duplicate_endpoint_id([], _DuplicateId) :-
		fail.

	endpoint_descriptor_match(_Id, [Method-Path-Action-Options], Method, Path, Action, Options) :-
		!.
	endpoint_descriptor_match(Id, [], _Method, _Path, _Action, _Options) :-
		existence_error(rest_endpoint_id, Id).
	endpoint_descriptor_match(Id, [_,_| _], _Method, _Path, _Action, _Options) :-
		domain_error(rest_endpoint_id, duplicate(Id)).

	metadata_options([], []).
	metadata_options([produces(_MediaTypes)| Options], Metadata) :-
		!,
		metadata_options(Options, Metadata).
	metadata_options([Option| Options], [Option| Metadata]) :-
		metadata_options(Options, Metadata).

	normalize_rest_result(_Request, Result, Response) :-
		http_core::is_response(Result),
		!,
		Response = Result.
	normalize_rest_result(_Request, response(Response), Response) :-
		http_core::is_response(Response),
		!.
	normalize_rest_result(Request, ok(JSON), Response) :-
		!,
		json_response(Request, 200, JSON, Response).
	normalize_rest_result(Request, created(Location, JSON), Response) :-
		!,
		created_response(Request, Location, JSON, Response).
	normalize_rest_result(Request, no_content, Response) :-
		!,
		no_content_response(Request, Response).
	normalize_rest_result(Request, json(StatusSpec, JSON), Response) :-
		!,
		json_response(Request, StatusSpec, JSON, Response).
	normalize_rest_result(Request, json(StatusSpec, Headers, JSON), Response) :-
		!,
		json_response(Request, StatusSpec, Headers, JSON, Response).
	normalize_rest_result(Request, problem(StatusSpec, Type, Title, Detail), Response) :-
		!,
		problem_response(Request, StatusSpec, Type, Title, Detail, Response).
	normalize_rest_result(_Request, Result, _Response) :-
		domain_error(rest_result, Result).

	validate_open_api_request(Id, Request, Response) :-
		(	::open_api_validate_request(Id) ->
			self(Self),
			catch(
				open_api::validate_request(Self, Id, Request, Errors),
				Error,
				open_api_validation_exception_response(request, Request, Error, Response)
			),
			(	var(Response) ->
				(	Errors == [] ->
					true
				;	open_api_request_validation_error_response_(Request, Id, Errors, Response)
				)
			;	true
			)
		;	true
		).

	open_api_request_validation_error_response_(Request, Id, Errors, Response) :-
		(	::open_api_request_validation_error_response(Request, Id, Errors, Response) ->
			true
		;	default_open_api_request_validation_error_response(Request, Id, Errors, Response)
		).

	default_open_api_request_validation_error_response(Request, _Id, Errors, Response) :-
		default_open_api_request_validation_problem(Errors, Status, Type, Title, Detail),
		problem_response(Request, Status, Type, Title, Detail, Response).

	default_open_api_request_validation_problem(Errors, 415, 'urn:logtalk:open-api:unsupported-request-media-type', 'Unsupported Media Type', 'Request media type is not supported by the OpenAPI contract.') :-
		member(unsupported_request_media_type(_MediaType), Errors),
		!.
	default_open_api_request_validation_problem(Errors, 415, 'urn:logtalk:open-api:unsupported-request-media-type', 'Unsupported Media Type', 'Request body payload is not supported by the OpenAPI contract.') :-
		member(unsupported_request_body_payload(_PayloadType), Errors),
		!.
	default_open_api_request_validation_problem(Errors, 422, 'urn:logtalk:open-api:invalid-request-body', 'Unprocessable Content', 'Request body does not conform to the OpenAPI contract.') :-
		member(invalid_request_body(_MediaType, _SchemaErrors), Errors),
		!.
	default_open_api_request_validation_problem(_Errors, 400, 'urn:logtalk:open-api:invalid-request', 'Invalid Request', 'Request does not conform to the OpenAPI contract.').

	validate_open_api_response(Id, Request, CandidateResponse, Response) :-
		(	::open_api_validate_response(Id) ->
			self(Self),
			catch(
				open_api::validate_response(Self, Id, CandidateResponse, Errors),
				Error,
				open_api_validation_exception_response(response, Request, Error, Response)
			),
			(	var(Response) ->
				(	Errors == [] ->
					Response = CandidateResponse
				;	open_api_response_validation_error_response_(Request, Id, CandidateResponse, Errors, Response)
				)
			;	true
			)
		;	Response = CandidateResponse
		).

	open_api_response_validation_error_response_(Request, Id, CandidateResponse, Errors, Response) :-
		(	::open_api_response_validation_error_response(Request, Id, CandidateResponse, Errors, Response) ->
			true
		;	default_open_api_response_validation_error_response(Request, Id, CandidateResponse, Errors, Response)
		).

	default_open_api_response_validation_error_response(Request, _Id, _CandidateResponse, _Errors, Response) :-
		problem_response(Request, 500, 'urn:logtalk:open-api:invalid-response', 'Invalid Response', 'Response does not conform to the OpenAPI contract.', Response).

	open_api_validation_exception_response(request, Request, _Error, Response) :-
		problem_response(Request, 500, 'urn:logtalk:open-api:request-validation-error', 'OpenAPI Validation Error', 'OpenAPI request contract validation failed.', Response).
	open_api_validation_exception_response(response, Request, _Error, Response) :-
		problem_response(Request, 500, 'urn:logtalk:open-api:response-validation-error', 'OpenAPI Validation Error', 'OpenAPI response contract validation failed.', Response).

	run_rest_action(Id, Action, Request, Result, Response) :-
		Goal =.. [Action, Request, Result0],
		(	catch(::Goal, Error, normalize_rest_caught_term(Request, Id, Error, Response)) ->
			(	var(Response) ->
				Result = Result0
			;	true
			)
		;	rest_action_failure_response_(Request, Id, Response)
		).

	normalize_rest_caught_term(Request, _Id, Term, Response) :-
		rest_result_term(Term),
		!,
		normalize_rest_result(Request, Term, Response).
	normalize_rest_caught_term(Request, Id, Error, Response) :-
		normalize_rest_error(Request, Id, Error, Response).

	rest_result_term(Result) :-
		http_core::is_response(Result),
		!.
	rest_result_term(response(Response)) :-
		http_core::is_response(Response),
		!.
	rest_result_term(ok(_JSON)).
	rest_result_term(created(_Location, _JSON)).
	rest_result_term(no_content).
	rest_result_term(json(_StatusSpec, _JSON)).
	rest_result_term(json(_StatusSpec, _Headers, _JSON)).
	rest_result_term(problem(_StatusSpec, _Type, _Title, _Detail)).

	normalize_rest_error(Request, _Id, problem(StatusSpec, Type, Title, Detail), Response) :-
		!,
		problem_response(Request, StatusSpec, Type, Title, Detail, Response).
	normalize_rest_error(Request, _Id, error(problem(StatusSpec, Type, Title, Detail), _Context), Response) :-
		!,
		problem_response(Request, StatusSpec, Type, Title, Detail, Response).
	normalize_rest_error(Request, Id, Error, Response) :-
		(	::rest_action_error_response(Request, Id, Error, Response) ->
			true
		;	problem_response(Request, 500, 'about:blank', 'Internal Server Error', 'Unhandled REST action error.', Response)
		).

	rest_action_failure_response_(Request, Id, Response) :-
		(	::rest_action_failure_response(Request, Id, Response) ->
			true
		;	problem_response(Request, 500, 'about:blank', 'Internal Server Error', 'REST action failed.', Response)
		).

	normalize_status(Status, _NormalizedStatus) :-
		\+ ground(Status),
		instantiation_error.
	normalize_status(status(Code, Reason), status(Code, Reason)) :-
		!,
		http_core::response(http(1, 1), status(Code, Reason), [], empty, [], _).
	normalize_status(Code, status(Code, Reason)) :-
		integer(Code),
		!,
		(	reason_phrase(Code, Reason) ->
			true
		;	domain_error(http_status_code, Code)
		).
	normalize_status(Status, _NormalizedStatus) :-
		domain_error(http_status, Status).

	reason_phrase(200, 'OK').
	reason_phrase(201, 'Created').
	reason_phrase(202, 'Accepted').
	reason_phrase(204, 'No Content').
	reason_phrase(400, 'Bad Request').
	reason_phrase(401, 'Unauthorized').
	reason_phrase(403, 'Forbidden').
	reason_phrase(404, 'Not Found').
	reason_phrase(405, 'Method Not Allowed').
	reason_phrase(406, 'Not Acceptable').
	reason_phrase(409, 'Conflict').
	reason_phrase(415, 'Unsupported Media Type').
	reason_phrase(422, 'Unprocessable Content').
	reason_phrase(500, 'Internal Server Error').

:- end_category.
