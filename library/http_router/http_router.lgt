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


:- category(http_router,
	extends([http_json_term_helpers, http_text_helpers])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Declarative HTTP router category for objects implementing the http_handler_protocol protocol.',
		remarks is [
			'Content negotiation' - 'Importing objects can optionally define ``route_produces/2`` clauses so the router negotiates the request ``Accept`` header and annotates matched requests with the negotiated ``response_media_type(MediaType)`` property.',
			'Middleware chaining' - 'Importing objects can optionally define ordered ``middleware/2`` descriptors whose handlers either continue with a possibly rewritten request or short-circuit with a response.',
			'Response middleware' - 'Importing objects can optionally define ordered ``response_middleware/2`` descriptors whose handlers transform the final response after route dispatch or short-circuit handling.',
			'OpenAPI derivation' - 'Router objects can automatically expose ``open_api_provider_protocol`` operation descriptors derived from ``route/4``, ``route_metadata/2``, and ``route_produces/2`` plus optional top-level OpenAPI hooks. When ``request_body/1`` or ``responses/1`` metadata is absent, the router first checks optional example hooks and then attempts to infer inline request and response schemas by probing route handlers with synthetic annotated requests.',
			'Route metadata' - 'Importing objects can optionally define ``route_metadata/2`` clauses so matched requests are annotated with additional route-specific metadata properties before the route handler is called.',
			'Route declarations' - 'Importing objects must define ``route/4`` clauses using the descriptor shape ``route(Id, Method, PathTemplate, Handler)``.',
			'Path templates' - 'Path-template atoms support literal segments, anonymous ``*`` wildcard segments, plain ``{name}`` placeholders, and typed placeholders such as ``{id:integer}`` and ``{score:number}``.',
			'HEAD fallback' - 'A ``HEAD`` request matches an exact ``head`` route first and otherwise falls back to a matching ``get`` route.',
			'OPTIONS handling' - 'An ``OPTIONS`` request matches an explicit ``options`` route first and otherwise returns an automatic ``200 OK`` response with an ``Allow`` header listing the methods supported by the matched path.',
			'Custom error responses' - 'Importing objects can optionally define ``route_not_found_response/2`` and ``route_method_not_allowed_response/3`` predicates to customize ``404`` and ``405`` responses.',
			'Custom negotiation failures' - 'Importing objects can optionally define ``route_not_acceptable_response/3`` to customize ``406 Not Acceptable`` responses for route-level content negotiation failures.',
			'Request annotations' - 'Matched requests are annotated with the explicit properties ``route(Id)`` and ``path_params(Pairs)`` plus any additional metadata declared by ``route_metadata/2`` before the route handler is called.'
		]
	]).

	:- public(handle/2).
	:- mode(handle(+compound, -compound), one_or_error).
	:- info(handle/2, [
		comment is 'Routes a normalized HTTP request using the importing object ``route/4`` clauses. Importing objects can optionally define ordered ``middleware/2`` descriptors whose handlers either continue with a possibly rewritten request or short-circuit with a response, ordered ``response_middleware/2`` descriptors whose handlers transform the final response after route dispatch or short-circuit handling, and ``route_metadata/2`` clauses whose metadata is merged into matched requests before the route handler is called. Importing objects can also optionally define ``route_produces/2`` clauses so the router negotiates the request ``Accept`` header and annotates matched requests with the negotiated ``response_media_type(MediaType)`` property. Matched requests are annotated with route metadata before calling the selected route handler predicate in the importing object. Importing objects can optionally define ``route_not_found_response/2``, ``route_method_not_allowed_response/3``, and ``route_not_acceptable_response/3`` to customize ``404``, ``405``, and ``406`` handling. ``OPTIONS`` requests return an automatic ``200 OK`` response with an ``Allow`` header when no explicit ``options`` route is defined for the matched path. Path matches with unsupported methods return ``405 Method Not Allowed`` responses including an ``Allow`` header derived from the matching path templates. Negotiation failures return ``406 Not Acceptable``.',
		argnames is ['Request', 'Response']
	]).

	:- public(api_info/1).
	:- mode(api_info(-compound), one).
	:- info(api_info/1, [
		comment is 'Returns the top-level OpenAPI info descriptor used when a router object also implements the ``open_api_provider_protocol`` protocol. Importing objects can override the default descriptor by defining ``open_api_info/1``.',
		argnames is ['Info']
	]).

	:- public(servers/1).
	:- mode(servers(-list(compound)), one).
	:- info(servers/1, [
		comment is 'Returns the OpenAPI server descriptors used when a router object also implements the ``open_api_provider_protocol`` protocol. Importing objects can override the default empty list by defining ``open_api_servers/1``.',
		argnames is ['Servers']
	]).

	:- public(security/1).
	:- mode(security(-list(compound)), zero_or_one).
	:- info(security/1, [
		comment is 'Optional OpenAPI top-level security hook passthrough used when a router object also implements the ``open_api_provider_protocol`` protocol. Importing objects can define ``open_api_security/1`` to expose top-level security requirements.',
		argnames is ['Security']
	]).

	:- public(operations/1).
	:- mode(operations(-list(compound)), one).
	:- info(operations/1, [
		comment is 'Derives OpenAPI operation descriptors from ``route/4``, ``route_metadata/2``, and ``route_produces/2`` when a router object also implements the ``open_api_provider_protocol`` protocol. Recognized route metadata terms are ``summary/1``, ``description/1``, ``tags/1``, ``deprecated/1``, ``security/1``, ``parameters/1``, ``request_body/1``, and ``responses/1``. When ``request_body/1`` or ``responses/1`` metadata is absent, the router first checks optional ``route_open_api_request_body_example/2`` and ``route_open_api_response_example/2`` hooks and then attempts to infer inline request and response schemas by probing the route handler with a synthetic annotated request. Explicit metadata always takes precedence over inferred descriptors.',
		argnames is ['Operations']
	]).

	:- public(schema/2).
	:- mode(schema(?atom, ?compound), zero_or_more).
	:- info(schema/2, [
		comment is 'Optional OpenAPI schema hook passthrough used when a router object also implements the ``open_api_provider_protocol`` protocol. Importing objects can define ``open_api_schema/2`` to expose reusable schemas referenced by derived route operations.',
		argnames is ['Name', 'Schema']
	]).

	:- public(security_scheme/2).
	:- mode(security_scheme(?atom, ?compound), zero_or_more).
	:- info(security_scheme/2, [
		comment is 'Optional OpenAPI security scheme hook passthrough used when a router object also implements the ``open_api_provider_protocol`` protocol. Importing objects can define ``open_api_security_scheme/2`` to expose reusable security schemes referenced by derived route operations.',
		argnames is ['Name', 'SecurityScheme']
	]).

	:- protected(middleware/2).
	:- mode(middleware(?atom, ?atom), zero_or_more).
	:- info(middleware/2, [
		comment is 'Optional hook predicate that enumerates ordered middleware descriptors. ``Id`` is the middleware identifier and ``Handler`` is the name of a declared local predicate with arity 2, typically protected, that receives the current request and returns either ``continue(Request)`` or ``respond(Response)``.',
		argnames is ['Id', 'Handler']
	]).

	:- protected(response_middleware/2).
	:- mode(response_middleware(?atom, ?atom), zero_or_more).
	:- info(response_middleware/2, [
		comment is 'Optional hook predicate that enumerates ordered response middleware descriptors. ``Id`` is the middleware identifier and ``Handler`` is the name of a declared local predicate with arity 3, typically protected, that receives the request used to produce the current response and returns the transformed response. Routed requests preserve their route metadata annotations and negotiated media type, when applicable.',
		argnames is ['Id', 'Handler']
	]).

	:- protected(route/4).
	:- mode(route(?atom, ?atom, ?atom, ?atom), zero_or_more).
	:- info(route/4, [
		comment is 'Hook predicate that enumerates route descriptors. ``Id`` is the route identifier, ``Method`` is a lowercase HTTP method atom, ``PathTemplate`` is a path-template atom using literal segments, anonymous ``*`` wildcard segments, and optional plain or typed placeholders such as ``{id}``, ``{id:integer}``, and ``{score:number}``, and ``Handler`` is the name of a declared local predicate with arity 2, typically protected, that receives the annotated request and returns the response.',
		argnames is ['Id', 'Method', 'PathTemplate', 'Handler']
	]).

	:- protected(route_metadata/2).
	:- mode(route_metadata(?atom, ?list(compound)), zero_or_one).
	:- info(route_metadata/2, [
		comment is 'Optional hook predicate that declares additional metadata properties for a route. When defined, ``Metadata`` must be a list of compound terms. The router removes any existing request properties with the same functors, prepends the metadata to the matched request, and then adds the standard ``route/1`` and ``path_params/1`` annotations.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(route_open_api_request_body_example/2).
	:- mode(route_open_api_request_body_example(?atom, ?compound), zero_or_more).
	:- info(route_open_api_request_body_example/2, [
		comment is 'Optional hook predicate that enumerates example request bodies for OpenAPI inference. Each ``Body`` must be either ``empty`` or a normalized ``content(MediaType, Payload)`` term. These examples are only consulted when explicit ``request_body/1`` route metadata is absent.',
		argnames is ['Id', 'Body']
	]).

	:- protected(route_open_api_response_example/2).
	:- mode(route_open_api_response_example(?atom, ?compound), zero_or_more).
	:- info(route_open_api_response_example/2, [
		comment is 'Optional hook predicate that enumerates example normalized HTTP responses for OpenAPI inference. These examples are only consulted when explicit ``responses/1`` route metadata is absent.',
		argnames is ['Id', 'Response']
	]).

	:- protected(open_api_info/1).
	:- mode(open_api_info(-compound), one).
	:- info(open_api_info/1, [
		comment is 'Hook predicate that importing router objects can override to customize the default top-level OpenAPI info descriptor.',
		argnames is ['Info']
	]).

	:- protected(open_api_servers/1).
	:- mode(open_api_servers(-list(compound)), one).
	:- info(open_api_servers/1, [
		comment is 'Hook predicate that importing router objects can override to customize the default OpenAPI server descriptors.',
		argnames is ['Servers']
	]).

	:- protected(open_api_security/1).
	:- mode(open_api_security(-list(compound)), zero_or_one).
	:- info(open_api_security/1, [
		comment is 'Optional hook predicate that importing router objects can define to expose top-level OpenAPI security requirements.',
		argnames is ['Security']
	]).

	:- protected(open_api_schema/2).
	:- mode(open_api_schema(?atom, ?compound), zero_or_more).
	:- info(open_api_schema/2, [
		comment is 'Optional hook predicate that importing router objects can define to expose reusable OpenAPI schemas.',
		argnames is ['Name', 'Schema']
	]).

	:- protected(open_api_security_scheme/2).
	:- mode(open_api_security_scheme(?atom, ?compound), zero_or_more).
	:- info(open_api_security_scheme/2, [
		comment is 'Optional hook predicate that importing router objects can define to expose reusable OpenAPI security schemes.',
		argnames is ['Name', 'SecurityScheme']
	]).

	:- protected(route_produces/2).
	:- mode(route_produces(?atom, ?list(atom)), zero_or_one).
	:- info(route_produces/2, [
		comment is 'Optional hook predicate that declares the response media types produced by a route. When defined, ``MediaTypes`` must be a non-empty list of media type atoms. The router negotiates the request ``Accept`` header against this list, annotates successful matches with ``response_media_type(MediaType)``, and returns ``406 Not Acceptable`` when no produced type matches.',
		argnames is ['Id', 'MediaTypes']
	]).

	:- protected(route_not_found_response/2).
	:- mode(route_not_found_response(+compound, -compound), zero_or_one).
	:- info(route_not_found_response/2, [
		comment is 'Optional hook predicate that customizes ``404 Not Found`` responses. When defined by the importing object, it is called with the unmatched request and must return the response to send.',
		argnames is ['Request', 'Response']
	]).

	:- protected(route_method_not_allowed_response/3).
	:- mode(route_method_not_allowed_response(+compound, +list(atom), -compound), zero_or_one).
	:- info(route_method_not_allowed_response/3, [
		comment is 'Optional hook predicate that customizes ``405 Method Not Allowed`` responses. When defined by the importing object, it is called with the request, the effective allowed methods list as lowercase atoms, and must return the response to send.',
		argnames is ['Request', 'AllowedMethods', 'Response']
	]).

	:- protected(route_not_acceptable_response/3).
	:- mode(route_not_acceptable_response(+compound, +list(atom), -compound), zero_or_one).
	:- info(route_not_acceptable_response/3, [
		comment is 'Optional hook predicate that customizes ``406 Not Acceptable`` responses. When defined by the importing object, it is called with the request, the normalized produced media types for the matched route, and must return the response to send.',
		argnames is ['Request', 'ProducedMediaTypes', 'Response']
	]).

	:- uses(list, [
		append/3, member/2, memberchk/2, reverse/2
	]).

	handle(Request, Response) :-
		middleware_outcome(Request, Outcome),
		handle_middleware_outcome(Outcome, EffectiveRequest, Response0),
		response_middleware_response(EffectiveRequest, Response0, Response).

	api_info(Info) :-
		::open_api_info(Info).

	servers(Servers) :-
		::open_api_servers(Servers).

	security(Security) :-
		::open_api_security(Security).

	operations(Operations) :-
		findall(Operation, open_api_operation(Operation), Operations).

	schema(Name, Schema) :-
		::open_api_schema(Name, Schema).

	security_scheme(Name, SecurityScheme) :-
		::open_api_security_scheme(Name, SecurityScheme).

	open_api_operation(operation(RouteId, Method, PathTemplate, Summary, Parameters, RequestBody, Responses, Properties)) :-
		::route(RouteId, Method, PathTemplate, Handler),
		route_open_api_metadata(RouteId, Metadata),
		route_open_api_summary(RouteId, Metadata, Summary),
		route_open_api_parameters(PathTemplate, Metadata, Parameters),
		route_open_api_request_body(RouteId, Method, PathTemplate, Handler, Metadata, RequestBody),
		route_open_api_responses(Method, RouteId, PathTemplate, Handler, Metadata, Responses),
		route_open_api_properties(Metadata, Properties).

	route_open_api_metadata(RouteId, Metadata) :-
		( 	::route_metadata(RouteId, Metadata0) ->
			normalize_route_metadata(Metadata0, Metadata)
		; 	Metadata = []
		).

	route_open_api_summary(RouteId, Metadata, Summary) :-
		( 	route_metadata_property_value(summary, Metadata, Summary) ->
			true
		; 	Summary = RouteId
		).

	route_open_api_parameters(PathTemplate, Metadata, Parameters) :-
		default_open_api_path_parameters(PathTemplate, DefaultParameters),
		( 	route_metadata_property_value(parameters, Metadata, MetadataParameters0) ->
			normalize_open_api_metadata_parameters(MetadataParameters0, MetadataParameters),
			merge_open_api_parameters(DefaultParameters, MetadataParameters, Parameters)
		; 	Parameters = DefaultParameters
		).

	normalize_open_api_metadata_parameters([], []) :-
		!.
	normalize_open_api_metadata_parameters([Parameter| Parameters], [Parameter| NormalizedParameters]) :-
		!,
		normalize_open_api_metadata_parameters(Parameters, NormalizedParameters).
	normalize_open_api_metadata_parameters(Parameters, _NormalizedParameters) :-
		domain_error(http_router_open_api_parameters, Parameters).

	default_open_api_path_parameters(PathTemplate, Parameters) :-
		template_path_segments(PathTemplate, TemplateSegments),
		template_segments_open_api_parameters(TemplateSegments, Parameters).

	template_segments_open_api_parameters([], []).
	template_segments_open_api_parameters([literal(_)| TemplateSegments], Parameters) :-
		template_segments_open_api_parameters(TemplateSegments, Parameters).
	template_segments_open_api_parameters([wildcard| TemplateSegments], Parameters) :-
		template_segments_open_api_parameters(TemplateSegments, Parameters).
	template_segments_open_api_parameters([parameter(Name, Type)| TemplateSegments], [parameter(Name, path, 'Path parameter.', true, Schema)| Parameters]) :-
		path_parameter_open_api_schema(Type, Schema),
		template_segments_open_api_parameters(TemplateSegments, Parameters).

	path_parameter_open_api_schema(integer, {type-integer}) :-
		!.
	path_parameter_open_api_schema(number, {type-number}) :-
		!.
	path_parameter_open_api_schema(_Type, {type-string}).

	merge_open_api_parameters([], MetadataParameters, MetadataParameters).
	merge_open_api_parameters([DefaultParameter| DefaultParameters], MetadataParameters0, [Parameter| Parameters]) :-
		DefaultParameter = parameter(Name, path, _Description, _Required, _Schema),
		( 	select_open_api_parameter(Name, path, MetadataParameters0, Parameter, MetadataParameters1) ->
			true
		; 	Parameter = DefaultParameter,
			MetadataParameters1 = MetadataParameters0
		),
		merge_open_api_parameters(DefaultParameters, MetadataParameters1, Parameters).

	select_open_api_parameter(Name, In, [Parameter| Parameters], Parameter, Parameters) :-
		Parameter = parameter(Name, In, _Description, _Required, _Schema),
		!.
	select_open_api_parameter(Name, In, [Parameter| Parameters0], SelectedParameter, [Parameter| Parameters]) :-
		select_open_api_parameter(Name, In, Parameters0, SelectedParameter, Parameters).

	route_open_api_request_body(RouteId, Method, PathTemplate, Handler, Metadata, RequestBody) :-
		( 	route_metadata_property_value(request_body, Metadata, RequestBody) ->
			true
		; 	inferred_open_api_request_body_examples(RouteId, RequestBody) ->
			true
		; 	inferred_open_api_request_body(RouteId, Method, PathTemplate, Handler, RequestBody) ->
			true
		; 	RequestBody = none
		).

	route_open_api_responses(Method, RouteId, PathTemplate, Handler, Metadata, Responses) :-
		( 	route_metadata_property_value(responses, Metadata, Responses) ->
			true
		; 	inferred_open_api_responses_examples(RouteId, Responses) ->
			true
		; 	inferred_open_api_responses(Method, RouteId, PathTemplate, Handler, Responses) ->
			true
		; 	default_open_api_responses(Method, RouteId, Responses)
		).

	default_open_api_responses(Method, RouteId, [response(Status, Description, MediaTypes)]) :-
		default_open_api_response_status(Method, Status),
		default_open_api_response_description(Status, Description),
		( 	::route_produces(RouteId, ProducedMediaTypes0) ->
			normalize_produced_media_types(ProducedMediaTypes0, ProducedMediaTypes),
			open_api_media_descriptors(ProducedMediaTypes, MediaTypes)
		; 	MediaTypes = []
		).

	default_open_api_response_status(post, 201) :-
		!.
	default_open_api_response_status(delete, 204) :-
		!.
	default_open_api_response_status(_Method, 200).

	default_open_api_response_description(201, 'Created') :-
		!.
	default_open_api_response_description(204, 'No Content') :-
		!.
	default_open_api_response_description(_Status, 'Successful response').

	open_api_media_descriptors([], []).
	open_api_media_descriptors([MediaType| MediaTypes], [media(MediaType, {})| MediaDescriptors]) :-
		open_api_media_descriptors(MediaTypes, MediaDescriptors).

	inferred_open_api_request_body_examples(RouteId, RequestBody) :-
		findall(Body, route_open_api_request_body_example_(RouteId, Body), Bodies),
		Bodies \== [],
		request_body_examples_descriptor(Bodies, RequestBody).

	route_open_api_request_body_example_(RouteId, Body) :-
		::route_open_api_request_body_example(RouteId, Body0),
		normalize_open_api_request_body_example(Body0, Body).

	normalize_open_api_request_body_example(empty, empty) :-
		!.
	normalize_open_api_request_body_example(content(MediaType, Payload), content(MediaType, Payload)) :-
		atom(MediaType),
		!.
	normalize_open_api_request_body_example(Body, _NormalizedBody) :-
		domain_error(http_router_open_api_request_body_example, Body).

	request_body_examples_descriptor(Bodies, RequestBody) :-
		request_body_examples_descriptor(Bodies, true, [], Required, MediaDescriptors),
		MediaDescriptors \== [],
		RequestBody = request_body('Inferred request body', Required, MediaDescriptors).

	request_body_examples_descriptor([], Required, MediaDescriptors, Required, MediaDescriptors).
	request_body_examples_descriptor([empty| Bodies], _Required0, MediaDescriptors0, Required, MediaDescriptors) :-
		!,
		request_body_examples_descriptor(Bodies, false, MediaDescriptors0, Required, MediaDescriptors).
	request_body_examples_descriptor([content(MediaType, Payload)| Bodies], Required0, MediaDescriptors0, Required, MediaDescriptors) :-
		inferred_open_api_payload_schema(Payload, Schema),
		merge_open_api_media_descriptor(media(MediaType, Schema), MediaDescriptors0, MediaDescriptors1),
		request_body_examples_descriptor(Bodies, Required0, MediaDescriptors1, Required, MediaDescriptors).

	inferred_open_api_request_body(RouteId, Method, PathTemplate, Handler, RequestBody) :-
		catch(
			(	route_open_api_default_probe_media_type(RouteId, ProbeMediaType),
				route_open_api_probe_request(RouteId, Method, PathTemplate, ProbeMediaType, ProbeBody, Request),
				once(call_route_handler(Handler, Request, _Response)),
				nonvar(ProbeBody),
				ProbeBody = content(MediaType, Payload),
				inferred_open_api_payload_schema(Payload, Schema),
				RequestBody = request_body('Inferred request body', true, [media(MediaType, Schema)])
			),
			_Error,
			fail
		).

	inferred_open_api_responses_examples(RouteId, Responses) :-
		findall(Response, route_open_api_response_example_(RouteId, Response), ExampleResponses),
		ExampleResponses \== [],
		response_examples_descriptors(ExampleResponses, [], Responses),
		Responses \== [].

	route_open_api_response_example_(RouteId, Response) :-
		::route_open_api_response_example(RouteId, Response0),
		normalize_open_api_response_example(Response0, Response).

	normalize_open_api_response_example(Response, Response) :-
		http::is_response(Response),
		!.
	normalize_open_api_response_example(Response, _NormalizedResponse) :-
		domain_error(http_router_open_api_response_example, Response).

	response_examples_descriptors([], Responses, Responses).
	response_examples_descriptors([Response| ExampleResponses], Responses0, Responses) :-
		inferred_open_api_response_descriptor(Response, Status, Description),
		response_open_api_media_descriptors(Response, MediaDescriptors),
		merge_open_api_response_descriptor(response(Status, Description, MediaDescriptors), Responses0, Responses1),
		response_examples_descriptors(ExampleResponses, Responses1, Responses).

	inferred_open_api_responses(Method, RouteId, PathTemplate, Handler, [response(Status, Description, MediaDescriptors)]) :-
		catch(
			inferred_open_api_responses_(Method, RouteId, PathTemplate, Handler, Status, Description, MediaDescriptors),
			_Error,
			fail
		).

	inferred_open_api_responses_(Method, RouteId, PathTemplate, Handler, Status, Description, MediaDescriptors) :-
		route_open_api_produced_media_types(RouteId, ProducedMediaTypes),
		ProducedMediaTypes = [_| _],
		inferred_open_api_response_descriptor_from_media_types(ProducedMediaTypes, RouteId, Method, PathTemplate, Handler, Status, Description),
		inferred_open_api_response_media_descriptors(ProducedMediaTypes, RouteId, Method, PathTemplate, Handler, MediaDescriptors).
	inferred_open_api_responses_(Method, RouteId, PathTemplate, Handler, Status, Description, MediaDescriptors) :-
		route_open_api_probe_response(RouteId, Method, PathTemplate, Handler, none, Response),
		inferred_open_api_response_descriptor(Response, Status, Description),
		response_open_api_media_descriptors(Response, MediaDescriptors).

	route_open_api_produced_media_types(RouteId, ProducedMediaTypes) :-
		( 	::route_produces(RouteId, ProducedMediaTypes0) ->
			normalize_produced_media_types(ProducedMediaTypes0, ProducedMediaTypes)
		; 	ProducedMediaTypes = []
		).

	route_open_api_default_probe_media_type(RouteId, ProbeMediaType) :-
		( 	route_open_api_produced_media_types(RouteId, [ProbeMediaType| _]) ->
			true
		; 	ProbeMediaType = none
		).

	route_open_api_probe_request(RouteId, Method, PathTemplate, ProbeMediaType, Body, Request) :-
		route_open_api_probe_path(PathTemplate, Path, PathParams),
		Request0 = request(Method, origin(Path), http(1, 1), [], Body, []),
		annotate_open_api_probe_request(RouteId, PathParams, Request0, Request1),
		annotate_open_api_probe_response_media_type(ProbeMediaType, Request1, Request).

	route_open_api_probe_response(RouteId, Method, PathTemplate, Handler, ProbeMediaType, Response) :-
		route_open_api_probe_request(RouteId, Method, PathTemplate, ProbeMediaType, empty, Request),
		once(call_route_handler(Handler, Request, Response)),
		http::is_response(Response).
	route_open_api_probe_response(RouteId, Method, PathTemplate, Handler, ProbeMediaType, Response) :-
		route_open_api_probe_request(RouteId, Method, PathTemplate, ProbeMediaType, _ProbeBody, Request),
		once(call_route_handler(Handler, Request, Response)),
		http::is_response(Response).

	route_open_api_probe_path(PathTemplate, Path, PathParams) :-
		template_path_segments(PathTemplate, TemplateSegments),
		template_segments_probe_path(TemplateSegments, PathSegments, PathParams),
		path_from_segments(PathSegments, Path).

	template_segments_probe_path([], [], []).
	template_segments_probe_path([literal(Segment)| TemplateSegments], [Segment| PathSegments], PathParams) :-
		template_segments_probe_path(TemplateSegments, PathSegments, PathParams).
	template_segments_probe_path([wildcard| TemplateSegments], ['wildcard'| PathSegments], PathParams) :-
		template_segments_probe_path(TemplateSegments, PathSegments, PathParams).
	template_segments_probe_path([parameter(Name, Type)| TemplateSegments], [Segment| PathSegments], [Name-Value| PathParams]) :-
		probe_path_parameter_value(Type, Name, Segment, Value),
		template_segments_probe_path(TemplateSegments, PathSegments, PathParams).

	probe_path_parameter_value(integer, _Name, '42', 42) :-
		!.
	probe_path_parameter_value(number, _Name, '42.5', 42.5) :-
		!.
	probe_path_parameter_value(_Type, _Name, '42', '42').

	path_from_segments(Segments, Path) :-
		path_from_segments_codes(Segments, Codes),
		atom_codes(Path, Codes).

	path_from_segments_codes([], [0'/]) :-
		!.
	path_from_segments_codes([Segment| Segments], [0'/| Codes]) :-
		atom_codes(Segment, SegmentCodes),
		append(SegmentCodes, TailCodes, Codes),
		path_from_segments_tail_codes(Segments, TailCodes).

	path_from_segments_tail_codes([], []).
	path_from_segments_tail_codes([Segment| Segments], [0'/| Codes]) :-
		atom_codes(Segment, SegmentCodes),
		append(SegmentCodes, TailCodes, Codes),
		path_from_segments_tail_codes(Segments, TailCodes).

	annotate_open_api_probe_request(RouteId, PathParams, request(Method, Target, Version, Headers, Body, Properties0), request(Method, Target, Version, Headers, Body, Properties)) :-
		route_metadata_properties(RouteId, Properties0, Properties1),
		remove_property_functor(Properties1, path_params, Properties2),
		remove_property_functor(Properties2, route, Properties3),
		Properties = [route(RouteId), path_params(PathParams)| Properties3].

	annotate_open_api_probe_response_media_type(none, Request, Request) :-
		!.
	annotate_open_api_probe_response_media_type(MediaType, request(Method, Target, Version, Headers, Body, Properties0), request(Method, Target, Version, Headers, Body, Properties)) :-
		remove_property_functor(Properties0, response_media_type, Properties1),
		Properties = [response_media_type(MediaType)| Properties1].

	inferred_open_api_response_descriptor_from_media_types([ProbeMediaType| _ProbeMediaTypes], RouteId, Method, PathTemplate, Handler, Status, Description) :-
		route_open_api_probe_response(RouteId, Method, PathTemplate, Handler, ProbeMediaType, Response),
		!,
		inferred_open_api_response_descriptor(Response, Status, Description).
	inferred_open_api_response_descriptor_from_media_types([_ProbeMediaType| ProbeMediaTypes], RouteId, Method, PathTemplate, Handler, Status, Description) :-
		inferred_open_api_response_descriptor_from_media_types(ProbeMediaTypes, RouteId, Method, PathTemplate, Handler, Status, Description).

	inferred_open_api_response_descriptor(response(_Version, status(Status, Reason), _Headers, _Body, _Properties), Status, Description) :-
		inferred_open_api_response_description(Status, Reason, Description).

	inferred_open_api_response_description(Status, Reason, Description) :-
		( 	default_open_api_response_status_description(Status, Description) ->
			true
		; 	atom(Reason),
			Reason \== '' ->
			Description = Reason
		; 	default_open_api_response_description(Status, Description)
		).

	default_open_api_response_status_description(200, 'Successful response') :-
		!.
	default_open_api_response_status_description(201, 'Created') :-
		!.
	default_open_api_response_status_description(204, 'No Content').

	inferred_open_api_response_media_descriptors(ProducedMediaTypes, RouteId, Method, PathTemplate, Handler, MediaDescriptors) :-
		inferred_open_api_response_media_descriptors(ProducedMediaTypes, RouteId, Method, PathTemplate, Handler, [], MediaDescriptors).

	inferred_open_api_response_media_descriptors([], _RouteId, _Method, _PathTemplate, _Handler, MediaDescriptors, MediaDescriptors).
	inferred_open_api_response_media_descriptors([ProbeMediaType| ProducedMediaTypes], RouteId, Method, PathTemplate, Handler, MediaDescriptors0, MediaDescriptors) :-
		( 	catch(
				(	route_open_api_probe_response(RouteId, Method, PathTemplate, Handler, ProbeMediaType, Response),
					response_open_api_media_descriptors(Response, ResponseMediaDescriptors)
				),
				_Error,
				fail
			) ->
			append_new_open_api_media_descriptors(ResponseMediaDescriptors, MediaDescriptors0, MediaDescriptors1)
		; 	MediaDescriptors1 = MediaDescriptors0
		),
		inferred_open_api_response_media_descriptors(ProducedMediaTypes, RouteId, Method, PathTemplate, Handler, MediaDescriptors1, MediaDescriptors).

	append_new_open_api_media_descriptors([], MediaDescriptors, MediaDescriptors).
	append_new_open_api_media_descriptors([MediaDescriptor| MediaDescriptors0], MediaDescriptors1, MediaDescriptors) :-
		merge_open_api_media_descriptor(MediaDescriptor, MediaDescriptors1, MediaDescriptors2),
		append_new_open_api_media_descriptors(MediaDescriptors0, MediaDescriptors2, MediaDescriptors).

	merge_open_api_response_descriptor(response(Status, Description, MediaDescriptors), [], [response(Status, Description, MediaDescriptors)]).
	merge_open_api_response_descriptor(response(Status, Description, MediaDescriptors), [response(Status, Description0, MediaDescriptors0)| Responses0], [response(Status, Description1, MediaDescriptors1)| Responses0]) :-
		!,
		merge_open_api_response_description(Description0, Description, Description1),
		merge_open_api_media_descriptors(MediaDescriptors, MediaDescriptors0, MediaDescriptors1).
	merge_open_api_response_descriptor(Response, [Response0| Responses0], [Response0| Responses]) :-
		merge_open_api_response_descriptor(Response, Responses0, Responses).

	merge_open_api_response_description(Description, _AlternativeDescription, Description).

	merge_open_api_media_descriptors([], MediaDescriptors, MediaDescriptors).
	merge_open_api_media_descriptors([MediaDescriptor| MediaDescriptors0], MediaDescriptors1, MediaDescriptors) :-
		merge_open_api_media_descriptor(MediaDescriptor, MediaDescriptors1, MediaDescriptors2),
		merge_open_api_media_descriptors(MediaDescriptors0, MediaDescriptors2, MediaDescriptors).

	merge_open_api_media_descriptor(media(MediaType, Schema), [], [media(MediaType, Schema)]).
	merge_open_api_media_descriptor(media(MediaType, Schema), [media(MediaType, Schema0)| MediaDescriptors0], [media(MediaType, Schema1)| MediaDescriptors0]) :-
		!,
		merge_open_api_schema(Schema0, Schema, Schema1).
	merge_open_api_media_descriptor(MediaDescriptor, [MediaDescriptor0| MediaDescriptors0], [MediaDescriptor0| MediaDescriptors]) :-
		merge_open_api_media_descriptor(MediaDescriptor, MediaDescriptors0, MediaDescriptors).

	merge_open_api_schema(Schema, Schema, Schema) :-
		!.
	merge_open_api_schema(Schema0, Schema, {oneOf-Alternatives}) :-
		schema_one_of_alternatives(Schema0, Alternatives0),
		( 	memberchk(Schema, Alternatives0) ->
			Alternatives = Alternatives0
		; 	append(Alternatives0, [Schema], Alternatives)
		).

	schema_one_of_alternatives({oneOf-Alternatives}, Alternatives) :-
		!.
	schema_one_of_alternatives(Schema, [Schema]).

	response_open_api_media_descriptors(response(_Version, _Status, _Headers, empty, _Properties), []) :-
		!.
	response_open_api_media_descriptors(response(_Version, _Status, _Headers, content(MediaType, Payload), _Properties), [media(MediaType, Schema)]) :-
		inferred_open_api_payload_schema(Payload, Schema).

	inferred_open_api_payload_schema(Payload, {}) :-
		var(Payload),
		!.
	inferred_open_api_payload_schema(json(Value), Schema) :-
		!,
		normalize_open_api_json_value(Value, JsonValue),
		normalized_json_value_open_api_schema(JsonValue, Schema).
	inferred_open_api_payload_schema(form(Pairs), Schema) :-
		!,
		normalize_open_api_json_pairs(Pairs, JsonPairs),
		normalized_json_object_schema(JsonPairs, Schema).
	inferred_open_api_payload_schema(text(_Text), {type-string}) :-
		!.
	inferred_open_api_payload_schema(binary(_Bytes), {type-string}) :-
		!.
	inferred_open_api_payload_schema(multipart(_Parts), {}) :-
		!.
	inferred_open_api_payload_schema(_Payload, {}).

	normalize_open_api_json_value(Value, JsonValue) :-
		^^normalize_json_value(Value, JsonValue).

	normalize_open_api_json_pairs(Pairs, JsonPairs) :-
		^^normalize_json_pairs(Pairs, JsonPairs).

	normalized_json_value_open_api_schema(Value, {}) :-
		var(Value),
		!.
	normalized_json_value_open_api_schema(@true, {type-boolean}) :-
		!.
	normalized_json_value_open_api_schema(@false, {type-boolean}) :-
		!.
	normalized_json_value_open_api_schema(Value, {type-integer}) :-
		integer(Value),
		!.
	normalized_json_value_open_api_schema(Value, {type-number}) :-
		number(Value),
		!.
	normalized_json_value_open_api_schema(Value, {type-string}) :-
		atom(Value),
		!.
	normalized_json_value_open_api_schema([], {type-array}) :-
		!.
	normalized_json_value_open_api_schema([Value| Values], Schema) :-
		!,
		normalized_json_array_schema([Value| Values], Schema).
	normalized_json_value_open_api_schema(Value, Schema) :-
		^^json_object_pairs(Value, Pairs),
		!,
		normalized_json_object_schema(Pairs, Schema).
	normalized_json_value_open_api_schema(_Value, {}).

	normalized_json_array_schema(Values, {type-array, items-ItemSchema}) :-
		normalized_json_array_items_schema(Values, ItemSchema),
		!.
	normalized_json_array_schema(_Values, {type-array}).

	normalized_json_array_items_schema([Value| Values], ItemSchema) :-
		normalized_json_value_open_api_schema(Value, ItemSchema),
		normalized_json_array_items_schema_(Values, ItemSchema).

	normalized_json_array_items_schema_([], _ItemSchema).
	normalized_json_array_items_schema_([Value| Values], ItemSchema) :-
		normalized_json_value_open_api_schema(Value, ItemSchema),
		normalized_json_array_items_schema_(Values, ItemSchema).

	normalized_json_object_schema(Pairs, {type-object, properties-Properties, required-Required, additionalProperties- @false}) :-
		json_property_schema_pairs(Pairs, PropertyPairs, Required),
		^^pairs_to_object(PropertyPairs, Properties).

	json_property_schema_pairs([], [], []).
	json_property_schema_pairs([Pair| Pairs], [Key-Schema| PropertyPairs], [Key| Required]) :-
		^^pair_key_value(Pair, Key, Value),
		normalized_json_value_open_api_schema(Value, Schema),
		json_property_schema_pairs(Pairs, PropertyPairs, Required).

	route_open_api_properties(Metadata, Properties) :-
		( 	Metadata == [] ->
			Properties = []
		; 	route_open_api_properties_(Metadata, Properties)
		).

	route_open_api_properties_([], []).
	route_open_api_properties_([description(Description)| Metadata], [description(Description)| Properties]) :-
		!,
		route_open_api_properties_(Metadata, Properties).
	route_open_api_properties_([tags(Tags)| Metadata], [tags(Tags)| Properties]) :-
		!,
		route_open_api_properties_(Metadata, Properties).
	route_open_api_properties_([deprecated(Deprecated)| Metadata], [deprecated(Deprecated)| Properties]) :-
		!,
		route_open_api_properties_(Metadata, Properties).
	route_open_api_properties_([security(Security)| Metadata], [security(Security)| Properties]) :-
		!,
		route_open_api_properties_(Metadata, Properties).
	route_open_api_properties_([_| Metadata], Properties) :-
		route_open_api_properties_(Metadata, Properties).

	route_metadata_property_value(_Name, [], _Value) :-
		fail.
	route_metadata_property_value(Name, [Property| _Metadata], Value) :-
		nonvar(Property),
		functor(Property, Name, 1),
		arg(1, Property, Value),
		!.
	route_metadata_property_value(Name, [_Property| Metadata], Value) :-
		route_metadata_property_value(Name, Metadata, Value).

	middleware_outcome(Request, Outcome) :-
		findall(Handler, ::middleware(_Id, Handler), Handlers),
		apply_middlewares(Handlers, Request, Outcome).

	apply_middlewares([], Request, continue(Request)).
	apply_middlewares([Handler| Handlers], Request, Outcome) :-
		call_middleware(Handler, Request, Action),
		middleware_action_outcome(Action, Request, Handlers, Outcome).

	middleware_action_outcome(continue(Request), _CurrentRequest, Handlers, Outcome) :-
		apply_middlewares(Handlers, Request, Outcome).
	middleware_action_outcome(respond(Response), Request, _Handlers, respond(Request, Response)).

	call_middleware(Handler, Request, Action) :-
		atom(Handler),
		!,
		Goal =.. [Handler, Request, Action],
		::Goal,
		validate_middleware_action(Action).
	call_middleware(Handler, _Request, _Action) :-
		domain_error(http_router_middleware, Handler).

	validate_middleware_action(continue(Request)) :-
		http::is_request(Request),
		!.
	validate_middleware_action(respond(Response)) :-
		http::is_response(Response),
		!.
	validate_middleware_action(Action) :-
		domain_error(http_router_middleware_action, Action).

	response_middleware_response(Request, Response0, Response) :-
		findall(Handler, ::response_middleware(_Id, Handler), Handlers),
		apply_response_middlewares(Handlers, Request, Response0, Response).

	apply_response_middlewares([], _Request, Response, Response).
	apply_response_middlewares([Handler| Handlers], Request, Response0, Response) :-
		call_response_middleware(Handler, Request, Response0, Response1),
		apply_response_middlewares(Handlers, Request, Response1, Response).

	call_response_middleware(Handler, Request, Response0, Response) :-
		atom(Handler),
		!,
		Goal =.. [Handler, Request, Response0, Response],
		::Goal,
		validate_response_middleware_response(Response).
	call_response_middleware(Handler, _Request, _Response0, _Response) :-
		domain_error(http_router_response_middleware, Handler).

	validate_response_middleware_response(Response) :-
		http::is_response(Response),
		!.
	validate_response_middleware_response(Response) :-
		domain_error(http_router_response_middleware_response, Response).

	handle_middleware_outcome(respond(Request, Response), Request, Response).
	handle_middleware_outcome(continue(Request), EffectiveRequest, Response) :-
		( 	routable_path(Request, Path) ->
			( 	matched_route(Request, Path, RouteId, Handler, PathParams) ->
				annotate_request(RouteId, PathParams, Request, RoutedRequest),
				route_response(RouteId, Handler, RoutedRequest, EffectiveRequest, Response)
			; 	automatic_options_response(Request, Path, Response) ->
				EffectiveRequest = Request
			; 	allowed_route_methods(Path, AllowedMethods) ->
				EffectiveRequest = Request,
				method_not_allowed_response(Request, AllowedMethods, Response)
			; 	EffectiveRequest = Request,
				not_found_response(Request, Response)
			)
		; 	EffectiveRequest = Request,
			not_found_response(Request, Response)
		).

	route_response(RouteId, Handler, Request, EffectiveRequest, Response) :-
		( 	negotiated_route_request(RouteId, Request, NegotiatedRequest) ->
			EffectiveRequest = NegotiatedRequest,
			call_route_handler(Handler, NegotiatedRequest, Response)
		; 	EffectiveRequest = Request,
			handle_not_acceptable_response(RouteId, Request, Response)
		).

	negotiated_route_request(RouteId, Request, NegotiatedRequest) :-
		( 	::route_produces(RouteId, ProducedMediaTypes0) ->
			normalize_produced_media_types(ProducedMediaTypes0, ProducedMediaTypes),
			negotiated_response_media_type(Request, ProducedMediaTypes, MediaType),
			annotate_response_media_type(MediaType, Request, NegotiatedRequest)
		; 	NegotiatedRequest = Request
		).

	normalize_produced_media_types(ProducedMediaTypes0, ProducedMediaTypes) :-
		ProducedMediaTypes0 = [_| _],
		!,
		normalize_produced_media_type_list(ProducedMediaTypes0, ProducedMediaTypes).
	normalize_produced_media_types(ProducedMediaTypes0, _ProducedMediaTypes) :-
		domain_error(http_router_route_produces, ProducedMediaTypes0).

	normalize_produced_media_type_list([], []).
	normalize_produced_media_type_list([ProducedMediaType0| ProducedMediaTypes0], [ProducedMediaType| ProducedMediaTypes]) :-
		normalize_produced_media_type(ProducedMediaType0, ProducedMediaType),
		normalize_produced_media_type_list(ProducedMediaTypes0, ProducedMediaTypes).

	normalize_produced_media_type(ProducedMediaType0, ProducedMediaType) :-
		atom(ProducedMediaType0),
		media_type_atom_parts(ProducedMediaType0, Type, Subtype),
		Type \== '*',
		Subtype \== '*',
		!,
		atom_codes(Type, TypeCodes),
		atom_codes(Subtype, SubtypeCodes),
		append(TypeCodes, [0'/| SubtypeCodes], ProducedMediaTypeCodes),
		atom_codes(ProducedMediaType, ProducedMediaTypeCodes).
	normalize_produced_media_type(ProducedMediaType, _NormalizedProducedMediaType) :-
		domain_error(http_router_route_media_type, ProducedMediaType).

	negotiated_response_media_type(Request, ProducedMediaTypes, MediaType) :-
		accept_header_values(Request, AcceptHeaderValues),
		( 	AcceptHeaderValues == [] ->
			ProducedMediaTypes = [MediaType| _]
		; 	accept_header_specs(AcceptHeaderValues, AcceptSpecs),
			AcceptSpecs \== [],
			best_produced_media_type(ProducedMediaTypes, AcceptSpecs, MediaType)
		).

	accept_header_values(Request, AcceptHeaderValues) :-
		findall(AcceptHeaderValue, http::header(Request, accept, AcceptHeaderValue), AcceptHeaderValues).

	accept_header_specs(AcceptHeaderValues, AcceptSpecs) :-
		accept_header_specs(AcceptHeaderValues, [], AcceptSpecs0),
		reverse(AcceptSpecs0, AcceptSpecs).

	accept_header_specs([], AcceptSpecs, AcceptSpecs).
	accept_header_specs([AcceptHeaderValue| AcceptHeaderValues], AcceptSpecs0, AcceptSpecs) :-
		text_codes(AcceptHeaderValue, AcceptHeaderCodes),
		split_codes(AcceptHeaderCodes, 0',, AcceptItemCodesList),
		accept_item_specs(AcceptItemCodesList, AcceptSpecs0, AcceptSpecs1),
		accept_header_specs(AcceptHeaderValues, AcceptSpecs1, AcceptSpecs).

	accept_item_specs([], AcceptSpecs, AcceptSpecs).
	accept_item_specs([AcceptItemCodes| AcceptItemCodesList], AcceptSpecs0, AcceptSpecs) :-
		( 	accept_spec(AcceptItemCodes, AcceptSpec) ->
			AcceptSpecs1 = [AcceptSpec| AcceptSpecs0]
		; 	AcceptSpecs1 = AcceptSpecs0
		),
		accept_item_specs(AcceptItemCodesList, AcceptSpecs1, AcceptSpecs).

	accept_spec(AcceptItemCodes0, accept_spec(Type, Subtype, Quality, Specificity)) :-
		trim_ows_codes(AcceptItemCodes0, AcceptItemCodes),
		AcceptItemCodes \== [],
		split_codes(AcceptItemCodes, 0';, [MediaRangeCodes| ParameterCodesList]),
		accept_media_range(MediaRangeCodes, Type, Subtype, Specificity),
		accept_quality(ParameterCodesList, Quality),
		Quality > 0.0.

	accept_media_range(MediaRangeCodes0, Type, Subtype, Specificity) :-
		trim_ows_codes(MediaRangeCodes0, MediaRangeCodes),
		media_range_parts(MediaRangeCodes, Type, Subtype),
		accept_range_specificity(Type, Subtype, Specificity).

	accept_quality(ParameterCodesList, Quality) :-
		accept_quality(ParameterCodesList, 1.0, Quality).

	accept_quality([], Quality, Quality).
	accept_quality([ParameterCodes0| _ParameterCodesList], _DefaultQuality, Quality) :-
		q_parameter_quality(ParameterCodes0, Quality),
		!.
	accept_quality([_ParameterCodes| ParameterCodesList], DefaultQuality, Quality) :-
		accept_quality(ParameterCodesList, DefaultQuality, Quality).

	q_parameter_quality(ParameterCodes0, Quality) :-
		trim_ows_codes(ParameterCodes0, ParameterCodes),
		split_codes(ParameterCodes, 0'=, [NameCodes0, ValueCodes0]),
		trim_ows_codes(NameCodes0, NameCodes1),
		lowercase_ascii_codes(NameCodes1, [0'q]),
		trim_ows_codes(ValueCodes0, ValueCodes),
		number_codes(Quality, ValueCodes),
		Quality >= 0.0,
		Quality =< 1.0.

	best_produced_media_type(ProducedMediaTypes, AcceptSpecs, MediaType) :-
		best_produced_media_type(ProducedMediaTypes, AcceptSpecs, none, MediaType).

	best_produced_media_type([], _AcceptSpecs, none, _MediaType) :-
		fail.
	best_produced_media_type([], _AcceptSpecs, choice(MediaType, _Quality, _Specificity), MediaType).
	best_produced_media_type([ProducedMediaType| ProducedMediaTypes], AcceptSpecs, BestChoice0, MediaType) :-
		( 	best_accept_match(ProducedMediaType, AcceptSpecs, Quality, Specificity) ->
			better_media_choice(choice(ProducedMediaType, Quality, Specificity), BestChoice0, BestChoice1)
		; 	BestChoice1 = BestChoice0
		),
		best_produced_media_type(ProducedMediaTypes, AcceptSpecs, BestChoice1, MediaType).

	best_accept_match(ProducedMediaType, AcceptSpecs, Quality, Specificity) :-
		media_type_atom_parts(ProducedMediaType, Type, Subtype),
		best_accept_match(AcceptSpecs, Type, Subtype, none, Quality, Specificity).

	best_accept_match([], _Type, _Subtype, none, _Quality, _Specificity) :-
		fail.
	best_accept_match([], _Type, _Subtype, choice(_Type, _Subtype, Quality, Specificity), Quality, Specificity).
	best_accept_match([AcceptSpec| AcceptSpecs], Type, Subtype, BestChoice0, Quality, Specificity) :-
		( 	accept_spec_matches(AcceptSpec, Type, Subtype, MatchQuality, MatchSpecificity) ->
			better_accept_choice(choice(Type, Subtype, MatchQuality, MatchSpecificity), BestChoice0, BestChoice1)
		; 	BestChoice1 = BestChoice0
		),
		best_accept_match(AcceptSpecs, Type, Subtype, BestChoice1, Quality, Specificity).

	accept_spec_matches(accept_spec(Type, Subtype, Quality, Specificity), Type, Subtype, Quality, Specificity).
	accept_spec_matches(accept_spec(Type, '*', Quality, Specificity), Type, _Subtype, Quality, Specificity).
	accept_spec_matches(accept_spec('*', '*', Quality, Specificity), _Type, _Subtype, Quality, Specificity).

	better_media_choice(Choice, none, Choice) :-
		!.
	better_media_choice(choice(MediaType, Quality, Specificity), choice(_BestMediaType, BestQuality, BestSpecificity), choice(MediaType, Quality, Specificity)) :-
		( 	Quality > BestQuality
		; 	Quality =:= BestQuality,
			Specificity > BestSpecificity
		),
		!.
	better_media_choice(_Choice, BestChoice, BestChoice).

	better_accept_choice(Choice, none, Choice) :-
		!.
	better_accept_choice(choice(Type, Subtype, Quality, Specificity), choice(_BestType, _BestSubtype, BestQuality, BestSpecificity), choice(Type, Subtype, Quality, Specificity)) :-
		( 	Quality > BestQuality
		; 	Quality =:= BestQuality,
			Specificity > BestSpecificity
		),
		!.
	better_accept_choice(_Choice, BestChoice, BestChoice).

	accept_range_specificity('*', '*', 0) :-
		!.
	accept_range_specificity(Type, '*', 1) :-
		Type \== '*',
		!.
	accept_range_specificity(Type, Subtype, 2) :-
		Type \== '*',
		Subtype \== '*'.

	annotate_response_media_type(MediaType, request(Method, Target, Version, Headers, Body, Properties0), Request) :-
		remove_property_functor(Properties0, response_media_type, Properties1),
		Properties = [response_media_type(MediaType)| Properties1],
		http::request(Method, Target, Version, Headers, Body, Properties, Request).

	matched_route(Request, Path, RouteId, Handler, PathParams) :-
		http::method(Request, Method),
		preferred_route_method(Method, RouteMethod),
		::route(RouteId, RouteMethod, Template, Handler),
		template_matches(Template, Path, PathParams),
		!.

	preferred_route_method(head, head).
	preferred_route_method(head, get).
	preferred_route_method(Method, Method) :-
		Method \== head.

	allowed_route_methods(Path, AllowedMethods) :-
		findall(
			RouteAllowedMethods,
			( 	::route(_RouteId, RouteMethod, Template, _Handler),
				template_matches(Template, Path, _PathParams),
				route_allowed_methods(RouteMethod, RouteAllowedMethods)
			),
			RouteAllowedMethodLists
		),
		RouteAllowedMethodLists \== [],
		merge_allowed_method_lists(RouteAllowedMethodLists, AllowedMethods).

	route_allowed_methods(get, [get, head]).
	route_allowed_methods(Method, [Method]) :-
		Method \== get.

	merge_allowed_method_lists(RouteAllowedMethodLists, AllowedMethods) :-
		merge_allowed_method_lists(RouteAllowedMethodLists, [], AllowedMethods).

	merge_allowed_method_lists([], AllowedMethods, AllowedMethods).
	merge_allowed_method_lists([RouteAllowedMethods| RouteAllowedMethodLists], AllowedMethods0, AllowedMethods) :-
		append_new_methods(RouteAllowedMethods, AllowedMethods0, AllowedMethods1),
		merge_allowed_method_lists(RouteAllowedMethodLists, AllowedMethods1, AllowedMethods).

	append_new_methods([], Methods, Methods).
	append_new_methods([Method| Methods0], Methods1, Methods) :-
		( 	member(Method, Methods1) ->
			Methods2 = Methods1
		; 	append(Methods1, [Method], Methods2)
		),
		append_new_methods(Methods0, Methods2, Methods).

	automatic_options_response(Request, Path, Response) :-
		http::method(Request, options),
		allowed_route_methods(Path, AllowedMethods0),
		effective_allowed_methods(AllowedMethods0, AllowedMethods),
		http::version(Request, Version),
		allow_header_value(AllowedMethods, AllowValue),
		http::response(Version, status(200, 'OK'), [allow-AllowValue], empty, [], Response).

	routable_path(Request, Path) :-
		http::target(Request, Target),
		target_path(Target, Path).

	target_path(origin(Path), Path).
	target_path(origin(Path, _Query), Path).
	target_path(absolute(Components), Path) :-
		( 	member(path(Path), Components) ->
			true
		; 	Path = ''
		).

	annotate_request(RouteId, PathParams, request(Method, Target, Version, Headers, Body, Properties0), Request) :-
		route_metadata_properties(RouteId, Properties0, Properties1),
		remove_property_functor(Properties1, path_params, Properties2),
		remove_property_functor(Properties2, route, Properties3),
		Properties = [route(RouteId), path_params(PathParams)| Properties3],
		http::request(Method, Target, Version, Headers, Body, Properties, Request).

	route_metadata_properties(RouteId, Properties0, Properties) :-
		( 	::route_metadata(RouteId, Metadata0) ->
			normalize_route_metadata(Metadata0, Metadata),
			merge_route_metadata(Metadata, Properties0, Properties)
		; 	Properties = Properties0
		).

	normalize_route_metadata([], []) :-
		!.
	normalize_route_metadata([MetadataProperty0| Metadata0], [MetadataProperty| Metadata]) :-
		!,
		normalize_route_metadata_property(MetadataProperty0, MetadataProperty),
		normalize_route_metadata(Metadata0, Metadata).
	normalize_route_metadata(Metadata0, _Metadata) :-
		domain_error(http_router_route_metadata, Metadata0).

	normalize_route_metadata_property(MetadataProperty0, MetadataProperty) :-
		nonvar(MetadataProperty0),
		compound(MetadataProperty0),
		functor(MetadataProperty0, Functor, Arity),
		Functor \== '.',
		Arity > 0,
		\+ reserved_route_metadata_functor(Functor),
		!,
		MetadataProperty = MetadataProperty0.
	normalize_route_metadata_property(MetadataProperty, _NormalizedMetadataProperty) :-
		domain_error(http_router_route_metadata_property, MetadataProperty).

	reserved_route_metadata_functor(route).
	reserved_route_metadata_functor(path_params).
	reserved_route_metadata_functor(response_media_type).

	merge_route_metadata([], Properties, Properties).
	merge_route_metadata([MetadataProperty| Metadata], Properties0, Properties) :-
		functor(MetadataProperty, Functor, _),
		remove_property_functor(Properties0, Functor, Properties1),
		merge_route_metadata(Metadata, Properties1, Properties2),
		Properties = [MetadataProperty| Properties2].

	open_api_info(info(Title, '1.0.0', 'HTTP router API', [])) :-
		this(This),
		( 	atom(This) ->
			Title = This
		; 	Title = 'HTTP Router API'
		).

	open_api_servers([]).

	remove_property_functor([], _Functor, []).
	remove_property_functor([Property| Properties], Functor, FilteredProperties) :-
		functor(Property, PropertyFunctor, _),
		( 	PropertyFunctor == Functor ->
			remove_property_functor(Properties, Functor, FilteredProperties)
		; 	FilteredProperties = [Property| FilteredProperties0],
			remove_property_functor(Properties, Functor, FilteredProperties0)
		).

	call_route_handler(Handler, Request, Response) :-
		atom(Handler),
		!,
		Goal =.. [Handler, Request, Response],
		::Goal.
	call_route_handler(Handler, _Request, _Response) :-
		domain_error(http_router_handler, Handler).

	template_matches(Template, Path, PathParams) :-
		template_path_segments(Template, TemplateSegments),
		path_segments(Path, PathSegments),
		template_segments_path_params(TemplateSegments, PathSegments, PathParams).

	template_path_segments(Template, TemplateSegments) :-
		atom(Template),
		!,
		path_segments(Template, Segments),
		template_segment_terms(Segments, TemplateSegments).
	template_path_segments(Template, _TemplateSegments) :-
		domain_error(http_router_path_template, Template).

	template_segment_terms([], []).
	template_segment_terms([Segment| Segments], [TemplateSegment| TemplateSegments]) :-
		template_segment(Segment, TemplateSegment),
		template_segment_terms(Segments, TemplateSegments).

	template_segment('*', wildcard) :-
		!.
	template_segment(Segment, parameter(Name, Type)) :-
		placeholder_segment_descriptor(Segment, Name, Type),
		!.
	template_segment(Segment, _TemplateSegment) :-
		malformed_placeholder_segment(Segment),
		!,
		domain_error(http_router_path_template_segment, Segment).
	template_segment(Segment, literal(Segment)).

	placeholder_segment_descriptor(Segment, Name, Type) :-
		atom_codes(Segment, [0'{| Rest]),
		append(DescriptorCodes, [0'}], Rest),
		DescriptorCodes \== [],
		\+ memberchk(0'{, DescriptorCodes),
		\+ memberchk(0'}, DescriptorCodes),
		placeholder_descriptor_name_type(DescriptorCodes, Name, Type).

	placeholder_descriptor_name_type(DescriptorCodes, Name, Type) :-
		( 	append(NameCodes, [0':| TypeCodes], DescriptorCodes) ->
			NameCodes \== [],
			TypeCodes \== [],
			atom_codes(Name, NameCodes),
			placeholder_segment_type(TypeCodes, Type)
		; 	atom_codes(Name, DescriptorCodes),
			Type = string
		).

	placeholder_segment_type(TypeCodes, integer) :-
		atom_codes(integer, TypeCodes),
		!.
	placeholder_segment_type(TypeCodes, number) :-
		atom_codes(number, TypeCodes).

	malformed_placeholder_segment(Segment) :-
		atom_codes(Segment, [0'{| _]).
	malformed_placeholder_segment(Segment) :-
		atom_codes(Segment, Codes),
		append(_, [0'}], Codes).

	template_segments_path_params([], [], []).
	template_segments_path_params([literal(Segment)| TemplateSegments], [Segment| PathSegments], PathParams) :-
		!,
		template_segments_path_params(TemplateSegments, PathSegments, PathParams).
	template_segments_path_params([wildcard| TemplateSegments], [_Segment| PathSegments], PathParams) :-
		!,
		template_segments_path_params(TemplateSegments, PathSegments, PathParams).
	template_segments_path_params([parameter(Name, Type)| TemplateSegments], [Segment| PathSegments], [Name-Value| PathParams]) :-
		template_segment_path_value(Type, Segment, Value),
		template_segments_path_params(TemplateSegments, PathSegments, PathParams).

	template_segment_path_value(integer, Segment, Value) :-
		atom_codes(Segment, Codes),
		catch(number_codes(Value, Codes), _, fail),
		integer(Value),
		!.
	template_segment_path_value(number, Segment, Value) :-
		atom_codes(Segment, Codes),
		catch(number_codes(Value, Codes), _, fail),
		!.
	template_segment_path_value(string, Segment, Segment).

	path_segments(Path, Segments) :-
		atom(Path),
		!,
		atom_codes(Path, Codes0),
		strip_leading_slash(Codes0, Codes),
		path_segments_codes(Codes, Segments).
	path_segments(Path, _Segments) :-
		domain_error(http_router_path, Path).

	strip_leading_slash([0'/| Codes], Codes) :-
		!.
	strip_leading_slash(Codes, Codes).

	path_segments_codes([], []) :-
		!.
	path_segments_codes(Codes, [Segment| Segments]) :-
		path_segment_codes(Codes, SegmentCodes, Tail, HasSeparator),
		atom_codes(Segment, SegmentCodes),
		( 	HasSeparator == true ->
			( 	Tail == [] ->
				Segments = ['']
			; 	path_segments_codes(Tail, Segments)
			)
		; 	Segments = []
		).

	path_segment_codes([], [], [], false).
	path_segment_codes([0'/| Codes], [], Codes, true) :-
		!.
	path_segment_codes([Code| Codes], [Code| SegmentCodes], Tail, HasSeparator) :-
		path_segment_codes(Codes, SegmentCodes, Tail, HasSeparator).

	not_found_response(Request, Response) :-
		( 	::route_not_found_response(Request, Response) ->
			true
		; 	default_not_found_response(Request, Response)
		).

	handle_not_acceptable_response(RouteId, Request, Response) :-
		( 	::route_produces(RouteId, ProducedMediaTypes0) ->
			normalize_produced_media_types(ProducedMediaTypes0, ProducedMediaTypes)
		; 	ProducedMediaTypes = []
		),
		( 	::route_not_acceptable_response(Request, ProducedMediaTypes, Response) ->
			true
		; 	default_not_acceptable_response(Request, Response)
		).

	default_not_found_response(Request, Response) :-
		http::version(Request, Version),
		http::response(Version, status(404, 'Not Found'), [], content('text/plain', text('Not Found')), [], Response).

	default_not_acceptable_response(Request, Response) :-
		http::version(Request, Version),
		http::response(Version, status(406, 'Not Acceptable'), [], content('text/plain', text('Not Acceptable')), [], Response).

	method_not_allowed_response(Request, AllowedMethods0, Response) :-
		effective_allowed_methods(AllowedMethods0, AllowedMethods),
		( 	::route_method_not_allowed_response(Request, AllowedMethods, Response) ->
			true
		; 	default_method_not_allowed_response(Request, AllowedMethods, Response)
		).

	default_method_not_allowed_response(Request, AllowedMethods, Response) :-
		http::version(Request, Version),
		allow_header_value(AllowedMethods, AllowValue),
		http::response(Version, status(405, 'Method Not Allowed'), [allow-AllowValue], content('text/plain', text('Method Not Allowed')), [], Response).

	effective_allowed_methods(AllowedMethods0, AllowedMethods) :-
		append_new_methods([options], AllowedMethods0, AllowedMethods).

	allow_header_value(AllowedMethods, AllowValue) :-
		allow_header_codes(AllowedMethods, AllowCodes),
		atom_codes(AllowValue, AllowCodes).

	allow_header_codes([Method], MethodCodes) :-
		!,
		method_name_codes(Method, MethodCodes).
	allow_header_codes([Method| Methods], AllowCodes) :-
		method_name_codes(Method, MethodCodes),
		allow_header_codes(Methods, RemainingCodes),
		append(MethodCodes, [0',, 32| RemainingCodes], AllowCodes).

	method_name_codes(Method, MethodCodes) :-
		atom(Method),
		atom_codes(Method, MethodCodes0),
		uppercase_ascii_codes(MethodCodes0, MethodCodes).

	uppercase_ascii_codes(Codes, UppercaseCodes) :-
		^^uppercase_ascii_codes(Codes, UppercaseCodes).

	lowercase_ascii_codes(Codes, LowercaseCodes) :-
		^^lowercase_ascii_codes(Codes, LowercaseCodes).

	text_codes(Text, Codes) :-
		atom(Text),
		!,
		atom_codes(Text, Codes).
	text_codes([], []) :-
		!.
	text_codes([Code| Codes], [Code| Codes]) :-
		integer(Code),
		!.
	text_codes([Char| Chars], Codes) :-
		chars_codes([Char| Chars], Codes).

	chars_codes([], []).
	chars_codes([Char| Chars], [Code| Codes]) :-
		atom_codes(Char, [Code]),
		chars_codes(Chars, Codes).

	media_type_atom_parts(MediaType, Type, Subtype) :-
		atom_codes(MediaType, MediaTypeCodes0),
		trim_ows_codes(MediaTypeCodes0, MediaTypeCodes),
		media_range_parts(MediaTypeCodes, Type, Subtype).

	media_range_parts(MediaRangeCodes, Type, Subtype) :-
		split_codes(MediaRangeCodes, 0'/, [TypeCodes0, SubtypeCodes0]),
		trim_ows_codes(TypeCodes0, TypeCodes1),
		trim_ows_codes(SubtypeCodes0, SubtypeCodes1),
		TypeCodes1 \== [],
		SubtypeCodes1 \== [],
		lowercase_ascii_codes(TypeCodes1, TypeCodes),
		lowercase_ascii_codes(SubtypeCodes1, SubtypeCodes),
		atom_codes(Type, TypeCodes),
		atom_codes(Subtype, SubtypeCodes).

	split_codes(Codes, Separator, Parts) :-
		split_codes(Codes, Separator, [], Parts).

	split_codes([], _Separator, CurrentPartCodes0, [CurrentPartCodes]) :-
		reverse(CurrentPartCodes0, CurrentPartCodes).
	split_codes([Separator| Codes], Separator, CurrentPartCodes0, [CurrentPartCodes| Parts]) :-
		!,
		reverse(CurrentPartCodes0, CurrentPartCodes),
		split_codes(Codes, Separator, [], Parts).
	split_codes([Code| Codes], Separator, CurrentPartCodes0, Parts) :-
		split_codes(Codes, Separator, [Code| CurrentPartCodes0], Parts).

	trim_ows_codes(Codes0, Codes) :-
		drop_leading_ows_codes(Codes0, Codes1),
		reverse(Codes1, ReversedCodes1),
		drop_leading_ows_codes(ReversedCodes1, ReversedCodes),
		reverse(ReversedCodes, Codes).

	drop_leading_ows_codes([Code| Codes0], Codes) :-
		ows_code(Code),
		!,
		drop_leading_ows_codes(Codes0, Codes).
	drop_leading_ows_codes(Codes, Codes).

	ows_code(9).
	ows_code(32).

:- end_category.
