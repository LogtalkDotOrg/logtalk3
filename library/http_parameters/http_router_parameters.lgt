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


:- category(http_router_parameters,
	extends(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-28,
		comment is 'Declarative router companion category that reuses ``http_parameters`` declarations for request extraction and OpenAPI metadata generation.',
		remarks is [
			'Route declarations' - 'Importing objects define parameter declarations per route using ``route_parameter_declarations/2``.',
			'Handler helper' - 'Route handlers can call ``route_parameters/2`` on the routed request to extract typed values using the matched route declarations.',
			'OpenAPI metadata' - 'The category generates route metadata for query and path parameters, form request bodies, and a default ``400 Bad Request`` response descriptor from the same declarations. Importing objects can extend or override that metadata using ``route_parameter_extra_metadata/2`` or by overriding ``route_metadata/2`` and delegating with ``^^route_metadata/2``.'
		]
	]).

	:- public(route_parameters/2).
	:- mode(route_parameters(+compound, -list(compound)), one_or_error).
	:- info(route_parameters/2, [
		comment is 'Extracts typed parameters for the route identified by the routed request ``route/1`` annotation.',
		argnames is ['Request', 'Parameters']
	]).

	:- protected(route_parameter_declarations/2).
	:- mode(route_parameter_declarations(?atom, ?list(compound)), zero_or_one).
	:- info(route_parameter_declarations/2, [
		comment is 'Optional hook predicate that returns the ``http_parameters`` declaration list for a route identifier.',
		argnames is ['RouteId', 'Declarations']
	]).

	:- protected(route_parameter_request_body_description/2).
	:- mode(route_parameter_request_body_description(?atom, ?atom), zero_or_one).
	:- info(route_parameter_request_body_description/2, [
		comment is 'Optional hook predicate that customizes the generated OpenAPI request-body description for form parameter declarations.',
		argnames is ['RouteId', 'Description']
	]).

	:- protected(route_parameter_extra_metadata/2).
	:- mode(route_parameter_extra_metadata(?atom, ?list(compound)), zero_or_one).
	:- info(route_parameter_extra_metadata/2, [
		comment is 'Optional hook predicate that returns additional route metadata terms to merge with the metadata generated from parameter declarations.',
		argnames is ['RouteId', 'Metadata']
	]).

	:- uses(list, [
		append/3, member/2
	]).

	route_parameters(Request, Parameters) :-
		validate_request(Request),
		routed_request_route(Request, RouteId),
		route_parameter_declarations_or_empty(RouteId, Declarations),
		http_parameters::parameters(Request, Declarations, Parameters).

	route_metadata(RouteId, Metadata) :-
		generated_route_metadata(RouteId, GeneratedMetadata),
		route_parameter_extra_metadata_or_empty(RouteId, ExtraMetadata),
		merge_route_parameter_metadata(GeneratedMetadata, ExtraMetadata, Metadata),
		Metadata \== [].

	validate_request(Request) :-
		http_core::is_request(Request),
		!.
	validate_request(Request) :-
		domain_error(http_request, Request).

	routed_request_route(Request, RouteId) :-
		http_core::property(Request, route(RouteId)),
		!.
	routed_request_route(Request, _RouteId) :-
		domain_error(http_routed_request, Request).

	route_parameter_declarations_or_empty(RouteId, Declarations) :-
		(   ::route_parameter_declarations(RouteId, Declarations0) ->
			validate_route_parameter_declarations(RouteId, Declarations0),
			Declarations = Declarations0
		;   Declarations = []
		).

	validate_route_parameter_declarations(RouteId, Declarations) :-
		(   route_path_declarations(Declarations, PathDeclarations) ->
			validate_route_path_declarations(PathDeclarations, RouteId)
		;   true
		).

	route_path_declarations(Declarations, PathDeclarations) :-
		nonvar(Declarations),
		route_path_declarations_list(Declarations, PathDeclarations).

	route_path_declarations_list([], []).
	route_path_declarations_list([Declaration| Declarations], PathDeclarations) :-
		(   route_path_declaration(Declaration, Name, Type) ->
			PathDeclarations = [parameter(Name, Type)| RestPathDeclarations]
		;   PathDeclarations = RestPathDeclarations
		),
		route_path_declarations_list(Declarations, RestPathDeclarations).

	route_path_declaration(parameter(Name, path, Type, _Options), Name, Type) :-
		atom(Name),
		supported_path_scalar_type(Type).

	supported_path_scalar_type(string).
	supported_path_scalar_type(text).
	supported_path_scalar_type(atom).
	supported_path_scalar_type(integer).
	supported_path_scalar_type(number).
	supported_path_scalar_type(boolean).

	validate_route_path_declarations([], _RouteId) :-
		!.
	validate_route_path_declarations([parameter(Name, Type)| Declarations], RouteId) :-
		::route(RouteId, _Method, PathTemplate, _Handler),
		^^path_template_parameters(PathTemplate, PathTemplateParameters),
		(   select_path_template_parameter(Name, PathTemplateParameters, PathTemplateType) ->
			validate_route_path_declaration_type(Name, Type, PathTemplateType)
		;   domain_error(http_parameter_declaration(Name, path), not_in_route_path(RouteId))
		),
		validate_route_path_declarations(Declarations, RouteId).

	select_path_template_parameter(Name, [parameter(Name, Type)| _Parameters], Type) :-
		!.
	select_path_template_parameter(Name, [_Parameter| Parameters], Type) :-
		select_path_template_parameter(Name, Parameters, Type).

	validate_route_path_declaration_type(_Name, integer, integer) :-
		!.
	validate_route_path_declaration_type(_Name, number, number) :-
		!.
	validate_route_path_declaration_type(_Name, string, string) :-
		!.
	validate_route_path_declaration_type(_Name, text, string) :-
		!.
	validate_route_path_declaration_type(_Name, atom, string) :-
		!.
	validate_route_path_declaration_type(Name, Type, PathTemplateType) :-
		domain_error(http_parameter_declaration(Name, path), incompatible_route_path_type(Type, PathTemplateType)).

	route_parameter_extra_metadata_or_empty(RouteId, Metadata) :-
		(   ::route_parameter_extra_metadata(RouteId, Metadata0) ->
			Metadata = Metadata0
		;   Metadata = []
		).

	generated_route_metadata(RouteId, Metadata) :-
		route_parameter_declarations_or_empty(RouteId, Declarations),
		(   Declarations == [] ->
			Metadata = []
		;   generated_route_parameters_metadata(Declarations, ParametersMetadata),
			generated_route_request_body_metadata(RouteId, Declarations, RequestBodyMetadata),
			generated_route_responses_metadata(ResponsesMetadata),
			append_generated_metadata(ParametersMetadata, RequestBodyMetadata, ResponsesMetadata, Metadata)
		).

	generated_route_parameters_metadata(Declarations, [parameters(Parameters)]) :-
		http_parameters::open_api_parameters(Declarations, Parameters),
		Parameters \== [],
		!.
	generated_route_parameters_metadata(_Declarations, []).

	generated_route_request_body_metadata(RouteId, Declarations, [request_body(RequestBody)]) :-
		route_request_body_description(RouteId, Description),
		http_parameters::open_api_request_body(Declarations, Description, RequestBody),
		!.
	generated_route_request_body_metadata(_RouteId, _Declarations, []).

	route_request_body_description(RouteId, Description) :-
		(   ::route_parameter_request_body_description(RouteId, Description0) ->
			Description = Description0
		;   Description = 'Form parameters.'
		).

	generated_route_responses_metadata([responses([Response])]) :-
		http_parameters::open_api_bad_request_response(Response).

	append_generated_metadata([], [], ResponsesMetadata, ResponsesMetadata).
	append_generated_metadata([], RequestBodyMetadata, ResponsesMetadata, Metadata) :-
		append(RequestBodyMetadata, ResponsesMetadata, Metadata).
	append_generated_metadata(ParametersMetadata, [], ResponsesMetadata, Metadata) :-
		append(ParametersMetadata, ResponsesMetadata, Metadata).
	append_generated_metadata(ParametersMetadata, RequestBodyMetadata, ResponsesMetadata, Metadata) :-
		append(ParametersMetadata, RequestBodyMetadata, Metadata0),
		append(Metadata0, ResponsesMetadata, Metadata).

	merge_route_parameter_metadata(GeneratedMetadata, ExtraMetadata, Metadata) :-
		metadata_property(parameters, GeneratedMetadata, GeneratedParameters),
		metadata_property(request_body, GeneratedMetadata, GeneratedRequestBody),
		metadata_property(responses, GeneratedMetadata, GeneratedResponses),
		metadata_property(parameters, ExtraMetadata, ExtraParameters),
		metadata_property(request_body, ExtraMetadata, ExtraRequestBody),
		metadata_property(responses, ExtraMetadata, ExtraResponses),
		ordinary_metadata_terms(ExtraMetadata, Metadata0),
		merge_parameters_metadata(GeneratedParameters, ExtraParameters, MergedParameters),
		merge_request_body_metadata(GeneratedRequestBody, ExtraRequestBody, MergedRequestBody),
		merge_responses_metadata(GeneratedResponses, ExtraResponses, MergedResponses),
		append_metadata_property(parameters, MergedParameters, Metadata0, Metadata1),
		append_metadata_property(request_body, MergedRequestBody, Metadata1, Metadata2),
		append_metadata_property(responses, MergedResponses, Metadata2, Metadata).

	metadata_property(_Name, [], none).
	metadata_property(Name, [Property| _Metadata], Value) :-
		nonvar(Property),
		functor(Property, Name, 1),
		arg(1, Property, Value),
		!.
	metadata_property(Name, [_Property| Metadata], Value) :-
		metadata_property(Name, Metadata, Value).

	ordinary_metadata_terms([], []).
	ordinary_metadata_terms([Property| Metadata], Terms) :-
		(   special_metadata_property(Property) ->
			Terms = RestTerms
		;   Terms = [Property| RestTerms]
		),
		ordinary_metadata_terms(Metadata, RestTerms).

	special_metadata_property(Property) :-
		nonvar(Property),
		compound(Property),
		functor(Property, Functor, 1),
		member(Functor, [parameters, request_body, responses]).

	merge_parameters_metadata(none, none, none).
	merge_parameters_metadata(GeneratedParameters, none, GeneratedParameters) :-
		GeneratedParameters \== none,
		!.
	merge_parameters_metadata(none, ExtraParameters, ExtraParameters) :-
		!.
	merge_parameters_metadata(GeneratedParameters, ExtraParameters, MergedParameters) :-
		merge_open_api_parameters(GeneratedParameters, ExtraParameters, MergedParameters).

	merge_open_api_parameters([], ExtraParameters, ExtraParameters).
	merge_open_api_parameters([GeneratedParameter| GeneratedParameters], ExtraParameters0, [Parameter| Parameters]) :-
		GeneratedParameter = parameter(Name, In, _Description, _Required, _Schema),
		(   select_open_api_parameter(Name, In, ExtraParameters0, Parameter, ExtraParameters) ->
			true
		;   Parameter = GeneratedParameter,
			ExtraParameters = ExtraParameters0
		),
		merge_open_api_parameters(GeneratedParameters, ExtraParameters, Parameters).

	select_open_api_parameter(Name, In, [Parameter| Parameters], Parameter, Parameters) :-
		Parameter = parameter(Name, In, _Description, _Required, _Schema),
		!.
	select_open_api_parameter(Name, In, [Parameter| Parameters0], SelectedParameter, [Parameter| Parameters]) :-
		select_open_api_parameter(Name, In, Parameters0, SelectedParameter, Parameters).

	merge_request_body_metadata(_GeneratedRequestBody, ExtraRequestBody, ExtraRequestBody) :-
		ExtraRequestBody \== none,
		!.
	merge_request_body_metadata(GeneratedRequestBody, _ExtraRequestBody, GeneratedRequestBody).

	merge_responses_metadata(none, none, none).
	merge_responses_metadata(GeneratedResponses, none, GeneratedResponses) :-
		GeneratedResponses \== none,
		!.
	merge_responses_metadata(none, ExtraResponses, ExtraResponses) :-
		!.
	merge_responses_metadata(_GeneratedResponses, ExtraResponses, ExtraResponses) :-
		responses_define_error(ExtraResponses),
		!.
	merge_responses_metadata(GeneratedResponses, ExtraResponses, MergedResponses) :-
		append(ExtraResponses, GeneratedResponses, MergedResponses).

	responses_define_error([response(Status, _Description, _MediaDescriptors)| _Responses]) :-
		Status == 400,
		!.
	responses_define_error([_Response| Responses]) :-
		responses_define_error(Responses).

	append_metadata_property(_Name, none, Metadata, Metadata).
	append_metadata_property(Name, Value, Metadata0, Metadata) :-
		Property =.. [Name, Value],
		append(Metadata0, [Property], Metadata).

:- end_category.
