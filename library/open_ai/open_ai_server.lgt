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


:- object(open_ai_server(_Backend_),
	implements([http_handler_protocol, open_api_provider_protocol]),
	imports(rest)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-13,
		comment is 'OpenAI-compatible REST server facade backed by an application object.',
		parameters is ['Backend' - 'Application object implementing ``open_ai_backend_protocol``.']
	]).

	:- uses(list, [
		append/2
	]).

	:- protected(open_api_info/1).
	:- mode(open_api_info(-compound), one).
	:- info(open_api_info/1, [
		comment is 'Hook returning the OpenAPI information object for the server facade.',
		argnames is ['Info']
	]).

	:- protected(open_api_servers/1).
	:- mode(open_api_servers(-list(compound)), one).
	:- info(open_api_servers/1, [
		comment is 'Hook returning the OpenAPI server list for the local server facade.',
		argnames is ['Servers']
	]).

	:- protected(open_api_security/1).
	:- mode(open_api_security(-list(compound)), zero_or_one).
	:- info(open_api_security/1, [
		comment is 'Hook returning the OpenAPI security requirements for the server facade.',
		argnames is ['Security']
	]).

	:- protected(open_api_security_scheme/2).
	:- mode(open_api_security_scheme(?atom, ?compound), zero_or_more).
	:- info(open_api_security_scheme/2, [
		comment is 'Hook enumerating OpenAPI security schemes for the server facade.',
		argnames is ['Name', 'Scheme']
	]).

	:- protected(open_api_schema/2).
	:- mode(open_api_schema(?atom, ?compound), zero_or_more).
	:- info(open_api_schema/2, [
		comment is 'Hook enumerating named OpenAPI schemas for the server facade.',
		argnames is ['Name', 'Schema']
	]).

	:- protected(endpoint/5).
	:- mode(endpoint(?atom, ?atom, ?atom, ?atom, ?list(compound)), zero_or_more).
	:- info(endpoint/5, [
		comment is 'Hook enumerating REST endpoint descriptors derived from the generated OpenAI catalog.',
		argnames is ['Id', 'Method', 'Path', 'Action', 'Options']
	]).

	:- protected(open_api_validate_request/1).
	:- mode(open_api_validate_request(?atom), zero_or_more).
	:- info(open_api_validate_request/1, [
		comment is 'Hook selecting operations whose requests should be validated against the OpenAPI contract.',
		argnames is ['Id']
	]).

	:- protected(open_api_validate_response/1).
	:- mode(open_api_validate_response(?atom), zero_or_more).
	:- info(open_api_validate_response/1, [
		comment is 'Hook selecting operations whose responses should be validated against the OpenAPI contract.',
		argnames is ['Id']
	]).

	:- protected(dispatch_open_ai/2).
	:- mode(dispatch_open_ai(+compound, --term), one).
	:- info(dispatch_open_ai/2, [
		comment is 'Dispatches a routed OpenAI request to the backend object and returns a REST result. Backend application errors should be returned as REST result terms; unexpected backend failure or error is left to the ``rest`` action failure handling.',
		argnames is ['Request', 'Result']
	]).

	open_api_info(Info) :-
		open_ai_api::api_info(Info).

	open_api_servers([
		server('/', 'OpenAI-compatible local server')
	]).

	open_api_security(Security) :-
		open_ai_api::security(Security).

	open_api_security_scheme(Name, Scheme) :-
		open_ai_api::security_scheme(Name, Scheme).

	open_api_schema(Name, Schema) :-
		open_ai_api::schema(Name, Schema).

	endpoint(Id, Method, Path, dispatch_open_ai, Options) :-
		open_ai_catalog::operation(Id, Tag, Method, Path, Summary, Parameters, RequestBody, Responses),
		open_ai_catalog::operation_properties(Id, Properties),
		endpoint_options(Tag, Summary, Parameters, RequestBody, Responses, Properties, Options).

	open_api_validate_request(Id) :-
		open_ai_catalog::operation(Id, _, _, _, _, _, _, _).

	open_api_validate_response(Id) :-
		open_ai_catalog::operation(Id, _, _, _, _, _, _, _).

	dispatch_open_ai(Request, Result) :-
		http_core::property(Request, route(OperationId)),
		_Backend_::handle_open_ai(OperationId, Request, Result).

	endpoint_options(Tag, Summary, Parameters, RequestBody, Responses, Properties, Options) :-
		Base = [
			summary(Summary),
			tags([Tag]),
			parameters(Parameters),
			responses(Responses),
			produces(['application/json'])
		],
		request_body_option(RequestBody, RequestBodyOptions),
		append([Base, RequestBodyOptions, Properties], Options).

	request_body_option(none, []) :-
		!.
	request_body_option(RequestBody, [request_body(RequestBody)]).

:- end_object.
