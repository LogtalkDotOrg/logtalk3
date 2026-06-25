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


:- object(open_ai_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-25,
		comment is 'OpenAI-compatible client facade using http_client(http_socket_process) for HTTPS/WSS support.'
	]).

	:- public(request/5).
	:- mode(request(+atom, +list(pair), +compound, --compound, +list(compound)), one_or_error).
	:- info(request/5, [
		comment is 'Calls an OpenAI operation by operation identifier. Path parameters are supplied as ``Name-Value`` pairs. The body is ignored for methods without a request body. Options include ``base_url/1``, ``api_key/1``, ``organization/1``, ``project/1``, and ordinary ``http_client`` request options.',
		argnames is ['OperationId', 'PathParameters', 'Body', 'Response', 'Options']
	]).

	:- public(operation_url/4).
	:- mode(operation_url(+atom, +list(pair), -atom, +list(compound)), one_or_error).
	:- info(operation_url/4, [
		comment is 'Builds the absolute request URL for an operation and path parameters.',
		argnames is ['OperationId', 'PathParameters', 'URL', 'Options']
	]).

	:- uses(list, [
		member/2, reverse/2, select/3
	]).

	request(OperationId, PathParameters, Body, Response, Options) :-
		open_ai_catalog::operation(OperationId, _Tag, Method, _Path, _Summary, _Parameters, RequestBody, _Responses),
		valid_operation_method(Method),
		operation_url(OperationId, PathParameters, URL, Options),
		http_request_options(Options, HTTPOptions0),
		request_body_options(RequestBody, Body, HTTPOptions0, HTTPOptions),
		request_method(Method, URL, Body, Response, HTTPOptions).

	operation_url(OperationId, PathParameters, URL, Options) :-
		open_ai_catalog::operation(OperationId, _Tag, _Method, Path, _Summary, _Parameters, _RequestBody, _Responses),
		base_url(Options, BaseURL),
		expand_path_parameters(PathParameters, Path, ExpandedPath),
		atom_concat(BaseURL, ExpandedPath, URL).

	request_method(get, URL, _Body, Response, Options) :-
		http_client(http_socket_process)::get(URL, Response, Options).
	request_method(delete, URL, _Body, Response, Options) :-
		http_client(http_socket_process)::delete(URL, Response, Options).
	request_method(post, URL, Body, Response, Options) :-
		http_client(http_socket_process)::post(URL, Body, Response, Options).

	valid_operation_method(Method) :-
		(	operation_method(Method) ->
			true
		;	domain_error(open_ai_operation_method, Method)
		).

	operation_method(get).
	operation_method(delete).
	operation_method(post).

	request_body_options(none, _Body, Options, Options).
	request_body_options(request_body(_, _, _), _Body, Options, Options).

	base_url(Options, BaseURL) :-
		(	select(base_url(BaseURL), Options, _) ->
			true
		;	BaseURL = 'https://api.openai.com/v1'
		).

	http_request_options(Options, HTTPOptions) :-
		request_headers(Options, Headers),
		http_request_options(Options, [headers(Headers)], HTTPOptions).

	http_request_options([], Acc, HTTPOptions) :-
		reverse(Acc, HTTPOptions).
	http_request_options([Option| Options], Acc, HTTPOptions) :-
		(	http_request_option(Option, Acc, Acc2) ->
			http_request_options(Options, Acc2, HTTPOptions)
		;	domain_error(open_ai_client_option, Option)
		).

	request_headers(Options, Headers) :-
		(	member(headers(Headers0), Options) ->
			true
		;	Headers0 = []
		),
		api_key_headers(Options, Headers0, Headers1),
		organization_headers(Options, Headers1, Headers2),
		project_headers(Options, Headers2, Headers).

	api_key_headers(Options, Headers0, [authorization-Authorization| Headers0]) :-
		member(api_key(APIKey), Options),
		!,
		atom_concat('Bearer ', APIKey, Authorization).
	api_key_headers(_, Headers, Headers).

	organization_headers(Options, Headers0, [openai_organization-Organization| Headers0]) :-
		member(organization(Organization), Options),
		!.
	organization_headers(_, Headers, Headers).

	project_headers(Options, Headers0, [openai_project-Project| Headers0]) :-
		member(project(Project), Options),
		!.
	project_headers(_, Headers, Headers).

	http_request_option(base_url(_), Acc, Acc).
	http_request_option(api_key(_), Acc, Acc).
	http_request_option(organization(_), Acc, Acc).
	http_request_option(project(_), Acc, Acc).
	http_request_option(headers(_), Acc, Acc).
	http_request_option(query(Query), Acc, [query(Query)| Acc]).
	http_request_option(version(Version), Acc, [version(Version)| Acc]).
	http_request_option(properties(Properties), Acc, [properties(Properties)| Acc]).
	http_request_option(connection_options(ConnectionOptions), Acc, [connection_options(ConnectionOptions)| Acc]).

	expand_path_parameters([], Path, Path).
	expand_path_parameters([Name-Value| Parameters], Path0, Path) :-
		atom(Name),
		atom(Value),
		!,
		atom_concat('{', Name, Prefix),
		atom_concat(Prefix, '}', Placeholder),
		replace_placeholder(Path0, Placeholder, Value, Path1),
		expand_path_parameters(Parameters, Path1, Path).
	expand_path_parameters([Parameter| _], _, _) :-
		domain_error(open_ai_path_parameter, Parameter).

	replace_placeholder(Path0, Placeholder, Value, Path) :-
		sub_atom(Path0, Before, _Length, After, Placeholder),
		!,
		sub_atom(Path0, 0, Before, _, Prefix),
		sub_atom(Path0, _, After, 0, Suffix),
		atom_concat(Prefix, Value, PrefixValue),
		atom_concat(PrefixValue, Suffix, Path1),
		replace_placeholder(Path1, Placeholder, Value, Path).
	replace_placeholder(Path, _, _, Path).

:- end_object.
