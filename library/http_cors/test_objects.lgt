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


:- object(cors_response_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Router object used by the http_cors tests to exercise response middleware decoration and route metadata overrides.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares route metadata used by the CORS response middleware test router object.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(add_cors_headers/3).
	:- info(add_cors_headers/3, [
		comment is 'Response middleware handler that decorates routed responses with CORS headers.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- protected(show_page/2).
	:- info(show_page/2, [
		comment is 'Route handler used by the CORS response middleware router object for the ``GET /cors/pages/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	response_middleware(cors, add_cors_headers).

	route(show_page, get, '/cors/pages/{id}', show_page).

	route_metadata(show_page, [
		cors([
			allowed_origins(['https://app.example.com']),
			expose_headers([x_trace_id]),
			allow_credentials(true)
		])
	]).

	add_cors_headers(Request, Response0, Response) :-
		Defaults = [
			allowed_origins([]),
			allowed_methods([get]),
			allowed_headers(requested),
			expose_headers([]),
			allow_credentials(false),
			max_age(none)
		],
		http_cors::add_response_headers(Request, Response0, Response, Defaults).

	show_page(Request, Response) :-
		http::property(Request, route(show_page)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, cors([
			allowed_origins(['https://app.example.com']),
			expose_headers([x_trace_id]),
			allow_credentials(true)
		])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [x_trace_id-'abc-123'], content('text/plain', text(cors_page)), [], Response).

:- end_object.


:- object(cors_automatic_options_http_router,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-25,
		comment is 'Router object used by the http_cors tests to exercise automatic ``OPTIONS`` preflight generation using the router hook and annotated synthetic request context.'
	]).

	:- protected(route_metadata/2).
	:- info(route_metadata/2, [
		comment is 'Declares route metadata used by the automatic ``OPTIONS`` CORS test router object.',
		argnames is ['Id', 'Metadata']
	]).

	:- protected(route_automatic_options_response/3).
	:- info(route_automatic_options_response/3, [
		comment is 'Builds CORS preflight responses for automatic ``OPTIONS`` requests using the current synthetic request annotations.',
		argnames is ['Request', 'EffectiveMethods', 'Response']
	]).

	:- protected(show_page/2).
	:- info(show_page/2, [
		comment is 'Route handler used by the automatic ``OPTIONS`` CORS test router object for the ``GET /cors/options/{id}`` path.',
		argnames is ['Request', 'Response']
	]).

	route(show_page, get, '/cors/options/{id}', show_page).

	route_metadata(show_page, [
		cors([
			allowed_origins(['https://app.example.com']),
			allowed_methods([get]),
			allowed_headers(requested),
			allow_credentials(true),
			max_age(600)
		])
	]).

	route_automatic_options_response(Request, EffectiveMethods, Response) :-
		EffectiveMethods == [get, head, options],
		http::property(Request, automatic_options(true)),
		http::property(Request, effective_methods(EffectiveMethods)),
		http::property(Request, route(show_page)),
		http::property(Request, path_params([id-'42'])),
		http::property(Request, cors([
			allowed_origins(['https://app.example.com']),
			allowed_methods([get]),
			allowed_headers(requested),
			allow_credentials(true),
			max_age(600)
		])),
		Defaults = [
			allowed_origins([]),
			allowed_methods([get, post]),
			allowed_headers([]),
			expose_headers([]),
			allow_credentials(false),
			max_age(none)
		],
		http_cors::preflight_response(Request, Response, Defaults).

	show_page(Request, Response) :-
		http::property(Request, route(show_page)),
		http::property(Request, path_params([id-'42'])),
		http::version(Request, Version),
		http::response(Version, status(200, 'OK'), [], content('text/plain', text(cors_options_page)), [], Response).

:- end_object.
