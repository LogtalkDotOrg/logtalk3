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
		date is 2026-06-05,
		comment is 'Unit tests for the "http_cors" library.'
	]).

	:- uses(http_core, [
		body/2, header/3, status/2
	]).

	cover(http_cors).

	test(http_cors_request_origin_2_01, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		http_cors::request_origin(Request, Origin),
		Origin == 'https://app.example.com'.

	test(http_cors_is_cors_request_1_01, true) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		http_cors::is_cors_request(Request).

	test(http_cors_is_cors_request_1_02, fail) :-
		Request = request(get, origin('/resource'), http(1, 1), [], empty, []),
		http_cors::is_cors_request(Request).

	test(http_cors_is_preflight_request_1_01, true) :-
		Request = request(options, origin('/resource'), http(1, 1), [origin-'https://app.example.com', access_control_request_method-'POST'], empty, []),
		http_cors::is_preflight_request(Request).

	test(http_cors_preflight_response_3_01, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'Content-Type, X-Trace-Id'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers([content_type, x_trace_id]),
			allow_credentials(true),
			max_age(3600)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, 'Content-Type, X-Trace-Id'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_max_age, '3600'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_02, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://blocked.example.com',
				access_control_request_method-'POST'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(403, 'Forbidden')),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		body(Response, empty).

	test(http_cors_preflight_response_3_03, error(domain_error(http_cors_preflight_request, _))) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		http_cors::preflight_response(Request, _Response, [allowed_origins(['https://app.example.com'])]).

	test(http_cors_preflight_response_3_04, deterministic) :-
		Request = request(options, origin('/resource'), http(1, 1), [origin-'https://app.example.com', access_control_request_method-'GET'], empty, []),
		http_cors::preflight_response(Request, Response, [allowed_origins(any), allow_credentials(true)]),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_allow_methods, 'GET'),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		body(Response, empty).

	test(http_cors_preflight_response_3_05, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'3gpp-Sbi-Message-Priority'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(requested)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, '3gpp-Sbi-Message-Priority'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_06, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'X-Trace-Id,'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(requested)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(403, 'Forbidden')),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		\+ header(Response, access_control_allow_origin, _),
		body(Response, empty).

	test(http_cors_preflight_response_3_07, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'X-Trace-Id,,X-Other'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(requested)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(403, 'Forbidden')),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		\+ header(Response, access_control_allow_origin, _),
		body(Response, empty).

	test(http_cors_preflight_response_3_08, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'Foo!Bar'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(['foo!bar'])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, 'Foo!bar'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_09, error(domain_error(option, allowed_headers(['Foo!Bar'])))) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'Foo!Bar'
			],
			empty,
			[]
		),
		http_cors::preflight_response(Request, _Response, [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(['Foo!Bar'])
		]).

	test(http_cors_preflight_response_3_10, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'X-Trace-Id'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(any)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, '*'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_11, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'X-Trace-Id'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(any),
			allow_credentials(true)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, 'X-Trace-Id'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_12, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'Authorization'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post]),
			allowed_headers(any)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, 'Authorization'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_13, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST',
				access_control_request_headers-'X-Trace-Id'
			],
			empty,
			[
				cors([
					allowed_headers(['*'])
				])
			]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods([post])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, access_control_allow_headers, '*'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

	test(http_cors_preflight_response_3_14, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://api.example.com',
				access_control_request_method-'POST'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://*.example.com']),
			allowed_methods([post])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://api.example.com'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		body(Response, empty).

	test(http_cors_preflight_response_3_15, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://a.b.example.com',
				access_control_request_method-'POST'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://*.example.com']),
			allowed_methods([post])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(403, 'Forbidden')),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		\+ header(Response, access_control_allow_origin, _),
		body(Response, empty).

	test(http_cors_preflight_response_3_16, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://api.example.com:8443',
				access_control_request_method-'POST'
			],
			empty,
			[]
		),
		Options = [
			allowed_origins(['https://*.example.com:8443']),
			allowed_methods([post])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://api.example.com:8443'),
		header(Response, access_control_allow_methods, 'POST'),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		body(Response, empty).

	test(http_cors_preflight_response_3_17, error(domain_error(option, allowed_origins(['https://api*.example.com'])))) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://api.example.com',
				access_control_request_method-'POST'
			],
			empty,
			[]
		),
		http_cors::preflight_response(Request, _Response, [
			allowed_origins(['https://api*.example.com']),
			allowed_methods([post])
		]).

	test(http_cors_preflight_response_3_18, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'POST'
			],
			empty,
			[
				effective_methods([get, head, post, options])
			]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods(any)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, allow, 'GET, HEAD, POST, OPTIONS'),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'GET, HEAD, POST'),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		body(Response, empty).

	test(http_cors_preflight_response_3_19, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'GET'
			],
			empty,
			[
				effective_methods([get, head, options])
			]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods(['*'])
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'GET, HEAD'),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		body(Response, empty).

	test(http_cors_preflight_response_3_20, deterministic) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'PUT'
			],
			empty,
			[
				effective_methods([get, head, post, options])
			]
		),
		Options = [
			allowed_origins(['https://app.example.com']),
			allowed_methods(any)
		],
		http_cors::preflight_response(Request, Response, Options),
		status(Response, status(403, 'Forbidden')),
		header(Response, vary, 'Origin, Access-Control-Request-Method'),
		\+ header(Response, access_control_allow_origin, _),
		body(Response, empty).

	test(http_cors_preflight_response_3_21, error(domain_error(http_cors_options, [allowed_methods(any)]))) :-
		Request = request(
			options,
			origin('/resource'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'GET'
			],
			empty,
			[]
		),
		http_cors::preflight_response(Request, _Response, [
			allowed_origins(['https://app.example.com']),
			allowed_methods(any)
		]).

	test(http_cors_add_response_headers_4_01, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [x_trace_id-'abc-123'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(['https://app.example.com']),
			expose_headers([x_trace_id]),
			allow_credentials(true)
		],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_expose_headers, 'X-Trace-Id'),
		header(Response, vary, 'Origin'),
		header(Response, x_trace_id, 'abc-123').

	test(http_cors_add_response_headers_4_02, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://blocked.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(ok)), []),
		Options = [allowed_origins(['https://app.example.com'])],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		status(Response, status(200, 'OK')),
		header(Response, vary, 'Origin'),
		body(Response, content('text/plain', text(ok))),
		\+ header(Response, access_control_allow_origin, _).

	test(http_cors_add_response_headers_4_03, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [vary-'Origin'], content('text/plain', text(ok)), []),
		Options = [allowed_origins(['https://app.example.com'])],
		http_cors::add_response_headers(Request, Response0, Response1, Options),
		http_cors::add_response_headers(Request, Response1, Response2, Options),
		Response1 == Response2,
		header(Response2, vary, 'Origin').

	test(http_cors_add_response_headers_4_04, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), ['foo!bar'-'baz'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(['https://app.example.com']),
			expose_headers(['foo!bar'])
		],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_expose_headers, 'Foo!bar'),
		header(Response, vary, 'Origin'),
		header(Response, 'foo!bar', 'baz').

	test(http_cors_add_response_headers_4_05, error(domain_error(option, expose_headers(['Foo!Bar'])))) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), ['foo!bar'-'baz'], content('text/plain', text(ok)), []),
		http_cors::add_response_headers(Request, Response0, _Response, [
			allowed_origins(['https://app.example.com']),
			expose_headers(['Foo!Bar'])
		]).

	test(http_cors_add_response_headers_4_06, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [x_trace_id-'abc-123'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(['https://app.example.com']),
			expose_headers(any)
		],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_expose_headers, '*'),
		header(Response, vary, 'Origin'),
		header(Response, x_trace_id, 'abc-123').

	test(http_cors_add_response_headers_4_07, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [x_trace_id-'abc-123', set_cookie-'SID=abc'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(['https://app.example.com']),
			allow_credentials(true),
			expose_headers(any)
		],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_expose_headers, 'X-Trace-Id'),
		header(Response, vary, 'Origin'),
		header(Response, x_trace_id, 'abc-123').

	test(http_cors_add_response_headers_4_08, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [vary-('*')], content('text/plain', text(ok)), []),
		Options = [allowed_origins(['https://app.example.com'])],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, vary, '*').

	test(http_cors_add_response_headers_4_09, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [x_trace_id-'abc-123'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(['https://app.example.com']),
			allow_credentials(true),
			expose_headers(any)
		],
		http_cors::add_response_headers(Request, Response0, Response1, Options),
		http_cors::add_response_headers(Request, Response1, Response2, Options),
		Response1 == Response2,
		header(Response2, access_control_expose_headers, 'X-Trace-Id').

	test(http_cors_add_response_headers_4_10, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [x_trace_id-'abc-123'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(any),
			allow_credentials(true),
			expose_headers(any)
		],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_expose_headers, 'X-Trace-Id'),
		header(Response, vary, 'Origin'),
		header(Response, x_trace_id, 'abc-123').

	test(http_cors_add_response_headers_4_11, deterministic) :-
		Request = request(get, origin('/resource'), http(1, 1), [origin-'https://api.example.com'], empty, []),
		Response0 = response(http(1, 1), status(200, 'OK'), [x_trace_id-'abc-123'], content('text/plain', text(ok)), []),
		Options = [
			allowed_origins(['https://*.example.com']),
			allow_credentials(true),
			expose_headers(any)
		],
		http_cors::add_response_headers(Request, Response0, Response, Options),
		header(Response, access_control_allow_origin, 'https://api.example.com'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_expose_headers, 'X-Trace-Id'),
		header(Response, vary, 'Origin'),
		header(Response, x_trace_id, 'abc-123').

	test(http_cors_router_integration_2_01, deterministic) :-
		Request = request(get, origin('/cors/pages/42'), http(1, 1), [origin-'https://app.example.com'], empty, []),
		cors_response_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_expose_headers, 'X-Trace-Id'),
		header(Response, vary, 'Origin'),
		body(Response, content('text/plain', text(cors_page))).

	test(http_cors_router_integration_2_02, deterministic) :-
		Request = request(
			options,
			origin('/cors/options/42'),
			http(1, 1),
			[
				origin-'https://app.example.com',
				access_control_request_method-'GET',
				access_control_request_headers-'Content-Type, X-Trace-Id'
			],
			empty,
			[]
		),
		cors_automatic_options_http_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, allow, 'GET, HEAD, OPTIONS'),
		header(Response, access_control_allow_origin, 'https://app.example.com'),
		header(Response, access_control_allow_methods, 'GET'),
		header(Response, access_control_allow_headers, 'Content-Type, X-Trace-Id'),
		header(Response, access_control_allow_credentials, 'true'),
		header(Response, access_control_max_age, '600'),
		header(Response, vary, 'Origin, Access-Control-Request-Method, Access-Control-Request-Headers'),
		body(Response, empty).

:- end_object.
