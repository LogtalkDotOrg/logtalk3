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
		date is 2026-07-08,
		comment is 'Unit tests for the http_authenticate library.'
	]).

	:- uses(http_core, [
		body/2, header/3, property/2, status/2
	]).

	cover(http_authenticate).
	cover(http_htpasswd_verifier(_)).
	cover(http_server_core_basic_handler(_, _, _)).
	cover(http_authenticate_test_verifier).
	cover(http_authenticate_test_handler).
	cover(http_authenticate_test_router).

	test(http_authenticate_01, deterministic(Challenge == ParsedChallenge)) :-
		Challenge = basic_challenge([
			realm('test-realm'),
			charset(utf_8)
		]),
		http_authenticate::generate_challenge(Challenge, HeaderValue),
		http_authenticate::parse_challenge(HeaderValue, ParsedChallenge).

	test(http_authenticate_02, deterministic(Authorization == ParsedAuthorization)) :-
		Authorization = basic_authorization([
			username('Mufasa'),
			password('Circle Of Life')
		]),
		http_authenticate::generate_authorization(Authorization, HeaderValue),
		http_authenticate::parse_authorization(HeaderValue, ParsedAuthorization).

	test(http_authenticate_03, deterministic) :-
		request_with_authorization('/protected', Request),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, continue(VerifiedRequest), [realm('test-realm')]),
		property(VerifiedRequest, basic_username('Mufasa')),
		property(VerifiedRequest, basic_realm('test-realm')),
		property(VerifiedRequest, basic_authorization(basic_authorization([
			username('Mufasa'),
			password('Circle Of Life')
		]))).

	test(http_authenticate_04, deterministic) :-
		Request = request(get, origin('/protected'), http(1, 1), [], empty, []),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, respond(Response), [realm('test-realm')]),
		status(Response, status(401, 'Unauthorized')),
		http_authenticate::challenge(Response, basic_challenge([
			realm('test-realm'),
			charset(none)
		])).

	test(http_authenticate_05, deterministic) :-
		request_with_invalid_authorization('/protected', Request),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, respond(Response), [realm('test-realm')]),
		status(Response, status(401, 'Unauthorized')).

	test(http_authenticate_06, deterministic) :-
		request_with_authorization('/protected', Request),
		http_server_core_basic_handler(http_authenticate_test_verifier, http_authenticate_test_handler, [realm('test-realm')])::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('Mufasa'))).

	test(http_authenticate_07, deterministic) :-
		Request = request(get, origin('/secret'), http(1, 1), [], empty, []),
		http_authenticate_test_router::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		header(Response, x_router_stage, routed),
		http_authenticate::challenge(Response, basic_challenge([
			realm('test-realm'),
			charset(none)
		])).

	test(http_authenticate_08, deterministic) :-
		request_with_authorization('/secret', Request),
		http_authenticate_test_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, x_router_stage, routed),
		body(Response, content('text/plain', text('Mufasa'))).

	test(http_authenticate_09, deterministic, [condition(current_prolog_flag(bounded, false))]) :-
		^^file_path('test_files/supported.htpasswd', Path),
		http_htpasswd_verifier(Path)::verify('ignored-realm', 'Mufasa', 'Circle Of Life').

	test(http_authenticate_10, error(domain_error(http_password_file(Path), unsupported(1, '2y')))) :-
		^^file_path('test_files/unsupported.htpasswd', Path),
		http_htpasswd_verifier(Path)::verify('ignored-realm', 'Mufasa', 'Circle Of Life').

	test(http_authenticate_11, deterministic) :-
		request_with_malformed_authorization('/protected', Request),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, respond(Response), [realm('test-realm')]),
		status(Response, status(401, 'Unauthorized')).

	test(http_authenticate_12, error(existence_error(http_password_file, Path))) :-
		^^file_path('test_files/missing.htpasswd', Path),
		request_with_authorization('/protected', Request),
		http_authenticate::protect_request(Request, http_htpasswd_verifier(Path), _Action, [realm('ignored-realm')]).

	test(http_authenticate_13, error(domain_error(option, status(status(200, 'OK'))))) :-
		Request = request(get, origin('/protected'), http(1, 1), [], empty, []),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, _Action, [status(status(200, 'OK'))]).

	test(http_authenticate_14, error(domain_error(http_authenticate_unauthorized_response_overlay_option, realm('unexpected')))) :-
		Response0 = response(http(1, 1), status(401, 'Unauthorized'), [], empty, []),
		Challenge = basic_challenge([
			realm('test-realm'),
			charset(none)
		]),
		http_authenticate::unauthorized_response(Challenge, Response0, _Response, [realm('unexpected')]).

	test(http_authenticate_15, error(domain_error(http_password_file(Path), invalid(1)))) :-
		^^file_path('test_files/invalid_sha.htpasswd', Path),
		http_htpasswd_verifier(Path)::verify('ignored-realm', 'Mufasa', 'Circle Of Life').

	test(http_authenticate_16, deterministic(ParsedChallenge == Challenge)) :-
		Challenge = basic_challenge([
			realm('test-realm'),
			charset(utf_8)
		]),
		OverlayStatus = status(401, ['U', 'n', 'a', 'u', 't', 'h', 'o', 'r', 'i', 'z', 'e', 'd']),
		OverlayBody = content('text/plain', text('overlay body')),
		Response0 = response(http(1, 1), status(401, 'Unauthorized'), [x_base-yes], empty, [base_property(ok)]),
		http_authenticate::unauthorized_response(Challenge, Response0, Response, [
			status(OverlayStatus),
			headers([x_overlay-yes]),
			body(OverlayBody),
			properties([overlay_property(ok)])
		]),
		status(Response, OverlayStatus),
		header(Response, x_base, yes),
		header(Response, x_overlay, yes),
		body(Response, OverlayBody),
		property(Response, base_property(ok)),
		property(Response, overlay_property(ok)),
		http_authenticate::challenge(Response, ParsedChallenge).

	test(http_authenticate_17, error(domain_error(option, headers([x_test-foo(bar)])))) :-
		Request = request(get, origin('/protected'), http(1, 1), [], empty, []),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, _Action, [headers([x_test-foo(bar)]), realm('test-realm')]).

	test(http_authenticate_18, error(domain_error(option, body(foo)))) :-
		Request = request(get, origin('/protected'), http(1, 1), [], empty, []),
		http_authenticate::protect_request(Request, http_authenticate_test_verifier, _Action, [body(foo), realm('test-realm')]).

	test(http_authenticate_19, error(domain_error(http_authenticate_unauthorized_response_overlay_option, status(status(403, 'Forbidden'))))) :-
		Response0 = response(http(1, 1), status(401, 'Unauthorized'), [], empty, []),
		Challenge = basic_challenge([
			realm('test-realm'),
			charset(none)
		]),
		http_authenticate::unauthorized_response(Challenge, Response0, _Response, [status(status(403, 'Forbidden'))]).

	test(http_authenticate_20, deterministic) :-
		^^file_path('test_files/apr1_supported.htpasswd', Path),
		http_htpasswd_verifier(Path)::verify('ignored-realm', 'Mufasa', 'Circle Of Life').

	test(http_authenticate_21, fail) :-
		^^file_path('test_files/apr1_supported.htpasswd', Path),
		http_htpasswd_verifier(Path)::verify('ignored-realm', 'Mufasa', 'wrong password').

	% auxiliary predicates

	request_with_authorization(Path, Request) :-
		Authorization = basic_authorization([
			username('Mufasa'),
			password('Circle Of Life')
		]),
		http_authenticate::generate_authorization(Authorization, HeaderValue),
		Request = request(get, origin(Path), http(1, 1), [authorization-HeaderValue], empty, []).

	request_with_invalid_authorization(Path, Request) :-
		Authorization = basic_authorization([
			username('Mufasa'),
			password('wrong password')
		]),
		http_authenticate::generate_authorization(Authorization, HeaderValue),
		Request = request(get, origin(Path), http(1, 1), [authorization-HeaderValue], empty, []).

	request_with_malformed_authorization(Path, Request) :-
		Request = request(get, origin(Path), http(1, 1), [authorization-'Basic YWJj'], empty, []).

:- end_object.
