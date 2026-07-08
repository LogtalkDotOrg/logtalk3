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


:- object(tests(_HTTPSocket_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Unit tests for the "http_digest" library.'
	]).

	:- uses(http_core, [
		body/2, headers/2, property/2, request/7, response/6, status/2
	]).

	:- uses(_HTTPSocket_, [
		close_listener/1, open_listener/4, serve_listener/5
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(http_digest).
	cover(http_server_core_digest_handler(_, _, _, _)).
	cover(http_router_digest_auth(_, _, _)).
	cover(http_digest_test_verifier).
	cover(http_digest_test_handler).
	cover(http_digest_test_router).
	cover(http_client_digest_session).

	test(http_digest_01, deterministic(Challenge == ParsedChallenge)) :-
		challenge_options(sha256, 1700000000, Options),
		http_digest::unauthorized_response(Challenge, Response, Options),
		status(Response, status(401, 'Unauthorized')),
		http_digest::challenge(Response, ParsedChallenge).

	test(http_digest_02, deterministic) :-
		authorized_request(sha256, '/protected', 1700000000, AuthorizedRequest),
		http_digest::authorization(AuthorizedRequest, Authorization),
		protect_options(sha256, 1700000000, Options),
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, continue(VerifiedRequest), Options),
		property(VerifiedRequest, digest_authorization(Authorization)),
		property(VerifiedRequest, digest_username('Mufasa')),
		property(VerifiedRequest, digest_realm('test-realm')),
		property(VerifiedRequest, digest_algorithm(sha256)),
		property(VerifiedRequest, digest_qop(auth)),
		property(VerifiedRequest, digest_nonce_count(1)),
		property(VerifiedRequest, digest_ha1(_HA1)).

	test(http_digest_03, deterministic) :-
		authorized_request(sha512_256, '/protected', 1700000000, AuthorizedRequest),
		protect_options(sha512_256, 1700000000, Options),
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, continue(VerifiedRequest), Options),
		property(VerifiedRequest, digest_algorithm(sha512_256)).

	test(http_digest_04, deterministic) :-
		authorized_request(sha256, '/protected', 1700000000, AuthorizedRequest),
		protect_options(sha256, 1700000302, BaseOptions),
		StaleOptions = [nonce_ttl(300)| BaseOptions],
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, respond(Response), StaleOptions),
		status(Response, status(401, 'Unauthorized')),
		http_digest::challenge(Response, digest_challenge(Fields)),
		memberchk(stale(true), Fields).

	test(http_digest_05, deterministic(Rspauth \== none)) :-
		verified_request(sha256, '/protected', 1700000000, VerifiedRequest),
		response(http(1, 1), status(200, 'OK'), [], empty, [], Response0),
		http_digest::add_authentication_info(VerifiedRequest, Response0, Response, [nextnonce('next-nonce')]),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce('next-nonce'), Fields),
		memberchk(qop(auth), Fields),
		memberchk(nonce_count(1), Fields),
		memberchk(rspauth(Rspauth), Fields).

	test(http_digest_06, deterministic) :-
		verified_request(sha256, '/protected', 1700000000, VerifiedRequest),
		response(http(1, 1), status(200, 'OK'), [], empty, [], Response0),
		http_digest::add_authentication_info(VerifiedRequest, Response0, Response, []),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce(none), Fields),
		memberchk(qop(auth), Fields),
		memberchk(nonce_count(1), Fields).

	test(http_digest_07, deterministic) :-
		verified_request(sha256, '/protected', 1700000000, VerifiedRequest),
		response(http(1, 1), status(200, 'OK'), [], empty, [], Response0),
		http_digest::add_authentication_info(VerifiedRequest, Response0, Response, [nextnonce(true), nonce_secret('secret'), current_time(1700000000)]),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce(NextNonce), Fields),
		atom(NextNonce),
		once(sub_atom(NextNonce, _, 1, _, ':')),
		memberchk(qop(auth), Fields).

	test(http_digest_08, deterministic) :-
		authorized_request(md5, '/protected', 1700000000, AuthorizedRequest),
		protect_options_without_accepted_algorithms(md5, 1700000000, Options),
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, continue(VerifiedRequest), Options),
		property(VerifiedRequest, digest_algorithm(md5)).

	test(http_digest_09, deterministic(Challenge == ParsedChallenge)) :-
		Challenge = digest_challenge([
			realm('test-realm'),
			domains(['/protected', '/admin']),
			nonce('server-nonce'),
			opaque('opaque-token'),
			stale(false),
			algorithm(sha256),
			qops([auth]),
			userhash(false),
			charset(utf_8)
		]),
		response(http(1, 0), status(200, 'OK'), [x_test-'base', server-'demo'], content('text/plain', text(base)), [etag(base), cache_control([private])], Response0),
		OverlayOptions = [
			status(status(401, 'Unauthorized')),
			headers([x_test-'overlay']),
			body(content('text/plain', text(overlay))),
			properties([etag(overlay), connection([close])])
		],
		http_digest::unauthorized_response(Challenge, Response0, Response, OverlayOptions),
		status(Response, status(401, 'Unauthorized')),
		body(Response, content('text/plain', text(overlay))),
		headers(Response, ResponseHeaders),
		memberchk(x_test-'overlay', ResponseHeaders),
		memberchk(server-'demo', ResponseHeaders),
		\+ memberchk(x_test-'base', ResponseHeaders),
		property(Response, etag(overlay)),
		property(Response, connection([close])),
		property(Response, cache_control([private])),
		\+ property(Response, etag(base)),
		http_digest::challenge(Response, ParsedChallenge).

	test(http_digest_10, error(domain_error(option, unsupported_option(foo)))) :-
		challenge_for_algorithm(sha256, 1700000000, Challenge),
		response(http(1, 1), status(401, 'Unauthorized'), [], empty, [], Response0),
		http_digest::unauthorized_response(Challenge, Response0, _Response, [unsupported_option(foo)]).

	test(http_digest_11, deterministic) :-
		request_for_path('/protected', Request),
		challenge_options_without_qop(md5, 1700000000, ChallengeOptions),
		http_digest::unauthorized_response(Challenge, _Response0, ChallengeOptions),
		atom_codes('Mufasa', UsernameCodes),
		atom_codes('Circle Of Life', PasswordCodes),
		http_digest::authorize_request(Request, Challenge, UsernameCodes, PasswordCodes, AuthorizedRequest, []),
		http_digest::authorization(AuthorizedRequest, digest_authorization(AuthorizationFields)),
		memberchk(qop(none), AuthorizationFields),
		memberchk(nonce_count(none), AuthorizationFields),
		memberchk(cnonce(none), AuthorizationFields),
		protect_options_without_qop(md5, 1700000000, ProtectOptions),
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, continue(VerifiedRequest), ProtectOptions),
		once(property(VerifiedRequest, digest_qop(none))),
		\+ property(VerifiedRequest, digest_nonce_count(_)),
		response(http(1, 1), status(200, 'OK'), [], empty, [], Response1),
		http_digest::add_authentication_info(VerifiedRequest, Response1, Response2, []),
		http_digest::authentication_info(Response2, digest_authentication_info(AuthenticationInfoFields)),
		memberchk(qop(none), AuthenticationInfoFields),
		memberchk(nonce_count(none), AuthenticationInfoFields),
		memberchk(cnonce(none), AuthenticationInfoFields).

	test(http_digest_12, deterministic) :-
		challenge_for_algorithm(md5, 1700000000, digest_challenge(ChallengeFields)),
		memberchk(nonce(Nonce), ChallengeFields),
		Authorization = digest_authorization([
			username('Mufasa'),
			userhash(false),
			realm('test-realm'),
			nonce(Nonce),
			uri('/protected'),
			response(abcd),
			algorithm(md5),
			opaque(none),
			qop(auth_int),
			nonce_count(1),
			cnonce('client-nonce')
		]),
		http_digest::generate_authorization(Authorization, HeaderValue),
		request(get, origin('/protected'), http(1, 1), [authorization-HeaderValue], empty, [], Request),
		protect_options(md5, 1700000000, Options),
		http_digest::protect_request(Request, http_digest_test_verifier, respond(Response), Options),
		status(Response, status(401, 'Unauthorized')).

	test(http_digest_auth_int_01, deterministic) :-
		verified_auth_int_request(md5, post, '/protected', content('text/plain', text('hello')), 1700000000, VerifiedRequest),
		once(property(VerifiedRequest, digest_qop(auth_int))),
		once(property(VerifiedRequest, digest_username('Mufasa'))).

	test(http_digest_auth_int_02, deterministic) :-
		authorized_auth_int_request(md5, post, '/protected', content('text/plain', text('hello')), 1700000000, AuthorizedRequest),
		AuthorizedRequest = request(Method, Target, Version, Headers, _Body, Properties),
		request(Method, Target, Version, Headers, content('text/plain', text('changed')), Properties, TamperedRequest),
		protect_options_auth_int(md5, 1700000000, Options),
		http_digest::protect_request(TamperedRequest, http_digest_test_verifier, respond(Response), Options),
		status(Response, status(401, 'Unauthorized')).

	test(http_digest_auth_int_03, deterministic) :-
		verified_auth_int_request(sha256, post, '/protected', content('text/plain', text('hello')), 1700000000, VerifiedRequest),
		response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(one)), [], ResponseA0),
		response(http(1, 1), status(200, 'OK'), [], content('text/plain', text(two)), [], ResponseB0),
		http_digest::add_authentication_info(VerifiedRequest, ResponseA0, ResponseA, [nextnonce('next-nonce')]),
		http_digest::add_authentication_info(VerifiedRequest, ResponseB0, ResponseB, [nextnonce('next-nonce')]),
		http_digest::authentication_info(ResponseA, digest_authentication_info(FieldsA)),
		http_digest::authentication_info(ResponseB, digest_authentication_info(FieldsB)),
		memberchk(qop(auth_int), FieldsA),
		memberchk(rspauth(RspauthA), FieldsA),
		memberchk(rspauth(RspauthB), FieldsB),
		RspauthA \== none,
		RspauthA \== RspauthB.

	test(http_digest_13, error(domain_error(http_digest_header(authentication_info), invalid(nc)))) :-
		response(http(1, 1), status(200, 'OK'), [authentication_info-'qop=auth, rspauth="abcd", cnonce="client-nonce", nc=zzzzzzzz'], empty, [], Response),
		http_digest::authentication_info(Response, _AuthenticationInfo).

	test(http_digest_14, error(domain_error(http_digest_header(www_authenticate), invalid(syntax)))) :-
		response(http(1, 1), status(401, 'Unauthorized'), [www_authenticate-'Digest realm="test-realm", nonce="test-nonce",'], empty, [], Response),
		http_digest::challenge(Response, _Challenge).

	test(http_digest_test_verifier_01, deterministic) :-
		http_digest_test_verifier::ha1(md5, 'test-realm', 'Mufasa', MD5HA1),
		http_digest_test_verifier::ha1(sha256, 'test-realm', 'Mufasa', SHA256HA1),
		http_digest_test_verifier::ha1(sha512_256, 'test-realm', 'Mufasa', SHA512256HA1),
		atom(MD5HA1),
		atom(SHA256HA1),
		atom(SHA512256HA1),
		MD5HA1 \== SHA256HA1,
		SHA256HA1 \== SHA512256HA1.

	test(http_digest_test_handler_01, deterministic) :-
		request(get, origin('/protected'), http(1, 1), [], empty, [digest_username('Mufasa')], Request),
		http_digest_test_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('Mufasa'))).

	test(http_server_core_digest_handler_01, deterministic) :-
		request_for_path('/protected', Request),
		protect_options(sha256, 1700000000, Options),
		http_server_core_digest_handler(http_digest_test_verifier, http_digest_test_handler, Options, [])::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		http_digest::challenge(Response, _Challenge).

	test(http_server_core_digest_handler_02, deterministic) :-
		authorized_request(sha256, '/protected', 1700000000, Request),
		protect_options(sha256, 1700000000, Options),
		http_server_core_digest_handler(http_digest_test_verifier, http_digest_test_handler, Options, [nextnonce('next-nonce')])::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('Mufasa'))),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce('next-nonce'), Fields).

	test(http_server_core_digest_handler_03, deterministic) :-
		authorized_request(md5, '/protected', 1700000000, Request),
		protect_options_without_accepted_algorithms(md5, 1700000000, Options),
		http_server_core_digest_handler(http_digest_test_verifier, http_digest_test_handler, Options, [])::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', text('Mufasa'))),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce(none), Fields).

	test(http_router_digest_auth_01, deterministic) :-
		request_for_path('/secret', Request),
		http_digest_test_router::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		headers(Response, Headers),
		memberchk(x_router_stage-routed, Headers),
		\+ http_digest::authentication_info(Response, _AuthenticationInfo),
		http_digest::challenge(Response, _Challenge).

	test(http_router_digest_auth_02, deterministic) :-
		authorized_request(sha256, '/secret', 1700000000, Request),
		http_digest_test_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		headers(Response, Headers),
		memberchk(x_router_stage-routed, Headers),
		body(Response, content('text/plain', text('Mufasa'))),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce('next-nonce'), Fields).

	test(http_router_digest_auth_03, deterministic) :-
		request_for_path('/public', Request),
		http_digest_test_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		headers(Response, Headers),
		memberchk(x_router_stage-routed, Headers),
		body(Response, content('text/plain', text(public))),
		\+ http_digest::authentication_info(Response, _AuthenticationInfo).

	test(http_router_digest_auth_04, deterministic) :-
		authorized_request(sha256, '/secret', 1699999698, Request),
		http_digest_test_router::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		headers(Response, Headers),
		memberchk(x_router_stage-routed, Headers),
		\+ http_digest::authentication_info(Response, _AuthenticationInfo),
		http_digest::challenge(Response, digest_challenge(Fields)),
		memberchk(stale(true), Fields).

	test(http_router_digest_auth_05, deterministic) :-
		authorized_request(sha512_256, '/secret-sha512', 1700000000, Request),
		http_digest_test_router::handle(Request, Response),
		status(Response, status(200, 'OK')),
		headers(Response, Headers),
		memberchk(x_router_stage-routed, Headers),
		body(Response, content('text/plain', text('Mufasa'))),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce('next-nonce'), Fields).

	:- if((
		current_logtalk_flag(prolog_dialect, Dialect),
		(	Dialect == eclipse; Dialect == gnu;
			Dialect == sicstus; Dialect == swi;
			Dialect == trealla,
			current_prolog_flag(version_data, trealla(Major, Minor, Patch, _)),
			v(Major, Minor, Patch) @>= v(2, 90, 3);
			Dialect == xvm
		)
	)).

		test(http_client_digest_session_01, deterministic) :-
			http_cookie_jar::open(Jar),
			http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(Jar)]),
			http_client_digest_session(_HTTPSocket_)::cookie_jar(Session, Jar),
			http_client_digest_session(_HTTPSocket_)::close(Session),
			http_cookie_jar::cookie_count(Jar, 0),
			http_cookie_jar::close(Jar).

		test(http_client_digest_session_02, error(domain_error(http_client_digest_session_request_option, unsupported_option)), [cleanup(catch(http_client_digest_session(_HTTPSocket_)::close(Session), _, true))]) :-
			http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life'),
			http_client_digest_session(_HTTPSocket_)::get(Session, 'http://example.com/', _Response, [unsupported_option]).

		test(http_client_digest_session_03, error(domain_error(http_client_digest_session_request_option, digest_options([stale(true)]))), [cleanup(catch(http_client_digest_session(_HTTPSocket_)::close(Session), _, true))]) :-
			http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life'),
			http_client_digest_session(_HTTPSocket_)::get(Session, 'http://example.com/', _Response, [digest_options([stale(true)])]).

		test(http_client_digest_session_04, error(domain_error(http_client_digest_session_request_option, digest_options([cnonce('')]))), [cleanup(catch(http_client_digest_session(_HTTPSocket_)::close(Session), _, true))]) :-
			http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life'),
			http_client_digest_session(_HTTPSocket_)::get(Session, 'http://example.com/', _Response, [digest_options([cnonce('')])]).

		test(http_client_digest_session_05, error(domain_error(http_client_digest_session, foo))) :-
			http_client_digest_session(_HTTPSocket_)::cookie_jar(foo, _Jar).

		test(http_client_digest_session_06, deterministic) :-
			http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(none)]),
			http_client_digest_session(_HTTPSocket_)::cookie_jar(Session, none),
			http_client_digest_session(_HTTPSocket_)::close(Session).

		test(http_client_digest_session_https_01, deterministic(Probe == probe('example.com', 443, [connection_transport(tls)])), [cleanup(catch(http_client_digest_session(http_digest_probe_socket)::close(Session), _, true))]) :-
			http_client_digest_session(http_digest_probe_socket)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(none)]),
			http_client_digest_session(http_digest_probe_socket)::get(Session, 'https://example.com/profile', Response, []),
			http_client_digest_session(http_digest_probe_socket)::close(Session),
			once(property(Response, Probe)).

		:- if(current_logtalk_flag(threads, supported)).
			:- threaded.

			test(http_client_digest_session_07, deterministic) :-
				protect_options(sha256, 1700000000, DigestOptions),
				open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(serve_listener(Listener, http_server_core_digest_handler(http_digest_test_verifier, http_digest_request_echo_handler, DigestOptions, []), 10, _ClientInfos, [shutdown(keep_open)]), Tag),
				request_echo_url(Port, '/echo', URL),
				http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life'),
				http_client_digest_session(_HTTPSocket_)::head(Session, URL, HeadResponse, []),
				http_client_digest_session(_HTTPSocket_)::delete(Session, URL, DeleteResponse, []),
				http_client_digest_session(_HTTPSocket_)::post(Session, URL, content('text/plain', text(post)), PostResponse, []),
				http_client_digest_session(_HTTPSocket_)::put(Session, URL, content('text/plain', text(put)), PutResponse, []),
				http_client_digest_session(_HTTPSocket_)::patch(Session, URL, content('text/plain', text(patch)), PatchResponse, []),
				http_client_digest_session(_HTTPSocket_)::close(Session),
				once(threaded_exit(serve_listener(Listener, http_server_core_digest_handler(http_digest_test_verifier, http_digest_request_echo_handler, DigestOptions, []), 10, _ClientInfos, [shutdown(keep_open)]), Tag)),
				catch(close_listener(Listener), _, true),
				body(HeadResponse, empty),
				body(DeleteResponse, content('text/plain', text(delete))),
				body(PostResponse, content('text/plain', text(post))),
				body(PutResponse, content('text/plain', text(put))),
				body(PatchResponse, content('text/plain', text(patch))).

			test(http_client_digest_session_08, deterministic(Cookies == [session-'jar'])) :-
				protect_options(sha256, 1700000000, DigestOptions),
				open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(serve_listener(Listener, http_server_core_digest_handler(http_digest_test_verifier, http_digest_request_echo_handler, DigestOptions, []), 2, _ClientInfos, [shutdown(keep_open)]), Tag),
				request_echo_url(Port, '/login', LoginURL),
				request_echo_url(Port, '/request-info?lang=en', URL),
				http_cookie_jar::open(Jar),
				http_cookie_jar::store_set_cookies(Jar, LoginURL, [set_cookie(session, 'jar', [path-('/'), http_only-true])]),
				http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(Jar), headers([accept-'text/plain', x_default-'1']), query([page-'1']), version(http(1, 1)), properties([trace(default), keep(default)]), digest_options([cnonce('default-cnonce'), nonce_count(2)])]),
				http_client_digest_session(_HTTPSocket_)::get(Session, URL, Response, [headers([accept-'application/json']), query([page-'2', item-'7']), version(http(1, 0)), properties([trace(request), cookies([session-'property'])]), cookies([session-'explicit']), digest_options([cnonce('request-cnonce')])]),
				http_client_digest_session(_HTTPSocket_)::close(Session),
				once(threaded_exit(serve_listener(Listener, http_server_core_digest_handler(http_digest_test_verifier, http_digest_request_echo_handler, DigestOptions, []), 2, _ClientInfos, [shutdown(keep_open)]), Tag)),
				catch(close_listener(Listener), _, true),
				http_cookie_jar::request_cookies(Jar, URL, Cookies),
				http_cookie_jar::close(Jar),
				body(Response, content('application/json', json({lang-en, page-'2', item-'7', session-'explicit', major-1, minor-0}))).

			test(http_client_digest_session_09, deterministic) :-
				open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(serve_listener(Listener, http_digest_multipart_summary_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag),
				request_echo_url(Port, '/form-info', URL),
				http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(none)]),
				http_client_digest_session(_HTTPSocket_)::post(Session, URL, form_data([field(title, 'Logtalk', [])]), Response, []),
				http_client_digest_session(_HTTPSocket_)::close(Session),
				once(threaded_exit(serve_listener(Listener, http_digest_multipart_summary_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag)),
				catch(close_listener(Listener), _, true),
				status(Response, status(200, 'OK')),
				body(Response, content('text/plain', text(Summary))),
				atom_concat('title=Logtalk; boundary=logtalk-form-data-', _, Summary).

			test(http_client_digest_session_10, deterministic) :-
				open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(serve_listener(Listener, http_digest_multipart_summary_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag),
				request_echo_url(Port, '/form-info', URL),
				http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(none)]),
				http_client_digest_session(_HTTPSocket_)::post(Session, URL, form_data([field(title, 'Logtalk', [])]), Response, [properties([content_type('Multipart/Form-Data', [boundary-'fixed-boundary'])])]),
				http_client_digest_session(_HTTPSocket_)::close(Session),
				once(threaded_exit(serve_listener(Listener, http_digest_multipart_summary_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag)),
				catch(close_listener(Listener), _, true),
				status(Response, status(200, 'OK')),
				body(Response, content('text/plain', text('title=Logtalk; boundary=fixed-boundary'))).

			test(http_client_digest_session_auth_int_01, deterministic) :-
				protect_options_auth_int(sha256, 1700000000, DigestOptions),
				open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(serve_listener(Listener, http_server_core_digest_handler(http_digest_test_verifier, http_digest_request_echo_handler, DigestOptions, []), 2, _ClientInfos, [shutdown(keep_open)]), Tag),
				request_echo_url(Port, '/echo', URL),
				http_client_digest_session(_HTTPSocket_)::open(Session, 'Mufasa', 'Circle Of Life', [cookie_jar(none)]),
				http_client_digest_session(_HTTPSocket_)::post(Session, URL, content('text/plain', text('post-int')), Response, []),
				http_client_digest_session(_HTTPSocket_)::close(Session),
				once(threaded_exit(serve_listener(Listener, http_server_core_digest_handler(http_digest_test_verifier, http_digest_request_echo_handler, DigestOptions, []), 2, _ClientInfos, [shutdown(keep_open)]), Tag)),
				catch(close_listener(Listener), _, true),
				status(Response, status(200, 'OK')),
				body(Response, content('text/plain', text('post-int'))).

		:- endif.

	:- endif.

	% auxiliary predicates

	challenge_options(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), qops([auth]), nonce_secret('secret'), current_time(CurrentTime)]).

	challenge_options_without_qop(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), qops([]), nonce_secret('secret'), current_time(CurrentTime)]).

	protect_options(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), accepted_algorithms([Algorithm]), qops([auth]), nonce_secret('secret'), current_time(CurrentTime)]).

	challenge_options_auth_int(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), qops([auth_int]), nonce_secret('secret'), current_time(CurrentTime)]).

	protect_options_auth_int(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), accepted_algorithms([Algorithm]), qops([auth_int]), nonce_secret('secret'), current_time(CurrentTime)]).

	protect_options_without_qop(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), accepted_algorithms([Algorithm]), qops([]), nonce_secret('secret'), current_time(CurrentTime)]).

	protect_options_without_accepted_algorithms(Algorithm, CurrentTime, [realm('test-realm'), algorithm(Algorithm), qops([auth]), nonce_secret('secret'), current_time(CurrentTime)]).

	request_echo_url(Port, Path, URL) :-
		atomic_list_concat(['http://127.0.0.1:', Port, Path], URL).

	request_for_path(Path, Request) :-
		request(get, origin(Path), http(1, 1), [], empty, [], Request).

	challenge_for_algorithm(Algorithm, CurrentTime, Challenge) :-
		challenge_options(Algorithm, CurrentTime, Options),
		http_digest::unauthorized_response(Challenge, _Response, Options).

	authorized_request(Algorithm, Path, CurrentTime, AuthorizedRequest) :-
		request_for_path(Path, Request),
		challenge_for_algorithm(Algorithm, CurrentTime, Challenge),
		http_digest::authorize_request(Request, Challenge, 'Mufasa', 'Circle Of Life', AuthorizedRequest, [cnonce('client-nonce'), nonce_count(1)]).

	authorized_auth_int_request(Algorithm, Method, Path, Body, CurrentTime, AuthorizedRequest) :-
		request(Method, origin(Path), http(1, 1), [], Body, [], Request),
		challenge_options_auth_int(Algorithm, CurrentTime, ChallengeOptions),
		http_digest::unauthorized_response(Challenge, _Response, ChallengeOptions),
		http_digest::authorize_request(Request, Challenge, 'Mufasa', 'Circle Of Life', AuthorizedRequest, [cnonce('client-nonce'), nonce_count(1)]).

	verified_request(Algorithm, Path, CurrentTime, VerifiedRequest) :-
		authorized_request(Algorithm, Path, CurrentTime, AuthorizedRequest),
		protect_options(Algorithm, CurrentTime, Options),
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, continue(VerifiedRequest), Options).

	verified_auth_int_request(Algorithm, Method, Path, Body, CurrentTime, VerifiedRequest) :-
		authorized_auth_int_request(Algorithm, Method, Path, Body, CurrentTime, AuthorizedRequest),
		protect_options_auth_int(Algorithm, CurrentTime, Options),
		http_digest::protect_request(AuthorizedRequest, http_digest_test_verifier, continue(VerifiedRequest), Options).

:- end_object.
