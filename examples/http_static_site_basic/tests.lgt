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
		date is 2026-06-02,
		comment is 'Unit tests for the "http_static_site_basic" example.'
	]).

	:- uses(http_core, [
		body/2, header/3, request/7, status/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(static_site_basic_fixture).
	cover(static_site_basic_http_handler(_)).
	cover(static_site_basic_server).
	cover(static_site_basic_client).
	cover(http_static_site_basic_demo).

	cleanup :-
		static_site_basic_fixture::workspace_root(Root),
		static_site_basic_fixture::cleanup(Root).

	test(http_static_site_basic_handler_01, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		Handler::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		static_site_basic_fixture::realm(Realm),
		http_authenticate::challenge(Response, basic_challenge([
			realm(Realm),
			charset(none)
		])).

	test(http_static_site_basic_handler_02, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request_with_invalid_authorization('/docs/guide.txt', Request),
		Handler::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		static_site_basic_fixture::realm(Realm),
		http_authenticate::challenge(Response, basic_challenge([
			realm(Realm),
			charset(none)
		])).

	test(http_static_site_basic_handler_03, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request_with_authorization('/', Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, accept_ranges, 'bytes'),
		body(Response, content('text/html', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'index.html', File).

	test(http_static_site_basic_handler_04, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request_with_authorization('/docs/guide.txt', Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'docs', DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', File).

	test(http_static_site_basic_handler_05, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request_with_authorization('/browse/docs/', Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/html', text(HTML))),
		once(sub_atom(HTML, _, _, _, 'class="http-directory-listing theme-ocean"')),
		once(sub_atom(HTML, _, _, _, 'rel="stylesheet" href="/assets/listing.css"')),
		once(sub_atom(HTML, _, _, _, 'class="directory-listing-table theme-ocean columns-name-type-modified"')),
		once(sub_atom(HTML, _, _, _, '<a href="guide.txt">guide.txt</a>')),
		once(sub_atom(HTML, _, _, _, '<a href="api/">api/</a>')),
		once(sub_atom(HTML, _, _, _, 'text/plain')),
		\+ sub_atom(HTML, _, _, _, '?sort=size'),
		once(sub_atom(HTML, _, _, _, 'Parent directory')).

	test(http_static_site_basic_handler_06, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request_with_authorization('/browse/docs/guide.txt', Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'docs', DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', File).

	test(http_static_site_basic_handler_07, deterministic) :-
		static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
		protected_handler(DocumentRoot, PasswordFile, Handler),
		request_with_authorization('/assets/listing.css', Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/css', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'assets', AssetsDirectory),
		os::path_concat(AssetsDirectory, 'listing.css', File).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_static_site_basic_client_01, deterministic) :-
			static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 4), Tag),
			catch(
				static_site_basic_client::run(Port, result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)),
				Error,
				( 	cleanup_server_thread(DocumentRoot, PasswordFile, Listener, Tag, 4),
					throw(Error)
				)
			),
			once(threaded_exit(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 4), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			status(ChallengeResponse, status(401, 'Unauthorized')),
			static_site_basic_fixture::realm(Realm),
			http_authenticate::challenge(ChallengeResponse, basic_challenge([
				realm(Realm),
				charset(none)
			])),
			status(HomeResponse, status(200, 'OK')),
			body(HomeResponse, content('text/html', text(HomeHTML))),
			once(sub_atom(HomeHTML, _, _, _, 'Authenticated static site example')),
			status(GuideResponse, status(200, 'OK')),
			body(GuideResponse, content('text/plain', text('Guide for the authenticated static site example.'))),
			status(ListingResponse, status(200, 'OK')),
			body(ListingResponse, content('text/html', text(ListingHTML))),
			once(sub_atom(ListingHTML, _, _, _, '<a href="guide.txt">guide.txt</a>')),
			once(sub_atom(ListingHTML, _, _, _, 'class="http-directory-listing theme-ocean"')),
			once(sub_atom(ListingHTML, _, _, _, 'text/plain')).

		test(http_static_site_basic_client_02, deterministic) :-
			static_site_basic_fixture::prepare(DocumentRoot, PasswordFile),
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 1), Tag),
			catch(
				( 	authorized_request_options(Options),
					atomic_list_concat(['http://127.0.0.1:', Port, '/browse/docs/guide.txt'], URL),
					http_client::get(URL, Response, Options)
				),
				Error,
				( 	cleanup_server_thread(DocumentRoot, PasswordFile, Listener, Tag, 1),
					throw(Error)
				)
			),
			once(threaded_exit(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, 1), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			status(Response, status(200, 'OK')),
			body(Response, content('text/plain', text('Guide for the authenticated static site example.'))).

		test(http_static_site_basic_demo_01, deterministic) :-
			http_static_site_basic_demo::run(result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)),
			status(ChallengeResponse, status(401, 'Unauthorized')),
			static_site_basic_fixture::realm(Realm),
			http_authenticate::challenge(ChallengeResponse, basic_challenge([
				realm(Realm),
				charset(none)
			])),
			status(HomeResponse, status(200, 'OK')),
			body(HomeResponse, content('text/html', text(HomeHTML))),
			once(sub_atom(HomeHTML, _, _, _, 'Authenticated static site example')),
			status(GuideResponse, status(200, 'OK')),
			body(GuideResponse, content('text/plain', text('Guide for the authenticated static site example.'))),
			status(ListingResponse, status(200, 'OK')),
			body(ListingResponse, content('text/html', text(ListingHTML))),
			once(sub_atom(ListingHTML, _, _, _, '<a href="api/">api/</a>')),
			once(sub_atom(ListingHTML, _, _, _, 'class="directory-listing-table theme-ocean columns-name-type-modified"')).

		cleanup_server_thread(DocumentRoot, PasswordFile, Listener, Tag, Count) :-
			catch(http_socket::close_listener(Listener), _, true),
			catch(once(threaded_exit(static_site_basic_server::serve_listener(Listener, DocumentRoot, PasswordFile, Count), Tag)), _, true),
			static_site_basic_fixture::workspace_root(Root),
			catch(static_site_basic_fixture::cleanup(Root), _, true).

	:- endif.

	protected_handler(DocumentRoot, PasswordFile, Handler) :-
		static_site_basic_fixture::realm(Realm),
		Handler = http_server_basic_handler(http_htpasswd_verifier(PasswordFile), static_site_basic_http_handler(DocumentRoot), [realm(Realm)]).

	request_with_authorization(Path, Request) :-
		authorization_header(HeaderValue),
		Request = request(get, origin(Path), http(1, 1), [authorization-HeaderValue], empty, []).

	request_with_invalid_authorization(Path, Request) :-
		Authorization = basic_authorization([
			username('Mufasa'),
			password('wrong password')
		]),
		http_authenticate::generate_authorization(Authorization, HeaderValue),
		Request = request(get, origin(Path), http(1, 1), [authorization-HeaderValue], empty, []).

	authorization_header(HeaderValue) :-
		static_site_basic_fixture::username(Username),
		static_site_basic_fixture::password(Password),
		Authorization = basic_authorization([
			username(Username),
			password(Password)
		]),
		http_authenticate::generate_authorization(Authorization, HeaderValue).

	authorized_request_options([headers([authorization-HeaderValue])]) :-
		authorization_header(HeaderValue).

:- end_object.
