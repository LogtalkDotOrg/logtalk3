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
		comment is 'Unit tests for the "http_static_site_digest" example.'
	]).

	:- uses(http_core, [body/2, header/3, request/7, status/2]).

	:- uses(list, [
		memberchk/2
	]).

	cover(static_site_digest_fixture).
	cover(static_site_digest_verifier).
	cover(static_site_digest_http_handler(_)).
	cover(static_site_digest_server).
	cover(static_site_digest_client).
	cover(http_static_site_digest_demo).

	cleanup :-
		static_site_digest_fixture::workspace_root(Root),
		static_site_digest_fixture::cleanup(Root).

	test(http_static_site_digest_handler_01, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		request_for_path('/', Request),
		Handler::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		digest_challenge_fields(Response, Fields),
		static_site_digest_fixture::realm(Realm),
		static_site_digest_fixture::algorithm(Algorithm),
		memberchk(realm(Realm), Fields),
		memberchk(algorithm(Algorithm), Fields),
		memberchk(qops([auth]), Fields).

	test(http_static_site_digest_handler_02, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		invalid_authorized_request('/docs/guide.txt', CurrentTime, Request),
		Handler::handle(Request, Response),
		status(Response, status(401, 'Unauthorized')),
		digest_challenge_fields(Response, Fields),
		static_site_digest_fixture::realm(Realm),
		memberchk(realm(Realm), Fields).

	test(http_static_site_digest_handler_03, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		authorized_request('/', CurrentTime, Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, accept_ranges, 'bytes'),
		body(Response, content('text/html', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'index.html', File),
		http_digest::authentication_info(Response, digest_authentication_info(Fields)),
		memberchk(nextnonce(none), Fields).

	test(http_static_site_digest_handler_04, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		authorized_request('/docs/guide.txt', CurrentTime, Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'docs', DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', File).

	test(http_static_site_digest_handler_05, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		authorized_request('/browse/docs/', CurrentTime, Request),
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

	test(http_static_site_digest_handler_06, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		authorized_request('/browse/docs/guide.txt', CurrentTime, Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'docs', DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', File).

	test(http_static_site_digest_handler_07, deterministic) :-
		static_site_digest_fixture::prepare(DocumentRoot),
		static_site_digest_fixture::fixed_current_time(CurrentTime),
		protected_handler(DocumentRoot, CurrentTime, Handler),
		authorized_request('/assets/listing.css', CurrentTime, Request),
		Handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/css', file(File, 0, _Length))),
		os::path_concat(DocumentRoot, 'assets', AssetsDirectory),
		os::path_concat(AssetsDirectory, 'listing.css', File).

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

		:- if(current_logtalk_flag(threads, supported)).

			:- threaded.

			test(http_static_site_digest_client_01, deterministic) :-
				static_site_digest_fixture::prepare(DocumentRoot),
				http_socket::open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(static_site_digest_server::serve_listener(Listener, DocumentRoot, 7), Tag),
				catch(
					static_site_digest_client::run(Port, result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)),
					Error,
					( 	cleanup_server_thread(DocumentRoot, Listener, Tag, 7),
						throw(Error)
					)
				),
				once(threaded_exit(static_site_digest_server::serve_listener(Listener, DocumentRoot, 7), Tag)),
				catch(http_socket::close_listener(Listener), _, true),
				status(ChallengeResponse, status(401, 'Unauthorized')),
				digest_challenge_fields(ChallengeResponse, ChallengeFields),
				static_site_digest_fixture::realm(Realm),
				memberchk(realm(Realm), ChallengeFields),
				status(HomeResponse, status(200, 'OK')),
				body(HomeResponse, content('text/html', text(HomeHTML))),
				once(sub_atom(HomeHTML, _, _, _, 'Digest-authenticated static site example')),
				http_digest::authentication_info(HomeResponse, digest_authentication_info(HomeFields)),
				memberchk(nextnonce(none), HomeFields),
				status(GuideResponse, status(200, 'OK')),
				body(GuideResponse, content('text/plain', text('Guide for the digest-authenticated static site example.'))),
				status(ListingResponse, status(200, 'OK')),
				body(ListingResponse, content('text/html', text(ListingHTML))),
				once(sub_atom(ListingHTML, _, _, _, '<a href="guide.txt">guide.txt</a>')),
				once(sub_atom(ListingHTML, _, _, _, 'class="http-directory-listing theme-ocean"')),
				once(sub_atom(ListingHTML, _, _, _, 'text/plain')).

			test(http_static_site_digest_client_02, deterministic) :-
				static_site_digest_fixture::prepare(DocumentRoot),
				http_socket::open_listener('127.0.0.1', Port, Listener, []),
				threaded_once(static_site_digest_server::serve_listener(Listener, DocumentRoot, 2), Tag),
				catch(
					static_site_digest_client::fetch_guide(Port, Response),
					Error,
					( 	cleanup_server_thread(DocumentRoot, Listener, Tag, 2),
						throw(Error)
					)
				),
				once(threaded_exit(static_site_digest_server::serve_listener(Listener, DocumentRoot, 2), Tag)),
				catch(http_socket::close_listener(Listener), _, true),
				status(Response, status(200, 'OK')),
				body(Response, content('text/plain', text('Guide for the digest-authenticated static site example.'))).

			test(http_static_site_digest_demo_01, deterministic) :-
				http_static_site_digest_demo::run(result(ChallengeResponse, HomeResponse, GuideResponse, ListingResponse)),
				status(ChallengeResponse, status(401, 'Unauthorized')),
				digest_challenge_fields(ChallengeResponse, ChallengeFields),
				static_site_digest_fixture::realm(Realm),
				memberchk(realm(Realm), ChallengeFields),
				status(HomeResponse, status(200, 'OK')),
				body(HomeResponse, content('text/html', text(HomeHTML))),
				once(sub_atom(HomeHTML, _, _, _, 'Digest-authenticated static site example')),
				status(GuideResponse, status(200, 'OK')),
				body(GuideResponse, content('text/plain', text('Guide for the digest-authenticated static site example.'))),
				status(ListingResponse, status(200, 'OK')),
				body(ListingResponse, content('text/html', text(ListingHTML))),
				once(sub_atom(ListingHTML, _, _, _, '<a href="api/">api/</a>')),
				once(sub_atom(ListingHTML, _, _, _, 'class="directory-listing-table theme-ocean columns-name-type-modified"')).

			cleanup_server_thread(DocumentRoot, Listener, Tag, Count) :-
				catch(http_socket::close_listener(Listener), _, true),
				catch(once(threaded_exit(static_site_digest_server::serve_listener(Listener, DocumentRoot, Count), Tag)), _, true),
				static_site_digest_fixture::workspace_root(Root),
				catch(static_site_digest_fixture::cleanup(Root), _, true).

		:- endif.

	:- endif.

	protected_handler(DocumentRoot, CurrentTime, Handler) :-
		static_site_digest_fixture::protect_options(CurrentTime, ProtectOptions),
		Handler = http_server_digest_handler(static_site_digest_verifier, static_site_digest_http_handler(DocumentRoot), ProtectOptions, []).

	request_for_path(Path, Request) :-
		request(get, origin(Path), http(1, 1), [], empty, [], Request).

	digest_challenge_fields(Response, Fields) :-
		http_digest::challenge(Response, digest_challenge(Fields)).

	challenge_for_time(CurrentTime, Challenge) :-
		static_site_digest_fixture::challenge_options(CurrentTime, ChallengeOptions),
		http_digest::unauthorized_response(Challenge, _Response, ChallengeOptions).

	authorized_request(Path, CurrentTime, AuthorizedRequest) :-
		request_for_path(Path, Request),
		challenge_for_time(CurrentTime, Challenge),
		static_site_digest_fixture::username(Username),
		static_site_digest_fixture::password(Password),
		static_site_digest_fixture::authorize_options(AuthorizeOptions),
		http_digest::authorize_request(Request, Challenge, Username, Password, AuthorizedRequest, AuthorizeOptions).

	invalid_authorized_request(Path, CurrentTime, AuthorizedRequest) :-
		request_for_path(Path, Request),
		challenge_for_time(CurrentTime, Challenge),
		static_site_digest_fixture::username(Username),
		static_site_digest_fixture::authorize_options(AuthorizeOptions),
		http_digest::authorize_request(Request, Challenge, Username, 'wrong password', AuthorizedRequest, AuthorizeOptions).

:- end_object.
