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
		comment is 'Unit tests for the "http_static_site" example.'
	]).

	:- uses(http_core, [body/2, header/3, request/7, status/2]).

	cover(static_site_fixture).
	cover(static_site_http_handler(_)).
	cover(static_site_server).
	cover(static_site_client).
	cover(http_static_site_demo).

	cleanup :-
		static_site_fixture::document_root(Root),
		static_site_fixture::cleanup(Root).

	test(http_static_site_handler_01, deterministic) :-
		static_site_fixture::prepare(Root),
		request(get, origin('/'), http(1, 1), [], empty, [], Request),
		static_site_http_handler(Root)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		header(Response, accept_ranges, 'bytes'),
		body(Response, content('text/html', file(File, 0, _Length))),
		os::path_concat(Root, 'index.html', File).

	test(http_static_site_handler_02, deterministic) :-
		static_site_fixture::prepare(Root),
		request(get, origin('/docs/guide.txt'), http(1, 1), [], empty, [], Request),
		static_site_http_handler(Root)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, _Length))),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', File).

	test(http_static_site_handler_03, deterministic) :-
		static_site_fixture::prepare(Root),
		request(get, origin('/browse/docs/'), http(1, 1), [], empty, [], Request),
		static_site_http_handler(Root)::handle(Request, Response),
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

	test(http_static_site_handler_04, deterministic) :-
		static_site_fixture::prepare(Root),
		request(get, origin('/browse/docs/guide.txt'), http(1, 1), [], empty, [], Request),
		static_site_http_handler(Root)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/plain', file(File, 0, _Length))),
		os::path_concat(Root, 'docs', DocsDirectory),
		os::path_concat(DocsDirectory, 'guide.txt', File).

	test(http_static_site_handler_05, deterministic) :-
		static_site_fixture::prepare(Root),
		request(get, origin('/assets/listing.css'), http(1, 1), [], empty, [], Request),
		static_site_http_handler(Root)::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('text/css', file(File, 0, _Length))),
		os::path_concat(Root, 'assets', AssetsDirectory),
		os::path_concat(AssetsDirectory, 'listing.css', File).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_static_site_client_01, deterministic) :-
			static_site_fixture::prepare(Root),
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(static_site_server::serve_listener(Listener, Root, 3), Tag),
			catch(
				static_site_client::run(Port, result(HomeResponse, GuideResponse, ListingResponse)),
				Error,
				( 	cleanup_server_thread(Root, Listener, Tag, 3),
					throw(Error)
				)
			),
			http_socket_transport::request_listener_shutdown(Listener),
			threaded_exit(static_site_server::serve_listener(Listener, Root, 3), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			status(HomeResponse, status(200, 'OK')),
			body(HomeResponse, content('text/html', text(HomeHTML))),
			once(sub_atom(HomeHTML, _, _, _, 'Static site example')),
			status(GuideResponse, status(200, 'OK')),
			body(GuideResponse, content('text/plain', text('Guide for the static site example.'))),
			status(ListingResponse, status(200, 'OK')),
			body(ListingResponse, content('text/html', text(ListingHTML))),
			once(sub_atom(ListingHTML, _, _, _, '<a href="guide.txt">guide.txt</a>')),
			once(sub_atom(ListingHTML, _, _, _, 'class="http-directory-listing theme-ocean"')),
			once(sub_atom(ListingHTML, _, _, _, 'text/plain')).

		test(http_static_site_client_02, deterministic) :-
			static_site_fixture::prepare(Root),
			http_socket_transport::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(static_site_server::serve_listener(Listener, Root, 1), Tag),
			catch(
				( 	atomic_list_concat(['http://127.0.0.1:', Port, '/browse/docs/guide.txt'], URL),
					http_client::get(URL, Response, [])
				),
				Error,
				( 	cleanup_server_thread(Root, Listener, Tag, 1),
					throw(Error)
				)
			),
			http_socket_transport::request_listener_shutdown(Listener),
			threaded_exit(static_site_server::serve_listener(Listener, Root, 1), Tag),
			catch(http_socket_transport::close_listener(Listener), _, true),
			status(Response, status(200, 'OK')),
			body(Response, content('text/plain', text('Guide for the static site example.'))).

		test(http_static_site_demo_01, deterministic) :-
			http_static_site_demo::run(result(HomeResponse, GuideResponse, ListingResponse)),
			status(HomeResponse, status(200, 'OK')),
			body(HomeResponse, content('text/html', text(HomeHTML))),
			once(sub_atom(HomeHTML, _, _, _, 'Static site example')),
			status(GuideResponse, status(200, 'OK')),
			body(GuideResponse, content('text/plain', text('Guide for the static site example.'))),
			status(ListingResponse, status(200, 'OK')),
			body(ListingResponse, content('text/html', text(ListingHTML))),
			once(sub_atom(ListingHTML, _, _, _, '<a href="api/">api/</a>')),
			once(sub_atom(ListingHTML, _, _, _, 'class="directory-listing-table theme-ocean columns-name-type-modified"')).

		cleanup_server_thread(Root, Listener, Tag, Count) :-
			http_socket_transport::request_listener_shutdown(Listener),
			catch(threaded_exit(static_site_server::serve_listener(Listener, Root, Count), Tag), _, true),
			catch(http_socket_transport::close_listener(Listener), _, true),
			catch(static_site_fixture::cleanup(Root), _, true).

	:- endif.

:- end_object.
