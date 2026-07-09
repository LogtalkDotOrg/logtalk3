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


:- object(tests(_HTTPTransport_),
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Unit tests for the "http_session" library.'
	]).

	:- uses(http_core, [
		body/2, property/2
	]).

	:- uses(_HTTPTransport_, [
		close_listener/1, open_listener/4, serve_listener/5
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(http_cookie_jar).
	cover(http_client_session(_)).
	cover(http_server_core_session).
	cover(http_server_core_session_handler(_, _)).
	cover(http_server_core_session_router(_)).
	cover(http_server_core_session_event_logger).
	cover(http_session_test_handler).
	cover(http_session_request_echo_handler).
	cover(http_server_core_session_counter_handler).

	cleanup :-
		^^clean_file('test_cookie_jar_state.tmp'),
		^^clean_file('test_invalid_cookie_jar_item.tmp'),
		^^clean_file('test_invalid_cookie_jar_state.tmp'),
		^^clean_file('test_http_client_session_cookie_jar_state.tmp').

	test(http_cookie_jar_01, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_02, deterministic(Cookies == [session-'2'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '2', [path-('/'), http_only-true])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_03, deterministic(Cookies == [])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::request_cookies(Jar, 'http://sub.example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_04, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [domain-'example.com', path-('/')])]),
		http_cookie_jar::request_cookies(Jar, 'http://sub.example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_05, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/foo/bar', [set_cookie(session, '1', [http_only-true])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/foo/baz', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_06, deterministic(Cookies == [])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/foo/bar', [set_cookie(session, '1', [http_only-true])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/bar', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_07, deterministic(Cookies == [])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://example.com/login', [set_cookie(session, '1', [path-('/'), secure-true])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_08, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://example.com/login', [set_cookie(session, '1', [path-('/'), secure-true])]),
		http_cookie_jar::request_cookies(Jar, 'https://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_09, deterministic(Cookies == [])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '0', [path-('/'), max_age-0])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_10, deterministic(Cookies == [session-'1'])) :-
		date::format_date_time(date_time(2099, 1, 1, 0, 0, 0), 0, http_date, Expires),
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), expires-Expires])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_11, deterministic(Cookies == [])) :-
		date::format_date_time(date_time(2000, 1, 1, 0, 0, 0), 0, http_date, Expires),
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), expires-Expires])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_12, deterministic(Count == 1)) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::cookie_count(Jar, Count),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_13, deterministic(Cookies == [session-'1'])) :-
		^^file_path('test_cookie_jar_state.tmp', Path),
		http_cookie_jar::open(SourceJar),
		http_cookie_jar::store_set_cookies(SourceJar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::save(SourceJar, Path),
		http_cookie_jar::close(SourceJar),
		http_cookie_jar::open(RestoredJar, [cookies_file(Path)]),
		http_cookie_jar::request_cookies(RestoredJar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(RestoredJar).

	test(http_cookie_jar_14, deterministic(Cookies == [])) :-
		http_cookie_jar::open(Jar, [cookies_file(none)]),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::clear(Jar),
		http_cookie_jar::cookie_count(Jar, 0),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_15, deterministic) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/docs/index', [set_cookie(docs, '2', [path-'/docs'])]),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/docs/index', [set_cookie(root, '1', [path-('/')])]),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/docs/page', RequestCookies),
		http_cookie_jar::cookies(Jar, StoredCookies),
		http_cookie_jar::close(Jar),
		RequestCookies == [docs-'2', root-'1'],
		StoredCookies == [
			cookie(docs, '2', [domain-'example.com', path-'/docs', host_only-true, session-true]),
			cookie(root, '1', [domain-'example.com', path-('/'), host_only-true, session-true])
		].

	test(http_cookie_jar_16, deterministic) :-
		date::format_date_time(date_time(2099, 1, 1, 0, 0, 0), 0, http_date, Expires),
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), expires-Expires])]),
		http_cookie_jar::cookies(Jar, [cookie(session, '1', Attributes)]),
		http_cookie_jar::close(Jar),
		memberchk(expires-date_time(2099, 1, 1, 0, 0, 0), Attributes).

	test(http_cookie_jar_17, deterministic) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://example.com/login', [set_cookie(session, '1', [path-('/'), secure-true, same_site-none])]),
		http_cookie_jar::cookies(Jar, [cookie(session, '1', Attributes)]),
		http_cookie_jar::close(Jar),
		memberchk(same_site-none, Attributes).

	test(http_cookie_jar_18, deterministic((Count == 0, Cookies == []))) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), same_site-none])]),
		http_cookie_jar::cookie_count(Jar, Count),
		http_cookie_jar::cookies(Jar, Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_19, deterministic) :-
		^^file_path('test_cookie_jar_state.tmp', Path),
		http_cookie_jar::open(SourceJar),
		http_cookie_jar::store_set_cookies(SourceJar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), same_site-lax])]),
		http_cookie_jar::save(SourceJar, Path),
		http_cookie_jar::close(SourceJar),
		http_cookie_jar::open(RestoredJar, [cookies_file(Path)]),
		http_cookie_jar::cookies(RestoredJar, [cookie(session, '1', Attributes)]),
		http_cookie_jar::close(RestoredJar),
		memberchk(same_site-lax, Attributes).

	test(http_cookie_jar_20, error(domain_error(http_cookie_jar_set_cookie, bad_cookie)), [cleanup(catch(http_cookie_jar::close(Jar), _, true))]) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [bad_cookie]).

	test(http_cookie_jar_21, error(domain_error(http_cookie_jar_persisted_cookie, bad_cookie)), [cleanup(catch(http_cookie_jar::close(Jar), _, true))]) :-
		^^file_path('test_invalid_cookie_jar_item.tmp', Path),
		^^create_text_file('test_invalid_cookie_jar_item.tmp', 'saved_http_cookie_jar(1, [bad_cookie]).\n'),
		http_cookie_jar::open(Jar),
		http_cookie_jar::load(Jar, Path).

	test(http_cookie_jar_22, error(domain_error(http_cookie_jar_persisted_cookies, _)), [cleanup(catch(http_cookie_jar::close(Jar), _, true))]) :-
		^^file_path('test_invalid_cookie_jar_state.tmp', Path),
		^^create_text_file('test_invalid_cookie_jar_state.tmp', 'foo.\n'),
		http_cookie_jar::open(Jar),
		http_cookie_jar::load(Jar, Path).

	test(http_cookie_jar_23, error(domain_error(http_cookie_jar, foo))) :-
		http_cookie_jar::cookie_count(foo, _Count).

	test(http_cookie_jar_24, error(instantiation_error)) :-
		http_cookie_jar::cookie_count(_Jar, _Count).

	test(http_cookie_jar_25, error(instantiation_error)) :-
		http_cookie_jar::cookie_count(cookie_jar(_JarId), _Count).

	test(http_cookie_jar_26, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://Example.COM', [set_cookie(session, '1', [domain-'.Example.COM'])]),
		http_cookie_jar::request_cookies(Jar, 'http://sub.example.com', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_27, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://user@[::1]:8080/login', [set_cookie(session, '1', [path-('/')])]),
		http_cookie_jar::request_cookies(Jar, 'http://[::1]:8080/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_28, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://[::1]/login', [set_cookie(session, '1', [path-('/')])]),
		http_cookie_jar::request_cookies(Jar, 'http://[::1]/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_29, deterministic((SameSiteCookies == [session-'1'], CrossSiteCookies == []))) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://app.example.com/login', [set_cookie(session, '1', [path-('/'), same_site-strict])]),
		http_cookie_jar::request_cookies(Jar, 'https://app.example.com/dashboard', request_context(get, source_url('https://app.example.com/home'), false), SameSiteCookies),
		http_cookie_jar::request_cookies(Jar, 'https://app.example.com/dashboard', request_context(get, source_url('https://other.example.net/home'), false), CrossSiteCookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_30, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://app.example.com/login', [set_cookie(session, '1', [path-('/')])]),
		http_cookie_jar::request_cookies(Jar, 'https://app.example.com/dashboard', request_context(get, source_url('https://other.example.net/home'), true), Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_31, deterministic(Cookies == [])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://app.example.com/login', [set_cookie(session, '1', [path-('/')])]),
		http_cookie_jar::request_cookies(Jar, 'https://app.example.com/dashboard', request_context(post, source_url('https://other.example.net/home'), true), Cookies),
		http_cookie_jar::close(Jar).

	test(http_cookie_jar_32, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'https://app.example.com/login', [set_cookie(session, '1', [path-('/'), secure-true, same_site-none])]),
		http_cookie_jar::request_cookies(Jar, 'https://app.example.com/dashboard', request_context(post, source_origin('https://other.example.net'), false), Cookies),
		http_cookie_jar::close(Jar).

	test(http_client_session_00, deterministic(Cookies == [session-'1'])) :-
		^^file_path('test_http_client_session_cookie_jar_state.tmp', Path),
		http_cookie_jar::open(SourceJar),
		http_cookie_jar::store_set_cookies(SourceJar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::save(SourceJar, Path),
		http_cookie_jar::close(SourceJar),
		http_client_session(_HTTPTransport_)::open(Session, [cookies_file(Path)]),
		http_client_session(_HTTPTransport_)::cookie_jar(Session, RestoredJar),
		http_cookie_jar::request_cookies(RestoredJar, 'http://example.com/dashboard', Cookies),
		http_client_session(_HTTPTransport_)::close(Session).

	test(http_client_session_04, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_client_session(_HTTPTransport_)::open(Session, [cookie_jar(Jar), headers([accept-'application/json']), query([lang-'en']), version(http(1, 0)), properties([trace(default), keep(default)])]),
		http_client_session(_HTTPTransport_)::cookie_jar(Session, Jar),
		http_client_session(_HTTPTransport_)::close(Session),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_client_session_05, error(domain_error(http_client_session, foo))) :-
		http_client_session(_HTTPTransport_)::cookie_jar(foo, _Jar).

	test(http_client_session_06, error(domain_error(http_client_session_options, [cookie_jar(none), cookies_file('cookies.tmp')]))) :-
		http_client_session(_HTTPTransport_)::open(_Session, [cookie_jar(none), cookies_file('cookies.tmp')]).

	test(http_client_session_07, error(domain_error(http_client_session_option, properties([cookies([session-'1'])])))) :-
		http_client_session(_HTTPTransport_)::open(_Session, [properties([cookies([session-'1'])])]).

	test(http_client_session_08, error(domain_error(http_client_session_request_option, unsupported_option)), [cleanup(catch(http_client_session(_HTTPTransport_)::close(Session), _, true))]) :-
		http_client_session(_HTTPTransport_)::open(Session),
		http_client_session(_HTTPTransport_)::get(Session, 'http://example.com/', _Response, [unsupported_option]).

	test(http_client_session_08a, error(domain_error(http_client_session_request_option, source_url('/relative'))), [cleanup(catch(http_client_session(_HTTPTransport_)::close(Session), _, true))]) :-
		http_client_session(_HTTPTransport_)::open(Session),
		http_client_session(_HTTPTransport_)::get(Session, 'http://example.com/', _Response, [source_url('/relative')]).

	test(http_client_session_08b, error(domain_error(http_client_session_request_option, source_origin('https://other.example.net/path'))), [cleanup(catch(http_client_session(_HTTPTransport_)::close(Session), _, true))]) :-
		http_client_session(_HTTPTransport_)::open(Session),
		http_client_session(_HTTPTransport_)::get(Session, 'http://example.com/', _Response, [source_origin('https://other.example.net/path')]).

	test(http_client_session_11, error(instantiation_error)) :-
		http_client_session(_HTTPTransport_)::cookie_jar(_Session, _Jar).

	test(http_client_session_12, error(instantiation_error)) :-
		http_client_session(_HTTPTransport_)::cookie_jar(http_client_session(_SessionId), _Jar).

	test(http_client_session_13, error(instantiation_error)) :-
		http_client_session(_HTTPTransport_)::open(_Session, [cookie_jar(cookie_jar(_JarId))]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_client_session_01, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/visits'], URL),
			http_client_session(_HTTPTransport_)::open(Session),
			http_client_session(_HTTPTransport_)::get(Session, URL, FirstResponse, []),
			http_client_session(_HTTPTransport_)::get(Session, URL, SecondResponse, []),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			body(FirstResponse, content('text/plain', text('1'))),
			body(SecondResponse, content('text/plain', text('2'))).

		test(http_client_session_02, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/visits'], URL),
			http_client_session(_HTTPTransport_)::open(Session, [cookie_jar(none)]),
			http_client_session(_HTTPTransport_)::get(Session, URL, FirstResponse, []),
			http_client_session(_HTTPTransport_)::get(Session, URL, SecondResponse, []),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			body(FirstResponse, content('text/plain', text('1'))),
			body(SecondResponse, content('text/plain', text('1'))).

		test(http_client_session_03, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_test_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/visits'], URL),
			http_client_session(_HTTPTransport_)::open(Session),
			http_client_session(_HTTPTransport_)::get(Session, URL, Response, [cookies([visits-'7'])]),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_test_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			body(Response, content('text/plain', text('8'))).

		test(http_client_session_09, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_request_echo_handler, 5, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/echo'], URL),
			http_client_session(_HTTPTransport_)::open(Session),
			http_client_session(_HTTPTransport_)::head(Session, URL, HeadResponse, []),
			http_client_session(_HTTPTransport_)::delete(Session, URL, DeleteResponse, []),
			http_client_session(_HTTPTransport_)::post(Session, URL, content('text/plain', text(post)), PostResponse, []),
			http_client_session(_HTTPTransport_)::put(Session, URL, content('text/plain', text(put)), PutResponse, []),
			http_client_session(_HTTPTransport_)::patch(Session, URL, content('text/plain', text(patch)), PatchResponse, []),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_request_echo_handler, 5, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			body(HeadResponse, empty),
			body(DeleteResponse, content('text/plain', text(delete))),
			body(PostResponse, content('text/plain', text(post))),
			body(PutResponse, content('text/plain', text(put))),
			body(PatchResponse, content('text/plain', text(patch))).

		test(http_client_session_10, deterministic(Cookies == [session-'1'])) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/login'], LoginURL),
			atomic_list_concat(['http://127.0.0.1:', Port, '/request-info'], URL),
			http_cookie_jar::open(Jar),
			http_cookie_jar::store_set_cookies(Jar, LoginURL, [set_cookie(session, '1', [path-('/'), http_only-true])]),
			http_client_session(_HTTPTransport_)::open(Session, [cookie_jar(Jar), headers([accept-'application/json']), query([lang-'en', page-'1']), version(http(1, 1)), properties([trace(default), keep(default)])]),
			http_client_session(_HTTPTransport_)::get(Session, URL, Response, [headers([accept-'application/json']), query([page-'2', item-'7']), version(http(1, 0)), properties([trace(request), cookies([session-'property'])]), cookies([session-'explicit'])]),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			http_cookie_jar::request_cookies(Jar, URL, Cookies),
			http_cookie_jar::close(Jar),
			body(Response, content('application/json', json({lang-'en', page-'2', item-'7', session-'explicit', major-1, minor-0}))).

		test(http_client_session_14, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/login'], LoginURL),
			atomic_list_concat(['http://127.0.0.1:', Port, '/cookie-info'], URL),
			http_cookie_jar::open(Jar),
			http_cookie_jar::store_set_cookies(Jar, LoginURL, [set_cookie(session, '1', [path-('/'), http_only-true])]),
			http_client_session(_HTTPTransport_)::open(Session, [cookie_jar(Jar)]),
			http_client_session(_HTTPTransport_)::get(Session, URL, Response, [source_url('https://other.example.net/start'), top_level_navigation(true)]),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			http_cookie_jar::close(Jar),
			body(Response, content('text/plain', text('1'))).

		test(http_client_session_15, deterministic) :-
			open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/login'], LoginURL),
			atomic_list_concat(['http://127.0.0.1:', Port, '/cookie-info'], URL),
			http_cookie_jar::open(Jar),
			http_cookie_jar::store_set_cookies(Jar, LoginURL, [set_cookie(session, '1', [path-('/'), http_only-true])]),
			http_client_session(_HTTPTransport_)::open(Session, [cookie_jar(Jar)]),
			http_client_session(_HTTPTransport_)::post(Session, URL, content('text/plain', text(post)), Response, [source_url('https://other.example.net/start'), top_level_navigation(true)]),
			http_client_session(_HTTPTransport_)::close(Session),
			once(threaded_exit(serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(keep_open)]), Tag)),
			catch(close_listener(Listener), _, true),
			http_cookie_jar::close(Jar),
			body(Response, content('text/plain', text('none'))).

	:- endif.

		test(http_server_core_session_01, deterministic((Data == [visits-1], SessionState == anonymous))) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request0),
			http_server_core_session::begin(Manager, Request0, Request),
			http_server_core_session::current(Request, Session),
			http_server_core_session::set(Session, visits, 1),
			http_server_core_session::data(Session, Data),
			http_core::property(Request, http_server_core_session_state(SessionState)),
			server_session_response(Response0),
			http_server_core_session::finish(Request, Response0, Response),
			server_session_cookie(Response, set_cookie(session, SessionId, Attributes)),
			http_server_core_session::close(Manager),
			atom(SessionId),
			memberchk(http_only-true, Attributes),
			memberchk(same_site-lax, Attributes).

		test(http_server_core_session_02, deterministic((Visits == 1, \+ http_core::property(Response2, set_cookies(_))))) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_server_core_session::current(Request21, Session2),
			http_server_core_session::get(Session2, visits, Visits),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			http_server_core_session::close(Manager).

		test(http_server_core_session_03, deterministic(memberchk(max_age-0, Attributes))) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_server_core_session::current(Request21, Session2),
			http_server_core_session::destroy(Session2),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			server_session_cookie(Response2, set_cookie(session, '', Attributes)),
			http_server_core_session::close(Manager).

		test(http_server_core_session_04, deterministic((NewId \== OldId, Visits == 1))) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, OldId, _Attributes1)),
			server_session_request([session-OldId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_server_core_session::current(Request21, Session2),
			http_server_core_session::renew(Session2, NewId),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			server_session_cookie(Response2, set_cookie(session, NewId, _Attributes2)),
			server_session_request([session-NewId], Request30),
			http_server_core_session::begin(Manager, Request30, Request31),
			http_server_core_session::current(Request31, Session3),
			http_server_core_session::get(Session3, visits, Visits),
			server_session_response(Response30),
			http_server_core_session::finish(Request31, Response30, _Response3),
			http_server_core_session::close(Manager).

		test(http_server_core_session_05, deterministic((SessionState == stale, memberchk(max_age-0, Attributes)))) :-
			http_server_core_session::open(Manager, [idle_timeout(0)]),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_core::property(Request21, http_server_core_session_state(SessionState)),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			server_session_cookie(Response2, set_cookie(session, '', Attributes)),
			http_server_core_session::close(Manager).

		test(http_server_core_session_06, deterministic((Collected == 1, Count == 0))) :-
			http_server_core_session::open(Manager, [idle_timeout(0)]),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, _Response1),
			http_server_core_session::gc(Manager, Collected),
			http_server_core_session::count(Manager, Count),
			http_server_core_session::close(Manager).

		test(http_server_core_session_07, deterministic((Removed == 1, Data == []))) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			http_server_core_session::remove(Session1, visits, Removed),
			http_server_core_session::data(Session1, Data),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, _Response1),
			http_server_core_session::close(Manager).

		test(http_server_core_session_08, deterministic((RemovalFailed == true, Visits == 1, \+ http_core::property(Response2, set_cookies(_))))) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_server_core_session::current(Request21, Session2),
			(	http_server_core_session::remove(Session2, missing, _Value) ->
				RemovalFailed = false
			;	RemovalFailed = true
			),
			http_server_core_session::get(Session2, visits, Visits),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			http_server_core_session::close(Manager).

		test(http_server_core_session_09, deterministic((SessionState == stale, memberchk(max_age-0, Attributes)))) :-
			http_server_core_session::open(Manager, [absolute_timeout(0)]),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_core::property(Request21, http_server_core_session_state(SessionState)),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			server_session_cookie(Response2, set_cookie(session, '', Attributes)),
			http_server_core_session::close(Manager).

		test(http_server_core_session_10, deterministic((SessionState == stale, Count == 0, memberchk(max_age-0, Attributes)))) :-
			http_server_core_session::open(Manager, [idle_timeout(0), gc_interval(0)]),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_core::property(Request21, http_server_core_session_state(SessionState)),
			http_server_core_session::count(Manager, Count),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			server_session_cookie(Response2, set_cookie(session, '', Attributes)),
			http_server_core_session::close(Manager).

		test(http_server_core_session_11, deterministic((Visits == 1, memberchk(path-('/app'), Attributes), memberchk(secure-true, Attributes), \+ http_core::property(Response2, set_cookies(_))))) :-
			http_server_core_session::open(Manager, [cookie_name(sid), cookie_attributes([path-('/app'), secure-true])]),
			server_session_request([], Request10),
			http_server_core_session::begin(Manager, Request10, Request11),
			http_server_core_session::current(Request11, Session1),
			http_server_core_session::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(sid, SessionId, Attributes)),
			server_session_request([sid-SessionId], Request20),
			http_server_core_session::begin(Manager, Request20, Request21),
			http_server_core_session::current(Request21, Session2),
			http_server_core_session::get(Session2, visits, Visits),
			server_session_response(Response20),
			http_server_core_session::finish(Request21, Response20, Response2),
			http_server_core_session::close(Manager).

		test(http_server_core_session_12, deterministic((memberchk(created(SessionId), Events), memberchk(touched(SessionId), Events), memberchk(resumed(SessionId), Events), memberchk(renewed(SessionId, NewId), Events), memberchk(touched(NewId), Events)))) :-
			http_server_core_session_event_logger::reset,
			http_server_core_session_event_logger::open(Manager),
			server_session_request([], Request10),
			http_server_core_session_event_logger::begin(Manager, Request10, Request11),
			http_server_core_session_event_logger::current(Request11, Session1),
			http_server_core_session_event_logger::set(Session1, visits, 1),
			server_session_response(Response10),
			http_server_core_session_event_logger::finish(Request11, Response10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			server_session_request([session-SessionId], Request20),
			http_server_core_session_event_logger::begin(Manager, Request20, Request21),
			http_server_core_session_event_logger::current(Request21, Session2),
			http_server_core_session_event_logger::renew(Session2, NewId),
			server_session_response(Response20),
			http_server_core_session_event_logger::finish(Request21, Response20, _Response2),
			http_server_core_session_event_logger::events(Events),
			http_server_core_session_event_logger::close(Manager),
			http_server_core_session_event_logger::reset.

			test(http_server_core_session_13, deterministic((Data == [], Count == 0, \+ http_core::property(Response, set_cookies(_))))) :-
				http_server_core_session::open(Manager),
				server_session_request([], Request0),
				http_server_core_session::begin(Manager, Request0, Request),
				http_server_core_session::current(Request, Session),
				http_server_core_session::data(Session, Data),
				server_session_response(Response0),
				http_server_core_session::finish(Request, Response0, Response),
				http_server_core_session::count(Manager, Count),
				http_server_core_session::close(Manager).

			test(http_server_core_session_14, deterministic((Data == [], Count == 1))) :-
				http_server_core_session::open(Manager),
				server_session_request([], Request0),
				http_server_core_session::begin(Manager, Request0, Request),
				http_server_core_session::ensure(Request, Session),
				http_server_core_session::data(Session, Data),
				http_server_core_session::count(Manager, Count),
				server_session_response(Response0),
				http_server_core_session::finish(Request, Response0, Response),
				server_session_cookie(Response, set_cookie(session, _SessionId, _Attributes)),
				http_server_core_session::close(Manager).

			test(http_server_core_session_15, deterministic) :-
				http_server_core_session::open(Manager),
				server_session_request([], Request0),
				http_server_core_session::begin(Manager, Request0, Request),
				http_server_core_session::current(Request, Session),
				http_server_core_session::set(Session, visits, 1),
				http_core::response(http(1, 1), status(200, 'OK'), [], empty, [connection([close]), set_cookies([set_cookie(existing, '1', [path-('/'), http_only-true]), set_cookie(other, '2', [path-('/app')])])], Response0),
				http_server_core_session::finish(Request, Response0, Response),
				http_core::property(Response, connection([close])),
				http_core::property(Response, set_cookies(SetCookies)),
				memberchk(set_cookie(session, _SessionId, _Attributes), SetCookies),
				memberchk(set_cookie(existing, '1', [path-('/'), http_only-true]), SetCookies),
				memberchk(set_cookie(other, '2', [path-('/app')]), SetCookies),
				http_server_core_session::close(Manager).

			test(http_server_core_session_16, deterministic((User == alice, Data == [visits-1, user-alice]))) :-
				http_server_core_session::open(Manager),
				server_session_request([], Request0),
				http_server_core_session::begin(Manager, Request0, Request),
				http_server_core_session::current(Request, Session),
				http_server_core_session::set(Session, visits, 1),
				http_server_core_session::set(Session, user, alice),
				http_server_core_session::get(Session, user, User),
				http_server_core_session::data(Session, Data),
				server_session_response(Response0),
				http_server_core_session::finish(Request, Response0, _Response),
				http_server_core_session::close(Manager).

			test(http_server_core_session_17, deterministic((memberchk(max_age-TTL, Attributes), TTL >= 0, TTL =< 5, memberchk(path-('/app'), Attributes), memberchk(secure-true, Attributes), \+ member(expires-_, Attributes), \+ member(max_age-99, Attributes)))) :-
				date::format_date_time(date_time(2099, 1, 1, 0, 0, 0), 0, http_date, Expires),
				http_server_core_session::open(Manager, [cookie_attributes([path-('/app'), secure-true, expires-Expires, max_age-99]), idle_timeout(10), absolute_timeout(5)]),
				server_session_request([], Request0),
				http_server_core_session::begin(Manager, Request0, Request),
				http_server_core_session::current(Request, Session),
				http_server_core_session::set(Session, visits, 1),
				server_session_response(Response0),
				http_server_core_session::finish(Request, Response0, Response),
				server_session_cookie(Response, set_cookie(session, _SessionId, Attributes)),
				http_server_core_session::close(Manager).

			test(http_server_core_session_18, deterministic((memberchk(secure-true, Attributes), memberchk(same_site-none, Attributes)))) :-
				http_server_core_session::open(Manager, [cookie_attributes([path-('/'), http_only-true, secure-true, same_site-none])]),
				server_session_request([], Request0),
				http_server_core_session::begin(Manager, Request0, Request),
				http_server_core_session::current(Request, Session),
				http_server_core_session::set(Session, visits, 1),
				server_session_response(Response0),
				http_server_core_session::finish(Request, Response0, Response),
				server_session_cookie(Response, set_cookie(session, _SessionId, Attributes)),
				http_server_core_session::close(Manager).

			test(http_server_core_session_19, error(domain_error(option, cookie_attributes([path-('/'), http_only-true, same_site-none])))) :-
				http_server_core_session::open(_Manager, [cookie_attributes([path-('/'), http_only-true, same_site-none])]).

		test(http_server_core_session_handler_01, deterministic) :-
			http_server_core_session::open(Manager),
			server_session_request([], Request10),
			http_server_core_session_handler(Manager, http_server_core_session_counter_handler)::handle(Request10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			body(Response1, content('text/plain', text('1'))),
			server_session_request([session-SessionId], Request20),
			http_server_core_session_handler(Manager, http_server_core_session_counter_handler)::handle(Request20, Response2),
			body(Response2, content('text/plain', text('2'))),
			http_server_core_session::close(Manager).

		test(http_router_server_session_01, deterministic) :-
			http_server_core_session::open(Manager),
			server_session_request_path('/visits', [], Request10),
			http_server_core_session_router(Manager)::handle(Request10, Response1),
			server_session_cookie(Response1, set_cookie(session, SessionId, _Attributes1)),
			body(Response1, content('text/plain', text('1'))),
			server_session_request_path('/visits', [session-SessionId], Request20),
			http_server_core_session_router(Manager)::handle(Request20, Response2),
			body(Response2, content('text/plain', text('2'))),
			http_server_core_session::close(Manager).

		server_session_request(Cookies, Request) :-
			server_session_request_path('/session', Cookies, Request).

		server_session_request_path(Path, Cookies, Request) :-
			(	Cookies == [] ->
				Properties = []
			;	Properties = [cookies(Cookies)]
			),
			http_core::request(get, origin(Path), http(1, 1), [], empty, Properties, Request).

		server_session_response(Response) :-
			http_core::response(http(1, 1), status(200, 'OK'), [], empty, [], Response).

		server_session_cookie(Response, SetCookie) :-
			http_core::property(Response, set_cookies(SetCookies)),
			server_session_cookie_(SetCookies, SetCookie).

		server_session_cookie_([SetCookie| _SetCookies], SetCookie) :-
			!.
		server_session_cookie_([_SetCookie| SetCookies], SetCookie) :-
			server_session_cookie_(SetCookies, SetCookie).

:- end_object.
