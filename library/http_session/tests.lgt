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
		date is 2026-05-23,
		comment is 'Unit tests for the http_session library.'
	]).

	:- uses(http, [
		body/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(http_cookie_jar).
	cover(http_session).
	cover(http_session_test_handler).
	cover(http_session_request_echo_handler).

	cleanup :-
		^^clean_file('test_cookie_jar_state.tmp'),
		^^clean_file('test_invalid_cookie_jar_item.tmp'),
		^^clean_file('test_invalid_cookie_jar_state.tmp'),
		^^clean_file('test_http_session_cookie_jar_state.tmp').

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

	test(http_cookie_jar_17, error(domain_error(http_cookie_jar_set_cookie, bad_cookie)), [cleanup(catch(http_cookie_jar::close(Jar), _, true))]) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [bad_cookie]).

	test(http_cookie_jar_18, error(domain_error(http_cookie_jar_persisted_cookie, bad_cookie)), [cleanup(catch(http_cookie_jar::close(Jar), _, true))]) :-
		^^file_path('test_invalid_cookie_jar_item.tmp', Path),
		^^create_text_file('test_invalid_cookie_jar_item.tmp', 'saved_http_cookie_jar(1, [bad_cookie]).\n'),
		http_cookie_jar::open(Jar),
		http_cookie_jar::load(Jar, Path).

	test(http_cookie_jar_19, error(domain_error(http_cookie_jar_persisted_cookies, _)), [cleanup(catch(http_cookie_jar::close(Jar), _, true))]) :-
		^^file_path('test_invalid_cookie_jar_state.tmp', Path),
		^^create_text_file('test_invalid_cookie_jar_state.tmp', 'foo.\n'),
		http_cookie_jar::open(Jar),
		http_cookie_jar::load(Jar, Path).

	test(http_cookie_jar_20, error(domain_error(http_cookie_jar, foo))) :-
		http_cookie_jar::cookie_count(foo, _Count).

	test(http_cookie_jar_21, error(instantiation_error)) :-
		http_cookie_jar::cookie_count(_Jar, _Count).

	test(http_cookie_jar_22, error(instantiation_error)) :-
		http_cookie_jar::cookie_count(cookie_jar(_JarId), _Count).

	test(http_session_00, deterministic(Cookies == [session-'1'])) :-
		^^file_path('test_http_session_cookie_jar_state.tmp', Path),
		http_cookie_jar::open(SourceJar),
		http_cookie_jar::store_set_cookies(SourceJar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_cookie_jar::save(SourceJar, Path),
		http_cookie_jar::close(SourceJar),
		http_session::open(Session, [cookies_file(Path)]),
		http_session::cookie_jar(Session, RestoredJar),
		http_cookie_jar::request_cookies(RestoredJar, 'http://example.com/dashboard', Cookies),
		http_session::close(Session).

	test(http_session_04, deterministic(Cookies == [session-'1'])) :-
		http_cookie_jar::open(Jar),
		http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-('/'), http_only-true])]),
		http_session::open(Session, [cookie_jar(Jar), headers([accept-'application/json']), query([lang-'en']), version(http(1, 0)), properties([trace(default), keep(default)])]),
		http_session::cookie_jar(Session, Jar),
		http_session::close(Session),
		http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies),
		http_cookie_jar::close(Jar).

	test(http_session_05, error(domain_error(http_session, foo))) :-
		http_session::cookie_jar(foo, _Jar).

	test(http_session_06, error(domain_error(http_session_options, [cookie_jar(none), cookies_file('cookies.tmp')]))) :-
		http_session::open(_Session, [cookie_jar(none), cookies_file('cookies.tmp')]).

	test(http_session_07, error(domain_error(http_session_option, properties([cookies([session-'1'])])))) :-
		http_session::open(_Session, [properties([cookies([session-'1'])])]).

	test(http_session_08, error(domain_error(http_session_request_option, unsupported_option)), [cleanup(catch(http_session::close(Session), _, true))]) :-
		http_session::open(Session),
		http_session::get(Session, 'http://example.com/', _Response, [unsupported_option]).

	test(http_session_11, error(instantiation_error)) :-
		http_session::cookie_jar(_Session, _Jar).

	test(http_session_12, error(instantiation_error)) :-
		http_session::cookie_jar(http_session(_SessionId), _Jar).

	test(http_session_13, error(instantiation_error)) :-
		http_session::open(_Session, [cookie_jar(cookie_jar(_JarId))]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_session_01, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket::serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(close)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/visits'], URL),
			http_session::open(Session),
			http_session::get(Session, URL, FirstResponse, []),
			http_session::get(Session, URL, SecondResponse, []),
			http_session::close(Session),
			once(threaded_exit(http_socket::serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(close)]), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			body(FirstResponse, content('text/plain', text('1'))),
			body(SecondResponse, content('text/plain', text('2'))).

		test(http_session_02, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket::serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(close)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/visits'], URL),
			http_session::open(Session, [cookie_jar(none)]),
			http_session::get(Session, URL, FirstResponse, []),
			http_session::get(Session, URL, SecondResponse, []),
			http_session::close(Session),
			once(threaded_exit(http_socket::serve_listener(Listener, http_session_test_handler, 2, _ClientInfos, [shutdown(close)]), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			body(FirstResponse, content('text/plain', text('1'))),
			body(SecondResponse, content('text/plain', text('1'))).

		test(http_session_03, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket::serve_listener(Listener, http_session_test_handler, 1, _ClientInfos, [shutdown(close)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/visits'], URL),
			http_session::open(Session),
			http_session::get(Session, URL, Response, [cookies([visits-'7'])]),
			http_session::close(Session),
			once(threaded_exit(http_socket::serve_listener(Listener, http_session_test_handler, 1, _ClientInfos, [shutdown(close)]), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			body(Response, content('text/plain', text('8'))).

		test(http_session_09, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket::serve_listener(Listener, http_session_request_echo_handler, 5, _ClientInfos, [shutdown(close)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/echo'], URL),
			http_session::open(Session),
			http_session::head(Session, URL, HeadResponse, []),
			http_session::delete(Session, URL, DeleteResponse, []),
			http_session::post(Session, URL, content('text/plain', text(post)), PostResponse, []),
			http_session::put(Session, URL, content('text/plain', text(put)), PutResponse, []),
			http_session::patch(Session, URL, content('text/plain', text(patch)), PatchResponse, []),
			http_session::close(Session),
			once(threaded_exit(http_socket::serve_listener(Listener, http_session_request_echo_handler, 5, _ClientInfos, [shutdown(close)]), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			body(HeadResponse, empty),
			body(DeleteResponse, content('text/plain', text(delete))),
			body(PostResponse, content('text/plain', text(post))),
			body(PutResponse, content('text/plain', text(put))),
			body(PatchResponse, content('text/plain', text(patch))).

		test(http_session_10, deterministic(Cookies == [session-'1'])) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket::serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(close)]), Tag),
			atomic_list_concat(['http://127.0.0.1:', Port, '/login'], LoginURL),
			atomic_list_concat(['http://127.0.0.1:', Port, '/request-info'], URL),
			http_cookie_jar::open(Jar),
			http_cookie_jar::store_set_cookies(Jar, LoginURL, [set_cookie(session, '1', [path-('/'), http_only-true])]),
			http_session::open(Session, [cookie_jar(Jar), headers([accept-'application/json']), query([lang-'en', page-'1']), version(http(1, 1)), properties([trace(default), keep(default)])]),
			http_session::get(Session, URL, Response, [headers([accept-'application/json']), query([page-'2', item-'7']), version(http(1, 0)), properties([trace(request), cookies([session-'property'])]), cookies([session-'explicit'])]),
			http_session::close(Session),
			once(threaded_exit(http_socket::serve_listener(Listener, http_session_request_echo_handler, 1, _ClientInfos, [shutdown(close)]), Tag)),
			catch(http_socket::close_listener(Listener), _, true),
			http_cookie_jar::request_cookies(Jar, URL, Cookies),
			http_cookie_jar::close(Jar),
			body(Response, content('application/json', json({lang-'en', page-'2', item-'7', session-'explicit', major-1, minor-0}))).

	:- endif.

:- end_object.
