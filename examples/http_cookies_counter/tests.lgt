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
		date is 2026-06-12,
		comment is 'Unit tests for the "http_cookies_counter" example.'
	]).

	:- uses(http_core, [
		body/2, property/2, request/7, status/2
	]).

	cover(cookie_counter_http_handler).
	cover(cookie_counter_server).
	cover(cookie_counter_client).
	cover(http_cookies_counter_demo).

	test(http_cookies_counter_handler_01, deterministic) :-
		request(get, origin('/visits'), http(1, 1), [host-host('example.com')], empty, [], Request),
		cookie_counter_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({message-'Visit counter stored in the client cookie.', visits-1}))),
		property(Response, set_cookies([set_cookie(visits, '1', [path-('/'), max_age-600, http_only-true])])).

	test(http_cookies_counter_handler_02, deterministic) :-
		request(get, origin('/visits'), http(1, 1), [host-host('example.com')], empty, [cookies([visits-'1'])], Request),
		cookie_counter_http_handler::handle(Request, Response),
		status(Response, status(200, 'OK')),
		body(Response, content('application/json', json({message-'Visit counter stored in the client cookie.', visits-2}))),
		property(Response, set_cookies([set_cookie(visits, '2', [path-('/'), max_age-600, http_only-true])])).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		test(http_cookies_counter_client_01, deterministic) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(http_socket::serve_listener(Listener, cookie_counter_http_handler, 2, _ClientInfos, [shutdown(close)]), Tag),
			catch(
				cookie_counter_client::run(Port, result(FirstResponse, StoredCookiePairs, SecondResponse)),
				Error,
				( 	cleanup_server_thread(Listener, Tag),
					throw(Error)
				)
			),
			http_socket::request_listener_shutdown(Listener),
			threaded_exit(http_socket::serve_listener(Listener, cookie_counter_http_handler, 2, _ClientInfos, [shutdown(close)]), Tag),
			catch(http_socket::close_listener(Listener), _, true),
			StoredCookiePairs == [visits-'1'],
			status(FirstResponse, status(200, 'OK')),
			body(FirstResponse, content('application/json', json({message-'Visit counter stored in the client cookie.', visits-1}))),
			status(SecondResponse, status(200, 'OK')),
			body(SecondResponse, content('application/json', json({message-'Visit counter stored in the client cookie.', visits-2}))).

		test(http_cookies_counter_demo_01, deterministic) :-
			http_cookies_counter_demo::run(result(FirstResponse, StoredCookiePairs, SecondResponse)),
			StoredCookiePairs == [visits-'1'],
			status(FirstResponse, status(200, 'OK')),
			body(FirstResponse, content('application/json', json({message-'Visit counter stored in the client cookie.', visits-1}))),
			status(SecondResponse, status(200, 'OK')),
			body(SecondResponse, content('application/json', json({message-'Visit counter stored in the client cookie.', visits-2}))).

		cleanup_server_thread(Listener, Tag) :-
			http_socket::request_listener_shutdown(Listener),
			catch(threaded_exit(http_socket::serve_listener(Listener, cookie_counter_http_handler, 2, _ClientInfos, [shutdown(close)]), Tag), _, true),
			catch(http_socket::close_listener(Listener), _, true).

	:- endif.

:- end_object.
