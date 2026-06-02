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


% This handler illustrates the common cookie pattern: the server decides what
% value to store, encodes that value as a Set-Cookie header, and later reads
% the returned Cookie header to recover the state it previously delegated to
% the client. Each new request is therefore self-describing without requiring
% any mutable counter on the server side.

:- object(cookie_counter_http_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-02,
		comment is 'HTTP handler for the cookie counter example.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	% The example exposes a single route so the cookie round-trip remains the
	% only moving part. Repeating the same GET request shows that the state is
	% not preserved on the server side but in the client cookie.
	handle(Request, Response) :-
		http_core::method(Request, get),
		http_core::target(Request, origin('/visits')),
		!,
		reply_with_visit(Request, Response).
	handle(Request, Response) :-
		http_core::version(Request, Version),
		json_response(Version, status(404, 'Not Found'), {code-not_found, message-'Unknown route'}, [], Response).

	% Incoming Cookie headers are already normalized by the HTTP library into
	% a cookies(Pairs) property. The handler uses that normalized view when
	% reading the previously stored count and calls http_cookies directly when
	% generating the outgoing Set-Cookie header text controlled by the server.
	reply_with_visit(Request, Response) :-
		http_core::version(Request, Version),
		next_visit_count(Request, VisitCount),
		visit_set_cookie(VisitCount, SetCookie),
		json_response(
			Version,
			status(200, 'OK'),
			{message-'Visit counter stored in the client cookie.', visits-VisitCount},
			[set_cookies([SetCookie])],
			Response
		).

	next_visit_count(Request, VisitCount) :-
		( 	http_core::property(Request, cookies(Pairs)),
			memberchk(visits-CurrentText, Pairs),
			cookie_integer(CurrentText, CurrentCount) ->
			VisitCount is CurrentCount + 1
		; 	VisitCount = 1
		).

	% Malformed cookie values are ignored so the example can restart from the
	% first visit instead of crashing on bad client input.
	cookie_integer(Text, Integer) :-
		atom(Text),
		atom_codes(Text, Codes),
		catch(number_codes(Integer, Codes), _, fail),
		integer(Integer),
		Integer >= 0.

	visit_set_cookie(VisitCount, SetCookie) :-
		number_codes(VisitCount, Codes),
		atom_codes(VisitText, Codes),
		SetCookie = set_cookie(visits, VisitText, [path-('/'), max_age-600, http_only-true]).

	json_response(Version, Status, Body, Properties, Response) :-
		http_core::response(Version, Status, [], content('application/json', json(Body)), Properties, Response).

:- end_object.


% This server variant is intentionally bounded. The caller decides how many
% accepted connections to serve before the listener closes, which makes the
% object convenient for demos and tests where the number of HTTP exchanges is
% known in advance.

:- object(cookie_counter_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Small local HTTP server used by the cookie counter example.'
	]).

	:- public(serve/2).
	:- mode(serve(?integer, +integer), one_or_error).
	:- info(serve/2, [
		comment is 'Opens a local listener and serves the requested number of client connections before shutting down.',
		argnames is ['Port', 'Count']
	]).

	serve(Port, Count) :-
		http_socket::open_listener('127.0.0.1', Port, Listener, []),
		catch(
			http_socket::serve_listener(Listener, cookie_counter_http_handler, Count, _ClientInfos, [shutdown(close)]),
			Error,
			(	catch(http_socket::close_listener(Listener), _, true),
				throw(Error)
			)
		).

:- end_object.


% The client now uses the explicit http_session/http_cookie_jar layer. That
% keeps cookie handling realistic and reusable while still letting the example
% inspect the Name-Value pairs currently stored by the session after each
% request.

:- object(cookie_counter_client).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'HTTP client used by the cookie counter example, backed by an explicit http_client_session cookie jar.'
	]).

	:- public(visit/4).
	:- mode(visit(+integer, +compound, -compound, -list(compound)), one_or_error).
	:- info(visit/4, [
		comment is 'Calls the counter endpoint using the given session and returns the response plus the cookie pairs currently stored in the session jar for that endpoint.',
		argnames is ['Port', 'Session', 'Response', 'StoredCookiePairs']
	]).

	:- public(run/2).
	:- mode(run(+integer, -compound), one_or_error).
	:- info(run/2, [
		comment is 'Performs two visits using one explicit session so the returned result shows both the first stored cookie state and the second request that reuses it automatically.',
		argnames is ['Port', 'Result']
	]).

	visit(Port, Session, Response, StoredCookiePairs) :-
		visits_url(Port, URL),
		http_client_session::get(Session, URL, Response, []),
		session_cookie_pairs(Session, URL, StoredCookiePairs).

	run(Port, result(FirstResponse, StoredCookiePairs, SecondResponse)) :-
		http_client_session::open(Session),
		catch(
			run_with_session(Port, Session, FirstResponse, StoredCookiePairs, SecondResponse),
			Error,
			( 	catch(http_client_session::close(Session), _, true),
				throw(Error)
			)
		),
		http_client_session::close(Session).

	run_with_session(Port, Session, FirstResponse, StoredCookiePairs, SecondResponse) :-
		visit(Port, Session, FirstResponse, StoredCookiePairs),
		visit(Port, Session, SecondResponse, _UpdatedCookiePairs).

	% The client can inspect the explicit session jar to show what cookie state
	% is currently stored for the example endpoint after each response.
	session_cookie_pairs(Session, URL, CookiePairs) :-
		http_client_session::cookie_jar(Session, Jar),
		( 	Jar == none ->
			CookiePairs = []
		; 	http_cookie_jar::request_cookies(Jar, URL, CookiePairs)
		).

	visits_url(Port, URL) :-
		number_codes(Port, PortCodes),
		atom_codes(PortAtom, PortCodes),
		atom_concat('http://127.0.0.1:', PortAtom, Prefix),
		atom_concat(Prefix, '/visits', URL).

:- end_object.


% The demo keeps the example self-contained when backend threads are
% available: a worker thread serves exactly the two client requests while the
% main thread runs the client workflow. This mirrors the style used by the
% other recent HTTP examples.

:- object(http_cookies_counter_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Self-contained demo object for the cookie counter example.'
	]).

	:- public(run/0).
	:- info(run/0, [
		comment is 'Runs the complete example and prints a short summary when backend threads are available.',
		argnames is []
	]).

	:- public(run/1).
	:- info(run/1, [
		comment is 'Runs the complete example and returns both responses plus the stored cookie pairs when backend threads are available.',
		argnames is ['Result']
	]).

	:- if(current_logtalk_flag(threads, supported)).

		:- threaded.

		run :-
			run(Result),
			print_result(Result).

		run(Result) :-
			http_socket::open_listener('127.0.0.1', Port, Listener, []),
			threaded_once(serve_demo_requests(Listener), Tag),
			catch(
				cookie_counter_client::run(Port, Result),
				Error,
				(	cleanup_demo(Listener, Tag),
					throw(Error)
				)
			),
			once(threaded_exit(serve_demo_requests(Listener), Tag)),
			catch(http_socket::close_listener(Listener), _, true).

		% The client performs two one-shot GET requests, so the demo server only
		% needs to accept two HTTP connections before closing the listener.
		serve_demo_requests(Listener) :-
			http_socket::serve_listener(Listener, cookie_counter_http_handler, 2, _ClientInfos, [shutdown(close)]).

		cleanup_demo(Listener, Tag) :-
			catch(http_socket::close_listener(Listener), _, true),
			catch(once(threaded_exit(serve_demo_requests(Listener), Tag)), _, true).

		print_result(result(FirstResponse, StoredCookiePairs, SecondResponse)) :-
			http_core::body(FirstResponse, content('application/json', json({visits-FirstVisit, message-FirstMessage}))),
			http_core::body(SecondResponse, content('application/json', json({visits-SecondVisit, message-SecondMessage}))),
			write('First visit: '),
			write(FirstVisit),
			write(' ('),
			write(FirstMessage),
			write(')'),
			nl,
			write('Session cookie pairs: '),
			write(StoredCookiePairs),
			nl,
			write('Second visit: '),
			write(SecondVisit),
			write(' ('),
			write(SecondMessage),
			write(')'),
			nl.

	:- else.

		run :-
			write('This demo needs backend thread support. Run cookie_counter_server::serve/2 and cookie_counter_client::run/2 in separate sessions instead.'),
			nl.

		run(_Result) :-
			throw(error(resource_error(threads), http_cookies_counter_demo::run/1)).

	:- endif.

:- end_object.
