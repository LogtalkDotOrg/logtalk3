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


:- object(http_session_test_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Local HTTP handler used by the http_session library tests.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		next_visit_count(Request, VisitCount),
		number_codes(VisitCount, VisitCountCodes),
		atom_codes(VisitCountText, VisitCountCodes),
		http_core::response(
			Version,
			status(200, 'OK'),
			[],
			content('text/plain', text(VisitCountText)),
			[set_cookies([set_cookie(visits, VisitCountText, [path-('/'), http_only-true])])],
			Response
		).

	next_visit_count(Request, VisitCount) :-
		(	http_core::property(Request, cookies(Pairs)),
			memberchk(visits-CurrentText, Pairs),
			atom_codes(CurrentText, CurrentTextCodes),
			catch(number_codes(CurrentCount, CurrentTextCodes), _, fail) ->
			VisitCount is CurrentCount + 1
		;	VisitCount = 1
		).

:- end_object.


:- object(http_session_request_echo_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-23,
		comment is 'Local HTTP handler used to exercise the http_session request option and wrapper predicates.'
	]).

	:- uses(list, [
		memberchk/2
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::target(Request, Target),
		(	request_info_target_(Target) ->
			request_info_response_(Request, Version, Response)
		;	cookie_info_target_(Target) ->
			cookie_info_response_(Request, Version, Response)
		;	echo_response_(Request, Version, Response)
		).

	request_info_target_(origin('/request-info')).
	request_info_target_(origin('/request-info', _Query)).

	cookie_info_target_(origin('/cookie-info')).
	cookie_info_target_(origin('/cookie-info', _Query)).

	request_info_response_(Request, Version, Response) :-
		http_core::property(Request, query_pairs(QueryPairs)),
		memberchk(lang-Lang, QueryPairs),
		memberchk(page-Page, QueryPairs),
		memberchk(item-Item, QueryPairs),
		http_core::version(Request, http(Major, Minor)),
		http_core::property(Request, cookies(CookiePairs)),
		memberchk(session-Session, CookiePairs),
		Body = {lang-Lang, page-Page, item-Item, session-Session, major-Major, minor-Minor},
		http_core::response(Version, status(200, 'OK'), [], content('application/json', json(Body)), [], Response).

	cookie_info_response_(Request, Version, Response) :-
		(	http_core::property(Request, cookies(CookiePairs)),
			memberchk(session-Session, CookiePairs) ->
			Body = content('text/plain', text(Session))
		;	Body = content('text/plain', text('none'))
		),
		http_core::response(Version, status(200, 'OK'), [], Body, [], Response).

	echo_response_(Request, Version, Response) :-
		http_core::body(Request, Body),
		(	Body == empty ->
			http_core::method(Request, Method),
			ResponseBody = content('text/plain', text(Method))
		;	ResponseBody = Body
		),
		http_core::response(Version, status(200, 'OK'), [], ResponseBody, [], Response).

:- end_object.


	:- object(http_server_session_counter_handler,
		implements(http_handler_protocol)).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-05-28,
			comment is 'Local HTTP handler used by the http_session library tests to exercise the plain server-session handler wrapper.'
		]).

		handle(Request, Response) :-
			http_server_session::current(Request, Session),
			(	http_server_session::get(Session, visits, CurrentCount) ->
				VisitCount is CurrentCount + 1
			;	VisitCount = 1
			),
			http_server_session::set(Session, visits, VisitCount),
			http_core::version(Request, Version),
			number_codes(VisitCount, VisitCountCodes),
			atom_codes(VisitCountText, VisitCountCodes),
			http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(VisitCountText)), [], Response).

	:- end_object.


	:- object(http_server_session_event_logger,
		extends(http_server_session)).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-05-28,
			comment is 'Local server-session specialization used by the ``http_session`` library tests to record lifecycle hook events.'
		]).

		:- public(reset/0).
		:- mode(reset, one).
		:- info(reset/0, [
			comment is 'Clears the recorded server-session lifecycle events.',
			argnames is []
		]).

		:- public(events/1).
		:- mode(events(-list(compound)), one).
		:- info(events/1, [
			comment is 'Returns the recorded server-session lifecycle events in call order.',
			argnames is ['Events']
		]).

		:- private(event_/1).
		:- dynamic(event_/1).
		:- mode(event_(?compound), zero_or_more).
		:- info(event_/1, [
			comment is 'Recorded server-session lifecycle events.',
			argnames is ['Event']
		]).

		reset :-
			retractall(event_(_)).

		events(Events) :-
			findall(Event, event_(Event), Events).

		http_server_session_event(_Manager, Event) :-
			assertz(event_(Event)).

	:- end_object.


	:- object(http_server_session_router(_Manager_),
		implements(http_handler_protocol),
		imports([http_router, http_router_server_session(_Manager_)])).

		:- info([
			version is 1:0:0,
			author is 'Paulo Moura',
			date is 2026-05-28,
			comment is 'Local router object used by the ``http_session`` library tests to exercise the router-side server-session middleware helpers.'
		]).

		:- protected(visits/2).
		:- info(visits/2, [
			comment is 'Route handler used by the server-session router test object for the ``/visits`` path.',
			argnames is ['Request', 'Response']
		]).

		middleware(server_session, annotate_server_session_request).
		response_middleware(server_session, add_server_session_response).

		route(visits, get, '/visits', visits).

		visits(Request, Response) :-
			http_core::property(Request, route(visits)),
			http_server_session::current(Request, Session),
			(	http_server_session::get(Session, visits, CurrentCount) ->
				VisitCount is CurrentCount + 1
			;	VisitCount = 1
			),
			http_server_session::set(Session, visits, VisitCount),
			http_core::version(Request, Version),
			number_codes(VisitCount, VisitCountCodes),
			atom_codes(VisitCountText, VisitCountCodes),
			http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(VisitCountText)), [], Response).

	:- end_object.
