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
		http::version(Request, Version),
		next_visit_count(Request, VisitCount),
		number_codes(VisitCount, VisitCountCodes),
		atom_codes(VisitCountText, VisitCountCodes),
		http::response(
			Version,
			status(200, 'OK'),
			[],
			content('text/plain', text(VisitCountText)),
			[set_cookies([set_cookie(visits, VisitCountText, [path-('/'), http_only-true])])],
			Response
		).

	next_visit_count(Request, VisitCount) :-
		( 	http::property(Request, cookies(Pairs)),
			memberchk(visits-CurrentText, Pairs),
			atom_codes(CurrentText, CurrentTextCodes),
			catch(number_codes(CurrentCount, CurrentTextCodes), _, fail) ->
			VisitCount is CurrentCount + 1
		; 	VisitCount = 1
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
		http::version(Request, Version),
		http::target(Request, Target),
		( 	request_info_target_(Target) ->
			request_info_response_(Request, Version, Response)
		; 	echo_response_(Request, Version, Response)
		).

	request_info_target_(origin('/request-info')).
	request_info_target_(origin('/request-info', _Query)).

	request_info_response_(Request, Version, Response) :-
		http::property(Request, query_pairs(QueryPairs)),
		memberchk(lang-Lang, QueryPairs),
		memberchk(page-Page, QueryPairs),
		memberchk(item-Item, QueryPairs),
		http::version(Request, http(Major, Minor)),
		http::property(Request, cookies(CookiePairs)),
		memberchk(session-Session, CookiePairs),
		Body = {lang-Lang, page-Page, item-Item, session-Session, major-Major, minor-Minor},
		http::response(Version, status(200, 'OK'), [], content('application/json', json(Body)), [], Response).

	echo_response_(Request, Version, Response) :-
		http::body(Request, Body),
		( 	Body == empty ->
			http::method(Request, Method),
			ResponseBody = content('text/plain', text(Method))
		; 	ResponseBody = Body
		),
		http::response(Version, status(200, 'OK'), [], ResponseBody, [], Response).

:- end_object.
