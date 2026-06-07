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


:- object(http_authenticate_test_verifier,
	implements(http_authenticate_verifier_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Local Basic verifier used by the http_authenticate library tests.'
	]).

	verify('test-realm', 'Mufasa', 'Circle Of Life').

:- end_object.


:- object(http_authenticate_test_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Local HTTP handler used by the http_authenticate library tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::property(Request, basic_username(Username)),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Username)), [], Response).

:- end_object.


:- object(http_authenticate_test_router,
	implements(http_handler_protocol),
	imports([
		http_router,
		http_router_basic_auth(http_authenticate_test_verifier, [realm('test-realm')])
	])).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Router object used by the http_authenticate library tests to exercise Basic-auth route protection.'
	]).

	:- protected(show_secret/2).
	:- info(show_secret/2, [
		comment is 'Protected route handler used by the auth test router object for the ``/secret`` path.',
		argnames is ['Request', 'Response']
	]).

	:- protected(add_router_stage/3).
	:- info(add_router_stage/3, [
		comment is 'Response middleware handler that marks routed responses produced by the auth test router object.',
		argnames is ['Request', 'Response0', 'Response']
	]).

	:- protected(authorize_routed_request/2).
	:- info(authorize_routed_request/2, [
		comment is 'Route-authorization hook that delegates Basic-auth checks to the imported router helper category.',
		argnames is ['Request', 'Action']
	]).

	route(show_secret, get, '/secret', show_secret).

	route_metadata(show_secret, [basic_auth([])]).

	response_middleware(add_router_stage, add_router_stage).

	authorize_routed_request(Request, Action) :-
		^^authorize_basic_auth_request(Request, Action).

	add_router_stage(_Request, response(Version, Status, Headers0, Body, Properties), Response) :-
		http_core::response(Version, Status, [x_router_stage-routed| Headers0], Body, Properties, Response).

	show_secret(Request, Response) :-
		http_core::property(Request, route(show_secret)),
		http_core::property(Request, basic_username(Username)),
		http_core::version(Request, Version),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Username)), [], Response).

:- end_object.
