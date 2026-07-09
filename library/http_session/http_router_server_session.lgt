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


:- category(http_router_server_session(_Manager_)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Optional server-session middleware helpers for router objects importing the ``http_router`` category.',
		parameters is [
			'Manager' - 'Server-session manager handle used for request begin and response finish processing.'
		]
	]).

	:- protected(annotate_server_session_request/2).
	:- mode(annotate_server_session_request(+compound, -compound), one_or_error).
	:- info(annotate_server_session_request/2, [
		comment is 'Router middleware helper that annotates a normalized request with a server-session handle and state before route dispatch.',
		argnames is ['Request', 'Action'],
		exceptions is [
			'The configured manager is not an open server-session manager handle' - domain_error(http_server_core_session, 'Manager'),
			'``Request`` is not a valid normalized HTTP request' - domain_error(http_server_core_session_request, 'Request')
		]
	]).

	:- protected(add_server_session_response/3).
	:- mode(add_server_session_response(+compound, +compound, -compound), one_or_error).
	:- info(add_server_session_response/3, [
		comment is 'Router response-middleware helper that finalizes server-session cookie lifecycle changes after route dispatch.',
		argnames is ['Request', 'Response0', 'Response'],
		exceptions is [
			'``Request`` is not a valid annotated server-session request' - domain_error(http_server_core_session_request, 'Request'),
			'``Response0`` is not a valid normalized HTTP response' - domain_error(http_server_core_session_response, 'Response0'),
			'The decorated response violates normalized HTTP response semantics' - domain_error(http_header_semantics, 'Header')
		]
	]).

	annotate_server_session_request(Request, continue(AnnotatedRequest)) :-
		http_server_core_session::begin(_Manager_, Request, AnnotatedRequest).

	add_server_session_response(Request, Response0, Response) :-
		http_server_core_session::finish(Request, Response0, Response).

:- end_category.
