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


:- object(routes,
	implements(http_handler_protocol),
	imports(http_router)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Example HTTP router with three routes, illustrating the http_router, http_server, and http_client libraries.'
	]).

	:- uses(http_core, [
		version/2, body/2, response/6
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- protected(hello/2).
	:- info(hello/2, [
		comment is 'Route handler for the ``GET /hello`` route.',
		argnames is ['Request', 'Response']
	]).

	:- protected(show_user/2).
	:- info(show_user/2, [
		comment is 'Route handler for the ``GET /users/{id:integer}`` route.',
		argnames is ['Request', 'Response']
	]).

	:- protected(echo/2).
	:- info(echo/2, [
		comment is 'Route handler for the ``POST /echo`` route.',
		argnames is ['Request', 'Response']
	]).

	% route(Id, Method, PathTemplate, Handler)
	route(hello, get, '/hello', hello).
	route(show_user, get, '/users/{id:integer}', show_user).
	route(echo, post, '/echo', echo).

	% GET /hello
	% a plain static route returning a fixed text response
	hello(Request, Response) :-
		version(Request, Version),
		response(
			Version, status(200, 'OK'), [],
			content('text/plain', text('Hello from the Logtalk http_router example!')),
			[], Response
		).

	% GET /users/{id:integer}
	% a route with a typed path-template placeholder; the router
	% annotates the request with a path_params(Pairs) property
	% holding the extracted (and already typed) path parameters
	show_user(Request, Response) :-
		version(Request, Version),
		Request = request(_Method, _Target, _Version, _Headers, _Body, Properties),
		memberchk(path_params(PathParams), Properties),
		memberchk(id-Id, PathParams),
		atomic_list_concat(['User #', Id], Text),
		response(
			Version, status(200, 'OK'), [],
			content('text/plain', text(Text)),
			[], Response
		).

	% POST /echo
	% a route that reads the request body and echoes it back
	echo(Request, Response) :-
		version(Request, Version),
		body(Request, content(_MediaType, text(RequestText))),
		atomic_list_concat(['Echo: ', RequestText], ResponseText),
		response(
			Version, status(200, 'OK'), [],
			content('text/plain', text(ResponseText)),
			[], Response
		).

:- end_object.
