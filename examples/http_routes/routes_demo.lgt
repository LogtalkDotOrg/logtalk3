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


:- object(routes_demo).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Starts a local server running the routes handler, calls each of its three routes using the ``http_client`` library, and prints the responses.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Starts the server, calls the three example routes, stops the server.'
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	run :-
		% start a loopback server serving the "routes" router object;
		% Port is left unbound so the backend picks a free port
		http_server::start('127.0.0.1', Port, routes, Server, []),
		atomic_list_concat(['http://127.0.0.1:', Port], BaseURL),
		call_hello(BaseURL),
		call_show_user(BaseURL),
		call_echo(BaseURL),
		http_server::stop(Server).

	% GET /hello
	call_hello(BaseURL) :-
		atomic_list_concat([BaseURL, '/hello'], URL),
		http_client::get(URL, Response, []),
		print_response('GET /hello', Response).

	% GET /users/42
	call_show_user(BaseURL) :-
		atomic_list_concat([BaseURL, '/users/42'], URL),
		http_client::get(URL, Response, []),
		print_response('GET /users/42', Response).

	% POST /echo
	call_echo(BaseURL) :-
		atomic_list_concat([BaseURL, '/echo'], URL),
		http_client::post(
			URL,
			content('text/plain', text('Hello, router!')),
			Response,
			[]
		),
		print_response('POST /echo', Response).

	print_response(Label, response(_Version, status(Code, Reason), _Headers, content(_MediaType, text(Text)), _Properties)) :-
		write(Label), write(' -> '), write(Code), write(' '), write(Reason), nl,
		write('  '), write(Text), nl.

:- end_object.
