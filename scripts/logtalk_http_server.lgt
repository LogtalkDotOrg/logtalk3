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


:- object(logtalk_http_server_handler(_DocumentRoot_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-07,
		comment is 'HTTP handler for the ``logtalk_http_server`` scripts.'
	]).

	handle(Request, Response) :-
		request_path(Request, Path),
		http_static_files::serve(Path, Request, _DocumentRoot_, [directory_listing(true)], Response0),
		close_connection(Response0, Response).

	close_connection(
		response(Version, Status, Headers, Body, Properties),
		response(Version, Status, Headers, Body, [connection([close])| Properties])
	).

	request_path(Request, '/') :-
		http_core::target(Request, origin('/')),
		!.
	request_path(Request, Path) :-
		http_core::target(Request, origin(TargetPath)),
		!,
		relative_path(TargetPath, Path).
	request_path(Request, Path) :-
		http_core::target(Request, origin(TargetPath, _Query)),
		relative_path(TargetPath, Path).

	relative_path(TargetPath, Path) :-
		atom_concat('/', RelativePath, TargetPath),
		( 	sub_atom(RelativePath, _, 1, 0, '/') ->
			sub_atom(RelativePath, 0, _, 1, Path)
		; 	Path = RelativePath
		).

:- end_object.


:- object(logtalk_http_server).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-07,
		comment is 'Local HTTP server used by the ``logtalk_http_server`` scripts.'
	]).

	:- public(serve/2).
	:- mode(serve(+integer, +atom), one_or_error).
	:- info(serve/2, [
		comment is 'Serves files under the given document root on the loopback address until the process is interrupted.',
		argnames is ['Port', 'DocumentRoot']
	]).

	:- public(server_ready/1).
	:- mode(server_ready(+integer), one).
	:- info(server_ready/1, [
		comment is 'Prints the server URL after the listener is ready.',
		argnames is ['Port']
	]).

	serve(Port, DocumentRoot) :-
		http_server::open('127.0.0.1', Port, Server, [transport(http_socket_transport)]),
		server_ready(Port),
		catch(
			serve_requests(Server, logtalk_http_server_handler(DocumentRoot)),
			Error,
			(catch(http_server::close(Server), _, true), throw(Error))
		),
		http_server::close(Server).

	server_ready(Port) :-
		write('Server URL: http://127.0.0.1:'),
		write(Port),
		write('/'),
		nl.

	serve_requests(Server, Handler) :-
		http_server::serve_once(Server, Handler, _),
		serve_requests(Server, Handler).

:- end_object.
