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


:- object(target_http_server_handler,
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-10,
		comment is 'Target echo handler used by the http_server tests.'
	]).

	handle(Request, Response) :-
		http_core::version(Request, Version),
		http_core::target(Request, Target),
		target_text(Target, Text),
		http_core::response(Version, status(200, 'OK'), [], content('text/plain', text(Text)), [], Response).

	target_text(origin(Path), Path).
	target_text(origin(Path, Query), Text) :-
		atom_concat(Path, '?', Prefix),
		atom_concat(Prefix, Query, Text).

:- end_object.


:- object(probe_http_server_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-10,
		comment is 'Probe transport used by the http_server tests to exercise facade cleanup paths.'
	]).

	% only implement the predicates actually required by the tests

	supported_request_scheme(http).

	open_listener(Host, Port, probe_listener(Host, BoundPort, Options), Options) :-
		( 	var(Port) ->
			BoundPort = 0,
			Port = BoundPort
		; 	BoundPort = Port
		).

	close_listener(_Listener).

	serve_listener(_Listener, _Handler, _Count, [], _Options).

:- end_object.
