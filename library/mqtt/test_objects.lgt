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


:- object(probe_mqtt_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Probe transport object used by the MQTT tests to verify scheme-derived defaults.'
	]).

	supported_request_scheme(http).
	supported_request_scheme(https).

	supported_websocket_scheme(ws).
	supported_websocket_scheme(wss).

	open_connection(Host, Port, probe_connection(Host, Port, Options), Options).

	close_connection(_Connection).

	connection_streams(_Connection, probe_input, probe_output).

:- end_object.


:- object(probe_mqtt_tcp_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Probe transport object with plain HTTP scheme support only.'
	]).

	supported_request_scheme(http).

	open_connection(Host, Port, probe_connection(Host, Port, Options), Options).

	close_connection(_Connection).

	connection_streams(_Connection, probe_input, probe_output).

:- end_object.


:- object(probe_mqtt_file_transport,
	implements(http_transport_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-29,
		comment is 'Probe transport object that exposes file-backed binary streams for MQTT session tests.'
	]).

	:- uses(list, [
		member/2
	]).

	supported_request_scheme(http).
	supported_request_scheme(https).

	supported_websocket_scheme(ws).
	supported_websocket_scheme(wss).

	open_connection(Host, Port, probe_file_connection(Host, Port, Options, Input, Output), Options) :-
		connection_file(response_file, Options, ResponseFile),
		connection_file(request_file, Options, RequestFile),
		open(ResponseFile, read, Input, [type(binary)]),
		catch(
			open(RequestFile, write, Output, [type(binary)]),
			Error,
			( 	close(Input),
				throw(Error)
			)
		).

	close_connection(probe_file_connection(_Host, _Port, _Options, Input, Output)) :-
		catch(close(Input), _, true),
		catch(close(Output), _, true).

	connection_streams(probe_file_connection(_Host, _Port, _Options, Input, Output), Input, Output).

	connection_file(Name, Options, File) :-
		functor(Template, Name, 1),
		member(Template, Options),
		!,
		arg(1, Template, File).

:- end_object.
