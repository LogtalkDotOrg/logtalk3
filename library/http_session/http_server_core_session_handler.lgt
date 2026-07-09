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


:- object(http_server_core_session_handler(_Manager_, _Handler_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-09,
		comment is 'Wrapper HTTP handler object that applies server-session begin and finish processing around another portable HTTP handler.',
		parameters is [
			'Manager' - 'Server-session manager handle.',
			'Handler' - 'Wrapped object implementing the ``http_handler_protocol`` protocol.'
		]
	]).

	handle(Request0, Response) :-
		http_server_core_session::begin(_Manager_, Request0, Request),
		_Handler_::handle(Request, Response0),
		http_server_core_session::finish(Request, Response0, Response).

:- end_object.
