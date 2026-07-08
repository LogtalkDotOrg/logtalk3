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


:- object(http_server_core_basic_handler(_Verifier_, _Handler_, _ProtectOptions_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Wrapper HTTP handler object that applies Basic request verification around another portable HTTP handler.',
		parameters is [
			'Verifier' - 'Verifier object implementing the ``http_authenticate_verifier_protocol`` protocol.',
			'Handler' - 'Wrapped object implementing the ``http_handler_protocol`` protocol.',
			'ProtectOptions' - 'Options passed to ``http_authenticate::protect_request/4``.'
		]
	]).

	handle(Request0, Response) :-
		http_authenticate::protect_request(Request0, _Verifier_, Action, _ProtectOptions_),
		(	Action = continue(Request) ->
			_Handler_::handle(Request, Response)
		;	Action = respond(Response)
		).

:- end_object.
