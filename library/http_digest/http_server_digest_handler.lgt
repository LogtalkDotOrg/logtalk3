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


:- object(http_server_digest_handler(_Verifier_, _Handler_, _ProtectOptions_, _AuthenticationInfoOptions_),
	implements(http_handler_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-29,
		comment is 'Wrapper HTTP handler object that applies Digest request verification and Authentication-Info response decoration around another portable HTTP handler.',
		parameters is [
			'Verifier' - 'Verifier object implementing the ``http_digest_verifier_protocol`` protocol.',
			'Handler' - 'Wrapped object implementing the ``http_handler_protocol`` protocol.',
			'ProtectOptions' - 'Options passed to ``http_digest::protect_request/4``.',
			'AuthenticationInfoOptions' - 'Options passed to ``http_digest::add_authentication_info/4`` on successful request verification.'
		]
	]).

	handle(Request0, Response) :-
		http_digest::protect_request(Request0, _Verifier_, Action, _ProtectOptions_),
		(	Action = continue(Request) ->
			_Handler_::handle(Request, Response0),
			http_digest::add_authentication_info(Request, Response0, Response, _AuthenticationInfoOptions_)
		;	Action = respond(Response)
		).

:- end_object.
