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


:- protocol(http_websocket_service_handler_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-11,
		comment is 'Protocol for callback objects used by the callback-driven WebSocket service loops.'
	]).

	:- public(handle/2).
	:- mode(handle(+compound, -list(compound)), one_or_error).
	:- info(handle/2, [
		comment is 'Processes a received normalized WebSocket message and returns a list of zero or more session actions. Plain normalized messages are written back on the same session before the next read. When used with the registry-backed server helper, the list may also contain the action wrappers ``reply(Message)``, ``broadcast(Message)``, and ``broadcast_others(Message)``.',
		argnames is ['Message', 'Replies'],
		exceptions is [
			'The implementing handler may throw message-specific or reply-construction exceptions' - error
		]
	]).

:- end_protocol.
