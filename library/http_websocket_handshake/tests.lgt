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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-06-04,
		comment is 'Unit tests for the "http_websocket" library.'
	]).

	:- uses(http_websocket_handshake, [
		websocket_accept/2, websocket_opening_key/1
	]).

	cover(http_websocket_handshake).

	test(http_websocket_handshake_accept_2_01, deterministic(Accept == 's3pPLMBiTxaQ9kYGzzhZRbK+xOo=')) :-
		websocket_accept('dGhlIHNhbXBsZSBub25jZQ==', Accept).

	test(http_websocket_handshake_opening_key_1_01, deterministic) :-
		websocket_opening_key(Key),
		base64::parse(atom(Key), Bytes),
		Bytes = [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _].

:- end_object.
