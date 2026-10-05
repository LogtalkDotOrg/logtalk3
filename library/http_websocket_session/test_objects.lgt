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


:- object(fixed_masking_key_http_websocket_client_session,
	extends(http_websocket_session(client, atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-05-24,
		comment is 'Test helper object that overrides client masking-key generation with a fixed key.'
	]).

	:- protected(generate_masking_key/1).
	:- mode(generate_masking_key(-list(byte)), one).
	:- info(generate_masking_key/1, [
		comment is 'Returns the fixed masking key used by the tests.',
		argnames is ['Key']
	]).

	generate_masking_key([1, 2, 3, 4]).

:- end_object.
