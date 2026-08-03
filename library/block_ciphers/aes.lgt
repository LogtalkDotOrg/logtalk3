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


:- object(aes128,
	extends(aes_common(16, 4, 10))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'AES-128 block cipher.',
		see_also is [aes192, aes256]
	]).

:- end_object.


:- object(aes192,
	extends(aes_common(24, 6, 12))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'AES-192 block cipher.',
		see_also is [aes128, aes256]
	]).

:- end_object.


:- object(aes256,
	extends(aes_common(32, 8, 14))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'AES-256 block cipher.',
		see_also is [aes128, aes192]
	]).

:- end_object.
