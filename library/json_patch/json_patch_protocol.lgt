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


:- protocol(json_patch_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-03,
		comment is 'JSON Patch (RFC 6902) application protocol.'
	]).

	:- public(apply/3).
	:- mode(apply(++list, ++term, --term), zero_or_one_or_error).
	:- info(apply/3, [
		comment is 'Applies a JSON Patch document represented as a list of operation objects to a JSON term and returns the patched JSON term.',
		argnames is ['Patch', 'OldJSON', 'NewJSON'],
		exceptions is [
			'``Patch`` is a variable' - instantiation_error,
			'``OldJSON`` is a variable' - instantiation_error,
			'``Patch`` is neither a variable nor a list' - type_error(list, 'Patch'),
			'An operation in ``Patch`` is malformed' - domain_error(json_patch_operation, 'Operation')
		]
	]).

:- end_protocol.
