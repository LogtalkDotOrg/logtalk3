%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- protocol(ulid_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-05-17,
		comment is 'Universally Unique Lexicographically Sortable Identifier (ULID) generator protocol.'
	]).

	:- public(generate/1).
	:- mode(generate(--ulid), one).
	:- info(generate/1, [
		comment is 'Generates a new ULID.',
		argnames is ['ULID']
	]).

	:- public(generate/2).
	:- mode(generate(+integer, --ulid), one).
	:- info(generate/2, [
		comment is 'Generates a new ULID from a timestamp (number of miliseconds since the Unix epoch: 00:00:00 UTC on January 1, 1970).',
		argnames is ['Milliseconds', 'ULID']
	]).

	:- public(timestamp/2).
	:- mode(timestamp(++ulid, -integer), one).
	:- info(timestamp/2, [
		comment is 'Returns the given ULID timestamp (number of miliseconds since the Unix epoch: 00:00:00 UTC on January 1, 1970).',
		argnames is ['ULID', 'Milliseconds']
	]).

:- end_protocol.
