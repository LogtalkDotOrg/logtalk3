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


:- protocol(ulid_protocol).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-07-06,
		comment is 'Universally Unique Lexicographically Sortable Identifier (ULID) generator protocol.'
	]).

	:- public(generate/1).
	:- mode(generate(--ulid), one_or_error).
	:- info(generate/1, [
		comment is 'Generates a new ULID.',
		argnames is ['ULID'],
		exceptions is [
			'The representation parameter is a variable' - instantiation_error,
			'The representation parameter is not a variable but also neither ``atom``, ``chars``, nor ``codes``' - domain_error(ulid_representation, 'Representation')
		]
	]).

	:- public(generate/2).
	:- mode(generate(+integer, --ulid), one_or_error).
	:- info(generate/2, [
		comment is 'Generates a new ULID from a timestamp (number of milliseconds since the Unix epoch: 00:00:00 UTC on January 1, 1970).',
		argnames is ['Milliseconds', 'ULID'],
		exceptions is [
			'The representation parameter is a variable' - instantiation_error,
			'The representation parameter is not a variable but also neither ``atom``, ``chars``, nor ``codes``' - domain_error(ulid_representation, 'Representation'),
			'``Milliseconds`` is a variable' - instantiation_error,
			'``Milliseconds`` is neither a variable nor an integer' - type_error(integer, 'Milliseconds'),
			'``Milliseconds`` is outside the 48-bit range required by the ULID specification' - domain_error(ulid_timestamp, 'Milliseconds')
		]
	]).

	:- public(generate/8).
	:- mode(generate(+integer, +integer, +integer, +integer, +integer, +integer, +integer, --ulid), one_or_error).
	:- info(generate/8, [
		comment is 'Generates a new ULID from a timestamp discrete components.',
		argnames is ['Year', 'Month', 'Day',  'Hours', 'Minutes', 'Seconds', 'Milliseconds', 'ULID'],
		exceptions is [
			'The representation parameter is a variable' - instantiation_error,
			'The representation parameter is not a variable but also neither ``atom``, ``chars``, nor ``codes``' - domain_error(ulid_representation, 'Representation'),
			'``Milliseconds`` is a variable' - instantiation_error,
			'``Milliseconds`` is neither a variable nor an integer' - type_error(integer, 'Milliseconds'),
			'``Milliseconds`` is an integer but outside the 0..999 range' - domain_error(ulid_millisecond, 'Milliseconds'),
			'The timestamp is outside the 48-bit range required by the ULID specification' - domain_error(ulid_timestamp, 'Timestamp')
		]
	]).

	:- public(timestamp/2).
	:- mode(timestamp(++ulid, -integer), one_or_error).
	:- info(timestamp/2, [
		comment is 'Returns the given ULID timestamp (number of milliseconds since the Unix epoch: 00:00:00 UTC on January 1, 1970).',
		argnames is ['ULID', 'Milliseconds'],
		exceptions is [
			'The representation parameter is a variable' - instantiation_error,
			'The representation parameter is not a variable but also neither ``atom``, ``chars``, nor ``codes``' - domain_error(ulid_representation, 'Representation'),
			'``ULID`` is a variable' - instantiation_error,
			'``ULID`` is neither a variable nor an atom when using the ``atom`` representation' - type_error(atom, 'ULID'),
			'``ULID`` is neither a variable nor a list of characters when using the ``chars`` representation' - type_error(chars, 'ULID'),
			'``ULID`` is neither a variable nor a list of character codes when using the ``codes`` representation' - type_error(codes, 'ULID'),
			'``ULID`` is not a valid ULID' - domain_error(ulid, 'ULID')
		]
	]).

	:- public(timestamp/8).
	:- mode(timestamp(++ulid, -integer, -integer, -integer, -integer, -integer, -integer, -integer), one_or_error).
	:- info(timestamp/8, [
		comment is 'Decodes a ULID into its timestamp discrete components.',
		argnames is ['ULID', 'Year', 'Month', 'Day',  'Hours', 'Minutes', 'Seconds', 'Milliseconds'],
		exceptions is [
			'The representation parameter is a variable' - instantiation_error,
			'The representation parameter is not a variable but also neither ``atom``, ``chars``, nor ``codes``' - domain_error(ulid_representation, 'Representation'),
			'``ULID`` is a variable' - instantiation_error,
			'``ULID`` is neither a variable nor an atom when using the ``atom`` representation' - type_error(atom, 'ULID'),
			'``ULID`` is neither a variable nor a list of characters when using the ``chars`` representation' - type_error(chars, 'ULID'),
			'``ULID`` is neither a variable nor a list of character codes when using the ``codes`` representation' - type_error(codes, 'ULID'),
			'``ULID`` is not a valid ULID' - domain_error(ulid, 'ULID')
		]
	]).

:- end_protocol.
